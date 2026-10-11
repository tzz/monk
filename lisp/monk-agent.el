;;; monk-agent.el --- Standalone agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The standalone execution mode.  An agent is `emacs --batch' on the
;; host it manages.  It converges the local policy on start, then
;; watches for drift in autopilot mode, and converges in full again
;; every `monk-agent-full-interval'.  It needs no controller.
;;
;; Every outcome goes to a ledger as JSON Lines (`monk-event'), and to
;; journald when `systemd-cat' exists.  A central Monk observes by
;; tailing the ledger over TRAMP (`monk-agent-observe'); it does not
;; control the agent.  A controller that wants control runs the same
;; policy in supervised or autopilot mode over TRAMP; the two do not
;; fight, because both apply the same policy and the watch settle
;; window drops the events the other side produced.
;;
;; Two systemd units, both generated here and installed by the policy
;; through `monk-agent-units', or from a controller through
;; `monk-agent-install':
;;
;; - monk-agent.service: the long-lived loop above.  A watcher must
;;   stay resident, so this one is not socket activated.
;; - monk-agent.socket + monk-agent@.service: an on-demand query
;;   endpoint on a Unix socket, `Accept=yes'.  systemd accepts each
;;   connection and starts one `emacs --batch -f monk-agent-serve'
;;   with the connection on stdin and stdout.  Emacs cannot adopt an
;;   inherited listening socket, but it reads and writes stdio well,
;;   so the inetd shape is the right one.  Idle cost is zero.
;;
;; The endpoint speaks one JSON object per line.  Requests:
;;   {"op":"status"}            -> last run-end event, agent liveness
;;   {"op":"tail","n":50}       -> last N ledger lines
;;   {"op":"history-runs","since":"7d","limit":20}
;;   {"op":"history-results","name":"~/.zshrc","changed":true}
;;                              -> rows from the history database
;;   {"op":"plan"}              -> dry run now, events streamed back
;;   {"op":"run"}               -> converge now, events streamed back
;; Every response line is a `monk-event' record or {"op":..., "ok":...}.
;; From the controller, `monk-agent-query' sends a request through
;; `systemd-run' or `socat' over TRAMP, so no port is opened.
;;
;; The policy file sets `monk-batch-policy'.

;;; Code:

(require 'monk)
(require 'monk-file)
(require 'monk-system)
(require 'monk-watch)
(require 'monk-event)
(require 'monk-cli)
(require 'timer)
;; Every agent run goes to the history database when this Emacs has
;; SQLite; the ledger is written in any case.
(when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  (require 'monk-history))

(defgroup monk-agent nil
  "Standalone Monk agent."
  :group 'monk
  :prefix "monk-agent-")

(defconst monk-agent-journal-program "systemd-cat"
  "Program that writes to journald.")

(defconst monk-agent-journal-identifier "monk"
  "SYSLOG_IDENTIFIER for journald lines.")

(defconst monk-agent-loop-interval 1.0
  "Seconds between wakeups of the batch loop.")

(defcustom monk-agent-ledger-file nil
  "Where the agent appends its events.
Nil means events.ndjson under `monk-home-directory', next to the
history database."
  :type '(choice (const nil) file))

(defun monk-agent-ledger-path ()
  "Return the ledger path."
  (expand-file-name (or monk-agent-ledger-file
                        (concat (monk-home-directory) "events.ndjson"))))

(defcustom monk-agent-log-level 'warn
  "`monk-log-level' while the agent loop runs.
The agent prints its own lines per change and per run, so `info'
logging repeats them.  Set to `info' to see passes and probes."
  :type '(choice (const debug) (const info) (const warn) (const nil)))

(defcustom monk-agent-full-interval 3600
  "Seconds between full converges.
Drift watching covers owned paths; a full converge covers the rest:
packages, cron, services, and paths whose parent did not exist yet."
  :type 'number)

(defcustom monk-agent-splay 0
  "Maximum random delay in seconds before the first and each full converge.
Spreads a fleet's runs out so a shared server does not see them all
at once.  Ansible's `ansible-pull --sleep'.  Zero means no delay."
  :type 'natnum)

(defcustom monk-agent-policy-repo nil
  "Directory of the git checkout the policy is loaded from, or nil.
When set, the agent fetches and fast-forwards it before each full
converge.  When the checkout did not move and
`monk-agent-only-if-changed' is non-nil, the full converge is skipped;
the drift watch still runs.  Ansible's `ansible-pull -o'."
  :type '(choice (const nil) directory))

(defcustom monk-agent-only-if-changed nil
  "When non-nil, a timed full converge runs only after the policy repo moved.
The first converge on start always runs.  See `monk-agent-policy-repo'."
  :type 'boolean)

(defvar monk-agent--policy-revision nil
  "The revision of `monk-agent-policy-repo' at the last converge.")

(defconst monk-agent-socket-path "/run/monk/agent.sock"
  "Unix socket the query endpoint listens on.")

(defconst monk-agent-user-socket-path "%t/monk/agent.sock"
  "Unix socket for a user instance.  %t is XDG_RUNTIME_DIR under systemd.")

(defconst monk-agent-service-name "monk-agent.service")
(defconst monk-agent-launchd-label "org.monk.agent"
  "Label of the launchd job that runs the agent loop on macOS.")
(defconst monk-agent-socket-name "monk-agent.socket")
(defconst monk-agent-template-name "monk-agent@.service")

(defconst monk-agent-ops '("status" "tail" "plan" "run" "history-runs" "history-results"
                           "subscribe" "watch-mode" "watch-log")
  "Operations the query endpoint accepts.")

(defconst monk-agent-local-socket-name "agent.sock"
  "Socket file under the Monk state directory for a local, same-user agent.")

(defconst monk-agent-pid-file-name "agent.pid"
  "File under the Monk state directory with the agent's pid and socket path.")

(defconst monk-agent-tail-default 50
  "Lines a tail request returns when it gives no count.")

(defcustom monk-agent-emacs "/usr/bin/emacs"
  "Emacs binary the units run."
  :type 'file)

(defcustom monk-agent-load-path nil
  "Directory with the Monk Lisp files on the target.
Nil means the directory of this file at install time."
  :type '(choice (const nil) directory))

(defcustom monk-agent-policy-file nil
  "Policy file the units load.  Nil means `load-file-name' at install time."
  :type '(choice (const nil) file))

(defcustom monk-agent-etcd nil
  "When non-nil, also publish runs and presence to etcd at `monk-etcd-url'.
The ledger stays the source of truth; etcd is for a fleet view.  A
failed etcd call is logged and does not fail the run."
  :type 'boolean)

(defvar monk-agent--full-timer nil
  "Timer for the periodic full converge.")

(defvar monk-agent--etcd-lease nil
  "Lease that keeps the presence key alive.")

(defvar monk-agent--etcd-timer nil
  "Timer that refreshes the presence lease.")

(defvar monk-agent--server nil
  "The local socket server process, while the agent runs.")

(defvar monk-agent--running t
  "Nil makes `monk-agent-main' return.")

;;;; Ledger

(defun monk-agent--journal (line)
  "Send LINE to journald when `systemd-cat' is on the path."
  (when (executable-find monk-agent-journal-program)
    (with-temp-buffer
      (setq default-directory (file-name-as-directory (expand-file-name temporary-file-directory)))
      (insert line)
      (call-process-region (point-min) (point-max) monk-agent-journal-program
                           nil nil nil "-t" monk-agent-journal-identifier))))

(defun monk-agent-record (run)
  "Append the events of RUN to the ledger and journald."
  (let ((file (monk-agent-ledger-path)))
    (make-directory (file-name-directory file) t)
    (monk-run-write-events run file)
    (dolist (event (monk-run-events run))
      (monk-agent--journal (monk-event--json event)))))

(declare-function monk-etcd-publish-run "monk-etcd")
(declare-function monk-etcd-announce "monk-etcd")
(declare-function monk-etcd-keepalive "monk-etcd")
(defvar monk-etcd-agent-ttl)

(defun monk-agent--etcd (thunk)
  "Call THUNK when etcd publishing is on; log instead of failing."
  (when monk-agent-etcd
    (require 'monk-etcd)
    (condition-case err
        (funcall thunk)
      (error (monk-log 'warn "etcd: %s" (error-message-string err)) nil))))

(defun monk-agent--announce (run)
  "Refresh presence in etcd with the summary of RUN, or a heartbeat."
  (monk-agent--etcd
   (lambda ()
     (setq monk-agent--etcd-lease
           (monk-etcd-announce (system-name)
                               `((mode . "standalone")
                                 (last_summary . ,(and run (monk-run-summary run)))
                                 (ok . ,(if (or (null run) (monk-run-ok-p run)) t :false)))
                               monk-agent--etcd-lease)))))

(defcustom monk-agent-print-changes t
  "When non-nil, the agent prints one line per changed or failed result.
The summary line is printed in any case."
  :type 'boolean)

(defun monk-agent--report (run &optional _verbose)
  "The agent's report function: record RUN, publish it, print the summary.
Changed and failed results were printed as they finished by
`monk-cli'; on a terminal they scrolled above the status line."
  (monk-agent-record run)
  (monk-agent--etcd (lambda () (monk-etcd-publish-run run)))
  (monk-agent--announce run)
  (unless (and monk-agent-print-changes (memq #'monk-cli--progress monk-progress-functions))
    (when monk-agent-print-changes
      (dolist (r (monk-run-results run))
        (when (or (monk-result-changed-p r) (monk-result-failed-p r))
          (monk-cli-print-result r)))))
  (monk-cli-print-line (format "%s %s  %s%s" (monk-cli--color "1" "monk-agent")
                               (format-time-string "%T")
                               (monk-cli-summary-line run)
                               (monk-agent--stale-note))))

(defun monk-agent--stale-note ()
  "Return a note when this agent still runs code older than its source.
After `monk-agent--reload-if-stale' that is only the case when the
reload failed; the line then says so on every run until it works."
  (let ((stale (monk-stale-libraries)))
    (if (null stale)
        ""
      (monk-cli--color "33" (format "  STALE CODE: %s (reload failed; see the log)"
                                    (mapconcat (lambda (c) (symbol-name (car c))) stale ", "))))))

(defvar monk-agent--policy-file-mtime nil
  "Modification time of `monk-agent-policy-file' when it was last loaded.")

(declare-function monk-reload "monk-reload" (&optional verbose))

(defun monk-agent--load-new-libraries ()
  "Load Monk libraries that exist on disk but were never loaded here.
A library added to the tree after the agent started, such as
`monk-connect', is not stale: it is absent.  `monk-reload' does not
know it; this does.  Return the features loaded."
  (let (loaded)
    (dolist (feature monk-libraries)
      (when (and (not (featurep feature))
                 (not (memq feature '(monk-testdrive monk-tutorial monk-etcd monk-translate monk-embark)))
                 (locate-library (symbol-name feature)))
        (condition-case err
            (progn (require feature) (push feature loaded))
          (error (monk-log 'warn "agent: loading %s failed: %s" feature (error-message-string err))))))
    (nreverse loaded)))

(defun monk-agent--reload-if-stale ()
  "Reload Monk and the policy file when their source changed since they loaded.
The agent is a long-lived Emacs and must follow its own source: an
edit to a library or to the policy applies at the next timed
converge, with no restart.  `monk-reload' restarts the watch on the
new code and clears the caches.  Return a short note for the run
line, or nil when nothing was stale."
  (let* ((stale (monk-stale-libraries))
         (policy monk-agent-policy-file)
         (policy-mtime (and policy (file-exists-p policy)
                            (file-attribute-modification-time (file-attributes policy))))
         (policy-stale (and policy-mtime
                            (or (null monk-agent--policy-file-mtime)
                                (time-less-p monk-agent--policy-file-mtime policy-mtime))))
         (notes nil))
    (when stale
      (require 'monk-reload)
      (condition-case err
          (let ((inhibit-message t))
            (monk-reload)
            (push (format "reloaded %s" (mapconcat (lambda (c) (symbol-name (car c))) stale ", ")) notes))
        (error (monk-log 'warn "agent: reload failed: %s" (error-message-string err))
               (push "reload FAILED" notes))))
    (let ((new (monk-agent--load-new-libraries)))
      (when new
        (push (format "loaded new %s" (mapconcat #'symbol-name new ", ")) notes)))
    ;; An agent that started before the socket existed, or whose reload
    ;; redefined the server without restarting it, serves from now on.
    (when (and (null monk-agent--server) (fboundp 'monk-agent-serve-local))
      (when (monk-agent-serve-local)
        (push "socket started" notes)))
    (when policy-stale
      (condition-case err
          (progn (load policy nil t)
                 (setq monk-agent--policy-file-mtime policy-mtime)
                 (push (format "reloaded policy %s" (file-name-nondirectory policy)) notes))
        (error (monk-log 'warn "agent: policy reload failed: %s" (error-message-string err))
               (push "policy reload FAILED" notes))))
    (and notes (string-join (nreverse notes) ", "))))

;;;; Loop

(defvar monk-inventory)
(declare-function monk-inventory-find "monk-inventory" (name))

(defun monk-agent-machine ()
  "Return the machine the agent manages: this host.
When an inventory is loaded and names this host, that entry is used,
so inventory variables, groups, and their classes apply, as they do
from a controller.  Otherwise a bare local machine."
  (or (and (bound-and-true-p monk-inventory)
           (fboundp 'monk-inventory-find)
           (monk-inventory-find (car (split-string (system-name) "\\."))))
      (monk-local-machine)))

(defvar monk-agent--machine nil
  "The machine of the running agent.")

(defun monk-agent-splay-seconds ()
  "Return a random delay in [0, `monk-agent-splay'] seconds, or 0."
  (if (> monk-agent-splay 0) (random (1+ monk-agent-splay)) 0))

(defun monk-agent-policy-revision ()
  "Return the HEAD revision of `monk-agent-policy-repo', or nil without one."
  (and monk-agent-policy-repo
       (file-directory-p monk-agent-policy-repo)
       (condition-case err
           (monk--git-in monk-agent-policy-repo "rev-parse" "HEAD")
         (error (monk-log 'warn "policy repo: %s" (error-message-string err)) nil))))

(defun monk-agent-update-policy-repo ()
  "Fetch and fast-forward `monk-agent-policy-repo'.
Return `changed' when HEAD moved, `same' when it did not, and nil
when there is no repo or the update failed.  A failed fetch is
logged; the converge then runs with the policy already on disk."
  (when monk-agent-policy-repo
    (let ((before (monk-agent-policy-revision)))
      (condition-case err
          (progn
            (monk--git-in monk-agent-policy-repo "pull" "--ff-only" "--quiet")
            (let ((after (monk-agent-policy-revision)))
              (setq monk-agent--policy-revision after)
              (if (equal before after) 'same 'changed)))
        (error (monk-log 'warn "policy repo update: %s" (error-message-string err)) nil)))))

(defun monk-agent--full-converge (policy &optional timed)
  "Converge the whole local POLICY and record it.
With TIMED, this is a scheduled converge: wait a splay first, update
the policy repo, and when `monk-agent-only-if-changed' is set and the
repo did not move, skip the converge."
  (when timed
    (let ((delay (monk-agent-splay-seconds)))
      (when (> delay 0)
        (monk-log 'info "splay: %ds" delay)
        (sleep-for delay)))
    ;; Follow the source: an edited library or policy is picked up
    ;; here, in place.  The watch comes back on the new code.
    (let ((note (monk-agent--reload-if-stale)))
      (when note
        (monk-cli-print-line (format "%s %s build %s: %s" (monk-cli--color "1" "monk-agent")
                                     monk-version monk-build note)))))
  (let ((update (monk-agent-update-policy-repo)))
    (when (eq update 'changed)
      (monk-cli-print-line (format "policy repo moved to %s" (substring monk-agent--policy-revision 0 12)))
      (when monk-agent-policy-file
        (load monk-agent-policy-file nil t)))
    (if (and timed monk-agent-only-if-changed monk-agent-policy-repo (not (eq update 'changed)))
        (monk-log 'info "policy repo unchanged; full converge skipped")
      (let ((monk-watch--running t))
        (monk-run-report (monk-converge (or monk-agent--machine (monk-agent-machine)) policy))))))

(defun monk-agent--idle-text ()
  "Text of the watching line: paths, runs, uptime, next full converge."
  (let ((w monk-watch--current)
        (next (and monk-agent--full-timer
                   (max 0 (round (float-time (time-subtract (timer--time monk-agent--full-timer)
                                                             (current-time))))))))
    (if (null w)
        "watching"
      (format "watching %d dir(s)  %d run(s)  up %s%s"
              (length (monk-watch-watched w)) (monk-watch-runs w)
              (monk-format-duration (float-time (time-since (monk-watch-started w))) t)
              (if next (format "  full in %s" (monk-format-duration next)) "")))))

(defun monk-agent-stop ()
  "Make the agent loop return after the current iteration."
  (setq monk-agent--running nil))

(defun monk-agent-main (&optional policy)
  "Run the standalone agent until `monk-agent-stop', SIGTERM, or SIGINT.
POLICY defaults to `monk-batch-policy', set by the policy file loaded
before this.  Callable as `emacs --batch -l POLICY-FILE -f monk-agent-main'
or from a wrapper that picks the policy itself.  Converges on start,
then watches for drift in autopilot, then converges in full every
`monk-agent-full-interval' seconds.  An interrupt (SIGINT) in a
terminal stops it cleanly: the watch is torn down and the last run
is already recorded."
  (let ((policy (or policy monk-batch-policy
                    (signal 'monk-policy-error (list "monk-batch-policy is not set"))))
        (monk-report-function #'monk-agent--report)
        (monk-log-level monk-agent-log-level)
        (monk-watch-mode 'autopilot)
        (m (monk-agent-machine)))
    ;; The policy file is what defined the policy, unless told otherwise;
    ;; the agent reloads it when it changes.
    (unless monk-agent-policy-file
      (let ((file (monk-definition-file policy)))
        (when file
          (setq monk-agent-policy-file (concat (file-name-sans-extension file) ".el")))))
    (setq monk-agent--running t
          monk-agent--machine m
          monk-agent--policy-file-mtime
          (and monk-agent-policy-file (file-exists-p monk-agent-policy-file)
               (file-attribute-modification-time (file-attributes monk-agent-policy-file))))
    (monk-cli-enable)
    ;; In batch, SIGINT (C-c) raises `quit' out of `accept-process-output',
    ;; and SIGTERM runs `kill-emacs-hook'.  Both must end the loop, not
    ;; leave a half-torn watch.
    (add-hook 'kill-emacs-hook #'monk-agent-stop)
    (monk-cli-print-line
     (format "%s %s build %s start on %s as %s, ledger %s, socket %s, full converge every %ss%s"
             (monk-cli--color "1" "monk-agent") monk-version monk-build (oref m name)
             (monk-owner-string (monk-owner))
             (abbreviate-file-name (monk-agent-ledger-path))
             (abbreviate-file-name (monk-agent-local-socket-path)) monk-agent-full-interval
             (if monk-cli--tty "" "  (no terminal: progress off)")))
    (let ((delay (monk-agent-splay-seconds)))
      (when (> delay 0)
        (monk-cli-print-line (format "splay: first converge in %ds" delay))
        (sleep-for delay)))
    (monk-agent-serve-local)
    (monk-agent--full-converge policy)
    (monk-watch-start m policy)
    (monk-cli-idle-start #'monk-agent--idle-text)
    (setq monk-agent--full-timer
          (run-at-time monk-agent-full-interval monk-agent-full-interval
                       #'monk-agent--full-converge policy t))
    (when monk-agent-etcd
      (require 'monk-etcd)
      (setq monk-agent--etcd-timer
            (run-at-time 0 (/ monk-etcd-agent-ttl 3.0)
                         (lambda ()
                           (monk-agent--etcd
                            (lambda ()
                              (unless (and monk-agent--etcd-lease
                                           (monk-etcd-keepalive monk-agent--etcd-lease))
                                (setq monk-agent--etcd-lease nil)
                                (monk-agent--announce nil))))))))
    (unwind-protect
        (condition-case nil
            (while monk-agent--running
              ;; Timers and file notifications fire inside this wait.
              (accept-process-output nil monk-agent-loop-interval))
          (quit (monk-cli-print-line "monk-agent: interrupted")))
      (remove-hook 'kill-emacs-hook #'monk-agent-stop)
      (dolist (timer (list monk-agent--full-timer monk-agent--etcd-timer))
        (when timer (cancel-timer timer)))
      (setq monk-agent--full-timer nil
            monk-agent--etcd-timer nil)
      (monk-watch-stop)
      (monk-agent-stop-local)
      (monk-cli-idle-stop)
      (monk-cli-disable)
      (monk-cli-print-line "monk-agent: stop"))))

;;;; systemd units

(defun monk-agent--exec-start (function)
  "Return the ExecStart line that runs FUNCTION in batch Emacs."
  (format "%s --batch -L %s -l %s -f %s"
          monk-agent-emacs
          (shell-quote-argument (directory-file-name
                                 (or monk-agent-load-path
                                     (file-name-directory (or load-file-name buffer-file-name
                                                              default-directory)))))
          (shell-quote-argument (or monk-agent-policy-file load-file-name ""))
          function))

(defun monk-agent-service-unit (&optional user)
  "Return the text of monk-agent.service.  With USER, a user instance."
  (concat "[Unit]\n"
          "Description=Monk standalone agent\n"
          "Documentation=https://github.com/tzz/monk\n"
          (if user "" "After=network-online.target\nWants=network-online.target\n")
          "\n[Service]\n"
          "Type=simple\n"
          "ExecStart=" (monk-agent--exec-start "monk-agent-main") "\n"
          "Restart=on-failure\n"
          "RestartSec=10\n"
          (if user "" "StateDirectory=monk\nRuntimeDirectory=monk\n")
          "Environment=HOME=" (if user "%h" "/root") "\n"
          "KillSignal=SIGTERM\n"
          "TimeoutStopSec=30\n"
          "\n[Install]\n"
          "WantedBy=" (if user "default.target" "multi-user.target") "\n"))

(defun monk-agent-socket-unit (&optional user)
  "Return the text of monk-agent.socket.  With USER, a user instance."
  (concat "[Unit]\n"
          "Description=Monk agent query socket\n"
          "\n[Socket]\n"
          "ListenStream=" (if user monk-agent-user-socket-path monk-agent-socket-path) "\n"
          "Accept=yes\n"
          "SocketMode=0600\n"
          "MaxConnections=4\n"
          "\n[Install]\n"
          "WantedBy=sockets.target\n"))

(defun monk-agent-template-unit ()
  "Return the text of monk-agent@.service, one instance per connection."
  (concat "[Unit]\n"
          "Description=Monk agent query %i\n"
          "CollectMode=inactive-or-failed\n"
          "\n[Service]\n"
          "Type=simple\n"
          "ExecStart=" (monk-agent--exec-start "monk-agent-serve") "\n"
          "StandardInput=socket\n"
          "StandardOutput=socket\n"
          "StandardError=journal\n"
          "TimeoutStartSec=600\n"))

(defun monk-agent-launchd-job (&optional policy-file)
  "Return the launchd job alist that runs `monk-agent-main' from POLICY-FILE.
KeepAlive restarts the loop when it exits; ThrottleInterval keeps a
crash loop from spinning.  Output goes under the Monk state directory."
  (let* ((policy (or policy-file monk-agent-policy-file load-file-name ""))
         (log (expand-file-name "agent.log" (if (fboundp 'monk-home-directory)
                                                (funcall 'monk-home-directory)
                                              "~/.local/share/monk/"))))
    (list (cons 'ProgramArguments
                (list monk-agent-emacs "--batch"
                      "-L" (directory-file-name
                            (or monk-agent-load-path
                                (file-name-directory (or load-file-name buffer-file-name default-directory))))
                      "-l" policy
                      "-f" "monk-agent-main"))
          (cons 'RunAtLoad t)
          (cons 'KeepAlive t)
          (cons 'ThrottleInterval 30)
          (cons 'StandardOutPath log)
          (cons 'StandardErrorPath log)
          (cons 'EnvironmentVariables
                (list (cons 'PATH "/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"))))))

(defun monk-agent-launchd-units (&rest args)
  "Policy fragment: install and load the agent as a launchd user agent.
The macOS counterpart of `monk-agent-units'.  ARGS: :policy-file, and
common keys.  Call inside a policy.  There is no socket endpoint on
launchd; query the agent through its ledger or history."
  (apply #'monk-launchd monk-agent-launchd-label
         :plist (monk-agent-launchd-job (plist-get args :policy-file))
         :loaded t
         (monk--common-initargs args)))

(defun monk-agent-units (&rest args)
  "Policy fragment: install and enable the agent units on this machine.
ARGS: :user t for a user instance, :socket nil to skip the query
endpoint, and common keys.  Call inside a policy."
  (let* ((user (plist-get args :user))
         (socket (if (plist-member args :socket) (plist-get args :socket) t))
         (dir (if user monk-systemd-user-unit-directory monk-systemd-unit-directory))
         (sudo (not user)))
    (monk-systemd-unit monk-agent-service-name
                       :content (monk-agent-service-unit user)
                       :enabled t :started t :user-unit user :sudo sudo)
    (when socket
      (monk-file (concat dir monk-agent-template-name)
                 :content (monk-agent-template-unit) :sudo sudo)
      (monk-systemd-unit monk-agent-socket-name
                         :content (monk-agent-socket-unit user)
                         :enabled t :started t :user-unit user :sudo sudo
                         :watch (list (concat dir monk-agent-template-name))
                         :watch-action 'daemon-reload))))

;;;; Query endpoint (socket activated, one process per connection)

(defvar monk-agent--respond-to nil
  "Where responses go: nil for stdout, else a process to send to.
Bound per connection by the socket server.")

(defun monk-agent--respond (object)
  "Write OBJECT as one JSON line to stdout, or to the connection being served."
  (let ((line (monk-event-line object)))
    (if (and monk-agent--respond-to (process-live-p monk-agent--respond-to))
        (process-send-string monk-agent--respond-to line)
      (princ line))))

(defun monk-agent--ledger-tail (n)
  "Return the last N lines of the ledger as strings, oldest first."
  (let ((file (monk-agent-ledger-path)))
    (if (not (file-exists-p file))
        nil
      (with-temp-buffer
        (insert-file-contents file)
        (let ((lines (split-string (buffer-string) "\n" t)))
          (last lines n))))))

(defun monk-agent--last-run-end ()
  "Return the most recent run-end event alist from the ledger, or nil."
  (seq-find (lambda (e) (equal (alist-get 'event e) "run-end"))
            (mapcar #'monk-event-parse (reverse (monk-agent--ledger-tail 500)))))

(defun monk-agent--service-active-p ()
  "Return non-nil when monk-agent.service is active, per systemctl."
  (let ((default-directory (file-name-as-directory (expand-file-name temporary-file-directory))))
    (and (executable-find "systemctl")
         (zerop (call-process "systemctl" nil nil nil "is-active" "--quiet"
                              monk-agent-service-name)))))

(defun monk-agent-status-alist ()
  "Return what an agent says about itself.
Host, owner, build, watch state, sudo state, last run."
  (let ((w (and (boundp 'monk-watch--current) monk-watch--current)))
    `((host . ,(system-name))
      (owner . ,(monk-owner-string (monk-owner)))
      (scope . ,(if (monk-root-p) "root" "user"))
      (pid . ,(emacs-pid))
      (version . ,monk-version)
      (build . ,monk-build)
      (agent_active . ,(if (or monk-agent--server (monk-agent--service-active-p)) t :false))
      (watching . ,(if w t :false))
      (watch_mode . ,(and w (symbol-name monk-watch-mode)))
      (watch_dirs . ,(and w (length (monk-watch-watched w))))
      (watch_runs . ,(and w (monk-watch-runs w)))
      (watch_started . ,(and w (monk-event--time (monk-watch-started w))))
      (next_full . ,(and monk-agent--full-timer
                         (monk-event--time (timer--time monk-agent--full-timer))))
      (sudo . ,(monk-agent--sudo-state))
      (ledger . ,(monk-agent-ledger-path))
      (last_run . ,(monk-agent--last-run-end)))))

(defun monk-agent--sudo-state ()
  "Return what a sudo resource would meet here: `root', `cached', or `password'.
`sudo -n true' succeeds under NOPASSWD or while the timestamp is
fresh; otherwise it fails at once instead of prompting."
  (let ((default-directory temporary-file-directory))
    (cond ((monk-root-p) "root")
          ((not (executable-find "sudo")) "none")
          ((zerop (call-process "sudo" nil nil nil "-n" "true")) "cached")
          (t "password"))))

(defvar monk-agent--subscribers nil
  "Connections that asked for every event of this agent.")

(defun monk-agent--broadcast (event)
  "Send EVENT, an alist, to every live subscriber; drop the dead ones."
  (when monk-agent--subscribers
    (let ((line (monk-event-line event)))
      (setq monk-agent--subscribers
            (seq-filter (lambda (proc)
                          (when (process-live-p proc)
                            (condition-case nil
                                (progn (process-send-string proc line) t)
                              (error nil))))
                        monk-agent--subscribers)))))

(defun monk-agent--broadcast-progress (event data)
  "Progress hook: turn EVENT with DATA into a record for subscribers."
  (when monk-agent--subscribers
    (pcase event
      ('run-start (monk-agent--broadcast (monk-event-run 'run-start (plist-get data :run))))
      ('node-end (when monk--current-run
                   (monk-agent--broadcast
                    (monk-event-result (plist-get data :result) monk--current-run
                                       (plist-get data :pass)))))
      ('run-end (monk-agent--broadcast (monk-event-run 'run-end (plist-get data :run)))))))

;;;; Local socket server (same user, same host)
;;
;; The systemd endpoint spawns one Emacs per connection through socket
;; activation.  A plain agent, `make monk' in a terminal or a launchd
;; job, serves its own socket from inside the loop: a Unix socket under
;; the Monk state directory, mode 0600, so only the owner (or root)
;; can connect.  That is the scope rule made concrete: a user connects
;; to their own agent, a root agent's socket lives under /etc/monk.

(defun monk-agent-local-socket-path ()
  "Return the path of the local agent socket for this owner."
  (expand-file-name monk-agent-local-socket-name (monk-home-directory)))

(defun monk-agent-pid-file ()
  "Return the path of the pid file for this owner."
  (expand-file-name monk-agent-pid-file-name (monk-home-directory)))

(defun monk-agent--write-pid-file ()
  "Write pid, socket, owner, and build so a session can find this agent."
  (with-temp-file (monk-agent-pid-file)
    (insert (monk-event-line `((pid . ,(emacs-pid))
                               (socket . ,(monk-agent-local-socket-path))
                               (owner . ,(monk-owner-string (monk-owner)))
                               (host . ,(system-name))
                               (version . ,monk-version)
                               (build . ,monk-build)
                               (started . ,(monk-event--time)))))))

(defun monk-agent-read-pid-file ()
  "Return the pid file as an alist when the agent it names is alive, else nil.
A stale file from a crashed agent is removed."
  (let ((file (monk-agent-pid-file)))
    (when (file-readable-p file)
      (let* ((info (with-temp-buffer (insert-file-contents file)
                                     (monk-event-parse (string-trim (buffer-string)))))
             (pid (alist-get 'pid info)))
        (if (and pid (process-attributes pid))
            info
          (ignore-errors (delete-file file))
          nil)))))

(defun monk-agent--connection-filter (proc chunk)
  "Handle CHUNK from PROC; each complete line in it is one JSON request."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk)))
    (while (string-match "\n" buffer)
      (let ((line (substring buffer 0 (match-beginning 0))))
        (setq buffer (substring buffer (match-end 0)))
        (let ((monk-agent--respond-to proc))
          (condition-case err
              (let ((request (monk-event-parse line)))
                (if request
                    (monk-agent-handle request)
                  (monk-agent--respond '((ok . :false) (error . "expected one JSON object")))))
            (error (monk-agent--respond `((ok . :false) (error . ,(error-message-string err)))))))))
    (process-put proc 'monk-partial buffer)))

(defun monk-agent--connection-sentinel (proc _event)
  "Forget PROC when it closes."
  (unless (process-live-p proc)
    (setq monk-agent--subscribers (delq proc monk-agent--subscribers))))

(defun monk-agent-serve-local ()
  "Start serving the local socket.  Return the server process, or nil.
Nil when this Emacs has no local sockets.  A stale socket file from
an earlier agent is replaced."
  (let ((path (monk-agent-local-socket-path)))
    (make-directory (file-name-directory path) t)
    (when (file-exists-p path) (delete-file path))
    (condition-case err
        (let ((server (make-network-process
                       :name "monk-agent-local" :server t :family 'local :service path
                       :noquery t :coding 'utf-8-unix
                       :filter #'monk-agent--connection-filter
                       :sentinel #'monk-agent--connection-sentinel)))
          (set-file-modes path #o600)
          (setq monk-agent--server server)
          (add-hook 'monk-progress-functions #'monk-agent--broadcast-progress)
          (add-hook 'monk-watch-functions #'monk-agent--broadcast-watch)
          (monk-agent--write-pid-file)
          (monk-log 'info "agent socket %s" path)
          server)
      (error (monk-log 'warn "agent socket: %s" (error-message-string err)) nil))))

(defun monk-agent--broadcast-watch (kind data)
  "Watch hook: forward a drift KIND with DATA to subscribers."
  (when monk-agent--subscribers
    (pcase kind
      ('drift (monk-agent--broadcast (monk-event-drift (plist-get data :key) (plist-get data :path)
                                                       (plist-get data :action))))
      (_ nil))))

(defun monk-agent-stop-local ()
  "Stop the local socket server and remove its files."
  (remove-hook 'monk-progress-functions #'monk-agent--broadcast-progress)
  (remove-hook 'monk-watch-functions #'monk-agent--broadcast-watch)
  (dolist (proc monk-agent--subscribers) (ignore-errors (delete-process proc)))
  (setq monk-agent--subscribers nil)
  (when monk-agent--server
    (ignore-errors (delete-process monk-agent--server))
    (setq monk-agent--server nil))
  (ignore-errors (delete-file (monk-agent-local-socket-path)))
  (ignore-errors (delete-file (monk-agent-pid-file))))

(defun monk-agent-handle (request)
  "Handle REQUEST, an alist from one JSON line.  Write the response to stdout."
  (let ((op (alist-get 'op request)))
    (pcase op
      ("status"
       (monk-agent--respond (append '((op . "status") (ok . t)) (monk-agent-status-alist))))
      ("tail"
       (dolist (line (monk-agent--ledger-tail (or (alist-get 'n request) monk-agent-tail-default)))
         (princ line) (princ "\n"))
       (monk-agent--respond `((op . "tail") (ok . t))))
      ((or "history-runs" "history-results")
       (require 'monk-history)
       (let* ((filters (monk-history-filters-from-json request))
              (rows (if (equal op "history-runs")
                        (apply #'monk-history-runs filters)
                      (apply #'monk-history-results filters))))
         (monk-agent--respond `((op . ,op) (ok . t)
                                (count . ,(length rows))
                                (rows . ,(monk-history-rows-json rows))))))
      ("subscribe"
       ;; From now on every event of this agent goes to the connection,
       ;; until it closes.  The reply says who the agent is, so the
       ;; subscriber can show it.
       (when monk-agent--respond-to
         (cl-pushnew monk-agent--respond-to monk-agent--subscribers))
       (monk-agent--respond (append '((op . "subscribe") (ok . t)) (monk-agent-status-alist))))
      ("watch-mode"
       (let ((mode (alist-get 'mode request)))
         (if (and (fboundp 'monk-watch-active-p) (monk-watch-active-p))
             (progn
               (if (member mode '("assisted" "autopilot"))
                   (setq monk-watch-mode (intern mode))
                 (monk-watch-toggle-mode))
               (monk-agent--respond `((op . "watch-mode") (ok . t) (mode . ,(symbol-name monk-watch-mode)))))
           (monk-agent--respond '((op . "watch-mode") (ok . :false) (error . "no watch is active"))))))
      ("watch-log"
       (let ((lines (and (get-buffer monk-watch-log-buffer-name)
                         (with-current-buffer monk-watch-log-buffer-name
                           (split-string (buffer-string) "\n" t)))))
         (monk-agent--respond `((op . "watch-log") (ok . t)
                                (lines . ,(vconcat (last lines (or (alist-get 'n request) monk-agent-tail-default))))))))
      ((or "plan" "run")
       (let* ((policy (or monk-batch-policy
                          (signal 'monk-policy-error (list "monk-batch-policy is not set"))))
              (monk-report-function #'monk-agent--report)
              (live nil)
              (monk-progress-functions
               (cons (lambda (event data)
                       (pcase event
                         ('run-start (setq live (plist-get data :run)))
                         ('node-end
                          (monk-agent--respond
                           (monk-event-result (plist-get data :result) live nil)))))
                     monk-progress-functions))
              (run (monk-converge (monk-agent-machine) policy :dry-run (equal op "plan"))))
         (monk-run-report run)
         (monk-agent--respond (monk-event-run 'run-end run))))
      (_
       (monk-agent--respond `((op . ,(or op "")) (ok . :false)
                              (error . ,(format "unknown op; expected one of %s"
                                                (string-join monk-agent-ops ", ")))))))))

(defun monk-agent-serve ()
  "Serve one connection on stdin and stdout, then exit.
This is the ExecStart of monk-agent@.service.  One request line in,
JSON lines out.  Errors become a response with ok false and exit 0,
so systemd does not mark the instance failed for a bad request."
  (let ((line (condition-case nil (read-string "") (error nil))))
    (condition-case err
        (let ((request (and line (monk-event-parse line))))
          (if request
              (monk-agent-handle request)
            (monk-agent--respond `((ok . :false) (error . "expected one JSON object")))))
      (error
       (monk-agent--respond `((ok . :false) (error . ,(error-message-string err))))))
    (kill-emacs 0)))

;;;; Observation from a controller

(defvar monk-agent-observe-functions nil
  "Functions called with one parsed event alist per ledger line.")

(defun monk-agent--observe-filter (proc chunk)
  "Filter for PROC: split CHUNK into lines and dispatch complete ones."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk))
        (lines nil))
    (while (string-match "\n" buffer)
      (push (substring buffer 0 (match-beginning 0)) lines)
      (setq buffer (substring buffer (match-end 0))))
    (process-put proc 'monk-partial buffer)
    (dolist (line (nreverse lines))
      (let ((event (monk-event-parse line)))
        (when event
          (run-hook-with-args 'monk-agent-observe-functions event))))))

(cl-defmethod monk-agent-observe ((m monk-machine) &optional ledger)
  "Tail the ledger of the agent on M and dispatch its events.
LEDGER defaults to `monk-agent-ledger-file' on M.  Return the process;
kill it to stop.  Runs `tail -F' through TRAMP, so it needs no port
and no daemon beyond SSH."
  (let* ((default-directory (monk-machine-default-directory m))
         (file (or ledger monk-agent-ledger-file))
         (proc (start-file-process (format "monk-observe-%s" (oref m name))
                                   (generate-new-buffer
                                    (format " *monk-observe-%s*" (oref m name)))
                                   "tail" "-n" "0" "-F" file)))
    (set-process-filter proc #'monk-agent--observe-filter)
    (set-process-query-on-exit-flag proc nil)
    (monk-log 'info "observing %s:%s" (oref m name) file)
    proc))

(cl-defmethod monk-agent-query ((m monk-machine) request &optional user)
  "Send REQUEST, an alist, to the agent socket on M.  Return the response events.
Uses `socat' on M through TRAMP, so no port is opened.  With USER,
talk to the user instance socket."
  (let* ((socket (if user
                     (concat (or (monk-machine-fact m 'runtime-dir)
                                 (format "/run/user/%s" (monk-exec-string m "id" "-u")))
                             "/monk/agent.sock")
                   monk-agent-socket-path))
         (exec (monk-exec m "socat" "-t" "600" "-" (concat "UNIX-CONNECT:" socket)
                          :input (monk-event-line request)
                          :sudo (not user))))
    (unless (monk-exec-ok-p exec)
      (signal 'monk-exec-error (list (oref m name) "socat" (monk-exec-exit-code exec)
                                     (string-trim (monk-exec-output exec)))))
    (delq nil (mapcar #'monk-event-parse
                      (split-string (monk-exec-output exec) "\n" t)))))

(cl-defmethod monk-agent-status ((m monk-machine) &optional user)
  "Return the status alist of the agent on M, or signal.
With USER, ask the user instance."
  (let ((response (car (last (monk-agent-query m '((op . "status")) user)))))
    (unless (eq t (alist-get 'ok response))
      (signal 'monk-error (list "agent status failed" response)))
    response))

(declare-function monk-history-runs "monk-history")
(declare-function monk-history-results "monk-history")
(declare-function monk-history-filters-from-json "monk-history")
(declare-function monk-history-rows-json "monk-history")

(cl-defmethod monk-agent-history ((m monk-machine) what &optional filters user)
  "Return history rows from the agent on M.
WHAT is `runs' or `results'.  FILTERS is a plist as for
`monk-history-runs', sent as JSON with underscores.  With USER, ask
the user instance.  Each row is an alist with symbol keys."
  (let* ((request (append `((op . ,(format "history-%s" what)))
                          (cl-loop for (key value) on filters by #'cddr
                                   collect (cons (intern (string-replace
                                                          "-" "_" (substring (symbol-name key) 1)))
                                                 (if (null value) :false value)))))
         (response (car (last (monk-agent-query m request user)))))
    (unless (eq t (alist-get 'ok response))
      (signal 'monk-error (list "agent history failed" response)))
    (append (alist-get 'rows response) nil)))

(cl-defmethod monk-agent-remote-run ((m monk-machine) &optional dry-run user)
  "Ask the agent on M to converge, or to plan with DRY-RUN.  Return a `monk-run'.
With USER, ask the user instance."
  (monk-events-to-run
   (monk-agent-query m `((op . ,(if dry-run "plan" "run"))) user)))

(monk-note-load 'monk-agent)
(provide 'monk-agent)
;;; monk-agent.el ends here
