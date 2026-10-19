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
(defvar monk-reload-last-failures)
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
                           "subscribe" "watch-mode" "watch-log" "machine")
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

(defvar monk-agent--run-origin nil
  "Why the run in progress runs: a short string for the summary line.
`timed', `start', `drift', `source edit', `SIGUSR1', or `plan for
pid N' / `apply for pid N' when a connected session asked.  Bound
around each run by its starter; nil means the run came from the watch.")

(defun monk-agent--report (run &optional _verbose)
  "The agent's report function: record RUN, publish it, print the summary.
The line names the run's origin, so a run a session asked for over the
socket does not read like a timer run.  Changed and failed results were
printed as they finished by `monk-cli'; on a terminal they scrolled
above the status line."
  (monk-agent-record run)
  (monk-agent--etcd (lambda () (monk-etcd-publish-run run)))
  (monk-agent--announce run)
  (unless (and monk-agent-print-changes (memq #'monk-cli--progress monk-progress-functions))
    (when monk-agent-print-changes
      (dolist (r (monk-run-results run))
        (when (or (monk-result-changed-p r) (monk-result-failed-p r))
          (monk-cli-print-result r)))))
  (monk-cli-print-line (format "%s %s  %s%s%s" (monk-cli--color "1" "monk-agent")
                               (format-time-string "%T")
                               (monk-cli-summary-line run)
                               (monk-cli--color "90" (format "  (%s)" (or monk-agent--run-origin
                                                                          (if (monk-run-reasons run) "drift" "run"))))
                               (monk-agent--stale-note))))

(defvar monk-agent--reload-failed nil
  "Non-nil after a reload attempt failed; cleared by one that succeeds.
The stale note says \"reload failed\" only when this is set: source
newer than the process is otherwise just an edit the next converge
will pick up (an edit seconds after start once read as a failure).")

(defun monk-agent--stale-note ()
  "Return a note when this agent runs code older than its source.
Says whether a reload failed, or whether the edit is simply pending."
  (let ((stale (monk-stale-libraries)))
    (cond
     ((null stale) "")
     (monk-agent--reload-failed
      (monk-cli--color "31" (format "  STALE CODE: %s (reload failed; see above)"
                                    (mapconcat (lambda (c) (symbol-name (car c))) stale ", "))))
     (t (monk-cli--color "90" (format "  source edited: %s (reload at the next converge)"
                                      (mapconcat (lambda (c) (symbol-name (car c))) stale ", ")))))))

;;;; Following the source
;;
;; Two ways to make the agent pick up a change before the hourly
;; converge.  (a) A source watch: filenotify on the Monk lisp directory
;; and on the policy file's directory; an edit there, debounced,
;; runs the same reload-and-converge as the timer.  (b) SIGUSR1: the
;; same, on demand, from a shell (`kill -USR1 PID') or from a session
;; (`monk-agent-poke').  Both are the timed path, so the run line says
;; what was reloaded.

(defcustom monk-agent-source-watch t
  "When non-nil, the agent watches its own source and reloads on edits.
The Monk Lisp directory and the policy file's directory are watched;
a change there runs the timed converge after `monk-agent-source-debounce'."
  :type 'boolean)

(defcustom monk-agent-source-debounce 2.0
  "Seconds of quiet after a source edit before the agent reloads.
An editor saves several files in a burst; one reload covers them."
  :type 'number)

(defvar monk-agent--source-descriptors nil
  "Filenotify descriptors of the source watch, when that backend is used.")

(defvar monk-agent--source-process nil
  "The fswatch process of the source watch, when that backend is used.")

(defvar monk-agent--source-dirs nil
  "The real directories the source watch was started on.")

(defvar monk-agent--source-repath-timer nil
  "Timer that re-resolves the source directories; see `monk-watch-repath-interval'.")

(defvar monk-agent--source-backend nil
  "Which backend watches the source.
`fswatch', `filenotify-files', `filenotify', or nil for none.")

(defvar monk-agent--source-timer nil
  "The debounce timer of the source watch, while pending.")

(defvar monk-agent--policy nil
  "The policy the agent runs.
For a converge started by a signal or a source edit.")

(defun monk-agent-source-directories ()
  "Return the directories the source watch covers.
Monk's Lisp directory and the policy file's directory."
  ;; Real paths: the tree is often reached through a symlink
  ;; (~/source/emacs/monk -> ~/ai_workspace/source/monk), and FSEvents
  ;; reports under the real directory while kqueue watches the inode.
  (delete-dups
   (mapcar #'file-truename
           (delq nil (list (let ((f (locate-library "monk"))) (and f (file-name-directory f)))
                           (and monk-agent-policy-file (file-name-directory monk-agent-policy-file)))))))

(defun monk-agent--source-event (event)
  "Handle filenotify EVENT under a source directory.
A changed .el file schedules a reload after the debounce."
  (let ((file (nth 2 event)))
    (when (and (stringp file) (string-suffix-p ".el" file)
               (memq (nth 1 event) '(changed created renamed attribute-changed)))
      (when monk-agent--source-timer (cancel-timer monk-agent--source-timer))
      (setq monk-agent--source-timer
            (run-at-time monk-agent-source-debounce nil #'monk-agent--source-fire file)))))

(defun monk-agent--source-fire (file)
  "The debounce ended after an edit to FILE: reload and converge.
The same path as the hourly timer."
  (setq monk-agent--source-timer nil)
  (monk-log 'info "source edit: %s" (abbreviate-file-name file))
  (monk-cli-print-line (format "%s source edit %s" (monk-cli--color "1" "monk-agent")
                               (abbreviate-file-name file)))
  (let ((monk-agent--run-origin "source edit"))
    (monk-agent--full-converge monk-agent--policy t)))

(defun monk-agent--source-fswatch-filter (proc chunk)
  "Feed NUL-separated paths in CHUNK from fswatch PROC to the event handler."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk)))
    (while (string-match "\0" buffer)
      (let ((path (substring buffer 0 (match-beginning 0))))
        (setq buffer (substring buffer (match-end 0)))
        (unless (string-empty-p path)
          (monk-agent--source-event (list proc 'changed path)))))
    (process-put proc 'monk-partial buffer)))

(defun monk-agent-source-watch-start ()
  "Watch the source directories with the same backend the drift watch uses.
fswatch on macOS when present: FSEvents reports writes, which kqueue
through filenotify does not for files under a watched directory; on
kqueue every .el file is watched by name as well.  Return the backend
in use, nil when nothing could be watched."
  (monk-agent-source-watch-stop)
  (let ((dirs (monk-agent-source-directories))
        (backend (monk-watch--choose-backend)))
    (setq monk-agent--source-dirs dirs)
    ;; the kernel watches where the symlinks pointed at start; look
    ;; again now and then, as the drift watch does
    (when monk-watch-repath-interval
      (setq monk-agent--source-repath-timer
            (run-at-time monk-watch-repath-interval monk-watch-repath-interval
                         #'monk-agent--source-repath)))
    (pcase backend
      ('fswatch
       (let ((proc (apply #'start-process "monk-source-fswatch" nil monk-watch-fswatch-program
                          (append monk-watch-fswatch-args
                                  (mapcan (lambda (re) (list "--exclude" re)) monk-watch-fswatch-excludes)
                                  (mapcar #'directory-file-name dirs)))))
         (set-process-filter proc #'monk-agent--source-fswatch-filter)
         (set-process-query-on-exit-flag proc nil)
         (setq monk-agent--source-process proc
               monk-agent--source-backend 'fswatch)))
      ('filenotify
       (require 'filenotify)
       (let ((files (and (monk-watch-file-watch-needed-p)
                         (mapcan (lambda (dir) (directory-files dir t "\\.el\\'")) dirs))))
         (dolist (path (append dirs files))
           (condition-case err
               (push (file-notify-add-watch path '(change attribute-change) #'monk-agent--source-event)
                     monk-agent--source-descriptors)
             (file-notify-error (monk-log 'warn "source watch on %s: %s" path (error-message-string err)))))
         (when monk-agent--source-descriptors
           (setq monk-agent--source-backend (if files 'filenotify-files 'filenotify)))))
      (_ nil))
    (when monk-agent--source-backend
      (monk-log 'info "source watch (%s) on %s" monk-agent--source-backend
                (mapconcat #'abbreviate-file-name dirs ", ")))
    monk-agent--source-backend))

(defun monk-agent--source-repath ()
  "Resolve the source directories again; when they moved, watch the new ones."
  (unless (equal (monk-agent-source-directories) monk-agent--source-dirs)
    (monk-cli-print-line (format "%s source tree moved to %s; watching again"
                                 (monk-cli--color "1" "monk-agent")
                                 (mapconcat #'abbreviate-file-name (monk-agent-source-directories) ", ")))
    (monk-agent-source-watch-start)))

(defun monk-agent-source-watch-stop ()
  "Stop the source watch."
  (when monk-agent--source-repath-timer
    (cancel-timer monk-agent--source-repath-timer)
    (setq monk-agent--source-repath-timer nil))
  (dolist (desc monk-agent--source-descriptors)
    (ignore-errors (file-notify-rm-watch desc)))
  (setq monk-agent--source-descriptors nil)
  (when monk-agent--source-process
    (ignore-errors (delete-process monk-agent--source-process))
    (setq monk-agent--source-process nil))
  (setq monk-agent--source-backend nil)
  (when monk-agent--source-timer
    (cancel-timer monk-agent--source-timer)
    (setq monk-agent--source-timer nil)))

(defun monk-agent--on-sigusr1 ()
  "SIGUSR1: reload what changed and converge now, as the timer would."
  (interactive)
  (monk-cli-print-line (format "%s SIGUSR1: converging now" (monk-cli--color "1" "monk-agent")))
  (let ((monk-agent--run-origin "SIGUSR1"))
    (monk-agent--full-converge monk-agent--policy t)))

(defun monk-agent-poke (&optional pid)
  "Make the agent of this user converge now, with a reload first.
PID defaults to the one in the pid file; without a pid file, the
Emacs processes running `monk-agent-main' on this host are tried.
Works before the agent serves its socket, which is the point."
  (interactive)
  (let ((pid (or pid
                 (alist-get 'pid (monk-agent-read-pid-file))
                 (car (delq nil (mapcar (lambda (p)
                                          (and (string-match-p "monk-agent-main\\|monk-tzz-agent\\|monk-tzz-sync-agent"
                                                               (or (alist-get 'args (process-attributes p)) ""))
                                               p))
                                        (list-system-processes))))
                 (user-error "No Monk agent process found for %s on this host" (user-login-name)))))
    (signal-process pid 'sigusr1)
    (message "Sent SIGUSR1 to the agent (pid %d); it reloads and converges now" pid)
    pid))

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
          (error (monk-cli-print-line (format "%s loading %s failed: %s" (monk-cli--color "31" "monk-agent")
                                              feature (error-message-string err)))))))
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
          (progn
            ;; `monk-reload' logs its summary (which the CLI prints as a
            ;; line) and also `message's it.  In batch that message goes
            ;; to stderr behind the terminal's back and lands after the
            ;; idle line, so it is dropped here; the format is "%s", so
            ;; the test is on the text.  Errors must stay visible, so
            ;; the message is caught, not inhibited (an `inhibit-message'
            ;; here swallowed the reason for a failed reload, 2026-10-11).
            (cl-letf* ((real (symbol-function 'message))
                       ((symbol-function 'message)
                        (lambda (fmt &rest args)
                          (if (and fmt (string-prefix-p "monk: " (apply #'format-message fmt args)))
                              nil
                            (apply real fmt args)))))
              (monk-reload))
            (if monk-reload-last-failures
                (progn
                  (setq monk-agent--reload-failed t)
                  (monk-cli-print-line
                   (format "%s reload: %s" (monk-cli--color "31" "monk-agent")
                           (mapconcat (lambda (f) (format "%s: %s" (car f) (cdr f)))
                                      monk-reload-last-failures "; ")))
                  (push (format "reload FAILED for %s" (mapconcat (lambda (f) (symbol-name (car f)))
                                                                  monk-reload-last-failures ", "))
                        notes))
              (setq monk-agent--reload-failed nil)
              (push (format "reloaded %s" (mapconcat (lambda (c) (symbol-name (car c))) stale ", ")) notes)))
        (error
         (setq monk-agent--reload-failed t)
         (monk-cli-print-line (format "%s reload failed: %s" (monk-cli--color "31" "monk-agent")
                                      (error-message-string err)))
         (push "reload FAILED" notes))))
    (let ((new (monk-agent--load-new-libraries)))
      (when new
        (push (format "loaded new %s" (mapconcat #'symbol-name new ", ")) notes)))
    ;; The source watch runs on the code it was started with; after a
    ;; reload of this file its filter is the old closure, so restart it.
    (when (and stale monk-agent-source-watch (fboundp 'monk-agent-source-watch-start))
      (monk-agent-source-watch-start))
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
        (error (monk-cli-print-line (format "%s policy reload failed: %s" (monk-cli--color "31" "monk-agent")
                                            (error-message-string err)))
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

(defvar monk-agent--converging nil
  "Non-nil while a full converge runs.
A signal or edit meanwhile is queued, not nested.")

(defvar monk-agent--converge-again nil
  "Non-nil when a converge was asked for during one; it runs after.")

(defvar monk-agent--respond-to)
(defvar monk-agent--machine)
(defvar monk-agent--queued-requests nil
  "Plan and run requests that arrived while a run was in progress, oldest last.
Each is (REQUEST . CONNECTION).  Served in order once the run ends;
read-only requests never queue, they answer in the middle of a run.")

(defun monk-agent--run-in-progress-p ()
  "Return non-nil while a converge of any kind runs in this agent."
  (or monk-agent--converging monk--current-run))

(defun monk-agent--serve-queued-requests ()
  "Serve the plan and run requests that waited for the run to end."
  (while (and monk-agent--queued-requests (not (monk-agent--run-in-progress-p)))
    (pcase-let ((`(,request . ,connection) (pop monk-agent--queued-requests)))
      (when (or (null connection) (process-live-p connection))
        (let ((monk-agent--respond-to connection))
          (condition-case err
              (monk-agent-handle request)
            (error (monk-agent--respond `((ok . :false) (error . ,(error-message-string err)))))))))))

(defun monk-agent--full-converge (policy &optional timed)
  "Converge the whole local POLICY and record it.
With TIMED, this is a scheduled converge: wait a splay first, update
the policy repo, and when `monk-agent-only-if-changed' is set and the
repo did not move, skip the converge.  A second request while one
runs is remembered and served right after."
  (if (monk-agent--run-in-progress-p)
      ;; also when a socket plan or run is on: `monk--current-run' is
      ;; set for the whole of any run, and timers fire inside monk-exec
      (setq monk-agent--converge-again t)
    (let ((monk-agent--converging t))
      (monk-agent--full-converge-1 policy timed))
    (when monk-agent--converge-again
      (setq monk-agent--converge-again nil)
      (monk-agent--full-converge policy timed))
    (monk-agent--serve-queued-requests)))

(defun monk-agent--full-converge-1 (policy &optional timed)
  "The body of `monk-agent--full-converge'; POLICY and TIMED as there."
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
      (let ((monk-watch--running t)
            (monk-agent--run-origin (or monk-agent--run-origin (if timed "timed" "start"))))
        (monk-run-report (monk-converge (or monk-agent--machine (monk-agent-machine)) policy))))))

(defun monk-agent--idle-text ()
  "Text of the watching line: paths, runs, uptime, next full converge."
  (let ((w monk-watch--current)
        (next (and monk-agent--full-timer
                   (max 0 (round (float-time (time-subtract (timer--time monk-agent--full-timer)
                                                             (current-time))))))))
    (if (null w)
        "watching"
      (format "%s %d dir(s) %d link(s)  %d run(s)  up %s%s  source: %s"
              (if (monk-watch-backend-alive-p w)
                  "watching"
                (monk-cli--color "31" "WATCH DOWN, restart pending;"))
              (length (monk-watch-watched w))
              (length (monk-watch-link-targets (monk-watch-nodes w)))
              (monk-watch-runs w)
              (monk-format-duration (float-time (time-since (monk-watch-started w))) t)
              (if next (format "  full in %s" (monk-format-duration next)) "")
              (or monk-agent--source-backend "off")))))

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
    (setq monk-agent--policy policy)
    (define-key special-event-map [sigusr1] #'monk-agent--on-sigusr1)
    (monk-agent-serve-local)
    (monk-agent--full-converge policy)
    (monk-watch-start m policy)
    (when monk-agent-source-watch (monk-agent-source-watch-start))
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
      (monk-agent-source-watch-stop)
      (define-key special-event-map [sigusr1] #'ignore)
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
      (stale . ,(vconcat (mapcar (lambda (c) (symbol-name (car c))) (monk-stale-libraries))))
      (source_watch . ,(and monk-agent--source-backend (symbol-name monk-agent--source-backend)))
      (load_path . ,(let ((f (locate-library "monk"))) (and f (file-name-directory f))))
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
      ;; the origin rides on run-start so a session can tell a run it
      ;; asked for from one the agent started on its own (and not pop
      ;; its window for the latter, 2026-10-12)
      ('run-start (monk-agent--broadcast
                   (append (monk-event-run 'run-start (plist-get data :run))
                           (and monk-agent--run-origin (list (cons 'origin monk-agent--run-origin))))))
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

(defun monk-agent--connection-sentinel (proc event)
  "Forget PROC when it closes; say who left and why.
A subscriber that drops without `monk-disconnect' is worth a line: it
tells whether the agent closed it (a reload, a restart) or the other
side went away (2026-10-12: sessions lost their agent at every reload
and nothing said which end hung up)."
  (unless (process-live-p proc)
    (when (memq proc monk-agent--subscribers)
      (setq monk-agent--subscribers (delq proc monk-agent--subscribers))
      (monk-cli-print-line (format "%s subscriber %s left: %s (%d remain)"
                                   (monk-cli--color "90" "monk-agent")
                                   (or (process-get proc 'monk-client) "unnamed")
                                   (string-trim event) (length monk-agent--subscribers))))))

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
          (add-hook 'monk-facts-ready-functions #'monk-agent--broadcast-facts)
          (monk-agent--write-pid-file)
          (monk-log 'info "agent socket %s" path)
          server)
      (error (monk-log 'warn "agent socket: %s" (error-message-string err)) nil))))

(defun monk-agent-machine-alist (&optional m)
  "Return M, default the agent's machine, as the `machine' message body.
Facts, classes, groups, vars, where the facts came from and when."
  (let* ((m (or m monk-agent--machine (monk-agent-machine)))
         (description (monk-machine-describe m)))
    `((name . ,(oref m name))
      (classes . ,(vconcat (mapcar #'symbol-name (alist-get 'classes description))))
      (groups . ,(vconcat (mapcar #'symbol-name (alist-get 'groups description))))
      (facts . ,(monk-event--jsonable (alist-get 'facts description)))
      (vars . ,(monk-event--jsonable (alist-get 'vars description)))
      (facts_source . ,(if (oref m facts-source) (symbol-name (oref m facts-source)) "none"))
      (gathered . ,(and (oref m gathered) (monk-event--time (oref m gathered)))))))

(defun monk-agent--broadcast-facts (m)
  "Facts hook: tell subscribers the facts of M changed, with the new facts.
Runs after a live gather and after a cache load; only the agent's own
machine is of interest to a session."
  (when (and monk-agent--subscribers
             (or (eq m monk-agent--machine)
                 (and monk-agent--machine (equal (oref m name) (oref monk-agent--machine name)))))
    (monk-agent--broadcast (append '((event . "machine")) (monk-agent-machine-alist m)))))

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
  (remove-hook 'monk-facts-ready-functions #'monk-agent--broadcast-facts)
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
         (process-put monk-agent--respond-to 'monk-client (alist-get 'client request))
         (cl-pushnew monk-agent--respond-to monk-agent--subscribers)
         (monk-cli-print-line (format "%s subscriber %s joined (%d now)"
                                      (monk-cli--color "90" "monk-agent")
                                      (or (alist-get 'client request) "unnamed")
                                      (length monk-agent--subscribers))))
       ;; the machine rides along; a failure to describe it must not
       ;; cost the subscription, so it is logged and left out
       (monk-agent--respond (append '((op . "subscribe") (ok . t))
                                    (monk-agent-status-alist)
                                    (condition-case err
                                        (list (cons 'machine (monk-agent-machine-alist)))
                                      (error (monk-log 'warn "subscribe: machine not described: %s"
                                                       (error-message-string err))
                                             nil)))))
      ("watch-mode"
       (let ((mode (alist-get 'mode request)))
         (if (and (fboundp 'monk-watch-active-p) (monk-watch-active-p))
             (progn
               (if (member mode '("assisted" "autopilot"))
                   (setq monk-watch-mode (intern mode))
                 (monk-watch-toggle-mode))
               (monk-agent--respond `((op . "watch-mode") (ok . t) (mode . ,(symbol-name monk-watch-mode)))))
           (monk-agent--respond '((op . "watch-mode") (ok . :false) (error . "no watch is active"))))))
      ("machine"
       ;; what guards can see on the agent's machine; also sent with
       ;; the subscribe reply and whenever facts are gathered, so a
       ;; session never has to ask a busy agent (2026-10-12)
       (monk-agent--respond (append '((op . "machine") (ok . t)) (monk-agent-machine-alist))))
      ("watch-log"
       (let ((lines (and (get-buffer monk-watch-log-buffer-name)
                         (with-current-buffer monk-watch-log-buffer-name
                           (split-string (buffer-string) "\n" t)))))
         (monk-agent--respond `((op . "watch-log") (ok . t)
                                (lines . ,(vconcat (last lines (or (alist-get 'n request) monk-agent-tail-default))))))))
      ((and (or "plan" "run") (guard (monk-agent--run-in-progress-p)))
       ;; The socket is served from inside `monk-exec' while a run is
       ;; on, so this request arrived mid-run.  A run inside a run is
       ;; not allowed; it goes to the queue and starts when this one
       ;; ends.  The asker sees the run in progress through the
       ;; broadcast meanwhile.
       (setq monk-agent--queued-requests
             (append monk-agent--queued-requests (list (cons request monk-agent--respond-to))))
       (monk-cli-print-line (format "%s %s from %s queued behind the run in progress"
                                    (monk-cli--color "90" "monk-agent") op
                                    (or (alist-get 'client request) "a client"))))
      ((or "plan" "run")
       ;; A subscriber already receives every event through the
       ;; broadcast; streaming them again on the request would double
       ;; each result (108 for a 55-resource policy, 2026-10-11).  Only
       ;; a one-shot connection, the systemd endpoint or a script, gets
       ;; the stream on the request itself.
       (let* ((policy (or monk-batch-policy
                          (signal 'monk-policy-error (list "monk-batch-policy is not set"))))
              (subscribed (and monk-agent--respond-to
                               (memq monk-agent--respond-to monk-agent--subscribers)))
              (monk-agent--run-origin
               (format "%s for %s" op
                       (or (and (alist-get 'client request) (format "%s" (alist-get 'client request)))
                           (and monk-agent--respond-to "a socket client")
                           "the endpoint")))
              (monk-report-function #'monk-agent--report)
              (live nil)
              (monk-progress-functions
               (if subscribed
                   monk-progress-functions
                 (cons (lambda (event data)
                         (pcase event
                           ('run-start (setq live (plist-get data :run)))
                           ('node-end
                            (monk-agent--respond
                             (monk-event-result (plist-get data :result) live nil)))))
                       monk-progress-functions)))
              (run (monk-converge (monk-agent-machine) policy :dry-run (equal op "plan"))))
         (monk-run-report run)
         (unless subscribed
           (monk-agent--respond (monk-event-run 'run-end run)))
         ;; a source edit or SIGUSR1 that fired during this run
         (when monk-agent--converge-again
           (setq monk-agent--converge-again nil)
           (monk-agent--full-converge monk-agent--policy t))
         (monk-agent--serve-queued-requests)))
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
