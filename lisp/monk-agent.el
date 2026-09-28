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

(defcustom monk-agent-ledger-file
  (if (zerop (user-uid))
      "/var/lib/monk/events.ndjson"
    (expand-file-name "monk/events.ndjson"
                      (or (getenv "XDG_STATE_HOME") "~/.local/state")))
  "Where the agent appends its events."
  :type 'file)

(defcustom monk-agent-full-interval 3600
  "Seconds between full converges.
Drift watching covers owned paths; a full converge covers the rest:
packages, cron, services, and paths whose parent did not exist yet."
  :type 'number)

(defconst monk-agent-socket-path "/run/monk/agent.sock"
  "Unix socket the query endpoint listens on.")

(defconst monk-agent-user-socket-path "%t/monk/agent.sock"
  "Unix socket for a user instance.  %t is XDG_RUNTIME_DIR under systemd.")

(defconst monk-agent-service-name "monk-agent.service")
(defconst monk-agent-launchd-label "org.monk.agent"
  "Label of the launchd job that runs the agent loop on macOS.")
(defconst monk-agent-socket-name "monk-agent.socket")
(defconst monk-agent-template-name "monk-agent@.service")

(defconst monk-agent-ops '("status" "tail" "plan" "run" "history-runs" "history-results")
  "Operations the query endpoint accepts.")

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
  (let ((file (expand-file-name monk-agent-ledger-file)))
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

(defun monk-agent--report (run &optional _verbose)
  "The agent's report function: record RUN, publish it, print one line."
  (monk-agent-record run)
  (monk-agent--etcd (lambda () (monk-etcd-publish-run run)))
  (monk-agent--announce run)
  (message "monk-agent: %s" (monk-run-summary run)))

;;;; Loop

(defun monk-agent--full-converge (policy)
  "Converge the whole local POLICY and record it."
  (let ((monk-watch--running t))
    (monk-run-report (monk-converge (monk-local-machine) policy))))

(defun monk-agent-stop ()
  "Make the agent loop return after the current iteration."
  (setq monk-agent--running nil))

(defun monk-agent-main ()
  "Run the standalone agent until `monk-agent-stop' or a signal.
Uses `monk-batch-policy', set by the policy file loaded before this."
  (let ((policy (or monk-batch-policy
                    (signal 'monk-policy-error (list "monk-batch-policy is not set"))))
        (monk-report-function #'monk-agent--report)
        (monk-watch-mode 'autopilot)
        (m (monk-local-machine)))
    (setq monk-agent--running t)
    (monk-log 'info "agent start on %s, ledger %s" (oref m name) monk-agent-ledger-file)
    (monk-agent--full-converge policy)
    (monk-watch-start m policy)
    (setq monk-agent--full-timer
          (run-at-time monk-agent-full-interval monk-agent-full-interval
                       #'monk-agent--full-converge policy))
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
        (while monk-agent--running
          ;; Timers and file notifications fire inside this wait.
          (accept-process-output nil monk-agent-loop-interval))
      (dolist (timer (list monk-agent--full-timer monk-agent--etcd-timer))
        (when timer (cancel-timer timer)))
      (setq monk-agent--full-timer nil
            monk-agent--etcd-timer nil)
      (monk-watch-stop)
      (monk-log 'info "agent stop"))))

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

(defun monk-agent--respond (object)
  "Write OBJECT as one JSON line to stdout."
  (princ (monk-event-line object)))

(defun monk-agent--ledger-tail (n)
  "Return the last N lines of the ledger as strings, oldest first."
  (let ((file (expand-file-name monk-agent-ledger-file)))
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

(defun monk-agent-handle (request)
  "Handle REQUEST, an alist from one JSON line.  Write the response to stdout."
  (let ((op (alist-get 'op request)))
    (pcase op
      ("status"
       (monk-agent--respond
        `((op . "status") (ok . t)
          (host . ,(system-name))
          (agent_active . ,(if (monk-agent--service-active-p) t :false))
          (ledger . ,(expand-file-name monk-agent-ledger-file))
          (last_run . ,(monk-agent--last-run-end)))))
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
              (run (monk-converge (monk-local-machine) policy :dry-run (equal op "plan"))))
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

(provide 'monk-agent)
;;; monk-agent.el ends here
