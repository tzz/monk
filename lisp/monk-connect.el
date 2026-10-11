;;; monk-connect.el --- A session as a window onto a running agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of Monk.
;;
;; Monk is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Monk is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Monk.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The watch belongs with the files, in the one long-lived agent.  A
;; session that also watched would react to the same edits; two runs,
;; two reports.  So the session connects instead: `monk-connect' opens
;; the agent's local socket, subscribes, and from then on the agent's
;; runs show in `*monk*' as they happen, with the spinner, as a local
;; run would.  The header carries an AGENT badge with the agent's
;; owner, uptime, watch mode, and next full converge.  In that state
;; the usual keys steer the agent: `g' asks it to plan, `a' to apply,
;; `W' flips its watch mode, `w' shows its watch log.  `monk-watch'
;; refuses to start locally while connected, and says so.
;;
;; Scope: the socket is a file under the owner's Monk state directory,
;; mode 0600.  A user connects to their own agent; root's agent lives
;; under /etc/monk, and only root reaches it.  There is no way to ask
;; another user's agent anything, which is the rule, not a gap.
;;
;; Sudo resources run in the agent as the agent's user, through the
;; sudo hop; with NOPASSWD they work, without it they fail, and the
;; status says beforehand which it will be (`sudo: cached' or
;; `password').  Whoever pressed `a' asked for that.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'monk)
(require 'monk-event)
(require 'monk-agent)
(require 'monk-ui)

(defgroup monk-connect nil
  "A session connected to a running agent."
  :group 'monk)

(defcustom monk-connect-reply-timeout 600
  "Seconds to wait for an agent's answer to a request that runs a policy."
  :type 'natnum)

(defvar monk-connect--process nil
  "The connection to the agent, or nil.")

(defvar monk-connect--status nil
  "The agent's status alist.
From the subscribe reply, refreshed by `monk-connect-status'.")

(defvar monk-connect--run-events nil
  "Events of the run in progress on the agent, newest first.")

(defvar monk-connect--live-run nil
  "A stand-in `monk-run' for the run in progress on the agent.")

(defvar monk-connect--live-results nil
  "Results received so far for the run in progress.")

(defvar monk-connect--waiting nil
  "A list (OP . CALLBACK) while a steered request waits for its final reply.")

(defvar monk-connect--waiting-timer nil
  "Timer that gives up on a request the agent never answered.")

;;;; Connection

(defun monk-connect-active-p ()
  "Return non-nil when a connection to an agent is open."
  (and monk-connect--process (process-live-p monk-connect--process)))

(defun monk-connect-socket ()
  "Return the socket path of the running agent for this owner, or nil.
Reads the agent's pid file; a stale file means no agent."
  (let ((info (monk-agent-read-pid-file)))
    (and info (alist-get 'socket info))))

;;;###autoload
(defun monk-connect (&optional socket)
  "Connect this session to the running agent; its runs show in `*monk*'.
SOCKET defaults to the agent of this user on this host, found through
its pid file.  `*monk*' opens on the agent's last run, or empty with
the AGENT badge, and is selected: g plans on the agent, a applies, W
flips its watch mode, w shows its log, D disconnects.  Signal
`user-error' when no agent runs."
  (interactive)
  (when (monk-connect-active-p)
    (user-error "Already connected to the agent (%s); M-x monk-disconnect first"
                (alist-get 'host monk-connect--status)))
  (let ((path (or socket (monk-connect-socket)
                  (user-error "No agent is running for %s on this host (no pid file under %s)"
                              (user-login-name) (abbreviate-file-name (monk-home-directory))))))
    (unless (file-exists-p path)
      (user-error "The agent's socket %s is gone; is the agent alive?" path))
    (setq monk-connect--process
          (make-network-process :name "monk-connect" :family 'local :service path
                                :noquery t :coding 'utf-8-unix
                                :filter #'monk-connect--filter
                                :sentinel #'monk-connect--sentinel))
    (setq monk-connect--run-events nil monk-connect--live-run nil
          monk-connect--live-results nil monk-connect--waiting nil monk-connect--status nil)
    (monk-connect--send '((op . "subscribe")))
    ;; the subscribe reply fills the status; wait briefly so the
    ;; message can name the agent
    (let ((deadline (+ (float-time) 3)))
      (while (and (null monk-connect--status) (< (float-time) deadline))
        (accept-process-output monk-connect--process 0.1)))
    (if monk-connect--status
        (progn
          (monk-connect--show-last-run)
          (message "Connected to the agent on %s (%s, %s build %s, watching %s dir(s), sudo %s)"
                   (alist-get 'host monk-connect--status) (alist-get 'owner monk-connect--status)
                   (alist-get 'version monk-connect--status) (alist-get 'build monk-connect--status)
                   (alist-get 'watch_dirs monk-connect--status) (alist-get 'sudo monk-connect--status)))
      (message "Connected to %s; the agent has not answered yet" (abbreviate-file-name path)))
    monk-connect--process))

;;;###autoload
(defun monk-disconnect ()
  "Close the connection to the agent.  `*monk*' keeps the last run."
  (interactive)
  (when monk-connect--process
    (ignore-errors (delete-process monk-connect--process))
    (setq monk-connect--process nil))
  (setq monk-connect--status nil monk-connect--live-run nil monk-connect--waiting nil)
  (when monk-connect--waiting-timer
    (cancel-timer monk-connect--waiting-timer)
    (setq monk-connect--waiting-timer nil))
  (when (get-buffer monk-ui-buffer-name)
    (with-current-buffer monk-ui-buffer-name (force-mode-line-update)))
  (message "Disconnected from the agent"))

(defun monk-connect--send (request)
  "Send REQUEST, an alist, to the agent."
  (unless (monk-connect-active-p)
    (user-error "Not connected to an agent; M-x monk-connect"))
  (process-send-string monk-connect--process (monk-event-line request)))

(defun monk-connect--sentinel (_proc event)
  "The agent went away with EVENT: say so and clear the state."
  (unless (monk-connect-active-p)
    (setq monk-connect--process nil monk-connect--status nil monk-connect--live-run nil)
    (when (get-buffer monk-ui-buffer-name)
      (with-current-buffer monk-ui-buffer-name (force-mode-line-update)))
    (message "Agent connection closed: %s" (string-trim event))))

(defun monk-connect--filter (proc chunk)
  "Split CHUNK into JSON lines from PROC and handle each."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk)))
    (while (string-match "\n" buffer)
      (let ((line (substring buffer 0 (match-beginning 0))))
        (setq buffer (substring buffer (match-end 0)))
        (let ((event (monk-event-parse line)))
          (when event
            (condition-case err
                (monk-connect--handle event)
              (error (monk-log 'warn "connect: %s on %S" (error-message-string err) event)))))))
    (process-put proc 'monk-partial buffer)))

;;;; Events into *monk*

(defun monk-connect--handle (event)
  "Route one EVENT from the agent."
  (let ((op (alist-get 'op event))
        (kind (alist-get 'event event)))
    (cond
     ((equal op "subscribe")
      (setq monk-connect--status event))
     ((equal op "status")
      (setq monk-connect--status event)
      (monk-connect--refresh-header))
     ((member op '("watch-mode" "watch-log" "history-runs" "history-results"))
      (monk-connect--reply op event))
     ((and op (not (eq t (alist-get 'ok event))))
      (message "Agent: %s failed: %s" op (alist-get 'error event)))
     ((equal kind "run-start") (monk-connect--run-start event))
     ((equal kind "result") (monk-connect--result event))
     ((equal kind "run-end") (monk-connect--run-end event))
     ((equal kind "drift")
      (message "Agent: drift %s %s" (alist-get 'action event) (alist-get 'path event))))))

(defun monk-connect--run-start (event)
  "The agent began a run, per EVENT: open a live row in `*monk*'."
  (let* ((hosts (append (alist-get 'hosts event) nil))
         (run (monk-run-create :machines (mapcar (lambda (h) (monk-machine :name h)) hosts)
                               :dry-run (eq t (alist-get 'dry_run event))
                               :passes 0
                               :start-time (monk-run-id-time (alist-get 'run event)))))
    (setq monk-connect--live-run run
          monk-connect--live-results nil
          monk-connect--run-events (list event))
    (unless noninteractive
      (with-current-buffer (monk-ui--buffer)
        (setq monk-ui--run run)
        (monk-ui-reset-cache)
        (monk-ui-refresh)
        (display-buffer (current-buffer))
        (monk-ui--spinner-start)))))

(defun monk-connect--result (event)
  "A result EVENT arrived: add it to the live run and redraw."
  (push event monk-connect--run-events)
  (when monk-connect--live-run
    (let ((result (monk-event-to-result event)))
      (setf (monk-result-scope result) (and (alist-get 'scope event) (intern (alist-get 'scope event))))
      (push result monk-connect--live-results)
      (setf (monk-run-results monk-connect--live-run) (reverse monk-connect--live-results))
      (unless noninteractive
        (with-current-buffer (monk-ui--buffer)
          (setq monk-ui--spinner-index (1+ monk-ui--spinner-index))
          (monk-ui-reset-cache)
          (monk-ui-refresh))))))

(defun monk-connect--run-end (event)
  "The run finished with EVENT: rebuild it from its events and show it.
A request waiting for this run gets it."
  (push event monk-connect--run-events)
  (let ((run (monk-events-to-run (reverse monk-connect--run-events))))
    (setq monk-connect--live-run nil monk-connect--live-results nil)
    (unless noninteractive
      (monk-ui--spinner-stop)
      (with-current-buffer (monk-ui--buffer)
        (setq monk-ui--run run)
        (monk-ui-reset-cache)
        (monk-ui-refresh)
        (force-mode-line-update)))
    ;; Any finished run settles a waiting plan or run request: the
    ;; agent serializes requests, so the run that ends is the one asked
    ;; for, or a queued one that supersedes it.  Matching on dry-run
    ;; left `a' after `g' waiting forever (2026-10-11).
    (when (and monk-connect--waiting (member (car monk-connect--waiting) '("plan" "run")))
      (monk-connect--reply (car monk-connect--waiting) run))
    (message "Agent: %s" (monk-run-summary run))))

(defun monk-connect--reply (op value)
  "Hand VALUE to the callback waiting for OP, if any."
  (when (and monk-connect--waiting (equal (car monk-connect--waiting) op))
    (let ((callback (cdr monk-connect--waiting)))
      (setq monk-connect--waiting nil)
      (when monk-connect--waiting-timer
        (cancel-timer monk-connect--waiting-timer)
        (setq monk-connect--waiting-timer nil))
      (funcall callback value))))

(defun monk-connect--give-up (op)
  "Stop waiting for OP after `monk-connect-reply-timeout'; say so."
  (when (and monk-connect--waiting (equal (car monk-connect--waiting) op))
    (setq monk-connect--waiting nil monk-connect--waiting-timer nil)
    (message "Agent: no answer to %s after %ds; the request is dropped here (the agent may still be working)"
             op monk-connect-reply-timeout)))

(defun monk-connect--show-last-run ()
  "Open `*monk*' on the agent: its last run from the history, else an empty report.
The buffer is selected, so the agent keys work at once."
  (unless noninteractive
    (let* ((host (alist-get 'host monk-connect--status))
           (run (and (featurep 'monk-history) host
                     (let ((row (car (monk-history-runs :host host :limit 1))))
                       (and row (monk-history-run (alist-get 'id row))))))
           (buffer (monk-ui-show
                    (or run
                        (monk-run-create :machines (list (monk-machine :name (or host (system-name))))
                                         :passes 0 :dry-run t)))))
      (pop-to-buffer buffer)
      (when (monk-ui-select-window-p (with-current-buffer buffer monk-ui--run))
        (select-window (get-buffer-window buffer)))))
  (monk-connect--refresh-header))

(defun monk-connect--refresh-header ()
  "Redraw the `*monk*' header, which carries the AGENT badge."
  (when (get-buffer monk-ui-buffer-name)
    (with-current-buffer monk-ui-buffer-name (force-mode-line-update))))

;;;; Steering

(defun monk-connect--client-name ()
  "Return how this session names itself to the agent: `emacs pid N (USER@HOST)'."
  (format "emacs pid %d (%s@%s)" (emacs-pid) (user-login-name) (system-name)))

(defun monk-connect-request (op &optional extra callback)
  "Send OP with EXTRA alist pairs to the agent.
CALLBACK gets the final reply.  One steered request at a time."
  (when monk-connect--waiting
    (user-error "The agent is still answering %s (C-u %s to drop that and send anyway)"
                (car monk-connect--waiting) (substitute-command-keys "\\[monk-connect-drop-request]")))
  (setq monk-connect--waiting (cons op (or callback #'ignore)))
  (when monk-connect--waiting-timer (cancel-timer monk-connect--waiting-timer))
  (setq monk-connect--waiting-timer
        (run-at-time monk-connect-reply-timeout nil #'monk-connect--give-up op))
  (monk-connect--send (append (list (cons 'op op) (cons 'client (monk-connect--client-name))) extra)))

(defun monk-connect-drop-request ()
  "Forget the request this session is waiting on, so another can be sent."
  (interactive)
  (if (null monk-connect--waiting)
      (message "Not waiting on the agent")
    (message "Dropped the wait for %s" (car monk-connect--waiting))
    (setq monk-connect--waiting nil)
    (when monk-connect--waiting-timer
      (cancel-timer monk-connect--waiting-timer)
      (setq monk-connect--waiting-timer nil))))

(defun monk-connect-plan ()
  "Ask the agent to plan its policy; the plan shows in `*monk*'."
  (interactive)
  (monk-connect-request "plan" nil (lambda (_run) (message "Agent plan ready; a applies it on the agent"))))

(defun monk-connect-run ()
  "Ask the agent to converge its policy now, after confirmation."
  (interactive)
  (let ((sudo (alist-get 'sudo monk-connect--status)))
    (when (yes-or-no-p (format "Apply on the agent (%s, sudo %s)? "
                               (alist-get 'host monk-connect--status) sudo))
      (monk-connect-request "run"))))

(defun monk-connect-toggle-watch-mode ()
  "Flip the agent's watch between assisted and autopilot."
  (interactive)
  (monk-connect-request "watch-mode" nil
                        (lambda (reply)
                          (setf (alist-get 'watch_mode monk-connect--status) (alist-get 'mode reply))
                          (monk-connect--refresh-header)
                          (message "Agent watch: %s" (alist-get 'mode reply)))))

(defun monk-connect-watch-log ()
  "Show the agent's watch log in `*monk-watch*'."
  (interactive)
  (monk-connect-request "watch-log" nil
                        (lambda (reply)
                          (with-current-buffer (get-buffer-create monk-watch-log-buffer-name)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert (format ";; agent on %s\n" (alist-get 'host monk-connect--status)))
                              (dolist (line (append (alist-get 'lines reply) nil))
                                (insert line "\n")))
                            (special-mode)
                            (goto-char (point-max))
                            (pop-to-buffer (current-buffer))))))

(defun monk-connect-status (&optional callback)
  "Ask the agent for its status; CALLBACK gets the alist.
Interactively, show it."
  (interactive)
  (monk-connect--send '((op . "status")))
  (when (or callback (called-interactively-p 'any))
    (let ((deadline (+ (float-time) 3)) (before monk-connect--status))
      (while (and (eq before monk-connect--status) (< (float-time) deadline))
        (accept-process-output monk-connect--process 0.1))
      (if callback
          (funcall callback monk-connect--status)
        (message "%s" (monk-connect-status-line))))))

(defun monk-connect-status-line ()
  "Return one line about the connected agent, for the header and messages."
  (let ((s monk-connect--status))
    (if (null s)
        "AGENT (no status yet)"
      (format "AGENT %s  %s  %s build %s  %s%s  sudo %s%s"
              (alist-get 'host s) (alist-get 'owner s)
              (alist-get 'version s) (alist-get 'build s)
              (if (eq t (alist-get 'watching s))
                  (format "%s over %s dir(s), %s run(s)" (alist-get 'watch_mode s)
                          (alist-get 'watch_dirs s) (alist-get 'watch_runs s))
                "not watching")
              (if (alist-get 'watch_started s)
                  (format ", up %s" (monk-format-duration
                                     (float-time (time-since (parse-iso8601-time-string (alist-get 'watch_started s)))) t))
                "")
              (alist-get 'sudo s)
              (if (alist-get 'next_full s)
                  (format "  full in %s" (monk-format-duration
                                          (float-time (time-subtract (parse-iso8601-time-string (alist-get 'next_full s))
                                                                     (current-time)))))
                "")))))

;;;; Hooks into the UI and the watch

(defun monk-connect--ui-badge ()
  "Return the AGENT badge for the `*monk*' header, or an empty string."
  (if (monk-connect-active-p)
      (concat (propertize " AGENT " 'face 'monk-ui-watch
                          'help-echo (concat (monk-connect-status-line)
                                             "\ng plan on the agent, a apply, W watch mode, w watch log, D disconnect"))
              " ")
    ""))

(defun monk-connect--refuse-local-watch (&rest _)
  "Advice for `monk-watch-start': a connected session does not watch."
  (when (monk-connect-active-p)
    (user-error "This session is connected to the agent on %s, which watches; M-x monk-disconnect to watch here"
                (alist-get 'host monk-connect--status))))

(advice-add 'monk-watch-start :before #'monk-connect--refuse-local-watch)

(monk-note-load 'monk-connect)
(provide 'monk-connect)
;;; monk-connect.el ends here
