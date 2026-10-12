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

(defcustom monk-connect-require-same-build t
  "When non-nil, refuse to connect to an agent on another build than this session.
The two sides speak the same protocol only when they run the same
code; a session on a newer build asked an older agent for an op it
did not have and waited for an hour (2026-10-12).  `monk-reconnect'
pokes the agent to reload and connects once the builds agree."
  :type 'boolean)

(defcustom monk-connect-reload-wait 20
  "Seconds `monk-reconnect' waits for a poked agent to reload before giving up."
  :type 'natnum)

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

(defvar monk-connect--prefix nil
  "TRAMP directory of the host the connection goes to; nil for this host.")

(defvar monk-connect--known-prefixes nil
  "Every PREFIX this session connected to, newest first; nil is this host.
`monk-reconnect-all' walks it.")

(defvar monk-connect--asked nil
  "Non-nil while this session has asked the agent for a plan or run.
The next run-start is then one this session asked for: its window
comes up.  Runs the agent starts on its own only update `*monk*'
when it is already visible (`monk-ui-show-unasked-runs').")

(defvar monk-connect--machine nil
  "The agent's machine as last sent: with the subscribe reply, and again
whenever the agent gathers facts.  `i' reads this; it never asks.")

(defvar monk-connect--lost nil
  "A list (HOST . EVENT) after the agent closed the connection on us.
Nil while connected and after `monk-disconnect'.  While set, `*monk*'
shows a LOST badge and the agent keys offer to reconnect instead of
quietly running the policy here (2026-10-11: a session that lost its
agent ran a 56-resource plan locally on `g').")

;;;; Connection

(defun monk-connect-active-p ()
  "Return non-nil when a connection to an agent is open."
  (and monk-connect--process (process-live-p monk-connect--process)))

(defun monk-connect-socket ()
  "Return the socket path of the running agent for this owner, or nil.
Reads the agent's pid file; a stale file means no agent."
  (let ((info (monk-agent-read-pid-file)))
    (and info (alist-get 'socket info))))

;;;; Targets: this host, or a host reached over TRAMP

(defconst monk-connect-local-label "local"
  "Completion label for the agent on this host.")

(defconst monk-connect-remote-pid-files
  '("~/.local/share/monk/agent.pid" "/etc/monk/agent.pid")
  "Where a remote agent's pid file can be: the owner's XDG home, then root's.")

(defconst monk-connect-pipe-commands
  '(("nc" "-U") ("socat" "-" "UNIX-CONNECT:"))
  "Programs that pipe stdin and stdout to a Unix socket, in preference
order, as (PROGRAM ARG...).  The socket path is appended to the last
argument when it ends in a colon, else added as its own argument.")

(defconst monk-connect-op-labels
  '(("plan" . "a plan") ("run" . "an apply") ("machine" . "the machine's facts")
    ("watch-log" . "the watch log") ("watch-mode" . "the watch mode change")
    ("status" . "its status") ("history-runs" . "the run history")
    ("history-results" . "the result history"))
  "What each request op is called where a person reads it.")

(defun monk-connect-op-label (op)
  "Return the readable name of request OP, for the badge and messages."
  (or (cdr (assoc op monk-connect-op-labels)) op))

(defvar monk-connect-target-history nil
  "Minibuffer history for `monk-connect' targets.")

(defun monk-connect-targets ()
  "Return the connect targets as ((LABEL . PREFIX) ...).
PREFIX is nil for this host, else a TRAMP directory such as
\"/ssh:user@host:\" or \"/rpc:host:\".  Every inventory machine with
an address is a target, with its method; this host comes first."
  (cons (cons monk-connect-local-label nil)
        (delq nil
              (mapcar (lambda (m)
                        (unless (monk-machine-local-p m)
                          (cons (format "%s (%s %s)" (oref m name)
                                        (monk-machine-method m) (oref m address))
                                (monk-machine-remote-prefix m))))
                      (bound-and-true-p monk-inventory)))))

(defun monk-connect-read-target ()
  "Ask which agent to connect to; return its PREFIX (nil for this host).
Offers `monk-connect-targets'; free text is a TRAMP prefix or a
METHOD:HOST pair, so a host outside the inventory works too."
  (let* ((targets (monk-connect-targets))
         (answer (completing-read "Connect to the Monk agent on: " targets nil nil nil
                                  'monk-connect-target-history monk-connect-local-label)))
    (cond
     ((assoc answer targets) (cdr (assoc answer targets)))
     ((string-empty-p answer) nil)
     ((string-prefix-p "/" answer)
      (if (string-suffix-p ":" answer) answer (file-name-as-directory answer)))
     ((string-match "\\`\\([a-z0-9]+\\):\\(.+\\)\\'" answer)
      (format "/%s:%s:" (match-string 1 answer) (match-string 2 answer)))
     (t (format "/%s:%s:" monk-tramp-method answer)))))

(defun monk-connect-remote-socket (prefix)
  "Return the socket path of the agent behind PREFIX, as that host sees it.
Reads the pid files in `monk-connect-remote-pid-files' over TRAMP and
keeps the first whose pid is alive there.  Signal `user-error' when
none is."
  (let ((default-directory prefix) found)
    (dolist (candidate monk-connect-remote-pid-files)
      (let ((file (expand-file-name candidate prefix)))
        (when (and (not found) (file-readable-p file))
          (let* ((info (with-temp-buffer (insert-file-contents file)
                                         (monk-event-parse (string-trim (buffer-string)))))
                 (pid (alist-get 'pid info)))
            (when (and pid (eq 0 (process-file "kill" nil nil nil "-0" (number-to-string pid))))
              (setq found (alist-get 'socket info)))))))
    (or found
        (user-error "No agent is running on %s (no live pid file at %s)"
                    (file-remote-p prefix 'host)
                    (string-join monk-connect-remote-pid-files " or ")))))

(defun monk-connect-pipe-command (prefix socket)
  "Return the command that pipes to SOCKET on the host behind PREFIX.
The first program of `monk-connect-pipe-commands' found there wins."
  (let ((default-directory (or prefix default-directory)) command)
    (dolist (candidate monk-connect-pipe-commands)
      (when (and (not command) (executable-find (car candidate) (and prefix t)))
        (let ((args (cdr candidate)))
          (setq command
                (cons (car candidate)
                      (if (string-suffix-p ":" (car (last args)))
                          (append (butlast args) (list (concat (car (last args)) socket)))
                        (append args (list socket))))))))
    (or command
        (user-error "Neither nc nor socat is on %s; one is needed to reach the agent's socket"
                    (if prefix (file-remote-p prefix 'host) "this host")))))

(defun monk-connect--open (prefix socket)
  "Open the connection to SOCKET on the host behind PREFIX.  Return the process.
This host: a local socket process.  Elsewhere: `nc' or `socat' run
over TRAMP, so the same JSON Lines flow through ssh or rpc."
  (if (null prefix)
      (make-network-process :name "monk-connect" :family 'local :service socket
                            :noquery t :coding 'utf-8-unix
                            :filter #'monk-connect--filter
                            :sentinel #'monk-connect--sentinel)
    (let* ((default-directory prefix)
           (command (monk-connect-pipe-command prefix socket))
           (process-connection-type nil)
           (proc (apply #'start-file-process "monk-connect" nil command)))
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc #'monk-connect--filter)
      (set-process-sentinel proc #'monk-connect--sentinel)
      proc)))

;;;###autoload
(defun monk-connect (&optional socket prefix)
  "Connect this session to a running agent; its runs show in `*monk*'.
Interactively, ask which host: this one, any inventory machine with an
address (over its TRAMP method, ssh or rpc), or a typed METHOD:HOST.
SOCKET is the agent's socket path on that host, found through its pid
file when nil.  PREFIX is the TRAMP directory of the host, nil for
this one.  `*monk*' opens on the agent's last run, or empty with the
AGENT badge, and is selected: g plans on the agent, a applies, W flips
its watch mode, w shows its log, D disconnects.  Signal `user-error'
when no agent runs there.  Already connected: show `*monk*' again."
  (interactive (if (monk-connect-active-p) (list nil nil) (list nil (monk-connect-read-target))))
  (if (monk-connect-active-p)
      ;; already connected: the command is the way back to the window
      (progn
        (monk-connect--show-last-run)
        (message "Connected to the agent on %s; D disconnects"
                 (alist-get 'host monk-connect--status))
        monk-connect--process)
    (monk-connect--open-session socket prefix)))

(defun monk-connect--open-session (socket prefix)
  "Open a new connection; SOCKET and PREFIX as for `monk-connect'."
  (let ((path (or socket
                  (if prefix
                      (monk-connect-remote-socket prefix)
                    (or (monk-connect-socket)
                        (user-error "No agent is running for %s on this host (no pid file under %s)"
                                    (user-login-name) (abbreviate-file-name (monk-home-directory))))))))
    (unless (file-exists-p (if prefix (concat prefix path) path))
      (user-error "The agent's socket %s is gone; is the agent alive?" path))
    (setq monk-connect--process (monk-connect--open prefix path)
          monk-connect--prefix prefix)
    (setq monk-connect--known-prefixes
          (cons prefix (delete prefix monk-connect--known-prefixes)))
    (setq monk-connect--run-events nil monk-connect--live-run nil
          monk-connect--live-results nil monk-connect--waiting nil monk-connect--status nil
          monk-connect--lost nil)
    (monk-connect--send `((op . "subscribe") (client . ,(monk-connect--client-name))))
    ;; the subscribe reply fills the status; wait briefly so the
    ;; message can name the agent
    (let ((deadline (+ (float-time) 3)))
      (while (and (null monk-connect--status) (< (float-time) deadline))
        (accept-process-output monk-connect--process 0.1)))
    (when (and monk-connect--status monk-connect-require-same-build
               (not (equal monk-build (alist-get 'build monk-connect--status))))
      (let ((agent-build (alist-get 'build monk-connect--status))
            (host (alist-get 'host monk-connect--status)))
        (monk-connect--close)
        (user-error "The agent on %s runs build %s, this session %s; M-x monk-reconnect makes it reload and connects"
                    host agent-build monk-build)))
    (if monk-connect--status
        (progn
          (monk-connect--show-last-run)
          (message "Connected to the agent on %s (%s, %s build %s, watching %s dir(s), sudo %s)"
                   (alist-get 'host monk-connect--status) (alist-get 'owner monk-connect--status)
                   (alist-get 'version monk-connect--status) (alist-get 'build monk-connect--status)
                   (alist-get 'watch_dirs monk-connect--status) (alist-get 'sudo monk-connect--status)))
      (message "Connected to %s%s; the agent has not answered yet"
               (if prefix (file-remote-p prefix 'host) "") (abbreviate-file-name path)))
    monk-connect--process))

(defun monk-connect--close ()
  "Close the connection and clear its state, without a message."
  (when monk-connect--process
    (set-process-sentinel monk-connect--process #'ignore)
    (ignore-errors (delete-process monk-connect--process))
    (setq monk-connect--process nil))
  (setq monk-connect--status nil monk-connect--live-run nil monk-connect--waiting nil
        monk-connect--lost nil)
  (when monk-connect--waiting-timer
    (cancel-timer monk-connect--waiting-timer)
    (setq monk-connect--waiting-timer nil)))

(defun monk-connect--poke (prefix)
  "Send SIGUSR1 to the agent behind PREFIX (nil: this host), so it reloads.
Return non-nil when a signal was sent."
  (if (null prefix)
      (let ((info (monk-agent-read-pid-file)))
        (when info
          (signal-process (alist-get 'pid info) 'sigusr1)
          t))
    (let* ((default-directory prefix)
           (pid (monk-connect--remote-pid prefix)))
      (and pid (eq 0 (process-file "kill" nil nil nil "-USR1" (number-to-string pid)))))))

(defun monk-connect--remote-pid (prefix)
  "Return the pid of the live agent behind PREFIX, or nil."
  (let ((default-directory prefix) found)
    (dolist (candidate monk-connect-remote-pid-files)
      (let ((file (expand-file-name candidate prefix)))
        (when (and (not found) (file-readable-p file))
          (let* ((info (with-temp-buffer (insert-file-contents file)
                                         (monk-event-parse (string-trim (buffer-string)))))
                 (pid (alist-get 'pid info)))
            (when (and pid (eq 0 (process-file "kill" nil nil nil "-0" (number-to-string pid))))
              (setq found pid))))))
    found))

;;;###autoload
(defun monk-reconnect (&optional prefix)
  "Reconnect to the agent behind PREFIX, default the current or last one.
Poke the agent to reload its source first, wait up to
`monk-connect-reload-wait' seconds for its build to match this
session's, then connect.  Signal `user-error' when the builds still
differ: the agent could not reload, and its log says why."
  (interactive)
  (let ((prefix (or prefix monk-connect--prefix)))
    (when (monk-connect-active-p)
      (monk-connect--close))
    (unless (monk-connect--poke prefix)
      (user-error "No agent is running on %s to reconnect to"
                  (if prefix (file-remote-p prefix 'host) "this host")))
    (let ((deadline (+ (float-time) monk-connect-reload-wait))
          (monk-connect-require-same-build nil)
          (last-build nil))
      (while (and (< (float-time) deadline)
                  (not (equal last-build monk-build)))
        (sleep-for 1)
        (condition-case nil
            (let ((inhibit-message t))
              (monk-connect--open-session nil prefix)
              (setq last-build (alist-get 'build monk-connect--status))
              (unless (equal last-build monk-build)
                (monk-connect--close)))
          (user-error nil)))
      (if (equal last-build monk-build)
          (message "Reconnected to the agent on %s, build %s"
                   (alist-get 'host monk-connect--status) monk-build)
        (user-error "The agent on %s is still on build %s after %ds (this session: %s); see its log"
                    (if prefix (file-remote-p prefix 'host) "this host")
                    (or last-build "unknown") monk-connect-reload-wait monk-build)))))

;;;###autoload
(defun monk-reconnect-all ()
  "Reconnect every agent this session has used: the current one and the history.
Each is poked and waited for in turn; the last one stays connected.
Say which ones came back and which did not."
  (interactive)
  (let* ((prefixes (reverse monk-connect--known-prefixes))
         (ok nil) (bad nil))
    (unless prefixes
      (user-error "No agent has been connected to in this session"))
    (dolist (prefix prefixes)
      (condition-case err
          (progn (monk-reconnect prefix)
                 (push (if prefix (file-remote-p prefix 'host) "local") ok))
        (user-error (push (format "%s (%s)" (if prefix (file-remote-p prefix 'host) "local")
                                  (error-message-string err))
                          bad))))
    (message "Reconnected: %s%s"
             (if ok (string-join (nreverse ok) ", ") "none")
             (if bad (format "; failed: %s" (string-join (nreverse bad) "; ")) ""))))

;;;###autoload
(defun monk-disconnect ()
  "Close the connection to the agent.  `*monk*' keeps the last run."
  (interactive)
  (when monk-connect--process
    (ignore-errors (delete-process monk-connect--process))
    (setq monk-connect--process nil))
  (setq monk-connect--status nil monk-connect--live-run nil monk-connect--waiting nil
        monk-connect--lost nil monk-connect--machine nil)
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
    (setq monk-connect--lost (cons (alist-get 'host monk-connect--status) (string-trim event))
          monk-connect--process nil monk-connect--status nil monk-connect--live-run nil
          monk-connect--waiting nil)
    (when monk-connect--waiting-timer
      (cancel-timer monk-connect--waiting-timer)
      (setq monk-connect--waiting-timer nil))
    (monk-connect--refresh-header)
    (message "Agent connection closed: %s; C-c m A or M-x monk-connect to reconnect" (string-trim event))))

(defun monk-connect-lost-p ()
  "Return non-nil when the agent dropped this session and nobody reconnected."
  (and (not (monk-connect-active-p)) monk-connect--lost t))

(defun monk-connect-reconnect-or-error (what)
  "Offer to reconnect before WHAT, a verb phrase, runs on the agent.
Called by the `*monk*' keys when the connection was lost.  Reconnect
on `y', else signal `user-error'.  Return non-nil once connected."
  (if (yes-or-no-p (format "The agent on %s dropped this session (%s).  Reconnect and %s there? "
                           (or (car monk-connect--lost) "this host") (cdr monk-connect--lost) what))
      (progn (monk-connect nil monk-connect--prefix) (monk-connect-active-p))
    (user-error "Not connected; M-x monk-disconnect to forget the agent and work locally")))

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
      (setq monk-connect--status event)
      (when (alist-get 'machine event)
        (setq monk-connect--machine (monk-connect-machine-from-reply (alist-get 'machine event)))))
     ((equal kind "machine")
      (setq monk-connect--machine (monk-connect-machine-from-reply event))
      (monk-connect--refresh-machine-buffer))
     ((equal op "status")
      (setq monk-connect--status event)
      (monk-connect--refresh-header))
     ;; a failed reply settles the wait too; an op the agent does not
     ;; know (older build than this session) left the session "still
     ;; answering" forever (2026-10-12)
     ((and op (not (eq t (alist-get 'ok event))))
      (monk-connect--fail op event)
      (message "Agent could not send %s: %s%s" (monk-connect-op-label op) (alist-get 'error event)
               (monk-connect--build-mismatch-note)))
     ((member op '("watch-mode" "watch-log" "history-runs" "history-results" "machine"))
      (monk-connect--reply op event))
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
    (monk-run-context-put 'agent (alist-get 'host monk-connect--status) run)
    (setq monk-connect--live-run run
          monk-connect--live-results nil
          monk-connect--run-events (list event))
    (let ((asked (or monk-connect--asked
                     ;; the agent names the asker in the origin
                     (let ((origin (alist-get 'origin event)))
                       (and origin (string-match-p (regexp-quote (monk-connect--client-name)) origin))))))
      ;; consumed by this run-start, batch or not
      (setq monk-connect--asked nil)
      (unless noninteractive
        (with-current-buffer (monk-ui--buffer)
          (setq monk-ui--run run)
          (monk-ui-reset-cache)
          (monk-ui-refresh)
          (unless (monk-ui-display-buffer (current-buffer) (not asked))
            (message "Agent: %s started (%s); *monk* follows it"
                     (if (monk-run-dry-run run) "plan" "run")
                     (or (alist-get 'origin event) "agent")))
          (monk-ui--spinner-start))))))

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
    (monk-run-context-put 'agent (alist-get 'host monk-connect--status) run)
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
      (monk-connect--refresh-header)
      (when callback (funcall callback value)))))

(defun monk-connect--fail (op _event)
  "Drop the wait for OP after the agent answered it with an error."
  (when (and monk-connect--waiting (equal (car monk-connect--waiting) op))
    (setq monk-connect--waiting nil)
    (when monk-connect--waiting-timer
      (cancel-timer monk-connect--waiting-timer)
      (setq monk-connect--waiting-timer nil))))

(defun monk-connect--build-label ()
  "Return the agent's version and build for the badge.
The same as this session's: `0.8.0 .9'.  Another build: `0.7.2
.7 ≠ .9', so a session that connected with the check off sees it."
  (let ((agent (alist-get 'build monk-connect--status))
        (version (alist-get 'version monk-connect--status)))
    (cond
     ((null agent) "")
     ((equal agent monk-build) (format "%s %s" version (monk-connect--short-build agent)))
     (t (format "%s %s ≠ %s" version (monk-connect--short-build agent) (monk-connect--short-build monk-build))))))

(defun monk-connect--short-build (build)
  "Return BUILD without its date when it is today's: `2026-10-12.9' -> `.9'."
  (if (string-prefix-p (format-time-string "%F") build)
      (substring build 10)
    build))

(defun monk-connect--build-mismatch-note ()
  "Return a note when the agent runs another build than this session, else \"\"."
  (let ((agent (alist-get 'build monk-connect--status)))
    (if (and agent (not (equal agent monk-build)))
        (format " (the agent is build %s, this session %s: it has not reloaded yet)" agent monk-build)
      "")))

(defun monk-connect--give-up (op)
  "Stop waiting for OP after `monk-connect-reply-timeout'; say so."
  (when (and monk-connect--waiting (equal (car monk-connect--waiting) op))
    (setq monk-connect--waiting nil monk-connect--waiting-timer nil)
    (monk-connect--refresh-header)
    (message "Agent: no answer with %s after %ds; no longer waiting (the agent may still be working)"
             (monk-connect-op-label op) monk-connect-reply-timeout)))

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
      (let ((window (monk-ui-display-buffer buffer)))
        (when (and (window-live-p window)
                   (monk-ui-select-window-p (with-current-buffer buffer monk-ui--run)))
          (select-window window)))))
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
    (user-error "Still waiting for the agent to send %s; k in *monk* stops waiting"
                (monk-connect-op-label (car monk-connect--waiting))))
  (setq monk-connect--waiting (cons op (or callback #'ignore)))
  (when (member op '("plan" "run"))
    (setq monk-connect--asked t))
  (when monk-connect--waiting-timer (cancel-timer monk-connect--waiting-timer))
  (setq monk-connect--waiting-timer
        (run-at-time monk-connect-reply-timeout nil #'monk-connect--give-up op))
  (monk-connect--refresh-header)
  (monk-connect--send (append (list (cons 'op op) (cons 'client (monk-connect--client-name))) extra)))

(defun monk-connect-drop-request ()
  "Forget the request this session is waiting on, so another can be sent."
  (interactive)
  (if (null monk-connect--waiting)
      (message "Not waiting on the agent")
    (message "No longer waiting for %s from the agent" (monk-connect-op-label (car monk-connect--waiting)))
    (setq monk-connect--waiting nil)
    (when monk-connect--waiting-timer
      (cancel-timer monk-connect--waiting-timer)
      (setq monk-connect--waiting-timer nil))
    (monk-connect--refresh-header)))

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

(defun monk-connect-machine-from-reply (reply)
  "Return a `monk-machine' built from the agent's `machine' REPLY.
Facts and vars come back as alists with symbol keys; classes and
groups as symbols; the facts source and time as the agent had them."
  (let ((m (monk-machine :name (alist-get 'name reply))))
    (oset m classes (mapcar #'intern (append (alist-get 'classes reply) nil)))
    (oset m groups (mapcar #'intern (append (alist-get 'groups reply) nil)))
    (oset m facts (mapcar (lambda (c) (cons (car c) (if (vectorp (cdr c)) (append (cdr c) nil) (cdr c))))
                          (alist-get 'facts reply)))
    (oset m vars (mapcar (lambda (c) (cons (car c) (if (vectorp (cdr c)) (append (cdr c) nil) (cdr c))))
                         (alist-get 'vars reply)))
    (let ((source (alist-get 'facts_source reply)))
      (oset m facts-source (and source (not (equal source "none")) (intern source))))
    (when (alist-get 'gathered reply)
      (oset m gathered (parse-iso8601-time-string (alist-get 'gathered reply))))
    m))

(defun monk-connect-inspect-machine ()
  "Show what guards can see on the agent's machine, from the copy the agent sent.
No request goes out: the agent sends its machine on connect and after
every fact gather, so this works while the agent is busy."
  (interactive)
  (unless monk-connect--machine
    (user-error "The agent has not sent its machine yet (an agent on an older build does not)"))
  (monk-ui-inspect-machine nil monk-connect--machine))

(defun monk-connect--refresh-machine-buffer ()
  "Redraw the agent's facts buffer when it is open: the facts just changed."
  (when (and monk-connect--machine
             (get-buffer (format monk-ui-detail-buffer-format
                                 (concat (oref monk-connect--machine name) " facts"))))
    (save-window-excursion
      (monk-ui-inspect-machine nil monk-connect--machine))))

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
  "Return the AGENT badge for the `*monk*' header, or an empty string.
Connected: AGENT with the host.  Dropped by the agent: AGENT LOST,
until a reconnect or `monk-disconnect'."
  (cond
   ((monk-connect-active-p)
    (concat (propertize (format " AGENT %s %s " (or (alist-get 'host monk-connect--status) "")
                                (monk-connect--build-label))
                        'face 'monk-ui-watch
                        'help-echo (concat (monk-connect-status-line)
                                           (format "\nagent build %s, this session %s"
                                                   (or (alist-get 'build monk-connect--status) "?") monk-build)
                                           "\ng plan on the agent, a apply, W watch mode, w watch log, k drop the pending request, D disconnect"))
            (if monk-connect--waiting
                (propertize (format " asked for %s… " (monk-connect-op-label (car monk-connect--waiting)))
                            'face 'monk-ui-partial
                            'help-echo "The agent has not answered yet; k stops waiting")
              "")
            " "))
   (monk-connect--lost
    (concat (propertize (format " AGENT LOST %s " (or (car monk-connect--lost) ""))
                        'face 'monk-ui-failed
                        'help-echo (format "The agent closed the connection: %s\nC-c m A or M-x monk-connect to reconnect; D to forget it"
                                           (cdr monk-connect--lost)))
            " "))
   (t "")))

(defun monk-connect--refuse-local-watch (&rest _)
  "Advice for `monk-watch-start': a connected session does not watch."
  (when (monk-connect-active-p)
    (user-error "This session is connected to the agent on %s, which watches; M-x monk-disconnect to watch here"
                (alist-get 'host monk-connect--status))))

(advice-add 'monk-watch-start :before #'monk-connect--refuse-local-watch)

(monk-note-load 'monk-connect)
(provide 'monk-connect)
;;; monk-connect.el ends here
