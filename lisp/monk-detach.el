;;; monk-detach.el --- Run a policy from a process that outlives this Emacs  -*- lexical-binding: t; -*-

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

;; Some policies take too long for the session that asks for them, or
;; replace the Emacs that would run them: a Homebrew rebuild of Emacs
;; itself takes a quarter of an hour.  A running process keeps its
;; mapped binary when the file on disk is replaced, so the session
;; survives such a rebuild; it just must not block on it.
;;
;; `monk-detach' writes a one-shot shell script under the Monk state
;; directory that starts a fresh `emacs --batch' with the same load
;; path and policy file, and runs it with `nohup' outside this Emacs's
;; process group.  The batch Emacs first waits for the asking Emacs to
;; exit (its PID is in the script), then converges the policy with
;; terminal progress into a log file, then tells the user through the
;; desktop notification the platform has.  The script stays on disk,
;; so the same install can be run again from a shell, and the log
;; stays as the record.
;;
;; `monk-detach-running-emacsen' lists the other Emacs processes on
;; the host, so a caller can say which ones will keep running the old
;; build until they are restarted.
;;
;; Nothing here is macOS-only except the notification fallback order;
;; on Linux `notify-send' is tried first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'monk)

(declare-function monk-cli-enable "monk-cli")
(declare-function monk-cli-print-line "monk-cli" (text))

(defgroup monk-detach nil
  "Policies that run after this Emacs quits."
  :group 'monk)

(defcustom monk-detach-emacs nil
  "The Emacs binary the detached job runs.  Nil means this one.
Set it when this Emacs is the one about to be replaced and a stable
copy exists elsewhere, for example `/usr/bin/emacs'."
  :type '(choice (const nil) file))

(defcustom monk-detach-wait-seconds 600
  "How long the detached job waits for the asking Emacs to exit.
When the time is up it goes on regardless and says so in the log."
  :type 'natnum)

(defconst monk-detach-script-name "detach-%s.sh"
  "Script file name under the Monk state directory; %s is the job name.")

(defconst monk-detach-log-name "detach-%s.log"
  "Log file name under the Monk state directory; %s is the job name.")

(defconst monk-detach-poll-seconds 2
  "How often the detached job checks whether the asking Emacs is gone.")

;;;; Other Emacsen on this host

(defun monk-detach-running-emacsen ()
  "Return the Emacs processes on this host other than this one.
Each is (PID . COMMAND-LINE).  Reads the process list through
`list-system-processes' and `process-attributes', so no `ps' runs.
A batch Emacs such as the Monk agent counts: it keeps an old build too."
  (let ((me (emacs-pid)) out)
    (dolist (pid (list-system-processes))
      (unless (eql pid me)
        (let* ((attrs (process-attributes pid))
               (comm (alist-get 'comm attrs))
               (args (alist-get 'args attrs)))
          (when (and comm
                     (or (string-match-p "\\`[Ee]macs\\(-[0-9.]+\\)?\\'" comm)
                         (and args (string-match-p "Emacs\\.app/Contents/MacOS/Emacs\\|/bin/emacs\\(-[0-9.]+\\)? " (concat args " ")))))
            (push (cons pid (or args comm)) out)))))
    (nreverse out)))

(defun monk-detach-describe-emacsen (emacsen)
  "Return EMACSEN from `monk-detach-running-emacsen' as short lines.
The command line is shown as the binary's name and its arguments,
not its directory: `Emacs --batch -f monk-agent-main' says which
one it is, `/opt/homebrew/Cellar/emacs-plus@32/32.0.50/...' does not."
  (mapconcat (lambda (cell)
               (let* ((command (cdr cell))
                      (space (string-search " " command))
                      (program (file-name-nondirectory (if space (substring command 0 space) command)))
                      (rest (if space (substring command space) ""))
                      (what (if (string-empty-p (string-trim rest)) "(interactive)" (string-trim rest))))
                 (format "  pid %-7d %s %s" (car cell) program
                         (truncate-string-to-width what 60 nil nil "..."))))
             emacsen "\n"))

;;;; The script

(defun monk-detach-script-path (name)
  "Return the script path for job NAME."
  (expand-file-name (format monk-detach-script-name name) (monk-home-directory)))

(defun monk-detach-log-path (name)
  "Return the log path for job NAME."
  (expand-file-name (format monk-detach-log-name name) (monk-home-directory)))

(defun monk-detach--emacs-binary ()
  "Return the Emacs binary for the job: `monk-detach-emacs', else this one."
  (or monk-detach-emacs
      (expand-file-name invocation-name invocation-directory)))

(defun monk-detach-notify-command (title text)
  "Return a shell command that shows TITLE and TEXT as a desktop notification.
`osascript' on macOS, `notify-send' elsewhere; a no-op when neither
exists, so the script never fails on the notification."
  (format "if command -v osascript >/dev/null 2>&1; then osascript -e %s; elif command -v notify-send >/dev/null 2>&1; then notify-send %s %s; fi"
          (shell-quote-argument (format "display notification %S with title %S" text title))
          (shell-quote-argument title) (shell-quote-argument text)))

(defun monk-detach-script (name policy-file function &optional wait-pid)
  "Return the text of the one-shot script for job NAME.
The script runs POLICY-FILE's FUNCTION in a batch Emacs with this
load path, after waiting for WAIT-PID to exit, logging to the job's
log.  It is plain POSIX shell."
  (let ((log (monk-detach-log-path name))
        (emacs (monk-detach--emacs-binary))
        (load-dirs (seq-uniq (delq nil (mapcar (lambda (lib)
                                                 (let ((f (locate-library lib)))
                                                   (and f (file-name-directory f))))
                                               '("monk" "monk-detach"))))))
    (concat
     "#!/bin/sh\n"
     (format "# Monk detached job %s, written %s.  Safe to run again.\n" name (format-time-string "%F %T"))
     "set -u\n"
     (format "LOG=%s\n" (shell-quote-argument log))
     (format "WAIT_PID=%s\n" (or wait-pid 0))
     (format "echo \"== %s $(date '+%%F %%T') start (waiting for pid $WAIT_PID)\" >> \"$LOG\"\n" name)
     ;; wait for the asking Emacs; kill -0 says whether a pid is alive
     (format "n=0; while [ \"$WAIT_PID\" -gt 0 ] && kill -0 \"$WAIT_PID\" 2>/dev/null && [ $n -lt %d ]; do sleep %d; n=$((n+%d)); done\n"
             monk-detach-wait-seconds monk-detach-poll-seconds monk-detach-poll-seconds)
     (format "if [ \"$WAIT_PID\" -gt 0 ] && kill -0 \"$WAIT_PID\" 2>/dev/null; then echo \"pid $WAIT_PID still alive after %ds; going on\" >> \"$LOG\"; fi\n"
             monk-detach-wait-seconds)
     (format "%s --batch --no-init-file %s -l %s -f %s >> \"$LOG\" 2>&1\n"
             (shell-quote-argument emacs)
             (mapconcat (lambda (d) (concat "-L " (shell-quote-argument (directory-file-name d)))) load-dirs " ")
             (shell-quote-argument policy-file)
             (shell-quote-argument (symbol-name function)))
     "rc=$?\n"
     (format "echo \"== %s $(date '+%%F %%T') end rc=$rc\" >> \"$LOG\"\n" name)
     (format "if [ $rc -eq 0 ]; then %s; else %s; fi\n"
             (monk-detach-notify-command "Monk" (format "%s finished; log at %s" name (abbreviate-file-name log)))
             (monk-detach-notify-command "Monk" (format "%s FAILED (rc $rc); see %s" name (abbreviate-file-name log))))
     "exit $rc\n")))

(defun monk-detach-write (name policy-file function &optional wait-pid)
  "Write the script for job NAME and return its path.
POLICY-FILE, FUNCTION, and WAIT-PID as for `monk-detach-script'."
  (let ((path (monk-detach-script-path name)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (monk-detach-script name policy-file function wait-pid)))
    (set-file-modes path #o700)
    path))

(defun monk-detach-start (script)
  "Start SCRIPT detached from this Emacs: own session, no tty, nohup.
Return the process object of the launcher.  With `setsid' that
launcher exits at once and the job lives on in its own session, so
the object says nothing about the job; the log does."
  (let ((process-connection-type nil)
        (default-directory temporary-file-directory))
    ;; `setsid' puts it in its own session so quitting Emacs does not
    ;; take it down; not every macOS has setsid, nohup alone then.
    (if (executable-find "setsid")
        (start-process "monk-detach" nil "setsid" "nohup" script)
      (start-process "monk-detach" nil "nohup" script))))

;;;###autoload
(defun monk-detach (name policy-file function &optional wait-for-me)
  "Run FUNCTION from POLICY-FILE in a batch Emacs that outlives this one.
NAME labels the script and log under the Monk state directory.  With
WAIT-FOR-ME, the job first waits for this Emacs to exit.  Return the
log path.  The caller decides what to tell the user and whether to
quit; this only writes and starts the script."
  (let* ((script (monk-detach-write name policy-file function (and wait-for-me (emacs-pid)))))
    (monk-detach-start script)
    (monk-log 'info "detached %s: %s, log %s" name script (monk-detach-log-path name))
    (monk-detach-log-path name)))

;;;; Inside the detached job

(defun monk-detach-run (machine policy &optional name dry-run)
  "Converge POLICY on MACHINE with terminal progress; for the batch side.
NAME goes in the log lines.  With DRY-RUN, plan only.  Exit the batch
Emacs with the run's exit code, so the script can tell success from
failure."
  (require 'monk-cli)
  (monk-cli-enable)
  (let ((monk-log-level 'info))
    (monk-cli-print-line (format "%s %s build %s: %s %s on %s"
                                 (or name "monk-detach") monk-version monk-build
                                 (if dry-run "plan" "apply") policy (oref machine name)))
    (let ((run (monk-converge machine policy :dry-run dry-run)))
      (monk-run-report run)
      (kill-emacs (monk-run-exit-code run)))))

(monk-note-load 'monk-detach)
(provide 'monk-detach)
;;; monk-detach.el ends here
