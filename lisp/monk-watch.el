;;; monk-watch.el --- React to filesystem drift  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, files

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

;; Continuous enforcement.  `monk-watch-start' compiles a policy for a
;; set of machines, collects the paths their resources own, and waits
;; for drift.
;;
;; Local machines use `file-notify-add-watch' on the parent directory
;; of each owned path.  A directory watch sees creates, deletes, and
;; renames, which a file watch misses.  Events map back to the nodes
;; whose paths match, then to a run restricted to those nodes and
;; their dependents (`:only').
;;
;; Remote machines have no notification channel over TRAMP, so they
;; are polled: every `monk-watch-poll-interval' a dry run finds the
;; resources that would change, and those count as drift.
;;
;; Two modes.  `assisted' plans the affected resources and shows the
;; plan in *monk* for you to apply.  `autopilot' repairs them.  Either
;; way the event log in *monk-watch* records what fired and why.
;;
;; Monk's own repairs also touch watched paths.  Events that arrive
;; while a run is active, and for `monk-watch-settle' seconds after,
;; are dropped.  Without that, every autopilot repair would trigger
;; another run.

;;; Code:

(require 'monk)
(require 'filenotify)

(defgroup monk-watch nil
  "Filesystem watching for Monk."
  :group 'monk
  :prefix "monk-watch-")

;;;; Constants

(defconst monk-watch-log-buffer-name "*monk-watch*"
  "Buffer that receives watch events.")

(defconst monk-watch-notify-flags '(change attribute-change)
  "What `file-notify-add-watch' reports.")

(defconst monk-watch-modes '(assisted autopilot)
  "Reactions to drift.")

(defconst monk-watch-remote-poll-comment "poll"
  "Event kind recorded for drift found by polling.")

(defconst monk-watch-probe-timeout 1.0
  "Seconds `monk-watch-notifications-work-p' waits for its own event.")

;;;; Customization

(defcustom monk-watch-mode 'assisted
  "What to do when a watched path changes.
`assisted' runs a plan for the affected resources and shows it in
*monk*; you apply it.  `autopilot' repairs them without asking."
  :type '(choice (const assisted) (const autopilot)))

(defcustom monk-watch-debounce 1.0
  "Seconds to wait after the last event before reacting.
An editor save, a package install, or a git checkout produces a burst
of events; one run per burst is enough."
  :type 'number)

(defcustom monk-watch-settle 2.0
  "Seconds after a run ends during which events are ignored.
Filesystem notifications for Monk's own writes arrive after the run
returns.  Measured on Linux inotify: within 0.1 s, but macOS kqueue
and FSEvents batch for longer."
  :type 'number)

(defcustom monk-watch-poll-interval 300
  "Seconds between drift checks on remote machines.
Each check is a dry run over TRAMP, so keep this in minutes."
  :type 'number)

;;;; State

(cl-defstruct (monk-watch (:constructor monk-watch-create)
                          (:copier nil))
  "One active watch over a set of machines."
  machines
  policy
  options
  nodes                 ; all compiled nodes
  (descriptors nil)     ; file-notify descriptors for local directories
  (path-index nil)      ; alist of (EXPANDED-DIR . ((FILE . NODE) ...))
  (pending nil)         ; keys collected since the last reaction
  (pending-reasons nil) ; alist of (KEY . PATH) for the log
  debounce-timer
  poll-timer
  (last-run-end 0.0)
  (events 0)
  (runs 0)
  (started (current-time)))

(defvar monk-watch--current nil
  "The active watch, or nil.")

(defvar monk-watch--running nil
  "Non-nil while a watch-triggered run is in progress.")

(defvar monk-watch-functions nil
  "Functions called with EVENT and DATA on watch activity.
EVENT is `started', `drift', `run', or `stopped'.  `monk-ui' uses
this for its indicator.")

(defun monk-watch-active-p ()
  "Return non-nil when a watch is active."
  (and monk-watch--current t))

(defvar monk-watch--delivery nil
  "Cached probe result: `yes', `no', or nil for not yet probed.
A session does not gain or lose kernel notification support, so one
probe per session is enough.  Tests bind this.")

(defun monk-watch-notifications-work-p (&optional directory)
  "Return non-nil when file notifications are delivered for DIRECTORY.
`file-notify--library' says a backend was compiled in, not that the
kernel delivers events.  Sandboxes and some network filesystems
report a library and deliver nothing.  The first call creates a file
in a temporary directory under DIRECTORY, by default the variable
`temporary-file-directory', and waits up to `monk-watch-probe-timeout'
for the event.  Later calls return the cached answer."
  (pcase monk-watch--delivery
    ('yes t)
    ('no nil)
    (_ (setq monk-watch--delivery (if (monk-watch--probe directory) 'yes 'no))
       (eq monk-watch--delivery 'yes))))

(defun monk-watch--probe (directory)
  "Create a file under DIRECTORY and return non-nil when its event arrives."
  (when file-notify--library
    (let* ((temporary-file-directory (or directory temporary-file-directory))
           (dir (file-name-as-directory (make-temp-file "monk-probe" t)))
           (seen nil)
           (desc nil))
      (unwind-protect
          (condition-case nil
              (progn
                (setq desc (file-notify-add-watch dir '(change) (lambda (_) (setq seen t))))
                (with-temp-file (concat dir "probe") (insert "x"))
                (let ((deadline (+ (float-time) monk-watch-probe-timeout)))
                  (while (and (not seen) (< (float-time) deadline))
                    (accept-process-output nil 0.05)))
                seen)
            (file-notify-error nil))
        (when desc (file-notify-rm-watch desc))
        (delete-directory dir t)))))

;;;; Logging

(defun monk-watch--log (format-string &rest args)
  "Append FORMAT-STRING with ARGS as a timestamped line to the watch log."
  (with-current-buffer (get-buffer-create monk-watch-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format-time-string "%T ") (apply #'format format-string args) "\n"))
    (unless (derived-mode-p 'special-mode)
      (special-mode)))
  (apply #'monk-log 'info (concat "watch: " format-string) args))

;;;; Path index

(defun monk-watch--node-paths (node)
  "Return the expanded target-side paths that NODE owns."
  (let ((m (monk-node-machine node))
        (r (monk-node-resource node)))
    (mapcar (lambda (p) (monk-machine-expand m p (oref r sudo)))
            (monk-resource-paths r))))

(defun monk-watch--build-index (nodes)
  "Return an alist of (DIRECTORY . ((PATH . NODE) ...)) for local NODES.
Only machines without an address are indexed; remote ones poll."
  (let ((index nil))
    (dolist (node nodes)
      (when (monk-machine-local-p (monk-node-machine node))
        (dolist (path (monk-watch--node-paths node))
          (let* ((full (expand-file-name path))
                 (dir (file-name-directory (directory-file-name full))))
            (push (cons full node) (alist-get dir index nil nil #'equal))))))
    index))

(defun monk-watch--nodes-for-file (watch file)
  "Return the nodes in WATCH whose owned path is FILE or contains it."
  (let ((full (expand-file-name file))
        (hits nil))
    (dolist (entry (monk-watch-path-index watch))
      (dolist (cell (cdr entry))
        (let ((owned (car cell)))
          (when (or (equal owned full)
                    (and (monk-resource-owns-subtree-p (monk-node-resource (cdr cell)))
                         (string-prefix-p (file-name-as-directory owned) full)))
            (cl-pushnew (cdr cell) hits)))))
    hits))

;;;; Events

(defun monk-watch--suppressed-p (watch)
  "Return non-nil when WATCH must ignore events right now."
  (or monk-watch--running
      (< (- (float-time) (monk-watch-last-run-end watch)) monk-watch-settle)))

(defun monk-watch--on-event (event)
  "Handle filenotify EVENT: (DESCRIPTOR ACTION FILE [FILE1])."
  (let ((watch monk-watch--current))
    (when watch
      (pcase-let ((`(,_desc ,action ,file . ,rest) event))
        (cl-incf (monk-watch-events watch))
        (cond
         ((eq action 'stopped)
          (monk-watch--log "watch on %s stopped by the system" file))
         ((monk-watch--suppressed-p watch)
          nil)
         (t
          (dolist (f (cons file rest))
            (dolist (node (monk-watch--nodes-for-file watch f))
              (monk-watch--queue watch (monk-node-key node) f action)))))))))

(defun monk-watch--queue (watch key path action)
  "Record on WATCH drift on KEY at PATH after ACTION; arm the debounce timer."
  (unless (member key (monk-watch-pending watch))
    (push key (monk-watch-pending watch))
    (push (cons key (format "%s %s" action path)) (monk-watch-pending-reasons watch))
    (monk-watch--log "drift %s:%s (%s %s)" (car key) (cdr key) action path)
    (run-hook-with-args 'monk-watch-functions 'drift (list :key key :path path :action action)))
  (when (monk-watch-debounce-timer watch)
    (cancel-timer (monk-watch-debounce-timer watch)))
  (setf (monk-watch-debounce-timer watch)
        (run-at-time monk-watch-debounce nil #'monk-watch--react)))

;;;; Reaction

(defun monk-watch--react ()
  "Run the pending keys in the current mode."
  (let ((watch monk-watch--current))
    (when (and watch (monk-watch-pending watch) (not monk-watch--running))
      (let ((keys (nreverse (monk-watch-pending watch)))
            (reasons (nreverse (monk-watch-pending-reasons watch))))
        (setf (monk-watch-pending watch) nil
              (monk-watch-pending-reasons watch) nil
              (monk-watch-debounce-timer watch) nil)
        (monk-watch--run watch keys reasons)))))

(defun monk-watch--run (watch keys reasons)
  "Run on WATCH the resources named by KEYS and their dependents.
REASONS is an alist of (KEY . TEXT) for the log."
  (let ((monk-watch--running t)
        (mode monk-watch-mode))
    (cl-incf (monk-watch-runs watch))
    (monk-watch--log "%s run for %d key(s): %s" mode (length keys)
                     (mapconcat (lambda (k) (format "%s:%s" (car k) (cdr k))) keys ", "))
    (unwind-protect
        (let ((run (apply #'monk-orchestrate
                          (monk-watch-machines watch) (monk-watch-policy watch)
                          :only keys
                          :dry-run (eq mode 'assisted)
                          :watch-reasons reasons
                          (monk-watch-options watch))))
          (run-hook-with-args 'monk-watch-functions 'run (list :run run :keys keys :mode mode))
          (monk-run-report run)
          run)
      (setf (monk-watch-last-run-end watch) (float-time)))))

;;;; Remote polling

(defun monk-watch--poll ()
  "Dry-run the remote machines and treat would-repair as drift."
  (let ((watch monk-watch--current))
    (when (and watch (not monk-watch--running))
      (let ((remote (seq-remove #'monk-machine-local-p (monk-watch-machines watch))))
        (when remote
          (let* ((monk-watch--running t)
                 (probe (condition-case err
                            (apply #'monk-plan remote (monk-watch-policy watch)
                                   (monk-watch-options watch))
                          (error
                           (monk-watch--log "poll failed: %s" (error-message-string err))
                           nil)))
                 (drifted (and probe
                               (mapcar #'monk-result-key
                                       (seq-filter #'monk-result-changed-p
                                                   (monk-run-results probe))))))
            (setf (monk-watch-last-run-end watch) (float-time))
            (cl-incf (monk-watch-events watch))
            (when drifted
              (dolist (key drifted)
                (monk-watch--queue watch key (cdr key) monk-watch-remote-poll-comment)))))))))

;;;; Start and stop

(defun monk-watch--add-watches (watch)
  "Create filenotify watches for every directory in the index of WATCH."
  (dolist (entry (monk-watch-path-index watch))
    (let ((dir (car entry)))
      (if (not (file-directory-p dir))
          ;; The parent does not exist yet.  The resource that creates
          ;; it is in the index too, so a run will make it, and the
          ;; next start picks it up.
          (monk-watch--log "no watch on %s: directory absent" dir)
        (condition-case err
            (push (file-notify-add-watch dir monk-watch-notify-flags #'monk-watch--on-event)
                  (monk-watch-descriptors watch))
          (file-notify-error
           (monk-watch--log "cannot watch %s: %s" dir (error-message-string err))))))))

;;;###autoload
(defun monk-watch-start (machines policy &rest options)
  "Watch the paths that POLICY manages on MACHINES and react to drift.
MACHINES is a machine or a list.  OPTIONS pass to every run.  Local
machines get filesystem notifications; remote ones are polled every
`monk-watch-poll-interval' seconds.  Return the watch."
  (when monk-watch--current
    (monk-watch-stop))
  (let ((local (seq-some #'monk-machine-local-p (if (listp machines) machines (list machines)))))
    (when (and local (not file-notify--library))
      (signal 'monk-error (list "This Emacs has no file notification support")))
    (when (and local (not (monk-watch-notifications-work-p)))
      (monk-watch--log "warning: file notifications are not delivered here; local drift will not be seen")))
  (let* ((machines (if (listp machines) machines (list machines)))
         (nodes (monk--nodes-for machines policy))
         (watch (monk-watch-create :machines machines :policy policy :options options
                                   :nodes nodes
                                   :path-index (monk-watch--build-index nodes))))
    (setq monk-watch--current watch)
    (monk-watch--add-watches watch)
    (when (seq-remove #'monk-machine-local-p machines)
      (setf (monk-watch-poll-timer watch)
            (run-at-time monk-watch-poll-interval monk-watch-poll-interval #'monk-watch--poll)))
    (monk-watch--log "started: %d machine(s), %d directories, %d remote polled every %ds, mode %s"
                     (length machines) (length (monk-watch-descriptors watch))
                     (length (seq-remove #'monk-machine-local-p machines))
                     monk-watch-poll-interval monk-watch-mode)
    (run-hook-with-args 'monk-watch-functions 'started (list :watch watch))
    watch))

;;;###autoload
(defun monk-watch-stop ()
  "Stop the active watch."
  (interactive)
  (let ((watch monk-watch--current))
    (when watch
      (dolist (desc (monk-watch-descriptors watch))
        (when (file-notify-valid-p desc)
          (file-notify-rm-watch desc)))
      (when (monk-watch-debounce-timer watch)
        (cancel-timer (monk-watch-debounce-timer watch)))
      (when (monk-watch-poll-timer watch)
        (cancel-timer (monk-watch-poll-timer watch)))
      (setq monk-watch--current nil)
      (monk-watch--log "stopped after %d event(s), %d run(s)"
                       (monk-watch-events watch) (monk-watch-runs watch))
      (run-hook-with-args 'monk-watch-functions 'stopped (list :watch watch)))))

;;;###autoload
(defun monk-watch-toggle-mode ()
  "Switch between assisted and autopilot."
  (interactive)
  (setq monk-watch-mode (if (eq monk-watch-mode 'assisted) 'autopilot 'assisted))
  (monk-watch--log "mode %s" monk-watch-mode)
  (message "Monk watch: %s" monk-watch-mode))

(defun monk-watch-status ()
  "Return a one-line description of the active watch, or nil."
  (when monk-watch--current
    (let ((w monk-watch--current))
      (format "%s, %d dir(s), %d event(s), %d run(s), up %s"
              monk-watch-mode (length (monk-watch-descriptors w))
              (monk-watch-events w) (monk-watch-runs w)
              (format-seconds "%hh%mm%ss" (float-time (time-since (monk-watch-started w))))))))

(provide 'monk-watch)
;;; monk-watch.el ends here
