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
;; renames, which a file watch misses.  On kqueue (macOS, BSD) a
;; directory watch does not see writes to the files inside it, so
;; every owned path that exists is watched as well; a file created
;; later is picked up by the directory watch and then watched itself.
;; Events map back to the nodes whose paths match, then to a run
;; restricted to those nodes and their dependents (`:only').
;;
;; On macOS, `fswatch' (Homebrew) is preferred when installed.  It
;; uses FSEvents, which is recursive and reports writes, so one process
;; covers every watched directory, and a file created later needs no
;; new watch.  It prints one changed path per line; Monk reads that
;; stream through a process filter and maps paths the same way as
;; filenotify events.  `monk-watch-backend' picks.
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

(defconst monk-watch-fswatch-program "fswatch"
  "The fswatch binary.")

(defconst monk-watch-fswatch-args
  '("--latency" "0.2" "--event" "Created" "--event" "Updated"
    "--event" "Removed" "--event" "Renamed" "--event" "AttributeModified"
    "--print0")
  "Arguments for fswatch.
The events Monk cares about, NUL-separated paths because a path can
hold a newline.  `--latency' is the FSEvents coalescing window, kept
below `monk-watch-debounce'.  `--recursive' is added only for the
process that watches owned subtrees: a home directory that holds a
few managed dotfiles must not be walked, or every application's
scratch file under it becomes an event.")

(defconst monk-watch-fswatch-excludes '("/\\.git/" "/\\.Trash/" "/Library/Caches/")
  "Regexps fswatch drops before Monk sees them.  Noise that no policy owns.")

;;;; Customization

(defcustom monk-watch-backend 'auto
  "How local filesystem changes are observed.
`auto' uses fswatch when `monk-watch-fswatch-program' is on the path
and the system is Darwin, else `filenotify'.  `fswatch' or `filenotify'
force one.  nil watches nothing locally; remote polling still runs."
  :type '(choice (const auto) (const fswatch) (const filenotify) (const nil)))

(defcustom monk-watch-follow-links t
  "When non-nil, a watched path that is a symlink is watched at its target too.
What the machine sees through ~/.tmux.conf is the file in the repo it
points at; an edit there is drift for whatever reads the link (an
rsync that copies through it, a program that loads it) even though
the link itself is unchanged.  The link is watched as well, so a
retarget is seen, and the repath check (`monk-watch-repath-interval')
moves the watch when the target changes.  Nil watches the link only."
  :type 'boolean)

(defcustom monk-watch-repath-interval 300
  "Seconds between checks that watched paths still resolve to the same places.
FSEvents and kqueue watch the directory a path resolved to when the
watch started; a symlink retargeted later (a checkout swapped under
~/source/x) leaves them watching the old tree.  Every this many
seconds the watch resolves its paths again and, when any moved,
starts over on the new ones.  Nil turns the check off."
  :type '(choice (const nil) natnum))

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
  (watched nil)         ; alist of (PATH . DESCRIPTOR) actually watched
  (backend nil)         ; `fswatch', `filenotify', or nil
  (process nil)         ; the fswatch process over parent directories, flat
  (tree-process nil)    ; the recursive fswatch over owned subtrees
  (path-index nil)      ; alist of (EXPANDED-DIR . ((FILE . NODE) ...))
  (pending nil)         ; keys collected since the last reaction
  (pending-reasons nil) ; alist of (KEY . PATH) for the log
  debounce-timer
  poll-timer
  (last-run-end 0.0)
  (events 0)
  (runs 0)
  (started (current-time))
  (repath-timer nil)       ; see `monk-watch-repath-interval'
  (repath-soon-timer nil)  ; armed by a link event; see `monk-watch--repath-soon'
  (path-signature nil))    ; what the owned paths resolved to at start

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

(defun monk-watch--choose-backend ()
  "Return the local backend to use now: `fswatch', `filenotify', or nil.
Decided by the option `monk-watch-backend'."
  (pcase monk-watch-backend
    ('auto (cond
            ((and (eq system-type 'darwin) (executable-find monk-watch-fswatch-program)) 'fswatch)
            (file-notify--library 'filenotify)
            (t nil)))
    ('fswatch (if (executable-find monk-watch-fswatch-program) 'fswatch
                (signal 'monk-error (list "fswatch is not on the path"))))
    ('filenotify (if file-notify--library 'filenotify
                   (signal 'monk-error (list "This Emacs has no file notification support"))))
    (_ nil)))

(defun monk-watch-file-watch-needed-p ()
  "Return non-nil when a directory watch misses writes to files inside it.
True for kqueue: `filenotify.el' watches the named file only, so a
change to a file's content under a watched directory is not reported."
  (eq file-notify--library 'kqueue))

(defun monk-watch--probe (directory)
  "Create a file under DIRECTORY and return non-nil when its event arrives.
The probe does what the watch does: it watches the directory, and on
kqueue the file too, then writes the file and waits."
  (when file-notify--library
    (let* ((temporary-file-directory (or directory temporary-file-directory))
           (dir (file-name-as-directory (make-temp-file "monk-probe" t)))
           (file (concat dir "probe"))
           (seen nil)
           (descs nil))
      (unwind-protect
          (condition-case nil
              (progn
                (with-temp-file file (insert "x"))
                (push (file-notify-add-watch dir '(change) (lambda (_) (setq seen t))) descs)
                (when (monk-watch-file-watch-needed-p)
                  (push (file-notify-add-watch file '(change) (lambda (_) (setq seen t))) descs))
                (with-temp-file file (insert "y"))
                (let ((deadline (+ (float-time) monk-watch-probe-timeout)))
                  (while (and (not seen) (< (float-time) deadline))
                    (accept-process-output nil 0.05)))
                seen)
            (file-notify-error nil))
        (dolist (d descs) (ignore-errors (file-notify-rm-watch d)))
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
  (apply #'monk-log (if (or (string-prefix-p "warning" format-string)
                            (string-prefix-p "started" format-string)
                            (string-prefix-p "drift" format-string)
                            (string-prefix-p "%s %s:%s" format-string))
                        'warn 'info)
         (concat (monk-watch-glyph) " " format-string) args))

(defconst monk-watch-glyph-prefix "M"
  "Prefix of watch lines on a terminal without UTF-8.
A letter: an eye emoji drew as a color glyph of its own size and
looked out of place next to text.")

(defconst monk-watch-glyph-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Prefix of watch lines on UTF-8: one frame of the 2x3 braille roll.
The same roll the terminal spins while watching, so a logged line
reads as a moment of it.")

(defconst monk-watch-glyph-drift "Δ"
  "Marks a drift line on a UTF-8 terminal.")

(defvar monk-watch--glyph-index 0
  "Which frame `monk-watch-glyph' gives next.")

(defun monk-watch-glyph ()
  "Return the watch prefix: a braille frame on UTF-8, else `M'.
The frame advances with each call, so consecutive lines roll."
  (if (monk-watch--utf8-p)
      (let ((frame (aref monk-watch-glyph-frames
                         (% monk-watch--glyph-index (length monk-watch-glyph-frames)))))
        (cl-incf monk-watch--glyph-index)
        frame)
    monk-watch-glyph-prefix))


(defun monk-watch--utf8-p ()
  "Return non-nil when the locale says UTF-8, or when not in batch."
  (or (not noninteractive)
      (let ((lang (or (getenv "LC_ALL") (getenv "LC_CTYPE") (getenv "LANG") "")))
        (and (string-match-p "[Uu][Tt][Ff]-?8" lang) t))))

;;;; Path index

(defun monk-watch--node-paths (node)
  "Return the expanded target-side paths that NODE owns.
With `monk-watch-follow-links', a local path that is a symlink
contributes its current target as well: ~/.tmux.conf ->
autodist/files/tmux.conf edited in the repo is drift for everything
that reads the link \(2026-10-16: an edit there changed nothing the
watch indexed).  The link stays owned too.  Targets are resolved at
index time; the repath check sees a retarget and reindexes."
  (let* ((m (monk-node-machine node))
         (r (monk-node-resource node))
         (paths (mapcar (lambda (p) (monk-machine-expand m p (oref r sudo)))
                        (monk-resource-paths r))))
    (if (not (and monk-watch-follow-links (monk-machine-local-p m)))
        paths
      (append paths
              (delq nil (mapcar (lambda (p)
                                  (and (file-symlink-p (directory-file-name p))
                                       (file-exists-p p)
                                       (file-truename p)))
                                paths))))))

(defun monk-watch-real-path (path)
  "Return PATH with every symlink in its directory part resolved.
The last component stays as written, so a managed symlink is indexed
under its own name and not under its target.  Kernels report the
real path of a changed file: when ~/autodist is a link into another
tree, an edit there arrives as the other tree's path, and an index
keyed on the spelling in the policy would never match it."
  (let* ((full (expand-file-name path))
         (dir (file-name-directory (directory-file-name full)))
         (name (file-name-nondirectory (directory-file-name full))))
    (if (and dir (file-directory-p dir))
        (concat (file-name-as-directory (file-truename dir)) name)
      full)))

(defun monk-watch--build-index (nodes)
  "Return an alist of (DIRECTORY . ((PATH . NODE) ...)) for local NODES.
Only machines without an address are indexed; remote ones poll.
Paths and directories are real paths, see `monk-watch-real-path'."
  (let ((index nil))
    (dolist (node nodes)
      (when (monk-machine-local-p (monk-node-machine node))
        (dolist (path (monk-watch--node-paths node))
          (let* ((full (monk-watch-real-path path))
                 (dir (file-name-directory (directory-file-name full))))
            (push (cons full node) (alist-get dir index nil nil #'equal))))))
    index))

(defun monk-watch--nodes-for-file (watch file)
  "Return the nodes in WATCH whose owned path is FILE or contains it.
FILE is compared as a real path, like the index."
  (let ((full (monk-watch-real-path file))
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
  "Return non-nil when WATCH must ignore events right now.
Any run in progress counts, not only one the watch started: a manual
apply repairs the same paths, and its writes are not drift.  Salt's
`disable_during_state_run'.  The settle window after a run covers
writes the kernel reports late."
  (or monk-watch--running
      monk--current-run
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
            (monk-watch--adopt-new-file watch f)
            ;; an event on an owned path that is a symlink is a possible
            ;; retarget: resolve again now, not at the next 300 s tick
            (when (and monk-watch-follow-links (monk-watch--owned-link-p watch f))
              (monk-watch--repath-soon watch))
            (let ((nodes (monk-watch--nodes-for-file watch f)))
              (if nodes
                  (dolist (node nodes)
                    (monk-watch--queue watch (monk-node-key node) f action))
                (monk-watch--unmanaged watch f action))))))))))

(defun monk-watch--owned-link-p (watch file)
  "Return non-nil when FILE is an owned path of WATCH and is a symlink now."
  (let ((full (monk-watch-real-path file)))
    (and (seq-some (lambda (entry) (assoc full (cdr entry)))
                   (monk-watch-path-index watch))
         (file-symlink-p (directory-file-name full)))))

(defconst monk-watch-repath-soon-delay 0.5
  "Seconds after a link event before the paths are resolved again.
Long enough for `ln -sf' (unlink then link) to finish both steps.")

(defun monk-watch--repath-soon (watch)
  "Arm one short timer that runs `monk-watch--repath' for WATCH."
  (unless (monk-watch-repath-soon-timer watch)
    (setf (monk-watch-repath-soon-timer watch)
          (run-at-time monk-watch-repath-soon-delay nil
                       (lambda ()
                         (when (eq watch monk-watch--current)
                           (setf (monk-watch-repath-soon-timer watch) nil)
                           (monk-watch--repath)))))))

(defvar monk-watch--unmanaged-seen nil
  "Paths already reported as unmanaged, so a busy directory logs once.")

(defconst monk-watch-unmanaged-log-limit 3
  "How many unmanaged paths are logged by name before the log goes quiet.
A flat watch on a parent such as ~ sees every file written there;
after a few, the count in `monk-watch-status' is the record.")

(defvar monk-watch--unmanaged-count 0
  "Unmanaged events seen since the watch started.")

(defvar monk-watch--unmanaged-by-directory nil
  "Alist of (DIRECTORY . COUNT): where the unmanaged events came from.
20,000 events in two hours with 0 runs (2026-10-12) needed this to
say which directory was the noise.")

(defconst monk-watch-noisy-directories-shown 3
  "How many noisy directories the stop line and the status name.")

(defun monk-watch-noisy-directories (&optional n)
  "Return the N (default `monk-watch-noisy-directories-shown') busiest
unmanaged directories as ((DIRECTORY . COUNT) ...), busiest first."
  (seq-take (sort (copy-sequence monk-watch--unmanaged-by-directory)
                  (lambda (a b) (> (cdr a) (cdr b))))
            (or n monk-watch-noisy-directories-shown)))

(defun monk-watch--noisy-directories-string ()
  "Return the busiest unmanaged directories as text, or \"\" when none."
  (let ((top (monk-watch-noisy-directories)))
    (if (null top)
        ""
      (format "; unmanaged mostly from %s"
              (mapconcat (lambda (c) (format "%s (%d)" (abbreviate-file-name (car c)) (cdr c)))
                         top ", ")))))

(defun monk-watch--unmanaged (watch file action)
  "Note that WATCH saw ACTION on FILE, which no resource owns.
The first few are logged by name, at info, so a watch that looks dead
can be checked; after that only the counts grow, per directory."
  (ignore watch)
  (cl-incf monk-watch--unmanaged-count)
  (let ((dir (file-name-directory (directory-file-name file))))
    (cl-incf (alist-get dir monk-watch--unmanaged-by-directory 0 nil #'equal)))
  (unless (member file monk-watch--unmanaged-seen)
    (push file monk-watch--unmanaged-seen)
    (cond
     ((<= (length monk-watch--unmanaged-seen) monk-watch-unmanaged-log-limit)
      (monk-log 'info "%s %s %s: not managed by any resource" (monk-watch-glyph) action file))
     ((= (length monk-watch--unmanaged-seen) (1+ monk-watch-unmanaged-log-limit))
      (monk-log 'info "%s more unmanaged paths; see the event count in the status" (monk-watch-glyph))))))

(defun monk-watch--queue (watch key path action)
  "Record on WATCH drift on KEY at PATH after ACTION; arm the debounce timer."
  (unless (member key (monk-watch-pending watch))
    (push key (monk-watch-pending watch))
    (push (cons key (format "%s %s" action path)) (monk-watch-pending-reasons watch))
    (monk-watch--log "%s %s:%s  %s %s"
                     (if (monk-watch--utf8-p) monk-watch-glyph-drift "drift")
                     (car key) (cdr key) action path)
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
    (monk-watch--log "%s run for %d key%s: %s" mode (length keys) (if (= 1 (length keys)) "" "s")
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

;;;; fswatch backend

(defun monk-watch--fswatch-filter (proc chunk)
  "Split CHUNK from PROC on NUL and feed each path to the event handler.
A partial path at the end of a chunk waits for the next one."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk))
        (paths nil))
    (while (string-match "\0" buffer)
      (push (substring buffer 0 (match-beginning 0)) paths)
      (setq buffer (substring buffer (match-end 0))))
    (process-put proc 'monk-partial buffer)
    (dolist (path (nreverse paths))
      (unless (string-empty-p path)
        ;; fswatch says which event only with --event-flags; Monk does
        ;; not need the kind, the check decides what to do.
        (monk-watch--on-event (list proc 'changed path))))))

(defun monk-watch--fswatch-sentinel (proc event)
  "Log the end of PROC with EVENT; a dead watcher is worth noticing."
  (unless (process-live-p proc)
    (monk-watch--log "fswatch exited: %s" (string-trim event))))

(defun monk-watch--fswatch-paths (watch)
  "Return (FLAT . TREES) for WATCH: directories to watch flat, subtrees to walk.
A resource that owns a subtree (a git checkout, an rsync source tree,
a managed directory) gets its own path, walked.  Any other owned path
gets its parent directory, not walked: FSEvents reports the changed
file's own path, so the parent is enough, and walking ~ would report
every application's scratch file.  A parent already inside a walked
tree is dropped."
  (let (flat trees)
    (dolist (entry (monk-watch-path-index watch))
      (dolist (cell (cdr entry))
        (let ((path (car cell))
              (node (cdr cell)))
          (if (and (monk-resource-owns-subtree-p (monk-node-resource node))
                   (file-directory-p path))
              (cl-pushnew (directory-file-name path) trees :test #'equal)
            (when (file-directory-p (car entry))
              (cl-pushnew (directory-file-name (car entry)) flat :test #'equal))))))
    (setq flat (seq-remove (lambda (d)
                             (seq-some (lambda (tree)
                                         (or (equal d tree)
                                             (string-prefix-p (file-name-as-directory tree) d)))
                                       trees))
                           flat))
    (cons (sort flat #'string<) (sort trees #'string<))))

(defun monk-watch--fswatch-spawn (name paths recursive)
  "Start one fswatch called NAME over PATHS, walking them when RECURSIVE."
  (let ((proc (apply #'start-process name nil monk-watch-fswatch-program
                     (append (and recursive '("--recursive"))
                             monk-watch-fswatch-args
                             (mapcan (lambda (re) (list "--exclude" re))
                                     monk-watch-fswatch-excludes)
                             paths))))
    (set-process-filter proc #'monk-watch--fswatch-filter)
    (set-process-sentinel proc #'monk-watch--fswatch-sentinel)
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun monk-watch--fswatch-start (watch)
  "Start fswatch for WATCH.
One flat process over parent directories of owned files, one
recursive process over owned subtrees.  Return the first process
started, or nil when nothing exists to watch."
  (pcase-let ((`(,flat . ,trees) (monk-watch--fswatch-paths watch)))
    (when flat
      (let ((proc (monk-watch--fswatch-spawn "monk-fswatch" flat nil)))
        (setf (monk-watch-process watch) proc)
        (dolist (d flat) (push (cons d proc) (monk-watch-watched watch)))))
    (when trees
      (let ((proc (monk-watch--fswatch-spawn "monk-fswatch-trees" trees t)))
        (setf (monk-watch-tree-process watch) proc)
        (dolist (d trees) (push (cons d proc) (monk-watch-watched watch)))))
    (or (monk-watch-process watch) (monk-watch-tree-process watch))))

;;;; Start and stop

(defun monk-watch--add-one (watch path)
  "Add a filenotify watch on PATH to WATCH.  Return the descriptor or nil."
  (condition-case err
      (let ((desc (file-notify-add-watch path monk-watch-notify-flags #'monk-watch--on-event)))
        (push desc (monk-watch-descriptors watch))
        (push (cons path desc) (monk-watch-watched watch))
        desc)
    (file-notify-error
     (monk-watch--log "cannot watch %s: %s" path (error-message-string err))
     nil)))

(defun monk-watch--add-watches (watch)
  "Create filenotify watches for the index of WATCH.
Every parent directory that exists is watched.  Where a directory
watch misses file writes, every owned file that exists is watched
too."
  (dolist (entry (monk-watch-path-index watch))
    (let ((dir (car entry)))
      (if (not (file-directory-p dir))
          ;; The parent does not exist yet.  The resource that creates
          ;; it is in the index too, so a run will make it, and the
          ;; next start picks it up.
          (monk-watch--log "no watch on %s: directory absent" dir)
        (monk-watch--add-one watch dir)
        (when (monk-watch-file-watch-needed-p)
          (dolist (cell (cdr entry))
            (let ((path (car cell)))
              (when (and (file-exists-p path) (not (file-directory-p path)))
                (monk-watch--add-one watch path)))))))))

(defun monk-watch--adopt-new-file (watch file)
  "On kqueue, add FILE to WATCH when it is an owned path that just appeared."
  (when (and (eq (monk-watch-backend watch) 'filenotify)
             (monk-watch-file-watch-needed-p)
             (file-exists-p file)
             (not (file-directory-p file))
             (not (assoc file (monk-watch-watched watch)))
             (monk-watch--nodes-for-file watch file))
    (monk-watch--add-one watch file)))

;;;###autoload
(defun monk-watch-start (machines policy &rest options)
  "Watch the paths that POLICY manages on MACHINES and react to drift.
MACHINES is a machine or a list.  OPTIONS pass to every run.  Local
machines get filesystem notifications; remote ones are polled every
`monk-watch-poll-interval' seconds.  Return the watch."
  (when monk-watch--current
    (monk-watch-stop))
  (let* ((machines (if (listp machines) machines (list machines)))
         (local (seq-some #'monk-machine-local-p machines))
         (backend (and local (monk-watch--choose-backend)))
         (nodes (monk--nodes-for machines policy))
         (watch (monk-watch-create :machines machines :policy policy :options options
                                   :nodes nodes :backend backend
                                   :path-index (monk-watch--build-index nodes))))
    (when (and local (null backend))
      (monk-watch--log "warning: no local backend; local drift will not be seen"))
    (when (and (eq backend 'filenotify) (not (monk-watch-notifications-work-p)))
      (monk-watch--log "warning: file notifications are not delivered here; local drift will not be seen"))
    (setq monk-watch--current watch)
    (setf (monk-watch-path-signature watch) (monk-watch--path-signature nodes))
    (pcase backend
      ('fswatch (monk-watch--fswatch-start watch))
      ('filenotify (monk-watch--add-watches watch)))
    (when (and backend monk-watch-repath-interval)
      (setf (monk-watch-repath-timer watch)
            (run-at-time monk-watch-repath-interval monk-watch-repath-interval #'monk-watch--repath)))
    (when (seq-remove #'monk-machine-local-p machines)
      (setf (monk-watch-poll-timer watch)
            (run-at-time monk-watch-poll-interval monk-watch-poll-interval #'monk-watch--poll)))
    (setq monk-watch--unmanaged-seen nil
          monk-watch--unmanaged-count 0
          monk-watch--unmanaged-by-directory nil)
    (let ((remote (length (seq-remove #'monk-machine-local-p machines)))
          (local (length (seq-filter #'monk-machine-local-p machines))))
      (monk-watch--log "started: %s, %s%s, mode %s"
                       (cond ((and (> local 0) (> remote 0))
                              (format "%d machines: %d local, %d remote" (length machines) local remote))
                             ((> remote 0) (format "%d remote machine%s" remote (if (= 1 remote) "" "s")))
                             (t (format "%d local machine%s" local (if (= 1 local) "" "s"))))
                       (pcase backend
                         ('fswatch (format "fswatch over %d dir%s" (length (monk-watch-watched watch))
                                           (if (= 1 (length (monk-watch-watched watch))) "" "s")))
                         ('filenotify (format "%d filenotify watch%s%s"
                                              (length (monk-watch-descriptors watch))
                                              (if (= 1 (length (monk-watch-descriptors watch))) "" "es")
                                              (if (monk-watch-file-watch-needed-p) ", dirs and files on kqueue" "")))
                         (_ (if (> local 0) "no local backend" "no local machine")))
                       (if (> remote 0) (format ", remote polled every %ds" monk-watch-poll-interval) "")
                       monk-watch-mode))
    (run-hook-with-args 'monk-watch-functions 'started (list :watch watch))
    watch))

;;;###autoload
(defun monk-watch--cancel-repath (watch)
  "Cancel the repath timer of WATCH, if any."
  (when (monk-watch-repath-timer watch)
    (cancel-timer (monk-watch-repath-timer watch))
    (setf (monk-watch-repath-timer watch) nil))
  (when (monk-watch-repath-soon-timer watch)
    (cancel-timer (monk-watch-repath-soon-timer watch))
    (setf (monk-watch-repath-soon-timer watch) nil)))

(defun monk-watch-stop ()
  "Stop the active watch."
  (interactive)
  (let ((watch monk-watch--current))
    (when watch
      (dolist (desc (monk-watch-descriptors watch))
        (when (file-notify-valid-p desc)
          (file-notify-rm-watch desc)))
      (dolist (proc (list (monk-watch-process watch) (monk-watch-tree-process watch)))
        (when (process-live-p proc)
          (set-process-sentinel proc nil)
          (delete-process proc)))
      (when (monk-watch-debounce-timer watch)
        (cancel-timer (monk-watch-debounce-timer watch)))
      (when (monk-watch-poll-timer watch)
        (cancel-timer (monk-watch-poll-timer watch)))
      (monk-watch--cancel-repath watch)
      (setq monk-watch--current nil)
      (monk-watch--log "stopped after %d event%s (%d unmanaged), %d run%s%s"
                       (monk-watch-events watch) (if (= 1 (monk-watch-events watch)) "" "s")
                       monk-watch--unmanaged-count
                       (monk-watch-runs watch) (if (= 1 (monk-watch-runs watch)) "" "s")
                       (monk-watch--noisy-directories-string))
      (run-hook-with-args 'monk-watch-functions 'stopped (list :watch watch)))))

;;;###autoload
(defun monk-watch-toggle-mode ()
  "Switch between assisted and autopilot."
  (interactive)
  (setq monk-watch-mode (if (eq monk-watch-mode 'assisted) 'autopilot 'assisted))
  (monk-watch--log "mode %s" monk-watch-mode)
  (message "Monk watch: %s" monk-watch-mode))

(defun monk-watch--carry-over (watch events runs started &optional unmanaged by-directory)
  "Give WATCH the EVENTS and RUNS counts and the STARTED time of an earlier watch.
UNMANAGED and BY-DIRECTORY carry the unmanaged counts.  A reload stops
and starts the watch; the status line must not forget."
  (setf (monk-watch-events watch) events
        (monk-watch-runs watch) runs
        (monk-watch-started watch) started)
  (when unmanaged (setq monk-watch--unmanaged-count unmanaged))
  (when by-directory (setq monk-watch--unmanaged-by-directory by-directory)))

(defun monk-watch--path-signature (nodes)
  "Return the real paths every local node in NODES owns, sorted.
Equal signatures mean the kernel watches the same places."
  (let (paths)
    (dolist (node nodes)
      (when (monk-machine-local-p (monk-node-machine node))
        (dolist (path (monk-watch--node-paths node))
          (push (monk-watch-real-path path) paths))))
    (sort (delete-dups paths) #'string<)))

(defun monk-watch--repath ()
  "Resolve the watched paths again; when any moved, restart the watch on them.
Runs every `monk-watch-repath-interval' seconds.  Nothing runs when a
run is in progress; the next tick looks again."
  (let ((w monk-watch--current))
    (cond
     ((null w) nil)
     ((or monk-watch--running monk--current-run)
      ;; a run is on: look again right after it, not a tick later
      (monk-watch--repath-soon w))
     (t
      (let ((now (monk-watch--path-signature (monk-watch-nodes w))))
        (unless (equal now (monk-watch-path-signature w))
          (let ((moved (seq-difference now (monk-watch-path-signature w))))
            (monk-watch--log "paths moved: %s; watching again" (string-join moved ", "))
            (run-hook-with-args 'monk-watch-functions 'repath (list :watch w :moved moved)))
          (let ((machines (monk-watch-machines w))
                (policy (monk-watch-policy w))
                (options (monk-watch-options w))
                (events (monk-watch-events w))
                (runs (monk-watch-runs w))
                (started (monk-watch-started w)))
            (monk-watch-stop)
            (let ((new (apply #'monk-watch-start machines policy options)))
              (when new (monk-watch--carry-over new events runs started))))))))))

(defun monk-watch-status ()
  "Return a one-line description of the active watch, or nil."
  (when monk-watch--current
    (let ((w monk-watch--current))
      (format "%s, %s, %d dir(s), %d event(s) (%d unmanaged), %d run(s), up %s%s"
              monk-watch-mode (or (monk-watch-backend w) "no local backend")
              (length (monk-watch-watched w))
              (monk-watch-events w) monk-watch--unmanaged-count (monk-watch-runs w)
              (monk-format-duration (float-time (time-since (monk-watch-started w))) t)
              (monk-watch--noisy-directories-string)))))

(monk-note-load 'monk-watch)
(provide 'monk-watch)
;;; monk-watch.el ends here
