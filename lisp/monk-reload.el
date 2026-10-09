;;; monk-reload.el --- Reload Monk from source  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, maint

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

;; `M-x monk-reload' reloads every loaded monk-* library from source,
;; in the order they were first loaded, and fixes up what a plain
;; `load' leaves behind:
;;
;;  - Keymaps.  `monk-ui-mode-map' and `monk-tutorial-mode-map' are
;;    `defvar's, so a key added to their definition never appears on
;;    reload.  They are unbound first, and every live buffer in one of
;;    those modes gets the rebuilt map.
;;  - The active watch.  Its timers and filenotify callbacks hold
;;    closures over the old code.  The watch is stopped before the
;;    reload and started again after with the same machines, policy,
;;    options, and mode.  Pending drift is lost; the next event or the
;;    next poll finds it again.
;;  - The spinner timer, restarted only if a run was live, which a
;;    reload from the minibuffer never is.
;;  - The image cache, cleared so changed drawing code shows.
;;  - `monk-watch--delivery', reset so the probe runs again.
;;
;; EIEIO classes redefine in place; existing resource objects and
;; machines keep working.  Structs (`monk-result', `monk-run',
;; `monk-node', `monk-watch') are rebuilt by each run, and a run in
;; progress keeps the old code until it returns.  Changing a struct
;; slot still needs a restart for any stored run you want to keep
;; reading, for example the one in `*monk*'; the summary says so when
;; a `*monk*' buffer holds a run.
;;
;; The source file wins over a stale .elc through `load-prefer-newer'.
;; This file reloads itself first, so a fix to the reloader applies in
;; the same invocation.

;;; Code:

(require 'cl-lib)
(declare-function monk-note-load "monk" (feature))
(declare-function monk-stale-libraries "monk")
(defvar monk-version)
(defvar monk-build)
(defvar monk-report-function)
(defvar monk-progress-functions)
(require 'seq)

;; The reloader must load without the rest of Monk, so it names what it
;; touches instead of requiring it.
(defvar monk-watch-mode)
(defvar monk-watch--current)
(defvar monk-watch--delivery)
(defvar monk-ui--spinner-timer)
(defvar monk-ui--run)
(declare-function monk-watch-active-p "monk-watch")
(declare-function monk-watch-stop "monk-watch")
(declare-function monk-watch-start "monk-watch")
(declare-function monk-watch-machines "monk-watch")
(declare-function monk-watch-policy "monk-watch")
(declare-function monk-watch-events "monk-watch")
(declare-function monk-watch-runs "monk-watch")
(declare-function monk-watch-started "monk-watch")
(declare-function monk-watch-options "monk-watch")
(declare-function monk-ui--spinner-stop "monk-ui")
(declare-function monk-ui-refresh "monk-ui")
(declare-function monk-log "monk")

(defconst monk-reload-feature-prefix "monk"
  "Features whose names start with this are reloaded.")

(defconst monk-reload-never '(monk-reload monk-test)
  "Features the main pass skips.
The reloader handles itself separately; the test suite is not part of
the package.")

(defconst monk-reload-keymap-modes
  '((monk-ui-mode . monk-ui-mode-map)
    (monk-tutorial-mode . monk-tutorial-mode-map))
  "Modes whose buffers get a rebuilt keymap: (MODE . MAP-VARIABLE).")

(defconst monk-reload-caches '(monk-ui--bar-cache)
  "Hash-table variables holding rendered images.  Cleared after a reload.")

(defvar monk-reload--self-reloaded nil
  "Non-nil while `monk-reload' runs its freshly loaded self.")

(defun monk-reload-features ()
  "Return the loaded monk features, oldest first.
`features' lists the newest first; loading in first-load order keeps
dependencies ahead of dependents."
  (reverse
   (seq-filter (lambda (f)
                 (and (string-prefix-p monk-reload-feature-prefix (symbol-name f))
                      (not (memq f monk-reload-never))))
               features)))

(defun monk-reload--library-file (feature)
  "Return the file FEATURE was loaded from, without extension, or nil."
  (let ((file (or (locate-library (symbol-name feature))
                  (symbol-file feature 'provide))))
    (and file (file-name-sans-extension file))))

(defun monk-reload-keymap-variables ()
  "Return the bound `monk-...-map' variables that hold keymaps."
  (let (out)
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (string-prefix-p "monk-" (symbol-name sym))
                           (string-suffix-p "-map" (symbol-name sym))
                           (keymapp (symbol-value sym)))
                  (push sym out))))
    out))

(defun monk-reload--forget-after-load-forms ()
  "Drop `after-load-alist' entries that wait on monk features.
Bodies from earlier loads would run again with stale closures."
  (setq after-load-alist
        (seq-remove (lambda (entry)
                      (let ((key (car entry)))
                        (and (symbolp key)
                             (string-prefix-p monk-reload-feature-prefix (symbol-name key)))))
                    after-load-alist)))

(defun monk-reload--snapshot-watch ()
  "Return a plist describing the active watch, or nil, and stop it."
  (when (and (fboundp 'monk-watch-active-p) (monk-watch-active-p))
    (let ((w monk-watch--current))
      (prog1 (list :machines (monk-watch-machines w)
                   :policy (monk-watch-policy w)
                   :options (monk-watch-options w)
                   :mode monk-watch-mode
                   ;; the counters and the start time carry over: a
                   ;; reload is not a new watch to the person reading
                   ;; the status line
                   :events (monk-watch-events w)
                   :runs (monk-watch-runs w)
                   :started (monk-watch-started w))
        (monk-watch-stop)))))

(defun monk-reload--restore-watch (snapshot)
  "Start a watch again from SNAPSHOT, a plist from `monk-reload--snapshot-watch'."
  (when (and snapshot (fboundp 'monk-watch-start))
    (setq monk-watch-mode (plist-get snapshot :mode))
    (let ((w (apply #'monk-watch-start (plist-get snapshot :machines) (plist-get snapshot :policy)
                    (plist-get snapshot :options))))
      (when (and w (plist-get snapshot :started))
        ;; struct setters by name: this file loads before monk-watch and
        ;; must not depend on its accessors at compile time
        (funcall (intern "monk-watch--carry-over") w (plist-get snapshot :events)
                 (plist-get snapshot :runs) (plist-get snapshot :started)))
      w)))

(defun monk-reload--refresh-buffers ()
  "Install the rebuilt keymaps in live monk buffers.  Return the count."
  (let ((n 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (pcase-dolist (`(,mode . ,map) monk-reload-keymap-modes)
          (when (and (derived-mode-p mode) (boundp map))
            (use-local-map (symbol-value map))
            (when (eq mode 'monk-ui-mode)
              (when (fboundp 'monk-ui-refresh) (ignore-errors (monk-ui-refresh)))
              (force-mode-line-update))
            (cl-incf n)))))
    n))

(defun monk-reload--clear-caches ()
  "Empty the image caches.  Return how many were cleared."
  (let ((n 0))
    (dolist (sym monk-reload-caches)
      (when (and (boundp sym) (hash-table-p (symbol-value sym)))
        (clrhash (symbol-value sym))
        (cl-incf n)))
    n))

(defun monk-reload--stored-runs-p ()
  "Return non-nil when a `*monk*' buffer holds a run from before the reload."
  (seq-some (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'monk-ui-mode)
                     (bound-and-true-p monk-ui--run))))
            (buffer-list)))

;;;###autoload
(defun monk-reload (&optional verbose)
  "Reload every loaded Monk library from source, dependencies first.
With VERBOSE, a prefix argument, list each file as it loads.  Files
that fail to load are reported at the end; the rest still load.  An
active watch is stopped and started again on the new code.  See the
Commentary for what survives a reload and what needs a restart."
  (interactive "P")
  (if (and (not monk-reload--self-reloaded)
           (monk-reload--library-file 'monk-reload))
      (let ((monk-reload--self-reloaded t)
            (load-prefer-newer t))
        (load (monk-reload--library-file 'monk-reload) nil t)
        (funcall 'monk-reload verbose))
    (monk-reload--run verbose)))

(defvar monk-reload-last-failures nil
  "Alist of (FEATURE . MESSAGE) from the last reload, for tests and debugging.")

(defun monk-reload--run (verbose)
  "The body of `monk-reload', run from its freshly loaded definition.
With VERBOSE, list each file as it loads."
  (let ((load-prefer-newer t)
        (loaded 0)
        (failed nil)
        (start (float-time))
        (before (and (boundp 'monk-build) (format "%s %s" monk-version monk-build)))
        ;; what the caller had wired: a batch agent's report function and
        ;; its terminal progress; both must come back as they were
        (report-function (and (boundp 'monk-report-function) monk-report-function))
        (cli-on (and (fboundp 'monk-cli--progress)
                     (memq 'monk-cli--progress (bound-and-true-p monk-progress-functions))))
        (watch (monk-reload--snapshot-watch))
        (stored (monk-reload--stored-runs-p)))
    (when (and (boundp 'monk-ui--spinner-timer) monk-ui--spinner-timer)
      (monk-ui--spinner-stop))
    (dolist (sym (monk-reload-keymap-variables))
      (makunbound sym))
    (monk-reload--forget-after-load-forms)
    (dolist (feature (monk-reload-features))
      (let ((file (monk-reload--library-file feature)))
        (if (null file)
            (push (cons feature "no file found") failed)
          (condition-case err
              (progn
                (load file nil (not verbose))
                (cl-incf loaded))
            (error
             (push (cons feature (error-message-string err)) failed))))))
    (when (boundp 'monk-watch--delivery)
      (setq monk-watch--delivery nil))
    (when report-function
      (setq monk-report-function report-function))
    (when (and cli-on (fboundp 'monk-cli-enable))
      (funcall 'monk-cli-enable))
    (let ((buffers (monk-reload--refresh-buffers))
          (caches (monk-reload--clear-caches)))
      (condition-case err
          (monk-reload--restore-watch watch)
        (error (push (cons 'monk-watch (error-message-string err)) failed)))
      (setq monk-reload-last-failures (reverse failed))
      (let ((summary
             (format "monk: %s%s, reloaded %d librar%s in %.1fs, %d buffer%s rekeyed, %d cache%s cleared%s%s%s%s"
                     (if (boundp 'monk-build) (format "%s build %s" monk-version monk-build) "no version")
                     (if (and before (not (equal before (format "%s %s" monk-version monk-build))))
                         (format " (was %s)" before)
                       "")
                     loaded (if (= loaded 1) "y" "ies")
                     (- (float-time) start)
                     buffers (if (= buffers 1) "" "s")
                     caches (if (= caches 1) "" "s")
                     (if watch ", watch restarted" "")
                     (let ((stale (and (fboundp 'monk-stale-libraries) (monk-stale-libraries))))
                       (if stale
                           (format "; STALE after reload: %s" (mapconcat (lambda (c) (symbol-name (car c))) stale ", "))
                         ""))
                     (if stored "; a stored run in *monk* predates the reload: rerun it if a struct slot changed" "")
                     (if failed
                         (format "; %d failed: %s" (length failed)
                                 (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                                            (nreverse failed) ", "))
                       ""))))
        (when (fboundp 'monk-log) (monk-log 'info "%s" summary))
        (message "%s" summary)
        loaded))))

(when (fboundp 'monk-note-load) (monk-note-load 'monk-reload))
(provide 'monk-reload)
;;; monk-reload.el ends here
