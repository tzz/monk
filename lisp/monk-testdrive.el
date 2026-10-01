;;; monk-testdrive.el --- Live test drive and tour  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, convenience

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

;; Two commands for the question "does Monk work here" after an Emacs
;; upgrade, a TRAMP change, or a new machine, when the unit tests pass.
;;
;; `M-x monk-testdrive' is automatic.  It builds a throwaway home
;; under `temporary-file-directory', binds HOME and the ledger to it,
;; and runs every feature against it: converge, plan and apply,
;; requisites, flapping, drift watching, the agent ledger, the report
;; buffers.  Every check is done by code: it reads files, run results,
;; and the text and properties of the *monk* buffers.  Nothing asks
;; you anything while it runs.  Two minutes, then a report.  `C-u'
;; runs one chosen section.
;;
;; `M-x monk-testdrive-tour' is for your eyes.  It sets up one thing
;; at a time with nothing running underneath, shows the buffer, and
;; asks whether it looked right: the PLAN header, the bars, a detail
;; buffer with a diff, the APPLY comparison, the WATCH badge.  Three
;; minutes.
;;
;; Safety.  Every path the drive touches is under the throwaway home,
;; and it is deleted at the end.  No resource in the drive uses :sudo,
;; packages, cron, or systemd, so nothing outside the home changes.
;; Remote machines are not used; TRAMP is exercised by unit tests.
;;
;; The report, *monk-testdrive*, has the summary first, one line per
;; step, and the tail of *monk-log* under a failure.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'monk)
(require 'monk-file)
(require 'monk-system)
(require 'monk-inventory)
(defvar monk-history--db)
(defvar monk-history--db-path)
(require 'monk-ui)
(require 'monk-watch)
(require 'monk-event)
(require 'monk-agent)
(require 'filenotify)
(require 'vc-git)
(require 'monk-package)
(require 'monk-bridge)
(require 'monk-sync)
(require 'monk-cli)
(require 'monk-translate)
(require 'monk-embark)

;; The drive checks history when monk-history is loaded; it needs an
;; Emacs with SQLite, so it is not required here.
(declare-function monk-history-runs "monk-history")
(declare-function monk-history-run "monk-history")
(declare-function monk-history-repairs "monk-history")

(defgroup monk-testdrive nil
  "Live test drive of Monk."
  :group 'monk
  :prefix "monk-testdrive-")

(defconst monk-testdrive-buffer-name "*monk-testdrive*"
  "The report buffer.")

(defconst monk-testdrive-statuses '(PASS FAIL SKIP)
  "Outcomes a step can record.")

(defconst monk-testdrive-log-tail-lines 12
  "Lines of *monk-log* shown under a failed step.")

(defcustom monk-testdrive-step-budget 20
  "Seconds a step may take before the report marks it SLOW."
  :type 'natnum)

(defvar monk-testdrive--home nil
  "The throwaway home directory, with a trailing slash.")

(defvar monk-testdrive--results nil
  "List of (SECTION NAME STATUS NOTE SECONDS), newest first.")

(defvar monk-testdrive--layout nil
  "Window configuration at the start.
The drive restores it after each step and keeps the report beside it.
The tour restores it only at the end: while it runs, the frame holds
`*monk*' alone, full width, so the bars and columns fit.")

(defvar monk-testdrive--touring nil
  "Non-nil while the tour runs.  Changes what the step macro does with windows.")

(defvar monk-testdrive--summary-marker nil
  "Where the summary goes in the report.")

(defvar monk-testdrive--waited 0
  "Seconds the current step spent waiting for the user; not the step's time.")

(define-error 'monk-testdrive-quit "test drive stopped")

;;;; Report

(defface monk-testdrive-pass '((t :inherit success)) "A passed step.")
(defface monk-testdrive-fail '((t :inherit error)) "A failed step.")
(defface monk-testdrive-skip '((t :inherit shadow)) "A skipped step.")

(define-derived-mode monk-testdrive-report-mode special-mode "Testdrive"
  "The test drive report.  `q' buries it."
  (setq-local truncate-lines nil))

(defun monk-testdrive--buf ()
  "Return the report buffer in its mode."
  (let ((buf (get-buffer-create monk-testdrive-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'monk-testdrive-report-mode)
        (monk-testdrive-report-mode)))
    buf))

(defun monk-testdrive--fontify (start end)
  "Color the status words between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\(PASS\\|FAIL\\|SKIP\\) " end t)
      (put-text-property (match-beginning 1) (match-end 1) 'face
                         (pcase (match-string 1)
                           ("PASS" 'monk-testdrive-pass)
                           ("FAIL" 'monk-testdrive-fail)
                           (_ 'monk-testdrive-skip))))))

(defun monk-testdrive--out (format-string &rest args)
  "Append FORMAT-STRING with ARGS to the report."
  (with-current-buffer (monk-testdrive--buf)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert (apply #'format format-string args) "\n")
        (monk-testdrive--fontify start (point))))
    (when-let* ((w (get-buffer-window (current-buffer) t)))
      (set-window-point w (point-max)))))

(defun monk-testdrive--show-report ()
  "Keep the report visible in a side window without taking focus.
Not during the tour: the tour shows the report once, at the end."
  (unless (or noninteractive monk-testdrive--touring)
    (let ((buf (monk-testdrive--buf)))
      (unless (get-buffer-window buf)
        (display-buffer buf '(display-buffer-in-side-window (side . right) (window-width . 0.4)))))))

(defun monk-testdrive--restore-layout ()
  "Restore the windows the drive started with, plus the report.
During the tour do nothing: each stop lays the frame out itself, and
the original layout comes back once at the end."
  (unless monk-testdrive--touring
    (when (and monk-testdrive--layout (window-configuration-p monk-testdrive--layout))
      (set-window-configuration monk-testdrive--layout))
    (monk-testdrive--show-report)))

(defun monk-testdrive--log-tail ()
  "Return the last lines of *monk-log*, or nil."
  (when-let* ((buf (get-buffer monk-log-buffer-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- monk-testdrive-log-tail-lines))
        (buffer-substring-no-properties (point) (point-max))))))

(defun monk-testdrive--record (section name status &optional note secs)
  "Record STATUS for step NAME in SECTION with NOTE and SECS."
  (push (list section name status note secs) monk-testdrive--results)
  (monk-testdrive--out "%s %s%s" status name (if secs (format "  (%.1fs)" secs) ""))
  (when (and note (not (string-empty-p note)))
    (monk-testdrive--out "    %s" note))
  (when (eq status 'FAIL)
    (when-let* ((tail (monk-testdrive--log-tail)))
      (monk-testdrive--out "    log:\n%s" (replace-regexp-in-string "^" "    | " tail t t)))))

;;;; Steps

(defmacro monk-testdrive--step (section name &rest body)
  "Run BODY as step NAME in SECTION, recording outcome and timing.
BODY returns PASS, FAIL, SKIP, or (STATUS . NOTE).  Errors become FAIL."
  (declare (indent 2))
  `(let ((t0 (float-time))
         (monk-testdrive--waited 0))
     (message "monk-testdrive: %s" ,name)
     (condition-case err
         (let* ((r (unwind-protect (progn ,@body)
                     (monk-testdrive--restore-layout)))
                (secs (- (float-time) t0 monk-testdrive--waited))
                (slow (and (> secs monk-testdrive-step-budget)
                           (format "SLOW: %.0fs, budget %ds" secs monk-testdrive-step-budget))))
           (pcase r
             (`(,st . ,note) (monk-testdrive--record ,section ,name st
                                                     (if slow (concat note "  " slow) note) secs))
             (st (monk-testdrive--record ,section ,name (or st 'PASS) slow secs))))
       (monk-testdrive-quit (signal 'monk-testdrive-quit nil))
       (quit (signal 'monk-testdrive-quit nil))
       (error (monk-testdrive--record ,section ,name 'FAIL
                                      (format "error: %s" (error-message-string err))
                                      (- (float-time) t0 monk-testdrive--waited))))))

(defun monk-testdrive--check (ok &optional note)
  "Return PASS when OK, else FAIL with NOTE."
  (if ok 'PASS (cons 'FAIL note)))

(defun monk-testdrive--checks (&rest pairs)
  "Return PASS when every (OK . NOTE) in PAIRS is OK.
Otherwise return FAIL with the NOTE of the first miss."
  (let ((miss (seq-find (lambda (p) (not (car p))) pairs)))
    (if miss (cons 'FAIL (cdr miss)) 'PASS)))

;;;; Asking (tour only)

(defun monk-testdrive--read-char (prompt choices)
  "Read one of CHOICES after PROMPT; the wait does not count as step time."
  (let ((t0 (float-time)))
    (unwind-protect (read-char-choice prompt choices)
      (cl-incf monk-testdrive--waited (- (float-time) t0)))))

(defun monk-testdrive--focus-monk ()
  "Make `*monk*' the only window in the frame and select it.
The tour owns the frame while it runs: your file and the report would
squeeze `*monk*' into a third of the width, and its header line and
columns need the whole of it.  The layout you had comes back when the
tour ends."
  (let ((buffer (monk-ui--buffer)))
    ;; `delete-other-windows' refuses to run from a side window, and
    ;; the report may be one.  Start from a live window in the main
    ;; area.  `window-main-window' is a parent when the main area is
    ;; split, and `select-window' wants a leaf, so take the first leaf.
    (when (window-parameter (selected-window) 'window-side)
      (select-window (or (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                                   (window-list nil 'no-minibuffer))
                         (frame-first-window))))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (monk-ui-refresh))
    (force-mode-line-update t)))

(defun monk-testdrive--focus-detail (name)
  "Show the detail buffer for machine NAME below `*monk*', and select it.
The buffer is built without `pop-to-buffer', so `display-buffer-alist'
has no say: exactly one window shows it, under `*monk*'."
  (monk-testdrive--focus-monk)
  (let ((detail (with-current-buffer monk-ui-buffer-name (monk-ui-detail-buffer name)))
        ;; Through `display-buffer', so the window remembers it was made
        ;; for this buffer and `q' deletes it instead of showing *monk*
        ;; twice.  The overriding action keeps `display-buffer-alist' out.
        (display-buffer-overriding-action
         (list #'display-buffer-below-selected
               (cons 'window-height (lambda (w) (fit-window-to-buffer w nil (- (frame-height) 8)))))))
    (pop-to-buffer detail)
    (goto-char (point-min))))

(defun monk-testdrive--windows-showing (buffer)
  "Return how many windows on any frame show BUFFER, with their frames' names."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (cons (length windows)
          (mapcar (lambda (w) (format "%s%s" (frame-parameter (window-frame w) 'name)
                                      (if (window-parameter w 'window-side) " (side window)" "")))
                  windows))))

(defun monk-testdrive--ask (prompt)
  "Ask PROMPT about what is on screen; return PASS, FAIL with a note, or SKIP."
  (redisplay)
  (pcase (monk-testdrive--read-char (concat prompt "  [y]es/[n]o/[s]kip/[q]uit ") '(?y ?n ?s ?q))
    (?y 'PASS)
    (?n (cons 'FAIL (let ((t0 (float-time)))
                      (unwind-protect (read-string "What did you see? ")
                        (cl-incf monk-testdrive--waited (- (float-time) t0))))))
    (?s 'SKIP)
    (_ (signal 'monk-testdrive-quit nil))))

;;;; The throwaway home

(defconst monk-testdrive--seed-files
  '(("dotfiles/zshrc" . "export DRIVE=1\n")
    ("dotfiles/gitconfig" . "[user]\n\tname = Drive\n")
    ("etc/motd" . "old motd\n"))
  "Files that exist before any policy runs: (RELATIVE . CONTENT).")

(defun monk-testdrive--make-home ()
  "Create the throwaway home and return it with a trailing slash."
  (let ((dir (file-name-as-directory (make-temp-file "monk-testdrive-" t))))
    (dolist (f monk-testdrive--seed-files)
      (let ((path (expand-file-name (car f) dir)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert (cdr f)))))
    dir))

(defun monk-testdrive--path (rel)
  "Return REL under the throwaway home."
  (expand-file-name rel monk-testdrive--home))

(defun monk-testdrive--text (rel)
  "Return the content of REL under the home, or nil."
  (let ((p (monk-testdrive--path rel)))
    (and (file-exists-p p)
         (with-temp-buffer (insert-file-contents p) (buffer-string)))))

(defun monk-testdrive--machine ()
  "Return a local machine named for the drive.
Facts are gathered once; `home' is overridden to the throwaway home
so `~' in the policy lands there."
  (let ((m (monk-local-machine :name "drive")))
    (monk-gather-facts m)
    (setf (alist-get 'home (oref m facts)) (directory-file-name monk-testdrive--home))
    m))

;;;; The policy the drive uses

(defun monk-testdrive--policy (m)
  "The drive's policy for M: dotfiles, a config file, a handler."
  (monk-directory "~/.ssh" :mode #o700)
  (monk-directory "~/.config/app")
  (monk-symlink "~/.zshrc" "~/dotfiles/zshrc")
  (monk-symlink "~/.gitconfig" "~/dotfiles/gitconfig")
  (monk-file "~/.config/app/app.conf" :content "level=3\n" :mode #o600)
  (monk-line-in-file "~/etc/motd" "welcome" :regexp "^welcome")
  (monk-command "app-reload" "sh"
                :args (list "-c" (format "echo reloaded >> %s" (monk-expand "~/.config/app/reloads")))
                :watch '("~/.config/app/app.conf"))
  (when (monk-class-p m 'drive)
    (monk-file "~/class-marker" :content "drive\n")))

;;;; Sections

(defun monk-testdrive--section-isolation ()
  "Prove the drive stands apart from this Emacs: no user state in, none out."
  (monk-testdrive--out "\n## Isolation")
  (monk-testdrive--step "isolation" "HOME and MONK_HOME point into the throwaway home"
    (let ((home (directory-file-name monk-testdrive--home)))
      (monk-testdrive--checks
       (cons (equal (getenv "HOME") home) (format "HOME is %s" (getenv "HOME")))
       (cons (string-prefix-p home (getenv monk-home-environment-variable))
             (format "%s is %s" monk-home-environment-variable (getenv monk-home-environment-variable)))
       (cons (string-prefix-p home (monk-home-directory)) (format "monk-home-directory %s" (monk-home-directory)))
       ;; this one reads ~ on purpose: it proves the redirection  [isolation-probe]
       (cons (string-prefix-p home (expand-file-name "~/x")) (format "~ expands to %s" (expand-file-name "~"))))))
  (monk-testdrive--step "isolation" "the history database and fact cache are the drive's own"
    (if (not (featurep 'monk-history))
        (cons 'SKIP "monk-history not loaded")
      (let ((path (monk-history-path)))
        (monk-testdrive--checks
         (cons (string-prefix-p (directory-file-name monk-testdrive--home) path) (format "history at %s" path))
         (cons (null (monk-history-runs :limit 1)) "history is not empty at the start")))))
  (monk-testdrive--step "isolation" "inventory, batch policy, and resource index start empty"
    (monk-testdrive--checks
     (cons (null monk-inventory) (format "inventory has %d machine(s)" (length monk-inventory)))
     (cons (null monk-inventory-derived) "derived group rules present")
     (cons (null monk-batch-policy) (format "batch policy is %S" monk-batch-policy))
     (cons (= 0 (hash-table-count monk-resource-index)) (format "%d indexed names" (hash-table-count monk-resource-index)))))
  (monk-testdrive--step "isolation" "every Monk user option is at its default"
    (let ((off (delq nil (mapcar (lambda (sym)
                                   (and (get sym 'standard-value)
                                        (not (equal (symbol-value sym) (eval (car (get sym 'standard-value)) t)))
                                        ;; the drive sets these two itself
                                        (not (memq sym '(monk-log-level monk-ui-style)))
                                        sym))
                                 (monk-testdrive-option-symbols)))))
      (monk-testdrive--check (null off) (format "customized: %S" off))))
  (monk-testdrive--step "isolation" "no step names a path outside the throwaway home"
    ;; Read the drive's own source: a literal home-relative path or a
    ;; guess at the user's disk is a regression in the drive itself.
    (let* ((file (or (locate-library "monk-testdrive.el") (locate-library "monk-testdrive")))
           (source (and file (concat (file-name-sans-extension file) ".el")))
           (bad nil))
      (if (not (and source (file-readable-p source)))
          (cons 'SKIP "cannot find the drive's source")
        (with-temp-buffer
          (insert-file-contents source)
          (goto-char (point-min))
          (while (re-search-forward "\\(?:expand-file-name\\|insert-file-contents\\|file-exists-p\\|find-file\\(?:-noselect\\)?\\) \"\\(~/[^\"]*\\|/Users/[^\"]*\\|/home/[^\"]*\\)\"" nil t)
            (unless (save-excursion (forward-line -1) (looking-at-p ".*\\[isolation-probe\\]"))
              (push (format "%d: %s" (line-number-at-pos) (match-string 1)) bad))))
        (monk-testdrive--check (null bad) (format "user paths in the drive: %S" (nreverse bad)))))))

(defun monk-testdrive--section-converge ()
  "Converge, converge again, and inspect the results."
  (monk-testdrive--out "\n## Converge")
  (let ((m (monk-testdrive--machine)) first second)
    (monk-testdrive--step "converge" "first run repairs everything"
      (setq first (monk-converge m #'monk-testdrive--policy))
      (monk-testdrive--checks
       (cons (monk-run-ok-p first) "run not ok")
       (cons (= 8 (monk-run-count first monk-status-repaired))
             (format "expected 8 repaired, got %s" (monk-run-summary first)))
       (cons (monk-run-converged-p first) "not converged")
       (cons (= 2 (monk-run-passes first)) (format "expected 2 passes, got %d" (monk-run-passes first)))))
    (monk-testdrive--step "converge" "files are as the policy says"
      (monk-testdrive--checks
       (cons (equal (expand-file-name "dotfiles/zshrc" monk-testdrive--home)
                    (file-symlink-p (monk-testdrive--path ".zshrc")))
             "~/.zshrc is not a link into dotfiles")
       (cons (= #o700 (logand (file-modes (monk-testdrive--path ".ssh")) #o7777)) ".ssh mode")
       (cons (equal "level=3\n" (monk-testdrive--text ".config/app/app.conf")) "app.conf content")
       (cons (equal "old motd\nwelcome\n" (monk-testdrive--text "etc/motd")) "motd line")
       (cons (equal "drive\n" (monk-testdrive--text "class-marker")) "class-gated file")))
    (monk-testdrive--step "converge" "watch handler ran once"
      (monk-testdrive--check (equal "reloaded\n" (monk-testdrive--text ".config/app/reloads"))
                             (format "reloads file: %S" (monk-testdrive--text ".config/app/reloads"))))
    (monk-testdrive--step "converge" "second run is all kept in one pass"
      (setq second (monk-converge m #'monk-testdrive--policy))
      (monk-testdrive--checks
       (cons (= 8 (monk-run-count second monk-status-kept)) (monk-run-summary second))
       (cons (= 1 (monk-run-passes second)) "more than one pass")
       (cons (equal "reloaded\n" (monk-testdrive--text ".config/app/reloads")) "handler ran again")))
    (monk-testdrive--step "converge" "report buffer shows one row, kept"
      (monk-ui-show second)
      (with-current-buffer (get-buffer monk-ui-buffer-name)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (monk-testdrive--checks
           (cons (string-match-p "^ ?drive " text) "no row for drive")
           (cons (string-match-p "kept" text) "no kept badge")
           (cons (string-match-p "APPLY" (substring-no-properties (monk-ui--header-line))) "header lacks APPLY")))))))

(defun monk-testdrive--section-plan ()
  "Plan, drift, apply, compare."
  (monk-testdrive--out "\n## Plan and apply")
  (let ((m (monk-testdrive--machine)) plan apply)
    (with-temp-file (monk-testdrive--path ".config/app/app.conf") (insert "level=9\n"))
    (delete-file (monk-testdrive--path ".gitconfig"))
    (monk-testdrive--step "plan" "plan sees two changes and touches nothing"
      (setq plan (monk-plan m #'monk-testdrive--policy))
      (monk-testdrive--checks
       (cons (monk-run-dry-run plan) "not a dry run")
       (cons (= 3 (monk-run-count plan monk-status-would-repair))
             (format "expected 3 would-repair (conf, link, handler), got %s" (monk-run-summary plan)))
       (cons (equal "level=9\n" (monk-testdrive--text ".config/app/app.conf")) "plan changed the file")))
    (monk-testdrive--step "plan" "plan buffer says PLAN and has a Would column"
      (monk-ui-show plan)
      (with-current-buffer (get-buffer monk-ui-buffer-name)
        (monk-testdrive--checks
         (cons (string-match-p "PLAN" (substring-no-properties (monk-ui--header-line))) "header lacks PLAN")
         (cons (equal "Would" (car (aref tabulated-list-format 4))) "column is not Would")
         (cons (eq 'monk-ui-plan-badge
                   (get-text-property 1 'face (monk-ui--header-line)))
               "PLAN badge face missing"))))
    (monk-testdrive--step "plan" "diff is in the detail buffer"
      (with-current-buffer (get-buffer monk-ui-buffer-name)
        (monk-ui-detail-buffer "drive"))
      (with-current-buffer (get-buffer (format monk-ui-detail-buffer-format "drive"))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (monk-testdrive--checks
           (cons (string-match-p "^-level=9$" text) "no removed line in diff")
           (cons (string-match-p "^\\+level=3$" text) "no added line in diff")
           (cons (save-excursion (goto-char (point-min))
                                 (re-search-forward "^\\+level=3$" nil t)
                                 (eq 'diff-added (get-text-property (match-beginning 0) 'face)))
                 "diff line not fontified")))))
    ;; Drift between plan and apply: someone restores the link by hand.
    (make-symbolic-link (expand-file-name "dotfiles/gitconfig" monk-testdrive--home)
                        (monk-testdrive--path ".gitconfig"))
    (monk-testdrive--step "plan" "apply reports the hand-made change as unfulfilled"
      (setq apply (monk-apply plan))
      (let ((comparison (monk-run-compare plan apply)))
        (monk-testdrive--checks
         (cons (eq plan (monk-run-plan apply)) "apply lost its plan")
         (cons (eq monk-plan-unfulfilled
                   (alist-get (cons "drive" "~/.gitconfig") comparison nil nil #'equal))
               (format "gitconfig verdict: %S" comparison))
         (cons (eq monk-plan-as-planned
                   (alist-get (cons "drive" "~/.config/app/app.conf") comparison nil nil #'equal))
               "app.conf not as planned")
         (cons (equal "level=3\n" (monk-testdrive--text ".config/app/app.conf")) "apply did not repair"))))
    (monk-testdrive--step "plan" "apply buffer has a Plan column with the verdict"
      (monk-ui-show apply)
      (with-current-buffer (get-buffer monk-ui-buffer-name)
        (let* ((row (cadr (car (monk-ui--entries))))
               (cell (substring-no-properties (aref row 7))))
          (monk-testdrive--checks
           (cons (= 9 (length tabulated-list-format)) "no Plan column")
           (cons (string-match-p "unfulfilled\\|○ 1\\|o 1" cell) (format "plan cell: %S" cell))
           (cons (string-match-p "from plan" (substring-no-properties (monk-ui--header-line)))
                 "header lacks 'from plan'")))))))

(defun monk-testdrive--section-graph ()
  "Requisites, thresholds, and flapping."
  (monk-testdrive--out "\n## Requisites and convergence")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "graph" "require orders and gates"
      (let ((run (monk-converge
                  m (lambda (_m)
                      (monk-command "after" "true" :creates "~/never" :require '("~/gate"))
                      (monk-line-in-file "~/absent-file" "x")
                      (monk-command "gated-off" "true" :creates "~/never" :require '("~/absent-file#x"))
                      (monk-directory "~/gate"))
                  :passes 1)))
        (let* ((names (mapcar #'monk-result-name (monk-run-results run)))
               (pos (lambda (n) (cl-position n names :test #'equal))))
          (monk-testdrive--checks
           (cons (< (funcall pos "~/gate") (funcall pos "after")) (format "order: %S" names))
           (cons (< (funcall pos "~/absent-file#x") (funcall pos "gated-off")) (format "order: %S" names))
           (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "after"))))
                 "after did not run")
           (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "gated-off"))))
                 "gated-off ran")))))
    (monk-testdrive--step "graph" "a bad ref is refused before anything runs"
      (condition-case err
          (progn (monk-converge m (lambda (_m) (monk-directory "~/x" :require '("nope"))))
                 (cons 'FAIL "no error"))
        (monk-requisite-error
         (monk-testdrive--check (string-match-p "matches no resource" (error-message-string err))
                                (error-message-string err)))))
    (monk-testdrive--step "graph" "two resources fighting are reported as flapping"
      (let ((run (monk-converge
                  m (lambda (_m)
                      (monk-file "~/fight" :content "a\n")
                      (monk-line-in-file "~/fight" "b" :create t))
                  :passes 6)))
        (monk-testdrive--checks
         (cons (monk-run-flapping-p run) "not flagged as flapping")
         (cons (= 2 (monk-run-passes run)) (format "%d passes, expected 2" (monk-run-passes run)))
         (cons (string-match-p "flapping" (monk-run-report-string run)) "report lacks flapping"))))
    (monk-testdrive--step "graph" "group threshold across two machines"
      (let* ((a (monk-testdrive--machine)) (b (monk-testdrive--machine)) (c (monk-testdrive--machine)))
        (oset a name "a") (oset b name "b") (oset c name "c")
        (dolist (x (list a b c)) (oset x groups '(pool)) (oset x classes (monk-hard-classes x)))
        (let ((run (monk-orchestrate
                    (list a b c)
                    (lambda (m)
                      (pcase (oref m name)
                        ("c" (monk-command "gate" "true" :creates "~/never"
                                           :require '((:ref (pool . "member") :min 1))))
                        ("a" (monk-command "member" "true" :creates "~/never" :only-if (lambda (_) nil)))
                        ("b" (monk-command "member" "true" :creates "~/never"))))
                    :passes 1)))
          (monk-testdrive--check
           (eq monk-status-repaired (monk-result-status (monk-run-result run '("c" . "gate"))))
           (monk-run-report-string run)))))))

(defun monk-testdrive--section-watch ()
  "Drift watching, with real notifications when the kernel delivers them."
  (monk-testdrive--out "\n## Watch")
  (let ((m (monk-testdrive--machine))
        (monk-watch-debounce 0.1)
        (monk-watch-settle 0.0)
        (monk-report-function #'ignore)
        (delivered (let ((monk-watch--delivery nil) (monk-watch-probe-timeout 0.5))
                     (monk-watch-notifications-work-p))))
    (monk-converge m #'monk-testdrive--policy)
    (monk-testdrive--step "watch" "notifications are delivered on this host"
      (if delivered 'PASS
        (cons 'SKIP "kernel delivers no events here (sandbox or network fs); watch steps use injected events")))
    (monk-testdrive--step "watch" "watch indexes the managed paths"
      (let ((monk-watch--delivery (if delivered 'yes 'no)))
        (unwind-protect
            (progn
              (monk-watch-start m #'monk-testdrive--policy)
              (let ((w monk-watch--current))
                (monk-testdrive--checks
                 (cons (monk-watch-active-p) "not active")
                 (cons (>= (length (monk-watch-path-index w)) 4)
                       (format "%d dirs indexed" (length (monk-watch-path-index w))))
                 (cons (monk-watch--nodes-for-file w (monk-testdrive--path ".config/app/app.conf"))
                       "app.conf not mapped to a node"))))
          (monk-watch-stop))))
    (monk-testdrive--step "watch" "assisted: drift yields a plan restricted to the resource"
      (let ((monk-watch-mode 'assisted) (monk-watch--delivery (if delivered 'yes 'no)) runs)
        (add-hook 'monk-watch-functions (lambda (e d) (when (eq e 'run) (push (plist-get d :run) runs))))
        (unwind-protect
            (progn
              (monk-watch-start m #'monk-testdrive--policy)
              (with-temp-file (monk-testdrive--path ".config/app/app.conf") (insert "drift\n"))
              (unless delivered
                (monk-watch--on-event (list 'd 'changed (monk-testdrive--path ".config/app/app.conf"))))
              (let ((deadline (+ (float-time) 3)))
                (while (and (null runs) (< (float-time) deadline))
                  (accept-process-output nil 0.05) (sit-for 0.02)))
              (let ((run (car runs)))
                (monk-testdrive--checks
                 (cons run "no run fired")
                 (cons (and run (monk-run-dry-run run)) "assisted run was not a plan")
                 (cons (and run (eq monk-status-would-repair
                                    (monk-result-status (monk-run-result run '("drive" . "~/.config/app/app.conf")))))
                       "conf not would-repair")
                 (cons (and run (eq monk-status-skipped
                                    (monk-result-status (monk-run-result run '("drive" . "~/.zshrc")))))
                       "unrelated resource was not skipped")
                 (cons (equal "drift\n" (monk-testdrive--text ".config/app/app.conf")) "assisted repaired"))))
          (remove-hook 'monk-watch-functions (car monk-watch-functions))
          (monk-watch-stop))))
    (monk-testdrive--step "watch" "autopilot: drift is repaired and the handler runs"
      (let ((monk-watch-mode 'autopilot) (monk-watch--delivery (if delivered 'yes 'no))
            (before (length (split-string (or (monk-testdrive--text ".config/app/reloads") "") "\n" t))))
        (unwind-protect
            (progn
              (monk-watch-start m #'monk-testdrive--policy)
              (unless delivered
                (monk-watch--on-event (list 'd 'changed (monk-testdrive--path ".config/app/app.conf"))))
              (when delivered
                (with-temp-file (monk-testdrive--path ".config/app/app.conf") (insert "drift2\n")))
              (let ((deadline (+ (float-time) 3)))
                (while (and (< (monk-watch-runs monk-watch--current) 1) (< (float-time) deadline))
                  (accept-process-output nil 0.05) (sit-for 0.02)))
              (let ((after (length (split-string (or (monk-testdrive--text ".config/app/reloads") "") "\n" t))))
                (monk-testdrive--checks
                 (cons (equal "level=3\n" (monk-testdrive--text ".config/app/app.conf")) "not repaired")
                 (cons (= after (1+ before)) (format "handler ran %d time(s), expected 1" (- after before))))))
          (monk-watch-stop))))))

(defun monk-testdrive--section-agent ()
  "Ledger, event round trip, query handler."
  (monk-testdrive--out "\n## Agent and events")
  (let ((m (monk-testdrive--machine))
        (monk-agent-ledger-file (monk-testdrive--path "state/events.ndjson")))
    (monk-testdrive--step "agent" "a run round-trips through JSON Lines"
      (let* ((run (monk-converge m #'monk-testdrive--policy))
             (lines (mapcar #'monk-event-line (monk-run-events run)))
             (back (monk-events-to-run (mapcar (lambda (l) (monk-event-parse (string-trim l))) lines))))
        (monk-testdrive--checks
         (cons (= (+ 2 (length (monk-run-results run))) (length lines)) "line count")
         (cons (= (length (monk-run-results run)) (length (monk-run-results back))) "result count")
         (cons (equal (monk-run-passes run) (monk-run-passes back)) "passes"))))
    (monk-testdrive--step "agent" "the ledger grows and status reads it back"
      (let ((run (monk-converge m #'monk-testdrive--policy)))
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
          (monk-agent-record run))
        (let* ((out (with-output-to-string (monk-agent-handle '((op . "status")))))
               (resp (monk-event-parse (string-trim out))))
          (monk-testdrive--checks
           (cons (file-exists-p monk-agent-ledger-file) "no ledger")
           (cons (eq t (alist-get 'ok resp)) "status not ok")
           (cons (equal "run-end" (alist-get 'event (alist-get 'last_run resp))) "no last run")))))
    (monk-testdrive--step "agent" "the unit files are well formed"
      (let ((monk-agent-emacs "/usr/bin/emacs")
            (monk-agent-load-path "/opt/monk/lisp")
            (monk-agent-policy-file "/etc/monk/policy.el"))
        (monk-testdrive--checks
         (cons (string-match-p "^\\[Service\\]$" (monk-agent-service-unit)) "service unit")
         (cons (string-match-p "^Accept=yes$" (monk-agent-socket-unit)) "socket unit")
         (cons (string-match-p "^StandardInput=socket$" (monk-agent-template-unit)) "template unit"))))))

(defun monk-testdrive--section-guards ()
  "CELL and CEL guards, skip reasons, the Salt failure model, query."
  (monk-testdrive--out "\n## Guards and failures")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "guards" ":when skips with the false clause named"
      (let ((run (monk-converge
                  m (lambda (_m)
                      (monk-directory "~/g-cell" :when '(and (class drive) (class mars)))
                      (monk-directory "~/g-cel" :when "'drive' in classes && facts.kernel != 'Plan9'")
                      (monk-directory "~/g-fn" :only-if (lambda (_m) nil))))))
        (monk-testdrive--checks
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/g-cell"))))
               "CELL guard did not skip")
         (cons (equal "class mars is absent"
                      (monk-result-comment (monk-run-result run '("drive" . "~/g-cell"))))
               (format "reason: %S" (monk-result-comment (monk-run-result run '("drive" . "~/g-cell")))))
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/g-cel"))))
               "CEL guard did not run")
         (cons (equal monk-skip-only-if
                      (monk-result-comment (monk-run-result run '("drive" . "~/g-fn"))))
               "only-if reason"))))
    (monk-testdrive--step "guards" "a bad guard fails the compile, not the run"
      (condition-case err
          (progn (monk-compile m (lambda (_m) (monk-directory "~/x" :when "nope == 1")))
                 (cons 'FAIL "compiled"))
        (monk-policy-error
         (monk-testdrive--checks
          (cons (string-match-p "bad :when guard" (cadr err)) (cadr err))))))
    (monk-testdrive--step "guards" "CEL round trip and explain"
      (let* ((form (monk-cel-parse "'drive' in classes && size(classes) > 2"))
             (cel (monk-cell-to-cel form)))
        (monk-testdrive--checks
         (cons (equal form (monk-cel-parse cel)) (format "round trip: %s" cel))
         (cons (monk-cell-eval m form) "guard false on the drive machine")
         (cons (string-match-p "class mars is absent"
                               (monk-cell-explain m '(and (class drive) (class mars))))
               "explain"))))
    (monk-testdrive--step "guards" "a failure does not stop the pass; only dependents skip"
      (let ((run (monk-converge
                  m (lambda (_m)
                      (monk-line-in-file "~/absent" "x")
                      (monk-directory "~/free")
                      (monk-directory "~/dependent" :require '("~/absent#x"))))))
        (monk-testdrive--checks
         (cons (eq monk-status-failed (monk-result-status (monk-run-result run '("drive" . "~/absent#x"))))
               "absent#x not failed")
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/free"))))
               "free did not run after the failure")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/dependent"))))
               "dependent ran")
         (cons (= 2 (monk-run-passes run)) (format "%d passes" (monk-run-passes run))))))
    (monk-testdrive--step "guards" "git resource clones and fast-forwards through VC"
      (if (not (executable-find vc-git-program))
          (cons 'SKIP "no git on this host")
        (let* ((origin (monk-testdrive--path "origin.git"))
               (work (monk-testdrive--path "seed"))
               (default-directory (file-name-as-directory monk-testdrive--home)))
          (make-directory work t)
          (let ((default-directory (file-name-as-directory work)))
            (vc-git--out-ok "init" "-q" "-b" "main")
            (with-temp-file (expand-file-name "f" work) (insert "one\n"))
            (vc-git--out-ok "-c" "user.name=drive" "-c" "user.email=d@x" "add" "f")
            (vc-git--out-ok "-c" "user.name=drive" "-c" "user.email=d@x" "commit" "-q" "-m" "one"))
          (vc-git--out-ok "clone" "-q" "--bare" work origin)
          (let* ((policy (lambda (_m) (monk-git "~/clone" origin :branch "main")))
                 (first (monk-converge m policy))
                 (_ (let ((default-directory (file-name-as-directory work)))
                      (with-temp-file (expand-file-name "f" work) (insert "two\n"))
                      (vc-git--out-ok "-c" "user.name=drive" "-c" "user.email=d@x" "commit" "-q" "-am" "two")
                      (vc-git--out-ok "push" "-q" origin "main")))
                 (plan (monk-plan m policy))
                 (second (monk-converge m policy)))
            (monk-testdrive--checks
             (cons (eq monk-status-repaired (monk-result-status (monk-run-result first '("drive" . "~/clone"))))
                   "clone not repaired")
             (cons (file-directory-p (monk-testdrive--path "clone/.git")) "no .git after clone")
             (cons (plist-get (monk-result-changes (monk-run-result plan '("drive" . "~/clone"))) :behind)
                   "plan did not see the new commit")
             (cons (equal "two\n" (monk-testdrive--text "clone/f")) "fast-forward did not land")
             (cons (eq monk-status-repaired (monk-result-status (monk-run-result second '("drive" . "~/clone"))))
                   "second run not repaired"))))))
    (monk-testdrive--step "guards" "query buffer lists matches with explanations"
      (save-window-excursion
        (monk-ui-query "'drive' in classes && facts.kernel == 'Plan9'" (list m)))
      (unwind-protect
          (with-current-buffer monk-ui-query-buffer-name
            (goto-char (point-min))
            (monk-testdrive--checks
             (cons (search-forward "0 of 1 machine" nil t) "count line")
             (cons (search-forward "– drive" nil t) "no-match row")
             (cons (search-forward "facts.kernel == 'Plan9' is false" nil t) "explanation in CEL")
             (cons (eq (key-binding "q") 'monk-ui-quit) "q")))
        (kill-buffer monk-ui-query-buffer-name)))))

(defun monk-testdrive--section-inspect ()
  "Inspector, folds, jump to source, rerun, and detail-buffer keys."
  (monk-testdrive--out "\n## Inspect and navigate")
  (let* ((m (monk-testdrive--machine))
         (policy-file (monk-testdrive--path "policy.el")))
    (with-temp-file policy-file
      (insert ";; -*- lexical-binding: t; -*-\n"
              "(defun monk-testdrive--file-policy (_m)\n"
              "  (monk-directory \"~/from-file\")\n"
              "  (monk-file \"~/from-file/note\" :content \"hi\\n\"))\n"))
    (load policy-file nil t)
    (unwind-protect
        (let ((plan (monk-plan m 'monk-testdrive--file-policy)))
          (monk-testdrive--step "inspect" "results and machines remember where they were defined"
            (let ((origin (monk-result-origin (car (monk-run-results plan)))))
              (monk-testdrive--checks
               (cons (equal (monk-origin-file origin) policy-file) (format "origin %S" origin))
               (cons (equal "~/from-file" (cdr origin)) "origin name"))))
          (monk-testdrive--step "inspect" "detail buffer: folded machine section, . jumps to the policy"
            (with-current-buffer (monk-ui-show plan)
              (monk-ui-detail-buffer "drive"))
            (with-current-buffer (format monk-ui-detail-buffer-format "drive")
              (goto-char (point-min))
              (let ((heading (search-forward "drive: " nil t))
                    (folded (progn (re-search-forward "\"\\(Linux\\|Darwin\\)\"" nil t)
                                   (get-text-property (point) 'invisible))))
                (goto-char (point-min))
                (search-forward "~/from-file/note" nil t)
                (let ((jumped (save-window-excursion
                                (monk-ui-find-definition)
                                (prog1 (and (equal (buffer-file-name) policy-file)
                                            (looking-at ".*~/from-file/note"))
                                  (kill-buffer)))))
                  (monk-testdrive--checks
                   (cons heading "no machine heading")
                   (cons folded "machine section not folded")
                   (cons (eq (key-binding "a") 'monk-ui-apply) "a not bound in detail")
                   (cons (eq (key-binding "q") 'monk-ui-quit) "q not bound in detail")
                   (cons jumped "jump to source"))))))
          (monk-testdrive--step "inspect" "i opens the inspector with facts unfolded"
            (with-current-buffer (monk-ui--buffer)
              (save-window-excursion (monk-ui-inspect-machine "drive")))
            (let ((name (format monk-ui-detail-buffer-format "drive facts")))
              (unwind-protect
                  (with-current-buffer name
                    (goto-char (point-min))
                    (monk-testdrive--checks
                     (cons (search-forward "classes" nil t) "no classes line")
                     (cons (and (search-forward "kernel" nil t)
                                (not (get-text-property (point) 'invisible)))
                           "facts folded in the inspector")))
                (kill-buffer name))))
          (monk-testdrive--step "inspect" "g in the plan buffer plans again and sees the repair"
            (monk-converge m 'monk-testdrive--file-policy)
            (with-current-buffer (monk-ui--buffer)
              (setq monk-ui--run plan)
              (monk-ui-rerun)
              (monk-testdrive--checks
               (cons (not (eq monk-ui--run plan)) "run not replaced")
               (cons (monk-run-dry-run monk-ui--run) "rerun of a plan was not a plan")
               (cons (= 0 (monk-run-count monk-ui--run monk-status-would-repair))
                     (format "%d would-repair after repair" (monk-run-count monk-ui--run monk-status-would-repair))))))
          (monk-testdrive--step "inspect" "history recorded the runs; RET rebuilds one"
            (if (not (featurep 'monk-history))
                (cons 'SKIP "monk-history not loaded (no SQLite in this Emacs?)")
              (let* ((rows (monk-history-runs :host "drive" :limit 5))
                     (back (and rows (monk-history-run (alist-get 'id (car rows)))))
                     (shown (with-current-buffer (monk-ui--buffer) monk-ui--run)))
                (monk-testdrive--checks
                 (cons (>= (length rows) 3) (format "%d history rows for drive" (length rows)))
                 (cons back "run did not rebuild")
                 (cons (and back shown (equal (monk-run-summary back) (monk-run-summary shown)))
                       "rebuilt summary differs from the last shown run")
                 (cons (monk-history-repairs "drive" "~/from-file") "no repair timeline"))))))
      (fmakunbound 'monk-testdrive--file-policy)
      (when (get-file-buffer policy-file) (kill-buffer (get-file-buffer policy-file)))
      (when (get-buffer (format monk-ui-detail-buffer-format "drive"))
        (kill-buffer (format monk-ui-detail-buffer-format "drive"))))))

(defun monk-testdrive--section-salt ()
  "The requisites and run features borrowed from Salt."
  (monk-testdrive--out "\n## Salt-style features")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "salt" "prereq runs before its target, only when the target would change"
      (let* ((log (monk-testdrive--path "prereq.log"))
             (policy (lambda (_m)
                       (monk-command "stop-first" "sh"
                                     :args (list "-c" (format "echo stop >> %s" log))
                                     :creates "~/never"
                                     :prereq '("~/app.bin"))
                       (monk-file "~/app.bin" :content "v2\n")))
             (first (monk-converge m policy :passes 1))
             (second (monk-converge m policy :passes 1)))
        (monk-testdrive--checks
         (cons (equal '("stop-first" "~/app.bin") (mapcar #'monk-result-name (monk-run-results first)))
               (format "order %S" (mapcar #'monk-result-name (monk-run-results first))))
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result first '("drive" . "stop-first"))))
               "prereq did not run before a change")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result second '("drive" . "stop-first"))))
               "prereq ran when nothing would change")
         (cons (equal "stop\n" (monk-testdrive--text "prereq.log")) "log has more than one line"))))
    (monk-testdrive--step "salt" "listen restarts once after two changes across passes"
      (let* ((count (monk-testdrive--path "restarts"))
             (policy (lambda (_m)
                       (monk-file "~/svc/a.conf" :content "a\n")
                       (monk-file "~/svc/b.conf" :content "b\n" :require '("~/svc/a.conf"))
                       (monk-command "svc" "sh" :args (list "-c" (format "echo r >> %s" count))
                                     :listen '("~/svc/a.conf" "~/svc/b.conf"))))
             (run (monk-converge m policy)))
        (monk-testdrive--checks
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "svc")))) "listener kept")
         (cons (equal "r\n" (monk-testdrive--text "restarts"))
               (format "restarts: %S" (monk-testdrive--text "restarts"))))))
    (monk-testdrive--step "salt" "retry with backoff, then verify after repair"
      (let ((waits nil))
        (cl-letf (((symbol-function 'sit-for) (lambda (secs &rest _) (push secs waits) t)))
          (let ((run (monk-converge
                      m (lambda (_m)
                          (monk-command "flaky" "sh" :args '("-c" "false") :creates "~/never"
                                        :retry '(:attempts 3 :interval 1 :backoff 3))
                          (monk-command "checked" "true" :creates "~/never"
                                        :verify '("sh" "-c" "exit 7")))
                      :passes 1)))
            (monk-testdrive--checks
             (cons (equal '(1.0 3.0) (reverse waits)) (format "waits %S" (reverse waits)))
             (cons (string-match-p "attempt 3 of 3" (monk-result-comment (monk-run-result run '("drive" . "flaky"))))
                   "no attempt count in the comment")
             (cons (eq monk-status-failed (monk-result-status (monk-run-result run '("drive" . "checked"))))
                   "verify did not fail the command")
             (cons (string-match-p "verify failed: sh exited 7"
                                   (monk-result-comment (monk-run-result run '("drive" . "checked"))))
                   "verify reason missing"))))))
    (monk-testdrive--step "salt" "mock run touches nothing and keeps the order"
      (let ((run (monk-orchestrate (list m)
                                   (lambda (_m)
                                     (monk-directory "~/mocked-dir")
                                     (monk-file "~/mocked-dir/f" :content "x\n" :require '("~/mocked-dir"))
                                     (monk-command "h" "true" :onchanges '("~/mocked-dir/f")))
                                   :mock t)))
        (monk-testdrive--checks
         (cons (equal (list monk-comment-mocked monk-comment-mocked "onchanges: nothing changed")
                      (mapcar #'monk-result-comment (monk-run-results run)))
               (format "comments %S" (mapcar #'monk-result-comment (monk-run-results run))))
         (cons (not (file-exists-p (monk-testdrive--path "mocked-dir"))) "mock created a directory")
         (cons (= 1 (monk-run-passes run)) "mock ran more than one pass"))))
    (monk-testdrive--step "salt" ":fatal ends its machine, :handled rescues a failure"
      (let* ((machines (list m (let ((o (monk-local-machine :name "other")))
                                 (oset o facts (oref m facts))
                                 (oset o classes (monk-hard-classes o))
                                 o)))
             (run (monk-orchestrate
                   machines
                   (lambda (mm)
                     (when (equal (oref mm name) "drive")
                       (monk-line-in-file "~/absent" "x" :fatal t)
                       (monk-directory "~/after-fatal"))
                     (monk-line-in-file "~/absent2" "y")
                     (monk-directory "~/rescue" :onfail '("~/absent2#y") :handled t))
                   :passes 1)))
        (monk-testdrive--checks
         (cons (equal monk-skip-fatal (monk-result-comment (monk-run-result run '("drive" . "~/after-fatal"))))
               "after-fatal was not skipped")
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("other" . "~/rescue"))))
               "rescue on other did not run")
         (cons (member '("other" . "~/absent2#y") (monk-run-rescued run)) "absent2 on other not rescued")
         (cons (not (monk-run-ok-p run)) "run ok despite the unhandled fatal failure on drive"))))
    (monk-testdrive--step "salt" "availability: a brew resource on a host without brew is skipped"
      (if (executable-find "brew")
          (cons 'SKIP "brew is installed here")
        (let ((run (monk-converge m (lambda (_m) (monk-brew "tools" '("jq") :classes nil)) :passes 1)))
          (monk-testdrive--checks
           (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "tools")))) "not skipped")
           (cons (equal "brew is not on this machine" (monk-result-comment (monk-run-result run '("drive" . "tools"))))
                 (format "comment %S" (monk-result-comment (monk-run-result run '("drive" . "tools")))))))))
    (monk-testdrive--step "salt" "events carry tags; results carry seq"
      (let* ((run (monk-converge m #'monk-testdrive--policy :passes 1))
             (events (monk-run-events run))
             (id (monk-run-id run)))
        (monk-testdrive--checks
         (cons (equal (format "monk/run/%s/new" id) (alist-get 'tag (car events))) "no new tag")
         (cons (equal (format "monk/run/%s/prog/drive/0" id) (alist-get 'tag (nth 1 events))) "no prog tag")
         (cons (equal (format "monk/run/%s/end" id) (alist-get 'tag (car (last events)))) "no end tag"))))
    (monk-testdrive--step "salt" "rsync mirrors a tree, deletes strays, then is kept"
      (if (not (executable-find monk-rsync-program))
          (cons 'SKIP "no rsync on this host")
        (let* ((policy (lambda (_m)
                         (monk-directory "~/mirror-src/sub")
                         (monk-file "~/mirror-src/a" :content "a\n" :require '("~/mirror-src/sub"))
                         (monk-file "~/mirror-src/sub/b" :content "b\n" :require '("~/mirror-src/sub"))
                         (monk-rsync "mirror" '("~/mirror-src/") "~/mirror-dst/" :archive t :delete t
                                     :require '("~/mirror-src/a" "~/mirror-src/sub/b"))))
               (_ (progn (make-directory (monk-testdrive--path "mirror-dst") t)
                         (with-temp-file (monk-testdrive--path "mirror-dst/stray") (insert "x\n"))))
               (first (monk-converge m policy))
               (second (monk-converge m policy :passes 1))
               (dead (monk-converge m (lambda (_m)
                                        (monk-rsync "away" '("~/mirror-src/") "nowhere.invalid:x/"
                                                    :reachable "nowhere.invalid"))
                                    :passes 1)))
          (monk-testdrive--checks
           (cons (eq monk-status-repaired (monk-result-status (monk-run-result first '("drive" . "mirror"))))
                 "mirror not repaired")
           (cons (equal "b\n" (monk-testdrive--text "mirror-dst/sub/b")) "b did not arrive")
           (cons (not (file-exists-p (monk-testdrive--path "mirror-dst/stray"))) "stray not deleted")
           (cons (eq monk-status-kept (monk-result-status (monk-run-result second '("drive" . "mirror"))))
                 "second run not kept")
           (cons (eq monk-status-skipped (monk-result-status (monk-run-result dead '("drive" . "away"))))
                 "unreachable destination did not skip")
           (cons (equal "nowhere.invalid is not reachable"
                        (monk-result-comment (monk-run-result dead '("drive" . "away"))))
                 (format "reason %S" (monk-result-comment (monk-run-result dead '("drive" . "away")))))))))
    (monk-testdrive--step "salt" "terminal progress prints aligned change lines off a tty"
      (let ((lines nil) (monk-log-level nil))
        (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) lines))))
          (unwind-protect
              (progn (monk-cli-enable)
                     (monk-converge m (lambda (_m)
                                        (monk-directory "~/cli-a")
                                        (monk-directory "~/cli-longer-name-here"))
                                    :passes 1))
            (monk-cli-disable)))
        (let ((rows (seq-filter (lambda (l) (string-match-p "cli-" l)) lines)))
          (monk-testdrive--checks
           (cons (= 2 (length rows)) (format "%d rows" (length rows)))
           (cons (and (= 2 (length rows))
                      (= (string-search "directory" (car rows)) (string-search "directory" (cadr rows))))
                 "type column not aligned")
           (cons (not monk-cli--tty) "drive stderr is a tty?")
           (cons (not (seq-some (lambda (l) (string-search "\e[2K" l)) lines)) "escape bytes off a tty")))))
    (monk-testdrive--step "salt" "the watching line rolls in place and yields to a run"
      ;; Drive stderr is not a tty, so the terminal is faked: the writes
      ;; are collected and read back as frames.
      (let ((out nil) (monk-log-level nil) (monk-cli-idle-interval 0.02)
            ;; the timer fires with the caller's `default-directory', which
            ;; may be ~-relative and so re-expand under the throwaway HOME
            (default-directory temporary-file-directory))
        (cl-letf (((symbol-function 'monk-cli--write) (lambda (text) (push text out)))
                  ((symbol-function 'monk-cli-stderr-tty-p) (lambda () t))
                  ((symbol-function 'monk-cli-utf8-p) (lambda () t)))
          (unwind-protect
              (progn
                (monk-cli-enable)
                (monk-cli-idle-start (lambda () "watching 2 dir(s)"))
                (accept-process-output nil 0.15)
                (monk-cli-print-line "permanent")
                (accept-process-output nil 0.1)
                (monk-converge m (lambda (_m) (monk-directory "~/idle-run")) :passes 1)
                (accept-process-output nil 0.1))
            (monk-cli-idle-stop)
            (monk-cli-disable)))
        (let* ((text (apply #'concat (reverse out)))
               (frames (let (fs) (dolist (part (split-string text "\e\\[2K" t))
                                   (when (string-match "\\`\\(?:\e\\[[0-9;]*m\\)?\\(.\\)\\(?:\e\\[0m\\)? watching" part)
                                     (cl-pushnew (match-string 1 part) fs :test #'equal)))
                         fs))
               (after-run (let ((at (string-search "pass 1" text)))
                            (and at (string-search " watching" text at)))))
          (monk-testdrive--checks
           (cons (>= (length frames) 4) (format "%d distinct frames: %S" (length frames) frames))
           (cons (seq-every-p (lambda (f) (seq-position monk-cli-spinner-frames f)) frames) "not braille frames")
           (cons (string-search "permanent\n" text) "permanent line missing")
           (cons (string-search "pass 1" text) "run status line missing")
           (cons after-run "watching line did not come back after the run")))))
    (monk-testdrive--step "salt" "compound matcher translates to CELL and selects machines"
      (let* ((kernel (monk-machine-fact m 'kernel))
             (form (monk-salt-matcher-to-cell (format "G@kernel:%s and not L@nobody" kernel))))
        (monk-testdrive--checks
         (cons (equal `(and (class ,(intern (downcase kernel))) (not (in (name) (list "nobody")))) form)
               (format "%S" form))
         (cons (equal '("drive") (mapcar (lambda (x) (oref x name)) (monk-cell-select form (list m))))
               "drive did not match"))))))

(defun monk-testdrive--section-ansible ()
  "From Ansible: declared parameters, unreachable, predicates, no-log."
  (monk-testdrive--out "\n## Ansible-style features")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "ansible" "a misspelled keyword is refused when the policy compiles"
      (condition-case err
          (progn (monk-compile m (lambda (_m) (monk-directory "~/x" :mdoe #o700)))
                 (cons 'FAIL "compiled"))
        (monk-argument-error
         (monk-testdrive--checks
          (cons (string-match-p "Unsupported parameter for monk-directory: :mdoe" (cadr err)) (cadr err))
          (cons (string-match-p "Supported parameters include: :group, :mode, :owner" (cadr err)) "no list")))))
    (monk-testdrive--step "ansible" "changed-when makes a query; failed-when replaces the exit code"
      (let ((run (monk-converge
                  m (lambda (_m)
                      (monk-command "query" "sh" :args '("-c" "echo same") :changed-when '(= (stdout) "changed"))
                      (monk-command "tolerant" "sh" :args '("-c" "echo warn; exit 1") :creates "~/never"
                                    :failed-when '(contains (stdout) "ERROR")))
                  :passes 1)))
        (monk-testdrive--checks
         (cons (eq monk-status-kept (monk-result-status (monk-run-result run '("drive" . "query")))) "query not kept")
         (cons (equal "same" (monk-result-comment (monk-run-result run '("drive" . "query")))) "query comment")
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "tolerant"))))
               "exit 1 failed despite failed-when"))))
    (monk-testdrive--step "ansible" "no-log keeps a secret out of results and events"
      (let* ((secret "drive-secret-9f2a")
             (run (monk-converge m (lambda (_m) (monk-file "~/creds" :content (concat secret "\n") :no-log t)) :passes 1))
             (events (mapconcat #'monk-event-line (monk-run-events run) "")))
        (monk-testdrive--checks
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/creds")))) "not repaired")
         (cons (equal (concat secret "\n") (monk-testdrive--text "creds")) "file not written")
         (cons (not (string-search secret events)) "secret in events")
         (cons (not (string-search secret (monk-run-report-string run t))) "secret in report"))))
    (monk-testdrive--step "ansible" "an unreachable machine: its nodes marked, others finish, exit code 4"
      (let* ((dead (monk-machine :name "dead" :address "dead.invalid"))
             (run (cl-letf* ((real (symbol-function 'monk-exec))
                             ((symbol-function 'monk-exec)
                              (lambda (mm program &rest args)
                                (if (equal (oref mm name) "dead")
                                    (monk-exec-create :command (cons program args) :exit-code 255
                                                      :output "ssh: connect to host dead.invalid port 22: No route to host")
                                  (apply real mm program args)))))
                    (monk-orchestrate (list dead m)
                                      (lambda (mm)
                                        (monk-command "probe" "true" :creates "~/never")
                                        (when (equal (oref mm name) "drive")
                                          (monk-directory "~/needs-dead" :require '((dead . "probe")))
                                          (monk-directory "~/free-of-dead")))
                                      :passes 1))))
        (monk-testdrive--checks
         (cons (eq monk-status-unreachable (monk-result-status (monk-run-result run '("dead" . "probe")))) "dead not unreachable")
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/free-of-dead")))) "free did not run")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/needs-dead")))) "dependent ran")
         (cons (string-match-p "dead is unreachable" (monk-result-comment (monk-run-result run '("drive" . "~/needs-dead"))))
               "reason does not name the host")
         (cons (= 4 (monk-run-exit-code run)) (format "exit code %d" (monk-run-exit-code run)))
         (cons (string-match-p "1 unreachable" (monk-run-summary run)) "summary"))))))

(defun monk-testdrive--section-fleet ()
  "Tags, serial batches, compile-only check, direct Ansible modules, agent pull."
  (monk-testdrive--out "\n## Fleet controls")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "fleet" "tags narrow a run; requisites and always still pull in what they need"
      (let ((run (monk-orchestrate
                  (list m)
                  (lambda (_m)
                    (monk-directory "~/tag-base" :tags '(base))
                    (monk-file "~/tag-conf" :content "x\n" :require '("~/tag-base") :tags '(ssh))
                    (monk-directory "~/tag-dns" :tags '(dns))
                    (monk-directory "~/tag-always" :tags '(always))
                    (monk-directory "~/tag-slow" :tags '(ssh slow)))
                  :tags '(ssh) :skip-tags '(slow) :passes 1)))
        (monk-testdrive--checks
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/tag-conf")))) "ssh not run")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/tag-base")))) "base ran")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/tag-dns")))) "dns ran")
         (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "~/tag-always")))) "always skipped")
         (cons (eq monk-status-skipped (monk-result-status (monk-run-result run '("drive" . "~/tag-slow")))) "skip-tags ignored")
         (cons (file-exists-p (monk-testdrive--path "tag-conf")) "conf not written"))))
    (monk-testdrive--step "fleet" "serial batches stop after one fails past max-fail-percentage"
      (let* ((machines (mapcar (lambda (n)
                                 (let ((x (monk-local-machine :name n)))
                                   (oset x facts (oref m facts))
                                   (oset x classes (monk-hard-classes x))
                                   x))
                               '("s1" "s2" "s3")))
             (run (monk-orchestrate machines
                                    (lambda (mm)
                                      (when (equal (oref mm name) "s1")
                                        (monk-line-in-file "~/serial-missing" "x"))
                                      (monk-directory (concat "~/serial-" (oref mm name))))
                                    :serial 1 :max-fail-percentage 50 :passes 1)))
        (monk-testdrive--checks
         (cons (equal '(("s1") ("s2") ("s3")) (monk-run-batches run)) (format "batches %S" (monk-run-batches run)))
         (cons (eql 1 (monk-run-stopped-at-batch run)) (format "stopped at %S" (monk-run-stopped-at-batch run)))
         (cons (file-directory-p (monk-testdrive--path "serial-s1")) "batch 1 did not run")
         (cons (not (file-directory-p (monk-testdrive--path "serial-s2"))) "batch 2 ran")
         (cons (equal monk-skip-batch (monk-result-comment (monk-run-result run '("s3" . "~/serial-s3")))) "no batch skip comment")
         (cons (not (monk-run-ok-p run)) "run counted as ok")
         (cons (= 2 (monk-run-exit-code run)) (format "exit code %d" (monk-run-exit-code run))))))
    (monk-testdrive--step "fleet" "check-policy compiles with no connection and lists tags and errors"
      (let ((gathers 0))
        (cl-letf (((symbol-function 'monk-gather-facts) (lambda (_m) (cl-incf gathers))))
          (let* ((fresh (monk-local-machine :name "drive-check"))
                 (_ (progn (oset fresh facts (oref m facts)) (oset fresh classes (monk-hard-classes fresh))))
                 (good (monk-check-policy fresh (lambda (_m)
                                                  (monk-directory "~/c1" :tags '(base))
                                                  (monk-file "~/c2" :content "x" :tags '(ssh)))
                                          :tags '(ssh)))
                 (bad (monk-check-policy fresh (lambda (_m) (monk-directory "~/c1" :require '("nope")))))
                 (text (monk-check-policy-string good)))
            (monk-testdrive--checks
             (cons (= 0 gathers) (format "%d gathers" gathers))
             (cons (monk-check-policy-ok-p good) "good not ok")
             (cons (equal '(base ssh) (plist-get good :tags)) (format "tags %S" (plist-get good :tags)))
             (cons (equal '(("drive-check" . "~/c2")) (plist-get good :selected)) "selection")
             (cons (string-match-p "\\[ssh\\]" text) text)
             (cons (not (monk-check-policy-ok-p bad)) "bad ok")
             (cons (string-match-p "nope" (car (plist-get bad :unresolved))) "unresolved text"))))))
    (monk-testdrive--step "fleet" "an Ansible module runs directly from a payload zip"
      (let ((library (seq-find (lambda (d) (file-exists-p (expand-file-name "ansible/module_utils/basic.py" d)))
                               ;; only an explicit path or a sibling checkout of
                               ;; this Monk tree: no guesses about the user's disk
                               (delq nil (list (getenv "MONK_ANSIBLE_LIBRARY")
                                               (let ((here (or load-file-name (locate-library "monk-testdrive"))))
                                                 (and here (expand-file-name "../../ansible/lib" (file-name-directory here)))))))))
        (if (not (and library (executable-find "zip") (executable-find "python3")))
            (cons 'SKIP "no ansible checkout, zip, or python3 (set MONK_ANSIBLE_LIBRARY)")
          (require 'monk-bridge)
          (let* ((monk-ansible-library library)
                 (monk-ansible--payload nil)
                 (path (monk-testdrive--path "direct.txt"))
                 (policy (lambda (_m)
                           (monk-ansible "direct" "lineinfile" `((path . ,path) (line . "b=2") (create . t)))
                           (monk-ansible "bad" "file" '((path . "/nonexistent/monk-drive") (state . "file")))))
                 (plan (monk-plan m policy))
                 (planned (monk-run-result plan '("drive" . "direct")))
                 (run (monk-apply plan))
                 (again (monk-converge m policy :passes 1)))
            (monk-testdrive--checks
             (cons (eq monk-status-would-repair (monk-result-status planned)) (format "plan %s" (monk-result-status planned)))
             (cons (string-match-p "^\\+b=2$" (or (monk-result-diff-text planned) "")) (format "plan diff %S" (monk-result-diff-text planned)))
             (cons (eq monk-status-repaired (monk-result-status (monk-run-result run '("drive" . "direct")))) "apply")
             (cons (equal "b=2\n" (monk-testdrive--text "direct.txt")) "file")
             (cons (eq monk-status-kept (monk-result-status (monk-run-result again '("drive" . "direct")))) "not idempotent")
             (cons (eq monk-status-failed (monk-result-status (monk-run-result run '("drive" . "bad")))) "bad not failed")
             (cons (string-match-p "is absent" (monk-result-comment (monk-run-result run '("drive" . "bad")))) "bad message")
             (cons (file-exists-p (car monk-ansible--payload)) "payload zip missing"))))))
    (monk-testdrive--step "fleet" "the agent skips a timed converge while the policy repo stands still"
      (require 'monk-agent)
      (let ((converges 0)
            (monk-agent-only-if-changed t)
            (monk-agent-policy-repo (monk-testdrive--path "no-such-repo"))
            (monk-agent-splay 0)
            (updates '(same same changed same)))
        (cl-letf (((symbol-function 'monk-converge) (lambda (&rest _) (cl-incf converges) (monk-run-create)))
                  ((symbol-function 'monk-run-report) #'identity)
                  ((symbol-function 'monk-cli-print-line) #'ignore)
                  ((symbol-function 'monk-agent-update-policy-repo)
                   (lambda () (setq monk-agent--policy-revision "0123456789abcdef") (pop updates))))
          (monk-agent--full-converge #'ignore)
          (monk-agent--full-converge #'ignore t)
          (monk-agent--full-converge #'ignore t)
          (monk-agent--full-converge #'ignore t)
          (monk-testdrive--checks
           (cons (= 2 converges) (format "%d converges, wanted 2 (start + changed)" converges))))))))

(defun monk-testdrive--section-translate ()
  "Import and export through a scripted model: catalog, repair loop, round trip."
  (monk-testdrive--out "\n## Translate (Ansible and Salt)")
  (require 'monk-translate)
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "translate" "the catalog lists every declared constructor with its parameters"
      (let ((text (monk-translate-catalog)))
        (monk-testdrive--checks
         (cons (= (length monk-resource-specs)
                  (with-temp-buffer (insert text) (count-matches "^(monk-" (point-min) (point-max))))
               "constructor count")
         (cons (string-match-p ":mode <mode>" text) "mode param")
         (cons (string-match-p "COMMON KEYS" text) "common keys"))))
    (monk-testdrive--step "translate" "import: a bad answer is repaired with the compile errors, then checked"
      (let* ((bad "(defun monk-translate--imported-policy (m) (monk-directory \"~/imp\" :mdoe 1))")
             (good ";; -*- lexical-binding: t; -*-\n(defun monk-translate--imported-policy (m)\n  (monk-directory \"~/imp\" :mode #o700 :tags '(base))\n  (monk-line-in-file \"~/imp/f\" \"x=1\" :create t :require '(\"~/imp\")))\n")
             (answers (list bad good))
             (prompts nil)
             (monk-translate-ask-function
              (lambda (prompt callback &rest _)
                (push prompt prompts)
                (funcall callback (pop answers) nil)))
             (checked nil))
        (cl-letf (((symbol-function 'monk-ui-check-policy) (lambda (mm p &rest _) (setq checked (list (oref mm name) p)) nil))
                  ((symbol-function 'pop-to-buffer) #'set-buffer))
          (with-current-buffer (monk-translate-import 'ansible "- name: imp\n  file: path=~/imp state=directory\n" m)
            (monk-testdrive--checks
             (cons (equal good (buffer-string)) (format "buffer holds %S" (buffer-string)))
             (cons (= 1 monk-translate--round) (format "%d rounds" monk-translate--round))
             (cons (= 2 (length prompts)) (format "%d prompts" (length prompts)))
             (cons (string-match-p "Unsupported parameter for monk-directory: :mdoe" (car prompts)) "repair prompt lacks the error")
             (cons (string-match-p "MONK RESOURCE CATALOG" (cadr prompts)) "first prompt lacks the catalog")
             (cons (equal (list "drive" 'monk-translate--imported-policy) checked) (format "check %S" checked)))))))
    (monk-testdrive--step "translate" "export sends the policy as data and lands YAML; a re-import matches"
      (let* ((policy (lambda (_m)
                       (monk-directory "~/exp" :mode #o700 :tags '(base))
                       (monk-file "~/exp/f" :content "hi\n" :require '("~/exp") :when "'drive' in classes")))
             (yaml "---\n- hosts: all\n  tasks:\n    - name: \"~/exp\"\n      file: {path: \"~/exp\", state: directory, mode: \"0700\"}\n      tags: [base]\n    - name: \"~/exp/f\"\n      copy: {dest: \"~/exp/f\", content: \"hi\\n\"}\n      when: \"'drive' in group_names\"\n")
             (back ";; -*- lexical-binding: t; -*-\n(defun monk-translate--imported-policy (m)\n  (monk-directory \"~/exp\" :mode #o700 :tags '(base))\n  (monk-file \"~/exp/f\" :content \"hi\\n\" :require '(\"~/exp\") :when \"'drive' in classes\"))\n")
             (answers (list (concat "```yaml\n" yaml "```") back))
             (prompts nil)
             (monk-translate-ask-function
              (lambda (prompt callback &rest _)
                (push prompt prompts)
                (funcall callback (pop answers) nil)))
             (before (monk-translate-policy-data m policy))
             (exported nil))
        (cl-letf (((symbol-function 'monk-ui-check-policy) (lambda (&rest _) nil))
                  ((symbol-function 'pop-to-buffer) #'set-buffer))
          (with-current-buffer (monk-translate-export 'ansible m policy)
            (setq exported (buffer-string)))
          (with-current-buffer (monk-translate-import 'ansible exported m)
            (monk-testdrive--checks
             (cons (equal yaml exported) (format "export buffer %S" exported))
             (cons (string-match-p "(when . \"'drive' in classes\")" (cadr prompts)) "export prompt lacks the CEL guard")
             (cons (string-match-p "(tags base)" (cadr prompts)) "export prompt lacks tags")
             (cons (equal before (monk-translate-policy-data m 'monk-translate--imported-policy))
                   (format "round trip differs: %S" (monk-translate-policy-data m 'monk-translate--imported-policy))))))))))

(defun monk-testdrive--section-embark ()
  "The resource index and the Embark actions, with no Embark needed."
  (monk-testdrive--out "\n## Index and Embark actions")
  (require 'monk-embark)
  (let* ((m (monk-testdrive--machine))
         (policy-file (monk-testdrive--path "embark-policy.el")))
    (with-temp-file policy-file
      (insert ";; -*- lexical-binding: t; -*-\n"
              "(defun monk-testdrive--embark-policy (_m)\n"
              "  (monk-directory \"~/ix\" :tags '(base))\n"
              "  (monk-file \"~/ix/f\" :content \"x\\n\" :require '(\"~/ix\"))\n"
              "  (monk-command \"ix-reload\" \"true\" :creates \"~/never\" :watch '(\"~/ix/f\"))\n"
              "  (monk-directory \"~/ix-other\"))\n"))
    (load policy-file nil t)
    (unwind-protect
        (let ((monk-batch-policy 'monk-testdrive--embark-policy))
          (monk-testdrive--step "embark" "a bare name resolves to its definition through the index"
            (clrhash monk-resource-index)
            (let ((origin (monk-resource-origin-by-name "~/ix/f")))
              (monk-testdrive--checks
               (cons (equal policy-file (and origin (monk-origin-file origin))) (format "origin %S" origin))
               (cons (equal '(("~/ix/f" . require)) (monk-resource-dependents-by-name "~/ix")) "dependents of ~/ix")
               (cons (equal '(("ix-reload" . watch)) (monk-resource-dependents-by-name "~/ix/f")) "dependents of ~/ix/f")
               (cons (equal '("ix-reload" "~/ix" "~/ix-other" "~/ix/f") (monk-resource-names)) (format "names %S" (monk-resource-names))))))
          (monk-testdrive--step "embark" "the target finder sees a resource string in the policy file"
            (with-current-buffer (find-file-noselect policy-file)
              (goto-char (point-min))
              (goto-char (- (search-forward "\"~/ix/f\" :content") 10))
              (prog1 (pcase (monk-embark-resource-target)
                       (`(monk-resource ,name . ,_) (monk-testdrive--check (equal "~/ix/f" name) name))
                       (other (cons 'FAIL (format "target %S" other))))
                (kill-buffer))))
          (monk-testdrive--step "embark" "plan-only on a name runs it and its dependents, nothing else"
            (let ((reported nil))
              (cl-letf (((symbol-function 'monk-run-report) (lambda (run &rest _) (setq reported run) run))
                        ((symbol-function 'monk-embark--machines-for) (lambda (_n) (list m)))
                        ((symbol-function 'monk-embark--policy) (lambda () monk-batch-policy)))
                (monk-embark-plan-resource "~/ix/f")
                (monk-testdrive--checks
                 (cons (monk-run-dry-run reported) "not a plan")
                 (cons (eq monk-status-would-repair (monk-result-status (monk-run-result reported '("drive" . "~/ix/f")))) "f not planned")
                 (cons (eq monk-status-skipped (monk-result-status (monk-run-result reported '("drive" . "~/ix-other")))) "other ran")
                 ;; the watcher is a dependent, so it is in the plan
                 (cons (not (eq monk-status-skipped (monk-result-status (monk-run-result reported '("drive" . "ix-reload"))))) "watcher skipped"))))))
      (fmakunbound 'monk-testdrive--embark-policy))))

(defun monk-testdrive--section-facts ()
  "Fact cache, facts.d, structured diffs, generic package and service."
  (monk-testdrive--out "\n## Facts and diffs")
  (let ((m (monk-testdrive--machine)))
    (monk-testdrive--step "facts" "a content and mode change give two diff entries"
      (with-temp-file (monk-testdrive--path "two.conf") (insert "old\n"))
      (set-file-modes (monk-testdrive--path "two.conf") #o644)
      (let* ((plan (monk-plan m (lambda (_m) (monk-file "~/two.conf" :content "new\n" :mode #o600))))
             (r (monk-run-result plan '("drive" . "~/two.conf")))
             (text (monk-result-diff-text r)))
        (monk-testdrive--checks
         (cons (= 2 (length (monk-diff-entries (monk-result-diff r)))) "entry count")
         (cons (string-match-p "^\\+new$" text) "content hunk missing")
         (cons (string-match-p "mode: 0644 → 0600" text) (format "attributes line missing in %S" text)))))
    (monk-testdrive--step "facts" "facts.d files become (fact local) and CELL reads inside"
      (let ((d (monk-testdrive--path ".config/monk/facts.d/")))
        (make-directory d t)
        (with-temp-file (concat d "monk.fact") (insert "{\"role\": \"drive-box\"}"))
        (let ((fresh (monk-local-machine :name "drive")))
          (oset fresh facts (oref m facts))
          (oset fresh facts (append (list (cons 'local (cdar (monk--probe-local fresh)))) (oref fresh facts)))
          (oset fresh classes (monk-hard-classes fresh))
          (monk-testdrive--checks
           (cons (equal "drive-box" (monk-cell-eval fresh (monk-cell-guard "facts.local.monk.role"))) "CEL nested read")
           (cons (monk-cell-eval fresh '(= (get (fact local) monk role) "drive-box")) "CELL get")))))
    (monk-testdrive--step "facts" "cached facts: fresh spares a gather, stale shows a badge"
      (if (not (featurep 'monk-history))
          (cons 'SKIP "monk-history not loaded")
        (let ((gathers 0) (monk-log-level nil))
          (cl-letf (((symbol-function 'monk--probe-uname)
                     (lambda (_m) (cl-incf gathers) '((kernel . "Linux") (arch . "x") (hostname . "c") (nodename . "c"))))
                    ((symbol-function 'monk--probe-os) (lambda (_m) '((os-family . "Debian"))))
                    ((symbol-function 'monk--probe-env) (lambda (_m) nil))
                    ((symbol-function 'monk--probe-managers) (lambda (_m) nil))
                    ((symbol-function 'monk--probe-local) (lambda (_m) nil)))
            (let ((a (monk-local-machine :name "drive-cache")) (b (monk-local-machine :name "drive-cache")))
              (monk--nodes-for (list a) (lambda (_m) (monk-directory "~/x")))
              (monk--nodes-for (list b) (lambda (_m) (monk-directory "~/x")))
              (let ((stale (monk-local-machine :name "drive-cache")))
                (let ((monk-fact-ttl most-positive-fixnum)) (monk-facts-load stale))
                (oset stale gathered (time-subtract (current-time) (* 3 86400)))
                (monk-testdrive--checks
                 (cons (= 1 gathers) (format "%d gathers for two compiles" gathers))
                 (cons (eq 'cache (oref b facts-source)) "second compile not from cache")
                 (cons (string-prefix-p "·" (monk-ui-facts-badge b)) (format "fresh badge %S" (monk-ui-facts-badge b)))
                 (cons (string-prefix-p "!3d" (monk-ui-facts-badge stale)) (format "stale badge %S" (monk-ui-facts-badge stale)))
                 (cons (memq 'debian (oref b classes)) "classes not rebuilt from cache"))))))))
    (monk-testdrive--step "facts" "monk-package and monk-service dispatch on the manager facts"
      (let* ((facts (oref m facts))
             (deb (monk-local-machine :name "deb")) (mac (monk-local-machine :name "mac")))
        (oset deb facts (append '((pkg-mgr . "apt") (service-mgr . "systemd")) facts))
        (oset mac facts (append '((pkg-mgr . "homebrew") (service-mgr . "launchd")) facts))
        (dolist (x (list deb mac)) (oset x classes (monk-hard-classes x)))
        (let ((policy (lambda (_m) (monk-package "p" '("jq")) (monk-service "sshd"))))
          (monk-testdrive--checks
           (cons (equal '(monk-apt-resource monk-systemd-unit-resource)
                        (mapcar #'eieio-object-class-name (monk-compile deb policy)))
                 "debian dispatch")
           (cons (equal '(monk-brew-resource monk-launchd-resource)
                        (mapcar #'eieio-object-class-name (monk-compile mac policy)))
                 "mac dispatch")))))
    (monk-testdrive--step "facts" "derived groups follow the facts"
      (let ((monk-inventory nil) (monk-inventory-derived nil))
        (monk-inventory-define '(w :hosts ((a) (b))))
        (oset (monk-inventory-find 'a) facts '((kernel . "Linux") (disk . 10)))
        (oset (monk-inventory-find 'b) facts '((kernel . "Linux") (disk . 900)))
        (monk-group 'roomy "facts.disk > 100")
        (monk-keyed-groups '(fact kernel) "os")
        (monk-testdrive--checks
         (cons (equal '("b") (mapcar (lambda (x) (oref x name)) (monk-inventory-select 'roomy))) "roomy")
         (cons (memq 'os_linux (oref (monk-inventory-find 'a) groups)) "keyed group"))))))

(defconst monk-testdrive--sections
  '(("Isolation" monk-testdrive--section-isolation)
    ("Converge" monk-testdrive--section-converge)
    ("Plan and apply" monk-testdrive--section-plan)
    ("Requisites and convergence" monk-testdrive--section-graph)
    ("Guards and failures" monk-testdrive--section-guards)
    ("Inspect and navigate" monk-testdrive--section-inspect)
    ("Salt-style features" monk-testdrive--section-salt)
    ("Ansible-style features" monk-testdrive--section-ansible)
    ("Facts and diffs" monk-testdrive--section-facts)
    ("Fleet controls" monk-testdrive--section-fleet)
    ("Translate" monk-testdrive--section-translate)
    ("Index and Embark" monk-testdrive--section-embark)
    ("Watch" monk-testdrive--section-watch)
    ("Agent and events" monk-testdrive--section-agent))
  "The drive's sections: (TITLE FUNCTION).")

;;;; Tour

(defun monk-testdrive--tour-plan ()
  "Show a plan with a diff and ask about the header and bars."
  (let ((m (monk-testdrive--machine)))
    (monk-converge m #'monk-testdrive--policy)
    (with-temp-file (monk-testdrive--path ".config/app/app.conf") (insert "level=9\n"))
    (delete-file (monk-testdrive--path ".zshrc"))
    (monk-testdrive--step "tour" "PLAN header, purple badge, Would column, bar with two colors"
      (monk-ui-show (monk-plan m #'monk-testdrive--policy))
      (monk-testdrive--focus-monk)
      (monk-testdrive--ask "Top line of the window (above the column names): a purple PLAN box, a bar, '◐ 3' and '✓ 5', and 'a to apply'.  In the row: Would column shows 3?"))
    (monk-testdrive--step "tour" "detail buffer: groups, fontified diff"
      (monk-testdrive--focus-detail "drive")
      (monk-testdrive--ask "Detail shows 'would-repair (3)' first with a colored diff for app.conf, then 'kept'?"))))

(defun monk-testdrive--tour-apply ()
  "Apply from the plan buffer with `a' and ask about the comparison."
  (monk-testdrive--step "tour" "a applies the plan: APPLY from plan with a Plan column"
    (let ((plan (with-current-buffer (monk-ui--buffer) monk-ui--run)))
      (unless (and plan (monk-run-dry-run plan))
        (monk-ui-show (monk-plan (monk-testdrive--machine) #'monk-testdrive--policy))))
    (monk-testdrive--focus-monk)
    ;; The confirmation is answered for you: the point of this stop is
    ;; the apply view, not the prompt.
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (with-current-buffer monk-ui-buffer-name (monk-ui-apply)))
    (monk-testdrive--focus-monk)
    (monk-testdrive--ask "Top line: a blue APPLY box followed by 'from plan', and after a │ a green '✓ 3' (resources that went as planned).  In the row: a Plan column reading '✓ as planned'?")))

(defun monk-testdrive--tour-flapping ()
  "Show the flapping badge."
  (monk-testdrive--step "tour" "flapping badge in the header"
    (monk-ui-show
     (monk-converge (monk-testdrive--machine)
                    (lambda (_m)
                      (monk-file "~/fight" :content "a\n")
                      (monk-line-in-file "~/fight" "b" :create t))))
    (monk-testdrive--focus-monk)
    (monk-testdrive--ask "Top line ends with an amber 'flapping 2'?  Hover it: does the tooltip name ~/fight and ~/fight#b?")))

(defun monk-testdrive--tour-watch ()
  "Show the WATCH and AUTOPILOT badges."
  (let ((m (monk-testdrive--machine)) (monk-report-function #'ignore))
    (monk-converge m #'monk-testdrive--policy)
    (monk-testdrive--step "tour" "WATCH badge, then W toggles to AUTOPILOT"
      (let ((monk-watch--delivery 'no))
        (monk-watch-start m #'monk-testdrive--policy))
      (unwind-protect
          (progn
            (monk-ui-show (monk-converge m #'monk-testdrive--policy))
            (monk-testdrive--focus-monk)
            (let ((first (monk-testdrive--ask "Top line starts with a green WATCH box, before the APPLY box?")))
              (if (eq first 'PASS)
                  (progn
                    (with-current-buffer monk-ui-buffer-name (monk-ui-toggle-watch-mode))
                    (monk-testdrive--focus-monk)
                    (monk-testdrive--ask "After W: the box is amber and reads AUTOPILOT?  Press w: does the *monk-watch* log open?"))
                first)))
        (monk-watch-stop)))))

(defun monk-testdrive--tour-text ()
  "Toggle to text drawing and back."
  (monk-testdrive--step "tour" "t switches bars to block glyphs and back"
    (unless (with-current-buffer (monk-ui--buffer) monk-ui--run)
      (monk-ui-show (monk-converge (monk-testdrive--machine) #'monk-testdrive--policy)))
    (with-current-buffer monk-ui-buffer-name (monk-ui-toggle-style))
    (monk-testdrive--focus-monk)
    (let ((r (monk-testdrive--ask "Bars are now block characters (█░), same widths, same colors?")))
      (with-current-buffer monk-ui-buffer-name (monk-ui-toggle-style))
      r)))

(defun monk-testdrive--tour-inspect ()
  "Folds, the inspector, the query buffer, and q."
  (let ((m (monk-testdrive--machine)))
    (monk-ui-show (monk-converge m #'monk-testdrive--policy))
    (monk-testdrive--step "tour" "detail buffer opens with a folded machine line; TAB unfolds"
      (monk-testdrive--focus-detail "drive")
      (let ((showing (monk-testdrive--windows-showing (format monk-ui-detail-buffer-format "drive"))))
        (if (/= 1 (car showing))
            (cons 'FAIL (format "%d windows show the detail buffer: %S; display-buffer-alist has %d entries"
                                (car showing) (cdr showing) (length display-buffer-alist)))
          (monk-testdrive--ask "Second line reads '▸ drive: N classes, N facts, N vars (TAB to expand)'.  Press TAB on it: does it turn into ▾ with class chips and a facts table under it?  Below, is the 'kept (N)' group folded with '(TAB to expand)' while 'repaired' is open?  Press q: does the detail close and *monk* stay?"))))
    (monk-testdrive--step "tour" "/ query: matches, explanations, folded facts"
      (monk-testdrive--focus-monk)
      (save-window-excursion (monk-ui-query "'drive' in classes && facts.arch == 'nope'" (list m)))
      (pop-to-buffer monk-ui-query-buffer-name)
      (delete-other-windows)
      (prog1 (monk-testdrive--ask "Purple QUERY line at the top, then CELL and CEL renderings, '0 of 1 machine match', a grey '– drive' row with 'facts.arch == 'nope' is false: ...' after it, and a folded ▸ drive line?  Press e: does it ask for a new guard (C-g to cancel)?")
        (kill-buffer monk-ui-query-buffer-name)))
    (monk-testdrive--step "tour" "focus lands in *monk* after a command run"
      (delete-other-windows)
      (switch-to-buffer (monk-testdrive--buf))
      (let ((before (buffer-name (window-buffer (selected-window)))))
        (monk-run-report (monk-plan m #'monk-testdrive--policy))
        (redisplay)
        (let ((after (buffer-name (window-buffer (selected-window)))))
          (if (not (equal after monk-ui-buffer-name))
              (cons 'FAIL (format "selected window shows %s (was %s); monk-ui-select-window is %S, display-buffer-alist has %d entries"
                                  after before monk-ui-select-window (length display-buffer-alist)))
            (monk-testdrive--ask "The cursor is in the *monk* window (not in the report), so 'a' would apply at once?")))))))

(defun monk-testdrive--tour-facts ()
  "The fact age badge, fresh and stale, and the structured diff."
  (monk-testdrive--step "tour" "stale facts show an amber badge next to the machine"
    (let ((m (monk-testdrive--machine)))
      (oset m facts-source 'cache)
      (oset m gathered (time-subtract (current-time) (* 3 86400)))
      (unwind-protect
          (progn
            (monk-ui-show (monk-converge m #'monk-testdrive--policy))
            (monk-testdrive--focus-monk)
            (monk-testdrive--ask "In the machine row, after 'drive', an amber '!3d' badge?  Hover it: does the tooltip say the facts are from the cache and stale?  Press i: does the inspector line 'facts from' read 'the cache, 3d old, stale' in amber?"))
        (oset m facts-source 'live)
        (oset m gathered (current-time)))))
  (monk-testdrive--step "tour" "a mode change renders as an attributes block under the diff"
    (let ((m (monk-testdrive--machine)))
      (with-temp-file (monk-testdrive--path ".config/app/app.conf") (insert "level=9\n"))
      (set-file-modes (monk-testdrive--path ".config/app/app.conf") #o644)
      (monk-ui-show (monk-plan m #'monk-testdrive--policy))
      (monk-testdrive--focus-detail "drive")
      (monk-testdrive--ask "Under app.conf: a colored unified diff (red level=9, green level=3) followed by '--- a/... (attributes)' and a 'mode: 0644 → 0600' line in the changed color?"))))

(defun monk-testdrive--tour-fleet ()
  "Batch badges, the stopped-at-batch header, the check-policy buffer."
  (monk-testdrive--step "tour" "a serial run shows batch numbers and a stopped-at-batch warning"
    (let* ((m (monk-testdrive--machine))
           (machines (mapcar (lambda (n)
                               (let ((x (monk-local-machine :name n)))
                                 (oset x facts (oref m facts))
                                 (oset x classes (monk-hard-classes x))
                                 x))
                             '("t1" "t2" "t3"))))
      (monk-ui-show (monk-orchestrate machines
                                      (lambda (mm)
                                        (when (equal (oref mm name) "t1")
                                          (monk-line-in-file "~/tour-serial-missing" "x"))
                                        (monk-directory (concat "~/tour-serial-" (oref mm name))))
                                      :serial 1 :max-fail-percentage 50 :passes 1))
      (monk-testdrive--focus-monk)
      (monk-testdrive--ask "Rows t1 t2 t3 carry badges b1, b2×, b3×, the × ones dimmed?  Hover b2×: does the tooltip say the batch never started?  Does the header show a red 'stopped at batch 1/3'?")))
  (monk-testdrive--step "tour" "the check buffer lists resources with tags and marks a selection"
    (let ((m (monk-testdrive--machine))
          (policy-file (monk-testdrive--path "check-policy.el")))
      ;; A policy from a file, so `.' has a source to jump to; a lambda
      ;; typed here would have none.
      (with-temp-file policy-file
        (insert ";; -*- lexical-binding: t; -*-\n"
                "(defun monk-testdrive--check-policy (_m)\n"
                "  (monk-directory \"~/tour-c1\" :tags '(base))\n"
                "  (monk-file \"~/tour-c2\" :content \"x\" :tags '(ssh))\n"
                "  (monk-directory \"~/tour-c3\" :require '(\"nope\")))\n"))
      (load policy-file nil t)
      (unwind-protect
          (progn
            (monk-ui-check-policy m 'monk-testdrive--check-policy :tags '(ssh))
            (delete-other-windows)
            (monk-testdrive--ask "A *monk-check* buffer: a red 'CHECK FAILED' badge, an 'unresolved' line naming nope, a tags row with base and ssh as pills, and three resource lines where only ~/tour-c2 has the kept glyph and the others are dimmed?  Press . on a resource line: does it open check-policy.el at that line (M-, comes back)?  q closes?"))
        (fmakunbound 'monk-testdrive--check-policy)
        (when (get-buffer monk-ui-check-buffer-name) (kill-buffer monk-ui-check-buffer-name))))))

(defun monk-testdrive--tour-translate ()
  "Import and export with the real model, when efrit is loaded."
  (monk-testdrive--step "tour" "> exports this drive's policy as an Ansible playbook through the model"
    (if (not (and (require 'efrit-ask nil t) (fboundp 'efrit-ask-once)))
        (cons 'SKIP "efrit is not loaded; the translate stops need a model")
      (require 'monk-translate)
      (let ((m (monk-testdrive--machine))
            (monk-translate-ask-function nil)
            (done nil))
        (monk-ui-show (monk-plan m #'monk-testdrive--policy))
        (monk-testdrive--focus-monk)
        ;; Wait for the answer here, with the wait outside the step's
        ;; clock, so the question comes when the buffer is full.
        (cl-letf* ((real #'monk-translate--export-arrived)
                   ((symbol-function 'monk-translate--export-arrived)
                    (lambda (&rest args) (unwind-protect (apply real args) (setq done t)))))
          (monk-translate-export 'ansible m #'monk-testdrive--policy)
          (let ((t0 (float-time)))
            (while (and (not done) (< (- (float-time) t0) 120))
              (accept-process-output nil 0.2))
            (cl-incf monk-testdrive--waited (- (float-time) t0))))
        (pop-to-buffer monk-translate-export-buffer-name)
        (delete-other-windows)
        (monk-testdrive--ask "A *monk-export* buffer with a playbook: one play, hosts: all, one task per drive resource in policy order (two directories, two symlinks as file state=link, the app.conf copy, the motd lineinfile, an app-reload handler notified by app.conf)?  The echo area said whether ansible-playbook accepted it or is not installed?"))))
  (monk-testdrive--step "tour" "< imports that playbook back into a policy that compiles"
    (if (not (and (require 'efrit-ask nil t) (fboundp 'efrit-ask-once) (get-buffer monk-translate-export-buffer-name)))
        (cons 'SKIP "no model, or no export to import")
      (let ((m (monk-testdrive--machine))
            (monk-translate-ask-function nil)
            (yaml (with-current-buffer monk-translate-export-buffer-name (buffer-string)))
            (done nil))
        (cl-letf* ((real #'monk-ui-check-policy)
                   ((symbol-function 'monk-ui-check-policy)
                    (lambda (&rest args) (unwind-protect (apply real args) (setq done t)))))
          (monk-translate-import 'ansible yaml m)
          (let ((t0 (float-time)))
            (while (and (not done) (< (- (float-time) t0) 240))
              (accept-process-output nil 0.2))
            (cl-incf monk-testdrive--waited (- (float-time) t0))))
        (delete-other-windows)
        (pop-to-buffer monk-translate-import-buffer-name)
        (display-buffer monk-ui-check-buffer-name '(display-buffer-below-selected))
        (monk-testdrive--ask "Two buffers: *monk-import* with a (defun monk-translate--imported-policy (m) ...) that uses native constructors (monk-directory, monk-file, monk-line-in-file, monk-command with :watch), and *monk-check* under it saying CHECK OK with the same resource names?  The echo area named the repair rounds, if any?")))))

(defun monk-testdrive--tour-embark ()
  "Embark on a resource name in a policy file and on a *monk* row."
  (monk-testdrive--step "tour" "embark-act on a resource string offers the Monk actions"
    (if (not (featurep 'embark))
        (cons 'SKIP "embark is not loaded")
      (require 'monk-embark)
      (monk-embark-setup)
      (let ((policy-file (monk-testdrive--path "tour-policy.el")))
        (with-temp-file policy-file
          (insert ";; -*- lexical-binding: t; -*-\n"
                  "(defun monk-testdrive--tour-embark-policy (_m)\n"
                  "  (monk-directory \"~/tour-ix\")\n"
                  "  (monk-file \"~/tour-ix/f\" :content \"x\\n\" :require '(\"~/tour-ix\")))\n"))
        (load policy-file nil t)
        (unwind-protect
            (let ((monk-batch-policy 'monk-testdrive--tour-embark-policy))
              (clrhash monk-resource-index)
              (delete-other-windows)
              (find-file policy-file)
              (goto-char (point-min))
              (goto-char (- (search-forward "'(\"~/tour-ix\")") 4))
              (monk-testdrive--ask "Point is inside the string \"~/tour-ix\" in the :require.  Press your embark-act key: does the prompt say 'monk-resource' and offer . p a h w r?  Press .: does point move to the (monk-directory \"~/tour-ix\") line?  Then embark-act again and w: a *monk-dependents* buffer listing ~/tour-ix/f with 'require'?"))
          (fmakunbound 'monk-testdrive--tour-embark-policy)
          (when (get-buffer "*monk-dependents*") (kill-buffer "*monk-dependents*"))
          (when (get-file-buffer policy-file) (kill-buffer (get-file-buffer policy-file)))))))
  (monk-testdrive--step "tour" "embark-act on a *monk* row offers the machine actions"
    (if (not (featurep 'embark))
        (cons 'SKIP "embark is not loaded")
      (let ((m (monk-testdrive--machine)))
        (monk-ui-show (monk-plan m #'monk-testdrive--policy))
        (monk-testdrive--focus-monk)
        (goto-char (point-min))
        (while (and (not (tabulated-list-get-id)) (not (eobp))) (forward-line 1))
        (monk-testdrive--ask "Point is on the drive row.  Press embark-act: does it say 'monk-machine' and offer . p i /?  Press i: does the inspector for drive open (q closes)?")))))

(defun monk-testdrive--tour-history ()
  "The history buffer."
  (monk-testdrive--step "tour" "h opens the history list; RET, c, r"
    (if (not (featurep 'monk-history))
        (cons 'SKIP "monk-history not loaded")
      (with-current-buffer (monk-ui--buffer) (save-window-excursion (monk-ui-history :host "drive")))
      (pop-to-buffer monk-ui-history-buffer-name)
      (delete-other-windows)
      (monk-testdrive--ask "Blue HISTORY box with 'N runs on drive' at the top, rows newest first with a purple 'plan' or blue 'apply' in the Kind column and colored Changed counts?  Press c on a row: does a list of changes open?  q closes it?"))))

(defconst monk-testdrive--tour-stops
  '(("Plan" monk-testdrive--tour-plan)
    ("Apply" monk-testdrive--tour-apply)
    ("Flapping" monk-testdrive--tour-flapping)
    ("Inspect and query" monk-testdrive--tour-inspect)
    ("History" monk-testdrive--tour-history)
    ("Facts and diffs" monk-testdrive--tour-facts)
    ("Fleet controls" monk-testdrive--tour-fleet)
    ("Translate" monk-testdrive--tour-translate)
    ("Embark" monk-testdrive--tour-embark)
    ("Watch" monk-testdrive--tour-watch)
    ("Text fallback" monk-testdrive--tour-text))
  "The tour's stops: (TITLE FUNCTION).")

;;;; Driver

(defun monk-testdrive--summary ()
  "Write the summary at the top of the report."
  (let* ((rs (reverse monk-testdrive--results))
         (count (lambda (st) (cl-count st rs :key #'caddr)))
         (fails (seq-filter (lambda (r) (eq (caddr r) 'FAIL)) rs))
         (text (concat
                (format "%d PASS, %d FAIL, %d SKIP in %.0fs\n"
                        (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                        (apply #'+ (mapcar (lambda (r) (or (nth 4 r) 0)) rs)))
                (if fails
                    (concat "Failed:\n"
                            (mapconcat (lambda (r) (format "  %s: %s%s" (car r) (cadr r)
                                                           (if (nth 3 r) (concat " -- " (nth 3 r)) "")))
                                       fails "\n")
                            "\n")
                  "")
                "\n")))
    (with-current-buffer (monk-testdrive--buf)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char monk-testdrive--summary-marker)
          (insert text)
          (monk-testdrive--fontify monk-testdrive--summary-marker (point)))))
    (when noninteractive
      (princ (with-current-buffer (monk-testdrive--buf)
               (buffer-substring-no-properties (point-min) (point-max)))))))

(defun monk-testdrive--begin (title)
  "Start a report for TITLE, ask consent, make the home."
  (setq monk-testdrive--results nil)
  (with-current-buffer (monk-testdrive--buf)
    (let ((inhibit-read-only t)) (erase-buffer)))
  (monk-testdrive--show-report)
  (setq monk-testdrive--layout (current-window-configuration))
  (monk-testdrive--out "%s\n%s  Emacs %s  notifications %s\n%s\n" title (format-time-string "%F %T")
                       emacs-version (or file-notify--library "none")
                       (monk-version-string))
  (with-current-buffer (monk-testdrive--buf)
    (setq monk-testdrive--summary-marker (copy-marker (point-max))))
  (unless (or noninteractive
              (yes-or-no-p
               (format "Run %s (a throwaway home under %s, nothing outside it changes)? "
                       title (abbreviate-file-name temporary-file-directory))))
    (monk-testdrive--out "(declined)")
    (user-error "Test drive declined"))
  (setq monk-testdrive--home (monk-testdrive--make-home))
  (monk-testdrive--out "Throwaway home: %s\n" monk-testdrive--home))

(defun monk-testdrive--cleanup ()
  "Delete the throwaway home and the buffers the drive made."
  (when (and monk-testdrive--home (file-directory-p monk-testdrive--home))
    (delete-directory monk-testdrive--home t))
  (setq monk-testdrive--home nil)
  (dolist (name (list (format monk-ui-detail-buffer-format "drive")
                      (format monk-ui-detail-buffer-format "drive facts")
                      (format monk-ui-detail-buffer-format "plan")
                      monk-ui-query-buffer-name
                      monk-ui-history-buffer-name))
    (when (get-buffer name) (kill-buffer name))))

(defconst monk-testdrive-kept-options
  '(monk-progress-functions monk-result-functions monk-history-mode monk-fact-cache-functions)
  "User options the drive leaves alone.
Hooks and modes are how the libraries wire themselves together, not
preferences.  Resetting `monk-progress-functions' once silently
turned history recording off.")

(defun monk-testdrive-option-symbols ()
  "Return every `monk-' user option the drive resets.
See `monk-testdrive--with-defaults'."
  (let (out)
    (mapatoms (lambda (sym)
                (when (and (string-prefix-p "monk-" (symbol-name sym))
                           (custom-variable-p sym)
                           (boundp sym)
                           (not (memq sym monk-testdrive-kept-options))
                           (not (string-suffix-p "-functions" (symbol-name sym)))
                           (not (string-suffix-p "-hook" (symbol-name sym)))
                           (not (string-suffix-p "-mode" (symbol-name sym))))
                  (push sym out))))
    out))

(defmacro monk-testdrive--with-defaults (&rest body)
  "Run BODY with every Monk user option at its standard value.
The drive and the tour must not depend on what this Emacs has
customized, and must not change it: a tour that set
`monk-watch-mode' back to assisted at its end once overwrote the
user's setting.  Options with no standard value keep theirs."
  (declare (indent 0))
  `(let* ((symbols (monk-testdrive-option-symbols))
          (values (mapcar (lambda (sym)
                            (if (get sym 'standard-value)
                                (eval (car (get sym 'standard-value)) t)
                              (symbol-value sym)))
                          symbols)))
     (cl-progv symbols values
       ,@body)))

(defun monk-testdrive--run (parts)
  "Run PARTS, each (TITLE FUNCTION), inside the throwaway home; then wrap up.
Nothing of this Emacs leaks in or out: HOME and MONK_HOME point at the
throwaway home, so the history database and fact cache are fresh; the
inventory, batch policy, and resource index are bound empty; every
Monk user option is at its default."
  (let ((process-environment (append (list (concat "HOME=" (directory-file-name monk-testdrive--home))
                                           (concat monk-home-environment-variable "="
                                                   (directory-file-name (monk-testdrive--path "monk-home"))))
                                     process-environment))
        (monk-inventory nil)
        (monk-inventory-derived nil)
        (monk-batch-policy nil)
        (monk-resource-index (make-hash-table :test #'equal))
        (monk-history--db nil)
        (monk-history--db-path nil))
    (make-directory (monk-testdrive--path "monk-home") t)
    (monk-testdrive--run-with-defaults parts)))

(defun monk-testdrive--run-with-defaults (parts)
  "Run PARTS as `monk-testdrive--run' does, under default options."
  (monk-testdrive--with-defaults
    (let ((monk-log-level 'info)
          (monk-ui-style (if (and (not noninteractive) (monk-ui-svg-p)) 'auto 'text)))
      (monk-testdrive--run-parts parts))))

(defun monk-testdrive--run-parts (parts)
  "Run PARTS, each (TITLE FUNCTION), in order, then clean up and report."
  (let ()
    (unwind-protect
        (condition-case nil
            (dolist (p parts)
              (funcall (cadr p)))
          (monk-testdrive-quit
           (monk-testdrive--out "\n(stopped by user)")))
      (ignore-errors (monk-watch-stop))
      (ignore-errors (monk-testdrive--cleanup))
      (monk-testdrive--summary)
      (let ((monk-testdrive--touring nil))
        (monk-testdrive--restore-layout))
      (unless noninteractive
        (message "monk-testdrive: finished; report in %s (q to close)" monk-testdrive-buffer-name)))))

(defun monk-testdrive--pick (parts prompt)
  "Let the user pick one of PARTS with PROMPT; return a one-element list."
  (let ((name (completing-read prompt (mapcar #'car parts) nil t)))
    (list (assoc name parts))))

;;;###autoload
(defun monk-testdrive (&optional one-section)
  "Run every feature of Monk against a throwaway home and report.
With ONE-SECTION, run one chosen section.  Nothing asks you anything
while it runs; every check reads files, results, or buffer text."
  (interactive "P")
  (monk-testdrive--begin "Monk test drive")
  (monk-testdrive--run (if one-section
                           (monk-testdrive--pick monk-testdrive--sections "Section: ")
                         monk-testdrive--sections)))

;;;###autoload
(defun monk-testdrive-tour (&optional one-stop)
  "Walk through what only eyes can check in *monk*; with ONE-STOP, one stop.
Each stop sets one thing up with nothing running underneath, shows the
buffer, and asks whether it looked right."
  (interactive "P")
  (when noninteractive
    (user-error "The tour needs a display"))
  (let ((monk-testdrive--touring t))
    (monk-testdrive--begin "Monk tour")
    (monk-testdrive--run (if one-stop
                             (monk-testdrive--pick monk-testdrive--tour-stops "Stop: ")
                           monk-testdrive--tour-stops)))
  ;; The run restored your layout with `monk-testdrive--touring' still
  ;; bound; now that it is off, show the report beside it once.
  (monk-testdrive--restore-layout)
  (pop-to-buffer monk-testdrive-buffer-name))

(monk-note-load 'monk-testdrive)
(provide 'monk-testdrive)
;;; monk-testdrive.el ends here
