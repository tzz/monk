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
(require 'monk-ui)
(require 'monk-watch)
(require 'monk-event)
(require 'monk-agent)
(require 'filenotify)
(require 'vc-git)
(require 'monk-package)
(require 'monk-bridge)
(require 'monk-sync)

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
  "Show the detail buffer for machine NAME below `*monk*', and select it."
  (monk-testdrive--focus-monk)
  (with-current-buffer monk-ui-buffer-name
    (save-window-excursion (monk-ui-show-machine name)))
  (let ((detail (get-buffer (format monk-ui-detail-buffer-format name))))
    (select-window (split-window-below (max 6 (/ (window-height) 4))))
    (switch-to-buffer detail)
    (goto-char (point-min))))

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
        (save-window-excursion (monk-ui-show-machine "drive")))
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
              (save-window-excursion (monk-ui-show-machine "drive")))
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
    (monk-testdrive--step "salt" "compound matcher translates to CELL and selects machines"
      (let ((form (monk-salt-matcher-to-cell "G@kernel:Linux and not L@nobody")))
        (monk-testdrive--checks
         (cons (equal '(and (class linux) (not (in (name) (list "nobody")))) form) (format "%S" form))
         (cons (equal '("drive") (mapcar (lambda (x) (oref x name)) (monk-cell-select form (list m))))
               "drive did not match"))))))

(defconst monk-testdrive--sections
  '(("Converge" monk-testdrive--section-converge)
    ("Plan and apply" monk-testdrive--section-plan)
    ("Requisites and convergence" monk-testdrive--section-graph)
    ("Guards and failures" monk-testdrive--section-guards)
    ("Inspect and navigate" monk-testdrive--section-inspect)
    ("Salt-style features" monk-testdrive--section-salt)
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
        (setq monk-watch-mode 'assisted)
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
      (monk-testdrive--ask "Second line reads '▸ drive: N classes, N facts, N vars (TAB to expand)'.  Press TAB on it: does it turn into ▾ with class chips and a facts table under it?  Below, is the 'kept (N)' group folded with '(TAB to expand)' while 'repaired' is open?  Press q: does the detail close and *monk* stay?"))
    (monk-testdrive--step "tour" "/ query: matches, explanations, folded facts"
      (monk-testdrive--focus-monk)
      (save-window-excursion (monk-ui-query "'drive' in classes && facts.arch == 'nope'" (list m)))
      (pop-to-buffer monk-ui-query-buffer-name)
      (delete-other-windows)
      (prog1 (monk-testdrive--ask "Purple QUERY line at the top, then CELL and CEL renderings, '0 of 1 machine match', a grey '– drive' row with 'facts.arch == 'nope' is false: ...' after it, and a folded ▸ drive line?  Press e: does it ask for a new guard (C-g to cancel)?")
        (kill-buffer monk-ui-query-buffer-name)))
    (monk-testdrive--step "tour" "focus lands in *monk* after a command run"
      (with-current-buffer (monk-testdrive--buf) (delete-other-windows (get-buffer-window (current-buffer) t)))
      (switch-to-buffer (monk-testdrive--buf))
      (monk-run-report (monk-plan m #'monk-testdrive--policy))
      (redisplay)
      (monk-testdrive--ask "The cursor is now in the *monk* window (not in the report), so 'a' would apply at once?"))))

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
  (monk-testdrive--out "%s\n%s  Emacs %s  notifications %s\n" title (format-time-string "%F %T")
                       emacs-version (or file-notify--library "none"))
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

(defun monk-testdrive--run (parts)
  "Run PARTS, each (TITLE FUNCTION), inside the throwaway home; then wrap up."
  (let ((process-environment (cons (concat "HOME=" (directory-file-name monk-testdrive--home))
                                   process-environment))
        (monk-log-level 'info)
        (monk-ui-style (if (and (not noninteractive) (monk-ui-svg-p)) 'auto 'text)))
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

(provide 'monk-testdrive)
;;; monk-testdrive.el ends here
