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
                      (monk-line-in-file "~/absent-file" "x" :ignore-errors t)
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

(defconst monk-testdrive--sections
  '(("Converge" monk-testdrive--section-converge)
    ("Plan and apply" monk-testdrive--section-plan)
    ("Requisites and convergence" monk-testdrive--section-graph)
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

(defconst monk-testdrive--tour-stops
  '(("Plan" monk-testdrive--tour-plan)
    ("Apply" monk-testdrive--tour-apply)
    ("Flapping" monk-testdrive--tour-flapping)
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
                      (format monk-ui-detail-buffer-format "plan")))
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
