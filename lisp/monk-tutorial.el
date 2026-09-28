;;; monk-tutorial.el --- Interactive tutorial in a sandbox  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: help, tools

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

;; `M-x monk-tutorial' opens a lesson buffer beside a sandbox home.
;; Each lesson is prose followed by Lisp forms.  Put point in a form
;; and press `C-c C-c' to run it against the sandbox; the result shows
;; in the echo area and in *monk*.  `C-c C-n' and `C-c C-p' move
;; between lessons, `C-c C-r' resets the sandbox, `q' ends the
;; tutorial and deletes it.
;;
;; The forms are real: they call the same functions a policy file
;; calls.  `tutorial-home' is bound to the sandbox and `tutorial-m' to
;; a machine whose home is the sandbox, so `~' in every path lands
;; there and nothing outside it changes.
;;
;; The same lessons as prose are in docs/tutorial.md.

;;; Code:

(require 'cl-lib)
(require 'monk)
(require 'monk-file)
(require 'monk-system)
(require 'monk-package)
(require 'monk-inventory)
(require 'monk-ui)
(require 'monk-watch)
(require 'monk-event)
(require 'monk-agent)

(defconst monk-tutorial-buffer-name "*monk-tutorial*"
  "The lesson buffer.")

(defconst monk-tutorial-heading-regexp "^;;; \\* "
  "Start of a lesson.  A comment, so `read' skips it.")

(defvar monk-tutorial--home nil
  "The sandbox home, with a trailing slash.")

(defvar monk-tutorial--machine nil
  "The machine the lessons run against.")

(defvar tutorial-m nil
  "The sandbox machine, for forms in the lesson buffer.")

(defvar tutorial-home nil
  "The sandbox home directory, for forms in the lesson buffer.")

(defgroup monk-tutorial nil
  "Interactive Monk tutorial."
  :group 'monk
  :prefix "monk-tutorial-")

(defface monk-tutorial-heading
  '((t :inherit bold :height 1.2))
  "Lesson heading.")

;;;; Sandbox

(defun monk-tutorial--make-home ()
  "Create the sandbox home with a few files to manage."
  (let ((dir (file-name-as-directory (make-temp-file "monk-tutorial-" t))))
    (make-directory (expand-file-name "dotfiles" dir))
    (with-temp-file (expand-file-name "dotfiles/zshrc" dir) (insert "export EDITOR=emacs\n"))
    (with-temp-file (expand-file-name "dotfiles/gitconfig" dir) (insert "[user]\n\tname = You\n"))
    dir))

(defun monk-tutorial--machine ()
  "Return a local machine whose home is the sandbox."
  (let ((m (monk-local-machine :name "sandbox")))
    (monk-gather-facts m)
    (setf (alist-get 'home (oref m facts)) (directory-file-name monk-tutorial--home))
    (monk-add-class m 'tutorial)
    m))

(defun monk-tutorial-reset ()
  "Delete the sandbox and make a fresh one."
  (interactive)
  (ignore-errors (monk-watch-stop))
  (when (and monk-tutorial--home (file-directory-p monk-tutorial--home))
    (delete-directory monk-tutorial--home t))
  (setq monk-tutorial--home (monk-tutorial--make-home)
        monk-tutorial--machine (monk-tutorial--machine)
        tutorial-home monk-tutorial--home
        tutorial-m monk-tutorial--machine)
  (message "Sandbox: %s" monk-tutorial--home))

(defun monk-tutorial-quit ()
  "End the tutorial: stop any watch, delete the sandbox, bury the buffer."
  (interactive)
  (ignore-errors (monk-watch-stop))
  (when (and monk-tutorial--home (file-directory-p monk-tutorial--home))
    (delete-directory monk-tutorial--home t))
  (setq monk-tutorial--home nil tutorial-home nil tutorial-m nil)
  (quit-window t))

;;;; Running forms

(defun monk-tutorial-run ()
  "Evaluate the top-level form at point against the sandbox.
The result is shown in the echo area.  A `monk-run' is also shown in
*monk*."
  (interactive)
  (unless monk-tutorial--home
    (monk-tutorial-reset))
  (let* ((form (save-excursion
                 (end-of-defun)
                 (beginning-of-defun)
                 (read (current-buffer))))
         (default-directory monk-tutorial--home)
         (process-environment (cons (concat "HOME=" (directory-file-name monk-tutorial--home))
                                    process-environment))
         (value (eval form t)))
    (when (monk-run-p value)
      (monk-run-report value))
    (message "%s" (cond
                   ((monk-run-p value) (monk-run-summary value))
                   ((stringp value) value)
                   (t (truncate-string-to-width (format "%S" value) 200 nil nil "…"))))
    value))

(defun monk-tutorial-next ()
  "Move to the next lesson."
  (interactive)
  (end-of-line)
  (if (re-search-forward monk-tutorial-heading-regexp nil t)
      (beginning-of-line)
    (message "Last lesson")))

(defun monk-tutorial-previous ()
  "Move to the previous lesson."
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward monk-tutorial-heading-regexp nil t)
    (message "First lesson")))

(defvar monk-tutorial-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'monk-tutorial-run)
    (define-key map (kbd "C-c C-n") #'monk-tutorial-next)
    (define-key map (kbd "C-c C-p") #'monk-tutorial-previous)
    (define-key map (kbd "C-c C-r") #'monk-tutorial-reset)
    (define-key map (kbd "C-c C-q") #'monk-tutorial-quit)
    map)
  "Keymap for `monk-tutorial-mode'.")

(define-derived-mode monk-tutorial-mode emacs-lisp-mode "Monk tutorial"
  "Lessons with runnable forms.
\\{monk-tutorial-mode-map}"
  (setq-local comment-start ";; ")
  (font-lock-add-keywords nil `((,(concat monk-tutorial-heading-regexp ".*$") 0 'monk-tutorial-heading t)))
  (visual-line-mode 1))

;;;; Lessons

(defconst monk-tutorial--text
  ";;; Monk tutorial.  C-c C-c runs the form at point.  C-c C-n next lesson.
;;; C-c C-r resets the sandbox.  C-c C-q ends and deletes it.
;;;
;;; Two variables are bound for you: `tutorial-home', a throwaway
;;; directory, and `tutorial-m', a machine whose home is that
;;; directory.  Every ~ below lands in the sandbox.  The *monk* buffer
;;; opens on the right after each run; look at it after every form.

;;; * 1. A resource is a promise

;; A policy is a function of one argument, the machine.  Inside it,
;; constructors describe the state you want.  Nothing runs yet.

(defun tutorial-policy-1 (_m)
  (monk-directory \"~/.ssh\" :mode #o700)
  (monk-symlink \"~/.zshrc\" \"~/dotfiles/zshrc\")
  (monk-file \"~/.config/app.conf\" :content \"level=3\\n\"))

;; monk-converge checks each resource and repairs it.  Run this twice.
;; The first run says repaired; the second says kept, in one pass.
;; That is the whole idea: the same policy, applied again, changes
;; nothing.

(monk-converge tutorial-m #'tutorial-policy-1)

;; Look at the file.  The link target was expanded on the machine.

(file-symlink-p (expand-file-name \".zshrc\" tutorial-home))

;;; * 2. Plan, then apply

;; A plan is a dry run.  It returns would-repair where a change is
;; needed and touches nothing.  Break something first:

(with-temp-file (expand-file-name \".config/app.conf\" tutorial-home)
  (insert \"level=9\\n\"))

;; The plan buffer has a purple PLAN badge, a Would column, and a
;; diff in the detail view (RET on the row).  Press a in *monk* to
;; apply, or run the next form.

(setq tutorial-plan (monk-plan tutorial-m #'tutorial-policy-1))

;; Apply repeats the plan for real and compares.  The Plan column
;; says as planned.  Change the file by hand between plan and apply
;; and the column says unfulfilled or unplanned instead.

(monk-apply tutorial-plan)

;;; * 3. Classes and guards

;; Facts become classes: linux, darwin, debian, the machine name.
;; Inventory variables that are t become classes too.  The sandbox
;; machine carries the class `tutorial'.

(oref tutorial-m classes)

;; :classes runs a resource only when every class is present.
;; :only-if and :not-if take a function of the machine.  Skipped
;; resources show as skipped, not failed.

(monk-converge tutorial-m
  (lambda (m)
    (monk-file \"~/on-tutorial\" :content \"yes\\n\" :classes '(tutorial))
    (monk-file \"~/on-mars\" :content \"no\\n\" :classes '(mars))
    (monk-file \"~/only-if\" :content \"guard\\n\"
               :only-if (lambda (m) (monk-class-p m 'linux)))))

;; :when takes a CELL form: CEL in Lisp.  CELL is data, so Monk can
;; check it when the policy compiles, and a skipped resource says
;; which clause was false.  (class X), (fact KEY), (var KEY), (group
;; X), and (local) read the machine.  and, or, not, =, <, in,
;; starts-with, match, and size combine them.

(monk-cell-eval tutorial-m '(and (class tutorial) (= (fact kernel) \"Linux\")))

;; The same guard as a CEL string.  Monk translates CEL to CELL once,
;; at compile time.  Both notations are one language.

(monk-cel-parse \"'tutorial' in classes && facts.kernel == 'Linux'\")

(monk-cell-to-cel '(or (class mars) (> (size (classes)) 100)))

;; A skipped resource names the false clause in its comment.

(monk-converge tutorial-m
  (lambda (m)
    (monk-file \"~/when-cell\" :content \"cell\\n\"
               :when '(and (class tutorial) (not (class mars))))
    (monk-file \"~/when-cel\" :content \"cel\\n\"
               :when \"'mars' in classes || facts.arch == 'nope'\")))

;; Ask why.  monk-cell-explain walks the guard to the deciding part.

(monk-cell-explain tutorial-m '(and (class tutorial) (class mars)))

;; Which machines match?  monk-cell-select filters a list of machines.
;; In the *monk* buffer, / opens the same question as a buffer with
;; every machine, its verdict, and its facts folded under it.

(mapcar (lambda (m) (oref m name))
        (monk-cell-select \"'tutorial' in classes\" (list tutorial-m)))

;; What can a guard see?  monk-machine-describe lists classes, groups,
;; facts, and vars.  In *monk*, i shows this for the machine at point,
;; and each detail buffer starts with it folded; TAB opens it.

(monk-machine-describe tutorial-m)

;;; * 4. Requisites: require, watch, onchanges, onfail

;; :require orders and gates.  :watch also runs an action when the
;; watched resource changed: a systemd unit restarts, a command runs.
;; A command without :creates or :unless is always kept, so with
;; :watch it is a handler.

(defun tutorial-policy-4 (_m)
  (monk-file \"~/etc/app.conf\" :content \"level=3\\n\")
  (monk-command \"reload-app\" \"sh\"
                :args (list \"-c\" (format \"date >> %s\" (monk-expand \"~/etc/reloads\")))
                :watch '(\"~/etc/app.conf\"))
  (monk-command \"after-reload\" \"true\" :require '(\"reload-app\")))

;; First run: the file is created, so reload-app runs once, then
;; after-reload.  Second run: the file is kept, reload-app is kept
;; and does not run.

(monk-converge tutorial-m #'tutorial-policy-4)

(with-temp-buffer
  (insert-file-contents (expand-file-name \"etc/reloads\" tutorial-home))
  (count-lines 1 (point-max)))

;; A requisite that names nothing is a policy error, raised before
;; anything runs.

(condition-case err
    (monk-converge tutorial-m (lambda (_m) (monk-file \"~/x\" :content \"\" :require '(\"nope\"))))
  (monk-requisite-error (error-message-string err)))

;;; * 5. Convergence and flapping

;; Monk runs passes until a pass repairs nothing.  Two resources that
;; disagree about one file never settle.  Monk notices when a resource
;; repairs a second time, stops, and names it.

(monk-converge tutorial-m
  (lambda (_m)
    (monk-file \"~/fight\" :content \"a\\n\")
    (monk-line-in-file \"~/fight\" \"b\" :create t)))

;; The header shows an amber flapping badge; hover it for the names.

(monk-run-flapping (monk-converge tutorial-m
  (lambda (_m)
    (monk-file \"~/fight\" :content \"a\\n\")
    (monk-line-in-file \"~/fight\" \"b\" :create t))))

;;; * 6. Degree: success is not binary

;; A set resource reports how much of it holds.  Fake a package
;; manager that has two of four packages and refuses to install more.
;; The result is partial with degree 0.5, and a :degree threshold on a
;; requisite can accept it.

(cl-letf (((symbol-function 'monk-exec)
           (lambda (_m program &rest _)
             (if (equal program \"dpkg-query\")
                 (monk-exec-create :exit-code 0
                                   :output \"a installed\\nb installed\\nc x\\nd x\\n\")
               (monk-exec-create :exit-code 1 :output \"apt refused\")))))
  (let ((m (monk-local-machine :name \"deb\")))
    (oset m facts '((os-family . \"Debian\") (home . \"/tmp\")))
    (oset m classes (monk-hard-classes m))
    (monk-converge m
      (lambda (_m)
        (monk-apt \"pkgs\" '(\"a\" \"b\" \"c\" \"d\"))
        (monk-command \"needs-all\" \"true\" :require '(\"pkgs\"))
        (monk-command \"needs-half\" \"true\" :require '((:ref \"pkgs\" :degree 0.5))))
      :passes 1)))

;;; * 7. Several machines as one graph

;; monk-orchestrate compiles the policy for a list of machines and
;; runs every resource in one dependency graph, so a requisite can
;; point at another machine or at a group with a threshold.  Here two
;; sandbox machines share the group `pool'.

(let ((a (monk-tutorial--machine)) (b (monk-tutorial--machine)))
  (oset a name \"a\") (oset b name \"b\")
  (dolist (x (list a b)) (oset x groups '(pool)) (oset x classes (monk-hard-classes x)))
  (monk-orchestrate (list a b)
    (lambda (m)
      (monk-file \"~/member\" :content (oref m name))
      (when (equal (oref m name) \"b\")
        (monk-file \"~/after-a\" :content \"ok\\n\" :require '((a . \"~/member\")))
        (monk-file \"~/after-any\" :content \"ok\\n\"
                   :require '((:ref (pool . \"~/member\") :min 1)))))))

;;; * 8. Watching for drift

;; monk-watch-start keeps a policy enforced.  Local paths use
;; filenotify; remote machines poll.  In assisted mode a change opens
;; a plan in *monk*; in autopilot it repairs.  Some sandboxes deliver
;; no notifications; this checks first.

(monk-watch-notifications-work-p)

;; Start watching, then edit the file from another buffer or with the
;; form after it.  Within a second *monk* shows a PLAN whose header
;; says drift: and the path.  W in *monk* switches to autopilot.

(progn (setq monk-watch-mode 'assisted)
       (monk-watch-start tutorial-m #'tutorial-policy-1)
       (monk-watch-status))

(with-temp-file (expand-file-name \".config/app.conf\" tutorial-home)
  (insert \"drifted\\n\"))

;; Stop the watch before the next lesson.

(monk-watch-stop)

;;; * 9. Events and the agent

;; Every result serializes to one JSON line.  This is the ledger a
;; standalone agent writes, the stream a controller reads, and the
;; shape other tools normalize to.

(mapcar #'monk-event-line
        (monk-run-events (monk-converge tutorial-m #'tutorial-policy-1)))

;; The agent's query endpoint answers the same way.  Ask it for status
;; against a ledger in the sandbox.

(let ((monk-agent-ledger-file (expand-file-name \"state/events.ndjson\" tutorial-home)))
  (monk-agent-record (monk-converge tutorial-m #'tutorial-policy-1))
  (with-output-to-string (monk-agent-handle '((op . \"status\")))))

;; The systemd units the policy would install on a Linux host:

(monk-agent-service-unit)

(monk-agent-socket-unit)

;;; * 10. Your own resource

;; A resource is a class with a check and a repair.  Check must not
;; change the machine; it returns nil or a plist of changes.  Repair
;; applies the plist and returns a comment.  The constructor calls
;; monk-add-resource.  This one keeps a file's line count at N.

(defclass tutorial-lines-resource (monk-resource)
  ((count :initarg :count :type integer)))

(cl-defmethod monk-check ((r tutorial-lines-resource) (m monk-machine))
  (let* ((text (or (monk-file-read m (oref r name)) \"\"))
         (have (length (split-string text \"\\n\" t))))
    (unless (= have (oref r count))
      (list :lines (list have (oref r count))))))

(cl-defmethod monk-repair ((r tutorial-lines-resource) (m monk-machine) changes)
  (monk-file-write m (oref r name)
                   (mapconcat (lambda (i) (format \"line %d\" i))
                              (number-sequence 1 (oref r count)) \"\\n\"))
  (format \"%d -> %d lines\" (car (plist-get changes :lines)) (oref r count)))

(defun tutorial-lines (path count &rest args)
  (monk-add-resource
   (apply #'tutorial-lines-resource :name path
          (monk--resource-plist args :count count))))

(monk-converge tutorial-m (lambda (_m) (tutorial-lines \"~/ten\" 10)))

;; That is every feature.  C-c C-q deletes the sandbox.  The README has
;; the full resource catalog; docs/tutorial.md has these lessons as
;; prose.
"
  "The lesson text.")

;;;###autoload
(defun monk-tutorial ()
  "Open the Monk tutorial in a sandbox."
  (interactive)
  (monk-tutorial-reset)
  (let ((buffer (get-buffer-create monk-tutorial-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-tutorial-mode)
        (monk-tutorial-mode)
        (insert monk-tutorial--text)
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)
    (message "Sandbox %s.  C-c C-c runs the form at point." monk-tutorial--home)))

(defun monk-tutorial-forms ()
  "Return every top-level form in the lesson text, in order."
  (with-temp-buffer
    (insert monk-tutorial--text)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun monk-tutorial-check ()
  "Evaluate every lesson form against a fresh sandbox; return the failures.
Each failure is (FORM . ERROR-STRING).  Used by the test suite, so a
lesson cannot rot.  The watch lesson runs with notifications stubbed."
  (monk-tutorial-reset)
  (unwind-protect
      (let ((monk-report-function #'ignore)
            (monk-log-level nil)
            (monk-watch--delivery 'no)
            (default-directory monk-tutorial--home)
            (process-environment (cons (concat "HOME=" (directory-file-name monk-tutorial--home))
                                       process-environment))
            failures)
        (dolist (form (monk-tutorial-forms))
          (condition-case err
              (eval form t)
            (error (push (cons form (error-message-string err)) failures))))
        (nreverse failures))
    (ignore-errors (monk-watch-stop))
    (when (and monk-tutorial--home (file-directory-p monk-tutorial--home))
      (delete-directory monk-tutorial--home t))
    (setq monk-tutorial--home nil)))

(provide 'monk-tutorial)
;;; monk-tutorial.el ends here
