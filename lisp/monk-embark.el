;;; monk-embark.el --- Embark targets and actions for Monk  -*- lexical-binding: t; -*-

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

;; Two Embark target types, `monk-resource' and `monk-machine', with
;; their action keymaps.  After `monk-embark-setup', `embark-act' on
;; a resource name offers: jump to its definition, plan or apply only
;; it and its dependents, its history, what depends on it, a rename
;; across the policy tree.  On a machine name: plan, inspect, query,
;; the inventory entry.
;;
;; A resource name is found at point in four ways: the `monk-ui-result'
;; text property in the report, detail, check, and query buffers; the
;; string literal at point in a Lisp buffer; the active region; a
;; path-like token at point anywhere, as in a comment or a YAML value.
;; The last three count only when the text is a name the index knows
;; or looks like a path.  The resource under point in a `*monk*' row is
;; the machine, so there the target is `monk-machine'.  The index fills as policies compile, and
;; `monk-resource-origin-by-name' compiles `monk-batch-policy' once
;; when a name is unknown, so the first action after a fresh Emacs
;; works too.
;;
;; Nothing here requires Embark at load time.  `monk-embark-setup'
;; adds the finders and keymaps when Embark is present; call it from
;; the :config of your embark block.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'monk)
(require 'monk-ui)

(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-general-map)
(declare-function embark--truncate-target "embark" (target))
(declare-function monk-ui-history-resource "monk-ui" (host name))
(declare-function monk-inventory-find "monk-inventory" (name))
(defvar monk-inventory)

;;;; Finding targets

(defun monk-embark--string-at-point ()
  "Return (TEXT START END) for the string literal at point, or nil.
Only in Lisp buffers."
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-data-mode)
    (let ((state (syntax-ppss)))
      (when (nth 3 state)
        (let ((start (nth 8 state)))
          (save-excursion
            (goto-char start)
            (condition-case nil
                (let ((end (progn (forward-sexp) (point))))
                  (list (buffer-substring-no-properties (1+ start) (1- end)) (1+ start) (1- end)))
              (error nil))))))))

(defun monk-embark--resource-name-p (text)
  "Return non-nil when TEXT is an indexed name or looks like a resource path."
  (and (not (string-empty-p text))
       (or (gethash text monk-resource-index)
           (string-match-p "\\`[~/]" text))))

(defun monk-embark-resource-target ()
  "Embark target finder: a Monk resource at point.
Return (monk-resource NAME . (START . END)), or nil."
  (if-let* ((result (or (get-text-property (point) 'monk-ui-result)
                        (get-text-property (line-beginning-position) 'monk-ui-result))))
      (let ((name (monk-result-name result)))
        (cons 'monk-resource (cons (propertize name 'monk-result result)
                                   (cons (line-beginning-position) (line-end-position)))))
    (pcase (or (monk-embark--string-at-point)
               (monk-embark--region-text)
               (monk-embark--token-at-point))
      (`(,text ,start ,end)
       (when (monk-embark--resource-name-p text)
         (cons 'monk-resource (cons text (cons start end))))))))

(defun monk-embark--region-text ()
  "Return (TEXT START END) for the active region, trimmed, or nil."
  (when (use-region-p)
    (let ((text (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
      (and (not (string-empty-p text)) (not (string-match-p "\n" text))
           (list text (region-beginning) (region-end))))))

(defun monk-embark--token-at-point ()
  "Return (TEXT START END) for the path-like token at point, or nil.
Reads it as a file name, so `~/.zshrc' in a comment, a YAML value, or
a log line counts; a resource name with a `#' part, as line-in-file
names have, is read whole."
  (when-let* ((bounds (save-excursion
                        (let ((chars "-[:alnum:]_~/.#@+=,:%"))
                          (skip-chars-backward chars)
                          (let ((start (point)))
                            (skip-chars-forward chars)
                            (and (< start (point)) (cons start (point))))))))
    (let ((text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)) "[.,:]+" "[.,:]+")))
      (and (not (string-empty-p text)) (list text (car bounds) (cdr bounds))))))

(defun monk-embark-machine-target ()
  "Embark target finder: a Monk machine at point.
A `*monk*' row, a heading with the `monk-ui-machine' property, or a
symbol that names an inventory host in a Lisp buffer."
  (cond
   ((and (derived-mode-p 'monk-ui-mode) (tabulated-list-get-id))
    (cons 'monk-machine (cons (tabulated-list-get-id)
                              (cons (line-beginning-position) (line-end-position)))))
   ((or (get-text-property (point) 'monk-ui-machine)
        (get-text-property (line-beginning-position) 'monk-ui-machine))
    (let ((m (or (get-text-property (point) 'monk-ui-machine)
                 (get-text-property (line-beginning-position) 'monk-ui-machine))))
      (cons 'monk-machine (cons (oref m name) (cons (line-beginning-position) (line-end-position))))))
   ((and (derived-mode-p 'emacs-lisp-mode 'lisp-data-mode)
         (bound-and-true-p monk-inventory)
         (fboundp 'monk-inventory-find))
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
                (_ (monk-inventory-find text)))
      (cons 'monk-machine (cons text bounds))))))

;;;; Resolving

(defun monk-embark--machine (name)
  "Return the machine called NAME.
From the inventory when it has one, else a local machine named so."
  (or (and (fboundp 'monk-inventory-find) (bound-and-true-p monk-inventory)
           (monk-inventory-find name))
      (monk-local-machine :name name)))

(defun monk-embark--policy ()
  "Return the policy for a resource action.
The one shown in `*monk*' when it is still a function, else
`monk-batch-policy'."
  (let ((shown (let ((buffer (get-buffer monk-ui-buffer-name)))
                 (and buffer (with-current-buffer buffer
                               (and monk-ui--run (monk-run-policy monk-ui--run)))))))
    (or (and (functionp shown) shown)
        monk-batch-policy
        (user-error "No policy known: run a plan first, or set monk-batch-policy"))))

(defun monk-embark--machines-for (name)
  "Return the machines a resource called NAME was compiled for, as objects."
  (let ((names (plist-get (gethash name monk-resource-index) :machines)))
    (unless names
      (monk-resource-origin-by-name name)
      (setq names (plist-get (gethash name monk-resource-index) :machines)))
    (mapcar #'monk-embark--machine (or names (list (oref (monk-local-machine) name))))))

;;;; Resource actions

(defun monk-embark-find-resource (name)
  "Jump to where resource NAME is defined.
With a result at point, use its recorded origin; else the index, and
when the index is empty, compile the batch policy once to fill it."
  (interactive (list (read-string "Resource: " nil nil nil)))
  (let ((result (get-text-property 0 'monk-result name)))
    (if result
        (monk-result-find-definition result)
      (let ((origin (monk-resource-origin-by-name name)))
        (unless origin
          (user-error "No policy compiled defines %s; run a plan or check first" name))
        (monk-find-origin origin)))))

(defun monk-embark-plan-resource (name &optional apply)
  "Plan only resource NAME and its dependents on the machines it compiles for.
With APPLY, the prefix argument, apply instead of plan."
  (interactive (list (completing-read "Resource: " (monk-resource-names) nil nil) current-prefix-arg))
  (let* ((machines (monk-embark--machines-for name))
         (keys (mapcar (lambda (m) (cons (oref m name) name)) machines))
         (run (monk-orchestrate machines (monk-embark--policy)
                                :dry-run (not apply) :only keys)))
    (monk-run-report run)))

(defun monk-embark-apply-resource (name)
  "Apply only resource NAME and its dependents."
  (interactive (list (completing-read "Resource: " (monk-resource-names) nil nil)))
  (monk-embark-plan-resource name t))

(defun monk-embark-resource-history (name)
  "Show every time resource NAME changed, on the first machine it compiles for."
  (interactive (list (completing-read "Resource: " (monk-resource-names) nil nil)))
  (require 'monk-history)
  (monk-ui-history-resource (oref (car (monk-embark--machines-for name)) name) name))

(defun monk-embark-resource-dependents (name)
  "List the resources that require, watch, or otherwise point at NAME."
  (interactive (list (completing-read "Resource: " (monk-resource-names) nil nil)))
  (when (null (gethash name monk-resource-index))
    (monk-resource-origin-by-name name))
  (let ((dependents (monk-resource-dependents-by-name name))
        (entry (gethash name monk-resource-index)))
    (if (null dependents)
        (message "Nothing points at %s%s" name
                 (if entry "" " (not in any compiled policy)"))
      (with-current-buffer (get-buffer-create "*monk-dependents*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (special-mode)
          (local-set-key (kbd "q") #'monk-ui-quit)
          (local-set-key (kbd ".") #'monk-ui-find-definition)
          (insert (propertize name 'face 'monk-ui-machine)
                  (format "  %s  on %s\n\n" (plist-get entry :type)
                          (string-join (plist-get entry :machines) " ")))
          (dolist (cell dependents)
            (let ((other (gethash (car cell) monk-resource-index))
                  (start (point)))
              (insert (format "  %-10s %-14s %s\n" (cdr cell) (plist-get other :type) (car cell)))
              (put-text-property start (point) 'monk-ui-result
                                 (monk-result-create :name (car cell) :type (plist-get other :type)
                                                     :origin (plist-get other :origin)))))
          (goto-char (point-min)))
        (pop-to-buffer (current-buffer))))))

(defun monk-embark-rename-resource (name new)
  "Rename resource NAME to NEW across the policy tree, with confirmation.
Searches the project of the policy file for the string literal and
runs `query-replace' over the matches, so requisites that name it
follow.  The index is refreshed by the next compile."
  (interactive
   (let ((name (completing-read "Rename resource: " (monk-resource-names) nil nil)))
     (list name (read-string (format "Rename %s to: " name) name))))
  (let* ((origin (or (monk-resource-origin-by-name name)
                     (user-error "No policy compiled defines %s" name)))
         (file (monk-origin-file origin))
         (default-directory (file-name-directory file))
         (literal (format "%S" name))
         (replacement (format "%S" new)))
    (require 'project)
    (if-let* ((project (project-current nil default-directory)))
        (progn
          (xref-push-marker-stack)
          (project-query-replace-regexp (regexp-quote literal) replacement))
      (find-file file)
      (goto-char (point-min))
      (query-replace literal replacement))))

;;;; Machine actions

(defun monk-embark-plan-machine (name &optional apply)
  "Plan the known policy on machine NAME.
With APPLY, the prefix argument, apply instead."
  (interactive (list (read-string "Machine: ") current-prefix-arg))
  (monk-run-report (monk-orchestrate (list (monk-embark--machine name)) (monk-embark--policy)
                                     :dry-run (not apply))))

(defun monk-embark-inspect-machine (name)
  "Open the inspector for machine NAME, from the inventory or the current run."
  (interactive (list (read-string "Machine: ")))
  (let ((m (monk-embark--machine name)))
    (unless (oref m facts) (monk-facts-load m))
    ;; The inspector reads the run buffer or the inventory; an ad hoc
    ;; local machine is neither, so it is shown through a run with no
    ;; nodes, which is how the inspector finds machines anyway.
    (if (or (monk-ui--run-machine* name) (and (featurep 'monk-inventory) (monk-inventory-find name)))
        (monk-ui-inspect-machine name)
      (with-current-buffer (monk-ui-show (monk-run-create :machines (list m) :passes 0 :dry-run t))
        (monk-ui-inspect-machine name)))))

(defun monk-ui--run-machine* (name)
  "Return the machine NAME in the run shown in `*monk*', or nil."
  (let ((buffer (get-buffer monk-ui-buffer-name)))
    (and buffer (with-current-buffer buffer (monk-ui--run-machine name)))))

(defun monk-embark-query-machine (name)
  "Open the guard query buffer over machine NAME alone."
  (interactive (list (read-string "Machine: ")))
  (monk-ui-query (read-string "Guard (CEL, or CELL in parentheses): " nil 'monk-ui-query-history)
                 (list (monk-embark--machine name))))

(defun monk-embark-find-machine (name)
  "Jump to the inventory entry of machine NAME."
  (interactive (list (read-string "Machine: ")))
  (monk-machine-find-definition (monk-embark--machine name)))

;;;; Keymaps and setup

(defvar monk-embark-resource-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") #'monk-embark-find-resource)
    (define-key map (kbd "p") #'monk-embark-plan-resource)
    (define-key map (kbd "a") #'monk-embark-apply-resource)
    (define-key map (kbd "h") #'monk-embark-resource-history)
    (define-key map (kbd "w") #'monk-embark-resource-dependents)
    (define-key map (kbd "r") #'monk-embark-rename-resource)
    map)
  "Embark actions on a Monk resource name.")

(defvar monk-embark-machine-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") #'monk-embark-find-machine)
    (define-key map (kbd "p") #'monk-embark-plan-machine)
    (define-key map (kbd "i") #'monk-embark-inspect-machine)
    (define-key map (kbd "/") #'monk-embark-query-machine)
    map)
  "Embark actions on a Monk machine name.")

;;;###autoload
(defun monk-embark-setup ()
  "Teach Embark about Monk resources and machines.
Puts the two target finders first in `embark-target-finders', ahead
of the region and file finders, so a resource name is a resource
before it is a file, a string, or the region; binds the keymaps.
Safe to call twice.  Does nothing without Embark; say so when called
interactively."
  (interactive)
  (if (not (and (featurep 'embark) (boundp 'embark-target-finders)
                (boundp 'embark-keymap-alist) (boundp 'embark-general-map)))
      (progn (when (called-interactively-p 'any)
               (message "monk-embark: embark is not loaded; nothing to set up"))
             nil)
    (setq embark-target-finders
          (append '(monk-embark-resource-target monk-embark-machine-target)
                  (seq-remove (lambda (f) (memq f '(monk-embark-resource-target monk-embark-machine-target)))
                              embark-target-finders)))
    (set-keymap-parent monk-embark-resource-map embark-general-map)
    (set-keymap-parent monk-embark-machine-map embark-general-map)
    (setf (alist-get 'monk-resource embark-keymap-alist) 'monk-embark-resource-map
          (alist-get 'monk-machine embark-keymap-alist) 'monk-embark-machine-map)
    (when (called-interactively-p 'any)
      (message "monk-embark: finders %s; %d finders in all"
               (seq-position embark-target-finders 'monk-embark-resource-target)
               (length embark-target-finders)))
    t))

(defun monk-embark-status ()
  "Say whether Embark knows Monk's targets, and what the finder sees at point."
  (interactive)
  (let ((registered (and (boundp 'embark-target-finders)
                         (memq 'monk-embark-resource-target embark-target-finders)))
        (resource (monk-embark-resource-target))
        (machine (monk-embark-machine-target)))
    (message "monk-embark %s: finders %s (position %s); at point: %s"
             monk-build
             (if registered "registered" "NOT registered (call monk-embark-setup after embark loads)")
             (and registered (seq-position embark-target-finders 'monk-embark-resource-target))
             (cond (resource (format "resource %S" (cadr resource)))
                   (machine (format "machine %S" (cadr machine)))
                   (t "nothing Monk")))))

(with-eval-after-load 'embark (monk-embark-setup))

(monk-note-load 'monk-embark)
(provide 'monk-embark)
;;; monk-embark.el ends here
