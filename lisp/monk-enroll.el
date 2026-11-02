;;; monk-enroll.el --- Inventory editor and machine enrollment  -*- lexical-binding: t; -*-

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

;; Two things that change the inventory tree instead of a policy run.
;;
;; `monk-inventory-edit' opens `*monk-inventory*': the group tree as
;; folds, one line per host with its address, method, user, and vars
;; as pills, and the classes its facts gave it when they are known.
;; Keys add, move, rename, and remove hosts and groups and set vars;
;; every change rebuilds the machines at once and `s' writes the
;; inventory (`monk-inventory-path').  `.' jumps to the host's
;; entry in that file.  The tree in memory is the one `monk-inventory'
;; was built from, so a policy file that defines its inventory in code
;; can be edited too, and then migrated with `monk-inventory-migrate'.
;;
;; `monk-enroll' runs on a machine that is not in the inventory yet,
;; or from a controller for a host it can reach.  It gathers facts,
;; proposes a group from the classes (a Mac goes where the other Macs
;; are), writes the entry, saves the file, and offers to install the
;; agent for a policy entry.  On Linux with the `enroll' program on
;; PATH (mig5's harvester), it can also turn the machine's installed
;; state into an Ansible project and hand that to
;; `monk-translate-import', so a policy to start from lands in
;; `*monk-import*'.  macOS has no harvester; facts only.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'monk)
(require 'monk-inventory)
(require 'monk-ui)
(require 'monk-event)

(declare-function monk-translate-import "monk-translate" (dialect source machine))
(declare-function monk-agent-launchd-units "monk-agent" (&rest args))
(declare-function monk-agent-units "monk-agent" (&rest args))

(defgroup monk-enroll nil
  "Inventory editing and enrollment."
  :group 'monk)

(defconst monk-inventory-buffer-name "*monk-inventory*"
  "The inventory editor buffer.")

(defcustom monk-enroll-group-rules
  '((darwin . macs) (raspbian . pis) (debian . unix) (ubuntu . unix) (linux . unix))
  "Which group a new machine is proposed for, by the first class that matches.
Each entry is (CLASS . GROUP).  A GROUP not in the tree is skipped;
with no match the deepest group that holds hosts is proposed."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom monk-enroll-harvester "enroll"
  "The program `monk-enroll' runs on Linux to harvest installed state.
mig5's enroll: `enroll harvest' then `enroll manifest' give an Ansible
project that `monk-translate-import' reads.  Nil never harvests."
  :type '(choice (const nil) string))

(defvar monk-inventory-var-history nil
  "Minibuffer history for variable names in the editor.")

;;;; Faces

(defface monk-inventory-group
  '((t :inherit monk-ui-fold :weight bold))
  "Group heading in the inventory editor.")

(defface monk-inventory-pill
  '((t :inherit monk-ui-class))
  "A property pill on a host line.")

(defface monk-inventory-var
  '((t :inherit monk-ui-key))
  "A variable pill on a host or group line.")

;;;; The buffer

(defvar monk-inventory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'monk-ui-toggle-fold)
    (define-key map (kbd "<tab>") #'monk-ui-toggle-fold)
    (define-key map (kbd "n") #'monk-inventory-next-host)
    (define-key map (kbd "p") #'monk-inventory-previous-host)
    (define-key map (kbd "a") #'monk-inventory-add-host)
    (define-key map (kbd "G") #'monk-inventory-add-group)
    (define-key map (kbd "r") #'monk-inventory-rename)
    (define-key map (kbd "m") #'monk-inventory-move-host)
    (define-key map (kbd "v") #'monk-inventory-set-var)
    (define-key map (kbd "V") #'monk-inventory-unset-var)
    (define-key map (kbd "e") #'monk-inventory-set-property)
    (define-key map (kbd "d") #'monk-inventory-delete)
    (define-key map (kbd "f") #'monk-inventory-gather-facts)
    (define-key map (kbd "i") #'monk-inventory-inspect)
    (define-key map (kbd ".") #'monk-inventory-find-definition)
    (define-key map (kbd "s") #'monk-inventory-save-command)
    (define-key map (kbd "g") #'monk-inventory-refresh)
    (define-key map (kbd "E") #'monk-enroll)
    (define-key map (kbd "A") #'monk-enroll-accept)
    (define-key map (kbd "?") #'monk-inventory-help)
    (define-key map (kbd "q") #'monk-ui-quit)
    map)
  "Keymap for `monk-inventory-mode'.")

(define-derived-mode monk-inventory-mode special-mode "Monk-Inventory"
  "Edit the inventory tree: groups fold, hosts are lines.
\\{monk-inventory-mode-map}"
  (setq buffer-read-only t truncate-lines t)
  (setq-local line-move-ignore-invisible t)
  (buffer-disable-undo)
  (setq-local header-line-format '(:eval (monk-inventory--header-line))))

(defvar-local monk-inventory--dirty nil
  "Non-nil when the tree in memory differs from the file.")

(defun monk-inventory--header-line ()
  "Return the editor's header: file, counts, dirty mark."
  (concat
   (propertize " INVENTORY " 'face 'monk-ui-header)
   (propertize (format " %s " (if monk-inventory-tree-file
                                  (abbreviate-file-name monk-inventory-tree-file)
                                "(not saved yet)"))
               'face 'monk-ui-key)
   (propertize (format " %d host%s in %d group%s"
                       (length monk-inventory) (if (= 1 (length monk-inventory)) "" "s")
                       (length (monk-inventory-group-names)) (if (= 1 (length (monk-inventory-group-names))) "" "s"))
               'face 'monk-ui-header)
   (if monk-inventory--dirty
       (propertize "  unsaved: s writes the file" 'face 'monk-ui-partial)
     "")
   (propertize (substitute-command-keys "  \\[monk-inventory-help] keys") 'face 'monk-ui-header)))

(defun monk-inventory-help ()
  "Show the editor's keys."
  (interactive)
  (message "%s" (substitute-command-keys
                 "\\[monk-inventory-add-host] add host  \\[monk-inventory-add-group] add group  \\[monk-inventory-rename] rename  \\[monk-inventory-move-host] move host  \\[monk-inventory-set-var] set var  \\[monk-inventory-unset-var] unset var  \\[monk-inventory-set-property] address/user/port/method  \\[monk-inventory-delete] delete  \\[monk-inventory-gather-facts] facts  \\[monk-inventory-inspect] inspect  \\[monk-inventory-find-definition] file  \\[monk-enroll] enroll this machine  \\[monk-enroll-accept] accept (lift probation)  \\[monk-inventory-save-command] save  \\[monk-ui-toggle-fold] fold  \\[monk-ui-quit] quit")))

;;;###autoload
(defun monk-inventory-edit ()
  "Open the inventory editor on the current inventory.
With no inventory loaded, read `monk-inventory-path' first; with no
file either, start an empty tree named `world'."
  (interactive)
  (unless monk-inventory-tree
    (or (monk-inventory-load)
        (monk-inventory-define '(world) (monk-inventory-file))))
  (let ((buffer (get-buffer-create monk-inventory-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-inventory-mode)
        (monk-inventory-mode))
      (monk-inventory-refresh))
    (pop-to-buffer buffer)))

(defun monk-inventory-refresh ()
  "Redraw the editor from `monk-inventory-tree', keeping folds and point."
  (interactive)
  (when (derived-mode-p 'monk-inventory-mode)
    (let ((folds (monk-ui--fold-states))
          (at (monk-inventory--thing-at-point))
          (line (count-lines (point-min) (line-beginning-position)))
          (inhibit-read-only t))
      (erase-buffer)
      (when monk-inventory-tree
        (monk-inventory--insert-group-fold monk-inventory-tree folds 0))
      (goto-char (point-min))
      (let ((pos (and at (monk-inventory--position-of at))))
        (if pos (goto-char pos) (forward-line line))))))

(defun monk-inventory--insert-group-fold (group folds depth)
  "Insert GROUP as a fold at DEPTH, restoring states from FOLDS."
  (pcase-let* ((`(,name . ,plist) group)
               (id (intern (format "group-%s" name)))
               (indent (make-string (* 2 depth) ?\s))
               (hosts (plist-get plist :hosts))
               (children (plist-get plist :children)))
    (insert indent)
    (let ((start (point)))
      (monk-ui-insert-fold
       id
       (concat (propertize (symbol-name name) 'face 'monk-inventory-group)
               (propertize (format " (%d host%s%s)"
                                   (length (monk-inventory--hosts-under group))
                                   (if (= 1 (length (monk-inventory--hosts-under group))) "" "s")
                                   (if children (format ", %d group%s" (length children) (if (= 1 (length children)) "" "s")) ""))
                           'face 'monk-ui-skipped)
               (monk-inventory--vars-pills (plist-get plist :vars)))
       (lambda ()
         (dolist (host hosts)
           (monk-inventory--insert-host-line host (1+ depth)))
         (dolist (child children)
           (monk-inventory--insert-group-fold child folds (1+ depth))))
       (if (assq id folds) (cdr (assq id folds)) t)
       t)
      (put-text-property start (min (point-max) (1+ start)) 'monk-inventory-group name)
      (save-excursion
        (goto-char start)
        (put-text-property (line-beginning-position) (line-end-position) 'monk-inventory-group name)))))

(defun monk-inventory--hosts-under (group)
  "Return every host entry under GROUP, including in child groups."
  (append (plist-get (cdr group) :hosts)
          (apply #'append (mapcar #'monk-inventory--hosts-under (plist-get (cdr group) :children)))))

(defun monk-inventory--vars-pills (vars)
  "Return VARS, an alist, as pills; t values as the bare name."
  (mapconcat (lambda (cell)
               (concat " " (propertize (if (eq (cdr cell) t)
                                           (format " %s " (car cell))
                                         (format " %s=%s " (car cell) (monk-inventory--short (cdr cell))))
                                       'face 'monk-inventory-var)))
             vars ""))

(defun monk-inventory--short (value)
  "Return VALUE printed, cut for a pill."
  (truncate-string-to-width (prin1-to-string value) 24 nil nil "…"))

(defun monk-inventory--insert-host-line (host depth)
  "Insert the line for HOST, (NAME . PLIST), at DEPTH."
  (pcase-let* ((`(,name . ,plist) host)
               (m (monk-inventory-find name))
               (start (point)))
    (insert (make-string (* 2 depth) ?\s))
    (insert (propertize (symbol-name name) 'face 'monk-ui-machine))
    (when (plist-get plist :address)
      (insert " " (propertize (format " %s%s%s "
                                      (if (plist-get plist :user) (concat (plist-get plist :user) "@") "")
                                      (plist-get plist :address)
                                      (if (plist-get plist :port) (format ":%s" (plist-get plist :port)) ""))
                              'face 'monk-inventory-pill)))
    (when (plist-get plist :method)
      (insert " " (propertize (format " %s " (plist-get plist :method)) 'face 'monk-inventory-pill)))
    (unless (plist-get plist :address)
      (insert " " (propertize " local " 'face 'monk-inventory-pill)))
    (insert (monk-inventory--vars-pills (plist-get plist :vars)))
    (when (and m (oref m classes))
      (insert "  " (propertize (mapconcat #'symbol-name (seq-take (oref m classes) 6) " ") 'face 'monk-ui-skipped)
              (if (> (length (oref m classes)) 6) (propertize " …" 'face 'monk-ui-skipped) "")))
    (when (cdr (assq 'probation (plist-get plist :vars)))
      (insert "  " (propertize " ON PROBATION: A accepts " 'face 'monk-ui-partial)))
    (when (and m (not (oref m facts)))
      (insert "  " (propertize "no facts yet (f)" 'face 'monk-ui-skipped)))
    (insert "\n")
    (add-text-properties start (point) (list 'monk-inventory-host name 'monk-ui-machine m))))

;;;; Point

(defun monk-inventory--thing-at-point ()
  "Return (host . NAME), (group . NAME), or nil for the line at point."
  (let ((bol (line-beginning-position)))
    (cond
     ((get-text-property bol 'monk-inventory-host) (cons 'host (get-text-property bol 'monk-inventory-host)))
     ((get-text-property bol 'monk-inventory-group) (cons 'group (get-text-property bol 'monk-inventory-group)))
     ((get-text-property (point) 'monk-inventory-group) (cons 'group (get-text-property (point) 'monk-inventory-group))))))

(defun monk-inventory--position-of (thing)
  "Return the start of the line that shows THING.
THING is as `monk-inventory--thing-at-point' returns it."
  (let ((prop (if (eq (car thing) 'host) 'monk-inventory-host 'monk-inventory-group))
        (pos (point-min)) found)
    (while (and (not found) pos)
      (when (eq (get-text-property pos prop) (cdr thing)) (setq found pos))
      (setq pos (next-single-property-change pos prop)))
    (and found (save-excursion (goto-char found) (line-beginning-position)))))

(defun monk-inventory--host-at-point ()
  "Return the host symbol at point or signal."
  (pcase (monk-inventory--thing-at-point)
    (`(host . ,name) name)
    (_ (user-error "No host on this line"))))

(defun monk-inventory--group-at-point ()
  "Return the group at point: the group line, or the host's group."
  (pcase (monk-inventory--thing-at-point)
    (`(group . ,name) name)
    (`(host . ,name) (monk-inventory-group-of name))
    (_ (car monk-inventory-tree))))

(defun monk-inventory-next-host (&optional count)
  "Move to the next host line, COUNT times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((pos (next-single-property-change (line-end-position) 'monk-inventory-host)))
      (while (and pos (null (get-text-property pos 'monk-inventory-host)))
        (setq pos (next-single-property-change pos 'monk-inventory-host)))
      (if pos (goto-char pos) (user-error "No next host")))))

(defun monk-inventory-previous-host (&optional count)
  "Move to the previous host line, COUNT times."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((pos (previous-single-property-change (line-beginning-position) 'monk-inventory-host)))
      (while (and pos (> pos (point-min)) (null (get-text-property (1- pos) 'monk-inventory-host)))
        (setq pos (previous-single-property-change pos 'monk-inventory-host)))
      (if (and pos (> pos (point-min)))
          (progn (goto-char (1- pos)) (beginning-of-line))
        (user-error "No previous host")))))

;;;; Edits

(defun monk-inventory--apply (tree)
  "Make TREE the inventory, mark the buffer dirty, redraw.
In a directory inventory a host that lives in a file Monk did not
write (a hand-written `.el' or `.json') cannot be changed here; the edit
is refused with the file named, and the tree stays as it was."
  (if (and monk-inventory-tree-file (file-directory-p monk-inventory-tree-file))
      (condition-case err
          (monk-inventory-update tree t)
        (monk-inventory-error (user-error "%s" (cadr err))))
    (monk-inventory-update tree))
  (with-current-buffer (get-buffer-create monk-inventory-buffer-name)
    (setq monk-inventory--dirty (not (and monk-inventory-tree-file (file-directory-p monk-inventory-tree-file))))
    (monk-inventory-refresh)))

(defun monk-inventory--read-group (prompt &optional default)
  "Read a group name with PROMPT, completing over the tree.
DEFAULT is preselected."
  (intern (completing-read prompt (mapcar #'symbol-name (monk-inventory-group-names)) nil t nil nil
                           (and default (symbol-name default)))))

(defun monk-inventory-add-host (name group &optional address)
  "Add host NAME to GROUP with ADDRESS (empty for the local machine)."
  (interactive
   (let* ((group (monk-inventory--read-group "Add host to group: " (monk-inventory--group-at-point)))
          (name (read-string "Host name: "))
          (address (read-string "Address (empty for this machine): ")))
     (list name group address)))
  (let ((sym (intern (downcase (string-trim name)))))
    (when (monk-inventory-host-entry sym)
      (user-error "%s is already in the inventory under %s" sym (monk-inventory-group-of sym)))
    (monk-inventory--apply
     (apply #'monk-inventory-tree-put-host monk-inventory-tree group sym
            (and address (not (string-empty-p address)) (list :address address))))
    (goto-char (or (monk-inventory--position-of (cons 'host sym)) (point)))
    (message "%s added under %s" sym group)))

(defun monk-inventory-add-group (name parent)
  "Add group NAME under PARENT."
  (interactive
   (list (read-string "New group name: ")
         (monk-inventory--read-group "Under group: " (monk-inventory--group-at-point))))
  (monk-inventory--apply (monk-inventory-tree-add-group monk-inventory-tree parent (intern name)))
  (message "group %s added under %s" name parent))

(defun monk-inventory-rename (new)
  "Rename the host at point to NEW."
  (interactive (list (read-string (format "Rename %s to: " (monk-inventory--host-at-point)))))
  (let ((host (monk-inventory--host-at-point)))
    (monk-inventory--apply (monk-inventory-tree-rename-host monk-inventory-tree host (intern new)))
    (message "%s renamed to %s" host new)))

(defun monk-inventory-move-host (group)
  "Move the host at point to GROUP."
  (interactive (list (monk-inventory--read-group (format "Move %s to group: " (monk-inventory--host-at-point)))))
  (let* ((host (monk-inventory--host-at-point))
         (entry (monk-inventory-host-entry host)))
    (monk-inventory--apply (apply #'monk-inventory-tree-put-host monk-inventory-tree group host (cdr entry)))
    (goto-char (or (monk-inventory--position-of (cons 'host host)) (point)))
    (message "%s moved to %s" host group)))

(defun monk-inventory--known-vars ()
  "Return every variable name used in the tree, as strings."
  (let (out)
    (cl-labels ((walk (g)
                  (dolist (c (plist-get (cdr g) :vars)) (cl-pushnew (symbol-name (car c)) out :test #'equal))
                  (dolist (h (plist-get (cdr g) :hosts))
                    (dolist (c (plist-get (cdr h) :vars)) (cl-pushnew (symbol-name (car c)) out :test #'equal)))
                  (mapc #'walk (plist-get (cdr g) :children))))
      (when monk-inventory-tree (walk monk-inventory-tree)))
    (sort out #'string<)))

(defun monk-inventory-set-var (var value)
  "Set VAR to VALUE on the host or group at point.
VALUE is read as Lisp: t, nil, a string in quotes, a number."
  (interactive
   (let* ((var (completing-read "Variable: " (monk-inventory--known-vars) nil nil nil 'monk-inventory-var-history))
          (value (read-from-minibuffer (format "Value of %s (Lisp, t for a class): " var) "t" nil t)))
     (list (intern var) value)))
  (pcase (monk-inventory--thing-at-point)
    (`(host . ,host)
     (monk-inventory--apply (monk-inventory-tree-set-host-var monk-inventory-tree host var value)))
    (`(group . ,group)
     (monk-inventory--apply (monk-inventory-tree-set-group-var monk-inventory-tree group var value)))
    (_ (user-error "Nothing at point")))
  (message "%s = %S" var value))

(defun monk-inventory-unset-var (var)
  "Remove VAR from the host or group at point."
  (interactive
   (list (intern (completing-read "Unset variable: " (monk-inventory--known-vars) nil t))))
  (pcase (monk-inventory--thing-at-point)
    (`(host . ,host)
     (monk-inventory--apply (monk-inventory-tree-set-host-var monk-inventory-tree host var :unset)))
    (`(group . ,group)
     (monk-inventory--apply (monk-inventory-tree-set-group-var monk-inventory-tree group var :unset)))
    (_ (user-error "Nothing at point")))
  (message "%s unset" var))

(defun monk-inventory-set-property (key value)
  "Set KEY (:address :user :port :method) to VALUE on the host at point.
An empty VALUE removes the key."
  (interactive
   (let* ((host (monk-inventory--host-at-point))
          (entry (monk-inventory-host-entry host))
          (key (intern (concat ":" (completing-read "Property: " '("address" "user" "port" "method") nil t))))
          (current (plist-get (cdr entry) key))
          (value (read-string (format "%s %s (empty to remove): " host key)
                              (and current (format "%s" current)))))
     (list key value)))
  (let* ((host (monk-inventory--host-at-point))
         (value (cond ((string-empty-p value) nil)
                      ((eq key :port) (string-to-number value))
                      (t value))))
    (monk-inventory--apply (monk-inventory-tree-set-host-property monk-inventory-tree host key value))
    (message "%s %s %s" host key (or value "removed"))))

(defun monk-inventory-delete ()
  "Remove the host at point from the inventory, after confirmation."
  (interactive)
  (let ((host (monk-inventory--host-at-point)))
    (when (yes-or-no-p (format "Remove %s from the inventory? " host))
      (monk-inventory--apply (monk-inventory-tree-remove-host monk-inventory-tree host))
      (message "%s removed" host))))

(defun monk-inventory-gather-facts ()
  "Gather facts for the host at point, live, and redraw its classes."
  (interactive)
  (let* ((host (monk-inventory--host-at-point))
         (m (or (monk-inventory-find host) (user-error "No machine %s" host))))
    (message "Gathering facts on %s…" host)
    (monk-gather-facts m)
    (monk-inventory-refresh)
    (message "%s: %d facts, classes %s" host (length (oref m facts))
             (mapconcat #'symbol-name (oref m classes) " "))))

(defun monk-inventory-inspect ()
  "Show what guards can see on the host at point."
  (interactive)
  (let ((host (monk-inventory--host-at-point)))
    (monk-ui-inspect-machine (symbol-name host))))

(defun monk-inventory-find-definition ()
  "Open the inventory file at the host or group at point."
  (interactive)
  (let* ((thing (monk-inventory--thing-at-point))
         (own (and (eq (car thing) 'host) (cdr (assq (cdr thing) monk-inventory-origins))))
         (file (or own
                   (and monk-inventory-tree-file (not (file-directory-p monk-inventory-tree-file))
                        monk-inventory-tree-file)
                   (and monk-inventory-tree-file (expand-file-name "groups.el" monk-inventory-tree-file))
                   (user-error "The inventory has no file yet; s saves it"))))
    (unless (file-exists-p file)
      (user-error "%s is not written yet; s saves it" (abbreviate-file-name file)))
    (find-file-other-window file)
    (goto-char (point-min))
    (when thing
      (re-search-forward (format "(%s\\_>" (regexp-quote (symbol-name (cdr thing)))) nil t)
      (beginning-of-line))))

(defun monk-inventory-save-command ()
  "Write the inventory file."
  (interactive)
  (let ((file (monk-inventory-save)))
    (setq monk-inventory--dirty nil)
    (force-mode-line-update)
    (message "Inventory saved to %s" (abbreviate-file-name file))))

;;;; Enroll

(defun monk-enroll-propose-group (machine)
  "Return the group a MACHINE with facts belongs in.
By `monk-enroll-group-rules', else the deepest group with hosts."
  (let ((groups (monk-inventory-group-names)))
    (or (seq-some (lambda (rule)
                    (and (monk-class-p machine (car rule))
                         (memq (cdr rule) groups)
                         (cdr rule)))
                  monk-enroll-group-rules)
        ;; the deepest group that lists hosts, else the root
        (let (best)
          (cl-labels ((walk (g depth)
                        (when (and (plist-get (cdr g) :hosts) (or (null best) (> depth (cdr best))))
                          (setq best (cons (car g) depth)))
                        (dolist (c (plist-get (cdr g) :children)) (walk c (1+ depth)))))
            (when monk-inventory-tree (walk monk-inventory-tree 0)))
          (if best (car best) (car monk-inventory-tree))))))

(defun monk-enroll-harvest-available-p ()
  "Return non-nil when this is Linux and the harvester is on PATH."
  (and monk-enroll-harvester
       (eq system-type 'gnu/linux)
       (executable-find monk-enroll-harvester)))

(defun monk-enroll-harvest (machine)
  "Run the harvester for MACHINE and return the generated Ansible playbook text.
Two steps under a temporary directory: harvest, then manifest; the
result is `playbook.yml' with its roles beside it.  Signal
`monk-exec-error' when a step fails."
  (let* ((dir (make-temp-file "monk-enroll-" t))
         (harvest (expand-file-name "harvest" dir))
         (out (expand-file-name "ansible" dir)))
    (monk-exec-string machine monk-enroll-harvester "harvest" "--out" harvest)
    (monk-exec-string machine monk-enroll-harvester "manifest" "--harvest" harvest "--out" out)
    (let ((playbook (expand-file-name "playbook.yml" out)))
      (unless (file-exists-p playbook)
        (signal 'monk-exec-error (list (oref machine name) (list monk-enroll-harvester "manifest") 0
                                       (format "no playbook.yml under %s" out))))
      (with-temp-buffer
        (insert-file-contents playbook)
        ;; the roles are files next to it; the model gets the playbook
        ;; and the task files inlined after a marker per role
        (dolist (tasks (directory-files-recursively out "\\`main\\.yml\\'"))
          (when (string-match-p "/roles/" tasks)
            (goto-char (point-max))
            (insert (format "\n# --- %s ---\n" (file-relative-name tasks out)))
            (insert-file-contents tasks)
            (goto-char (point-max))))
        (buffer-string)))))

;;;###autoload
(defun monk-enroll (&optional machine)
  "Add MACHINE, default this one, to the inventory and save the file.
Gather its facts, propose a group from them (editable), write the
entry with the address when the machine is remote, save, and offer
the next steps: install the agent, and on Linux with `enroll' on
PATH, harvest the installed state into a starting policy in
`*monk-import*'.  Interactively with a prefix argument, ask for a
remote host name to enroll from here."
  (interactive
   (list (and current-prefix-arg
              (let ((host (read-string "Host to enroll (user@host or host): ")))
                (monk-machine :name (car (last (split-string host "@")))
                              :address (car (last (split-string host "@")))
                              :user (and (string-match-p "@" host) (car (split-string host "@"))))))))
  (unless monk-inventory-tree
    (or (monk-inventory-load)
        (monk-inventory-define '(world) (monk-inventory-file))))
  (let* ((m (or machine (monk-local-machine)))
         (name (intern (downcase (car (split-string (oref m name) "\\.")))))
         (existing (monk-inventory-host-entry name)))
    (when existing
      (unless (yes-or-no-p (format "%s is already in the inventory under %s; update it? "
                                   name (monk-inventory-group-of name)))
        (user-error "Nothing changed")))
    (message "Gathering facts on %s…" (oref m name))
    (monk-gather-facts m)
    (let* ((proposed (monk-enroll-propose-group m))
           (group (monk-inventory--read-group
                   (format "Group for %s (classes: %s): " name (mapconcat #'symbol-name (seq-take (oref m classes) 5) " "))
                   proposed))
           (plist (append (and (oref m address) (list :address (oref m address)))
                          (and (oref m user) (list :user (oref m user)))
                          (and (oref m method) (list :method (oref m method)))
                          (and existing (plist-member (cdr existing) :vars)
                               (list :vars (plist-get (cdr existing) :vars))))))
      (monk-inventory-update (apply #'monk-inventory-tree-put-host monk-inventory-tree group name plist) t)
      ;; the machine in the inventory gets the facts just gathered
      (let ((in (monk-inventory-find name)))
        (when in
          (oset in facts (oref m facts))
          (oset in gathered (oref m gathered))
          (oset in facts-source 'live)
          (oset in classes (monk-hard-classes in))
          (monk-inventory-apply-derived (list in))))
      (message "%s enrolled under %s; inventory saved to %s" name group
               (abbreviate-file-name (monk-inventory-file)))
      (when (get-buffer monk-inventory-buffer-name)
        (with-current-buffer monk-inventory-buffer-name (monk-inventory-refresh)))
      (monk-enroll--offer-next-steps (or (monk-inventory-find name) m)))))

(defun monk-enroll--offer-next-steps (machine)
  "Ask what to do after enrolling MACHINE: agent, harvest, nothing."
  (let* ((harvest-p (and (monk-machine-local-p machine) (monk-enroll-harvest-available-p)))
         (choices (append '(("open the inventory editor" . edit))
                          (and (monk-machine-local-p machine)
                               '(("plan the agent install for this machine" . agent)))
                          (and harvest-p
                               '(("harvest installed state into a starting policy (enroll)" . harvest)))
                          '(("nothing more" . done))))
         (answer (cdr (assoc (completing-read "Next: " choices nil t) choices))))
    (pcase answer
      ('edit (monk-inventory-edit))
      ('agent (monk-enroll-plan-agent machine))
      ('harvest (monk-enroll-import-harvest machine))
      (_ nil))))

(defconst monk-enroll-probation-var 'probation
  "The variable an agent sets on a host it enrolled by itself.
See `monk-agent-unknown-host'.")

;;;###autoload
(defun monk-enroll-accept (&optional host)
  "Accept HOST, default the one at point or this machine: lift its probation.
Removes the `probation' variable, saves the inventory, and when an
agent for that host answers on this machine's socket, tells it, so
its next run is real; an agent elsewhere notices at its next converge
through the file."
  (interactive)
  (let* ((host (or host
                   (and (derived-mode-p 'monk-inventory-mode)
                        (pcase (monk-inventory--thing-at-point) (`(host . ,h) h)))
                   (intern (downcase (car (split-string (system-name) "\\."))))))
         (entry (or (monk-inventory-host-entry host)
                    (user-error "%s is not in the inventory" host))))
    (unless (cdr (assq monk-enroll-probation-var (plist-get (cdr entry) :vars)))
      (user-error "%s is not on probation" host))
    (monk-inventory-update
     (monk-inventory-tree-set-host-var monk-inventory-tree host monk-enroll-probation-var :unset)
     t)
    (when (get-buffer monk-inventory-buffer-name)
      (with-current-buffer monk-inventory-buffer-name
        (setq monk-inventory--dirty nil)
        (monk-inventory-refresh)))
    ;; a local agent on probation for this very host: tell it now
    (when (and (fboundp 'monk-agent-read-pid-file)
               (equal (symbol-name host) (downcase (car (split-string (system-name) "\\."))))
               (monk-agent-read-pid-file))
      (condition-case err
          (let ((reply (monk-enroll--ask-agent '((op . "accept")))))
            (message "%s accepted; the agent answered %s" host (or (alist-get 'note reply) "and runs for real now")))
        (error (message "%s accepted in the inventory; the agent could not be told (%s), it notices at its next converge"
                        host (error-message-string err)))))
    (unless (and (fboundp 'monk-agent-read-pid-file) (monk-agent-read-pid-file))
      (message "%s accepted; saved to %s" host (abbreviate-file-name (monk-inventory-file))))))

(declare-function monk-agent-read-pid-file "monk-agent")
(declare-function monk-agent-local-socket-path "monk-agent")

(defun monk-enroll--ask-agent (request)
  "Send REQUEST to the local agent's socket and return its first reply alist."
  (require 'monk-agent)
  (let* ((socket (monk-agent-local-socket-path))
         (reply nil)
         (proc (make-network-process :name "monk-enroll-accept" :family 'local :service socket
                                     :noquery t :coding 'utf-8-unix
                                     :filter (lambda (_p chunk)
                                               (unless reply
                                                 (setq reply (monk-event-parse (car (split-string chunk "\n" t))))))
                                     :sentinel #'ignore)))
    (unwind-protect
        (progn
          (process-send-string proc (monk-event-line request))
          (let ((deadline (+ (float-time) 5)))
            (while (and (null reply) (< (float-time) deadline) (process-live-p proc))
              (accept-process-output proc 0.1)))
          (or reply (error "no answer within 5 s")))
      (ignore-errors (delete-process proc)))))

(defun monk-enroll-plan-agent (machine)
  "Plan the agent install for MACHINE: launchd on macOS, else systemd.
The policy file and batch entry are asked; `a' in `*monk*' installs."
  (interactive (list (monk-local-machine)))
  (require 'monk-agent)
  (let* ((policy-file (read-file-name "Policy file the agent loads: " nil nil t))
         (function (read-string "Batch entry function (empty for monk-agent-main): "))
         (darwin (monk-class-p machine 'darwin))
         (policy (lambda (_m)
                   (if darwin
                       (monk-agent-launchd-units :policy-file policy-file
                                                 :function (and (not (string-empty-p function)) function))
                     (monk-agent-units :user t)))))
    (monk-run-report (monk-plan machine policy))))

(defun monk-enroll-import-harvest (machine)
  "Harvest MACHINE's installed state with `enroll' and import it as a policy."
  (interactive (list (monk-local-machine)))
  (require 'monk-translate)
  (unless (monk-enroll-harvest-available-p)
    (user-error "The harvester (%s) is not on PATH, or this is not Linux" monk-enroll-harvester))
  (message "Harvesting %s with %s… this takes a minute" (oref machine name) monk-enroll-harvester)
  (let ((text (monk-enroll-harvest machine)))
    (monk-translate-import 'ansible text machine)))

(monk-note-load 'monk-enroll)
(provide 'monk-enroll)
;;; monk-enroll.el ends here
