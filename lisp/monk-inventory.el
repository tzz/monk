;;; monk-inventory.el --- Inventory for Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; An inventory is a tree of groups.  Each group has variables and
;; either child groups or hosts.  Variables merge from the root down,
;; so a host value wins over its group, and a child group wins over
;; its parent.  This matches the Ansible group tree, as Lisp data.
;;
;;   (monk-inventory-define
;;    '(world
;;      :vars ((observer . nil))
;;      :children
;;      ((home
;;        :vars ((mine . t))
;;        :children
;;        ((macs
;;          :hosts
;;          ((foxy :vars ((home_leader . t)))
;;           (moi :address "moi.local")))
;;         (unix
;;          :hosts
;;          ((kamen :address "kamen.local" :user "pi"))))))))
;;
;; A host with no :address is the local machine.  :method names the
;; TRAMP method that reaches a remote host ("ssh" unless given; "rpc"
;; for tramp-rpc).  Any variable whose
;; value is t becomes a class on the machine.
;;
;; Select machines with `monk-inventory-select', which accepts a host
;; name, a group name, a list of either, or the symbol `self' for the
;; host whose name matches the local host name.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'monk)
(require 'monk-cell)

(defvar monk-inventory nil
  "The current inventory: a list of `monk-machine' objects.")

(defvar monk-inventory-tree nil
  "The group tree the current inventory was built from, as given to
`monk-inventory-define'.  The editor and `monk-enroll' change this and
rebuild the machines from it; `monk-inventory-save' writes it.")

(defvar monk-inventory-tree-file nil
  "The file `monk-inventory-tree' was read from, or nil when it came from code.")

(defcustom monk-inventory-file nil
  "Where the editable inventory lives.  Nil means MONK_HOME/inventory.el.
`monk-inventory-load' reads it, `monk-inventory-save' writes it, and
`monk-enroll' on a new machine appends to it.  A policy file that
defines its inventory inline keeps working; `monk-inventory-migrate'
writes that inline tree here once, after which the policy file can
call `monk-inventory-load' instead."
  :type '(choice (const nil) file)
  :group 'monk)

(defconst monk-inventory-file-name "inventory.el"
  "File name under the Monk state directory for the inventory tree.")

(defun monk-inventory-file ()
  "Return the inventory file path: `monk-inventory-file' or the default."
  (expand-file-name (or monk-inventory-file
                        (expand-file-name monk-inventory-file-name (monk-home-directory)))))

(defconst monk-inventory-self 'self
  "Selector for the machine whose name is the local host name.")

(defun monk--inventory-merge-vars (parent child)
  "Return PARENT alist updated by CHILD alist."
  (let ((merged (copy-alist parent)))
    (dolist (cell child)
      (setf (alist-get (car cell) merged) (cdr cell)))
    merged))

(defun monk--inventory-walk (group vars groups)
  "Return machines under GROUP with inherited VARS and GROUPS."
  (pcase-let ((`(,name . ,plist) group))
    (unless (symbolp name)
      (signal 'monk-inventory-error (list "Group name must be a symbol" name)))
    (let ((vars (monk--inventory-merge-vars vars (plist-get plist :vars)))
          (groups (cons name groups))
          (result nil))
      (dolist (child (plist-get plist :children))
        (setq result (append result (monk--inventory-walk child vars groups))))
      (dolist (host (plist-get plist :hosts))
        (pcase-let ((`(,host-name . ,host-plist) host))
          (unless (symbolp host-name)
            (signal 'monk-inventory-error (list "Host name must be a symbol" host-name)))
          (let ((all-vars (monk--inventory-merge-vars vars (plist-get host-plist :vars))))
            (push (monk-machine
                   :name (symbol-name host-name)
                   :address (plist-get host-plist :address)
                   :user (plist-get host-plist :user)
                   :port (plist-get host-plist :port)
                   :method (plist-get host-plist :method)
                   :vars all-vars
                   :groups groups
                   :ignore-unreachable (and (cdr (assq 'ignore_unreachable all-vars)) t))
                  result))))
      result)))

(defun monk-inventory-define (tree &optional file)
  "Set `monk-inventory' from TREE.  Return the machine list.
FILE is where TREE came from, default the file being loaded; each
machine remembers it as its origin.  Signal `monk-inventory-error' on
duplicate host names."
  (let* ((machines (monk--inventory-walk tree nil nil))
         (names (mapcar (lambda (m) (oref m name)) machines))
         (file (or file load-file-name (buffer-file-name))))
    (when (/= (length names) (length (delete-dups (copy-sequence names))))
      (signal 'monk-inventory-error (list "Duplicate host names" names)))
    (dolist (m machines)
      (oset m origin (cons file (oref m name))))
    (setq monk-inventory-tree tree
          monk-inventory-tree-file file
          monk-inventory (nreverse machines))))

;;;; The inventory file
;;
;; The tree as data in a file of its own, so a machine can be added
;; from a new host or from the editor without touching a policy file
;; in a checkout.  The file holds one readable form, the tree; a
;; header comment says who wrote it.

(defun monk-inventory-load (&optional file)
  "Read the inventory tree from FILE and define it.
FILE defaults to `monk-inventory-file'.
Return the machine list, or nil with a message when the file does
not exist yet.  Derived group rules are kept; they are code, not data."
  (let ((file (expand-file-name (or file (monk-inventory-file)))))
    (if (not (file-readable-p file))
        (progn (monk-log 'info "inventory file %s does not exist yet" (abbreviate-file-name file)) nil)
      (let ((tree (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (read (current-buffer)))))
        (prog1 (monk-inventory-define tree file)
          (monk-inventory-apply-derived))))))

(defun monk-inventory-save (&optional file)
  "Write `monk-inventory-tree' to FILE, default `monk-inventory-file'.
Return the file.  The form is pretty-printed, one host per line."
  (let ((file (expand-file-name (or file (monk-inventory-file)))))
    (unless monk-inventory-tree
      (signal 'monk-inventory-error (list "No inventory tree to save")))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";;; Monk inventory -*- mode: lisp-data; -*-\n"
              ";; One form: the group tree.  Edited by monk-inventory-mode and monk-enroll;\n"
              ";; hand edits are fine, the file is read back as data.\n"
              (format ";; Written %s\n\n" (format-time-string "%F %T")))
      (let ((print-length nil) (print-level nil))
        (insert (monk-inventory--pp monk-inventory-tree) "\n")))
    (setq monk-inventory-tree-file file)
    (dolist (m monk-inventory) (oset m origin (cons file (oref m name))))
    file))

(defun monk-inventory--pp (tree)
  "Return TREE printed with one host entry per line and groups indented."
  (with-temp-buffer
    (monk-inventory--insert-group tree)
    (buffer-string)))

(defun monk-inventory--insert-group (group)
  "Insert GROUP at point; continuation lines align under the group's open paren."
  (pcase-let* ((`(,name . ,plist) group)
               (column (current-column))
               (pad (make-string (1+ column) ?\s)))
    (insert "(" (symbol-name name))
    (when (plist-member plist :vars)
      (insert "\n" pad ":vars " (prin1-to-string (plist-get plist :vars))))
    (when (plist-get plist :children)
      (insert "\n" pad ":children\n" pad "(")
      (let ((first t))
        (dolist (child (plist-get plist :children))
          (unless first (insert "\n" pad " "))
          (monk-inventory--insert-group child)
          (setq first nil)))
      (insert ")"))
    (when (plist-get plist :hosts)
      (insert "\n" pad ":hosts\n" pad "(")
      (let ((first t))
        (dolist (host (plist-get plist :hosts))
          (unless first (insert "\n" pad " "))
          (insert (prin1-to-string host))
          (setq first nil)))
      (insert ")"))
    (insert ")")))

(defun monk-inventory-migrate (&optional file)
  "Write the inventory defined in code to FILE, once.
FILE defaults to `monk-inventory-file'.
Refuse when the file exists: the file is then the source, not the code."
  (interactive)
  (let ((file (expand-file-name (or file (monk-inventory-file)))))
    (when (file-exists-p file)
      (user-error "%s exists; the inventory lives there now" (abbreviate-file-name file)))
    (monk-inventory-save file)
    (message "Inventory written to %s; load it with (monk-inventory-load) instead of the inline define"
             (abbreviate-file-name file))))

;;;; Editing the tree
;;
;; Pure functions over the tree, each returning a new tree; the
;; editor and `monk-enroll' call `monk-inventory-update' with one.

(defun monk-inventory-group-names (&optional tree)
  "Return every group name in TREE, default the current one, root first."
  (let ((tree (or tree monk-inventory-tree)) out)
    (cl-labels ((walk (g) (push (car g) out) (mapc #'walk (plist-get (cdr g) :children))))
      (when tree (walk tree)))
    (nreverse out)))

(defun monk-inventory-group-of (host &optional tree)
  "Return the name of the group that lists HOST, a symbol, in TREE, or nil."
  (let ((tree (or tree monk-inventory-tree)) found)
    (cl-labels ((walk (g)
                  (when (assq host (plist-get (cdr g) :hosts)) (setq found (car g)))
                  (mapc #'walk (plist-get (cdr g) :children))))
      (when tree (walk tree)))
    found))

(defun monk-inventory-host-entry (host &optional tree)
  "Return the (HOST . PLIST) entry for HOST, a symbol, in TREE, or nil."
  (let ((tree (or tree monk-inventory-tree)) found)
    (cl-labels ((walk (g)
                  (let ((e (assq host (plist-get (cdr g) :hosts)))) (when e (setq found e)))
                  (mapc #'walk (plist-get (cdr g) :children))))
      (when tree (walk tree)))
    found))

(defun monk-inventory--map-groups (tree fn)
  "Return TREE with FN applied to every group, children first."
  (pcase-let ((`(,name . ,plist) tree))
    (let* ((children (mapcar (lambda (c) (monk-inventory--map-groups c fn))
                             (plist-get plist :children)))
           (plist (copy-sequence plist)))
      (when (plist-member plist :children)
        (setq plist (plist-put plist :children children)))
      (funcall fn (cons name plist)))))

(defun monk-inventory-tree-put-host (tree group host &rest plist)
  "Return TREE with HOST (a symbol) under GROUP, its entry set to PLIST.
A host already elsewhere in the tree is moved.  Signal when GROUP does
not exist."
  (unless (memq group (monk-inventory-group-names tree))
    (signal 'monk-inventory-error (list "No such group" group)))
  (let ((tree (monk-inventory-tree-remove-host tree host)))
    (monk-inventory--map-groups
     tree
     (lambda (g)
       (if (not (eq (car g) group))
           g
         (cons (car g) (plist-put (copy-sequence (cdr g)) :hosts
                                  (append (plist-get (cdr g) :hosts) (list (cons host plist))))))))))

(defun monk-inventory-tree-remove-host (tree host)
  "Return TREE without HOST."
  (monk-inventory--map-groups
   tree
   (lambda (g)
     (if (not (assq host (plist-get (cdr g) :hosts)))
         g
       (let ((plist (copy-sequence (cdr g))))
         (setq plist (plist-put plist :hosts (assq-delete-all host (copy-sequence (plist-get plist :hosts)))))
         (when (null (plist-get plist :hosts))
           (setq plist (monk-inventory--plist-remove plist :hosts)))
         (cons (car g) plist))))))

(defun monk-inventory-tree-set-host-var (tree host var value)
  "Return TREE with VAR set to VALUE on HOST; VALUE `:unset' removes it."
  (let ((entry (or (monk-inventory-host-entry host tree)
                   (signal 'monk-inventory-error (list "No such host" host)))))
    (let* ((plist (copy-sequence (cdr entry)))
           (vars (copy-alist (plist-get plist :vars))))
      (setq vars (monk-inventory--vars-put vars var value))
      (setq plist (if vars (plist-put plist :vars vars) (monk-inventory--plist-remove plist :vars)))
      (apply #'monk-inventory-tree-put-host tree (monk-inventory-group-of host tree) host plist))))

(defun monk-inventory-tree-set-host-property (tree host key value)
  "Return TREE with KEY set to VALUE on HOST.
KEY is one of :address :user :port :method.
Nil VALUE removes the key.  Order is kept: the host stays in its group."
  (let* ((entry (or (monk-inventory-host-entry host tree)
                    (signal 'monk-inventory-error (list "No such host" host))))
         (plist (copy-sequence (cdr entry))))
    (setq plist (if value (plist-put plist key value) (monk-inventory--plist-remove plist key)))
    (monk-inventory--map-groups
     tree
     (lambda (g)
       (if (not (assq host (plist-get (cdr g) :hosts)))
           g
         (cons (car g) (plist-put (copy-sequence (cdr g)) :hosts
                                  (mapcar (lambda (h) (if (eq (car h) host) (cons host plist) h))
                                          (plist-get (cdr g) :hosts)))))))))

(defun monk-inventory-tree-add-group (tree parent group &optional vars)
  "Return TREE with GROUP, a new symbol, under PARENT, with VARS."
  (when (memq group (monk-inventory-group-names tree))
    (signal 'monk-inventory-error (list "Group exists" group)))
  (monk-inventory--map-groups
   tree
   (lambda (g)
     (if (not (eq (car g) parent))
         g
       (cons (car g) (plist-put (copy-sequence (cdr g)) :children
                                (append (plist-get (cdr g) :children)
                                        (list (if vars (list group :vars vars) (list group))))))))))

(defun monk-inventory-tree-set-group-var (tree group var value)
  "Return TREE with VAR set to VALUE on GROUP; VALUE `:unset' removes it."
  (monk-inventory--map-groups
   tree
   (lambda (g)
     (if (not (eq (car g) group))
         g
       (let* ((plist (copy-sequence (cdr g)))
              (vars (copy-alist (plist-get plist :vars))))
         (setq vars (monk-inventory--vars-put vars var value))
         (cons (car g) (if vars (plist-put plist :vars vars) (monk-inventory--plist-remove plist :vars))))))))

(defun monk-inventory--vars-put (vars var value)
  "Return VARS with VAR set to VALUE in place, or appended; `:unset' removes it.
The order a person wrote stays, so the file diff is one line."
  (cond
   ((eq value :unset) (assq-delete-all var (copy-alist vars)))
   ((assq var vars) (mapcar (lambda (c) (if (eq (car c) var) (cons var value) c)) vars))
   (t (append vars (list (cons var value))))))

(defun monk-inventory-tree-rename-host (tree host new)
  "Return TREE with HOST renamed to NEW, in place."
  (monk-inventory--map-groups
   tree
   (lambda (g)
     (if (not (assq host (plist-get (cdr g) :hosts)))
         g
       (cons (car g) (plist-put (copy-sequence (cdr g)) :hosts
                                (mapcar (lambda (h) (if (eq (car h) host) (cons new (cdr h)) h))
                                        (plist-get (cdr g) :hosts))))))))

(defun monk-inventory--plist-remove (plist key)
  "Return PLIST without KEY."
  (let (out)
    (while plist
      (let ((k (pop plist)) (v (pop plist)))
        (unless (eq k key) (setq out (append out (list k v))))))
    out))

(defun monk-inventory-update (tree &optional save)
  "Make TREE the inventory: rebuild machines, keep facts, apply derived groups.
With SAVE, write the file too.  Return the machine list."
  (let ((old monk-inventory))
    (monk-inventory-define tree (or monk-inventory-tree-file (monk-inventory-file)))
    ;; facts and classes already gathered carry over by name
    (dolist (m monk-inventory)
      (let ((was (seq-find (lambda (o) (equal (oref o name) (oref m name))) old)))
        (when (and was (oref was facts))
          (oset m facts (oref was facts))
          (oset m gathered (oref was gathered))
          (oset m facts-source (oref was facts-source))
          (oset m classes (monk-hard-classes m)))))
    (monk-inventory-apply-derived)
    (when save (monk-inventory-save))
    monk-inventory))

;;;; Derived groups and tags
;;
;; A derived group is a group whose members are decided by a guard
;; over facts, not listed by hand: Salt's derived grains, Ansible's
;; `constructed' plugin.  A keyed group makes one group per value of
;; a fact: `distro_ubuntu', `distro_raspbian'.  Both apply when the
;; machine's facts arrive, live or from the cache, so a plan for a
;; machine that is off still puts it in the right groups.  Groups
;; become classes, as inventory groups do.

(defvar monk-inventory-derived nil
  "List of derived group rules: (GROUP . GUARD) or (:keyed GUARD PREFIX SEPARATOR).")

(defun monk-group (name guard)
  "Declare derived group NAME: every machine for which GUARD is true belongs.
GUARD is a CELL form or a CEL string.  Call after `monk-inventory-define'."
  (if (assq name monk-inventory-derived)
      (setf (alist-get name monk-inventory-derived) (list 'guard (monk-cell-guard guard)))
    (setq monk-inventory-derived
          (append monk-inventory-derived (list (cons name (list 'guard (monk-cell-guard guard)))))))
  (monk-inventory-apply-derived)
  name)

(defun monk-keyed-groups (guard &optional prefix separator)
  "Declare one derived group per value of GUARD, a CELL form or CEL string.
The group name is PREFIX, SEPARATOR, and the value, lowercased and
made a symbol: (monk-keyed-groups \"facts.distribution\" \"distro\")
gives `distro_ubuntu'.  SEPARATOR defaults to `_'.  Ansible's
keyed_groups."
  (setq monk-inventory-derived
        (append monk-inventory-derived (list (list :keyed (monk-cell-guard guard) prefix (or separator "_")))))
  (monk-inventory-apply-derived)
  guard)

(defun monk-inventory-clear-derived ()
  "Forget every derived rule and the groups it produced."
  (dolist (m monk-inventory)
    (oset m groups (seq-remove (lambda (g) (get g 'monk-derived)) (oref m groups))))
  (setq monk-inventory-derived nil))

(defun monk--derived-group-symbol (prefix separator value)
  "Return the group symbol for VALUE under PREFIX with SEPARATOR."
  (let ((name (downcase (replace-regexp-in-string "[^[:alnum:]]+" separator (format "%s" value)))))
    (intern (if prefix (concat prefix separator name) name))))

(defun monk-inventory-apply-derived (&optional machines)
  "Recompute derived groups on MACHINES, default the inventory.
A machine without facts keeps whatever derived groups it had.  Return
the machines whose groups changed."
  (let (changed)
    (dolist (m (or machines monk-inventory))
      (when (oref m facts)
        (let ((before (oref m groups))
              (kept (seq-remove (lambda (g) (get g 'monk-derived)) (oref m groups)))
              (new nil))
          (dolist (rule monk-inventory-derived)
            (condition-case nil
                (pcase rule
                  (`(,group guard ,guard)
                   (when (monk-cell-eval m guard)
                     (put group 'monk-derived t)
                     (push group new)))
                  (`(:keyed ,guard ,prefix ,separator)
                   (let ((value (monk-cell-eval m guard)))
                     (when (and value (not (eq value t)))
                       (let ((group (monk--derived-group-symbol prefix separator value)))
                         (put group 'monk-derived t)
                         (push group new))))))
              (monk-guard-error nil)))
          (oset m groups (append kept (nreverse new)))
          (when (oref m classes)
            (oset m classes (monk-hard-classes m)))
          (unless (equal before (oref m groups))
            (push m changed)))))
    changed))

(defun monk-inventory-find (name)
  "Return the machine named NAME, a string or symbol, or nil.
The match ignores case: macOS reports its host name in upper case
while inventories are written in lower case."
  (let ((name (format "%s" name)))
    (seq-find (lambda (m) (string-equal-ignore-case (oref m name) name)) monk-inventory)))

(defun monk-inventory-select (selector)
  "Return the machines that SELECTOR matches.
SELECTOR is a host name, a group symbol, `self', or a list of those."
  (cond
   ((null monk-inventory)
    (signal 'monk-inventory-error (list "Inventory is empty")))
   ((listp selector)
    (delete-dups (apply #'append (mapcar #'monk-inventory-select selector))))
   ((eq selector monk-inventory-self)
    (let* ((host (car (split-string (system-name) "\\.")))
           (found (monk-inventory-find host)))
      (unless found
        (signal 'monk-inventory-error
                (list (format "Local host %s is not in the inventory; add it, or pass a machine"
                              host))))
      (list found)))
   (t
    (let* ((name (format "%s" selector))
           (symbol (intern name))
           (by-name (monk-inventory-find name))
           (by-group (seq-filter (lambda (m) (memq symbol (oref m groups)))
                                 monk-inventory)))
      (cond
       (by-name (list by-name))
       (by-group by-group)
       (t (signal 'monk-inventory-error
                  (list (format "No host or group named %s" name)))))))))

(defun monk-converge-all (selector policy &rest options)
  "Converge POLICY on every machine that SELECTOR matches.
OPTIONS pass to `monk-converge'.  Return a list of `monk-run'."
  (mapcar (lambda (m)
            (monk-run-report (apply #'monk-converge m policy options)))
          (monk-inventory-select selector)))

(defun monk-inventory--on-facts (m)
  "Apply derived groups to M when it is an inventory machine."
  (when (memq m monk-inventory)
    (monk-inventory-apply-derived (list m))))

(add-hook 'monk-facts-ready-functions #'monk-inventory--on-facts)

(monk-note-load 'monk-inventory)
(provide 'monk-inventory)
;;; monk-inventory.el ends here
