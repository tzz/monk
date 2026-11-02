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
(require 'json)
(require 'monk)
(require 'monk-cell)

(defvar monk-inventory nil
  "The current inventory: a list of `monk-machine' objects.")

(defvar monk-inventory-tree nil
  "The group tree the current inventory was built from, as given to
`monk-inventory-define'.  The editor and `monk-enroll' change this and
rebuild the machines from it; `monk-inventory-save' writes it.")

(defvar monk-inventory-tree-file nil
  "The file `monk-inventory-tree' was read from; nil when it came from code.")

(define-obsolete-variable-alias 'monk-inventory-file 'monk-inventory-path "0.9.2")

(defcustom monk-inventory-path nil
  "Where the inventory lives: a file, or a directory of fragments.
Nil means MONK_HOME/inventory.el.  A file holds one tree form.  A
directory holds any number of `.el' files, each a group form or a
list of group forms, and `.json' files in Ansible inventory shape (a
mapping of group name to hosts, children, vars); every file is read
and the groups are merged by name into one tree, so a directory can
hold one file per group, one per machine, or any mix.  `monk-enroll'
writes a new host as its own `HOST.el' or `HOST.json' in a directory,
and `monk-inventory-save' writes each fragment Monk wrote back with
the hosts it owns.
`monk-inventory-migrate' writes a tree defined in code here once."
  :type '(choice (const nil) file directory)
  :group 'monk)

(defvar monk-inventory-origins nil
  "Alist of (HOST . FILE): which fragment each host was read from.
Nil entries mean the single-file or in-code inventory.  Kept so a
save in a directory writes each host back where it came from and a
new host gets a file of its own.")

(defconst monk-inventory-fragment-extensions '("el" "json")
  "File extensions `monk-inventory-load' reads from a directory, in order.")

(defconst monk-inventory-file-name "inventory.el"
  "File name under the Monk state directory for the inventory tree.")

(defun monk-inventory-file ()
  "Return the inventory path: `monk-inventory-path' or the default file."
  (expand-file-name (or monk-inventory-path
                        (expand-file-name monk-inventory-file-name (monk-home-directory)))))

(defun monk-inventory-directory-p (&optional path)
  "Return non-nil when PATH, default `monk-inventory-file', is a directory."
  (file-directory-p (or path (monk-inventory-file))))

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

(defun monk-inventory-load (&optional path)
  "Read the inventory from PATH and define it.
PATH defaults to `monk-inventory-file': one tree in a file, or a
directory of fragments merged by group name (see `monk-inventory-path').
Return the machine list, or nil with a message when nothing exists
there yet.  Derived group rules are kept; they are code, not data."
  (let ((path (expand-file-name (or path (monk-inventory-file)))))
    (cond
     ((file-directory-p path)
      (let ((fragments (monk-inventory--fragment-files path)))
        (if (null fragments)
            (progn (monk-log 'info "inventory directory %s holds no fragments yet" (abbreviate-file-name path)) nil)
          (pcase-let ((`(,tree . ,origins) (monk-inventory--merge-fragments fragments)))
            (prog1 (monk-inventory-define tree path)
              (setq monk-inventory-origins origins)
              (dolist (m monk-inventory)
                (let ((from (cdr (assq (intern (oref m name)) origins))))
                  (when from (oset m origin (cons from (oref m name))))))
              (monk-inventory-apply-derived))))))
     ((not (file-readable-p path))
      (monk-log 'info "inventory file %s does not exist yet" (abbreviate-file-name path)) nil)
     (t
      (let ((tree (monk-inventory--read-el path)))
        (prog1 (monk-inventory-define (if (and (consp tree) (consp (car tree))) (car tree) tree) path)
          (setq monk-inventory-origins nil)
          (monk-inventory-apply-derived)))))))

;;;; Fragments: a directory of group forms, .el and .json

(defun monk-inventory--fragment-files (directory)
  "Return the fragment files under DIRECTORY, sorted, by name."
  (let (out)
    (dolist (ext monk-inventory-fragment-extensions)
      (setq out (append out (directory-files directory t (format "\\.%s\\'" ext)))))
    (sort (seq-remove (lambda (f) (string-prefix-p "." (file-name-nondirectory f))) out) #'string<)))

(defun monk-inventory--read-el (file)
  "Return the forms in FILE: one tree, or a list of group forms.
A file with one top-level form returns that form; with several, a
list of them.  A single group form is told from a list of them by
its car being a symbol."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (setq forms (nreverse forms))
      (cond
       ((null forms) nil)
       ((and (= 1 (length forms)) (symbolp (car (car forms)))) (car forms))
       ((= 1 (length forms)) (car forms))
       (t forms)))))

(defun monk-inventory--fragment-groups (file)
  "Return the group forms FILE contributes, as a list."
  (pcase (file-name-extension file)
    ("el"
     (let ((forms (monk-inventory--read-el file)))
       (cond ((null forms) nil)
             ((symbolp (car forms)) (list forms))
             (t forms))))
    ("json" (monk-inventory--json-groups file))
    (_ nil)))

(defun monk-inventory--json-groups (file)
  "Return the group forms in FILE, an inventory in JSON.
Ansible's shape: an object of group name to {hosts: {name: {vars...}},
children: {name: {...}}, vars: {...}}.  Host entries become
\(NAME :address A :user U :port P :method M :vars V): `ansible_host'
is the address, `ansible_user' the user, `ansible_port' the port,
`monk_method' the TRAMP method, anything else a var.  Nested children
become groups of their own."
  (let* ((data (condition-case err
                   (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'alist :array-type 'list
                                        :null-object nil :false-object :false))
                 (error (signal 'monk-inventory-error
                                (list (format "%s: %s" (abbreviate-file-name file) (error-message-string err)))))))
         (groups nil))
    (cl-labels ((host-form (name props)
                  (let ((plist nil) (vars nil))
                    (dolist (cell (and (listp props) props))
                      (pcase (car cell)
                        ('ansible_host (setq plist (plist-put plist :address (format "%s" (cdr cell)))))
                        ('ansible_user (setq plist (plist-put plist :user (format "%s" (cdr cell)))))
                        ('ansible_port (setq plist (plist-put plist :port (cdr cell))))
                        ('monk_method (setq plist (plist-put plist :method (format "%s" (cdr cell)))))
                        ('ansible_connection nil)
                        (_ (push (cons (car cell) (monk-inventory--json-value (cdr cell))) vars))))
                    (when vars (setq plist (plist-put plist :vars (nreverse vars))))
                    (cons (intern (format "%s" name)) plist)))
                (group-form (name body)
                  (let ((plist nil) (body (and (listp body) body)))
                    (when (alist-get 'vars body)
                      (setq plist (plist-put plist :vars
                                             (mapcar (lambda (c) (cons (car c) (monk-inventory--json-value (cdr c))))
                                                     (alist-get 'vars body)))))
                    (when (alist-get 'hosts body)
                      (setq plist (plist-put plist :hosts
                                             (mapcar (lambda (h) (host-form (car h) (cdr h))) (alist-get 'hosts body)))))
                    (when (alist-get 'children body)
                      (setq plist (plist-put plist :children
                                             (mapcar (lambda (c) (group-form (car c) (cdr c))) (alist-get 'children body)))))
                    (cons (intern (format "%s" name)) plist))))
      (dolist (top data)
        ;; the marker Monk writes is not a group
        (unless (eq (car top) '_monk)
          (push (group-form (car top) (cdr top)) groups))))
    (nreverse groups)))

(defun monk-inventory--json-value (value)
  "Return a JSON VALUE as the inventory stores it: false is nil, else as is."
  (if (eq value :false) nil value))

(defun monk-inventory--json-of-groups (groups)
  "Return GROUPS, a list of group forms, as an alist for `json-serialize'.
The inverse of `monk-inventory--json-groups'.  An empty host or group
is an empty object."
  (cl-labels ((host-json (host)
                (let ((plist (cdr host)) (out nil))
                  (when (plist-get plist :address) (push (cons 'ansible_host (plist-get plist :address)) out))
                  (when (plist-get plist :user) (push (cons 'ansible_user (plist-get plist :user)) out))
                  (when (plist-get plist :port) (push (cons 'ansible_port (plist-get plist :port)) out))
                  (when (plist-get plist :method) (push (cons 'monk_method (plist-get plist :method)) out))
                  (dolist (v (plist-get plist :vars))
                    (push (cons (car v) (monk-inventory--json-scalar (cdr v))) out))
                  (cons (car host) (if out (nreverse out) (make-hash-table)))))
              (group-json (g)
                (let ((plist (cdr g)) (out nil))
                  (when (plist-member plist :vars)
                    (push (cons 'vars (mapcar (lambda (v) (cons (car v) (monk-inventory--json-scalar (cdr v))))
                                              (plist-get plist :vars)))
                          out))
                  (when (plist-get plist :hosts)
                    (push (cons 'hosts (mapcar #'host-json (plist-get plist :hosts))) out))
                  (when (plist-get plist :children)
                    (push (cons 'children (mapcar #'group-json (plist-get plist :children))) out))
                  (cons (car g) (if out (nreverse out) (make-hash-table))))))
    (mapcar #'group-json groups)))

(defun monk-inventory--json-scalar (value)
  "Return an inventory VALUE as `json-serialize' wants it: nil is false, not null."
  (if (null value) :false value))

(defun monk-inventory--merge-fragments (files)
  "Merge the group forms of FILES into one tree.  Return (TREE . ORIGINS).
Groups merge by name: vars from later files win per key, hosts and
children append.  A group named as a child but defined nowhere is
created empty.  The root is the group no other group lists; two roots
or none is an error, as is a host that two files list."
  (let ((groups nil)        ; name -> plist, merged
        (parents nil)       ; child name -> parent name
        (origins nil)       ; host -> file
        (order nil))        ; names in first-seen order
    (cl-labels ((merge-group (form file)
                  (pcase-let ((`(,name . ,plist) form))
                    (unless (memq name order) (push name order))
                    (let ((current (alist-get name groups)))
                      (when (plist-member plist :vars)
                        (setq current (plist-put current :vars
                                                 (monk--inventory-merge-vars (plist-get current :vars)
                                                                             (plist-get plist :vars)))))
                      (dolist (host (plist-get plist :hosts))
                        (let ((seen (assq (car host) origins)))
                          (when seen
                            (signal 'monk-inventory-error
                                    (list (format "host %s is in both %s and %s" (car host)
                                                  (abbreviate-file-name (cdr seen)) (abbreviate-file-name file))))))
                        (push (cons (car host) file) origins)
                        (setq current (plist-put current :hosts (append (plist-get current :hosts) (list host)))))
                      (dolist (child (plist-get plist :children))
                        (setf (alist-get (car child) parents) name)
                        (merge-group child file))
                      (setf (alist-get name groups) current)))))
      (dolist (file files)
        (dolist (form (monk-inventory--fragment-groups file))
          (merge-group form file))))
    (let* ((names (nreverse order))
           (roots (seq-remove (lambda (n) (assq n parents)) names)))
      (cond
       ((null names) (signal 'monk-inventory-error (list "No groups in the inventory fragments")))
       ((cdr roots)
        (signal 'monk-inventory-error
                (list (format "Inventory fragments have %d roots: %s; one group must hold the others"
                              (length roots) (mapconcat #'symbol-name roots ", ")))))
       ((null roots) (signal 'monk-inventory-error (list "Inventory fragments have no root: a group cycle"))))
      (cl-labels ((build (name)
                    (let* ((plist (copy-sequence (alist-get name groups)))
                           (children (seq-filter (lambda (n) (eq (alist-get n parents) name)) names)))
                      (setq plist (monk-inventory--plist-remove plist :children))
                      (when children
                        (setq plist (plist-put plist :children (mapcar #'build children))))
                      (cons name plist))))
        (cons (build (car roots)) origins)))))

(defun monk-inventory-save (&optional path)
  "Write the inventory to PATH, default `monk-inventory-file'.
Return the path.  A file gets the whole tree, one host per line.  A
directory: see `monk-inventory--save-directory'."
  (let ((path (expand-file-name (or path (monk-inventory-file)))))
    (unless monk-inventory-tree
      (signal 'monk-inventory-error (list "No inventory tree to save")))
    (if (file-directory-p path)
        (monk-inventory--save-directory path)
      (monk-inventory--save-file path))))

(defun monk-inventory--save-directory (directory)
  "Write `monk-inventory-tree' as fragments under DIRECTORY.  Return DIRECTORY.
Monk rewrites only files it wrote (its marker says so), `.el' and
`.json' alike.  A host that came from a hand-written file stays
there: when the tree holds it unchanged nothing happens; when the
tree changed it, the save refuses and names the file to edit by hand,
rather than write a second copy that the next load would reject as a
duplicate.  A new host gets a file of its own, in the format most
fragments use: `.json' when JSON files outnumber Lisp ones, else `.el'."
  (let* ((hosts (monk-inventory--hosts-with-groups monk-inventory-tree))
         (by-file nil)
         (foreign nil)
         ;; new files take the format most fragments use; a tie is Lisp
         (fragments (monk-inventory--fragment-files directory))
         (json-count (seq-count (lambda (f) (equal (file-name-extension f) "json")) fragments))
         (new-ext (if (> json-count (- (length fragments) json-count)) "json" "el"))
         (skeleton-file (expand-file-name (concat "groups." new-ext) directory)))
    (dolist (cell hosts)
      (pcase-let ((`(,host ,group . ,plist) cell))
        (let* ((from (cdr (assq host monk-inventory-origins)))
               (monks (and from
                           (equal (file-name-directory from) (file-name-as-directory directory))
                           (monk-inventory--monk-wrote-p from))))
          (cond
           (monks (push (list group host plist) (alist-get from by-file nil nil #'equal)))
           (from
            ;; hand-written: it must still read the same
            (let ((was (monk-inventory--host-in-file host from)))
              (unless (and was (equal (car was) group) (equal (cdr was) plist))
                (push (cons host from) foreign))))
           (t (push (list group host plist)
                    (alist-get (expand-file-name (format "%s.%s" host new-ext) directory) by-file nil nil #'equal)))))))
    (when foreign
      (signal 'monk-inventory-error
              (list (format "%s changed in the editor but live%s in %s, which Monk did not write; edit %s by hand or move the host%s to a Monk file"
                            (mapconcat (lambda (f) (symbol-name (car f))) foreign ", ")
                            (if (cdr foreign) "" "s")
                            (mapconcat #'abbreviate-file-name (delete-dups (mapcar #'cdr foreign)) ", ")
                            (if (cdr foreign) "them" "it")
                            (if (cdr foreign) "s" "")))))
    (make-directory directory t)
    ;; a Monk-written fragment whose hosts all moved elsewhere goes away
    (dolist (file (monk-inventory--fragment-files directory))
      (when (and (not (assoc file by-file)) (not (equal file skeleton-file))
                 (not (string-prefix-p "." (file-name-nondirectory file)))
                 (seq-some (lambda (o) (equal (cdr o) file)) monk-inventory-origins)
                 (monk-inventory--monk-wrote-p file))
        (delete-file file)))
    (pcase-dolist (`(,file . ,entries) by-file)
      (let ((groups (delete-dups (mapcar #'car entries))))
        (monk-inventory--write-fragment
         file
         (mapcar (lambda (group)
                   (list group :hosts
                         (mapcar (lambda (e) (cons (nth 1 e) (nth 2 e)))
                                 (seq-filter (lambda (e) (eq (car e) group)) (reverse entries)))))
                 groups)
         "fragment")))
    ;; the skeleton: groups, vars, children, no hosts.  Written when
    ;; Monk wrote it before, or when no other fragment defines the
    ;; root group (a directory that started empty); a hand-written
    ;; skeleton under any name is left alone and not duplicated
    (let ((root (car monk-inventory-tree)))
      (when (or (and (file-exists-p skeleton-file) (monk-inventory--monk-wrote-p skeleton-file))
                (not (seq-some (lambda (f)
                                 (and (not (equal f skeleton-file))
                                      (assq root (ignore-errors (monk-inventory--fragment-groups f)))))
                               (monk-inventory--fragment-files directory))))
        (monk-inventory--write-fragment skeleton-file
                                        (list (monk-inventory--strip-hosts monk-inventory-tree))
                                        "groups")))
    (setq monk-inventory-tree-file directory
          monk-inventory-origins
          (append (mapcan (lambda (cell) (mapcar (lambda (e) (cons (nth 1 e) (car cell))) (cdr cell))) by-file)
                  (seq-remove (lambda (o) (assoc (cdr o) by-file)) monk-inventory-origins)))
    (dolist (m monk-inventory)
      (let ((from (cdr (assq (intern (oref m name)) monk-inventory-origins))))
        (oset m origin (cons (or from directory) (oref m name)))))
    directory))

(defun monk-inventory--host-in-file (host file)
  "Return (GROUP . PLIST) for HOST as FILE states it now, or nil."
  (let (found)
    (dolist (g (ignore-errors (monk-inventory--fragment-groups file)))
      (cl-labels ((walk (group)
                    (let ((e (assq host (plist-get (cdr group) :hosts))))
                      (when e (setq found (cons (car group) (cdr e)))))
                    (mapc #'walk (plist-get (cdr group) :children))))
        (walk g)))
    found))

(defun monk-inventory--write-fragment (file groups kind)
  "Write GROUPS, a list of group forms, to FILE as KIND (\"fragment\" or \"groups\").
Lisp or JSON by FILE's extension.  Both carry the marker Monk looks
for before rewriting a file: a header comment in Lisp, a `_monk' key
in JSON (JSON has no comments)."
  (let ((stamp (format-time-string "%F %T")))
    (with-temp-file file
      (if (equal (file-name-extension file) "json")
          (insert (monk-inventory--json-pretty
                   (append (list (cons '_monk (format "inventory %s; written %s by Monk; groups merge by name with the other fragments" kind stamp)))
                           (monk-inventory--json-of-groups groups)))
                  "\n")
        (insert (format ";;; Monk inventory %s -*- mode: lisp-data; -*-\n" kind)
                (format ";; Written %s by Monk; groups merge by name with the other fragments.\n\n" stamp))
        (dolist (g groups)
          (insert (monk-inventory--pp g) "\n"))))))

(defun monk-inventory--json-pretty (alist)
  "Return ALIST as indented JSON text."
  (with-temp-buffer
    (insert (json-serialize alist :null-object nil :false-object :false))
    (json-pretty-print-buffer)
    (buffer-string)))

(defun monk-inventory--monk-wrote-p (file)
  "Return non-nil when FILE carries Monk's marker, so Monk may rewrite it.
A `;;; Monk inventory' header in Lisp; a `_monk' key in JSON."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file nil 0 400)
         (if (equal (file-name-extension file) "json")
             (string-match-p "\"_monk\"" (buffer-string))
           (string-match-p "\\`;;; Monk inventory" (buffer-string))))))

(defun monk-inventory--hosts-with-groups (tree)
  "Return ((HOST GROUP . PLIST) ...) for every host in TREE, in order."
  (let (out)
    (cl-labels ((walk (g)
                  (dolist (h (plist-get (cdr g) :hosts))
                    (push (cons (car h) (cons (car g) (cdr h))) out))
                  (mapc #'walk (plist-get (cdr g) :children))))
      (walk tree))
    (nreverse out)))

(defun monk-inventory--strip-hosts (tree)
  "Return TREE with every :hosts removed."
  (monk-inventory--map-groups tree (lambda (g) (cons (car g) (monk-inventory--plist-remove (cdr g) :hosts)))))

(defun monk-inventory--save-file (file)
  "Write `monk-inventory-tree' as one form to FILE.  Return FILE."
  (progn
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";;; Monk inventory -*- mode: lisp-data; -*-\n"
              ";; One form: the group tree.  Edited by monk-inventory-mode and monk-enroll;\n"
              ";; hand edits are fine, the file is read back as data.\n"
              (format ";; Written %s\n\n" (format-time-string "%F %T")))
      (let ((print-length nil) (print-level nil))
        (insert (monk-inventory--pp monk-inventory-tree) "\n")))
    (setq monk-inventory-tree-file file
          monk-inventory-origins nil)
    (dolist (m monk-inventory) (oset m origin (cons file (oref m name))))
    file))

(defun monk-inventory--pp (tree)
  "Return TREE printed with one host entry per line and groups indented."
  (with-temp-buffer
    (monk-inventory--insert-group tree)
    (buffer-string)))

(defun monk-inventory--insert-group (group)
  "Insert GROUP at point; continuation lines align under its open paren."
  (pcase-let* ((`(,name . ,plist) group)
               (column (current-column))
               (pad (make-string (1+ column) ?\s)))
    (insert "(" (symbol-name name))
    (when (plist-member plist :vars)
      (insert "\n" pad ":vars " (monk-inventory--print-vars (plist-get plist :vars))))
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
          (insert (monk-inventory--print-host host))
          (setq first nil)))
      (insert ")"))
    (insert ")")))

(defun monk-inventory--print-vars (vars)
  "Return VARS, an alist, printed with `(name . nil)' kept dotted.
`prin1' writes (observer) for (observer . nil); both read back the
same, but a person reading the file expects the dot."
  (concat "(" (mapconcat (lambda (c) (if (null (cdr c))
                                          (format "(%s . nil)" (car c))
                                        (prin1-to-string c)))
                         vars " ")
          ")"))

(defun monk-inventory--print-host (host)
  "Return HOST, (NAME . PLIST), printed; see `monk-inventory--print-vars'."
  (let ((plist (cdr host)) (parts (list (symbol-name (car host)))))
    (while plist
      (let ((k (pop plist)) (v (pop plist)))
        (push (format "%s %s" k (if (eq k :vars) (monk-inventory--print-vars v) (prin1-to-string v))) parts)))
    (concat "(" (string-join (nreverse parts) " ") ")")))

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
  (let ((old monk-inventory)
        (previous-tree monk-inventory-tree))
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
    (when save
      ;; a refused save (a host that lives in a file Monk did not write)
      ;; must not leave memory ahead of disk: put the old tree back
      (condition-case err
          (monk-inventory-save)
        (monk-inventory-error
         (monk-inventory-define previous-tree monk-inventory-tree-file)
         (setq monk-inventory old)
         (signal (car err) (cdr err)))))
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
