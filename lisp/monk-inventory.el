;;; monk-inventory.el --- Inventory for Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix

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
;; A host with no :address is the local machine.  Any variable whose
;; value is t becomes a class on the machine.
;;
;; Select machines with `monk-inventory-select', which accepts a host
;; name, a group name, a list of either, or the symbol `self' for the
;; host whose name matches the local host name.

;;; Code:

(require 'monk)
(require 'monk-cell)

(defvar monk-inventory nil
  "The current inventory: a list of `monk-machine' objects.")

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
                   :vars all-vars
                   :groups groups
                   :ignore-unreachable (and (cdr (assq 'ignore_unreachable all-vars)) t))
                  result))))
      result)))

(defun monk-inventory-define (tree)
  "Set `monk-inventory' from TREE.  Return the machine list.
Signal `monk-inventory-error' on duplicate host names."
  (let* ((machines (monk--inventory-walk tree nil nil))
         (names (mapcar (lambda (m) (oref m name)) machines))
         (file (or load-file-name (buffer-file-name))))
    (when (/= (length names) (length (delete-dups (copy-sequence names))))
      (signal 'monk-inventory-error (list "Duplicate host names" names)))
    (dolist (m machines)
      (oset m origin (cons file (oref m name))))
    (setq monk-inventory (nreverse machines))))

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
