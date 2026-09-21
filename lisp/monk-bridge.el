;;; monk-bridge.el --- Call Ansible modules and Salt states from Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, processes

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

;; Two resources that delegate to other configuration tools installed
;; on the target machine.  Both tools expose only two modes, "check"
;; and "apply", so Monk maps its check phase to the check mode and its
;; repair phase to the apply mode.  The tool runs twice when a change
;; is needed.  Both tools start a Python interpreter per call, which
;; costs one to three seconds, so use these resources for modules Monk
;; does not implement natively.
;;
;; `monk-ansible' runs one module through `ansible' in ad-hoc mode,
;; against localhost with a local connection.  The `-o' flag gives one
;; line per host: `HOST | STATUS => JSON'.  `--check --diff' gives the
;; check result.  Modules that do not support check mode report
;; `skipped' and Monk treats that as a change on every pass, so such
;; a resource repairs in each pass and the run reports that it did not
;; converge.  Give those resources a `:creates' guard through a
;; `monk-command' instead, or accept the report.
;;
;; `monk-salt' runs one state function through `salt-call --local
;; --out=json state.single'.  `test=True' gives the check result.
;; The JSON is `{"local": {"ID": {"result": ..., "changes": ...,
;; "comment": ...}}}'.  A state that would change reports `result'
;; null in test mode.
;;
;; Both run on the target machine through `monk-exec', so a remote
;; machine needs the tool installed there.

;;; Code:

(require 'monk)
(require 'json)

(defconst monk-ansible-program "ansible"
  "The Ansible ad-hoc command.")

(defconst monk-ansible-host "localhost"
  "Host pattern for ad-hoc runs on the target itself.")

(defconst monk-ansible-connection "local"
  "Connection plugin for ad-hoc runs on the target itself.")

(defconst monk-ansible-status-ok "SUCCESS")
(defconst monk-ansible-status-changed "CHANGED")
(defconst monk-ansible-status-failed "FAILED!")
(defconst monk-ansible-status-unreachable "UNREACHABLE!")
(defconst monk-ansible-status-skipped "SKIPPED")

(defconst monk-ansible-line-regexp
  (concat "\\`[^|]+ | \\("
          (regexp-opt (list monk-ansible-status-ok monk-ansible-status-changed
                            monk-ansible-status-failed monk-ansible-status-unreachable
                            monk-ansible-status-skipped))
          "\\)\\( =>\\)? ?")
  "Match the prefix of an ansible -o line and capture the status word.
The status words are listed so that a diff line with a bar in it
does not match.")

(defconst monk-salt-program "salt-call"
  "The Salt local command.")

(defconst monk-salt-base-args
  '("--local" "--out=json" "--retcode-passthrough" "--log-level=quiet")
  "Arguments every salt-call invocation gets.
Log output goes to stderr, which `monk-exec' merges into the output,
so it must be off for the JSON to parse.")

(defconst monk-salt-result-key "local"
  "Top-level key of salt-call JSON output.")

;;;; Shared

(defun monk-bridge--format-args (args)
  "Return ARGS, an alist of (KEY . VALUE), as key=value strings.
Booleans become yes and no.  Lists join with commas."
  (mapcar (lambda (cell)
            (format "%s=%s" (car cell)
                    (pcase (cdr cell)
                      ('t "yes")
                      ('nil "no")
                      ((and (pred listp) l) (mapconcat (lambda (v) (format "%s" v)) l ","))
                      (v v))))
          args))

(defun monk-bridge--parse-json (text)
  "Parse TEXT as JSON into alists with symbol keys.  Return nil on failure.
JSON null becomes nil and JSON false becomes `:false', because Salt
uses null for \"would change\" and false for \"failed\"."
  (condition-case nil
      (json-parse-string text :object-type 'alist :array-type 'list
                         :null-object nil :false-object :false)
    (error nil)))

;;;; Ansible

(defclass monk-ansible-resource (monk-resource)
  ((module :initarg :module
           :type string
           :documentation "Module name, for example file or lineinfile.")
   (args :initarg :args
         :initform nil
         :documentation "Alist of module arguments.")
   (become :initarg :become
           :initform nil
           :documentation "Pass --become.  Prefer :sudo, which uses TRAMP."))
  "One Ansible module call on the target machine.")

(cl-defmethod monk--ansible-run ((r monk-ansible-resource) (m monk-machine) check)
  "Run the module of R on M.  With CHECK, pass --check --diff.
Return a plist (:status WORD :data ALIST :raw STRING)."
  (let* ((args (append (list monk-ansible-host
                             "-c" monk-ansible-connection
                             "-o"
                             "-m" (oref r module)
                             "-a" (mapconcat #'shell-quote-argument
                                             (monk-bridge--format-args (oref r args))
                                             " "))
                       (and (oref r become) '("--become"))
                       (and check '("--check" "--diff"))
                       (list :sudo (oref r sudo))))
         (process-environment (append '("ANSIBLE_NOCOLOR=1"
                                        "ANSIBLE_DEPRECATION_WARNINGS=False"
                                        "ANSIBLE_HOST_KEY_CHECKING=False")
                                      process-environment))
         (exec (apply #'monk-exec m monk-ansible-program args))
         (line (seq-find (lambda (l) (string-match-p monk-ansible-line-regexp l))
                         (split-string (monk-exec-output exec) "\n" t))))
    (unless line
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
    (string-match monk-ansible-line-regexp line)
    (let ((status (match-string 1 line))
          (json (substring line (match-end 0))))
      (list :status status
            :data (monk-bridge--parse-json json)
            :raw line))))

(defun monk-ansible--changes (status data)
  "Return the change plist for an Ansible result with STATUS and DATA."
  (cond
   ((member status (list monk-ansible-status-failed monk-ansible-status-unreachable))
    (signal 'monk-resource-error
            (list (format "ansible %s: %s" status (or (alist-get 'msg data) "no message")))))
   ;; A module without check mode reports SKIPPED in check mode.
   ((equal status monk-ansible-status-skipped)
    (list :ansible 'no-check-mode))
   ((or (equal status monk-ansible-status-changed)
        (eq (alist-get 'changed data) t))
    (list :ansible (or (alist-get 'msg data) t)))
   (t nil)))

(defun monk-ansible--diff-string (data)
  "Return the diff in Ansible result DATA as a string, or nil.
Modules return a string, one alist with before and after, or a list
of such alists."
  (let ((diff (alist-get 'diff data)))
    (cond
     ((stringp diff) diff)
     ((and (consp diff) (consp (car diff)) (consp (caar diff)))
      ;; A list of alists.
      (mapconcat #'monk-ansible--diff-string
                 (mapcar (lambda (d) (list (cons 'diff d))) diff)
                 ""))
     ((and (consp diff) (alist-get 'before diff))
      (format "--- before\n+++ after\n-%s\n+%s\n"
              (alist-get 'before diff) (alist-get 'after diff)))
     (t nil))))

(cl-defmethod monk-check ((r monk-ansible-resource) (m monk-machine))
  (let* ((outcome (monk--ansible-run r m t))
         (data (plist-get outcome :data))
         (changes (monk-ansible--changes (plist-get outcome :status) data)))
    ;; Keep the diff from this call so `monk-diff' does not run the
    ;; module a second time.
    (if (and changes (monk-ansible--diff-string data))
        (plist-put changes :diff (monk-ansible--diff-string data))
      changes)))

(cl-defmethod monk-diff ((_r monk-ansible-resource) (_m monk-machine) changes)
  (plist-get changes :diff))

(cl-defmethod monk-repair ((r monk-ansible-resource) (m monk-machine) _changes)
  (let* ((outcome (monk--ansible-run r m nil))
         (status (plist-get outcome :status))
         (data (plist-get outcome :data)))
    (when (member status (list monk-ansible-status-failed monk-ansible-status-unreachable))
      (signal 'monk-resource-error
              (list (format "ansible %s: %s" status (or (alist-get 'msg data) "no message")))))
    (format "%s %s" (oref r module) (or (alist-get 'msg data) (downcase status)))))

(defun monk-ansible (name module args &rest rest)
  "Ensure the state that Ansible MODULE with ARGS describes.
NAME identifies the resource.  ARGS is an alist of (KEY . VALUE).
REST: :become and common keys."
  (monk-add-resource
   (apply #'monk-ansible-resource
          :name name
          (monk--resource-plist rest
                                :module module
                                :args args
                                :become (plist-get rest :become)))))

;;;; Salt

(defclass monk-salt-resource (monk-resource)
  ((function :initarg :function
             :type string
             :documentation "State function, for example file.symlink.")
   (args :initarg :args
         :initform nil
         :documentation "Alist of state arguments.  `name' is added from NAME."))
  "One Salt state function call on the target machine.")

(cl-defmethod monk--salt-run ((r monk-salt-resource) (m monk-machine) test)
  "Run the state of R on M.  With TEST, pass test=True.
Return the state result alist: result, changes, comment."
  (let* ((args (append monk-salt-base-args
                       (list "state.single" (oref r function)
                             (format "name=%s" (oref r name)))
                       (monk-bridge--format-args (oref r args))
                       (and test '("test=True"))
                       (list :sudo (oref r sudo))))
         (exec (apply #'monk-exec m monk-salt-program args))
         (data (monk-bridge--parse-json (monk-exec-output exec)))
         (local (alist-get (intern monk-salt-result-key) data)))
    (cond
     ((null data)
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
     ;; A compile error returns a list of strings under "local".
     ((and (consp local) (stringp (car local)))
      (signal 'monk-resource-error (list (format "salt: %s" (string-join local "; ")))))
     ((null local)
      (signal 'monk-resource-error (list "salt: no state result in output")))
     (t
      ;; One state id maps to one result alist.
      (cdar local)))))

(defun monk-salt--changes (state)
  "Return the change plist for a Salt STATE result alist.
In test mode a pending change has result null and non-empty changes
or a comment that starts with a change verb."
  (let ((result (alist-get 'result state))
        (changes (alist-get 'changes state))
        (comment (alist-get 'comment state)))
    (cond
     ((eq result :false)
      (signal 'monk-resource-error (list (format "salt: %s" comment))))
     ((and (null result) (or changes (stringp comment)))
      (list :salt (or changes comment)))
     (changes (list :salt changes))
     (t nil))))

(cl-defmethod monk-check ((r monk-salt-resource) (m monk-machine))
  (monk-salt--changes (monk--salt-run r m t)))

(cl-defmethod monk-repair ((r monk-salt-resource) (m monk-machine) _changes)
  (let* ((state (monk--salt-run r m nil))
         (result (alist-get 'result state)))
    (when (or (null result) (eq result :false))
      (signal 'monk-resource-error
              (list (format "salt: %s" (alist-get 'comment state)))))
    (format "%s: %s" (oref r function) (alist-get 'comment state))))

(defun monk-salt (name function args &rest rest)
  "Ensure the state that Salt FUNCTION with ARGS describes for NAME.
NAME is the state name argument and the resource name.  ARGS is an
alist of (KEY . VALUE).  REST: common keys."
  (monk-add-resource
   (apply #'monk-salt-resource
          :name name
          (monk--resource-plist rest :function function :args args))))

(provide 'monk-bridge)
;;; monk-bridge.el ends here
