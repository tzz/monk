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
;; `monk-salt' runs state functions through `salt-call --local
;; --out=json'.  One resource alone uses `state.single'.  Several
;; independent `monk-salt' resources on one machine aggregate into one
;; `state.high' call with a JSON high-data document, so salt-call
;; starts once per pass, not once per resource, and requisites among
;; them are Salt's own.  `test=True' gives the check result.  The JSON
;; is `{"local": {"STATE_|-ID_|-NAME_|-FUN": {"result": ..., "changes":
;; ..., "comment": ..., "__id__": ..., "__run_num__": ...}}}'.  A state
;; that would change reports `result' null in test mode.
;;
;; `monk-salt-import-sls' turns an SLS file into `monk-salt' resources
;; through `state.show_low_sls', requisites included.
;; `monk-salt-state-functions' lists the state functions the target
;; knows, cached per run, for completion in `monk-salt'.
;; `monk-salt-matcher-to-cell' translates a compound matcher such as
;; `G@os_family:Debian and not L@a,b' into a CELL guard.
;;
;; Both run on the target machine through `monk-exec', so a remote
;; machine needs the tool installed there.

;;; Code:

(require 'monk)
(require 'monk-cell)
(require 'monk-package)
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

(defconst monk-salt-key-separator "_|-"
  "Separator inside a Salt state result key: STATE_|-ID_|-NAME_|-FUN.")

(defconst monk-salt-high-order-start 10000
  "First `order' value Salt assigns; aggregated states keep policy order from here.")

(defconst monk-salt-grain-facts
  '(("os_family" . os-family) ("os" . distribution) ("osrelease" . distribution-version)
    ("oscodename" . distribution-release) ("kernel" . kernel) ("osarch" . arch)
    ("cpuarch" . arch) ("host" . hostname) ("id" . hostname) ("fqdn" . nodename)
    ("nodename" . nodename) ("username" . user))
  "Salt grain names and the Monk fact each maps to.")

(defconst monk-salt-os-family-values
  '(("MacOS" . "Darwin"))
  "Salt `os_family' values that Monk's `os-family' spells differently.")

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
uses null for \"would change\" and false for \"failed\".  Stderr
noise lands in TEXT too, so when the whole text does not parse, the
last line that starts with `{' is tried alone."
  (or (condition-case nil
          (json-parse-string text :object-type 'alist :array-type 'list
                             :null-object nil :false-object :false)
        (error nil))
      (let ((line (car (last (seq-filter (lambda (l) (string-prefix-p "{" l))
                                         (split-string text "\n" t))))))
        (and line
             (condition-case nil
                 (json-parse-string line :object-type 'alist :array-type 'list
                                    :null-object nil :false-object :false)
               (error nil))))))

(defun monk-bridge--json-string (object)
  "Return OBJECT as compact JSON.  Alists with symbol keys become objects."
  (json-serialize (monk-bridge--jsonable object) :null-object nil :false-object :false))

(defun monk-bridge--jsonable (value)
  "Return VALUE in a shape `json-serialize' accepts."
  (cond
   ((or (null value) (eq value t) (stringp value) (numberp value) (eq value :false)) value)
   ((and (consp value) (consp (car value)) (or (symbolp (caar value)) (stringp (caar value))))
    (mapcar (lambda (cell) (cons (if (stringp (car cell)) (intern (car cell)) (car cell))
                                 (monk-bridge--jsonable (cdr cell))))
            value))
   ((listp value) (vconcat (mapcar #'monk-bridge--jsonable value)))
   ((vectorp value) (vconcat (mapcar #'monk-bridge--jsonable value)))
   (t (format "%s" value))))

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

(defun monk-salt--call (m arguments &optional sudo)
  "Run salt-call on M with ARGUMENTS after `monk-salt-base-args'.
SUDO runs it as root.  Return the value under `monk-salt-result-key'.
Signal on no JSON.
For a state.* call, a list of strings is a compile error and signals
too; other functions return lists of strings legitimately."
  (let* ((exec (apply #'monk-exec m monk-salt-program
                      (append monk-salt-base-args arguments (list :sudo sudo))))
         (data (monk-bridge--parse-json (monk-exec-output exec)))
         (local (and data (alist-get (intern monk-salt-result-key) data))))
    (cond
     ((null data)
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
     ((and (consp local) (stringp (car local))
           (seq-some (lambda (a) (and (stringp a) (string-prefix-p "state." a))) arguments))
      (signal 'monk-resource-error (list (format "salt: %s" (string-join local "; ")))))
     (t local))))

(defun monk-salt-split-key (key)
  "Return (STATE . FUN) from a Salt result KEY, a string or symbol.
The id and name in the middle can contain the separator, so they are
read from the value, not from KEY.  See `monk-salt-result-id'."
  (let* ((text (format "%s" key))
         (first (string-search monk-salt-key-separator text))
         (last (and first (let ((pos nil) (start 0))
                            (while (setq start (string-search monk-salt-key-separator text start))
                              (setq pos start start (1+ start)))
                            pos))))
    (if first
        (cons (substring text 0 first)
              (substring text (+ last (length monk-salt-key-separator))))
      (cons text nil))))

(defun monk-salt-result-id (key value)
  "Return the state id of a result: `__id__' in VALUE, else parsed from KEY."
  (or (alist-get '__id__ value)
      (let* ((text (format "%s" key))
             (parts (split-string text (regexp-quote monk-salt-key-separator))))
        (and (= 4 (length parts)) (nth 1 parts)))))

(cl-defmethod monk--salt-run ((r monk-salt-resource) (m monk-machine) test)
  "Run the state of R alone on M with `state.single'.  With TEST, test=True.
Return the state result alist: result, changes, comment."
  (let ((local (monk-salt--call
                m (append (list "state.single" (oref r function)
                                (format "name=%s" (oref r name)))
                          (monk-bridge--format-args (oref r args))
                          (and test '("test=True")))
                (oref r sudo))))
    (if (null local)
        (signal 'monk-resource-error (list "salt: no state result in output"))
      ;; One state id maps to one result alist.
      (cdar local))))

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

(cl-defmethod monk-resource-available-p ((_r monk-salt-resource) (m monk-machine))
  (monk-program-available-p m monk-salt-program))

;;;; Salt: aggregation through state.high

(defclass monk-salt-high-resource (monk-salt-resource)
  ((members :initarg :members
            :initform nil
            :documentation "The `monk-salt-resource' objects this call stands for."))
  "Several Salt states run as one `state.high' document.")

(defun monk-salt-high-data (resources)
  "Return the high-data alist for RESOURCES, a list of `monk-salt-resource'.
Each becomes {ID: {STATE: [{name: N}, {k: v}..., {order: I}, ...]}}.
Requisites among RESOURCES that Monk knows become Salt requisites of
the same kind, so Salt orders them; other refs are Monk's business."
  (let ((names (mapcar (lambda (r) (oref r name)) resources))
        (order (1- monk-salt-high-order-start)))
    (mapcar
     (lambda (r)
       (pcase-let* ((`(,state . ,fun) (let ((parts (split-string (oref r function) "\\.")))
                                        (cons (car parts) (string-join (cdr parts) "."))))
                    (args (append (list (list (cons 'name (oref r name))))
                                  (mapcar (lambda (cell) (list (cons (car cell) (cdr cell))))
                                          (oref r args))
                                  (list (list (cons 'order (cl-incf order))))
                                  (delq nil
                                        (mapcar
                                         (lambda (kind)
                                           (let ((targets
                                                  (delq nil
                                                        (mapcar (lambda (ref)
                                                                  (let ((ref (monk-parse-ref ref)))
                                                                    (and (null (monk-ref-scope ref))
                                                                         (member (monk-ref-name ref) names)
                                                                         (monk-ref-name ref))))
                                                                (slot-value r kind)))))
                                             (and targets
                                                  (list (cons kind
                                                              (mapcar (lambda (n)
                                                                        (let ((target (seq-find (lambda (x) (equal (oref x name) n)) resources)))
                                                                          (list (cons (intern (car (split-string (oref target function) "\\.")))
                                                                                      n))))
                                                                      targets))))))
                                         '(require watch onchanges onfail prereq))))))
         (cons (intern (oref r name))
               (list (cons (intern state) (append args (list fun)))))))
     resources)))

(defun monk-salt--run-high (m resources test sudo)
  "Run RESOURCES as one state.high on M.  With TEST, test=True.
Return an alist (ID . STATE-RESULT) keyed by resource name."
  (let ((local (monk-salt--call
                m (append (list "state.high" (monk-bridge--json-string (monk-salt-high-data resources)))
                          (and test '("test=True")))
                sudo)))
    (mapcar (lambda (cell) (cons (monk-salt-result-id (car cell) (cdr cell)) (cdr cell)))
            local)))

(cl-defmethod monk-resource-aggregate ((r monk-salt-resource) others (_m monk-machine))
  "Merge R and OTHERS, salt resources with the same sudo, into one state.high."
  (when (cl-every (lambda (o) (eq (null (oref o sudo)) (null (oref r sudo)))) others)
    (monk-salt-high-resource
     :name (format "%s (+%d)" (oref r name) (length others))
     :function "state.high"
     :members (cons r others)
     :sudo (oref r sudo))))

(cl-defmethod monk-check ((r monk-salt-high-resource) (m monk-machine))
  (let ((changes nil))
    (dolist (cell (monk-salt--run-high m (oref r members) t (oref r sudo)))
      (let ((member-changes (condition-case err
                                (monk-salt--changes (cdr cell))
                              (monk-resource-error (list :salt-failed (cadr err))))))
        (when member-changes
          (push (cons (car cell) member-changes) changes))))
    (and changes (list :salt-high (nreverse changes)))))

(cl-defmethod monk-repair ((r monk-salt-high-resource) (m monk-machine) changes)
  "Repair on M only the members of R that CHANGES say need it.
Salt orders them itself."
  (let* ((need (mapcar #'car (plist-get changes :salt-high)))
         (members (seq-filter (lambda (x) (member (oref x name) need)) (oref r members)))
         (results (monk-salt--run-high m members nil (oref r sudo)))
         (failed (seq-filter (lambda (cell)
                               (let ((res (alist-get 'result (cdr cell))))
                                 (or (null res) (eq res :false))))
                             results)))
    (monk-run-context-put (list 'salt-high-results (oref m name)) results)
    (when failed
      (signal 'monk-resource-error
              (list (format "salt: %s"
                            (mapconcat (lambda (c) (format "%s: %s" (car c) (alist-get 'comment (cdr c))))
                                       failed "; ")))))
    (format "state.high: %d state%s" (length members) (if (= 1 (length members)) "" "s"))))

(cl-defmethod monk-resource-disaggregate ((_a monk-salt-high-resource) (member monk-salt-resource) changes)
  (let ((mine (cdr (assoc (oref member name) (plist-get changes :salt-high)))))
    (and mine (if (plist-get mine :salt-failed)
                  (list :salt (plist-get mine :salt-failed))
                mine))))

;;;; Salt: state function completion

(defun monk-salt-state-functions (m)
  "Return the state functions salt-call on M knows, cached for the run.
Outside a run the call happens every time.  An error yields nil, so
completion degrades to free text."
  (monk-with-run-cache (list 'salt-state-functions (oref m name))
    (condition-case nil
        (let ((names (monk-salt--call m '("sys.list_state_functions"))))
          (and (listp names) (mapcar (lambda (n) (format "%s" n)) names)))
      (error nil))))

(defvar monk-salt-function-history nil
  "Minibuffer history for Salt state function names.")

(defun monk-salt-read-function (&optional machine)
  "Read a Salt state function name with completion from MACHINE.
MACHINE defaults to the local host."
  (let ((candidates (monk-salt-state-functions (or machine (monk-local-machine)))))
    (completing-read "Salt state function: " candidates nil nil nil 'monk-salt-function-history)))

;;;; Salt: import an SLS as Monk resources

(defun monk-salt-low-chunks (m sls &optional file-root pillar-root)
  "Return the low-data chunks of SLS on M, as `state.show_low_sls' gives them.
FILE-ROOT and PILLAR-ROOT point salt-call at ad hoc directories."
  (monk-salt--call m (append (delq nil (list (and file-root (concat "--file-root=" file-root))
                                             (and pillar-root (concat "--pillar-root=" pillar-root))))
                             (list "state.show_low_sls" sls))))

(defconst monk-salt-chunk-meta-keys
  '(state fun name __id__ __sls__ __env__ order require watch onchanges onfail prereq
    require_in watch_in onchanges_in onfail_in prereq_in listen listen_in use use_in
    require_any onfail_any onfail_all onfail_stop failhard)
  "Keys of a low chunk that are not arguments of the state function.")

(defun monk-salt-chunk-to-form (chunk)
  "Return a `monk-salt' form for low CHUNK, an alist.
Requisites become Monk requisites on the id.  The form is data, so a
policy file can hold it as written."
  (let* ((get (lambda (k) (alist-get k chunk)))
         (id (funcall get '__id__))
         (name (funcall get 'name))
         (function (format "%s.%s" (funcall get 'state) (funcall get 'fun)))
         (args (seq-remove (lambda (cell) (memq (car cell) monk-salt-chunk-meta-keys)) chunk))
         (refs (lambda (kind)
                 (let ((value (funcall get kind)))
                   (and value
                        (mapcar (lambda (req)
                                  ;; {mod: id-or-name}: keep the id or name
                                  (format "%s" (cdar req)))
                                value))))))
    (append (list 'monk-salt (or name id) function
                  (list 'quote (mapcar (lambda (c) (cons (car c) (cdr c))) args)))
            (and (not (equal name id)) (list :id id))
            (mapcan (lambda (kind)
                      (let ((r (funcall refs kind)))
                        (and r (list (intern (format ":%s" kind)) (list 'quote r)))))
                    '(require watch onchanges onfail prereq listen)))))

(defun monk-salt-import-sls (m sls &optional file-root pillar-root)
  "Return `monk-salt' forms for SLS on M, in Salt's order.
FILE-ROOT and PILLAR-ROOT as for `monk-salt-low-chunks'.  Paste the
forms into a policy, or `eval' them inside a compile."
  (let ((chunks (monk-salt-low-chunks m sls file-root pillar-root)))
    (mapcar #'monk-salt-chunk-to-form
            (sort (copy-sequence chunks)
                  (lambda (a b) (< (or (alist-get 'order a) 0) (or (alist-get 'order b) 0)))))))

(defun monk-salt-insert-sls (sls &optional file-root pillar-root)
  "Insert the `monk-salt' forms for SLS at point, from the local salt-call.
FILE-ROOT and PILLAR-ROOT as for `monk-salt-low-chunks'."
  (interactive "sSLS: ")
  (dolist (form (monk-salt-import-sls (monk-local-machine) sls file-root pillar-root))
    (pp form (current-buffer))))

;;;; Salt: compound matchers to CELL

(defun monk-salt--matcher-tokens (text)
  "Split compound matcher TEXT on whitespace.
Salt requires spaces around parentheses, so a parenthesis inside a
token belongs to a regexp such as P@os:(Ubuntu|Debian)."
  (split-string text "[ \t]+" t))

(defun monk-salt--glob-to-regexp (glob)
  "Return a regexp for fnmatch GLOB, anchored at both ends."
  (concat "\\`" (replace-regexp-in-string
                  "\\\\\\*" ".*"
                  (replace-regexp-in-string "\\\\\\?" "." (regexp-quote glob) t t)
                  t t)
          "\\'"))

(defun monk-salt--value-form (accessor value pcre)
  "Return a CELL comparison of ACCESSOR against VALUE.
VALUE is an fnmatch glob unless PCRE.  Salt matches case-insensitively;
an exact glob becomes `=' with the value as written."
  (cond
   (pcre (list 'match accessor (concat "\\`" value)))
   ((string-match-p "[*?\\[]" value) (list 'match accessor (monk-salt--glob-to-regexp value)))
   (t (list '= accessor value))))

(defun monk-salt--grain-accessor (key)
  "Return the CELL accessor for Salt grain KEY, and its value translator."
  (let ((fact (cdr (assoc key monk-salt-grain-facts))))
    (cond
     (fact (list 'fact fact))
     (t (list 'fact (intern key))))))

(defun monk-salt--leaf-to-cell (token)
  "Translate one matcher TOKEN such as G@os_family:Debian into CELL."
  (if (and (> (length token) 2) (eq (aref token 1) ?@))
      (let ((engine (aref token 0))
            (body (substring token 2)))
        (pcase engine
          ((or ?G ?P)
           (let* ((sep (string-search ":" body))
                  (key (and sep (substring body 0 sep)))
                  (value (and sep (substring body (1+ sep)))))
             (unless sep (signal 'monk-guard-error (list (format "matcher %s needs key:value" token))))
             (let ((value (if (equal key "os_family")
                              (or (cdr (assoc value monk-salt-os-family-values)) value)
                            value)))
               (if (and (equal key "kernel") (eq engine ?G) (not (string-match-p "[*?]" value)))
                   (list 'class (intern (downcase value)))
                 (monk-salt--value-form (monk-salt--grain-accessor key) value (eq engine ?P))))))
          ((or ?I ?J)
           (let* ((sep (string-search ":" body))
                  (key (and sep (substring body 0 sep)))
                  (value (and sep (substring body (1+ sep)))))
             (unless sep (signal 'monk-guard-error (list (format "matcher %s needs key:value" token))))
             (monk-salt--value-form (list 'var (intern key)) value (eq engine ?J))))
          (?L (list 'in '(name) (cons 'list (split-string body "," t))))
          (?E (list 'match '(name) (concat "\\`" body)))
          (?N (list 'group (intern body)))
          (?S (signal 'monk-guard-error (list "S@ (ipcidr) has no CELL form: Monk has no address fact")))
          (_ (signal 'monk-guard-error (list (format "unknown matcher engine %c in %s" engine token))))))
    ;; bare glob on the minion id
    (monk-salt--value-form '(name) token nil)))

(defun monk-salt-matcher-to-cell (text)
  "Translate compound matcher TEXT into a CELL form.
Engines: G@ grain glob, P@ grain regexp, I@ pillar glob, J@ pillar
regexp, L@ list, E@ id regexp, N@ nodegroup, and a bare id glob.
Operators: and, or, not, parentheses.  Grain names map through
`monk-salt-grain-facts'; `os_family' values through
`monk-salt-os-family-values'.  Salt compares case-insensitively and
CELL `=' does not, so check values against `monk-machine-describe'."
  (let ((tokens (monk-salt--matcher-tokens text)))
    (cl-labels
        ((parse-or ()
           (let ((left (parse-and)))
             (while (equal (car tokens) "or")
               (pop tokens)
               (setq left (list 'or left (parse-and))))
             left))
         (parse-and ()
           (let ((left (parse-not)))
             (while (and tokens (not (member (car tokens) '("or" ")"))))
               (when (equal (car tokens) "and") (pop tokens))
               (setq left (list 'and left (parse-not))))
             left))
         (parse-not ()
           (if (equal (car tokens) "not")
               (progn (pop tokens) (list 'not (parse-not)))
             (parse-primary)))
         (parse-primary ()
           (let ((token (pop tokens)))
             (cond
              ((null token) (signal 'monk-guard-error (list "matcher ended early")))
              ((equal token "(")
               (prog1 (parse-or)
                 (unless (equal (pop tokens) ")")
                   (signal 'monk-guard-error (list "matcher: missing )")))))
              (t (monk-salt--leaf-to-cell token))))))
      (let ((form (parse-or)))
        (when tokens
          (signal 'monk-guard-error (list (format "matcher: unexpected %s" (car tokens)))))
        (monk-cell-check form)))))

(defun monk-salt (name function args &rest rest)
  "Ensure the state that Salt FUNCTION with ARGS describes for NAME.
NAME is the state name argument and the resource name.  ARGS is an
alist of (KEY . VALUE).  REST: :id to keep an SLS id that differs from
the name, and common keys.  Independent salt resources on a machine
run as one `state.high' call."
  (monk-add-resource
   (apply #'monk-salt-resource
          :name name
          (monk--resource-plist rest :function function :args args))))

(provide 'monk-bridge)
;;; monk-bridge.el ends here
