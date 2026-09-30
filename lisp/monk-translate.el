;;; monk-translate.el --- Import from and export to Ansible and Salt with a model  -*- lexical-binding: t; -*-

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

;; Two commands, bound to `<' and `>' in `*monk*':
;;
;;   <  import: an Ansible playbook, role, or tasks file, or a Salt SLS,
;;      becomes a Monk policy in `*monk-import*'.
;;   >  export: the policy of the machine at point becomes a playbook
;;      or an SLS in `*monk-export*'.
;;
;; A language model does the mapping; Monk does everything that can be
;; exact.  The model is given the Monk catalog, generated from
;; `monk-resource-specs' at call time, so it maps against the
;; constructors that exist in this Emacs, with their parameters, types,
;; defaults, and choices, and never against what it remembers.  The
;; export side hands the model data, not Lisp: each compiled resource
;; as type, name, arguments, guard in CEL, requisites, and tags.
;;
;; Nothing the model returns is evaluated on receipt.  An import is
;; read as Lisp data, compiled with `monk-check-policy' against the
;; machine, with no connection, and shown in `*monk-check*'.  When the
;; compile fails, the error text goes back to the model, up to
;; `monk-translate-repair-rounds' times.  An export is checked with the
;; other tool when it is installed (`ansible-playbook --syntax-check',
;; `salt-call --local state.show_sls'), else only shown.
;;
;; The model is reached through `monk-translate-ask-function', by
;; default efrit's `efrit-ask-once' when efrit is loaded: one request,
;; off any session, answered once.  A test binds a fake and the drive
;; round-trips through it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'monk)
(require 'monk-cell)
(require 'monk-bridge)

(declare-function efrit-ask-once "efrit-ask")
(declare-function treesit-ready-p "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-major-mode-setup "treesit")
(defvar yaml-ts-mode--font-lock-settings)
(defvar treesit-primary-parser)
(defvar treesit-font-lock-settings)
(defvar treesit-font-lock-feature-list)
(declare-function monk-ui-check-policy "monk-ui" (machines policy &rest options))
(declare-function monk-ui--run-machine "monk-ui" (name))
(declare-function monk-ui-quit "monk-ui")
(defvar monk-ui--run)

(defgroup monk-translate nil
  "Import from and export to Ansible and Salt."
  :group 'monk)

(defcustom monk-translate-ask-function nil
  "Function that asks the model: (PROMPT CALLBACK &key SYSTEM PURPOSE MAX-TOKENS).
CALLBACK receives (TEXT nil) or (nil MESSAGE).  Nil means
`efrit-ask-once' when efrit is available.  A test binds its own."
  :type '(choice (const :tag "efrit-ask-once" nil) function))

(defcustom monk-translate-repair-rounds 3
  "How many times a policy that does not compile goes back to the model."
  :type 'natnum)

(defcustom monk-translate-max-tokens 16000
  "Room for the answer; a role of forty tasks needs it."
  :type 'natnum)

(defconst monk-translate-import-buffer-name "*monk-import*")
(defconst monk-translate-export-buffer-name "*monk-export*")

(defconst monk-translate-dialects '(("ansible" . ansible) ("salt" . salt))
  "Names the user picks from, and their symbols.")

(defconst monk-translate-fence-regexp "\\`[ \t\n]*```[a-z-]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t\n]*\\'"
  "A whole answer wrapped in one Markdown fence.")

(defconst monk-translate-ansible-check-program "ansible-playbook")
(defconst monk-translate-salt-check-program "salt-call")

(defvar monk-translate--policy-function 'monk-translate--imported-policy
  "Name the imported policy function must have.")

;;;; Asking

(defun monk-translate--ask (prompt callback &rest keys)
  "Send PROMPT to the model through `monk-translate-ask-function'.
CALLBACK and KEYS as for `efrit-ask-once'."
  (let ((ask (or monk-translate-ask-function
                 (and (require 'efrit-ask nil t) (fboundp 'efrit-ask-once) #'efrit-ask-once))))
    (unless ask
      (user-error "No model to ask: load efrit, or set monk-translate-ask-function"))
    (apply ask prompt callback :max-tokens monk-translate-max-tokens keys)))

(defun monk-translate--short (message)
  "Return MESSAGE cut to its first line and 200 characters."
  (truncate-string-to-width (car (split-string (format "%s" message) "\n")) 200 nil nil "..."))

(defun monk-translate-strip-fence (text)
  "Return TEXT without one Markdown code fence around all of it."
  (if (string-match monk-translate-fence-regexp text)
      (match-string 1 text)
    text))

;;;; The Monk catalog

(defun monk-translate--spec-line (spec &optional positional)
  "Return one catalog line for parameter SPEC; POSITIONAL says it is not a keyword."
  (let ((plist (cdr spec)))
    (format "  %s%s%s%s%s%s%s"
            (if positional "" ":")
            (car spec)
            (format " <%s>" (or (plist-get plist :type) 'any))
            (if (plist-get plist :required) " required" "")
            (if (plist-member plist :default) (format " default %S" (plist-get plist :default)) "")
            (if (plist-get plist :choices)
                (format " one of %s" (mapconcat (lambda (c) (format "%S" c)) (plist-get plist :choices) " "))
              "")
            (if (plist-get plist :doc) (format " -- %s" (plist-get plist :doc)) ""))))

(defun monk-translate-catalog ()
  "Return the Monk resource catalog as text, from `monk-resource-specs'.
Every constructor with its positional and keyword parameters, then the
common keys and the guard language.  Regenerated on each call, so a
resource defined a minute ago is in it."
  (with-temp-buffer
    (insert "MONK RESOURCE CATALOG\n"
            "A policy is (defun NAME (m) ...) whose body calls constructors in order.\n"
            "Positional parameters come first; keyword parameters as :key value.\n\n")
    (dolist (entry (reverse monk-resource-specs))
      (pcase-let ((`(,name ,positional ,keywords) (cons (car entry) (cdr entry))))
        (insert (format "(%s%s%s)\n" name
                        (mapconcat (lambda (sp) (format " %s" (upcase (symbol-name (car sp))))) positional "")
                        (if keywords " &key ..." "")))
        (let ((doc (documentation name)))
          (when doc (insert "  " (car (split-string doc "\n")) "\n")))
        (dolist (sp positional) (insert (monk-translate--spec-line sp t) "\n"))
        (dolist (sp keywords) (insert (monk-translate--spec-line sp) "\n"))
        (insert "\n")))
    (insert "COMMON KEYS on every constructor:\n"
            "  :when GUARD -- a CEL string or CELL form; run only when true\n"
            "  :classes (SYM...) -- sugar for (and (class SYM) ...)\n"
            "  :require (REF...) :watch (REF...) :onchanges (REF...) :onfail (REF...)\n"
            "     :prereq (REF...) :listen (REF...)\n"
            "     REF is a resource NAME string on the same machine, or (HOST . NAME)\n"
            "  :watch-action ACTION -- what a watch trigger does (for example 'restart)\n"
            "  :tags (SYM...) :sudo BOOL :retry PLIST :fatal BOOL :handled BOOL :no-log BOOL\n"
            "  :only-if FN :not-if FN -- functions of the machine; prefer :when\n\n"
            "GUARDS (CEL strings): classes is a list, facts and vars are maps, name is a string.\n"
            "  \"'darwin' in classes && facts.arch == 'arm64'\"\n"
            "  facts: kernel arch hostname nodename os-family distribution distribution-release\n"
            "         distribution-version home user pkg-mgr service-mgr, plus facts.local.*\n"
            "  classes include: linux darwin debian ubuntu fedora arch macos, the arch, the\n"
            "         hostname, pkg-mgr and service-mgr names, and inventory groups.\n"
            "  Ansible facts map: ansible_os_family -> facts['os-family'], ansible_distribution ->\n"
            "  facts.distribution, ansible_architecture -> facts.arch, ansible_hostname -> facts.hostname,\n"
            "  inventory_hostname -> name, group_names -> classes.\n"
            "  Salt grains map: os_family -> facts['os-family'], os -> facts.distribution,\n"
            "  kernel -> facts.kernel, osarch -> facts.arch, id -> name.\n\n"
            "PATHS: \"~/x\" is the home of the user on the target machine.\n"
            "ESCAPE HATCHES: (monk-ansible NAME MODULE ARGS-ALIST) runs one Ansible module as is;\n"
            "  (monk-salt NAME \"state.function\" ARGS-ALIST) runs one Salt state as is.  Use them\n"
            "  for anything with no native constructor above.  Never invent a constructor.\n")
    (buffer-string)))

;;;; Import

(defconst monk-translate-import-system
  "You translate configuration management code into Monk, an Emacs Lisp system.  You answer with Emacs Lisp only: no prose, no Markdown fence.  The answer is one file: a lexical-binding cookie line, then exactly one (defun %s (m) ...) whose body calls Monk constructors in the source order, then nothing else.  Rules: keep the source's names and order; every task or state becomes one constructor call; when: and unless: and onlyif: become :when with a CEL string; notify: and handlers, and Salt watch/watch_in, become :watch on the handler naming the notifier, with :watch-action; require/require_in become :require; tags: become :tags with symbols; become: yes becomes :sudo t; loops with_items/loop unroll into one call per item when the list is literal, else stay one call with a comment; anything without a native constructor becomes monk-ansible or monk-salt with the original arguments as an alist, never an invented constructor; variables you cannot resolve become (monk-machine-var m 'NAME) or a ;; TODO comment on the call; only the CEL syntax shown in the catalog.  A comment line ;; NOTE: ... above a call is welcome where you were unsure."
  "System prompt for import; %s is the policy function name.")

(defun monk-translate--import-prompt (dialect source)
  "Return the import prompt for SOURCE text in DIALECT."
  (format "%s\n\nSOURCE (%s):\n%s\n\nTranslate the source into one Monk policy function named %s."
          (monk-translate-catalog) dialect source monk-translate--policy-function))

(defun monk-translate--repair-prompt (previous errors)
  "Return a prompt that asks for PREVIOUS Lisp again, without ERRORS."
  (format "Your previous answer did not compile.  Return the whole file again, corrected, Lisp only.\n\nERRORS:\n%s\n\nPREVIOUS ANSWER:\n%s"
          errors previous))

(defun monk-translate-read-policy (text)
  "Read TEXT as the imported policy.  Return the defun form.
Signal `monk-policy-error' when TEXT is not one defun of the right name,
or does not read at all."
  (let ((forms nil) (pos 0))
    (condition-case err
        (while (progn (setq pos (or (string-match "[^ \t\n]" text pos) (length text)))
                      (< pos (length text)))
          (pcase-let ((`(,form . ,next) (read-from-string text pos)))
            (push form forms)
            (setq pos next)))
      (error (signal 'monk-policy-error (list (format "does not read as Lisp: %s" (error-message-string err))))))
    (let ((defun (seq-find (lambda (f) (and (consp f) (eq (car f) 'defun)
                                            (eq (cadr f) monk-translate--policy-function)))
                           forms)))
      (unless defun
        (signal 'monk-policy-error
                (list (format "no (defun %s (m) ...) in the answer" monk-translate--policy-function))))
      defun)))

(defun monk-translate-check-import (text machine)
  "Compile the policy in TEXT for MACHINE with no connection.
Return (REPORT . nil) from `monk-check-policy' when it read, or
\(nil . ERRORS) with the text to send back."
  (condition-case err
      (let ((defun (monk-translate-read-policy text)))
        ;; A fresh function each round; the old one must not linger.
        (fmakunbound monk-translate--policy-function)
        (eval defun t)
        (let ((report (monk-check-policy machine monk-translate--policy-function)))
          (if (monk-check-policy-ok-p report)
              (cons report nil)
            (cons report (monk-check-policy-string report)))))
    (error (cons nil (error-message-string err)))))

(defvar-local monk-translate--machine nil
  "The machine an import or export buffer belongs to.")
(defvar-local monk-translate--dialect nil
  "The dialect of the other side in this buffer.")
(defvar-local monk-translate--round 0
  "Repair rounds spent so far in this buffer.")

(defvar monk-translate-import-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'monk-translate-import-check)
    (define-key map (kbd "C-c C-k") #'monk-translate-import-repair)
    map)
  "Keymap for `monk-translate-import-mode'.")

(define-derived-mode monk-translate-import-mode emacs-lisp-mode "Monk-Import"
  "A policy the model translated.  Edit it, then \\[monk-translate-import-check] compiles it.
\\{monk-translate-import-mode-map}")

(defun monk-translate--import-buffer (machine dialect)
  "Return the import buffer for MACHINE and DIALECT, emptied."
  (let ((buffer (get-buffer-create monk-translate-import-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-translate-import-mode)
        (monk-translate-import-mode))
      (erase-buffer)
      (setq monk-translate--machine machine
            monk-translate--dialect dialect
            monk-translate--round 0))
    buffer))

(defun monk-translate--import-arrived (buffer text message)
  "Put TEXT from the model into BUFFER, or MESSAGE when it failed; then check."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (null text)
          (progn (insert (format ";; the model did not answer: %s\n" (monk-translate--short message)))
                 (message "monk import: %s" (monk-translate--short message)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (monk-translate-strip-fence text))
          (unless (bolp) (insert "\n")))
        (monk-translate-import-check)))))

(defun monk-translate-import-check ()
  "Compile the policy in this buffer against its machine; show `*monk-check*'.
On a compile error, send the errors back to the model for another
try, up to `monk-translate-repair-rounds' times, then leave the buffer
to you."
  (interactive)
  (pcase-let ((`(,report . ,errors) (monk-translate-check-import (buffer-string) monk-translate--machine)))
    (cond
     ((null errors)
      (require 'monk-ui)
      (monk-ui-check-policy monk-translate--machine monk-translate--policy-function)
      (message "monk import: %d resource(s) compile for %s; edit, then C-c C-c to check again"
               (length (plist-get report :nodes)) (oref monk-translate--machine name)))
     ((< monk-translate--round monk-translate-repair-rounds)
      (cl-incf monk-translate--round)
      (message "monk import: does not compile; repair round %d of %d..."
               monk-translate--round monk-translate-repair-rounds)
      (monk-translate-import-repair errors))
     (t
      (goto-char (point-max))
      (insert (format "\n;; does not compile after %d repair round(s):\n;; %s\n"
                      monk-translate--round (string-replace "\n" "\n;; " errors)))
      (message "monk import: still does not compile after %d rounds; see the end of the buffer"
               monk-translate--round)))))

(defun monk-translate-import-repair (&optional errors)
  "Send this buffer and ERRORS (default: a fresh check) back to the model."
  (interactive)
  (let ((errors (or errors (cdr (monk-translate-check-import (buffer-string) monk-translate--machine))
                    (user-error "It compiles; nothing to repair")))
        (buffer (current-buffer)))
    (monk-translate--ask
     (monk-translate--repair-prompt (buffer-string) errors)
     (lambda (text message) (monk-translate--import-arrived buffer text message))
     :system (format monk-translate-import-system monk-translate--policy-function)
     :purpose "monk-import-repair")))

;;;###autoload
(defun monk-translate-import (dialect source machine)
  "Translate SOURCE, text in DIALECT, into a Monk policy for MACHINE.
Interactively, DIALECT is asked, SOURCE is the region when active,
else a file, and MACHINE is the local host.  The answer lands in
`*monk-import*' and is compiled with no connection."
  (interactive
   (list (cdr (assoc (completing-read "Import from: " monk-translate-dialects nil t) monk-translate-dialects))
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (with-temp-buffer
             (insert-file-contents (read-file-name "Playbook, tasks file, or SLS: " nil nil t))
             (buffer-string)))
         (monk-local-machine)))
  (let ((buffer (monk-translate--import-buffer machine dialect)))
    (with-current-buffer buffer
      (insert (format ";; asking the model to translate %d lines of %s for %s...\n"
                      (length (split-string source "\n")) dialect (oref machine name))))
    (pop-to-buffer buffer)
    (monk-translate--ask
     (monk-translate--import-prompt dialect source)
     (lambda (text message) (monk-translate--import-arrived buffer text message))
     :system (format monk-translate-import-system monk-translate--policy-function)
     :purpose "monk-import")
    buffer))

;;;; Export

(defun monk-translate-resource-data (resource)
  "Return RESOURCE as an alist of plain data for the model.
Type, name, the declared arguments by slot, the guard as CEL,
requisites, watch action, tags, and sudo."
  (let ((args nil))
    (dolist (slot (eieio-class-slots (eieio-object-class resource)))
      (let* ((sname (cl--slot-descriptor-name slot))
             (key (intern (format ":%s" sname))))
        (unless (or (memq key monk--common-keys) (memq sname '(name origin)))
          (when (and (slot-boundp resource sname) (slot-value resource sname))
            (push (cons sname (slot-value resource sname)) args)))))
    (delq nil
          (list (cons 'type (monk-resource-type resource))
                (cons 'name (oref resource name))
                (cons 'args (nreverse args))
                (let ((guard (monk-resource-guard resource)))
                  (and guard (cons 'when (monk-cell-to-cel guard))))
                (and (oref resource require) (cons 'require (oref resource require)))
                (and (oref resource watch) (cons 'watch (oref resource watch)))
                (and (oref resource watch-action) (cons 'watch-action (oref resource watch-action)))
                (and (oref resource onchanges) (cons 'onchanges (oref resource onchanges)))
                (and (oref resource onfail) (cons 'onfail (oref resource onfail)))
                (and (oref resource tags) (cons 'tags (oref resource tags)))
                (and (oref resource sudo) (cons 'sudo t))))))

(defun monk-translate-policy-data (machine policy)
  "Compile POLICY for MACHINE with no connection; return the resources as data.
Signal `monk-policy-error' when the policy does not compile."
  (let ((report (monk-check-policy machine policy)))
    (unless (monk-check-policy-ok-p report)
      (signal 'monk-policy-error (list (monk-check-policy-string report))))
    (mapcar (lambda (node) (monk-translate-resource-data (monk-node-resource node)))
            (plist-get report :nodes))))

(defconst monk-translate-export-system
  "You translate Monk policy data into %s.  You answer with YAML only: no prose, no Markdown fence.  Rules: one task or state per resource, in the given order, named after the resource name; the Monk type and arguments say which module or state function to use (directory -> file state=directory / file.directory, file -> copy with content or template / file.managed, symlink -> file state=link / file.symlink, line-in-file -> lineinfile / file.line or file.replace, brew -> community.general.homebrew / pkg.installed, apt -> apt / pkg.installed, git -> git / git.latest, command -> command with creates / cmd.run with creates or unless, cron -> cron / cron.present, systemd-unit -> systemd / service.running, launchd -> community.general.launchd / service.running, osx-default -> community.general.osx_defaults / macdefaults.write, sysctl -> ansible.posix.sysctl / sysctl.present, rsync -> synchronize / rsync.synchronized, ansible -> the named module as is, salt -> the named state as is); when (CEL) becomes when: / onlyif with the equivalent expression over facts or grains (classes are group names or os facts); require becomes task order plus require:; watch becomes notify: with a handler named after the watched resource plus a handlers: section / watch:; tags carry over; sudo becomes become: yes; ~ paths stay ~ paths.  Where no module exists, write a command/cmd.run with a comment.  Emit a complete file: for Ansible a playbook with one play for hosts: all; for Salt one SLS."
  "System prompt for export; %s is the dialect.")

(defun monk-translate--export-prompt (dialect machine data)
  "Return the export prompt for DATA of MACHINE in DIALECT."
  (format "MACHINE %s: classes %s\n\nRESOURCES (Lisp data, in policy order):\n%s\nWrite the %s file."
          (oref machine name) (mapconcat #'symbol-name (oref machine classes) " ")
          (mapconcat (lambda (d) (pp-to-string d)) data "")
          (if (eq dialect 'ansible) "Ansible playbook" "Salt SLS")))

(defun monk-translate-check-export (dialect file)
  "Check FILE with the DIALECT's own tool.  Return nil when fine, else a reason.
Return `unchecked' when the tool is not installed."
  (pcase dialect
    ('ansible
     (if (not (executable-find monk-translate-ansible-check-program))
         'unchecked
       (with-temp-buffer
         (let ((process-environment (append '("ANSIBLE_NOCOLOR=1" "ANSIBLE_DEPRECATION_WARNINGS=False")
                                            process-environment)))
           (if (zerop (call-process monk-translate-ansible-check-program nil t nil "--syntax-check" file))
               nil
             (string-trim (buffer-string)))))))
    ('salt
     (if (not (executable-find monk-translate-salt-check-program))
         'unchecked
       (with-temp-buffer
         (let ((root (file-name-directory file))
               (sls (file-name-base file)))
           (if (zerop (call-process monk-translate-salt-check-program nil t nil
                                    "--local" "--retcode-passthrough" "--log-level=quiet"
                                    (format "--file-root=%s" root)
                                    "state.show_sls" sls))
               nil
             (string-trim (buffer-string)))))))))

(defvar monk-translate-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'monk-translate-export-check)
    map)
  "Keymap for `monk-translate-export-mode'.")

(define-derived-mode monk-translate-export-mode text-mode "Monk-Export"
  "A playbook or SLS the model wrote from a Monk policy.
YAML highlighting comes from `yaml-ts-mode' when its grammar is
installed; the mode itself stays text so the buffer works without it.
\\{monk-translate-export-mode-map}"
  (when (and (fboundp 'yaml-ts-mode) (fboundp 'treesit-ready-p)
             (ignore-errors (treesit-ready-p 'yaml t)))
    (setq-local treesit-primary-parser (treesit-parser-create 'yaml))
    (setq-local font-lock-defaults nil)
    (when (boundp 'yaml-ts-mode--font-lock-settings)
      (setq-local treesit-font-lock-settings yaml-ts-mode--font-lock-settings)
      (setq-local treesit-font-lock-feature-list
                  '((comment) (string type) (constant escape-sequence number property) (bracket delimiter error misc-punctuation)))
      (treesit-major-mode-setup))))

(defun monk-translate-export-check ()
  "Check this buffer with the other tool, when installed.  Report in the echo area."
  (interactive)
  (let* ((file (make-temp-file "monk-export-" nil
                               (if (eq monk-translate--dialect 'salt) ".sls" ".yml")))
         (verdict (unwind-protect
                      (progn (write-region (point-min) (point-max) file nil 'silent)
                             (monk-translate-check-export monk-translate--dialect file))
                    (delete-file file))))
    (pcase verdict
      ('nil (message "monk export: %s accepts it" monk-translate--dialect))
      ('unchecked (message "monk export: %s is not installed here; not checked" monk-translate--dialect))
      (reason (message "monk export: %s rejects it: %s" monk-translate--dialect
                       (car (last (split-string reason "\n" t))))
              (goto-char (point-max))
              (insert (format "\n# %s check failed:\n# %s\n" monk-translate--dialect
                              (string-replace "\n" "\n# " reason)))))))

(defun monk-translate--export-arrived (buffer text message)
  "Put TEXT from the model into BUFFER, or MESSAGE, then check it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (if (null text)
          (progn (insert (format "# the model did not answer: %s\n" (monk-translate--short message)))
                 (message "monk export: %s" (monk-translate--short message)))
        (insert (monk-translate-strip-fence text))
        (unless (bolp) (insert "\n"))
        (goto-char (point-min))
        (monk-translate-export-check)))))

;;;###autoload
(defun monk-translate-export (dialect machine policy)
  "Write the POLICY of MACHINE as a DIALECT file into `*monk-export*'.
Interactively, DIALECT is asked and MACHINE and POLICY come from the
run in `*monk*' at point.  The policy is compiled with no connection;
the resources go to the model as data."
  (interactive
   (let ((run (and (boundp 'monk-ui--run) monk-ui--run)))
     (unless run (user-error "Run a plan first; > exports the policy of the machine at point"))
     (list (cdr (assoc (completing-read "Export to: " monk-translate-dialects nil t) monk-translate-dialects))
           (or (and (fboundp 'tabulated-list-get-id) (tabulated-list-get-id)
                    (monk-ui--run-machine (tabulated-list-get-id)))
               (car (monk-run-machines run)))
           (monk-run-policy run))))
  (let ((data (monk-translate-policy-data machine policy))
        (buffer (get-buffer-create monk-translate-export-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-translate-export-mode)
        (monk-translate-export-mode))
      (erase-buffer)
      (setq monk-translate--machine machine
            monk-translate--dialect dialect)
      (insert (format "# asking the model to write %d resource(s) of %s as %s...\n"
                      (length data) (oref machine name) dialect)))
    (pop-to-buffer buffer)
    (monk-translate--ask
     (monk-translate--export-prompt dialect machine data)
     (lambda (text message) (monk-translate--export-arrived buffer text message))
     :system (format monk-translate-export-system dialect)
     :purpose "monk-export")
    buffer))

(monk-note-load 'monk-translate)
(provide 'monk-translate)
;;; monk-translate.el ends here
