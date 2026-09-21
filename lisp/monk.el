;;; monk.el --- Configuration management in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, files, processes, comm
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.2.0

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

;; Monk (management of networked komputers) is a configuration
;; management system.  Policy is Emacs Lisp.  Remote access is TRAMP.
;;
;; The model borrows from four systems:
;;
;; - CFEngine: a resource is a promise.  Each promise ends kept,
;;   repaired, partial, or failed.  Conditions are classes.  The agent
;;   runs until a pass repairs nothing.
;; - Chef: two phases.  Compile builds a list of resource objects.
;;   Converge applies them.  Guards are lambdas.
;; - Salt: each resource returns a structured result with name,
;;   status, degree, changes, and comment.  Requisites `require',
;;   `watch', `onchanges', and `onfail' order resources, across
;;   machines when the run covers more than one.
;; - Ansible: module semantics (symlink, line in file, cron, git).
;;
;; A policy is a function of one argument, the machine.  Inside it,
;; resource constructors such as `monk-symlink' add resources to the
;; current compile.  `monk-converge' checks and repairs each resource
;; on one machine.  `monk-orchestrate' does the same for a set of
;; machines as one dependency graph.
;;
;; Every resource accepts these keywords:
;;
;;   :only-if FN      run only when (funcall FN machine) is non-nil
;;   :not-if  FN      skip when (funcall FN machine) is non-nil
;;   :classes LIST    run only when the machine has every class in LIST
;;   :require REFS    run after REFS, and only when they succeeded
;;   :watch REFS      like :require, then run :watch-action when a REF changed
;;   :onchanges REFS  run only when a REF changed in this run
;;   :onfail REFS     run only when a REF failed in this run
;;   :sudo    BOOL    perform the resource as root
;;
;; A REF names a resource on the same machine ("~/.zshrc"), on another
;; machine ((kamen . "/etc/hosts")), or on a group ((unix . "/etc/hosts")).
;; A REF can carry thresholds: (:ref REF :min N :fraction F :degree D).
;;
;; See the README for the full resource catalog.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'tramp)

(defgroup monk nil
  "Configuration management in Emacs."
  :group 'tools
  :prefix "monk-")

;;;; Constants

(defconst monk-status-kept 'kept
  "Resource was already in the desired state.")
(defconst monk-status-repaired 'repaired
  "Resource was changed to reach the desired state.")
(defconst monk-status-partial 'partial
  "Resource is between states: some of it holds, the rest failed.")
(defconst monk-status-failed 'failed
  "Resource could not reach the desired state.")
(defconst monk-status-skipped 'skipped
  "Resource was not evaluated: a guard, class, or requisite did not match.")
(defconst monk-status-would-repair 'would-repair
  "Dry run: resource would be changed.")
(defconst monk-status-pending 'pending
  "Resource has not run yet.  Used by progress displays.")
(defconst monk-status-running 'running
  "Resource is being checked or repaired.  Used by progress displays.")

(defconst monk-statuses
  (list monk-status-kept monk-status-repaired monk-status-partial
        monk-status-failed monk-status-skipped monk-status-would-repair)
  "All statuses a finished result can carry, in display order.")

(defconst monk-max-passes 3
  "Upper bound on convergence passes in one run.")

(defconst monk-tramp-method "ssh"
  "TRAMP method for remote machines.")

(defconst monk-tramp-sudo-method "sudo"
  "TRAMP method used as the second hop for privileged operations.")

(defconst monk-log-buffer-name "*monk-log*"
  "Buffer that receives log lines.")

(defconst monk-requisite-kinds '(require watch onchanges onfail)
  "Requisite slots, in evaluation order.")

(defconst monk-degree-satisfied 1.0
  "Degree that means a resource fully holds.")

;;;; Customization

(defcustom monk-dry-run nil
  "When non-nil, runs report changes and perform none."
  :type 'boolean)

(defcustom monk-log-level 'info
  "Lowest log level that Monk records.
One of `debug', `info', `warn', or nil to record nothing."
  :type '(choice (const debug) (const info) (const warn) (const nil)))

(defcustom monk-show-diffs t
  "When non-nil, file content results carry a unified diff."
  :type 'boolean)

(defcustom monk-progress-functions nil
  "Functions called with progress events.
Each is called with two arguments: EVENT, one of `run-start',
`node-start', `node-end', `pass-end', `run-end', and DATA, a plist.
`monk-ui' adds its display function here."
  :type 'hook)

;;;; Errors

(define-error 'monk-error "Monk error")
(define-error 'monk-resource-error "Monk resource error" 'monk-error)
(define-error 'monk-exec-error "Monk command failed" 'monk-error)
(define-error 'monk-policy-error "Monk policy error" 'monk-error)
(define-error 'monk-inventory-error "Monk inventory error" 'monk-error)
(define-error 'monk-requisite-error "Monk requisite error" 'monk-error)

;;;; Logging

(defconst monk--log-levels '((debug . 0) (info . 1) (warn . 2))
  "Numeric ranks for `monk-log-level'.")

(defun monk--log-enabled-p (level)
  "Return non-nil when LEVEL is at or above `monk-log-level'."
  (and monk-log-level
       (>= (alist-get level monk--log-levels 0)
           (alist-get monk-log-level monk--log-levels 0))))

(defun monk-log (level format-string &rest args)
  "Log FORMAT-STRING with ARGS at LEVEL to `monk-log-buffer-name'."
  (when (monk--log-enabled-p level)
    (let ((line (apply #'format format-string args)))
      (with-current-buffer (get-buffer-create monk-log-buffer-name)
        (goto-char (point-max))
        (insert (format-time-string "%FT%T ")
                (upcase (symbol-name level)) " " line "\n"))
      (when (and noninteractive (not (eq level 'debug)))
        (message "%s" line)))))

(defun monk-progress (event &rest data)
  "Send EVENT with plist DATA to `monk-progress-functions'."
  (run-hook-with-args 'monk-progress-functions event data))

;;;; Results

(cl-defstruct (monk-result (:constructor monk-result-create)
                           (:copier nil))
  "Outcome of one resource on one machine."
  machine
  name
  type
  status
  (degree 0.0)
  changes
  comment
  diff
  (duration 0.0))

(defun monk-result-changed-p (result)
  "Return non-nil when RESULT repaired or would repair."
  (memq (monk-result-status result)
        (list monk-status-repaired monk-status-would-repair)))

(defun monk-result-failed-p (result)
  "Return non-nil when RESULT failed, in whole or in part."
  (memq (monk-result-status result)
        (list monk-status-failed monk-status-partial)))

(defun monk-result-key (result)
  "Return the ledger key of RESULT: (MACHINE-NAME . RESOURCE-NAME)."
  (cons (monk-result-machine result) (monk-result-name result)))

(cl-defstruct (monk-run (:constructor monk-run-create)
                        (:copier nil))
  "All results from one run over one or more machines."
  machines
  results
  passes
  dry-run
  (last-pass-repaired 0)
  (repaired-by-pass nil)
  (start-time (current-time))
  end-time)

(defun monk-run-flapping (run)
  "Return the keys of resources in RUN that repaired in two consecutive passes.
A resource that repairs, is checked again, and repairs again did not
hold: another resource undid it, or its check and repair disagree.
The classic case is one resource that installs a package and another
that removes it.  Each key is (MACHINE-NAME . RESOURCE-NAME).  Nil
means no flapping."
  (let ((by-pass (reverse (monk-run-repaired-by-pass run)))
        (flapping nil))
    (while (cdr by-pass)
      (dolist (key (cadr by-pass))
        (when (and (member key (car by-pass))
                   (not (member key flapping)))
          (push key flapping)))
      (setq by-pass (cdr by-pass)))
    (nreverse flapping)))

(defun monk-run-flapping-p (run)
  "Return non-nil when RUN has resources that flap.  See `monk-run-flapping'."
  (and (monk-run-flapping run) t))

(defun monk-run-machine (run)
  "Return the first machine of RUN.  Single-machine runs have one."
  (car (monk-run-machines run)))

(defun monk-run-count (run status &optional machine)
  "Return how many results in RUN have STATUS.
With MACHINE, a name string, count only that machine."
  (seq-count (lambda (r)
               (and (eq (monk-result-status r) status)
                    (or (null machine) (equal (monk-result-machine r) machine))))
             (monk-run-results run)))

(defun monk-run-ok-p (run)
  "Return non-nil when no result in RUN failed."
  (not (seq-some #'monk-result-failed-p (monk-run-results run))))

(defun monk-run-oscillating-p (run)
  "Return non-nil when RUN hit the pass cap with repairs still happening.
That means two or more resources disagree about the same state."
  (and (not (monk-run-dry-run run))
       (> (monk-run-last-pass-repaired run) 0)))

(defun monk-run-converged-p (run)
  "Return non-nil when RUN settled: nothing flaps and the pass cap was not hit."
  (not (or (monk-run-oscillating-p run) (monk-run-flapping-p run))))

;;;; Machines

(defclass monk-machine ()
  ((name :initarg :name
         :type string
         :documentation "Inventory name.  Used in logs and reports.")
   (address :initarg :address
            :initform nil
            :documentation "Host to connect to.  Nil means the local host.")
   (user :initarg :user
         :initform nil
         :documentation "Login user for remote connections.")
   (port :initarg :port
         :initform nil
         :documentation "SSH port for remote connections.")
   (vars :initarg :vars
         :initform nil
         :documentation "Alist of inventory variables after group merge.")
   (groups :initarg :groups
           :initform nil
           :documentation "List of inventory group symbols, most specific first.")
   (facts :initarg :facts
          :initform nil
          :documentation "Alist of facts.  Filled by `monk-gather-facts'.")
   (classes :initarg :classes
            :initform nil
            :documentation "List of class symbols.  Filled by `monk-gather-facts'."))
  "A machine that Monk manages.")

(cl-defmethod monk-machine-local-p ((m monk-machine))
  "Return non-nil when M is the local host."
  (null (oref m address)))

(cl-defmethod monk-machine-var ((m monk-machine) key &optional default)
  "Return inventory variable KEY of M, or DEFAULT."
  (let ((cell (assq key (oref m vars))))
    (if cell (cdr cell) default)))

(cl-defmethod monk-machine-fact ((m monk-machine) key &optional default)
  "Return fact KEY of M, or DEFAULT."
  (let ((cell (assq key (oref m facts))))
    (if cell (cdr cell) default)))

(cl-defmethod monk-machine-in-group-p ((m monk-machine) group)
  "Return non-nil when M belongs to GROUP, a symbol."
  (memq group (oref m groups)))

(cl-defmethod monk-machine-remote-prefix ((m monk-machine) &optional sudo)
  "Return the TRAMP prefix for M.
When SUDO is non-nil, add a sudo hop.  For the local host without
SUDO, return the empty string."
  (cond
   ((and (monk-machine-local-p m) (not sudo)) "")
   ((monk-machine-local-p m)
    (format "/%s::" monk-tramp-sudo-method))
   (t
    (let ((host (concat (and (oref m user) (concat (oref m user) "@"))
                        (oref m address)
                        (and (oref m port) (format "#%s" (oref m port))))))
      (if sudo
          (format "/%s:%s|%s::" monk-tramp-method host monk-tramp-sudo-method)
        (format "/%s:%s:" monk-tramp-method host))))))

(cl-defmethod monk-machine-path ((m monk-machine) path &optional sudo)
  "Return PATH as seen from the controller for machine M.
Expand `~' on the target, not on the controller."
  (let ((prefix (monk-machine-remote-prefix m sudo)))
    (if (string-empty-p prefix)
        (expand-file-name path)
      (concat prefix path))))

(defun monk-machine-local-path (full)
  "Return FULL without its TRAMP prefix, as the target machine sees it."
  (if (tramp-tramp-file-p full)
      (tramp-file-name-localname (tramp-dissect-file-name full))
    full))

(cl-defmethod monk-machine-default-directory ((m monk-machine) &optional sudo)
  "Return a `default-directory' value rooted on M.
When SUDO is non-nil, root it in the home of root."
  (let ((prefix (monk-machine-remote-prefix m sudo)))
    (if (string-empty-p prefix)
        (expand-file-name "~/")
      (concat prefix "~/"))))

(cl-defmethod monk-machine-expand ((m monk-machine) path &optional sudo)
  "Return PATH with a leading `~' expanded to the home directory on M.
Other paths return unchanged.  The result is a target-side path with
no TRAMP prefix, fit for link targets and command arguments.  With
SUDO, `~' is the home of root."
  (cond
   ((not (string-prefix-p "~" path)) path)
   ((and (string-prefix-p "~/" path) (not sudo) (monk-machine-fact m 'home))
    (concat (monk-machine-fact m 'home) (substring path 1)))
   (t (file-local-name (expand-file-name (monk-machine-path m path sudo))))))

(defun monk-local-machine (&rest slots)
  "Return a machine for the local host.  SLOTS override defaults."
  (apply #'monk-machine
         :name (or (plist-get slots :name) (system-name))
         (let ((rest (copy-sequence slots)))
           (cl-remf rest :name)
           rest)))

;;;; Command execution

(cl-defstruct (monk-exec (:constructor monk-exec-create)
                         (:copier nil))
  "Result of running a command on a machine."
  command
  (exit-code 0)
  (output ""))

(defun monk-exec-ok-p (exec)
  "Return non-nil when EXEC exited with zero."
  (eql 0 (monk-exec-exit-code exec)))

(cl-defgeneric monk-exec (machine program &rest args)
  "Run PROGRAM with ARGS on MACHINE.  Return a `monk-exec'.
ARGS can end with keyword pairs: :sudo BOOL to run as root, :input
STRING to send on standard input, :directory PATH to run in a
directory on MACHINE.")

(cl-defgeneric monk-exec-string (machine program &rest args)
  "Run PROGRAM with ARGS on MACHINE and return trimmed output.
Signal `monk-exec-error' on non-zero exit.")

(cl-defgeneric monk-exec-lines (machine program &rest args)
  "Run PROGRAM with ARGS on MACHINE and return output lines.")

(cl-defmethod monk-exec ((m monk-machine) program &rest args)
  (let (sudo input directory)
    (when (and args (keywordp (car (last args))))
      (signal 'monk-exec-error (list "Keyword without a value" (car (last args)))))
    (while (and (>= (length args) 2) (keywordp (car (last args 2))))
      (let ((value (car (last args)))
            (key (car (last args 2))))
        (setq args (butlast args 2))
        (pcase key
          (:sudo (setq sudo value))
          (:input (setq input value))
          (:directory (setq directory value))
          (_ (signal 'monk-exec-error (list "Unknown keyword" key))))))
    (let ((default-directory (if directory
                                 (monk-machine-path m (file-name-as-directory directory) sudo)
                               (monk-machine-default-directory m sudo)))
          (process-file-side-effects t))
      (monk-log 'debug "%s: exec %s %s" (oref m name) program
                (mapconcat #'shell-quote-argument args " "))
      (with-temp-buffer
        (let ((code
               (if input
                   (let ((stdin (make-temp-file "monk-stdin")))
                     (unwind-protect
                         (progn
                           (with-temp-file stdin (insert input))
                           (apply #'process-file program stdin t nil args))
                       (delete-file stdin)))
                 (apply #'process-file program nil t nil args))))
          (monk-exec-create :command (cons program args)
                            :exit-code code
                            :output (buffer-string)))))))

(cl-defmethod monk-exec-string ((m monk-machine) program &rest args)
  (let ((exec (apply #'monk-exec m program args)))
    (unless (monk-exec-ok-p exec)
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec)
                    (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
    (string-trim (monk-exec-output exec))))

(cl-defmethod monk-exec-lines ((m monk-machine) program &rest args)
  (split-string (apply #'monk-exec-string m program args) "\n" t))

(cl-defmethod monk-executable-find ((m monk-machine) program)
  "Return non-nil when PROGRAM is on the path of M."
  (let ((default-directory (monk-machine-default-directory m)))
    (executable-find program t)))

;;;; Facts and classes

(defvar monk-fact-probes nil
  "Alist of fact probes, as (NAME . FUNCTION) pairs.
Each FUNCTION takes a machine and returns an alist of facts to
merge.  Probes run in list order.")

(defun monk-register-fact-probe (name function)
  "Register FUNCTION as the fact probe NAME."
  (setf (alist-get name monk-fact-probes) function))

(defun monk--probe-uname (m)
  "Return kernel facts for M."
  (let ((kernel (monk-exec-string m "uname" "-s"))
        (arch (monk-exec-string m "uname" "-m"))
        (node (monk-exec-string m "uname" "-n")))
    (list (cons 'kernel kernel)
          (cons 'arch arch)
          (cons 'nodename node)
          (cons 'hostname (car (split-string node "\\."))))))

(defun monk--parse-os-release (text)
  "Parse os-release TEXT into an alist of downcased symbol keys."
  (let (result)
    (dolist (line (split-string text "\n" t))
      (when (string-match "\\`\\([A-Z_]+\\)=\"?\\([^\"]*\\)\"?\\'" line)
        (push (cons (intern (downcase (match-string 1 line)))
                    (match-string 2 line))
              result)))
    (nreverse result)))

(defun monk--probe-os (m)
  "Return operating system facts for M."
  (pcase (monk-machine-fact m 'kernel)
    ("Darwin"
     (list (cons 'os-family "Darwin")
           (cons 'distribution "MacOS")
           (cons 'distribution-version
                 (monk-exec-string m "sw_vers" "-productVersion"))))
    ("Linux"
     (let* ((path (monk-machine-path m "/etc/os-release"))
            (release (when (file-readable-p path)
                       (monk--parse-os-release
                        (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string)))))
            (id (alist-get 'id release ""))
            (like (alist-get 'id_like release "")))
       (list (cons 'os-family
                   (cond ((or (member id '("debian" "ubuntu" "raspbian"))
                              (string-match-p "debian" like))
                          "Debian")
                         ((or (member id '("fedora" "rhel" "centos"))
                              (string-match-p "rhel\\|fedora" like))
                          "RedHat")
                         (t (capitalize id))))
             (cons 'distribution (capitalize id))
             (cons 'distribution-release
                   (alist-get 'version_codename release ""))
             (cons 'distribution-version
                   (alist-get 'version_id release "")))))
    (_ (list (cons 'os-family "Unknown")))))

(defun monk--probe-env (m)
  "Return home and user facts for M."
  (list (cons 'home (monk-exec-string m "sh" "-c" "echo $HOME"))
        (cons 'user (monk-exec-string m "id" "-un"))))

(monk-register-fact-probe 'uname #'monk--probe-uname)
(monk-register-fact-probe 'os #'monk--probe-os)
(monk-register-fact-probe 'env #'monk--probe-env)

(defun monk--class-symbol (string)
  "Return a class symbol for STRING."
  (intern (downcase (replace-regexp-in-string "[^[:alnum:]]+" "_" string))))

(cl-defmethod monk-hard-classes ((m monk-machine))
  "Return classes derived from the facts of M."
  (let ((classes nil))
    (dolist (key '(kernel os-family distribution distribution-release arch))
      (let ((value (monk-machine-fact m key)))
        (when (and (stringp value) (not (string-empty-p value)))
          (push (monk--class-symbol value) classes))))
    (push (monk--class-symbol (oref m name)) classes)
    (dolist (group (oref m groups))
      (push group classes))
    (dolist (var (oref m vars))
      (when (eq (cdr var) t)
        (push (car var) classes)))
    (push (if (monk-machine-local-p m) 'local 'remote) classes)
    (delete-dups (nreverse classes))))

(cl-defmethod monk-gather-facts ((m monk-machine))
  "Run all fact probes on M.  Fill facts and classes.  Return M."
  (oset m facts nil)
  (dolist (probe monk-fact-probes)
    (condition-case err
        (dolist (fact (funcall (cdr probe) m))
          (setf (alist-get (car fact) (oref m facts)) (cdr fact)))
      (error
       (monk-log 'warn "%s: fact probe %s failed: %s"
                 (oref m name) (car probe) (error-message-string err)))))
  (oset m classes (monk-hard-classes m))
  m)

(cl-defmethod monk-class-p ((m monk-machine) class)
  "Return non-nil when M has CLASS."
  (memq class (oref m classes)))

(cl-defmethod monk-add-class ((m monk-machine) class)
  "Add soft CLASS to M."
  (cl-pushnew class (oref m classes)))

;;;; Resources

(defclass monk-resource ()
  ((name :initarg :name
         :type string
         :documentation "Unique name within a policy.  Usually the target path.")
   (only-if :initarg :only-if
            :initform nil
            :documentation "Guard.  Function of the machine.  Run when non-nil.")
   (not-if :initarg :not-if
           :initform nil
           :documentation "Guard.  Function of the machine.  Skip when non-nil.")
   (classes :initarg :classes
            :initform nil
            :documentation "Run only when the machine has all these classes.")
   (require :initarg :require
            :initform nil
            :documentation "Refs that must succeed before this resource runs.")
   (watch :initarg :watch
          :initform nil
          :documentation "Refs like `require'.  When one changed, run `watch-action'.")
   (watch-action :initarg :watch-action
                 :initform nil
                 :documentation "Action for `monk-action' when a watched ref changed.
Nil means the value of `monk-default-watch-action'.")
   (onchanges :initarg :onchanges
              :initform nil
              :documentation "Refs.  Run only when one of them changed in this run.")
   (onfail :initarg :onfail
           :initform nil
           :documentation "Refs.  Run only when one of them failed in this run.
The failed ref needs :ignore-errors, or its failure stops the run
before this resource is reached.")
   (sudo :initarg :sudo
         :initform nil
         :documentation "Perform the resource as root.")
   (ignore-errors :initarg :ignore-errors
                  :initform nil
                  :documentation "Report failure but do not stop the run."))
  :abstract t
  :documentation "Base class.  Subclasses implement `monk-check' and `monk-repair'.")

(cl-defgeneric monk-check (resource machine)
  "Inspect MACHINE.  Return nil when RESOURCE is in the desired state.
Otherwise return a plist of changes that `monk-repair' must make.
The plist can hold :degree, a number from 0 to 1 that says how much
of the resource already holds.  This method must not change MACHINE.")

(cl-defgeneric monk-repair (resource machine changes)
  "Apply CHANGES from `monk-check' for RESOURCE on MACHINE.
Return a comment string or nil.")

(cl-defgeneric monk-diff (resource machine changes)
  "Return a unified diff for CHANGES of RESOURCE on MACHINE, or nil."
  (ignore resource machine changes)
  nil)

(cl-defgeneric monk-action (resource machine action)
  "Perform ACTION on RESOURCE for MACHINE after a watched ref changed.
Return a comment string.")

(cl-defmethod monk-action ((r monk-resource) (m monk-machine) action)
  "Default: signal that R does not support ACTION on M."
  (signal 'monk-resource-error
          (list (format "%s does not support action %s on %s"
                        (eieio-object-class-name r) action (oref m name)))))

(cl-defgeneric monk-default-watch-action (resource)
  "Return the action RESOURCE performs when a watched ref changed.
Nil means the resource only re-checks.")

(cl-defmethod monk-default-watch-action ((_r monk-resource))
  nil)

(defconst monk-resource-class-prefix "monk-"
  "Prefix that resource class names carry.")

(defconst monk-resource-class-suffix "-resource"
  "Suffix that resource class names carry.")

(cl-defmethod monk-resource-type ((r monk-resource))
  "Return the short type name of R: the class name without prefix and suffix."
  (string-remove-suffix monk-resource-class-suffix
                        (string-remove-prefix monk-resource-class-prefix
                                              (symbol-name (eieio-object-class-name r)))))

(cl-defmethod monk-resource-applicable-p ((r monk-resource) (m monk-machine))
  "Return non-nil when guards and classes of R allow it to run on M."
  (and (cl-every (lambda (class) (monk-class-p m class)) (oref r classes))
       (or (null (oref r only-if)) (funcall (oref r only-if) m))
       (or (null (oref r not-if)) (not (funcall (oref r not-if) m)))))

;;;; Compile phase

(defvar monk--compile-resources nil
  "Resources collected during the current compile, newest first.")

(defvar monk--compile-machine nil
  "Machine for the current compile.")

(defun monk-current-machine ()
  "Return the machine of the current compile.  Signal outside a compile."
  (or monk--compile-machine
      (signal 'monk-policy-error
              (list "Resource constructor called outside a policy"))))

(defun monk-expand (path &optional sudo)
  "Return PATH with `~' expanded on the machine of the current compile.
Use this in a policy where a resource needs a target-side absolute
path, for example a symlink target.  With SUDO, `~' is the home of
root."
  (monk-machine-expand (monk-current-machine) path sudo))

(defun monk-add-resource (resource)
  "Add RESOURCE to the current compile.  Return RESOURCE."
  (unless monk--compile-machine
    (signal 'monk-policy-error
            (list "Resource constructor called outside a policy")))
  (let ((existing (seq-find (lambda (r) (equal (oref r name) (oref resource name)))
                            monk--compile-resources)))
    (when existing
      (signal 'monk-policy-error
              (list (format "Duplicate resource name %S (%s and %s)"
                            (oref resource name)
                            (monk-resource-type existing)
                            (monk-resource-type resource))))))
  (push resource monk--compile-resources)
  resource)

(cl-defmethod monk-compile ((m monk-machine) policy)
  "Run POLICY for M.  Return the list of resources in policy order."
  (let ((monk--compile-resources nil)
        (monk--compile-machine m))
    (funcall policy m)
    (nreverse monk--compile-resources)))

(defconst monk--common-keys
  '(:only-if :not-if :classes :require :watch :watch-action :onchanges :onfail
    :sudo :ignore-errors)
  "Keywords every resource constructor accepts.")

(defun monk--common-initargs (plist)
  "Return the entries of PLIST that `monk-resource' accepts."
  (let (out)
    (dolist (key monk--common-keys)
      (when (plist-member plist key)
        (setq out (plist-put out key (plist-get plist key)))))
    out))

(defun monk--resource-plist (plist &rest own)
  "Return initargs from the common keys in PLIST plus OWN."
  (append own (monk--common-initargs plist)))

;;;; Requisites

(cl-defstruct (monk-ref (:constructor monk-ref-create)
                        (:copier nil))
  "A parsed requisite reference."
  scope      ; nil for the same machine, a symbol for a host or group
  name       ; resource name
  min        ; at least this many matching nodes must satisfy, or nil
  fraction   ; at least this fraction of matching nodes, or nil
  (degree monk-degree-satisfied))  ; per-node degree threshold

(defun monk-parse-ref (spec)
  "Return a `monk-ref' for SPEC.
SPEC is a resource name string, a cons (SCOPE . NAME), or a plist
\(:ref SPEC :min N :fraction F :degree D)."
  (cond
   ((stringp spec)
    (monk-ref-create :name spec))
   ((and (consp spec) (keywordp (car spec)))
    (let ((inner (monk-parse-ref (plist-get spec :ref))))
      (when (plist-member spec :min)
        (setf (monk-ref-min inner) (plist-get spec :min)))
      (when (plist-member spec :fraction)
        (setf (monk-ref-fraction inner) (plist-get spec :fraction)))
      (when (plist-member spec :degree)
        (setf (monk-ref-degree inner) (plist-get spec :degree)))
      inner))
   ((and (consp spec) (symbolp (car spec)) (stringp (cdr spec)))
    (monk-ref-create :scope (car spec) :name (cdr spec)))
   (t (signal 'monk-requisite-error (list "Malformed requisite" spec)))))

(cl-defmethod monk-resource-refs ((r monk-resource) kind)
  "Return the parsed refs of KIND for R."
  (mapcar #'monk-parse-ref (slot-value r kind)))

;;;; Nodes and the ledger

(cl-defstruct (monk-node (:constructor monk-node-create)
                         (:copier nil))
  "One resource on one machine inside a run."
  machine
  resource
  (index 0)
  result)

(defun monk-node-key (node)
  "Return (MACHINE-NAME . RESOURCE-NAME) for NODE."
  (cons (oref (monk-node-machine node) name)
        (oref (monk-node-resource node) name)))

(defun monk--nodes-for-ref (ref node nodes)
  "Return the nodes in NODES that REF from NODE points at."
  (let ((name (monk-ref-name ref))
        (scope (monk-ref-scope ref))
        (own (monk-node-machine node)))
    (seq-filter
     (lambda (candidate)
       (and (not (eq candidate node))
            (equal (oref (monk-node-resource candidate) name) name)
            (let ((m (monk-node-machine candidate)))
              (cond
               ((null scope) (eq m own))
               ((equal (symbol-name scope) (oref m name)) t)
               (t (monk-machine-in-group-p m scope))))))
     nodes)))

(defun monk--result-satisfies-p (result degree)
  "Return non-nil when RESULT holds to at least DEGREE."
  (and result
       (or (memq (monk-result-status result)
                 (list monk-status-kept monk-status-repaired monk-status-would-repair))
           (>= (monk-result-degree result) degree))))

(defun monk--ref-satisfied-p (ref targets)
  "Return non-nil when enough of TARGETS satisfy REF.
Without :min or :fraction, every target must satisfy."
  (let* ((total (length targets))
         (good (seq-count (lambda (n)
                            (monk--result-satisfies-p (monk-node-result n)
                                                      (monk-ref-degree ref)))
                          targets)))
    (cond
     ((zerop total) nil)
     ((monk-ref-min ref) (>= good (monk-ref-min ref)))
     ((monk-ref-fraction ref) (>= (/ (float good) total) (monk-ref-fraction ref)))
     (t (= good total)))))

(defun monk--ref-changed-p (targets)
  "Return non-nil when any of TARGETS changed in this run."
  (seq-some (lambda (n) (and (monk-node-result n)
                             (monk-result-changed-p (monk-node-result n))))
            targets))

(defun monk--ref-failed-p (targets)
  "Return non-nil when any of TARGETS failed in this run."
  (seq-some (lambda (n) (and (monk-node-result n)
                             (monk-result-failed-p (monk-node-result n))))
            targets))

(defun monk--requisite-verdict (node nodes)
  "Decide whether NODE can run given the results in NODES.
Return (VERDICT . DETAIL).  VERDICT is `run', `skip', or `watch'.
`watch' means run, and perform the watch action afterwards."
  (let ((resource (monk-node-resource node))
        (verdict 'run)
        (detail nil))
    (catch 'done
      (dolist (kind '(require watch))
        (dolist (ref (monk-resource-refs resource kind))
          (let ((targets (monk--nodes-for-ref ref node nodes)))
            (unless (monk--ref-satisfied-p ref targets)
              (throw 'done (setq verdict 'skip
                                 detail (format "%s %s not satisfied" kind (monk-ref-name ref)))))
            (when (and (eq kind 'watch) (monk--ref-changed-p targets))
              (setq verdict 'watch
                    detail (format "watch %s changed" (monk-ref-name ref)))))))
      (let ((onchanges (monk-resource-refs resource 'onchanges)))
        (when (and onchanges
                   (not (seq-some (lambda (ref)
                                    (monk--ref-changed-p (monk--nodes-for-ref ref node nodes)))
                                  onchanges)))
          (throw 'done (setq verdict 'skip detail "onchanges: nothing changed"))))
      (let ((onfail (monk-resource-refs resource 'onfail)))
        (when (and onfail
                   (not (seq-some (lambda (ref)
                                    (monk--ref-failed-p (monk--nodes-for-ref ref node nodes)))
                                  onfail)))
          (throw 'done (setq verdict 'skip detail "onfail: nothing failed")))))
    (cons verdict detail)))

;;;; Scheduling

(defun monk--node-dependencies (node nodes)
  "Return the nodes in NODES that must run before NODE.
Signal `monk-requisite-error' when a ref matches nothing or asks for
more matches than exist, so a policy mistake surfaces before any
resource runs."
  (let ((name (oref (monk-node-resource node) name))
        (deps nil))
    (dolist (kind monk-requisite-kinds)
      (dolist (ref (monk-resource-refs (monk-node-resource node) kind))
        (let ((targets (monk--nodes-for-ref ref node nodes)))
          (when (null targets)
            (signal 'monk-requisite-error
                    (list (format "%s: %s ref %S matches no resource"
                                  name kind (monk-ref-name ref)))))
          (when (and (monk-ref-min ref) (> (monk-ref-min ref) (length targets)))
            (signal 'monk-requisite-error
                    (list (format "%s: %s ref %S asks for %d of %d"
                                  name kind (monk-ref-name ref)
                                  (monk-ref-min ref) (length targets)))))
          (dolist (target targets)
            (cl-pushnew target deps)))))
    deps))

(defun monk-schedule (nodes)
  "Return NODES in an order that respects requisites.
Nodes without a dependency between them keep policy order, machine
by machine in inventory order.  Signal `monk-requisite-error' on a
cycle or a bad ref."
  (let ((remaining (copy-sequence nodes))
        (deps (make-hash-table :test #'eq))
        (done (make-hash-table :test #'eq))
        (ordered nil))
    (dolist (node nodes)
      (puthash node (monk--node-dependencies node nodes) deps))
    (while remaining
      (let ((ready (seq-find (lambda (node)
                               (cl-every (lambda (dep) (gethash dep done))
                                         (gethash node deps)))
                             remaining)))
        (unless ready
          (signal 'monk-requisite-error
                  (list "Requisite cycle among"
                        (mapcar #'monk-node-key remaining))))
        (push ready ordered)
        (puthash ready t done)
        (setq remaining (delq ready remaining))))
    (nreverse ordered)))

;;;; Running one node

(defun monk--run-node (node nodes dry-run)
  "Check and repair NODE.  Return a `monk-result'.
NODES carries the results of earlier nodes for requisites.  When
DRY-RUN is non-nil, report instead of repairing.  A fatal failure
signals `monk-resource-error' with the result."
  (let* ((resource (monk-node-resource node))
         (machine (monk-node-machine node))
         (start (float-time))
         (result (monk-result-create :machine (oref machine name)
                                     :name (oref resource name)
                                     :type (monk-resource-type resource))))
    (monk-progress 'node-start :node node)
    (condition-case err
        (pcase-let ((`(,verdict . ,detail)
                     (if (monk-resource-applicable-p resource machine)
                         (monk--requisite-verdict node nodes)
                       (cons 'skip "guard or class did not match"))))
          (if (eq verdict 'skip)
              (setf (monk-result-status result) monk-status-skipped
                    (monk-result-comment result) detail)
            (let* ((changes (monk-check resource machine))
                   (degree (or (plist-get changes :degree) 0.0)))
              (cond
               ((and (null changes) (eq verdict 'watch))
                (monk--perform-watch resource machine result detail dry-run))
               ((null changes)
                (setf (monk-result-status result) monk-status-kept
                      (monk-result-degree result) monk-degree-satisfied))
               (dry-run
                (setf (monk-result-status result) monk-status-would-repair
                      (monk-result-degree result) degree
                      (monk-result-changes result) changes
                      (monk-result-diff result)
                      (and monk-show-diffs (monk-diff resource machine changes))))
               (t
                (setf (monk-result-diff result)
                      (and monk-show-diffs (monk-diff resource machine changes))
                      (monk-result-degree result) degree)
                (setf (monk-result-comment result) (monk-repair resource machine changes)
                      (monk-result-status result) monk-status-repaired
                      (monk-result-degree result) monk-degree-satisfied
                      (monk-result-changes result) changes))))))
      (error
       (setf (monk-result-status result)
             (if (> (monk-result-degree result) 0.0) monk-status-partial monk-status-failed)
             (monk-result-comment result) (error-message-string err))
       (monk-log 'warn "%s: %s %s %s: %s"
                 (oref machine name) (monk-resource-type resource)
                 (oref resource name) (monk-result-status result)
                 (error-message-string err))
       (unless (oref resource ignore-errors)
         (setf (monk-result-duration result) (- (float-time) start)
               (monk-node-result node) result)
         (monk-progress 'node-end :node node :result result)
         (signal 'monk-resource-error (list result)))))
    (setf (monk-result-duration result) (- (float-time) start)
          (monk-node-result node) result)
    (monk-progress 'node-end :node node :result result)
    result))

(defun monk--perform-watch (resource machine result detail dry-run)
  "Run the watch action of RESOURCE on MACHINE and fill RESULT.
DETAIL names the changed ref.  With DRY-RUN, report only."
  (let ((action (or (oref resource watch-action)
                    (monk-default-watch-action resource))))
    (cond
     ((null action)
      (setf (monk-result-status result) monk-status-kept
            (monk-result-degree result) monk-degree-satisfied
            (monk-result-comment result) detail))
     (dry-run
      (setf (monk-result-status result) monk-status-would-repair
            (monk-result-comment result) (format "%s: would %s" detail action)))
     (t
      (setf (monk-result-comment result)
            (format "%s: %s" detail (monk-action resource machine action))
            (monk-result-status result) monk-status-repaired
            (monk-result-degree result) monk-degree-satisfied)))))

;;;; Passes

(cl-defstruct (monk-pass (:constructor monk-pass-create)
                         (:copier nil))
  "Outcome of one pass over all nodes."
  results
  (repaired 0)
  (repaired-keys nil)
  stopped)

(defun monk--pass (nodes dry-run)
  "Run one pass over NODES in order.  Return a `monk-pass'.
Results from earlier passes are cleared first so requisites read this
pass only.  When DRY-RUN is non-nil, report only."
  (dolist (node nodes)
    (setf (monk-node-result node) nil))
  (let (results (repaired 0) (repaired-keys nil) (stopped nil))
    (catch 'monk-stop
      (dolist (node nodes)
        (let ((result (condition-case err
                          (monk--run-node node nodes dry-run)
                        (monk-resource-error
                         (push (cadr err) results)
                         (setq stopped t)
                         (throw 'monk-stop nil)))))
          (push result results)
          (when (monk-result-changed-p result)
            (cl-incf repaired)
            (push (monk-result-key result) repaired-keys)))))
    (monk-pass-create :results (nreverse results)
                      :repaired repaired
                      :repaired-keys (nreverse repaired-keys)
                      :stopped stopped)))

(defun monk--merge-pass-results (previous current)
  "Merge CURRENT pass results into PREVIOUS.  Return the new list.
A repair from an earlier pass stays unless the later pass failed the
same resource.  A skip never replaces a status that was earned.  Any
other earlier status yields to the later one, so a failure that
resolves to kept reads as kept."
  (if (null previous)
      current
    (let ((merged (copy-sequence previous)))
      (dolist (result current)
        (let ((earlier (seq-find (lambda (r) (equal (monk-result-key r)
                                                    (monk-result-key result)))
                                 merged)))
          (cond
           ((null earlier)
            (setq merged (append merged (list result))))
           ((monk-result-changed-p earlier)
            (when (monk-result-failed-p result)
              (setcar (memq earlier merged) result)))
           ;; A node that ran once and is skipped later, because its
           ;; onchanges target is now kept, keeps the status it earned.
           ((and (eq (monk-result-status result) monk-status-skipped)
                 (not (eq (monk-result-status earlier) monk-status-skipped)))
            nil)
           (t (setcar (memq earlier merged) result)))))
      merged)))

;;;; Runs

(defun monk--run-nodes (machines nodes dry-run max-passes)
  "Run NODES for MACHINES until nothing changes.  Return a `monk-run'."
  (let ((run (monk-run-create :machines machines :dry-run dry-run :passes 0))
        (ordered (monk-schedule nodes))
        (all-results nil)
        (pass 0)
        (continue t))
    (monk-progress 'run-start :run run :nodes ordered)
    ;; A quit or an error in a hook must still send run-end, or a
    ;; display keeps its spinner and its live state.
    (unwind-protect
        (while (and continue (< pass max-passes))
          (cl-incf pass)
          (let* ((outcome (monk--pass ordered dry-run))
                 (repaired (monk-pass-repaired outcome)))
            (setq all-results (monk--merge-pass-results all-results
                                                        (monk-pass-results outcome)))
            (setf (monk-run-last-pass-repaired run)
                  (if (monk-pass-stopped outcome) 0 repaired)
                  (monk-run-passes run) pass
                  (monk-run-results run) all-results)
            (push (monk-pass-repaired-keys outcome) (monk-run-repaired-by-pass run))
            (monk-log 'info "pass %d over %d node(s) repaired %d%s" pass (length ordered)
                      repaired (if (monk-pass-stopped outcome) ", stopped on failure" ""))
            (monk-progress 'pass-end :run run :pass pass :repaired repaired)
            ;; A resource that repaired in two passes in a row will
            ;; repair in every pass.  Stop now instead of at the cap.
            (setq continue (and (not dry-run)
                                (not (monk-pass-stopped outcome))
                                (> repaired 0)
                                (not (monk-run-flapping-p run))))))
      (setf (monk-run-end-time run) (current-time))
      (monk-progress 'run-end :run run))
    (when (monk-run-flapping-p run)
      (monk-log 'warn "flapping: %s"
                (mapconcat (lambda (key) (format "%s:%s" (car key) (cdr key)))
                           (monk-run-flapping run) ", ")))
    (when (and (monk-run-oscillating-p run) (not (monk-run-flapping-p run)))
      (monk-log 'warn "did not converge after %d passes" pass))
    (monk-log 'info "run end: %s" (monk-run-summary run))
    run))

(defun monk--nodes-for (machines policy)
  "Gather facts, compile POLICY, and return nodes for MACHINES."
  (let ((nodes nil) (index 0))
    (dolist (m machines)
      (unless (oref m facts)
        (monk-gather-facts m))
      (monk-log 'info "%s: classes: %s" (oref m name)
                (mapconcat #'symbol-name (oref m classes) " "))
      (dolist (resource (monk-compile m policy))
        (push (monk-node-create :machine m :resource resource :index index) nodes)
        (cl-incf index)))
    (nreverse nodes)))

(defun monk--run-options (options)
  "Return (DRY-RUN . MAX-PASSES) from the OPTIONS plist."
  (cons (if (plist-member options :dry-run) (plist-get options :dry-run) monk-dry-run)
        (or (plist-get options :passes) monk-max-passes)))

(cl-defgeneric monk-converge (machine policy &rest options)
  "Compile POLICY for MACHINE and apply it until nothing changes.
OPTIONS is a plist.  :dry-run overrides `monk-dry-run'.  :passes
overrides `monk-max-passes'.  Return a `monk-run'.")

(cl-defmethod monk-converge ((m monk-machine) policy &rest options)
  (pcase-let ((`(,dry-run . ,max-passes) (monk--run-options options)))
    (monk-log 'info "%s: converge start%s" (oref m name) (if dry-run " (dry run)" ""))
    (monk--run-nodes (list m) (monk--nodes-for (list m) policy) dry-run max-passes)))

(defun monk-orchestrate (machines policy &rest options)
  "Apply POLICY to MACHINES as one dependency graph.
Requisites that point at other machines or groups resolve inside this
run.  OPTIONS are as for `monk-converge'.  Return a `monk-run'."
  (pcase-let ((`(,dry-run . ,max-passes) (monk--run-options options)))
    (monk-log 'info "orchestrate %d machine(s)%s" (length machines)
              (if dry-run " (dry run)" ""))
    (monk--run-nodes machines (monk--nodes-for machines policy) dry-run max-passes)))

;;;; Text reporting

(defun monk-run-summary (run &optional machine)
  "Return a one-line summary of RUN, or of MACHINE within it."
  (format "%d kept, %d repaired, %d would repair, %d partial, %d failed, %d skipped, %d pass%s"
          (monk-run-count run monk-status-kept machine)
          (monk-run-count run monk-status-repaired machine)
          (monk-run-count run monk-status-would-repair machine)
          (monk-run-count run monk-status-partial machine)
          (monk-run-count run monk-status-failed machine)
          (monk-run-count run monk-status-skipped machine)
          (monk-run-passes run)
          (if (= 1 (monk-run-passes run)) "" "es")))

(defun monk-run-report-string (run &optional verbose)
  "Return a text report for RUN.
Without VERBOSE, omit kept and skipped results."
  (with-temp-buffer
    (dolist (m (monk-run-machines run))
      (insert (format "== %s%s: %s\n"
                      (oref m name)
                      (if (monk-run-dry-run run) " (dry run)" "")
                      (monk-run-summary run (oref m name)))))
    (when (monk-run-flapping-p run)
      (insert (format "!! flapping: %s\n"
                      (mapconcat (lambda (key) (format "%s:%s" (car key) (cdr key)))
                                 (monk-run-flapping run) ", "))))
    (when (and (monk-run-oscillating-p run) (not (monk-run-flapping-p run)))
      (insert (format "!! did not converge: %d resource(s) still repaired in pass %d\n"
                      (monk-run-last-pass-repaired run) (monk-run-passes run))))
    (let ((many (cdr (monk-run-machines run))))
      (dolist (result (monk-run-results run))
        (let ((status (monk-result-status result)))
          (when (or verbose
                    (not (memq status (list monk-status-kept monk-status-skipped))))
            (insert (format "%-13s %-14s %s%s" status
                            (monk-result-type result)
                            (if many (concat (monk-result-machine result) ":") "")
                            (monk-result-name result)))
            (when (eq status monk-status-partial)
              (insert (format " [%d%%]" (round (* 100 (monk-result-degree result))))))
            (when (monk-result-comment result)
              (insert ": " (monk-result-comment result)))
            (insert "\n")
            (when (and (monk-result-changes result) (not (monk-result-diff result)))
              (insert (format "              %S\n" (monk-result-changes result))))
            (when (monk-result-diff result)
              (insert (monk-result-diff result))
              (unless (string-suffix-p "\n" (monk-result-diff result))
                (insert "\n")))))))
    (buffer-string)))

(defvar monk-report-function #'monk-run-report-text
  "Function that shows a finished run.  Called with RUN and VERBOSE.
`monk-ui' sets this to its graphical report.")

(defun monk-run-report-text (run &optional verbose)
  "Print the text report for RUN.  With VERBOSE, include kept and skipped."
  (let ((text (monk-run-report-string run verbose)))
    (if noninteractive
        (princ text)
      (with-current-buffer (get-buffer-create "*monk-report*")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert text))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun monk-run-report (run &optional verbose)
  "Show a report for RUN through `monk-report-function'.  Return RUN.
VERBOSE passes through to the report function."
  (funcall monk-report-function run verbose)
  run)

;;;; Batch entry point

(defun monk-batch-converge (policy-file &rest machine-args)
  "Load POLICY-FILE and converge the local machine.
POLICY-FILE must set `monk-batch-policy' to a policy function.
MACHINE-ARGS are passed to `monk-local-machine'.  Exit non-zero when
any resource failed."
  (load policy-file nil t)
  (let* ((policy (or (bound-and-true-p monk-batch-policy)
                     (signal 'monk-policy-error
                             (list "monk-batch-policy is not set by" policy-file))))
         (run (monk-converge (apply #'monk-local-machine machine-args) policy)))
    (monk-run-report run)
    (unless (monk-run-ok-p run)
      (kill-emacs 1))
    run))

(provide 'monk)
;;; monk.el ends here
