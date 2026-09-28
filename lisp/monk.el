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
;;   :when GUARD      run only when GUARD is true.  GUARD is a CELL form
;;                    such as (and (class linux) (= (fact arch) "x86_64"))
;;                    or a CEL string such as "'linux' in classes".
;;                    See monk-cell.el.  A skipped resource reports the
;;                    clause that was false.
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
(require 'xref)

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

(defcustom monk-max-passes 6
  "Upper bound on convergence passes in one run.
A repair that holds costs one extra pass to confirm.  A flapping cycle
of N resources repeats a resource after N passes, so the bound also
limits the cycle length Monk can name: with 6 passes a cycle of up to
5 resources is caught."
  :type 'natnum)

(defcustom monk-retry-defaults '(:attempts 2 :interval 30 :backoff 1.0 :splay 0 :until success)
  "Defaults for a resource :retry plist.
:attempts is the total number of tries.  :interval is the wait in
seconds before try two.  Each later wait is the previous one times
:backoff.  :splay adds a random 0..SPLAY seconds to each wait.
:until is `success' (stop on kept or repaired) or `failure' (stop on
the first failure, for a resource expected to fail).  Salt's defaults,
plus :backoff."
  :type 'plist)

(defcustom monk-retry-max-interval 300
  "Longest wait between two tries of one resource, in seconds.
The run is synchronous, so the wait blocks Emacs.  `sit-for' keeps
redisplay and `C-g' working."
  :type 'number)

(defconst monk-tramp-method "ssh"
  "TRAMP method for remote machines.")

(defconst monk-tramp-sudo-method "sudo"
  "TRAMP method used as the second hop for privileged operations.")

(defconst monk-tramp-sudo-local-host "root@localhost"
  "User and host for a sudo hop on the local machine.
Written out rather than left empty: `/sudo::' makes TRAMP fill in the
system name, and when that name is not lower case, as on macOS, the
default-host match fails and TRAMP opens an ssh connection to the
local machine.  Seen as \"Opening connection for BOSMEJ... using ssh\".")

(defconst monk-log-buffer-name "*monk-log*"
  "Buffer that receives log lines.")

(defconst monk-requisite-kinds '(require watch onchanges onfail prereq listen)
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

(defcustom monk-result-functions nil
  "Functions called with each finished `monk-result' and its run.
Salt's returner hook, per result.  Called after `node-end' progress,
inside the run.  An error in a function is logged and ignored."
  :type 'hook)

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
(define-error 'monk-guard-error "Monk guard error" 'monk-error)
(define-error 'monk-cel-error "Monk CEL error" 'monk-guard-error)

;; monk-cell.el evaluates :when guards.  It declares the machine
;; accessors it needs instead of requiring monk, so the load order is
;; monk-cell first, then the rest of this file.
(require 'monk-cell)

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
  (duration 0.0)
  origin)               ; (FILE . NAME): where the resource was defined

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
  policy
  options
  plan
  reasons               ; alist of (KEY . TEXT): why a watch selected KEY
  (context (make-hash-table :test #'equal))  ; per-run cache, see `monk-run-context-get'
  (mock nil)            ; compile and order only; nothing is checked
  (rescued nil)         ; keys whose failure an :onfail handler with :handled t covered
  (last-pass-repaired 0)
  (repaired-by-pass nil)
  (start-time (current-time))
  end-time)

(defun monk-run-flapping (run)
  "Return the keys of resources in RUN that repaired in more than one pass.
A repair that held is kept in every later pass, so a second repair of
the same resource means something undid the first.  The passes need
not be adjacent: with A undoing B, B undoing C, and C undoing A, each
one repairs every third pass.  The classic two-resource case is one
that installs a package and one that removes it.  Each key is
\(MACHINE-NAME . RESOURCE-NAME).  Nil means no flapping."
  (let ((seen (make-hash-table :test #'equal))
        (flapping nil))
    (dolist (keys (reverse (monk-run-repaired-by-pass run)))
      (dolist (key keys)
        (when (and (gethash key seen) (not (member key flapping)))
          (push key flapping))
        (puthash key t seen)))
    (nreverse flapping)))

(defun monk-run-flapping-p (run)
  "Return non-nil when RUN has resources that flap.  See `monk-run-flapping'."
  (and (monk-run-flapping run) t))

(defun monk-run-machine (run)
  "Return the first machine of RUN.  Single-machine runs have one."
  (car (monk-run-machines run)))

(defun monk-run-result (run key)
  "Return the result in RUN for KEY, a (MACHINE-NAME . RESOURCE-NAME), or nil."
  (seq-find (lambda (r) (equal (monk-result-key r) key)) (monk-run-results run)))

;;;; Plan comparison

(defconst monk-plan-as-planned 'as-planned
  "Apply did what the plan said: both changed, or both left alone.")
(defconst monk-plan-unplanned 'unplanned
  "Apply changed a resource the plan said was kept.")
(defconst monk-plan-unfulfilled 'unfulfilled
  "The plan said the resource would change; apply left it kept.")
(defconst monk-plan-failed 'failed-planned
  "Apply failed on a resource that the plan said would succeed.")
(defconst monk-plan-new 'new
  "Apply saw a resource the plan did not have.")
(defconst monk-plan-missing 'missing
  "The plan had a resource that apply did not see.")

(defconst monk-plan-verdicts
  (list monk-plan-as-planned monk-plan-unplanned monk-plan-unfulfilled
        monk-plan-failed monk-plan-new monk-plan-missing)
  "All plan comparison verdicts, in display order.")

(defun monk--plan-verdict (planned actual)
  "Return the verdict for one resource from PLANNED and ACTUAL results."
  (cond
   ((null planned) monk-plan-new)
   ((null actual) monk-plan-missing)
   ;; A failure the plan already showed is not news.
   ((and (monk-result-failed-p planned) (monk-result-failed-p actual)) monk-plan-as-planned)
   ((monk-result-failed-p actual) monk-plan-failed)
   ((and (monk-result-changed-p planned) (monk-result-changed-p actual)) monk-plan-as-planned)
   ((monk-result-changed-p planned) monk-plan-unfulfilled)
   ((monk-result-changed-p actual) monk-plan-unplanned)
   (t monk-plan-as-planned)))

(defun monk-run-compare (plan apply)
  "Compare the dry run PLAN with the real run APPLY.
Return an alist of (KEY . VERDICT) in APPLY order, then plan-only
keys.  KEY is (MACHINE-NAME . RESOURCE-NAME).  VERDICT is one of
`monk-plan-verdicts'.  An apply that matches its plan has only
`as-planned' entries; anything else is worth a look, because the
machine changed between plan and apply, or a check and its repair
disagree."
  (let ((verdicts nil)
        (seen nil))
    (dolist (actual (monk-run-results apply))
      (let ((key (monk-result-key actual)))
        (push key seen)
        (push (cons key (monk--plan-verdict (monk-run-result plan key) actual)) verdicts)))
    (dolist (planned (monk-run-results plan))
      (unless (member (monk-result-key planned) seen)
        (push (cons (monk-result-key planned) monk-plan-missing) verdicts)))
    (nreverse verdicts)))

(defun monk-run-compare-counts (comparison)
  "Return an alist of (VERDICT . N) for COMPARISON from `monk-run-compare'."
  (let (counts)
    (dolist (cell comparison)
      (cl-incf (alist-get (cdr cell) counts 0)))
    counts))

(defun monk-run-as-planned-p (apply)
  "Return non-nil when APPLY has a plan and every verdict is as-planned."
  (and (monk-run-plan apply)
       (cl-every (lambda (cell) (eq (cdr cell) monk-plan-as-planned))
                 (monk-run-compare (monk-run-plan apply) apply))))

(defun monk-run-count (run status &optional machine)
  "Return how many results in RUN have STATUS.
With MACHINE, a name string, count only that machine."
  (seq-count (lambda (r)
               (and (eq (monk-result-status r) status)
                    (or (null machine) (equal (monk-result-machine r) machine))))
             (monk-run-results run)))

(defun monk-run-ok-p (run)
  "Return non-nil when no result in RUN failed.
A failure that an :onfail handler with :handled t covered is rescued
and does not count.  See `monk-run-rescued'."
  (not (seq-some (lambda (r)
                   (and (monk-result-failed-p r)
                        (not (member (monk-result-key r) (monk-run-rescued run)))))
                 (monk-run-results run))))

;;;; Run context

(defvar monk--current-run nil
  "The run in progress, for `monk-run-context-get' inside resources.")

(defun monk-run-context-get (key &optional run)
  "Return the value cached under KEY in RUN, or nil.
RUN defaults to the run in progress.  KEY is any `equal'-comparable
value; resources use (MACHINE-NAME . TAG).  Salt's `__context__'."
  (let ((run (or run monk--current-run)))
    (and run (gethash key (monk-run-context run)))))

(defun monk-run-context-put (key value &optional run)
  "Cache VALUE under KEY in RUN, default the run in progress.  Return VALUE."
  (let ((run (or run monk--current-run)))
    (when run (puthash key value (monk-run-context run)))
    value))

(defun monk-run-context-clear (key &optional run)
  "Forget KEY in RUN, default the run in progress."
  (let ((run (or run monk--current-run)))
    (when run (remhash key (monk-run-context run)))))

(defmacro monk-with-run-cache (key &rest body)
  "Return the value cached under KEY in the current run, or compute it with BODY.
Outside a run, BODY runs every time."
  (declare (indent 1))
  (let ((k (make-symbol "key")) (v (make-symbol "value")))
    `(let* ((,k ,key)
            (,v (monk-run-context-get ,k)))
       (or ,v (monk-run-context-put ,k (progn ,@body))))))

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
            :documentation "List of class symbols.  Filled by `monk-gather-facts'.")
   (origin :initarg :origin
           :initform nil
           :documentation "Where the inventory defined this machine: (FILE . NAME)."))
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
    (format "/%s:%s:" monk-tramp-sudo-method monk-tramp-sudo-local-host))
   (t
    (let ((host (concat (and (oref m user) (concat (oref m user) "@"))
                        (oref m address)
                        (and (oref m port) (format "#%s" (oref m port))))))
      (if sudo
          (format "/%s:%s|%s::" monk-tramp-method host monk-tramp-sudo-method)
        (format "/%s:%s:" monk-tramp-method host))))))

(cl-defmethod monk-machine-path ((m monk-machine) path &optional sudo)
  "Return PATH as seen from the controller for machine M.
Expand `~' on the target, not on the controller.  On the local host
without SUDO, a leading `~/' expands to the home fact when it is set,
so a policy can run against another home; otherwise to $HOME."
  (let ((prefix (monk-machine-remote-prefix m sudo)))
    (cond
     ((not (string-empty-p prefix)) (concat prefix path))
     ((and (string-prefix-p "~/" path) (monk-machine-fact m 'home))
      (expand-file-name (substring path 2) (monk-machine-fact m 'home)))
     (t (expand-file-name path)))))

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
        ;; The home fact, when known, beats $HOME: a test drive or an
        ;; agent may run the local machine under a different home.
        (file-name-as-directory (or (monk-machine-fact m 'home) (expand-file-name "~")))
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
    (let ((default-directory (cond
                              (directory
                               (monk-machine-path m (file-name-as-directory directory) sudo))
                              ;; A local command needs a directory that
                              ;; exists; the machine's home may not yet.
                              ((and (monk-machine-local-p m) (not sudo)
                                    (not (file-directory-p (monk-machine-default-directory m))))
                               (file-name-as-directory (expand-file-name temporary-file-directory)))
                              (t (monk-machine-default-directory m sudo))))
          (process-file-side-effects t))
      (monk-log 'debug "%s: exec %s %s" (oref m name) program
                (mapconcat #'shell-quote-argument args " "))
      (with-temp-buffer
        (let ((code
               (condition-case err
                   (if input
                       (let ((stdin (make-temp-file "monk-stdin")))
                         (unwind-protect
                             (progn
                               (with-temp-file stdin (insert input))
                               (apply #'process-file program stdin t nil args))
                           (delete-file stdin)))
                     (apply #'process-file program nil t nil args))
                 ;; A missing program is an exit like any other, code
                 ;; 127 as the shell would say, so callers that test
                 ;; `monk-exec-ok-p' need no separate error path.
                 (file-missing
                  (insert (error-message-string err))
                  127))))
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
  "Register FUNCTION as the fact probe NAME.
A new probe runs after the existing ones, so a probe can read the
facts of earlier probes: the os probe needs `kernel' from uname.
Registering NAME again replaces the function in place."
  (if (assq name monk-fact-probes)
      (setf (alist-get name monk-fact-probes) function)
    (setq monk-fact-probes (append monk-fact-probes (list (cons name function))))))

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
  "Return home and user facts for M.
The local host answers from Emacs itself.  A remote host needs one
command, because TRAMP has no primitive for the login name or home of
the remote user."
  (if (monk-machine-local-p m)
      (list (cons 'home (directory-file-name (expand-file-name "~")))
            (cons 'user (user-login-name)))
    (list (cons 'home (monk-exec-string m "sh" "-c" "echo $HOME"))
          (cons 'user (monk-exec-string m "id" "-un")))))

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

(cl-defmethod monk-machine-describe ((m monk-machine))
  "Return an alist that shows what guards can see on M.
Keys are `classes' (list), `groups' (list), `facts' (alist), and
`vars' (alist).  Facts and vars are sorted by key."
  (let ((by-key (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
    (list (cons 'classes (oref m classes))
          (cons 'groups (oref m groups))
          (cons 'facts (sort (copy-sequence (oref m facts)) by-key))
          (cons 'vars (sort (copy-sequence (oref m vars)) by-key)))))

(defun monk-machine-describe-string (m &optional indent)
  "Return `monk-machine-describe' of M as text, each line after INDENT."
  (let ((indent (or indent ""))
        (description (monk-machine-describe m)))
    (with-temp-buffer
      (insert indent (format "classes: %s\n"
                             (mapconcat #'symbol-name (alist-get 'classes description) " ")))
      (insert indent (format "groups:  %s\n"
                             (mapconcat #'symbol-name (alist-get 'groups description) " ")))
      (dolist (section '(facts vars))
        (insert indent (format "%s:\n" section))
        (dolist (cell (alist-get section description))
          (insert indent (format "  %-22s %S\n" (car cell) (cdr cell)))))
      (buffer-string))))

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
   (origin :initarg :origin
           :initform nil
           :documentation "Where the policy defined this resource: (FILE . NAME).
Filled by `monk-add-resource'.  Nil when the policy came from no file.")
   (only-if :initarg :only-if
            :initform nil
            :documentation "Guard.  Function of the machine.  Run when non-nil.")
   (not-if :initarg :not-if
           :initform nil
           :documentation "Guard.  Function of the machine.  Skip when non-nil.")
   (classes :initarg :classes
            :initform nil
            :documentation "Run only when the machine has all these classes.
Sugar for a :when guard of (and (class A) (class B) ...).")
   (when :initarg :when
         :initform nil
         :documentation "Guard.  A CELL form or a CEL string.  Run when true.
See `monk-cell-eval'.  Checked at compile time.")
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
           :documentation "Refs.  Run only when one of them failed in this run.")
   (prereq :initarg :prereq
           :initform nil
           :documentation "Refs.  Run before them, and only when one of them would change.
The refs are checked, not repaired, to decide.  Salt's prereq.")
   (listen :initarg :listen
           :initform nil
           :documentation "Refs like `watch', but the action runs once, after the run
converged, however many of the refs changed in however many passes.
Salt's listen.")
   (retry :initarg :retry
          :initform nil
          :documentation "Plist (:attempts N :interval S :backoff F :splay S :until STATUS).
A failed check or repair is tried again up to N times in the same pass.
See `monk-retry-defaults'.")
   (order :initarg :order
          :initform nil
          :documentation "`first' or `last': a tie-breaker among nodes the graph
does not order.  Requisites always win.")
   (fatal :initarg :fatal
          :initform nil
          :documentation "When non-nil, a failure of this resource ends the pass on its
machine.  Other machines go on.  Salt's failhard, per resource, off by default.")
   (handled :initarg :handled
            :initform nil
            :documentation "On an :onfail handler: when the handler succeeds, the failures
it handles count as rescued, not against the run.")
   (sudo :initarg :sudo
         :initform nil
         :documentation "Perform the resource as root."))
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

(cl-defgeneric monk-resource-paths (resource)
  "Return the target-side paths that RESOURCE owns, or nil.
A watcher uses these to map a filesystem event back to the resource.
Paths keep a leading `~'; the watcher expands them per machine.")

(cl-defmethod monk-resource-paths ((_r monk-resource))
  nil)

(cl-defgeneric monk-resource-owns-subtree-p (resource)
  "Return non-nil when a change under a path of RESOURCE is drift for it.
True for a directory: a file created inside it can change its mode
or ownership expectations.  Nil for a file.")

(cl-defmethod monk-resource-owns-subtree-p ((_r monk-resource))
  nil)

(cl-defgeneric monk-default-watch-action (resource)
  "Return the action RESOURCE performs when a watched ref changed.
Nil means the resource only re-checks.")

(cl-defmethod monk-default-watch-action ((_r monk-resource))
  nil)

(cl-defgeneric monk-resource-available-p (resource machine)
  "Return nil when RESOURCE can run on MACHINE, else a reason string.
Salt's `__virtual__': a resource type says why it cannot work here,
for example \"brew not found\", and the run skips it with that reason
instead of failing on the first command.")

(cl-defmethod monk-resource-available-p ((_r monk-resource) (_m monk-machine))
  nil)

(defun monk-program-available-p (m program)
  "Return nil when PROGRAM is on the PATH of M, else a reason string.
Cached per run and machine."
  (let* ((key (list 'program (oref m name) program))
         (state (or (monk-run-context-get key)
                    (monk-run-context-put
                     key (if (monk-exec-ok-p (monk-exec m "sh" "-c" (concat "command -v " program)))
                             'yes 'no)))))
    (and (eq state 'no) (format "%s is not on this machine" program))))


(cl-defgeneric monk-resource-class-init (resource machine)
  "Run once per run on MACHINE before the first RESOURCE of its class.
Salt's `mod_init': `apt-get update' once, not once per resource.
Return a comment string or nil.  Runs only when a resource of the
class is selected and applicable.")

(cl-defmethod monk-resource-class-init ((_r monk-resource) (_m monk-machine))
  nil)

(cl-defgeneric monk-resource-aggregate (resource others machine)
  "Return a resource that stands for RESOURCE and OTHERS together, or nil.
Salt's `mod_aggregate': same class, same MACHINE, no dependency path
between them.  The aggregate is checked and repaired once; its
changes are split back per member by `monk-resource-disaggregate'.
Nil means this class does not aggregate.")

(cl-defmethod monk-resource-aggregate ((_r monk-resource) _others (_m monk-machine))
  nil)

(cl-defgeneric monk-resource-disaggregate (aggregate member changes)
  "Return the part of CHANGES of AGGREGATE that belongs to MEMBER, or nil.")

(cl-defmethod monk-resource-disaggregate ((_a monk-resource) (_member monk-resource) changes)
  changes)

(cl-defgeneric monk-resource-verify (resource machine)
  "After a repair of RESOURCE on MACHINE, return nil when it holds, else a reason.
Salt's `check_cmd'.  A resource whose `monk-check' is a real check
needs none; the next pass re-checks it.  The default returns nil.")

(cl-defmethod monk-resource-verify ((_r monk-resource) (_m monk-machine))
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

(cl-defmethod monk-resource-guard ((r monk-resource))
  "Return the full CELL guard of R: its :classes and :when combined.
Nil when R has neither."
  (let ((parts (append (mapcar (lambda (c) (list 'class c)) (oref r classes))
                       (and (oref r when) (list (monk-cell-guard (oref r when)))))))
    (pcase parts
      ('nil nil)
      (`(,single) single)
      (_ (cons 'and parts)))))

(defconst monk-skip-only-if ":only-if returned nil"
  "Skip comment when the :only-if function said no.")

(defconst monk-skip-not-if ":not-if returned non-nil"
  "Skip comment when the :not-if function said no.")

(cl-defmethod monk-resource-skip-reason ((r monk-resource) (m monk-machine))
  "Return nil when R can run on M, else a string that says which guard failed.
The CELL guard is explained down to the false clause.  The :only-if
and :not-if functions are opaque, so their reason only names them."
  (let ((guard (monk-resource-guard r)))
    (cond
     ((and guard (not (monk-cell-eval m guard)))
      (monk-cell-explain m guard))
     ((and (oref r only-if) (not (funcall (oref r only-if) m)))
      monk-skip-only-if)
     ((and (oref r not-if) (funcall (oref r not-if) m))
      monk-skip-not-if))))

(cl-defmethod monk-resource-applicable-p ((r monk-resource) (m monk-machine))
  "Return non-nil when guards and classes of R allow it to run on M."
  (null (monk-resource-skip-reason r m)))

;;;; Compile phase

(defvar monk--compile-resources nil
  "Resources collected during the current compile, newest first.")

(defvar monk--compile-machine nil
  "Machine for the current compile.")

(defvar monk--compile-file nil
  "File that defines the policy of the current compile, or nil.")

(defun monk-definition-file (function)
  "Return the file that defines FUNCTION, or nil.
A named function is looked up in `load-history'.  Anything else, such
as a lambda, is credited to the file being loaded or visited when the
compile started, because that is where a policy lambda is written."
  (or (and (symbolp function)
           (let ((file (symbol-file function 'defun)))
             (and file (file-name-sans-extension file))))
      load-file-name
      (and (buffer-file-name) (buffer-file-name))))

(defun monk-origin-file (origin)
  "Return the source file for ORIGIN, with the .el extension present.
ORIGIN is (FILE . NAME).  A FILE without extension gets .el, and a
compiled .elc turns into its .el."
  (when (car origin)
    (let ((file (car origin)))
      (cond ((string-suffix-p ".elc" file) (concat (file-name-sans-extension file) ".el"))
            ((string-suffix-p ".el" file) file)
            (t (concat file ".el"))))))

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
  (unless (oref resource origin)
    (oset resource origin (cons monk--compile-file (oref resource name))))
  (unless (memq (oref resource order) '(nil first last))
    (signal 'monk-policy-error
            (list (format "Resource %S: :order must be first or last, not %S"
                          (oref resource name) (oref resource order)))))
  ;; A bad guard fails the compile, not the run: the policy author sees
  ;; the error at once, and a CEL string is parsed once, here.
  (when (oref resource when)
    (condition-case err
        (oset resource when (monk-cell-guard (oref resource when)))
      (monk-guard-error
       (signal 'monk-policy-error
               (list (format "Resource %S: bad :when guard: %s"
                             (oref resource name) (cadr err)))))))
  (push resource monk--compile-resources)
  resource)

(cl-defmethod monk-compile ((m monk-machine) policy)
  "Run POLICY for M.  Return the list of resources in policy order."
  (let ((monk--compile-resources nil)
        (monk--compile-machine m)
        (monk--compile-file (monk-definition-file policy)))
    (funcall policy m)
    (nreverse monk--compile-resources)))

(defconst monk--common-keys
  '(:only-if :not-if :classes :when :require :watch :watch-action :onchanges :onfail
    :prereq :listen :retry :order :fatal :handled :sudo)
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
  (selected t)
  result)

(defconst monk-comment-mocked "not called, mocked"
  "Comment on every result of a mock run.")

(defconst monk-skip-not-selected "not selected for this run"
  "Comment on a node that a partial run left alone.")

(defun monk--node-dependents (node nodes)
  "Return the nodes in NODES that name NODE in a requisite."
  (seq-filter (lambda (other)
                (and (not (eq other node))
                     (memq node (monk--node-dependencies other nodes))))
              nodes))

(defun monk-select-nodes (nodes keys)
  "Mark as selected the nodes in NODES whose key is in KEYS, and their dependents.
Dependents close transitively: a resource that watches a changed file
must run, and so must whatever watches it.  Every other node is
deselected and will be skipped.  Return the selected nodes."
  (let ((selected nil)
        (frontier (seq-filter (lambda (n) (member (monk-node-key n) keys)) nodes)))
    (while frontier
      (let ((node (pop frontier)))
        (unless (memq node selected)
          (push node selected)
          (setq frontier (append frontier (monk--node-dependents node nodes))))))
    (dolist (node nodes)
      (setf (monk-node-selected node) (and (memq node selected) t)))
    (seq-filter #'monk-node-selected nodes)))

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
  "Return non-nil when RESULT holds to at least DEGREE.
A node left out of a partial run counts as satisfied: the run that
selected the changed resources did not select this one because
nothing it owns changed."
  (and result
       (or (memq (monk-result-status result)
                 (list monk-status-kept monk-status-repaired monk-status-would-repair))
           (equal (monk-result-comment result) monk-skip-not-selected)
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

(defconst monk-skip-prereq "prereq: nothing would change"
  "Skip comment when no prereq target would change.")

(defun monk--prereq-would-change-p (ref targets)
  "Return non-nil when a node in TARGETS of prereq REF would change now.
The targets run later, so their results are not in yet.  Their
`monk-check' runs here: it is side-effect free by contract.  A target
that is itself skipped by its own guards or requisites does not
count, which is Salt's #68438 rule.  A check that signals counts as
a change: the target will fail, and the prerequiring resource is
better run than skipped."
  (ignore ref)
  (seq-some (lambda (target)
              (let ((r (monk-node-resource target))
                    (m (monk-node-machine target)))
                (and (monk-node-selected target)
                     (not (monk-resource-skip-reason r m))
                     (condition-case nil
                         (monk-check r m)
                       (error t)))))
            targets))

(defun monk--requisite-verdict (node nodes)
  "Decide whether NODE can run given the results in NODES.
Return (VERDICT . DETAIL).  VERDICT is `run', `skip', or `watch'.
`watch' means run, and perform the watch action afterwards.
`listen' refs gate like `require' but never trigger here; their
action runs once after the run converged."
  (let ((resource (monk-node-resource node))
        (verdict 'run)
        (detail nil))
    (catch 'done
      (dolist (kind '(require watch listen))
        (dolist (ref (monk-resource-refs resource kind))
          (let ((targets (monk--nodes-for-ref ref node nodes)))
            (unless (monk--ref-satisfied-p ref targets)
              (throw 'done (setq verdict 'skip
                                 detail (format "%s %s not satisfied" kind (monk-ref-name ref)))))
            (when (and (eq kind 'watch) (monk--ref-changed-p targets))
              (setq verdict 'watch
                    detail (format "watch %s changed" (monk-ref-name ref)))))))
      (let ((prereq (monk-resource-refs resource 'prereq)))
        (when (and prereq
                   (not (seq-some (lambda (ref)
                                    (monk--prereq-would-change-p
                                     ref (monk--nodes-for-ref ref node nodes)))
                                  prereq)))
          (throw 'done (setq verdict 'skip detail monk-skip-prereq))))
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
          ;; prereq points the other way: this node runs before its targets.
          (unless (eq kind 'prereq)
            (dolist (target targets)
              (cl-pushnew target deps))))))
    ;; The targets of another node's prereq depend on that node.
    (dolist (other nodes)
      (unless (eq other node)
        (dolist (ref (monk-resource-refs (monk-node-resource other) 'prereq))
          (when (memq node (monk--nodes-for-ref ref other nodes))
            (cl-pushnew other deps)))))
    deps))

(defun monk--order-rank (node)
  "Return 0, 1, or 2 for :order `first', nil, or `last' on NODE."
  (pcase (oref (monk-node-resource node) order)
    ('first 0) ('last 2) ('nil 1)
    (other (signal 'monk-policy-error
                   (list (format "%s: :order must be first or last, not %S"
                                 (oref (monk-node-resource node) name) other))))))

(defun monk--order-nodes (nodes)
  "Return NODES with :order first ahead and :order last behind, stably.
Only a tie-breaker: `monk-schedule' still lets requisites win."
  (sort nodes (lambda (a b) (< (monk--order-rank a) (monk--order-rank b)))))

(defun monk-schedule (nodes)
  "Return NODES in an order that respects requisites.
Nodes without a dependency between them keep policy order, machine
by machine in inventory order.  Signal `monk-requisite-error' on a
cycle or a bad ref."
  (let ((remaining (monk--order-nodes (copy-sequence nodes)))
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
DRY-RUN is non-nil, report instead of repairing.  A failure is a
result like any other: the pass goes on, and only resources that
:require or :watch this one skip because of it.  This is the Salt
execution model."
  (let* ((resource (monk-node-resource node))
         (machine (monk-node-machine node))
         (start (float-time))
         (result (monk-result-create :machine (oref machine name)
                                     :name (oref resource name)
                                     :type (monk-resource-type resource)
                                     :origin (oref resource origin))))
    (monk-progress 'node-start :node node)
    (pcase-let ((`(,verdict . ,detail)
                 (let* ((mock (and monk--current-run (monk-run-mock monk--current-run)))
                        (reason (and (monk-node-selected node)
                                     (or (monk-resource-skip-reason resource machine)
                                         ;; A mock run opens no connection,
                                         ;; so it cannot ask what is installed.
                                         (and (not mock)
                                              (monk-resource-available-p resource machine))))))
                   (cond
                    ((not (monk-node-selected node))
                     (cons 'skip monk-skip-not-selected))
                    (reason (cons 'skip reason))
                    (mock
                     ;; Requisites still evaluate in a mock run, so
                     ;; onchanges and onfail skips show up as they would.
                     (let ((v (monk--requisite-verdict node nodes)))
                       (if (eq (car v) 'skip) v (cons 'mock nil))))
                    (t (monk--requisite-verdict node nodes))))))
      (cond
       ((eq verdict 'skip)
        (setf (monk-result-status result) monk-status-skipped
              (monk-result-comment result) detail))
       ((eq verdict 'mock)
        (setf (monk-result-status result) monk-status-kept
              (monk-result-degree result) monk-degree-satisfied
              (monk-result-comment result) monk-comment-mocked))
       (t
        (monk--class-init-once resource machine)
        (monk--run-node-with-retry resource machine result verdict detail dry-run))))
    (setf (monk-result-duration result) (- (float-time) start)
          (monk-node-result node) result)
    (monk-progress 'node-end :node node :result result)
    (monk--run-result-functions result)
    result))

(defun monk--run-result-functions (result)
  "Call `monk-result-functions' with RESULT and the current run."
  (dolist (fn monk-result-functions)
    (condition-case err
        (funcall fn result monk--current-run)
      (error (monk-log 'warn "result function %s: %s" fn (error-message-string err))))))

(defun monk--class-init-once (resource machine)
  "Run `monk-resource-class-init' once per run for RESOURCE's class on MACHINE."
  (let ((key (list 'class-init (oref machine name) (eieio-object-class-name resource))))
    (unless (monk-run-context-get key)
      (monk-run-context-put key t)
      (let ((comment (monk-resource-class-init resource machine)))
        (when comment
          (monk-log 'info "%s: %s: %s" (oref machine name)
                    (monk-resource-type resource) comment))))))

(defun monk--retry-plan (resource)
  "Return the effective :retry plist of RESOURCE, or nil when it has none."
  (let ((own (oref resource retry)))
    (and own
         (let ((plan (copy-sequence monk-retry-defaults)))
           (cl-loop for (k v) on (if (eq own t) nil own) by #'cddr
                    do (setq plan (plist-put plan k v)))
           plan))))

(defun monk--retry-wait (plan attempt)
  "Sleep before try number ATTEMPT (2 or more) as PLAN says.
Returns the seconds waited."
  (let* ((base (* (float (plist-get plan :interval))
                  (expt (float (plist-get plan :backoff)) (- attempt 2))))
         (splay (let ((s (plist-get plan :splay)))
                  (if (and s (> s 0)) (/ (random (round (* 1000 s))) 1000.0) 0)))
         (wait (min monk-retry-max-interval (+ base splay))))
    (when (> wait 0)
      ;; `sit-for' rather than `sleep-for': redisplay runs, and C-g works.
      (sit-for wait))
    wait))

(defun monk--run-node-with-retry (resource machine result verdict detail dry-run)
  "Fill RESULT for RESOURCE on MACHINE, trying again per its :retry plist.
See `monk--run-node-once' for one try.  VERDICT, DETAIL, and DRY-RUN
pass through."
  (let* ((plan (monk--retry-plan resource))
         (attempts (if plan (max 1 (plist-get plan :attempts)) 1))
         (until (if plan (plist-get plan :until) 'success))
         (attempt 0)
         (done nil))
    (while (and (not done) (< attempt attempts))
      (cl-incf attempt)
      (when (> attempt 1)
        (unless dry-run
          (let ((waited (monk--retry-wait plan attempt)))
            (monk-log 'info "%s: %s: retry %d of %d after %.1fs"
                      (oref machine name) (oref resource name) attempt attempts waited))))
      (monk--run-node-once resource machine result verdict detail dry-run)
      (setq done (or dry-run
                     (pcase until
                       ('failure (monk-result-failed-p result))
                       (_ (not (monk-result-failed-p result)))))))
    (when (and plan (> attempt 1))
      (setf (monk-result-comment result)
            (format "%s (attempt %d of %d)" (or (monk-result-comment result) "") attempt attempts)))
    (when (and plan dry-run)
      (setf (monk-result-comment result)
            (concat (or (monk-result-comment result) "")
                    (format " [retry up to %d]" attempts))))
    result))

(defun monk--run-node-once (resource machine result verdict detail dry-run)
  "One check-and-repair of RESOURCE on MACHINE into RESULT.
VERDICT and DETAIL come from `monk--requisite-verdict'.  With DRY-RUN,
report only.  After a repair, `monk-resource-verify' can still fail it."
  (condition-case err
      (let* ((changes (monk-check resource machine))
             (degree (or (plist-get changes :degree) 0.0)))
        (cond
         ((and (null changes) (eq verdict 'watch))
          (monk--perform-watch resource machine result detail dry-run))
         ((null changes)
          (setf (monk-result-status result) monk-status-kept
                (monk-result-degree result) monk-degree-satisfied
                (monk-result-comment result) nil))
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
                (monk-result-changes result) changes)
          (let ((reason (monk-resource-verify resource machine)))
            (when reason
              ;; The repair ran, but the verification says the system is
              ;; still wrong: that is a failure, whatever the degree.
              (setf (monk-result-degree result) 0.0)
              (signal 'monk-resource-error
                      (list (format "repaired, but verify failed: %s" reason))))))))
    (error
     (setf (monk-result-status result)
           (if (> (monk-result-degree result) 0.0) monk-status-partial monk-status-failed)
           (monk-result-comment result) (error-message-string err))
     (monk-log 'warn "%s: %s %s %s: %s"
               (oref machine name) (monk-resource-type resource)
               (oref resource name) (monk-result-status result)
               (error-message-string err))))
  result)

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
  (repaired-keys nil))

(defconst monk-skip-fatal "an earlier :fatal resource failed on this machine"
  "Skip comment on nodes left after a :fatal failure on their machine.")

(defun monk--aggregate-groups (nodes)
  "Return NODES with aggregatable runs folded: a list of nodes and node lists.
A group is consecutive ready nodes on one machine whose resources
`monk-resource-aggregate' merges and that have no requisite between
them.  `monk-schedule' already put dependents after dependencies, so
consecutive same-class nodes with no refs among them are safe."
  (let (out (i 0) (n (length nodes)))
    (while (< i n)
      (let* ((node (nth i nodes))
             (r (monk-node-resource node))
             (m (monk-node-machine node))
             (group (list node))
             (j (1+ i)))
        (while (and (< j n)
                    (let* ((other (nth j nodes))
                           (ro (monk-node-resource other)))
                      (and (eq (monk-node-machine other) m)
                           (eq (eieio-object-class-name ro) (eieio-object-class-name r))
                           (monk-node-selected other)
                           (monk-node-selected node)
                           (not (monk--refs-among-p (cons other group))))))
          (push (nth j nodes) group)
          (cl-incf j))
        (setq group (nreverse group))
        (if (and (cdr group)
                 (monk-resource-aggregate r (mapcar #'monk-node-resource (cdr group)) m))
            (progn (push group out) (setq i j))
          (push node out) (cl-incf i))))
    (nreverse out)))

(defun monk--refs-among-p (nodes)
  "Return non-nil when any node in NODES names another in a requisite.
Refs that point outside NODES do not matter here."
  (seq-some (lambda (a)
              (seq-some (lambda (kind)
                          (seq-some (lambda (ref)
                                      (seq-some (lambda (b) (and (not (eq a b))
                                                                 (memq b (monk--nodes-for-ref ref a nodes))))
                                                nodes))
                                    (monk-resource-refs (monk-node-resource a) kind)))
                        monk-requisite-kinds))
            nodes))

(defun monk--run-group (group nodes dry-run)
  "Check and repair the aggregatable GROUP of NODES as one.  Return the results.
The aggregate resource runs once; each member gets a result built from
its share of the changes, so the report still shows every member.
With DRY-RUN, report only."
  (let* ((first (car group))
         (m (monk-node-machine first))
         (members (mapcar #'monk-node-resource group))
         (aggregate (monk-resource-aggregate (car members) (cdr members) m))
         (start (float-time)))
    (dolist (node group) (monk-progress 'node-start :node node))
    (let* ((mock (and monk--current-run (monk-run-mock monk--current-run)))
           (unavailable (and (not mock) (monk-resource-available-p aggregate m)))
           (skips (mapcar (lambda (node)
                            (or (and (not (monk-node-selected node)) monk-skip-not-selected)
                                (monk-resource-skip-reason (monk-node-resource node) m)
                                unavailable
                                (let ((v (monk--requisite-verdict node nodes)))
                                  (and (eq (car v) 'skip) (cdr v)))))
                          group))
           (all-changes nil)
           (aggregate-result (monk-result-create :machine (oref m name)
                                                 :name (oref aggregate name)
                                                 :type (monk-resource-type aggregate))))
      (cond
       ((seq-every-p #'identity skips) nil)
       (mock (setf (monk-result-status aggregate-result) monk-status-kept
                   (monk-result-comment aggregate-result) monk-comment-mocked))
       (t
        (monk--class-init-once aggregate m)
        (monk--run-node-once aggregate m aggregate-result 'run nil dry-run)
        (setq all-changes (monk-result-changes aggregate-result))))
      (cl-mapcar
       (lambda (node skip)
         (let* ((r (monk-node-resource node))
                (mine (and all-changes (monk-resource-disaggregate aggregate r all-changes)))
                (result (monk-result-create :machine (oref m name) :name (oref r name)
                                            :type (monk-resource-type r)
                                            :origin (oref r origin)
                                            :duration (- (float-time) start))))
           (cond
            (skip (setf (monk-result-status result) monk-status-skipped
                        (monk-result-comment result) skip))
            ((monk-result-failed-p aggregate-result)
             (setf (monk-result-status result) (monk-result-status aggregate-result)
                   (monk-result-comment result) (monk-result-comment aggregate-result)
                   (monk-result-changes result) mine))
            ((null mine)
             (setf (monk-result-status result) monk-status-kept
                   (monk-result-degree result) monk-degree-satisfied
                   (monk-result-comment result) (and mock monk-comment-mocked)))
            (t (setf (monk-result-status result) (monk-result-status aggregate-result)
                     (monk-result-degree result) monk-degree-satisfied
                     (monk-result-changes result) mine
                     (monk-result-comment result)
                     (format "%s (aggregated with %d other%s)"
                             (or (monk-result-comment aggregate-result) "")
                             (1- (length group)) (if (= 2 (length group)) "" "s")))))
           (setf (monk-node-result node) result)
           (monk-progress 'node-end :node node :result result)
           (monk--run-result-functions result)
           result))
       group skips))))

(defun monk--pass (nodes dry-run)
  "Run one pass over NODES in order.  Return a `monk-pass'.
Results from earlier passes are cleared first so requisites read this
pass only.  When DRY-RUN is non-nil, report only.  Every node runs:
a failure never stops the pass, except after a :fatal failure, which
ends the pass on that machine only."
  (dolist (node nodes)
    (setf (monk-node-result node) nil))
  (let (results (repaired 0) (repaired-keys nil) (fatal-machines nil))
    (dolist (item (monk--aggregate-groups nodes))
      (let* ((group (if (listp item) item (list item)))
             (m (monk-node-machine (car group)))
             (group-results
              (if (memq m fatal-machines)
                  (mapcar (lambda (node)
                            (let ((r (monk-node-resource node)))
                              (setf (monk-node-result node)
                                    (monk-result-create :machine (oref m name)
                                                        :name (oref r name)
                                                        :type (monk-resource-type r)
                                                        :origin (oref r origin)
                                                        :status monk-status-skipped
                                                        :comment monk-skip-fatal))))
                          group)
                (if (listp item)
                    (monk--run-group group nodes dry-run)
                  (list (monk--run-node item nodes dry-run))))))
        (cl-mapc (lambda (node result)
                   (push result results)
                   (when (monk-result-changed-p result)
                     (cl-incf repaired)
                     (push (monk-result-key result) repaired-keys))
                   (when (and (monk-result-failed-p result)
                              (oref (monk-node-resource node) fatal)
                              (not dry-run))
                     (monk-log 'warn "%s: %s failed and is :fatal; the rest of this machine is skipped"
                               (oref m name) (monk-result-name result))
                     (cl-pushnew m fatal-machines)))
                 group group-results)))
    (monk-pass-create :results (nreverse results)
                      :repaired repaired
                      :repaired-keys (nreverse repaired-keys))))

(defun monk--run-listeners (nodes run dry-run)
  "After convergence, run the watch action once per node in NODES with :listen.
A listening node fires when any of its refs changed in any pass of
RUN.  Results replace the node's result in RUN.  With DRY-RUN, report
what would fire.  Return the number that fired."
  (let ((changed-keys (apply #'append (monk-run-repaired-by-pass run)))
        (fired 0))
    (dolist (node nodes)
      (let* ((resource (monk-node-resource node))
             (machine (monk-node-machine node))
             (refs (monk-resource-refs resource 'listen)))
        (when (and refs (monk-node-selected node)
                   (not (monk-resource-skip-reason resource machine))
                   (monk-node-result node)
                   (not (monk-result-failed-p (monk-node-result node))))
          (let ((triggers (seq-filter
                           (lambda (ref)
                             (seq-some (lambda (target) (member (monk-node-key target) changed-keys))
                                       (monk--nodes-for-ref ref node nodes)))
                           refs)))
            (when triggers
              (cl-incf fired)
              (let ((result (monk-result-create :machine (oref machine name)
                                                :name (oref resource name)
                                                :type (monk-resource-type resource)
                                                :origin (oref resource origin))))
                (condition-case err
                    (monk--perform-watch resource machine result
                                         (format "listen: %s changed"
                                                 (mapconcat #'monk-ref-name triggers ", "))
                                         dry-run)
                  (error
                   (setf (monk-result-status result) monk-status-failed
                         (monk-result-comment result) (error-message-string err))))
                (setf (monk-node-result node) result)
                (setf (monk-run-results run)
                      (monk--merge-pass-results (monk-run-results run) (list result)))))))))
    fired))

(defun monk--collect-rescued (nodes run)
  "Record in RUN the failed keys of NODES that a :handled handler rescued."
  (dolist (node nodes)
    (let ((resource (monk-node-resource node))
          (result (monk-node-result node)))
      (when (and (oref resource handled) result
                 (memq (monk-result-status result)
                       (list monk-status-kept monk-status-repaired monk-status-would-repair)))
        (dolist (ref (monk-resource-refs resource 'onfail))
          (dolist (target (monk--nodes-for-ref ref node nodes))
            (when (and (monk-node-result target) (monk-result-failed-p (monk-node-result target)))
              (cl-pushnew (monk-node-key target) (monk-run-rescued run) :test #'equal))))))))

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

(defun monk--run-nodes (machines nodes dry-run max-passes policy options)
  "Run NODES for MACHINES until nothing changes.  Return a `monk-run'.
POLICY and OPTIONS are recorded on the run so `monk-apply' can repeat it."
  ;; An abbreviated `default-directory' such as ~/x is re-expanded with
  ;; $HOME at each file primitive.  A caller that rebinds HOME for the
  ;; run, as the test drive and the agent do, then points every
  ;; relative operation at a directory that does not exist.  Expand it
  ;; once now, while $HOME still means what the caller meant.
  (let ((default-directory (if (file-remote-p default-directory)
                               default-directory
                             (file-name-as-directory (expand-file-name default-directory))))
        (run (monk-run-create :machines machines :dry-run dry-run :passes 0
                              :policy policy
                              ;; The plan is a link, not an option to repeat.
                              :options (let ((o (copy-sequence options)))
                                         (cl-remf o :plan)
                                         (cl-remf o :watch-reasons)
                                         o)
                              :plan (plist-get options :plan)
                              :mock (plist-get options :mock)
                              :reasons (plist-get options :watch-reasons)))
        (ordered (monk-schedule nodes))
        (all-results nil)
        (pass 0)
        (continue t))
    (setq monk--current-run run)
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
            (setf (monk-run-last-pass-repaired run) repaired
                  (monk-run-passes run) pass
                  (monk-run-results run) all-results)
            (push (monk-pass-repaired-keys outcome) (monk-run-repaired-by-pass run))
            (monk-log 'info "pass %d over %d node(s) repaired %d" pass (length ordered) repaired)
            (monk-progress 'pass-end :run run :pass pass :repaired repaired)
            ;; A resource that repaired twice is in a cycle and will
            ;; repair again.  Stop now instead of at the cap.
            (setq continue (and (not dry-run)
                                (not (monk-run-mock run))
                                (> repaired 0)
                                (not (monk-run-flapping-p run))))))
      ;; Listeners fire once, after the passes, whatever changed when.
      (unless (monk-run-mock run)
        (let ((fired (condition-case err
                         (monk--run-listeners ordered run dry-run)
                       (error (monk-log 'warn "listeners: %s" (error-message-string err)) 0))))
          (when (> fired 0)
            (monk-log 'info "%d listener(s) fired after convergence" fired))))
      (monk--collect-rescued ordered run)
      (setq monk--current-run nil)
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
  (apply #'monk-orchestrate (list m) policy options))

(defun monk-orchestrate (machines policy &rest options)
  "Apply POLICY to MACHINES as one dependency graph.
Requisites that point at other machines or groups resolve inside this
run.  OPTIONS is a plist: :dry-run, :passes, :plan, an earlier dry run
to compare against, :mock, which compiles and orders the graph and
calls no resource (every result is kept with `monk-comment-mocked',
requisites still evaluate), and :only, a list of (MACHINE-NAME . RESOURCE-NAME)
keys.  With :only, just those resources and their transitive
dependents run; the rest compile, so requisites resolve, and are
skipped.  Return a `monk-run'."
  (pcase-let ((`(,dry-run . ,max-passes) (monk--run-options options))
              (only (plist-get options :only)))
    (monk-log 'info "%s %d machine(s)%s%s"
              (if dry-run "plan" "apply") (length machines)
              (if dry-run " (dry run)" "")
              (if only (format ", only %d key(s) and dependents" (length only)) ""))
    (let ((nodes (monk--nodes-for machines policy)))
      (when only
        (monk-select-nodes nodes only))
      (monk--run-nodes machines nodes dry-run max-passes policy options))))

(defun monk-plan (machines policy &rest options)
  "Dry-run POLICY on MACHINES.  Return a `monk-run' with no changes made.
MACHINES is a machine or a list.  OPTIONS are as for `monk-orchestrate'.
The result is a plan: give it to `monk-apply' to make the changes and
compare the outcome with what the plan said."
  (apply #'monk-orchestrate (if (listp machines) machines (list machines)) policy
         :dry-run t options))

(defun monk-apply (plan &rest options)
  "Run the policy of PLAN for real on the same machines.
PLAN is a `monk-run' from `monk-plan'.  The returned run carries PLAN
in its plan slot, so `monk-run-compare' can show what differed.
OPTIONS override those recorded on PLAN, except :dry-run."
  (unless (monk-run-dry-run plan)
    (signal 'monk-policy-error (list "monk-apply needs a dry run as its plan")))
  (let ((merged (copy-sequence (monk-run-options plan))))
    (while options
      (setq merged (plist-put merged (pop options) (pop options))))
    (setq merged (plist-put merged :dry-run nil))
    (setq merged (plist-put merged :plan plan))
    (apply #'monk-orchestrate (monk-run-machines plan) (monk-run-policy plan) merged)))

;;;; Describing changes

(defun monk--describe-value (value)
  "Return VALUE as short text for a change description."
  (cond
   ((eq value 'unset) "unset")
   ((eq value t) "yes")
   ((null value) "none")
   ((stringp value)
    (if (string-match-p "\n" value)
        (format "%S" value)
      value))
   ((integerp value) (number-to-string value))
   (t (format "%S" value))))

(defun monk-describe-changes (changes)
  "Return CHANGES, a plist from `monk-check', as one readable line.
Known keys read as prose: :before and :after as an arrow, :create,
:mode as octal, :retarget and :replace, :install as a count.  Keys
without a rule print as KEY VALUE.  :degree and :diff are omitted;
the report shows them elsewhere."
  (let (parts (rest (copy-sequence changes)))
    (when (or (plist-member rest :before) (plist-member rest :after))
      (push (format "%s → %s" (monk--describe-value (plist-get rest :before))
                    (monk--describe-value (plist-get rest :after)))
            parts)
      (cl-remf rest :before) (cl-remf rest :after))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (pcase key
          ((or :degree :diff) nil)
          (:create (push (if (stringp value) (format "create → %s" value) "create") parts))
          (:content (push "content differs" parts))
          (:mode (push (if (and (consp value) (integerp (car value)))
                           (format "mode %04o → %04o" (logand (car value) #o7777) (cadr value))
                         "mode")
                       parts))
          (:owner (push (format "owner %s → %s" (car value) (cadr value)) parts))
          (:group (push (format "group %s → %s" (car value) (cadr value)) parts))
          (:retarget (push (format "%s → %s" (car value) (cadr value)) parts))
          (:replace (push (format "replace %s with link → %s" (car value) (cadr value)) parts))
          (:remove (push (format "remove %s" value) parts))
          (:install (push (format "install %d: %s" (length value)
                                  (truncate-string-to-width (string-join value " ") 60 nil nil "…"))
                          parts))
          (:clone (push (format "clone %s (%s)" (car value) (cadr value)) parts))
          (:checkout (push (format "checkout %s → %s" (car value) (cadr value)) parts))
          (:behind (push (format "fast-forward to %s" (if (stringp value) (substring value 0 (min 8 (length value))) value)) parts))
          (:enabled (push (format "%s → %s" (car value) (if (cadr value) "enabled" "disabled")) parts))
          (:started (push (format "%s → %s" (car value) (if (cadr value) "started" "stopped")) parts))
          (:live (push (format "live %s → %s" (car value) (cadr value)) parts))
          (:persist (push (format "persist %s" value) parts))
          (:run (push (format "run %s" (string-join (mapcar (lambda (a) (format "%s" a)) value) " ")) parts))
          (_ (push (format "%s %s" (string-remove-prefix ":" (symbol-name key))
                           (monk--describe-value value))
                   parts)))))
    (string-join (nreverse parts) ", ")))

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
                      (monk-run-summary run (oref m name))))
      (when verbose
        (insert (monk-machine-describe-string m "   "))))
    (when (monk-run-reasons run)
      (insert "== triggered by drift:\n")
      (dolist (cell (monk-run-reasons run))
        (insert (format "   %s:%s  %s\n" (caar cell) (cdar cell) (cdr cell)))))
    (when (monk-run-plan run)
      (let ((counts (monk-run-compare-counts
                     (monk-run-compare (monk-run-plan run) run))))
        (insert (format "== against plan: %s\n"
                        (mapconcat (lambda (v)
                                     (format "%d %s" (alist-get v counts 0) v))
                                   monk-plan-verdicts ", ")))
        (dolist (cell (monk-run-compare (monk-run-plan run) run))
          (unless (eq (cdr cell) monk-plan-as-planned)
            (insert (format "   %-14s %s:%s\n" (cdr cell) (caar cell) (cdar cell)))))))
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
              (insert (format "              %s\n" (monk-describe-changes (monk-result-changes result)))))
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

;;;; Jumping to definitions

(defun monk-find-origin (origin &optional other-window)
  "Open the file of ORIGIN and move point to the definition of its name.
ORIGIN is (FILE . NAME).  The file is searched for NAME as a string
literal first, then as a bare word, from the top.  With OTHER-WINDOW,
show it in another window.  Push a marker so `M-,' comes back.
Signal `user-error' when ORIGIN has no file."
  (let ((file (monk-origin-file origin))
        (name (cdr origin)))
    (unless (and file (file-readable-p file))
      (user-error "No source file recorded for %s%s" name
                  (if file (format " (%s is not readable)" file) "")))
    (xref-push-marker-stack)
    (funcall (if other-window #'find-file-other-window #'find-file) file)
    (goto-char (point-min))
    (unless (or (search-forward (format "%S" name) nil t)
                (re-search-forward (concat "\\_<" (regexp-quote name) "\\_>") nil t))
      (message "Opened %s; %s is not written literally there" file name))
    (beginning-of-line)
    (recenter)))

(defun monk-result-find-definition (result &optional other-window)
  "Jump to the policy line that defined RESULT.  OTHER-WINDOW as in `find-file'."
  (monk-find-origin (or (monk-result-origin result)
                        (cons nil (monk-result-name result)))
                    other-window))

(cl-defmethod monk-machine-find-definition ((m monk-machine) &optional other-window)
  "Jump to the inventory entry that defined machine M.
OTHER-WINDOW as in `find-file-other-window'."
  (monk-find-origin (or (oref m origin) (cons nil (oref m name))) other-window))

;;;; Batch entry point

(defvar monk-batch-policy nil
  "The policy function a batch entry point runs.  Set by the policy file.")

(defun monk-batch-converge (policy-file &rest machine-args)
  "Load POLICY-FILE and converge the local machine.
POLICY-FILE must set `monk-batch-policy' to a policy function.
MACHINE-ARGS are passed to `monk-local-machine'.  Exit non-zero when
any resource failed."
  (load policy-file nil t)
  (let* ((policy (or monk-batch-policy
                     (signal 'monk-policy-error
                             (list "monk-batch-policy is not set by" policy-file))))
         (run (monk-converge (apply #'monk-local-machine machine-args) policy)))
    (monk-run-report run)
    (unless (monk-run-ok-p run)
      (kill-emacs 1))
    run))

(provide 'monk)
;;; monk.el ends here
