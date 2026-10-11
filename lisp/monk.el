;;; monk.el --- Configuration management in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, files, processes, comm
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.6.5

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

;;;; Version

(defconst monk-version "0.6.5"
  "Monk's version.  Patch for small work, minor for a large ask.
When the drive report shows a number older than what you were told,
the Emacs runs old code.")

(defconst monk-build "2026-10-11.6"
  "The build tag: the date of the last change and a counter.
Finer than `monk-version' so a report can tell two edits on one day
apart.  Bumped with every change that reaches a hand-over.")

(defconst monk-libraries
  '(monk monk-cell monk-file monk-system monk-package monk-sync monk-inventory
    monk-event monk-history monk-ui monk-watch monk-agent monk-cli monk-bridge
    monk-translate monk-embark monk-detach monk-connect monk-etcd monk-reload monk-testdrive monk-tutorial)
  "Every Monk library, for `monk-version'.")

(defvar monk--load-times nil
  "Alist of (FEATURE . TIME) recorded when each Monk library loaded.")

(defun monk-note-load (feature)
  "Record that FEATURE loaded now.  Each Monk library calls this at its end."
  (setf (alist-get feature monk--load-times) (current-time)))

(defun monk-stale-libraries ()
  "Return the loaded Monk libraries whose source changed after they loaded.
Each entry is (FEATURE . SECONDS-NEWER).  Compares the .el on disk with
the time the library loaded, so an edit that a reload missed shows up
here.  A library loaded from a .elc older than its .el counts too."
  (let (out)
    (dolist (feature monk-libraries)
      (when (featurep feature)
        (let* ((loaded (alist-get feature monk--load-times))
               (path (or (locate-library (concat (symbol-name feature) ".el"))
                         (locate-library (symbol-name feature))))
               (source (and path (concat (file-name-sans-extension path) ".el")))
               (mtime (and source (file-exists-p source)
                           (file-attribute-modification-time (file-attributes source)))))
          (when (and loaded mtime (time-less-p loaded mtime))
            (push (cons feature (float-time (time-subtract mtime loaded))) out)))))
    (nreverse out)))

(defun monk-format-duration (seconds &optional days)
  "Return SECONDS as a fixed-width `00h 00m 00s', or `0d 00h 00m 00s' with DAYS.
Every field is zero-padded, so a line that is rewritten each tick
does not shift as the numbers grow."
  (let* ((total (max 0 (round seconds)))
         (d (/ total 86400))
         (h (/ (% total 86400) 3600))
         (m (/ (% total 3600) 60))
         (sec (% total 60)))
    (if days
        (format "%dd %02dh %02dm %02ds" d h m sec)
      (format "%02dh %02dm %02ds" (+ h (* 24 d)) m sec))))

(defun monk-version-string ()
  "Return one line: version, build, where monk.el loaded from, and stale libraries."
  (let* ((stale (monk-stale-libraries))
         (from (or (locate-library "monk") "?")))
    (format "Monk %s build %s from %s%s"
            monk-version monk-build (abbreviate-file-name (file-name-directory from))
            (if stale
                (format "  STALE: %s (edited after load; run monk-reload)"
                        (mapconcat (lambda (c) (format "%s +%ds" (car c) (round (cdr c)))) stale ", "))
              ""))))

(defun monk-version ()
  "Show Monk's version and build, and which libraries are older than their source."
  (interactive)
  (message "%s" (monk-version-string)))

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
(defconst monk-status-unreachable 'unreachable
  "The machine could not be reached, so the resource was not tried.
Ansible's `dark'.  Not a failure of the policy: excluded from the
degree, counted on its own, and the rest of that machine is not
attempted in this run.")
(defconst monk-status-pending 'pending
  "Resource has not run yet.  Used by progress displays.")
(defconst monk-status-running 'running
  "Resource is being checked or repaired.  Used by progress displays.")

(defconst monk-statuses
  (list monk-status-kept monk-status-repaired monk-status-partial
        monk-status-failed monk-status-skipped monk-status-would-repair
        monk-status-unreachable)
  "All statuses a finished result can carry, in display order.")

(defconst monk-home-environment-variable "MONK_HOME"
  "Environment variable that names the Monk state directory.")

(defconst monk-home-root-directory "/etc/monk"
  "Monk state directory when Emacs runs as root.")

(defun monk-home-directory ()
  "Return the directory for Monk state on this host.
$MONK_HOME wins.  Root uses `monk-home-root-directory'.  Anyone else
uses $XDG_DATA_HOME/monk or ~/.local/share/monk."
  (file-name-as-directory
   (or (getenv monk-home-environment-variable)
       (if (zerop (user-uid))
           monk-home-root-directory
         (expand-file-name "monk" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))))

(defcustom monk-fact-ttl 86400
  "Seconds for which cached facts count as fresh.
A machine whose facts in the cache are younger than this is not
contacted for facts at compile time, so plans, queries, and mock runs
work while it is off.  Ansible's fact_caching_timeout, one day.
Nil turns the cache off."
  :type '(choice (const nil) natnum))

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
:until is `success' (stop on kept or repaired), `failure' (stop on the
first failure, for a resource expected to fail), or a function of the
result that returns non-nil when done.  Salt's defaults, plus
:backoff and the predicate form from Ansible's until."
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
(define-error 'monk-command-unchanged "Monk command ran without change" 'monk-error)
(define-error 'monk-cel-error "Monk CEL error" 'monk-guard-error)

;;;; Scopes and owners
;;
;; Three scopes, and a resource runs in exactly one:
;;
;;   user  as the invoking user, in their home.  Dotfiles, user
;;         crontab, brew.  No privilege.
;;   sudo  as the user, escalated per resource through the sudo hop
;;         (`:sudo t').  The identity is still the user: sudo logs it
;;         and gates it, `~' is the user's home.  System files, system
;;         packages and units.
;;   root  the process itself is uid 0: an agent under launchd or
;;         systemd.  No prompt is possible and nobody asked, so a
;;         policy must say it expects this (`monk-policy-scope').
;;
;; The scope of a resource is derived, never declared: `:sudo t' makes
;; it sudo, else user; a root process makes every resource root.  The
;; owner of a run is the login name and whether the process is root.
;; Mismatches are compile errors: a user resource in a root process
;; writes into the wrong home, a root policy in a user process has
;; nothing to be root with.  Scope and owner go into every event and
;; history row, so "what did sudo change this week" is a query.

(defconst monk-scope-user 'user)
(defconst monk-scope-sudo 'sudo)
(defconst monk-scope-root 'root)
(defconst monk-scopes (list monk-scope-user monk-scope-sudo monk-scope-root))

(defun monk-root-p ()
  "Return non-nil when this Emacs runs as root."
  (zerop (user-uid)))

(defun monk-owner ()
  "Return (LOGIN . ROOT-P) for the process: who runs policies here."
  (cons (user-login-name) (monk-root-p)))

(defun monk-owner-string (owner)
  "Return OWNER, from `monk-owner', as text: `tzz' or `root (tzz)'."
  (if (cdr owner)
      (if (equal (car owner) "root") "root" (format "root (%s)" (car owner)))
    (car owner)))

(defvar monk-policy-scopes nil
  "Alist of (POLICY-SYMBOL . SCOPE) declared with `monk-policy-scope'.")

(defun monk-policy-scope (policy &optional scope)
  "Declare that POLICY expects SCOPE; without SCOPE, return the declaration.
POLICY is a symbol.  SCOPE is `root' for a policy that runs in a root
process (an agent under launchd or systemd), else nil: the default,
user and sudo resources in a user process."
  (if scope
      (progn (unless (memq scope monk-scopes)
               (signal 'monk-policy-error (list "Unknown scope" scope)))
             (setf (alist-get policy monk-policy-scopes) scope))
    (alist-get policy monk-policy-scopes)))

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

(defvar monk-batch-print-function (lambda (line) (message "%s" line))
  "How a batch Emacs prints one log line.
`monk-cli' sets this to keep its status line intact.")

(defun monk-log (level format-string &rest args)
  "Log FORMAT-STRING with ARGS at LEVEL to `monk-log-buffer-name'."
  (when (monk--log-enabled-p level)
    (let ((line (apply #'format format-string args)))
      (with-current-buffer (get-buffer-create monk-log-buffer-name)
        (goto-char (point-max))
        (insert (format-time-string "%FT%T ")
                (upcase (symbol-name level)) " " line "\n"))
      (when (and noninteractive (not (eq level 'debug)))
        (funcall monk-batch-print-function line)))))

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
  origin                ; (FILE . NAME): where the resource was defined
  censored              ; non-nil when :no-log removed changes, diff, and comment
  scope)                ; `user', `sudo', or `root': see `monk-resource-scope'

(cl-defstruct (monk-diff-entry (:constructor monk-diff-entry-create)
                               (:copier nil))
  "One before/after pair of a result's diff.  Ansible's diff shape.
BEFORE and AFTER are strings for content, or alists for attributes
such as mode and owner.  The headers name what the two sides are,
usually the path and a tag such as `mode'.  KIND is `content' or
`attributes'."
  before after
  before-header after-header
  (kind 'content))

(defun monk-diff-content (before after path)
  "Return a content diff entry from BEFORE to AFTER for PATH."
  (monk-diff-entry-create :before (or before "") :after (or after "")
                          :before-header path :after-header path :kind 'content))

(defun monk-diff-attributes (path before after)
  "Return an attributes diff entry for PATH between alists BEFORE and AFTER."
  (monk-diff-entry-create :before before :after after
                          :before-header path :after-header path :kind 'attributes))

(defun monk-diff-entries (diff)
  "Return DIFF as a list of `monk-diff-entry'.
Accepts the old shape too: a unified-diff string becomes one entry
whose rendering is that string."
  (cond
   ((null diff) nil)
   ((stringp diff) (list (monk-diff-entry-create :before nil :after diff :kind 'rendered)))
   ((monk-diff-entry-p diff) (list diff))
   (t diff)))

(declare-function monk--unified-diff "monk-file" (old new label))

(defun monk-diff-render-entry (entry)
  "Return ENTRY as text: a unified diff, or `key: a → b' lines for attributes."
  (pcase (monk-diff-entry-kind entry)
    ('rendered (monk-diff-entry-after entry))
    ('attributes
     (let ((before (monk-diff-entry-before entry))
           (after (monk-diff-entry-after entry)))
       (concat (format "--- a/%s (attributes)\n+++ b/%s (attributes)\n"
                       (monk-diff-entry-before-header entry) (monk-diff-entry-after-header entry))
               (mapconcat (lambda (cell)
                            (let ((old (cdr (assq (car cell) before))))
                              (format "%s: %s → %s" (car cell)
                                      (if (integerp old) (format "%04o" old) (or old "unset"))
                                      (if (integerp (cdr cell)) (format "%04o" (cdr cell)) (cdr cell)))))
                          after "\n"))))
    (_ (require 'monk-file)
       (monk--unified-diff (monk-diff-entry-before entry) (monk-diff-entry-after entry)
                           (or (monk-diff-entry-before-header entry) "")))))

(defun monk-diff-render (diff)
  "Return DIFF, entries or a string, as one text block, or nil when empty."
  (let ((parts (delq nil (mapcar (lambda (e)
                                   (let ((text (monk-diff-render-entry e)))
                                     (and text (not (string-empty-p text)) text)))
                                 (monk-diff-entries diff)))))
    (and parts (mapconcat (lambda (p) (if (string-suffix-p "\n" p) p (concat p "\n"))) parts ""))))

(defun monk-result-diff-text (result)
  "Return the rendered diff of RESULT, or nil."
  (monk-diff-render (monk-result-diff result)))

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
  (unreachable nil)     ; alist of (MACHINE-NAME . REASON) for machines that did not answer
  (owner (monk-owner))  ; who runs this: see `monk-owner'
  (batches nil)         ; lists of machine names when :serial split the run
  (stopped-at-batch nil) ; index of the batch whose failures stopped the rest
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
  "Return non-nil when no result in RUN failed and every machine answered.
A failure that an :onfail handler with :handled t covered is rescued
and does not count.  An unreachable machine counts unless it has
`ignore-unreachable'.  A run that stopped at a batch is not ok.  See
`monk-run-rescued', `monk-run-unreachable'."
  (and (not (monk-run-stopped-at-batch run))
       (not (seq-some (lambda (r)
                        (and (monk-result-failed-p r)
                             (not (member (monk-result-key r) (monk-run-rescued run)))))
                      (monk-run-results run)))
       (not (seq-some (lambda (cell)
                        (let ((m (seq-find (lambda (m) (equal (oref m name) (car cell)))
                                           (monk-run-machines run))))
                          (not (and m (monk-machine-ignores-unreachable-p m)))))
                      (monk-run-unreachable run)))))

(declare-function monk-machine-ignores-unreachable-p "monk" (m))

(defun monk-run-unreachable-p (run machine-name)
  "Return the reason MACHINE-NAME was unreachable in RUN, or nil."
  (cdr (assoc machine-name (monk-run-unreachable run))))

(defun monk-run-exit-code (run)
  "Return the exit code a batch entry point gives for RUN.
Ansible's bit-safe codes: 0 when ok, 2 when a resource failed, 4 when
a machine was unreachable, 6 for both.  Rescued failures and machines
with `ignore-unreachable' do not count."
  (logior (if (seq-some (lambda (r) (and (monk-result-failed-p r)
                                         (not (member (monk-result-key r) (monk-run-rescued run)))))
                        (monk-run-results run))
              2 0)
          (if (seq-some (lambda (cell)
                          (let ((m (seq-find (lambda (m) (equal (oref m name) (car cell)))
                                             (monk-run-machines run))))
                            (not (and m (monk-machine-ignores-unreachable-p m)))))
                        (monk-run-unreachable run))
              4 0)))

;;;; Run context

(defvar monk--unreachable-at-compile nil
  "Alist of (MACHINE-NAME . REASON) found unreachable while gathering facts.
Set by `monk--nodes-for', read by `monk--run-nodes' into the run.")

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
   (gathered :initarg :gathered
             :initform nil
             :documentation "When the facts were gathered, a time value, or nil.
Set by `monk-gather-facts' and by the cache when it fills the facts.")
   (facts-source :initarg :facts-source
                 :initform nil
                 :documentation "`live' when this session gathered the facts, `cache' when they
came from the history database, nil when there are none.")
   (origin :initarg :origin
           :initform nil
           :documentation "Where the inventory defined this machine: (FILE . NAME).")
   (ignore-unreachable :initarg :ignore-unreachable
                       :initform nil
                       :documentation "When non-nil, this machine being unreachable does not make
the run not ok.  A laptop that is often away.  The inventory variable
`ignore_unreachable' sets it."))
  "A machine that Monk manages.")

(defvar monk-facts-ready-functions nil
  "Functions called with a machine once its facts are in, live or from the cache.
monk-inventory uses it to apply derived groups.")

(defvar monk-fact-cache-functions nil
  "Functions that store and load facts: (STORE M) and (LOAD M).
An alist with keys `store' and `load'.  monk-history installs the
SQLite-backed pair when it loads.  LOAD returns (FACTS . GATHERED) or
nil.")

(defun monk-facts-store (m)
  "Store the facts of M through `monk-fact-cache-functions'."
  (let ((store (alist-get 'store monk-fact-cache-functions)))
    (when store
      (condition-case err
          (funcall store m)
        (error (monk-log 'warn "%s: fact cache store failed: %s" (oref m name)
                         (error-message-string err)))))))

(defun monk-facts-load (m)
  "Fill M's facts from the cache when they are fresh.  Return non-nil when done."
  (let ((load (alist-get 'load monk-fact-cache-functions)))
    (when (and load monk-fact-ttl)
      (pcase (condition-case err
                 (funcall load m)
               (error (monk-log 'warn "%s: fact cache load failed: %s" (oref m name)
                                (error-message-string err))
                      nil))
        (`(,facts . ,gathered)
         (when (and facts gathered
                    (< (float-time (time-since gathered)) monk-fact-ttl))
           (oset m facts facts)
           (oset m gathered gathered)
           (oset m facts-source 'cache)
           (oset m classes (monk-hard-classes m))
           (run-hook-with-args 'monk-facts-ready-functions m)
           t))))))

(defun monk-facts-flush (&optional machines)
  "Forget the facts of MACHINES, default `monk-inventory', in memory and cache.
The next compile gathers them again.  Ansible's --flush-cache."
  (interactive)
  (let ((flush (alist-get 'flush monk-fact-cache-functions)))
    (dolist (m (or machines (bound-and-true-p monk-inventory)))
      (oset m facts nil)
      (oset m gathered nil)
      (oset m facts-source nil)
      (when flush (funcall flush m)))))

(cl-defmethod monk-machine-facts-age ((m monk-machine))
  "Return the age of M's facts in seconds, or nil when none were gathered."
  (and (oref m gathered) (float-time (time-since (oref m gathered)))))

(cl-defmethod monk-machine-facts-fresh-p ((m monk-machine))
  "Return non-nil when M's facts are live from this session or within the TTL."
  (or (eq (oref m facts-source) 'live)
      (let ((age (monk-machine-facts-age m)))
        (and age monk-fact-ttl (< age monk-fact-ttl)))))

(cl-defmethod monk-machine-ignores-unreachable-p ((m monk-machine))
  "Return non-nil when M being unreachable must not fail a run."
  (oref m ignore-unreachable))

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

(defconst monk-facts-d-directories
  '("~/.config/monk/facts.d" "/etc/monk/facts.d" "/etc/ansible/facts.d")
  "Directories whose *.fact files become the `local' fact, in order.
A later file with the same name overrides an earlier one.  Ansible's
facts.d, read too, so a machine that already publishes local facts
for Ansible needs nothing new.")

(defun monk--parse-fact-file (text)
  "Return TEXT as an alist: JSON when it parses, else INI sections."
  (or (condition-case nil
          (let ((parsed (json-parse-string text :object-type 'alist :null-object nil :false-object :false)))
            (and (listp parsed) parsed))
        (error nil))
      ;; INI: [section] then key=value; keys outside a section go under `general'
      (let ((section 'general) (result nil))
        (dolist (line (split-string text "\n" t))
          (let ((line (string-trim line)))
            (cond
             ((or (string-empty-p line) (string-prefix-p "#" line) (string-prefix-p ";" line)))
             ((string-match "\\`\\[\\(.+\\)\\]\\'" line)
              (setq section (intern (match-string 1 line))))
             ((string-match "\\`\\([^=]+?\\)\\s-*=\\s-*\\(.*\\)\\'" line)
              (let ((key (intern (match-string 1 line))) (value (match-string 2 line)))
                (setf (alist-get key (alist-get section result)) value))))))
        (mapcar (lambda (cell) (cons (car cell) (nreverse (cdr cell)))) (nreverse result)))))

(defun monk--probe-local (m)
  "Return the `local' fact of M from its facts.d files.
Each FILE.fact contributes (FILE . VALUE).  An executable file is
run and its output parsed; other files are read.  JSON first, INI
second.  A file that fails to parse contributes its error string, so
the problem is visible in the inspector rather than silent."
  (let ((local nil))
    (dolist (dir monk-facts-d-directories)
      (let ((full (monk-machine-path m dir)))
        (when (file-directory-p full)
          (dolist (file (directory-files full t "\\.fact\\'"))
            (let* ((name (intern (file-name-sans-extension (file-name-nondirectory file))))
                   (text (condition-case err
                             (if (file-executable-p file)
                                 (monk-exec-string m (monk-machine-local-path file))
                               (with-temp-buffer (insert-file-contents file) (buffer-string)))
                           (error (format "error: %s" (error-message-string err))))))
              (setf (alist-get name local)
                    (if (string-prefix-p "error:" text) text (monk--parse-fact-file text))))))))
    (and local (list (cons 'local (nreverse local))))))

(defconst monk-package-managers
  '((homebrew "brew") (apt "apt-get") (dnf "dnf") (yum "yum") (pacman "pacman")
    (zypper "zypper") (apk "apk") (pkg "pkg"))
  "Package managers Monk knows, as (NAME PROGRAM), in detection order.
Ansible's pkg_mgr names where they exist.")

(defconst monk-service-managers
  '((launchd "launchctl") (systemd "systemctl") (openrc "rc-service") (sysv "service"))
  "Service managers Monk knows, as (NAME PROGRAM), in detection order.")

(defun monk--probe-managers (m)
  "Return the `pkg-mgr' and `service-mgr' facts of M by looking for programs.
One `sh -c' per machine lists which of the known programs exist, so
the probe costs one round trip.  Ansible's pkg_mgr and service_mgr."
  (let* ((programs (delete-dups (append (mapcar #'cadr monk-package-managers)
                                        (mapcar #'cadr monk-service-managers))))
         (script (mapconcat (lambda (p) (format "command -v %s >/dev/null 2>&1 && echo %s" p p))
                            programs "; "))
         (found (split-string (or (ignore-errors (monk-exec-string m "sh" "-c" (concat script "; true"))) "")
                              "\n" t))
         (pkg (car (seq-find (lambda (entry) (member (cadr entry) found)) monk-package-managers)))
         (svc (car (seq-find (lambda (entry) (member (cadr entry) found)) monk-service-managers))))
    ;; On macOS, launchd is the service manager even though systemctl
    ;; may exist through a port; and brew wins over a stray apt.
    (when (equal (monk-machine-fact m 'kernel) "Darwin")
      (setq svc 'launchd)
      (when (member "brew" found) (setq pkg 'homebrew)))
    (list (cons 'pkg-mgr (and pkg (symbol-name pkg)))
          (cons 'service-mgr (and svc (symbol-name svc))))))

(monk-register-fact-probe 'uname #'monk--probe-uname)
(monk-register-fact-probe 'os #'monk--probe-os)
(monk-register-fact-probe 'env #'monk--probe-env)
(monk-register-fact-probe 'managers #'monk--probe-managers)
(monk-register-fact-probe 'local #'monk--probe-local)

(defun monk--class-symbol (string)
  "Return a class symbol for STRING."
  (intern (downcase (replace-regexp-in-string "[^[:alnum:]]+" "_" string))))

(cl-defmethod monk-hard-classes ((m monk-machine))
  "Return classes derived from the facts of M."
  (let ((classes nil))
    (dolist (key '(kernel os-family distribution distribution-release arch pkg-mgr service-mgr))
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
       ;; A machine that does not answer is not a broken probe.
       (when (monk-unreachable-error-p err)
         (signal (car err) (cdr err)))
       (monk-log 'warn "%s: fact probe %s failed: %s"
                 (oref m name) (car probe) (error-message-string err)))))
  (oset m classes (monk-hard-classes m))
  (oset m gathered (current-time))
  (oset m facts-source 'live)
  (monk-facts-store m)
  (run-hook-with-args 'monk-facts-ready-functions m)
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
   (tags :initarg :tags
         :initform nil
         :documentation "Symbols that select this resource from the command line.
`always' runs unless skipped by name; `never' runs only when named.
See `monk-tags-select-p'.  Ansible's tags.")
   (no-log :initarg :no-log
           :initform nil
           :documentation "When non-nil, the result's changes, diff, and comment are replaced
by `monk-censored' before anything records or shows them: the
ledger, the history, etcd, the socket, and the reports.  The status
and degree stay.  For a resource whose content is a secret.
Ansible's no_log.")
   (sudo :initarg :sudo
         :initform nil
         :documentation "Perform the resource as root."))
  :abstract t
  :documentation "Base class.  Subclasses implement `monk-check' and `monk-repair'.")

(defvar monk--check-evidence nil
  "What the check in progress saw, kept with its result even when nothing changed.
A `monk-check' method sets it with `monk-check-note'.  `monk--run-node-once'
moves it into the result's changes under :evidence, so a `kept' that
was wrong can be read back from the history instead of guessed at.")

(defun monk-check-note (&rest plist)
  "Record PLIST, keyword-value pairs, as evidence for the check in progress.
Later calls add keys.  Keep values small: they go into every history row."
  (while plist
    (setq monk--check-evidence (plist-put monk--check-evidence (pop plist) (pop plist)))))

(cl-defgeneric monk-check (resource machine)
  "Inspect MACHINE.  Return nil when RESOURCE is in the desired state.
Otherwise return a plist of changes that `monk-repair' must make.
The plist can hold :degree, a number from 0 to 1 that says how much
of the resource already holds.  This method must not change MACHINE.")

(cl-defgeneric monk-repair (resource machine changes)
  "Apply CHANGES from `monk-check' for RESOURCE on MACHINE.
Return a comment string or nil.")

(cl-defgeneric monk-diff (resource machine changes)
  "Return the diff for CHANGES of RESOURCE on MACHINE, or nil.
A list of `monk-diff-entry', one per before/after pair: a content
entry from `monk-diff-content', an attributes entry from
`monk-diff-attributes' for mode and ownership, or both.  A plain
unified-diff string is accepted too.  `monk-diff-render' turns any
of these into text at display time."
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
True for a resource whose content is the tree: a git checkout, an
rsync source.  Nil for a file, and nil for a directory too: a
`monk-directory' manages the directory's own mode and owner, which a
file written inside it cannot change.  Treating it as a subtree made
every scratch file under ~/.ssh a drift event (2026-10-01).")

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

(cl-defgeneric monk-resource-scope (resource)
  "Return the scope RESOURCE runs in on this process.
One of `user', `sudo', or `root'.")

(defun monk-check-scope (resource policy)
  "Signal `monk-policy-error' when RESOURCE and POLICY do not fit this process.
In a user process a policy declared `root' cannot run.  In a root
process a resource without `:sudo' acts in root's home, which is a
mistake unless the policy declared `root'."
  (let ((declared (and (symbolp policy) (monk-policy-scope policy))))
    (cond
     ((and (eq declared monk-scope-root) (not (monk-root-p)))
      (signal 'monk-policy-error
              (list (format "%s expects to run as root; this process is %s"
                            policy (monk-owner-string (monk-owner))))))
     ((and (monk-root-p) (not (eq declared monk-scope-root)) (not (oref resource sudo)))
      (signal 'monk-policy-error
              (list (format "%s: a user resource in a root process acts in root's home; declare (monk-policy-scope '%s 'root) if that is meant"
                            (oref resource name) (or policy "the policy"))))))))

(cl-defmethod monk-resource-scope ((r monk-resource))
  "Return `sudo' when R escalates, `root' when the process is root, else `user'."
  (cond ((monk-root-p) monk-scope-root)
        ((oref r sudo) monk-scope-sudo)
        (t monk-scope-user)))

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

;;;; unsupported: a placeholder that reports why

(defclass monk-unsupported-resource (monk-resource)
  ((reason :initarg :reason :type string))
  "A resource a generic constructor could not map on this machine.
It never runs; its check reports skipped with the reason.")

(cl-defmethod monk-resource-available-p ((r monk-unsupported-resource) (_m monk-machine))
  (oref r reason))

(cl-defmethod monk-check ((_r monk-unsupported-resource) (_m monk-machine)) nil)
(cl-defmethod monk-repair ((_r monk-unsupported-resource) (_m monk-machine) _changes) nil)

(defun monk-unsupported (name reason &rest args)
  "Add a resource NAME that is skipped with REASON.  ARGS: common keys."
  (monk-add-resource
   (apply #'monk-unsupported-resource :name name :reason reason (monk--common-initargs args))))

;;;; Compile phase

(defvar monk--compile-resources nil
  "Resources collected during the current compile, newest first.")

(defvar monk--compile-machine nil
  "Machine for the current compile.")

(defvar monk--compile-policy nil
  "The policy function being compiled, for `monk-check-scope'.")

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

(defvar monk-batch-policy nil
  "The policy function a batch entry point runs.  Set by the policy file.
Also what `monk-resource-origin-by-name' compiles when a name is unknown.")

;;;; Resource index
;;
;; Every compile records what it saw, so a bare name can be resolved
;; later: from an Embark action on a string in a policy file, from a
;; completion prompt, from a "who depends on this" question.  The index
;; is a side effect of compiling, never a source of truth: a plan or a
;; `monk-check-policy' refreshes it.

(defvar monk-resource-index (make-hash-table :test #'equal)
  "Resource name -> plist (:origin :type :machines :refs :seen).
:refs is an alist of (KIND . NAMES) this resource points at.")

(defun monk-index-resource (resource machine)
  "Record RESOURCE compiled for MACHINE in `monk-resource-index'."
  (let* ((name (oref resource name))
         (entry (gethash name monk-resource-index))
         (refs (delq nil (mapcar (lambda (kind)
                                   (let ((specs (slot-value resource kind)))
                                     (and specs (cons kind (mapcar (lambda (r) (monk-ref-name (monk-parse-ref r)))
                                                                   specs)))))
                                 '(require watch onchanges onfail prereq listen)))))
    (puthash name
             (list :origin (oref resource origin)
                   :type (monk-resource-type resource)
                   :machines (cl-adjoin (oref machine name) (plist-get entry :machines) :test #'equal)
                   :refs refs
                   :tags (oref resource tags)
                   :seen (current-time))
             monk-resource-index)))

(defun monk-resource-origin-by-name (name)
  "Return the (FILE . NAME) origin of the resource called NAME, or nil.
Answers from `monk-resource-index'.  When the index does not know
NAME and `monk-batch-policy' is set, the local policy is compiled once
with no connection to fill it, then the index is asked again."
  (or (plist-get (gethash name monk-resource-index) :origin)
      (and monk-batch-policy
           (progn (ignore-errors (monk-check-policy (monk-local-machine) monk-batch-policy))
                  (plist-get (gethash name monk-resource-index) :origin)))))

(defun monk-resource-names ()
  "Return every resource name the index knows, sorted."
  (let (names)
    (maphash (lambda (k _v) (push k names)) monk-resource-index)
    (sort names #'string<)))

(defun monk-resource-dependents-by-name (name)
  "Return an alist of (RESOURCE-NAME . KIND) for resources that point at NAME."
  (let (out)
    (maphash (lambda (other plist)
               (dolist (cell (plist-get plist :refs))
                 (when (member name (cdr cell))
                   (push (cons other (car cell)) out))))
             monk-resource-index)
    (sort out (lambda (a b) (string< (car a) (car b))))))

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
  (monk-index-resource resource monk--compile-machine)
  (monk-check-scope resource monk--compile-policy)
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
        (monk--compile-policy policy)
        (monk--compile-file (monk-definition-file policy)))
    (funcall policy m)
    (nreverse monk--compile-resources)))

(defconst monk--common-keys
  '(:only-if :not-if :classes :when :require :watch :watch-action :onchanges :onfail
    :prereq :listen :retry :order :fatal :handled :tags :no-log :sudo)
  "Keywords every resource constructor accepts.")

;;;; Tags
;;
;; Ansible's tag selection, `Taggable.evaluate_tags', without the
;; templating: a resource carries symbols, a run names the ones it
;; wants and the ones it skips.  The reserved names keep their Ansible
;; meaning so a policy ported from a playbook behaves the same.

(defconst monk-tag-always 'always
  "A resource with this tag runs unless a skip names it or `all'.")
(defconst monk-tag-never 'never
  "A resource with this tag runs only when a selection names it.")
(defconst monk-tag-all 'all
  "Selects every resource; as a skip, skips every one but `always'.")
(defconst monk-tag-tagged 'tagged
  "Selects every resource that has at least one tag.")
(defconst monk-tag-untagged 'untagged
  "Selects every resource that has no tag.")

(defun monk-tags-select-p (tags only skip)
  "Return non-nil when a resource with TAGS runs under ONLY and SKIP.
ONLY and SKIP are lists of symbols; nil ONLY selects everything.
Follows Ansible's `evaluate_tags': `always' runs unless skipped,
`never' runs only when named, `all', `tagged', and `untagged' select
by presence of tags."
  (let* ((tags (or tags (list monk-tag-untagged)))
         (tagged (not (equal tags (list monk-tag-untagged))))
         (run t))
    (when only
      (setq run (cond
                 ((memq monk-tag-always tags) t)
                 ((and (memq monk-tag-all only) (not (memq monk-tag-never tags))) t)
                 ((seq-intersection tags only) t)
                 ((and (memq monk-tag-tagged only) tagged (not (memq monk-tag-never tags))) t)
                 (t nil))))
    (when (and run skip)
      (cond
       ((memq monk-tag-all skip)
        (when (or (not (memq monk-tag-always tags)) (memq monk-tag-always skip))
          (setq run nil)))
       ((seq-intersection tags skip) (setq run nil))
       ((and (memq monk-tag-tagged skip) tagged) (setq run nil))))
    run))

(defun monk-tags-select-keys (nodes only skip)
  "Return the keys of NODES whose resource tags pass ONLY and SKIP."
  (mapcar #'monk-node-key
          (seq-filter (lambda (node)
                        (monk-tags-select-p (oref (monk-node-resource node) tags) only skip))
                      nodes)))

(defun monk-nodes-tags (nodes)
  "Return every tag on the resources of NODES, each once, sorted."
  (sort (seq-uniq (apply #'append (mapcar (lambda (n) (oref (monk-node-resource n) tags)) nodes)))
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

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

;;;; Declared parameters
;;
;; A resource constructor declares its parameters, as an Ansible
;; module declares its argument_spec.  The declaration is data, so
;; one place validates every call at compile time, fills defaults,
;; writes the docstring, and feeds the documentation and the
;; importers.  Before this, a misspelled keyword was silently ignored.
;;
;;   (monk-define-resource monk-file monk-file-resource
;;     "Ensure PATH has the given content."
;;     ((path :type path :doc "The file."))
;;     ((content :type string :doc "Exact content.")
;;      (source :type path :doc "Local file to copy from.")
;;      (mode :type mode)
;;      (owner :type string) (group :type string))
;;     :one-of (content source))
;;
;; Positional parameters come first; the first one is the resource
;; name unless a keyword parameter says `:name t'.  Keyword parameter
;; specs take :type, :required, :default, :choices, :aliases, :doc,
;; :no-log, and :slot when the initarg differs from the parameter.
;; Types: string, path, integer, number, boolean, mode (an integer
;; or nil), list, alist, symbol, function, any.

(define-error 'monk-argument-error "Monk argument error" 'monk-policy-error)

(defconst monk-parameter-types
  '(string path integer number boolean mode list alist symbol function any)
  "Types a declared parameter can name.")

(defun monk--parameter-type-ok-p (type value)
  "Return non-nil when VALUE fits TYPE, one of `monk-parameter-types'."
  (pcase type
    ((or 'string 'path) (stringp value))
    ('integer (integerp value))
    ('number (numberp value))
    ('boolean t)
    ('mode (or (null value) (integerp value)))
    ('list (listp value))
    ('alist (and (listp value) (cl-every #'consp value)))
    ('symbol (symbolp value))
    ('function (functionp value))
    ('any t)
    (_ (signal 'monk-argument-error (list (format "Unknown parameter type %S" type))))))

(defun monk--spec-key (spec)
  "Return the keyword for parameter SPEC, whose car is the parameter symbol."
  (intern (format ":%s" (car spec))))

(defun monk-validate-arguments (constructor args specs &optional one-of exclusive)
  "Check keyword ARGS of CONSTRUCTOR against SPECS.  Return the filled plist.
SPECS is a list of (NAME . PLIST) as in `monk-define-resource'.
Common keys pass through.  ONE-OF is a list of parameter names of
which at least one must be given; EXCLUSIVE a list of lists that
must not appear together.  Signal `monk-argument-error' with a
message in Ansible's words on an unknown key, a missing required
parameter, a wrong type, or a value outside :choices."
  (let* ((known (append monk--common-keys (mapcar #'monk--spec-key specs)
                        (mapcan (lambda (sp) (mapcar (lambda (a) (intern (format ":%s" a)))
                                                     (plist-get (cdr sp) :aliases)))
                                specs)))
         (out nil)
         (unknown nil))
    (unless (cl-evenp (length args))
      (signal 'monk-argument-error
              (list (format "%s: keyword arguments are not paired: %S" constructor args))))
    (cl-loop for (key _value) on args by #'cddr
             do (unless (memq key known) (push key unknown)))
    (when unknown
      (signal 'monk-argument-error
              (list (format "Unsupported parameter%s for %s: %s.  Supported parameters include: %s"
                            (if (cdr unknown) "s" "") constructor
                            (mapconcat #'symbol-name (nreverse unknown) ", ")
                            (mapconcat #'symbol-name
                                       (sort (copy-sequence (mapcar #'monk--spec-key specs))
                                             (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                                       ", ")))))
    (dolist (sp specs)
      (let* ((name (car sp))
             (plist (cdr sp))
             (key (monk--spec-key sp))
             (present (or (plist-member args key)
                          (seq-some (lambda (a) (plist-member args (intern (format ":%s" a))))
                                    (plist-get plist :aliases))))
             (value (cond ((plist-member args key) (plist-get args key))
                          (present (plist-get args (car present)))
                          (t (plist-get plist :default)))))
        ;; An explicit nil for an optional parameter means "not given",
        ;; as Ansible reads None: policies pass (plist-get entry :x)
        ;; through without checking.  The default then applies.
        (when (and present (null value) (not (plist-get plist :required))
                   (not (eq (plist-get plist :type) 'boolean)))
          (setq present nil
                value (plist-get plist :default)))
        (when (and (plist-get plist :required) (not present))
          (signal 'monk-argument-error
                  (list (format "%s: missing required parameter :%s" constructor name))))
        (when (and present (plist-get plist :type)
                   (not (monk--parameter-type-ok-p (plist-get plist :type) value)))
          (signal 'monk-argument-error
                  (list (format "%s: :%s must be %s, got %S" constructor name
                                (plist-get plist :type) value))))
        (when (and present (plist-get plist :choices)
                   (not (member value (plist-get plist :choices))))
          (signal 'monk-argument-error
                  (list (format "%s: :%s must be one of %s, got %S" constructor name
                                (mapconcat (lambda (c) (format "%S" c)) (plist-get plist :choices) ", ")
                                value))))
        (when (or present (plist-member plist :default))
          (setq out (plist-put out (intern (format ":%s" (or (plist-get plist :slot) name))) value)))))
    (when (and one-of (not (seq-some (lambda (n) (plist-member args (intern (format ":%s" n)))) one-of)))
      (signal 'monk-argument-error
              (list (format "%s: one of %s is required" constructor
                            (mapconcat (lambda (n) (format ":%s" n)) one-of ", ")))))
    (dolist (group exclusive)
      (when (> (seq-count (lambda (n) (plist-member args (intern (format ":%s" n)))) group) 1)
        (signal 'monk-argument-error
                (list (format "%s: %s are mutually exclusive" constructor
                              (mapconcat (lambda (n) (format ":%s" n)) group ", "))))))
    out))

(defun monk--wrap-doc (text &optional indent)
  "Return TEXT filled to 78 columns, continuation lines after INDENT."
  (with-temp-buffer
    (insert text)
    (let ((fill-column 78)
          (fill-prefix (or indent "  ")))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun monk--parameter-doc (specs)
  "Return docstring lines that describe SPECS, each wrapped."
  (mapconcat
   (lambda (sp)
     (let ((plist (cdr sp)))
       (monk--wrap-doc
        (format ":%s%s%s%s%s%s" (car sp)
                (if (plist-get plist :type) (format " %s" (upcase (symbol-name (plist-get plist :type)))) "")
                (if (plist-get plist :required) " (required)" "")
                (if (plist-member plist :default) (format " (default %S)" (plist-get plist :default)) "")
                (if (plist-get plist :choices)
                    (format " one of %s" (mapconcat (lambda (c) (format "%S" c)) (plist-get plist :choices) ", "))
                  "")
                (if (plist-get plist :doc) (format ": %s" (plist-get plist :doc)) "")))))
   specs "\n"))

(defvar monk-resource-specs nil
  "Alist of (CONSTRUCTOR . (POSITIONAL KEYWORDS)) for every declared resource.
Read by the documentation and by importers that map a foreign
module's arguments onto a Monk constructor.")

(defmacro monk-define-resource (name class doc positional keywords &rest options)
  "Define constructor NAME for resource CLASS with declared parameters.
DOC is the first docstring line.  POSITIONAL is a list of (PARAM
&rest PLIST) taken in order; the first is the resource name unless
one has `:name t'.  KEYWORDS is a list of (PARAM &rest PLIST) as for
`monk-validate-arguments'; a PLIST may hold :slot INITARG when the
class initarg differs from the parameter.  OPTIONS: :one-of NAMES,
:exclusive GROUPS, :classes LIST the type needs, used unless the call
gives its own :classes, :sudo t to force sudo, :name-form FORM to
compute the resource name from the parameters instead of taking the
first positional one, :init FORMS to run with the validated plist
bound to `args' before the instance is made.  The body of the
constructor validates, fills defaults, and calls `monk-add-resource'."
  (declare (indent 2) (doc-string 3))
  (let* ((one-of (plist-get options :one-of))
         (exclusive (plist-get options :exclusive))
         (fixed-classes (plist-get options :classes))
         (force-sudo (plist-get options :sudo))
         (init (plist-get options :init))
         (name-form (plist-get options :name-form))
         (pos-names (mapcar #'car positional))
         (name-param (and (not name-form)
                          (or (car (seq-find (lambda (sp) (plist-get (cdr sp) :name)) positional))
                              (car pos-names))))
         (full-doc (concat doc "\n"
                           (mapconcat (lambda (sp)
                                        (monk--wrap-doc
                                         (format "%s%s" (upcase (symbol-name (car sp)))
                                                 (if (plist-get (cdr sp) :doc)
                                                     (format ": %s" (plist-get (cdr sp) :doc))
                                                   ""))))
                                      positional "\n")
                           (if positional "\n" "")
                           (if keywords (concat "Keyword parameters:\n" (monk--parameter-doc keywords) "\n") "")
                           (monk--wrap-doc
                            (concat "Common keys: " (mapconcat #'symbol-name monk--common-keys " ") ".")))))
    `(progn
       (setf (alist-get ',name monk-resource-specs) (list ',positional ',keywords))
       (defun ,name (,@pos-names &rest rest)
         ,full-doc
         ,@(mapcar (lambda (sp)
                     `(unless (monk--parameter-type-ok-p ',(or (plist-get (cdr sp) :type) 'any) ,(car sp))
                        (signal 'monk-argument-error
                                (list (format "%s: %s must be %s, got %S" ',name ',(car sp)
                                              ',(or (plist-get (cdr sp) :type) 'any) ,(car sp))))))
                   positional)
         ;; A positional parameter may itself be called `args' or
         ;; `common'; the initargs read it under its own name, so the
         ;; validated plists bind under names no parameter can take.
         (let* ((monk--validated-keywords (monk-validate-arguments ',name rest ',keywords ',one-of ',exclusive))
                (monk--validated-common (monk--common-initargs rest))
                ,@(mapcar (lambda (sp) `(,(intern (format "monk--positional-%s" (car sp))) ,(car sp)))
                          (seq-remove (lambda (sp) (eq (car sp) name-param)) positional)))
           ;; The type's own classes apply unless the call gives
           ;; :classes itself, which then stands alone: a test or a
           ;; policy may want brew on a host Monk calls something else.
           ,@(when fixed-classes
               `((unless (plist-member rest :classes)
                   (setq monk--validated-common (plist-put monk--validated-common :classes ',fixed-classes)))))
           ,@(when force-sudo `((setq monk--validated-common (plist-put monk--validated-common :sudo t))))
           ;; :init and :name-form read the validated keywords as
           ;; `args' and the common keys as `common', and may rewrite
           ;; them.  A positional parameter named `args' is shadowed
           ;; inside them, which is why the initargs use the copies.
           (let ((args monk--validated-keywords)
                 (common monk--validated-common))
             (ignore args common)
             ,@init
             (monk-add-resource
              (apply #',class :name ,(or name-form name-param)
                     (append ,@(mapcar (lambda (sp)
                                         (let ((slot (or (plist-get (cdr sp) :slot) (car sp))))
                                           (and (not (eq (car sp) name-param))
                                                `(list ,(intern (format ":%s" slot))
                                                       ,(intern (format "monk--positional-%s" (car sp)))))))
                                       positional)
                             args common)))))))))

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

(defconst monk-censored "[censored: no-log]"
  "What replaces the changes, diff, and comment of a :no-log result.")

(defun monk-result-censor (result)
  "Replace what RESULT reveals with `monk-censored'.  Return RESULT.
Status, degree, name, machine, and duration stay: they say what
happened without saying what the content was."
  (setf (monk-result-changes result) (and (monk-result-changes result) (list :censored t))
        (monk-result-diff result) nil
        (monk-result-comment result) (if (monk-result-failed-p result)
                                         (format "%s failed" monk-censored)
                                       monk-censored)
        (monk-result-censored result) t)
  result)

(defconst monk-skip-not-selected "not selected for this run"
  "Comment on a node that a partial run left alone.")

(defun monk--node-dependents (node nodes)
  "Return the nodes in NODES that name NODE in a requisite."
  (seq-filter (lambda (other)
                (and (not (eq other node))
                     (memq node (monk--node-dependencies other nodes))))
              nodes))

(defun monk--node-owned-paths (node)
  "Return the expanded paths that NODE's resource owns on its machine."
  (let ((m (monk-node-machine node)))
    (mapcar (lambda (p) (monk-machine-expand m p))
            (monk-resource-paths (monk-node-resource node)))))

(defun monk--nodes-sharing-paths (node nodes)
  "Return the nodes in NODES on the same machine that own a path of NODE.
A path is shared when it is equal, or when one side owns the subtree
that contains the other's path.  A sync that reads ~/.zshrc must run
when the resource that owns ~/.zshrc changed, requisite or not."
  (let ((paths (monk--node-owned-paths node))
        (m (monk-node-machine node)))
    (seq-filter
     (lambda (other)
       (and (not (eq other node))
            (eq (monk-node-machine other) m)
            (let ((theirs (monk--node-owned-paths other))
                  (they-own-subtree (monk-resource-owns-subtree-p (monk-node-resource other)))
                  (we-own-subtree (monk-resource-owns-subtree-p (monk-node-resource node))))
              (seq-some (lambda (mine)
                          (seq-some (lambda (their)
                                      (or (equal mine their)
                                          (and they-own-subtree
                                               (string-prefix-p (file-name-as-directory their) mine))
                                          (and we-own-subtree
                                               (string-prefix-p (file-name-as-directory mine) their))))
                                    theirs))
                        paths))))
     nodes)))

(defun monk-select-nodes (nodes keys)
  "Select the nodes in NODES whose key is in KEYS, and what depends on them.
Two kinds of dependents close transitively: resources that name a
selected one in a requisite, and resources that own one of its paths,
such as a sync that copies a file another resource writes.  Every
other node is deselected and will be skipped.  Return the selected."
  (let ((selected nil)
        (frontier (seq-filter (lambda (n) (member (monk-node-key n) keys)) nodes)))
    (while frontier
      (let ((node (pop frontier)))
        (unless (memq node selected)
          (push node selected)
          (setq frontier (append frontier
                                 (monk--node-dependents node nodes)
                                 (monk--nodes-sharing-paths node nodes))))))
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

(defun monk--unreachable-ref (node nodes)
  "Return the unreachable machine among NODES that a ref of NODE points at, or nil."
  (catch 'found
    (dolist (kind '(require watch listen))
      (dolist (ref (monk-resource-refs (monk-node-resource node) kind))
        (dolist (target (monk--nodes-for-ref ref node nodes))
          (let ((r (monk-node-result target)))
            (when (and r (eq (monk-result-status r) monk-status-unreachable))
              (throw 'found (oref (monk-node-machine target) name)))))))
    nil))

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
                                 detail (let ((dark (monk--unreachable-ref node nodes)))
                                          (if dark
                                              (format "%s %s: %s is unreachable" kind (monk-ref-name ref) dark)
                                            (format "%s %s not satisfied" kind (monk-ref-name ref)))))))
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

(defconst monk-unreachable-error-regexp
  (rx (or "Connection refused" "No route to host" "Could not resolve hostname"
          "Network is unreachable" "Connection timed out" "Operation timed out"
          "Host is down" "Timeout" "not reachable" "ssh: connect to host"
          "Tramp failed to connect" "Couldn't find a proper `ls' command"
          "Login failed" "Host key verification failed" "Permission denied (publickey"
          "nodename nor servname provided" "Name or service not known"))
  "Error text that means the machine, not the resource, is the problem.")

(defun monk-unreachable-error-p (err)
  "Return non-nil when error condition ERR says the machine did not answer.
TRAMP signals `file-error' with the ssh text; `monk-exec-error' from a
remote command carries exit 255 from ssh."
  (or (and (eq (car err) 'monk-exec-error) (eql 255 (nth 3 err)))
      ;; `remote-file-error' and TRAMP's own conditions derive from
      ;; `file-error'; test the hierarchy, not the exact symbol.
      (and (or (memq 'file-error (get (car err) 'error-conditions))
               (eq (car err) 'monk-exec-error))
           (string-match-p monk-unreachable-error-regexp (error-message-string err)))))

(defun monk--machine-unreachable-p (machine)
  "Return the reason MACHINE was found unreachable in the current run, or nil."
  (and monk--current-run (monk-run-unreachable-p monk--current-run (oref machine name))))

(defun monk-unreachable-reason (text)
  "Return the part of error TEXT that says why the machine did not answer.
One line, without TRAMP's advice to clean up and without the command
that happened to hit the wall: the whole message is in the log."
  (let ((line (string-trim (car (split-string text "\n")))))
    ;; `Monk command failed: "h", ("uname" "-s"), 255, "ssh: ..."' →
    ;; the quoted ssh text at the end.
    (if (string-match "255, \"\\(.*\\)\"\\'" line)
        (match-string 1 line)
      line)))

(defun monk--mark-unreachable (machine reason)
  "Record in the current run that MACHINE did not answer, with REASON."
  (when monk--current-run
    (unless (monk-run-unreachable-p monk--current-run (oref machine name))
      (monk-log 'warn "%s: unreachable: %s" (oref machine name) reason)
      (push (cons (oref machine name) (monk-unreachable-reason reason))
            (monk-run-unreachable monk--current-run)))))

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
                                     :scope (monk-resource-scope resource)
                                     :origin (oref resource origin))))
    (monk-progress 'node-start :node node)
    (pcase-let ((`(,verdict . ,detail)
                 (let* ((mock (and monk--current-run (monk-run-mock monk--current-run)))
                        (dark (monk--machine-unreachable-p machine))
                        (reason (and (not dark) (monk-node-selected node)
                                     (or (condition-case err
                                             (monk-resource-skip-reason resource machine)
                                           (error (if (monk-unreachable-error-p err)
                                                      (progn (monk--mark-unreachable machine (error-message-string err))
                                                             (setq dark (error-message-string err))
                                                             nil)
                                                    (signal (car err) (cdr err)))))
                                         ;; A mock run opens no connection,
                                         ;; so it cannot ask what is installed.
                                         (and (not mock) (not dark)
                                              (condition-case err
                                                  (monk-resource-available-p resource machine)
                                                (error (if (monk-unreachable-error-p err)
                                                           (progn (monk--mark-unreachable machine (error-message-string err))
                                                                  (setq dark (error-message-string err))
                                                                  nil)
                                                         (signal (car err) (cdr err))))))))))
                   (cond
                    (dark (cons 'unreachable dark))
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
       ((eq verdict 'unreachable)
        (setf (monk-result-status result) monk-status-unreachable
              (monk-result-comment result) (monk-unreachable-reason detail)))
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
    (when (oref resource no-log)
      (monk-result-censor result))
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
                     (eq (monk-result-status result) monk-status-unreachable)
                     (pcase until
                       ('failure (monk-result-failed-p result))
                       ((pred functionp) (funcall until result))
                       (_ (not (monk-result-failed-p result)))))))
    (when (and plan (> attempt 1))
      (setf (monk-result-comment result)
            (format "%s (attempt %d of %d)" (or (monk-result-comment result) "") attempt attempts)))
    (when (and plan dry-run)
      (setf (monk-result-comment result)
            (concat (or (monk-result-comment result) "")
                    (format " [retry up to %d]" attempts))))
    result))

(defun monk--changes-without-evidence (changes)
  "Return CHANGES without the :evidence pair.
An all-evidence plist then reads as no change."
  (let ((rest (copy-sequence changes)))
    (cl-remf rest :evidence)
    rest))

(defun monk--run-node-once (resource machine result verdict detail dry-run)
  "One check-and-repair of RESOURCE on MACHINE into RESULT.
VERDICT and DETAIL come from `monk--requisite-verdict'.  With DRY-RUN,
report only.  After a repair, `monk-resource-verify' can still fail it."
  (condition-case err
      (let* ((monk--check-evidence nil)
             (changes (monk-check resource machine))
             (evidence monk--check-evidence)
             (degree (or (plist-get changes :degree) 0.0)))
        (when evidence
          (setq changes (plist-put (copy-sequence changes) :evidence evidence)))
        (cond
         ((and (null (monk--changes-without-evidence changes)) (eq verdict 'watch))
          (monk--perform-watch resource machine result detail dry-run))
         ((null (monk--changes-without-evidence changes))
          (setf (monk-result-status result) monk-status-kept
                (monk-result-degree result) monk-degree-satisfied
                (monk-result-comment result) nil
                (monk-result-changes result) (and evidence (list :evidence evidence))))
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
          (condition-case unchanged
              (setf (monk-result-comment result) (monk-repair resource machine changes)
                    (monk-result-status result) monk-status-repaired
                    (monk-result-degree result) monk-degree-satisfied
                    (monk-result-changes result) changes)
            ;; A query command ran and its predicate said nothing
            ;; changed: kept, with the output as the comment.
            (monk-command-unchanged
             (setf (monk-result-status result) monk-status-kept
                   (monk-result-degree result) monk-degree-satisfied
                   (monk-result-comment result) (cadr unchanged)
                   (monk-result-changes result) nil)))
          (let ((reason (monk-resource-verify resource machine)))
            (when reason
              ;; The repair ran, but the verification says the system is
              ;; still wrong: that is a failure, whatever the degree.
              (setf (monk-result-degree result) 0.0)
              (signal 'monk-resource-error
                      (list (format "repaired, but verify failed: %s" reason))))))))
    (error
     (if (monk-unreachable-error-p err)
         (progn
           (monk--mark-unreachable machine (error-message-string err))
           (setf (monk-result-status result) monk-status-unreachable
                 (monk-result-degree result) 0.0
                 (monk-result-comment result) (monk-unreachable-reason (error-message-string err))))
       (setf (monk-result-status result)
             (if (> (monk-result-degree result) 0.0) monk-status-partial monk-status-failed)
             (monk-result-comment result) (error-message-string err)))
     ;; The result itself carries the failure; reports and the CLI
     ;; print it.  Logged at info so a terminal does not see it twice.
     (monk-log 'info "%s: %s %s %s: %s"
               (oref machine name) (monk-resource-type resource)
               (oref resource name) (monk-result-status result)
               (if (oref resource no-log) monk-censored (error-message-string err)))))
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
                                                 :type (monk-resource-type aggregate)
                                                 :scope (monk-resource-scope aggregate))))
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
                                            :scope (monk-resource-scope r)
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
           (when (oref r no-log)
             (monk-result-censor result))
           (setf (monk-node-result node) result)
           (monk-progress 'node-end :node node :result result)
           (monk--run-result-functions result)
           result))
       group skips))))

(defvar monk--batch-machines nil
  "Machines the current batch acts on, or nil for all.
Bound by `monk--run-batches'.  Nodes on other machines keep the
result of their own batch, so requisites across batches resolve.")

(defun monk--in-batch-p (node)
  "Return non-nil when NODE's machine acts in the current batch."
  (or (null monk--batch-machines) (memq (monk-node-machine node) monk--batch-machines)))

(defun monk--pass (nodes dry-run)
  "Run one pass over NODES in order.  Return a `monk-pass'.
Results from earlier passes are cleared first so requisites read this
pass only.  When DRY-RUN is non-nil, report only.  Every node runs:
a failure never stops the pass, except after a :fatal failure, which
ends the pass on that machine only.  Nodes outside the current batch
are left as they are."
  (dolist (node nodes)
    (when (monk--in-batch-p node)
      (setf (monk-node-result node) nil)))
  (let (results (repaired 0) (repaired-keys nil) (fatal-machines nil))
    (dolist (item (seq-filter (lambda (item) (monk--in-batch-p (if (listp item) (car item) item)))
                              (monk--aggregate-groups nodes)))
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
                                                        :scope (monk-resource-scope r)
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
        (when (and refs (monk-node-selected node) (monk--in-batch-p node)
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
                                                :scope (monk-resource-scope resource)
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

;;;; Batches
;;
;; Ansible's `serial': a fleet change goes to a few machines first.  A
;; batch is a list of machines; the graph is one, so requisites across
;; batches still resolve, but a later batch waits for the earlier one
;; to finish, and a batch that fails past the threshold stops the ones
;; after it.  Failures count per batch, not cumulatively: the review
;; found Ansible's cumulative count to be an accident of the code.

(defun monk-serial-count (spec total)
  "Return how many of TOTAL machines SPEC selects for one batch.
SPEC is an integer or a string percentage such as `25%'.  A
percentage rounds down but never below one.  Ansible's `pct_to_int'."
  (cond
   ((integerp spec) spec)
   ((and (stringp spec) (string-match "\\`\\([0-9]+\\)%\\'" spec))
    (max 1 (floor (* total (/ (string-to-number (match-string 1 spec)) 100.0)))))
   (t (signal 'monk-policy-error (list "Bad :serial entry" spec)))))

(defun monk-serial-batches (machines serial)
  "Split MACHINES into batches by SERIAL.
SERIAL is nil, one spec, or a list of specs; the last spec repeats
until every machine has a batch.  A spec of zero or less puts all
remaining machines in one batch.  Ansible's `_get_serialized_batches'."
  (let ((specs (cond ((null serial) (list 0))
                     ((listp serial) serial)
                     (t (list serial))))
        (rest (copy-sequence machines))
        (total (length machines))
        (batches nil))
    (while rest
      (let ((count (monk-serial-count (car specs) total)))
        (if (<= count 0)
            (progn (push rest batches) (setq rest nil))
          (push (seq-take rest count) batches)
          (setq rest (seq-drop rest count))))
      (when (cdr specs) (setq specs (cdr specs))))
    (nreverse batches)))

(defun monk-batch-failed-machines (run machines)
  "Return the machines in MACHINES with a failed result or unreachable in RUN."
  (seq-filter (lambda (m)
                (let ((name (oref m name)))
                  (or (assoc name (monk-run-unreachable run))
                      (seq-some (lambda (r)
                                  (and (equal (monk-result-machine r) name)
                                       (monk-result-failed-p r)
                                       (not (member (monk-result-key r) (monk-run-rescued run)))))
                                (monk-run-results run)))))
              machines))

(defun monk-batch-over-threshold-p (failed total percentage)
  "Return non-nil when FAILED of TOTAL machines is more than PERCENTAGE.
The comparison is strictly greater, as in Ansible."
  (and percentage (> total 0)
       (> (* 100.0 (/ failed (float total))) percentage)))

(defconst monk-skip-batch "an earlier batch failed past :max-fail-percentage"
  "Skip comment on every node of a batch that never started.")

(defun monk--skip-batch (nodes machines)
  "Give every node of NODES on MACHINES a skipped result with `monk-skip-batch'."
  (dolist (node nodes)
    (when (memq (monk-node-machine node) machines)
      (let ((r (monk-node-resource node)) (m (monk-node-machine node)))
        (setf (monk-node-result node)
              (monk-result-create :machine (oref m name) :name (oref r name)
                                  :type (monk-resource-type r) :origin (oref r origin)
                                  :scope (monk-resource-scope r)
                                  :status monk-status-skipped :comment monk-skip-batch))))))

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
    (dolist (cell monk--unreachable-at-compile)
      (monk-log 'warn "%s: unreachable: %s" (car cell) (cdr cell))
      (push cell (monk-run-unreachable run)))
    (setq monk--unreachable-at-compile nil)
    (monk-progress 'run-start :run run :nodes (seq-filter #'monk--in-batch-p ordered))
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
      (monk-log 'info "flapping: %s"
                (mapconcat (lambda (key) (format "%s:%s" (car key) (cdr key)))
                           (monk-run-flapping run) ", ")))
    (when (and (monk-run-oscillating-p run) (not (monk-run-flapping-p run)))
      (monk-log 'warn "did not converge after %d passes" pass))
    (monk-log 'info "run end: %s" (monk-run-summary run))
    run))

(defun monk--nodes-for (machines policy)
  "Gather facts, compile POLICY, and return nodes for MACHINES.
A machine whose facts cannot be gathered because it does not answer
is recorded in `monk--unreachable-at-compile'; its policy still
compiles with the facts it has, so its nodes exist and report
unreachable, and requisites elsewhere can name it."
  (let ((nodes nil) (index 0))
    (setq monk--unreachable-at-compile nil)
    (dolist (m machines)
      (unless (oref m facts)
        ;; Fresh cached facts spare a connection; a stale or empty
        ;; cache means gathering, and an unreachable machine then falls
        ;; back to whatever the cache had, stale as it is, so its
        ;; policy still compiles with the right classes.
        (unless (monk-facts-load m)
          (condition-case err
              (monk-gather-facts m)
            (error
             (if (monk-unreachable-error-p err)
                 (progn
                   (push (cons (oref m name) (error-message-string err)) monk--unreachable-at-compile)
                   (let ((monk-fact-ttl most-positive-fixnum))
                     (monk-facts-load m))
                   (oset m classes (monk-hard-classes m)))
               (signal (car err) (cdr err)))))))
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
              (only (plist-get options :only))
              (tags (plist-get options :tags))
              (skip-tags (plist-get options :skip-tags))
              (serial (plist-get options :serial))
              (max-fail (plist-get options :max-fail-percentage)))
    (monk-log 'info "%s %d machine(s)%s%s%s"
              (if dry-run "plan" "apply") (length machines)
              (if dry-run " (dry run)" "")
              (if only (format ", only %d key(s) and dependents" (length only)) "")
              (if (or tags skip-tags) (format ", tags %S skip %S" tags skip-tags) ""))
    (let ((nodes (monk--nodes-for machines policy)))
      (when (or tags skip-tags)
        ;; Tag selection narrows :only, so requisites still pull in
        ;; what a selected resource needs.
        (let ((tagged (monk-tags-select-keys nodes tags skip-tags)))
          (setq only (if only (seq-filter (lambda (k) (member k tagged)) only) tagged))
          (unless only (setq only (list (cons nil nil))))))
      (when only
        (monk-select-nodes nodes only))
      (if (or (null serial) (null (cdr machines)))
          (monk--run-nodes machines nodes dry-run max-passes policy options)
        (monk--run-batches machines nodes serial max-fail dry-run max-passes policy options)))))

(defun monk--run-batches (machines nodes serial max-fail dry-run max-passes policy options)
  "Run NODES for MACHINES in the batches SERIAL gives.  Return one `monk-run'.
Each batch is a run over its machines only; the runs merge into one
with the same policy and options.  After a batch, when the share of
its machines that failed or were unreachable is more than MAX-FAIL
percent, the remaining batches are skipped with `monk-skip-batch'."
  (let* ((batches (monk-serial-batches machines serial))
         (merged nil)
         (index 0)
         (stopped nil))
    (monk-log 'info "serial: %d batch(es) of %s"
              (length batches) (mapconcat (lambda (b) (number-to-string (length b))) batches ", "))
    (dolist (batch batches)
      (cl-incf index)
      (if stopped
          (monk--skip-batch nodes batch)
        (let* ((run (let ((monk--batch-machines batch))
                      (monk--run-nodes batch nodes dry-run max-passes policy options)))
               (failed (length (monk-batch-failed-machines run batch))))
          (monk-log 'info "batch %d/%d: %d of %d machine(s) failed" index (length batches) failed (length batch))
          (when (and (not dry-run) (monk-batch-over-threshold-p failed (length batch) max-fail))
            (monk-log 'warn "batch %d: %d of %d failed, over %s%%; later batches are skipped"
                      index failed (length batch) max-fail)
            (setq stopped index))
          (setq merged (if merged (monk--merge-runs merged run) run)))))
    (when stopped
      (setf (monk-run-results merged)
            (append (monk-run-results merged)
                    (delq nil (mapcar (lambda (n) (and (equal (monk-result-comment (monk-node-result n)) monk-skip-batch)
                                                       (monk-node-result n)))
                                      (seq-filter #'monk-node-result nodes))))))
    (setf (monk-run-machines merged) machines
          (monk-run-batches merged) (mapcar (lambda (b) (mapcar (lambda (m) (oref m name)) b)) batches)
          (monk-run-stopped-at-batch merged) stopped)
    merged))

(defun monk--merge-runs (into run)
  "Fold RUN's results and bookkeeping into INTO.  Return INTO."
  (setf (monk-run-results into) (append (monk-run-results into) (monk-run-results run))
        (monk-run-passes into) (max (monk-run-passes into) (monk-run-passes run))
        (monk-run-rescued into) (append (monk-run-rescued into) (monk-run-rescued run))
        (monk-run-unreachable into) (append (monk-run-unreachable into) (monk-run-unreachable run))
        (monk-run-repaired-by-pass into) (append (monk-run-repaired-by-pass into) (monk-run-repaired-by-pass run))
        (monk-run-last-pass-repaired into) (+ (monk-run-last-pass-repaired into) (monk-run-last-pass-repaired run))
        (monk-run-end-time into) (monk-run-end-time run))
  into)

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
          ((or :degree :diff :evidence) nil)
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
          (:rebuild (push (format "rebuild %s" (mapconcat (lambda (c) (format "%s: %s" (car c) (cdr c))) value ", "))
                          parts))
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
          (:censored (push monk-censored parts))
          (_ (push (format "%s %s" (string-remove-prefix ":" (symbol-name key))
                           (monk--describe-value value))
                   parts)))))
    (string-join (nreverse parts) ", ")))

;;;; Text reporting

(defun monk-run-unselected-count (run &optional machine)
  "Return how many results of RUN, or of MACHINE, a partial run left out.
Those carry `monk-skip-not-selected': a watch or :only did not select
them, so their skip says nothing about the machine."
  (seq-count (lambda (r) (and (eq (monk-result-status r) monk-status-skipped)
                              (equal (monk-result-comment r) monk-skip-not-selected)
                              (or (null machine) (equal (monk-result-machine r) machine))))
             (monk-run-results run)))

(defun monk-run-summary (run &optional machine)
  "Return a one-line summary of RUN, or of MACHINE within it.
In a partial run the resources left out are not counted as skipped;
they are named apart, as `N not selected'."
  (let ((unreachable (monk-run-count run monk-status-unreachable machine))
        (unselected (monk-run-unselected-count run machine)))
    (format "%d kept, %d repaired, %d would repair, %d partial, %d failed, %d skipped%s%s, %d pass%s"
            (monk-run-count run monk-status-kept machine)
            (monk-run-count run monk-status-repaired machine)
            (monk-run-count run monk-status-would-repair machine)
            (monk-run-count run monk-status-partial machine)
            (monk-run-count run monk-status-failed machine)
            (- (monk-run-count run monk-status-skipped machine) unselected)
            (if (> unreachable 0) (format ", %d unreachable" unreachable) "")
            (if (> unselected 0) (format ", %d not selected" unselected) "")
            (monk-run-passes run)
            (if (= 1 (monk-run-passes run)) "" "es"))))

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
            (let ((text (monk-result-diff-text result)))
              (when text (insert text)))))))
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

;;;; Compile only

(defun monk-check-policy (machines policy &rest options)
  "Compile POLICY for MACHINES without a connection.  Return a plist.
Every machine is compiled with the facts it has: the cache, or none.
The plist holds :nodes, the compiled nodes; :tags, every tag in use;
:machines, the names; :errors, an alist of (MACHINE-NAME . MESSAGE)
for machines whose policy did not compile; and :unresolved, requisite
errors from ordering the graph.  OPTIONS take :tags and :skip-tags to
also report :selected, the keys tag selection would run.  Ansible's
`--syntax-check', `--list-tasks', and `--list-tags' in one call."
  (let ((machines (if (listp machines) machines (list machines)))
        (monk-fact-ttl most-positive-fixnum)
        (nodes nil) (errors nil) (unresolved nil) (index 0))
    (dolist (m machines)
      (unless (oref m facts)
        (monk-facts-load m)
        (oset m classes (monk-hard-classes m)))
      (condition-case err
          (dolist (resource (monk-compile m policy))
            (push (monk-node-create :machine m :resource resource :index index) nodes)
            (cl-incf index))
        (error (push (cons (oref m name) (error-message-string err)) errors))))
    (setq nodes (nreverse nodes))
    (condition-case err
        (monk-schedule nodes)
      (error (push (error-message-string err) unresolved)))
    (list :nodes nodes
          :machines (mapcar (lambda (m) (oref m name)) machines)
          :tags (monk-nodes-tags nodes)
          :errors (nreverse errors)
          :unresolved (nreverse unresolved)
          :selected (and (or (plist-get options :tags) (plist-get options :skip-tags))
                         (monk-tags-select-keys nodes (plist-get options :tags)
                                                (plist-get options :skip-tags))))))

(defun monk-check-policy-ok-p (report)
  "Return non-nil when REPORT from `monk-check-policy' found no problem."
  (and (null (plist-get report :errors)) (null (plist-get report :unresolved))))

(defun monk-check-policy-string (report)
  "Return REPORT from `monk-check-policy' as text, one resource per line."
  (with-temp-buffer
    (let ((by-machine (seq-group-by (lambda (n) (oref (monk-node-machine n) name))
                                    (plist-get report :nodes))))
      (dolist (name (plist-get report :machines))
        (insert (format "== %s: %d resource(s)\n" name (length (alist-get name by-machine nil nil #'equal))))
        (dolist (node (alist-get name by-machine nil nil #'equal))
          (let ((r (monk-node-resource node)))
            (insert (format "  %-12s %s%s\n" (monk-resource-type r) (oref r name)
                            (if (oref r tags) (format "  [%s]" (mapconcat #'symbol-name (oref r tags) " ")) "")))))))
    (when (plist-get report :tags)
      (insert (format "tags: %s\n" (mapconcat #'symbol-name (plist-get report :tags) " "))))
    (dolist (cell (plist-get report :errors))
      (insert (format "error: %s: %s\n" (car cell) (cdr cell))))
    (dolist (text (plist-get report :unresolved))
      (insert (format "unresolved: %s\n" text)))
    (buffer-string)))

;;;; Batch entry point


(defun monk-batch-check (policy-file &rest machine-args)
  "Load POLICY-FILE and compile it for the local machine, with no run.
MACHINE-ARGS are passed to `monk-local-machine'.  Print the report and
exit 1 when the policy does not compile or a requisite does not
resolve.  For CI."
  (load policy-file nil t)
  (let* ((policy (or monk-batch-policy
                     (signal 'monk-policy-error
                             (list "monk-batch-policy is not set by" policy-file))))
         (report (monk-check-policy (apply #'monk-local-machine machine-args) policy)))
    (dolist (line (split-string (monk-check-policy-string report) "\n" t))
      (funcall monk-batch-print-function line))
    (unless (monk-check-policy-ok-p report)
      (kill-emacs 1))
    report))

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
    (let ((code (monk-run-exit-code run)))
      (unless (zerop code)
        (kill-emacs code)))
    run))

(monk-note-load 'monk)
(provide 'monk)
;;; monk.el ends here
