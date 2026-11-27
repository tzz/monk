;;; monk-package.el --- Package and macOS resources for Monk  -*- lexical-binding: t; -*-

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

;; Resources: `monk-package' (any manager), and per manager `monk-brew',
;; `monk-apt', `monk-dnf', `monk-yum', `monk-pacman', `monk-zypper',
;; `monk-apk', `monk-pkg'; plus `monk-brew-tap' and `monk-osx-default'.
;;
;; One class, `monk-package-resource', holds what every manager shares:
;; a set of names, a state (present, absent, latest), and holds.  Each
;; manager is a subclass that answers a small protocol: which names
;; are installed, which have an update, which are held, and the
;; command lines for install, remove, upgrade, hold, and unhold.  The
;; check and the repair live on the base class once; a manager adds
;; only what is its own (brew: casks, rebuilds, tap trust, orphaned
;; kegs; apt: the index refresh, DEBIAN_FRONTEND).  Package resources
;; of one class and state on a machine aggregate into one command.
;;
;; `monk-package' is the policy-level entry: it picks the subclass from
;; the machine's `pkg-mgr' fact and maps names per manager, so one line
;; serves a Mac and a Pi; the per-manager constructors are for the
;; options only one manager has.

;;; Code:

(require 'monk)

(defconst monk-brew-program "brew")
(defconst monk-apt-program "apt-get")
(defconst monk-dpkg-query-program "dpkg-query")
(defconst monk-defaults-program "defaults")

(defun monk-set-degree (total missing)
  "Return the degree for a set resource with TOTAL members and MISSING absent."
  (if (zerop total)
      monk-degree-satisfied
    (/ (float (- total missing)) total)))

;;;; The package protocol: one base class, one subclass per manager

(defconst monk-package-states '(present absent latest)
  "What a package set may ask for: installed, not installed, or at the newest.")

(defclass monk-package-resource (monk-resource)
  ((packages :initarg :packages :type list
             :documentation "Package names, as this manager spells them.")
   (state :initarg :state :initform 'present
          :documentation "One of `monk-package-states'.")
   (hold :initarg :hold :initform nil
         :documentation "Non-nil: pin the packages so the manager's upgrades skip them.
Nil: do not touch holds.  `release': lift a hold."))
  :abstract t
  :documentation "A set of packages under one package manager.")

(cl-defgeneric monk-package-program (resource)
  "Return the program RESOURCE's manager is driven through.")

(cl-defgeneric monk-package-installed-names (resource machine)
  "Return every name an installed package on MACHINE answers to.
For RESOURCE's manager.  Aliases and provides count where the manager
knows them.  Cached per run by the caller; a repair clears the cache.")

(cl-defgeneric monk-package-outdated-names (resource machine)
  "Return the installed packages on MACHINE with a newer version available.
Only asked for state `latest'.")

(cl-defgeneric monk-package-held-names (resource machine)
  "Return the packages on MACHINE the manager holds back from upgrades.
The symbol `unsupported', the default, means this manager has no
holds; a `:hold' is then not a change, and the check logs that once.")

(cl-defgeneric monk-package-command (resource machine operation names)
  "Return the command (PROGRAM . ARGS) for OPERATION on NAMES for RESOURCE.
OPERATION is one of `install', `remove', `upgrade', `hold', `unhold'.
ARGS may end with `monk-exec' keywords (:sudo, :timeout).  Return
nil when the manager cannot do OPERATION; the resource then fails
with a reason.")

(cl-defgeneric monk-package-environment (resource)
  "Return extra `process-environment' entries for RESOURCE's manager, or nil.")

(cl-defgeneric monk-package-name-matches-p (resource name installed)
  "Return non-nil when package NAME counts as present given INSTALLED names.
The default is a plain match; brew also accepts a tapped name by its
last component.")

(cl-defmethod monk-package-held-names ((_r monk-package-resource) (_m monk-machine))
  'unsupported)

(cl-defmethod monk-package-environment ((_r monk-package-resource))
  nil)

(cl-defmethod monk-package-name-matches-p ((_r monk-package-resource) name installed)
  (and (member name installed) t))

(cl-defmethod monk-resource-available-p ((r monk-package-resource) (m monk-machine))
  (monk-program-available-p m (monk-package-program r)))

(cl-defmethod monk-resource-class-limit ((_r monk-package-resource))
  "One at a time: every package manager holds one database lock."
  1)

(cl-defmethod monk-package-outdated-names ((r monk-package-resource) (_m monk-machine))
  (signal 'monk-resource-error
          (list (format "%s: state latest is not supported for %s" (oref r name) (monk-resource-type r)))))

(defun monk-package--installed (r m)
  "Return the installed names for R on M, cached for the run per class."
  (monk-with-run-cache (list 'package-installed (oref m name) (eieio-object-class-name r))
    (monk-package-installed-names r m)))

(defun monk-package--forget-installed (r m)
  "Drop the cached installed names for R's class on M, after a repair."
  (monk-run-context-clear (list 'package-installed (oref m name) (eieio-object-class-name r))))

(cl-defmethod monk-check ((r monk-package-resource) (m monk-machine))
  "Compare R's packages on M with the state asked for.
Changes: :install, :remove, :upgrade, :hold, :unhold, each a name
list, and :degree over the whole set."
  (let* ((names (oref r packages))
         (state (oref r state))
         (installed (and (not (eq state 'absent)) (monk-package--installed r m)))
         (present-p (lambda (n) (monk-package-name-matches-p r n installed)))
         (changes nil) (wrong 0))
    (pcase state
      ('absent
       (let* ((installed (monk-package--installed r m))
              (there (seq-filter (lambda (n) (monk-package-name-matches-p r n installed)) names)))
         (when there
           (setq changes (plist-put changes :remove there) wrong (+ wrong (length there))))))
      ((or 'present 'latest)
       (let ((missing (seq-remove present-p names)))
         (when missing
           (setq changes (plist-put changes :install missing) wrong (+ wrong (length missing))))
         (when (eq state 'latest)
           (let* ((outdated (monk-package-outdated-names r m))
                  (behind (seq-filter (lambda (n) (and (funcall present-p n) (member n outdated))) names)))
             (when behind
               (setq changes (plist-put changes :upgrade behind) wrong (+ wrong (length behind))))))))
      (_ (signal 'monk-resource-error (list (format "%s: unknown package state %S" (oref r name) state)))))
    (when (and (oref r hold) (not (eq state 'absent)))
      (let ((held (monk-package-held-names r m)))
        (if (eq held 'unsupported)
            (monk-log 'info "%s: %s has no holds; :hold %S ignored"
                      (oref m name) (monk-resource-type r) (oref r hold))
          (let* ((want-held (not (eq (oref r hold) 'release)))
                 (wrong-hold (seq-filter (lambda (n) (and (funcall present-p n)
                                                           (if want-held (not (member n held)) (member n held))))
                                         names)))
            (when wrong-hold
              (setq changes (plist-put changes (if want-held :hold :unhold) wrong-hold)
                    wrong (+ wrong (length wrong-hold))))))))
    (and changes
         (plist-put changes :degree (monk-set-degree (length names) (min (length names) wrong))))))

(cl-defmethod monk-repair ((r monk-package-resource) (m monk-machine) changes)
  "Run the manager's commands for CHANGES of R on M.  Return a comment."
  (let ((process-environment (append (monk-package-environment r) process-environment))
        (notes nil))
    (dolist (step '((:install . install) (:upgrade . upgrade) (:remove . remove)
                    (:hold . hold) (:unhold . unhold)))
      (let ((names (plist-get changes (car step))))
        (when names
          (let ((command (monk-package-command r m (cdr step) names)))
            (unless command
              (signal 'monk-resource-error
                      (list (format "%s: %s cannot %s packages" (oref r name)
                                    (monk-resource-type r) (cdr step)))))
            (monk-package-run r m command)
            (push (format "%s %s" (pcase (cdr step)
                                    ('install "installed") ('upgrade "upgraded") ('remove "removed")
                                    ('hold "held") ('unhold "released"))
                          (string-join names " "))
                  notes)))))
    (monk-package--forget-installed r m)
    (string-join (nreverse notes) "; ")))

(cl-defgeneric monk-package-run (resource machine command)
  "Run COMMAND, (PROGRAM . ARGS), for RESOURCE on MACHINE; signal on failure.
Brew overrides this to trust a refused tap and retry.")

(cl-defmethod monk-package-run ((_r monk-package-resource) (m monk-machine) command)
  (apply #'monk-exec-string m (car command) (cdr command)))

(cl-defgeneric monk-package-aggregate-p (resource other)
  "Return non-nil when package RESOURCE and OTHER may merge into one command.
Same class, same state, same hold; a subclass adds its own terms.")

(cl-defmethod monk-package-aggregate-p ((r monk-package-resource) other)
  (and (eq (eieio-object-class-name r) (eieio-object-class-name other))
       (eq (oref r state) (oref other state))
       (equal (oref r hold) (oref other hold))))

(cl-defgeneric monk-package-aggregate-make (resource name packages)
  "Return a resource like RESOURCE named NAME over PACKAGES, for aggregation.
A subclass copies its own slots.")

(cl-defmethod monk-package-aggregate-make ((r monk-package-resource) name packages)
  (funcall (eieio-object-class r) :name name :packages packages
           :state (oref r state) :hold (oref r hold) :sudo (oref r sudo)))

(cl-defmethod monk-resource-aggregate ((r monk-package-resource) others (_m monk-machine))
  "Merge R and OTHERS, package resources alike in class and state, into one."
  (when (cl-every (lambda (o) (monk-package-aggregate-p r o)) others)
    (monk-package-aggregate-make
     r (format "%s (+%d)" (oref r name) (length others))
     (delete-dups (apply #'append (oref r packages) (mapcar (lambda (o) (oref o packages)) others))))))

(cl-defmethod monk-resource-disaggregate ((_a monk-package-resource) (member monk-package-resource) changes)
  "Return MEMBER's share of the aggregate's CHANGES, or nil when it had none."
  (let ((mine nil) (count 0))
    (dolist (key '(:install :upgrade :remove :hold :unhold :rebuild))
      (let ((names (seq-filter (lambda (n) (member (if (consp n) (car n) n) (oref member packages)))
                               (plist-get changes key))))
        (when names
          (setq mine (plist-put mine key names) count (+ count (length names))))))
    (and mine (plist-put mine :degree (monk-set-degree (length (oref member packages))
                                                       (min count (length (oref member packages))))))))

;;;; brew

(defclass monk-brew-resource (monk-package-resource)
  ((cask :initarg :cask
         :initform nil
         :documentation "Treat the names as casks.")
   (rebuild :initarg :rebuild
            :initform nil
            :documentation "When to build an installed formula again: nil never, t always,
or a function of the machine and the formula name that returns nil or
a reason string.  For formulae built from a moving branch, where
brew's version never changes but the source does."))
  "A set of Homebrew formulae or casks that must be installed.")

(defcustom monk-brew-build-timeout 0
  "Seconds a `brew install' or `brew reinstall' may run; 0 for no limit.
A source build (emacs-plus) takes as long as it takes; the general
`monk-exec-timeout' (ten minutes) stopped one mid-build on 2026-11-25
and the fallback started the build over.  Downloads and reads keep
the general timeout."
  :type 'natnum :group 'monk)

(defun monk--brew-build-timeout ()
  "Return the :timeout value for a brew install or reinstall."
  (if (zerop monk-brew-build-timeout) nil monk-brew-build-timeout))

(defun monk--brew-rebuild-reason (r m name)
  "Return why R must rebuild installed NAME on M, or nil.
A formula rebuilt earlier in this run is not due again: a reason such
as \"does not know its source commit\" holds after any build, so
without this the next pass would build it a second time and the run
would read as flapping."
  (let ((rebuild (oref r rebuild)))
    (cond ((null rebuild) nil)
          ((member name (monk-run-context-get (list 'brew-rebuilt (oref m name)))) nil)
          ((eq rebuild t) "rebuild requested")
          ((functionp rebuild) (funcall rebuild m name))
          (t (format "%s" rebuild)))))

(defun monk--brew-failure-summary-string (output)
  "Return the telling lines of failed brew OUTPUT.
The Error:, Warning:, fatal:, ssh: and curl: lines, else the last
line.  Brew prints a lot; the reason is in those.  Capped at four."
  (let* ((lines (split-string output "\n" t))
         (telling (seq-filter (lambda (l) (string-match-p "\\`\\(Error\\|Warning\\|fatal\\|error\\|ssh\\|curl\\):" (string-trim l)))
                              lines))
         (picked (or telling (last lines))))
    (mapconcat #'string-trim (seq-take picked 4) " | ")))

(defun monk--brew-failure-summary (exec)
  "Return the exit code and the telling lines of a failed brew EXEC."
  (format "exit %d: %s" (monk-exec-exit-code exec)
          (monk--brew-failure-summary-string (monk-exec-output exec))))

(cl-defmethod monk-resource-class-init ((_r monk-brew-resource) (m monk-machine))
  "Update Homebrew on M once per run, so no install call does it noisily.
A failed update is logged with brew's own error lines, and installs
go on against the taps as they are."
  (let ((exec (monk-exec m monk-brew-program "update" "--quiet")))
    (if (monk-exec-ok-p exec)
        "brew update"
      (monk-log 'warn "%s: brew update failed (%s); installs go on with stale taps"
                (oref m name) (monk--brew-failure-summary exec))
      nil)))

(cl-defmethod monk-package-program ((_r monk-brew-resource))
  monk-brew-program)

(cl-defmethod monk-package-environment ((_r monk-brew-resource))
  '("HOMEBREW_NO_AUTO_UPDATE=1"))

(cl-defmethod monk-package-aggregate-p ((r monk-brew-resource) other)
  "Brew sets merge when both are formulae or both casks and neither rebuilds.
A resource with a rebuild rule stays on its own: its install is a
build, not a download, and must not hold up the batch."
  (and (cl-call-next-method)
       (null (oref r rebuild)) (null (oref other rebuild))
       (eq (null (oref r cask)) (null (oref other cask)))))

(cl-defmethod monk-package-aggregate-make ((r monk-brew-resource) name packages)
  (monk-brew-resource :name name :packages packages :cask (oref r cask)
                      :state (oref r state) :hold (oref r hold)))

(defun monk--brew-json-names (entry cask)
  "Return every name that brew ENTRY, a parsed JSON object, answers to.
For a formula: name, full_name, aliases, oldnames.  For a CASK:
token, full_token, old_tokens.  Each also under its short name."
  (let* ((get (lambda (key) (gethash key entry)))
         (names (delq nil
                      (if cask
                          (append (list (funcall get "token") (funcall get "full_token"))
                                  (append (funcall get "old_tokens") nil))
                        (append (list (funcall get "name") (funcall get "full_name"))
                                (append (funcall get "aliases") nil)
                                (append (funcall get "oldnames") nil))))))
    (delete-dups (append names (mapcar #'file-name-nondirectory names)))))

(defun monk--brew-installed-names (m)
  "Return every name an installed formula or cask on M answers to.
One `brew info --json=v2 --installed' call per run, cached in the run
context and cleared by a repair.  Aliases count: rg is installed when
ripgrep is.  Casks count for a formula resource too, because `brew
install NAME' installs a cask of that name.  When the call fails,
fall back to `brew list', which knows no aliases."
  (monk-with-run-cache (list 'brew-installed (oref m name))
    (monk--brew-installed-names-uncached m)))

(defconst monk-brew-orphan-regexp
  "No available \\(?:formula with the name\\|cask with the token\\) \"\\([^\"]+\\)\""
  "Matches brew naming an installed keg whose formula or cask no longer exists.
A rename brew forgot (`mobile-shell' became `mosh') or a tap that went
away leaves such a keg; `brew info --installed' then fails as a whole.")

(defconst monk-brew-orphan-retries 5
  "How many orphaned kegs `monk--brew-installed-names' drops before giving up.")

(defun monk--brew-json-installed-names (output)
  "Return every installed name in OUTPUT, the JSON of `brew info --json=v2'."
  (let ((parsed (json-parse-string output)))
    (append (mapcan (lambda (e) (monk--brew-json-names e nil))
                    (append (gethash "formulae" parsed) nil))
            (mapcan (lambda (e) (monk--brew-json-names e t))
                    (append (gethash "casks" parsed) nil)))))

(defun monk--brew-installed-names-uncached (m)
  "Ask brew on M for installed names.  See `monk--brew-installed-names'.
When `brew info --installed' fails because of a keg brew has no
formula or cask for, ask again by name without it, up to
`monk-brew-orphan-retries' times, and say which kegs to remove.  Any
other failure falls back to `brew list', which knows no aliases."
  (let ((exec (monk-exec m monk-brew-program "info" "--json=v2" "--installed")))
    (if (monk-exec-ok-p exec)
        (monk--brew-json-installed-names (monk-exec-output exec))
      (let ((orphans nil) (names nil) (tries 0))
        (while (and exec (not (monk-exec-ok-p exec))
                    (string-match monk-brew-orphan-regexp (monk-exec-output exec))
                    (< tries monk-brew-orphan-retries))
          (push (match-string 1 (monk-exec-output exec)) orphans)
          (cl-incf tries)
          (let ((formulae (seq-difference (monk-exec-lines m monk-brew-program "list" "-1" "--formula") orphans))
                (casks (seq-difference (monk-exec-lines m monk-brew-program "list" "-1" "--cask") orphans)))
            (setq names nil exec nil)
            (dolist (part (list (cons "--formula" formulae) (cons "--cask" casks)))
              (when (and (cdr part) (null exec))
                (let ((one (apply #'monk-exec m monk-brew-program "info" "--json=v2" (car part) (cdr part))))
                  (if (monk-exec-ok-p one)
                      (setq names (append names (monk--brew-json-installed-names (monk-exec-output one))))
                    (setq exec one)))))))
        (cond
         ((and orphans (null exec))
          ;; plain `brew uninstall NAME' looks the formula up first and
          ;; fails the same way; --force skips that, and when brew
          ;; still refuses, the keg directory itself is the thing to remove
          (monk-log 'warn "%s: brew has no formula for installed keg%s %s; aliases read without %s.  Remove with: brew uninstall --force --ignore-dependencies %s  (or delete %s under $(brew --cellar))"
                    (oref m name) (if (cdr orphans) "s" "") (string-join (reverse orphans) " ")
                    (if (cdr orphans) "them" "it") (string-join (reverse orphans) " ")
                    (string-join (reverse orphans) " "))
          names)
         (t
          (monk-log 'warn "%s: brew info --installed failed, aliases unknown: %s"
                    (oref m name) (string-trim (monk-exec-output exec)))
          (append (monk-exec-lines m monk-brew-program "list" "-1" "--formula")
                  (monk-exec-lines m monk-brew-program "list" "-1" "--cask"))))))))

(cl-defmethod monk-package-installed-names ((_r monk-brew-resource) (m monk-machine))
  (monk--brew-installed-names m))

(cl-defmethod monk-package-name-matches-p ((_r monk-brew-resource) name installed)
  ;; A tapped name such as user/tap/formula is installed under its short name.
  (and (or (member name installed) (member (file-name-nondirectory name) installed)) t))

(cl-defmethod monk-package-outdated-names ((r monk-brew-resource) (m monk-machine))
  "The names `brew outdated' lists, formulae or casks as R is."
  (monk-exec-lines m monk-brew-program "outdated" "--quiet" (if (oref r cask) "--cask" "--formula")))

(cl-defmethod monk-package-held-names ((_r monk-brew-resource) (m monk-machine))
  (monk-exec-lines m monk-brew-program "list" "--pinned"))

(cl-defmethod monk-package-command ((r monk-brew-resource) (_m monk-machine) operation names)
  (let ((cask (and (oref r cask) '("--cask")))
        (timeout (list :timeout (monk--brew-build-timeout))))
    (pcase operation
      ('install (append (list monk-brew-program "install") cask names timeout))
      ('upgrade (append (list monk-brew-program "upgrade") cask names timeout))
      ('remove (append (list monk-brew-program "uninstall") cask names))
      ('hold (and (not (oref r cask)) (append (list monk-brew-program "pin") names)))
      ('unhold (and (not (oref r cask)) (append (list monk-brew-program "unpin") names))))))

(cl-defmethod monk-check ((r monk-brew-resource) (m monk-machine))
  "The shared check, plus brew's rebuild rule for formulae already installed."
  (let* ((changes (cl-call-next-method))
         (missing (plist-get changes :install))
         (rebuild (and (oref r rebuild) (not (eq (oref r state) 'absent))
                       (delq nil (mapcar (lambda (name)
                                           (unless (member name missing)
                                             (let ((why (monk--brew-rebuild-reason r m name)))
                                               (and why (cons name why)))))
                                         (oref r packages))))))
    (if (null rebuild)
        changes
      (let ((wrong (+ (length rebuild)
                      (cl-loop for key in '(:install :upgrade :remove :hold :unhold)
                               sum (length (plist-get changes key))))))
        (plist-put (plist-put (copy-sequence changes) :rebuild rebuild)
                   :degree (monk-set-degree (length (oref r packages))
                                            (min wrong (length (oref r packages)))))))))

(defconst monk-brew-untrusted-tap-regexp
  "untrusted tap \\([^ .\n]+\\)"
  "Matches the Homebrew refusal to install from a tap that is not trusted.
Group 1 is the tap name.")

(defun monk--brew-trust (m tap)
  "Run `brew trust TAP' on M.  Return non-nil when the command succeeded.
Older Homebrew has no `trust' subcommand.  Then the call fails and
this returns nil, and the caller proceeds as before."
  (let ((exec (monk-exec m monk-brew-program "trust" tap)))
    (if (monk-exec-ok-p exec)
        (progn (monk-log 'info "%s: trusted tap %s" (oref m name) tap) t)
      (monk-log 'debug "%s: brew trust %s failed: %s" (oref m name) tap
                (string-trim (monk-exec-output exec)))
      nil)))

(defvar monk--brew-trusted-this-repair nil
  "The tap `monk-package-run' trusted during the repair in progress, or nil.")

(cl-defmethod monk-package-run ((_r monk-brew-resource) (m monk-machine) command)
  "Run brew COMMAND on M, trusting a refused tap once.
Homebrew refuses formulae from a tap nobody ran `brew trust' on.
Trust it and retry; a second refusal propagates.  The tap trusted is
left in `monk--brew-trusted-this-repair' for the comment."
  (condition-case err
      (apply #'monk-exec-string m (car command) (cdr command))
    (monk-exec-error
     (let ((output (format "%s" (car (last err)))))
       (unless (and (string-match monk-brew-untrusted-tap-regexp output)
                    (monk--brew-trust m (match-string 1 output)))
         (signal (car err) (cdr err)))
       (setq monk--brew-trusted-this-repair (match-string 1 output))
       (apply #'monk-exec-string m (car command) (cdr command))))))

(defun monk--brew-rebuild-call (m name)
  "Build installed formula NAME on M again.
`brew reinstall' keeps the old keg aside and restores it when the
build fails.  When reinstall itself refuses, as it does for a keg
brew no longer recognizes, `brew install --overwrite' is the way
through; that is what the old Makefile spelled by hand."
  (condition-case err
      (monk-exec-string m monk-brew-program "reinstall" name
                        :timeout (monk--brew-build-timeout))
    (monk-exec-error
     ;; A build Monk stopped is not a refusal: starting it over with
     ;; --overwrite would run the whole build a second time.
     (when (monk-exec-timed-out-p (monk-exec-create :exit-code (nth 3 err)
                                                    :output (format "%s" (car (last err)))))
       (signal (car err) (cdr err)))
     (monk-log 'warn "%s: brew reinstall %s failed (%s); trying install --overwrite"
               (oref m name) name (monk--brew-failure-summary-string (format "%s" (car (last err)))))
     (monk-exec-string m monk-brew-program "install" "--overwrite" name
                       :timeout (monk--brew-build-timeout)))))

(cl-defmethod monk-repair ((r monk-brew-resource) (m monk-machine) changes)
  "The shared repair, then the rebuilds; the comment names any tap trusted."
  (let* ((monk--brew-trusted-this-repair nil)
         (shared (cl-call-next-method))
         (rebuild (plist-get changes :rebuild))
         (process-environment (append (monk-package-environment r) process-environment)))
    (dolist (cell rebuild)
      (monk--brew-rebuild-call m (car cell))
      (monk-run-context-put (list 'brew-rebuilt (oref m name))
                            (cons (car cell) (monk-run-context-get (list 'brew-rebuilt (oref m name))))))
    (monk-run-context-clear (list 'brew-installed (oref m name)))
    (monk-package--forget-installed r m)
    (string-join
     (delq nil (list (and (not (string-empty-p shared))
                          (concat shared (if monk--brew-trusted-this-repair
                                             (format " (after brew trust %s)" monk--brew-trusted-this-repair)
                                           "")))
                     (and rebuild (format "rebuilt %s" (mapconcat (lambda (c) (format "%s (%s)" (car c) (cdr c)))
                                                                   rebuild ", ")))))
     "; ")))

(monk-define-resource monk-brew monk-brew-resource
  "Ensure Homebrew PACKAGES are present, absent, or at the latest, in one call.
Independent brew resources on a machine aggregate into one command."
  ((name :type string :doc "Identifies the set.")
   (packages :type list :doc "Formula or cask names; aliases and taps count."))
  ((state :type symbol :default present :choices (present absent latest)
          :doc "present installs the missing, absent uninstalls, latest also upgrades.")
   (hold :type any :doc "t pins the formulae (brew pin); release unpins; nil leaves pins alone.")
   (cask :type boolean :doc "Treat the names as casks.")
   (rebuild :type any :doc "nil, t, or a function of (MACHINE NAME) returning nil or a reason: build an installed formula again."))
  :classes (darwin))

;;;; brew: versioned formulae and the running binary

(defun monk-brew-versioned-formulae (m stem &optional tap)
  "Return the versions of formula STEM that brew on M offers, newest first.
STEM is the name before `@', for example `emacs-plus'; TAP narrows
the search to one tap.  Each element is an integer, or a string for
a dotted version such as python's 3.12.  `brew search' is one call;
the result is cached for the run.  Nil when brew is absent or knows
no such formula."
  (monk-with-run-cache (list 'brew-versions (oref m name) stem tap)
    (let ((exec (monk-exec m monk-brew-program "search" "--formula"
                           (format "%s%s@" (if tap (concat tap "/") "") stem))))
      (when (monk-exec-ok-p exec)
        (let (versions)
          (dolist (line (split-string (monk-exec-output exec) "\n" t))
            (when (string-match (format "\\`\\(?:[^ /]+/[^ /]+/\\)?%s@\\([0-9][0-9.]*\\)\\'"
                                        (regexp-quote stem))
                                (string-trim line))
              (let ((v (match-string 1 line)))
                (cl-pushnew (if (string-match-p "\\." v) v (string-to-number v)) versions :test #'equal))))
          (sort versions (lambda (a b) (version< (format "%s" b) (format "%s" a)))))))))

(defun monk-brew-latest-formula (m stem &optional tap at-least)
  "Return the newest `STEM@N' brew on M offers, or nil.
With AT-LEAST, a version, return nil when the newest is older.  See
`monk-brew-versioned-formulae'."
  (let ((newest (car (monk-brew-versioned-formulae m stem tap))))
    (and newest
         (or (null at-least) (not (version< (format "%s" newest) (format "%s" at-least))))
         (format "%s@%s" stem newest))))

(defun monk-brew-opt-prefix (m formula)
  "Return brew's stable `opt' directory for FORMULA on M, or nil.
`$(brew --prefix FORMULA)' points at the installed keg through the
opt link, so a path built on it survives upgrades."
  (let ((exec (monk-exec m monk-brew-program "--prefix" formula)))
    (and (monk-exec-ok-p exec)
         (let ((dir (string-trim (monk-exec-output exec))))
           (and (not (string-empty-p dir)) (file-name-as-directory dir))))))

(defun monk-brew-outdated-p (m formula)
  "Return non-nil when FORMULA on M is installed and brew calls it outdated."
  (let ((exec (monk-exec m monk-brew-program "outdated" "--quiet" formula)))
    (and (monk-exec-ok-p exec)
         (member formula (split-string (monk-exec-output exec) "\n" t)))))

;;;; app-link: a formula's .app in /Applications

(defclass monk-app-link-resource (monk-resource)
  ((source :initarg :source :type string
           :documentation "The .app bundle to link, a path or FORMULA:App.app.")
   (applications :initarg :applications :initform "/Applications"
                 :documentation "Where the link goes."))
  "A symbolic link in /Applications to an application bundle a formula ships.
Homebrew formulae do not install apps into /Applications.  A link
through the formula's `opt' path follows upgrades; a copy would not.
The resource name is the link, for example `/Applications/Emacs.app'.")

(defun monk-app-link--source (r m)
  "Return the resolved source bundle path of R on M.
`FORMULA:Name.app' becomes `$(brew --prefix FORMULA)/Name.app'; a
path is expanded on M.  Signal `monk-resource-error' when brew does
not know the formula."
  (let ((source (oref r source)))
    (if (string-match "\\`\\([^/:]+\\):\\(.+\\)\\'" source)
        (let* ((formula (match-string 1 source))
               (app (match-string 2 source))
               (prefix (monk-brew-opt-prefix m formula)))
          (unless prefix
            (signal 'monk-resource-error (list (format "brew does not know %s; install it first" formula))))
          (concat prefix app))
      (monk-expand source))))

(cl-defmethod monk-resource-available-p ((_r monk-app-link-resource) (m monk-machine))
  (monk-program-available-p m monk-brew-program))

(cl-defmethod monk-check ((r monk-app-link-resource) (m monk-machine))
  (let* ((link (monk-machine-path m (oref r name) (oref r sudo)))
         (target (monk-app-link--source r m))
         (current (file-symlink-p link)))
    (cond
     ((not (file-directory-p (monk-machine-path m target (oref r sudo))))
      (signal 'monk-resource-error (list (format "%s does not exist; the formula may not have built it" target))))
     ((equal current target) nil)
     (current (list :retarget (list current target)))
     ((file-exists-p link)
      ;; a real bundle in the way, a copy from before: replace it, it
      ;; is what the link would show anyway
      (list :replace (list 'directory target)))
     (t (list :create target)))))

(cl-defmethod monk-repair ((r monk-app-link-resource) (m monk-machine) changes)
  (let ((link (monk-machine-path m (oref r name) (oref r sudo)))
        (target (monk-app-link--source r m)))
    (cond
     ((plist-get changes :replace) (delete-directory link t))
     ((plist-get changes :retarget) (delete-file link)))
    (make-symbolic-link target link t)
    (format "%s -> %s" (oref r name) target)))

(cl-defmethod monk-resource-paths ((r monk-app-link-resource))
  (list (oref r name)))

(monk-define-resource monk-app-link monk-app-link-resource
  "Ensure the .app at SOURCE is linked into /Applications as NAME.
SOURCE is `FORMULA:Name.app', resolved through `brew --prefix', or a
path.  NAME is the link, usually /Applications/Name.app."
  ((name :type path :doc "The link, for example /Applications/Emacs.app.")
   (source :type string :doc "FORMULA:Name.app or a path to the bundle."))
  ()
  :classes (darwin))

;;;; brew-tap

(defclass monk-brew-tap-resource (monk-resource)
  ()
  "A Homebrew tap NAME.")

(cl-defmethod monk-check ((r monk-brew-tap-resource) (m monk-machine))
  (unless (member (downcase (oref r name))
                  (mapcar #'downcase (monk-exec-lines m monk-brew-program "tap")))
    (list :tap (oref r name))))

(cl-defmethod monk-repair ((r monk-brew-tap-resource) (m monk-machine) _changes)
  (monk-exec-string m monk-brew-program "tap" (oref r name))
  ;; A tap is only useful when brew will install from it, so trust it
  ;; where Homebrew knows the concept.
  (if (monk--brew-trust m (oref r name))
      "tapped and trusted"
    "tapped"))

(monk-define-resource monk-brew-tap monk-brew-tap-resource
  "Ensure Homebrew tap NAME is present and trusted."
  ((name :type string :doc "The tap, for example user/repo."))
  ()
  :classes (darwin))

;;;; apt

(defclass monk-apt-resource (monk-package-resource)
  ((no-recommends :initarg :no-recommends :initform nil
                  :documentation "Pass --no-install-recommends to apt-get."))
  "A set of Debian packages.")

(cl-defmethod monk-package-program ((_r monk-apt-resource))
  monk-apt-program)

(cl-defmethod monk-package-environment ((_r monk-apt-resource))
  '("DEBIAN_FRONTEND=noninteractive"))

(cl-defmethod monk-package-installed-names ((_r monk-apt-resource) (m monk-machine))
  "Every installed package by name, from dpkg's database."
  ;; dpkg-query exits 1 when a name is unknown but prints the rest; the
  ;; whole database is asked, so no name is unknown
  (let ((exec (monk-exec m monk-dpkg-query-program "-W" "-f" "${Package} ${db:Status-Status}\n")))
    (delq nil (mapcar (lambda (line)
                        (pcase (split-string line " ")
                          (`(,name "installed") name)))
                      (split-string (monk-exec-output exec) "\n" t)))))

(cl-defmethod monk-package-outdated-names ((_r monk-apt-resource) (m monk-machine))
  "Packages `apt-get -s upgrade' would upgrade: the Inst lines of a simulation."
  (let ((exec (monk-exec m monk-apt-program "-s" "upgrade" "-q")))
    (delq nil (mapcar (lambda (line)
                        (and (string-match "\\`Inst \\([^ ]+\\)" line) (match-string 1 line)))
                      (split-string (monk-exec-output exec) "\n" t)))))

(cl-defmethod monk-package-held-names ((_r monk-apt-resource) (m monk-machine))
  (monk-exec-lines m "apt-mark" "showhold"))

(cl-defmethod monk-package-command ((r monk-apt-resource) (_m monk-machine) operation names)
  (let ((quiet (append '("-y" "-q") (and (oref r no-recommends) '("--no-install-recommends")))))
    (pcase operation
      ('install (append (list monk-apt-program "install") quiet names '(:sudo t)))
      ('upgrade (append (list monk-apt-program "install" "--only-upgrade") quiet names '(:sudo t)))
      ('remove (append (list monk-apt-program "remove" "-y" "-q") names '(:sudo t)))
      ('hold (append (list "apt-mark" "hold") names '(:sudo t)))
      ('unhold (append (list "apt-mark" "unhold") names '(:sudo t))))))

(defcustom monk-apt-update t
  "When non-nil, run `apt-get update' once per run before the first apt resource."
  :type 'boolean
  :group 'monk)

(cl-defmethod monk-resource-class-init ((_r monk-apt-resource) (m monk-machine))
  "Refresh the package index on M once per run, per `monk-apt-update'."
  (when monk-apt-update
    (let ((process-environment (cons "DEBIAN_FRONTEND=noninteractive" process-environment)))
      (if (monk-exec-ok-p (monk-exec m monk-apt-program "update" "-q" :sudo t))
          "apt-get update"
        (monk-log 'warn "%s: apt-get update failed; installs go on with a stale index" (oref m name))
        nil))))

(cl-defmethod monk-package-aggregate-p ((r monk-apt-resource) other)
  (and (cl-call-next-method)
       (eq (null (oref r no-recommends)) (null (oref other no-recommends)))))

(cl-defmethod monk-package-aggregate-make ((r monk-apt-resource) name packages)
  (monk-apt-resource :name name :packages packages :state (oref r state) :hold (oref r hold)
                     :no-recommends (oref r no-recommends) :sudo t))

(monk-define-resource monk-apt monk-apt-resource
  "Ensure Debian PACKAGES are present, absent, or at the latest, in one call.
Independent apt resources on a machine aggregate into one command."
  ((name :type string :doc "Identifies the set.")
   (packages :type list :doc "Package names."))
  ((state :type symbol :default present :choices (present absent latest)
          :doc "present installs the missing, absent removes, latest also upgrades.")
   (hold :type any :doc "t holds the packages (apt-mark hold); release unholds; nil leaves holds alone.")
   (no-recommends :type boolean :doc "Install without recommended packages."))
  :classes (debian)
  :sudo t)

;;;; dnf, yum, pacman, zypper, apk, pkg: the simple managers

;; Each knows how to list what is installed, what has an update, and
;; the verbs.  None has brew's aliases or apt's recommends; the base
;; class does the rest.  `monk--define-simple-package-manager' writes
;; the class, the methods, and the constructor from one spec.

(defmacro monk--define-simple-package-manager (name program classes doc &rest spec)
  "Define package resource NAME driven by PROGRAM for machines of CLASSES.
DOC heads the constructor's docstring.  SPEC is a plist: :installed
and :outdated, each (ARGS . PARSER) where PARSER turns the command
output into a name list; :held likewise or absent; :install, :upgrade,
:remove, :hold, :unhold, each a list of leading ARGS the names follow
\(nil: unsupported); :env, extra environment entries."
  (declare (indent 3))
  (let* ((class (intern (format "%s-resource" name)))
         (installed (plist-get spec :installed))
         (outdated (plist-get spec :outdated))
         (held (plist-get spec :held))
         (env (plist-get spec :env))
         (verbs '(install upgrade remove hold unhold)))
    `(progn
       (defclass ,class (monk-package-resource) ()
         ,(format "A set of packages under %s." program))
       (cl-defmethod monk-package-program ((_r ,class)) ,program)
       ,@(when env `((cl-defmethod monk-package-environment ((_r ,class)) ',env)))
       (cl-defmethod monk-package-installed-names ((_r ,class) (m monk-machine))
         (funcall ,(cdr installed) (monk-exec-output (monk-exec m ,program ,@(car installed)))))
       ,@(when outdated
           `((cl-defmethod monk-package-outdated-names ((_r ,class) (m monk-machine))
               (funcall ,(cdr outdated) (monk-exec-output (monk-exec m ,program ,@(car outdated)))))))
       ,@(when held
           `((cl-defmethod monk-package-held-names ((_r ,class) (m monk-machine))
               (funcall ,(cdr held) (monk-exec-output (monk-exec m ,@(car held)))))))
       (cl-defmethod monk-package-command ((_r ,class) (_m monk-machine) operation names)
         (pcase operation
           ,@(mapcar (lambda (verb)
                       (let ((args (plist-get spec (intern (format ":%s" verb)))))
                         `(',verb ,(and args `(append (list ,program) ',args names '(:sudo t))))))
                     verbs)))
       (monk-define-resource ,name ,class
         ,(concat doc "\nIndependent sets on a machine aggregate into one command.")
         ((name :type string :doc "Identifies the set.")
          (packages :type list :doc "Package names."))
         ((state :type symbol :default present :choices (present absent latest)
                 :doc "present installs the missing, absent removes, latest also upgrades.")
          (hold :type any :doc "t holds the packages back from upgrades; release lifts it; nil leaves holds alone."))
         :classes ,classes
         :sudo t))))

(defun monk-package--first-words (output)
  "Return the first word of each non-empty line of OUTPUT."
  (delq nil (mapcar (lambda (l) (car (split-string l "[ \t]+" t))) (split-string output "\n" t))))

(defun monk-package--versionlock-names (output)
  "Return the package names in OUTPUT of `dnf versionlock list'.
Each line is a lock pattern, NAME-EPOCH:VERSION-RELEASE.ARCH or with
`*' in place of parts; the name is what comes before the first
`-DIGIT' or `-EPOCH:'.  A line with a space is a message, not a lock."
  (delq nil (mapcar (lambda (l)
                      (let ((l (string-trim l)))
                        (and (not (string-empty-p l))
                             (not (string-match-p " " l))
                             (if (string-match "\\`\\(.+?\\)-\\(?:[0-9]+:\\|[0-9*]\\)" l)
                                 (match-string 1 l)
                               l))))
                    (split-string output "\n" t))))

(defun monk-package--dnf-check-update-names (output)
  "Return the names in OUTPUT of `dnf check-update'.
Lines read NAME.ARCH VERSION REPO."
  (delq nil (mapcar (lambda (l)
                      (and (string-match "\\`\\([^ .\t]+\\)\\(?:\\.[^ \t]+\\)? +[^ \t]+ +[^ \t]+\\'" l)
                           (match-string 1 l)))
                    (split-string output "\n" t))))

(monk--define-simple-package-manager monk-dnf "dnf" (redhat)
  "Ensure Fedora/RHEL PACKAGES are present, absent, or latest through dnf."
  :installed (("repoquery" "--installed" "--qf" "%{name}") . #'monk-package--first-words)
  :outdated (("check-update" "-q") . #'monk-package--dnf-check-update-names)
  :held (("dnf" "versionlock" "list" "-q") . #'monk-package--versionlock-names)
  :install ("install" "-y" "-q") :upgrade ("upgrade" "-y" "-q") :remove ("remove" "-y" "-q")
  :hold ("versionlock" "add") :unhold ("versionlock" "delete"))

(monk--define-simple-package-manager monk-yum "yum" (redhat)
  "Ensure RHEL/CentOS PACKAGES are present, absent, or latest through yum."
  :installed (("list" "installed" "-q") . #'monk-package--yum-installed-names)
  :outdated (("check-update" "-q") . #'monk-package--dnf-check-update-names)
  :install ("install" "-y" "-q") :upgrade ("update" "-y" "-q") :remove ("remove" "-y" "-q"))

(defun monk-package--yum-installed-names (output)
  "Return the names in OUTPUT of `yum list installed': NAME.ARCH VERSION REPO."
  (delq nil (mapcar (lambda (l)
                      (and (string-match "\\`\\([^ \t]+\\)\\.[^ \t.]+[ \t]+[^ \t]+[ \t]+" l)
                           (match-string 1 l)))
                    (split-string output "\n" t))))

(monk--define-simple-package-manager monk-pacman "pacman" (arch)
  "Ensure Arch PACKAGES are present, absent, or latest through pacman."
  :installed (("-Qq") . #'monk-package--first-words)
  :outdated (("-Qu") . #'monk-package--first-words)
  :install ("-S" "--noconfirm" "--needed") :upgrade ("-S" "--noconfirm") :remove ("-R" "--noconfirm"))

(monk--define-simple-package-manager monk-zypper "zypper" (suse)
  "Ensure openSUSE/SLES PACKAGES are present, absent, or latest through zypper."
  :installed (("--quiet" "search" "-i" "-t" "package" "--match-exact") . #'monk-package--zypper-installed)
  :outdated (("--quiet" "list-updates") . #'monk-package--zypper-updates)
  :held (("zypper" "--quiet" "locks") . #'monk-package--zypper-locks)
  :install ("--non-interactive" "install") :upgrade ("--non-interactive" "update") :remove ("--non-interactive" "remove")
  :hold ("--non-interactive" "addlock") :unhold ("--non-interactive" "removelock"))

(defun monk-package--zypper-table-column (output column)
  "Return COLUMN (0-based) of the `|'-separated rows in zypper table OUTPUT."
  (delq nil (mapcar (lambda (l)
                      (let ((cells (mapcar #'string-trim (split-string l "|"))))
                        (and (> (length cells) column)
                             (not (member (nth column cells) '("Name" "")))
                             (not (string-match-p "\\`-+\\'" (nth column cells)))
                             (nth column cells))))
                    (split-string output "\n" t))))

(defun monk-package--zypper-installed (output)
  "Names from `zypper search -i': status | name | summary | type."
  (monk-package--zypper-table-column output 1))

(defun monk-package--zypper-updates (output)
  "Names from `zypper list-updates'.
Columns: status | repo | name | current | available | arch."
  (monk-package--zypper-table-column output 2))

(defun monk-package--zypper-locks (output)
  "Names from `zypper locks': # | name | type | repository."
  (monk-package--zypper-table-column output 1))

(monk--define-simple-package-manager monk-apk "apk" (alpine)
  "Ensure Alpine PACKAGES are present, absent, or latest through apk."
  :installed (("info") . #'monk-package--first-words)
  :outdated (("version" "-l" "<") . #'monk-package--apk-outdated)
  :install ("add" "-q") :upgrade ("upgrade" "-q") :remove ("del" "-q"))

(defun monk-package--apk-outdated (output)
  "Names from `apk version -l <': NAME-VERSION < NEWER lines, version cut."
  (delq nil (mapcar (lambda (l)
                      (and (string-match "\\`\\([^ ]+\\)-[0-9][^ ]* *<" l) (match-string 1 l)))
                    (split-string output "\n" t))))

(monk--define-simple-package-manager monk-pkg "pkg" (freebsd)
  "Ensure FreeBSD PACKAGES are present, absent, or latest through pkg."
  :installed (("query" "%n") . #'monk-package--first-words)
  :outdated (("version" "-l" "<") . #'monk-package--pkg-outdated)
  :held (("pkg" "query" "-e" "%k = 1" "%n") . #'monk-package--first-words)
  :install ("install" "-y" "-q") :upgrade ("upgrade" "-y" "-q") :remove ("delete" "-y" "-q")
  :hold ("lock" "-y") :unhold ("unlock" "-y"))

(defun monk-package--pkg-outdated (output)
  "Names from `pkg version -l <': NAME-VERSION lines, the version cut."
  (delq nil (mapcar (lambda (l)
                      (let ((w (car (split-string l "[ \t]+" t))))
                        (and w (string-match "\\`\\(.+\\)-[0-9][^-]*\\'" w) (match-string 1 w))))
                    (split-string output "\n" t))))

;;;; package: generic over the machine's package manager

(defconst monk-package-manager-constructors
  '(("homebrew" . monk-brew) ("apt" . monk-apt) ("dnf" . monk-dnf) ("yum" . monk-yum)
    ("pacman" . monk-pacman) ("zypper" . monk-zypper) ("apk" . monk-apk) ("pkg" . monk-pkg))
  "The per-manager constructor for each `pkg-mgr' fact value.")

(defconst monk-package-manager-keys
  '(("homebrew" . :brew) ("apt" . :apt) ("dnf" . :dnf) ("yum" . :yum)
    ("pacman" . :pacman) ("zypper" . :zypper) ("apk" . :apk) ("pkg" . :pkg))
  "The keyword a package spec uses to name a package for each manager.")

(defun monk-package-names-for (packages manager)
  "Return the names in PACKAGES for MANAGER, a `pkg-mgr' fact value.
Each element of PACKAGES is a string, the same name everywhere, or a
list (NAME . PLIST) whose PLIST gives a manager's own name under its
key (`:brew', `:apt', ...): `(\"ripgrep\" :brew \"rg\")'.  A manager
without an entry uses NAME; `:MANAGER nil' leaves the package out
there (a tool that exists only on one platform)."
  (let ((key (cdr (assoc manager monk-package-manager-keys))))
    (delq nil
          (mapcar (lambda (spec)
                    (cond
                     ((stringp spec) spec)
                     ((and (consp spec) (stringp (car spec)))
                      (if (and key (plist-member (cdr spec) key))
                          (plist-get (cdr spec) key)
                        (car spec)))
                     (t (signal 'monk-argument-error
                                (list (format "monk-package: bad package spec %S" spec))))))
                  packages))))

(defun monk-package (name packages &rest args)
  "Ensure PACKAGES are present, absent, or latest through the machine's manager.
NAME identifies the set.  The `pkg-mgr' fact of the machine being
compiled picks the resource: homebrew gives `monk-brew', apt
`monk-apt', dnf `monk-dnf', and so on through
`monk-package-manager-constructors'.  PACKAGES are strings or
\(NAME :brew B :apt A ...) specs; see `monk-package-names-for'.  ARGS:
:state (present, absent, latest), :hold, :cask (brew only), and the
common keys.  A machine with no supported manager gets a resource
that reports skipped with the reason, so the policy compiles
everywhere.  Ansible's `package'."
  (let* ((manager (monk-machine-fact (monk-current-machine) 'pkg-mgr))
         (constructor (cdr (assoc manager monk-package-manager-constructors)))
         (names (and constructor (monk-package-names-for packages manager)))
         (rest (copy-sequence args)))
    (unless (equal manager "homebrew") (cl-remf rest :cask))
    (cond
     ((null constructor)
      (apply #'monk-unsupported name (format "package: no supported package manager (pkg-mgr is %s)" manager)
             args))
     ((null names)
      (apply #'monk-unsupported name (format "package: none of the packages are named for %s" manager)
             args))
     (t (apply constructor name names :classes nil rest)))))

;;;; osx-default

(defclass monk-osx-default-resource (monk-resource)
  ((domain :initarg :domain :type string)
   (key :initarg :key :type string)
   (value :initarg :value)
   (value-type :initarg :value-type
               :initform 'string
               :documentation "One of string, int, float, bool, array."))
  "A macOS user default DOMAIN KEY with VALUE.")

(defun monk--osx-default-normalize (type value)
  "Return VALUE as the string that `defaults read' prints for TYPE."
  (pcase type
    ('bool (if value "1" "0"))
    ('int (format "%d" value))
    ('float (format "%s" value))
    ('string (format "%s" value))
    ('array (format "(\n%s\n)"
                    (mapconcat (lambda (v) (format "    %s" v)) value ",\n")))
    (_ (signal 'monk-resource-error (list "Unknown defaults type" type)))))

(defun monk--osx-default-args (type value)
  "Return the arguments for `defaults write' for TYPE and VALUE."
  (pcase type
    ('bool (list "-bool" (if value "true" "false")))
    ('int (list "-int" (format "%d" value)))
    ('float (list "-float" (format "%s" value)))
    ('string (list "-string" (format "%s" value)))
    ('array (cons "-array" (mapcar (lambda (v) (format "%s" v)) value)))
    (_ (signal 'monk-resource-error (list "Unknown defaults type" type)))))

(defun monk--osx-default-clean (text)
  "Strip quotes and whitespace that `defaults read' adds to TEXT."
  (string-trim (replace-regexp-in-string "\"" "" text)))

(cl-defmethod monk-check ((r monk-osx-default-resource) (m monk-machine))
  (let* ((exec (monk-exec m monk-defaults-program "read" (oref r domain) (oref r key)))
         (current (and (monk-exec-ok-p exec)
                       (monk--osx-default-clean (monk-exec-output exec))))
         (want (monk--osx-default-clean
                (monk--osx-default-normalize (oref r value-type) (oref r value)))))
    (unless (equal current want)
      (list :before (or current 'unset) :after want))))

(cl-defmethod monk-repair ((r monk-osx-default-resource) (m monk-machine) _changes)
  (apply #'monk-exec-string m monk-defaults-program "write"
         (oref r domain) (oref r key)
         (monk--osx-default-args (oref r value-type) (oref r value)))
  (format "wrote %s" (oref r key)))

(monk-define-resource monk-osx-default monk-osx-default-resource
  "Ensure macOS default DOMAIN KEY is VALUE."
  ((domain :type string :doc "Preferences domain, for example com.apple.finder.")
   (key :type string :doc "The key.")
   (value :type any :doc "The value, as `defaults write' takes it."))
  ((type :type symbol :default string :choices (string int float bool array) :slot value-type
         :doc "How `defaults' types the value."))
  :classes (darwin)
  :name-form (format "%s:%s" domain key))

(monk-note-load 'monk-package)
(provide 'monk-package)
;;; monk-package.el ends here
