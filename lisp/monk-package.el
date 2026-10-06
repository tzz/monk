;;; monk-package.el --- Package and macOS resources for Monk  -*- lexical-binding: t; -*-

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

;; Resources: `monk-brew', `monk-brew-tap', `monk-apt',
;; `monk-osx-default'.
;;
;; Package resources take a list of names and install the missing ones
;; in one command.  One resource per platform list keeps the number of
;; package-manager invocations at one per check and one per repair.

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

;;;; brew

(defclass monk-brew-resource (monk-resource)
  ((packages :initarg :packages
             :type list
             :documentation "Formula names.")
   (cask :initarg :cask
         :initform nil
         :documentation "Treat the names as casks.")
   (rebuild :initarg :rebuild
            :initform nil
            :documentation "When to build an installed formula again: nil never, t always,
or a function of the machine and the formula name that returns nil or
a reason string.  For formulae built from a moving branch, where
brew's version never changes but the source does."))
  "A set of Homebrew formulae or casks that must be installed.")

(defun monk--brew-rebuild-reason (r m name)
  "Return why R must rebuild installed NAME on M, or nil."
  (let ((rebuild (oref r rebuild)))
    (cond ((null rebuild) nil)
          ((eq rebuild t) "rebuild requested")
          ((functionp rebuild) (funcall rebuild m name))
          (t (format "%s" rebuild)))))

(cl-defmethod monk-resource-available-p ((_r monk-brew-resource) (m monk-machine))
  (monk-program-available-p m monk-brew-program))

(defun monk--brew-failure-summary (exec)
  "Return the telling lines of a failed brew EXEC.
The Error:, Warning:, fatal:, ssh: and curl: lines, else the last
line.  Brew prints a lot; the reason is in those.  Capped at four."
  (let* ((lines (split-string (monk-exec-output exec) "\n" t))
         (telling (seq-filter (lambda (l) (string-match-p "\\`\\(Error\\|Warning\\|fatal\\|error\\|ssh\\|curl\\):" (string-trim l)))
                              lines))
         (picked (or telling (last lines))))
    (format "exit %d: %s" (monk-exec-exit-code exec)
            (mapconcat #'string-trim (seq-take picked 4) " | "))))

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

(cl-defmethod monk-resource-aggregate ((r monk-brew-resource) others (_m monk-machine))
  "Merge R and OTHERS, brew resources of one kind (formula or cask), into one.
A resource with a rebuild rule stays on its own: its install is a
build, not a download, and must not hold up the batch."
  (when (and (null (oref r rebuild))
             (cl-every (lambda (o) (and (null (oref o rebuild))
                                        (eq (null (oref o cask)) (null (oref r cask)))))
                       others))
    (monk-brew-resource
     :name (format "%s (+%d)" (oref r name) (length others))
     :packages (delete-dups (apply #'append (oref r packages)
                                   (mapcar (lambda (o) (oref o packages)) others)))
     :cask (oref r cask))))

(cl-defmethod monk-resource-disaggregate ((_a monk-brew-resource) (member monk-brew-resource) changes)
  (let ((mine (seq-filter (lambda (name) (member name (oref member packages)))
                          (plist-get changes :install))))
    (and mine (list :install mine
                    :degree (monk-set-degree (length (oref member packages)) (length mine))))))

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

(defun monk--brew-installed-names-uncached (m)
  "Ask brew on M for installed names.  See `monk--brew-installed-names'."
  (let ((exec (monk-exec m monk-brew-program "info" "--json=v2" "--installed")))
    (if (monk-exec-ok-p exec)
        (let ((parsed (json-parse-string (monk-exec-output exec))))
          (append (mapcan (lambda (e) (monk--brew-json-names e nil))
                          (append (gethash "formulae" parsed) nil))
                  (mapcan (lambda (e) (monk--brew-json-names e t))
                          (append (gethash "casks" parsed) nil))))
      (monk-log 'warn "%s: brew info --installed failed, aliases unknown: %s"
                (oref m name) (string-trim (monk-exec-output exec)))
      (append (monk-exec-lines m monk-brew-program "list" "-1" "--formula")
              (monk-exec-lines m monk-brew-program "list" "-1" "--cask")))))

(cl-defmethod monk-check ((r monk-brew-resource) (m monk-machine))
  (let* ((installed (monk--brew-installed-names m))
         (missing (seq-remove
                   (lambda (name)
                     (or (member name installed)
                         ;; A tapped name such as user/tap/formula is
                         ;; installed under its short name.
                         (member (file-name-nondirectory name) installed)))
                   (oref r packages)))
         (rebuild (and (oref r rebuild)
                       (delq nil (mapcar (lambda (name)
                                           (unless (member name missing)
                                             (let ((why (monk--brew-rebuild-reason r m name)))
                                               (and why (cons name why)))))
                                         (oref r packages))))))
    (when (or missing rebuild)
      (append (and missing (list :install missing))
              (and rebuild (list :rebuild rebuild))
              (list :degree (monk-set-degree (length (oref r packages))
                                             (+ (length missing) (length rebuild))))))))

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

(defun monk--brew-install-call (m arguments)
  "Run brew with ARGUMENTS on M, trusting a refused tap once.
Return the tap trusted, or nil.  Homebrew refuses formulae from a tap
nobody ran `brew trust' on.  Trust it and retry; a second refusal
propagates."
  (condition-case err
      (progn (apply #'monk-exec-string m monk-brew-program arguments) nil)
    (monk-exec-error
     (let ((output (format "%s" (car (last err)))))
       (unless (and (string-match monk-brew-untrusted-tap-regexp output)
                    (monk--brew-trust m (match-string 1 output)))
         (signal (car err) (cdr err)))
       (apply #'monk-exec-string m monk-brew-program arguments)
       (match-string 1 output)))))

(defun monk--brew-rebuild-call (m name)
  "Build installed formula NAME on M again.
`brew reinstall' keeps the old keg aside and restores it when the
build fails.  When reinstall itself refuses, as it does for a keg
brew no longer recognizes, `brew install --overwrite' is the way
through; that is what the old Makefile spelled by hand."
  (condition-case err
      (monk-exec-string m monk-brew-program "reinstall" name)
    (monk-exec-error
     (monk-log 'warn "%s: brew reinstall %s failed (%s); trying install --overwrite"
               (oref m name) name (car (last err)))
     (monk-exec-string m monk-brew-program "install" "--overwrite" name))))

(cl-defmethod monk-repair ((r monk-brew-resource) (m monk-machine) changes)
  (let ((process-environment (cons "HOMEBREW_NO_AUTO_UPDATE=1" process-environment))
        (install (plist-get changes :install))
        (rebuild (plist-get changes :rebuild))
        (trusted nil))
    (when install
      (setq trusted (monk--brew-install-call m (append (list "install")
                                                       (and (oref r cask) '("--cask"))
                                                       install))))
    (dolist (cell rebuild)
      (monk--brew-rebuild-call m (car cell)))
    (monk-run-context-clear (list 'brew-installed (oref m name)))
    (string-join
     (delq nil (list (and install (format "installed %s%s" (string-join install " ")
                                          (if trusted (format " (after brew trust %s)" trusted) "")))
                     (and rebuild (format "rebuilt %s" (mapconcat (lambda (c) (format "%s (%s)" (car c) (cdr c)))
                                                                   rebuild ", ")))))
     "; ")))

(monk-define-resource monk-brew monk-brew-resource
  "Ensure Homebrew PACKAGES are installed, in one call.
Independent brew resources on a machine aggregate into one install."
  ((name :type string :doc "Identifies the set.")
   (packages :type list :doc "Formula or cask names; aliases and taps count."))
  ((cask :type boolean :doc "Treat the names as casks.")
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

(defclass monk-apt-resource (monk-resource)
  ((packages :initarg :packages :type list))
  "A set of Debian packages that must be installed.")

(cl-defmethod monk-check ((r monk-apt-resource) (m monk-machine))
  (let* ((exec (apply #'monk-exec m monk-dpkg-query-program
                      "-W" "-f" "${Package} ${db:Status-Status}\n"
                      (oref r packages)))
         ;; dpkg-query exits 1 when any package is unknown but still
         ;; prints the known ones.
         (lines (split-string (monk-exec-output exec) "\n" t))
         (installed (delq nil
                          (mapcar (lambda (line)
                                    (pcase (split-string line " ")
                                      (`(,name "installed") name)))
                                  lines)))
         (missing (seq-remove (lambda (name) (member name installed))
                              (oref r packages))))
    (when missing
      (list :install missing
            :degree (monk-set-degree (length (oref r packages)) (length missing))))))

(cl-defmethod monk-repair ((_r monk-apt-resource) (m monk-machine) changes)
  (let ((process-environment (cons "DEBIAN_FRONTEND=noninteractive" process-environment)))
    (apply #'monk-exec-string m monk-apt-program "install" "-y" "-q"
           (append (plist-get changes :install) (list :sudo t))))
  (format "installed %s" (string-join (plist-get changes :install) " ")))

(cl-defmethod monk-resource-available-p ((_r monk-apt-resource) (m monk-machine))
  (monk-program-available-p m monk-apt-program))

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

(cl-defmethod monk-resource-aggregate ((r monk-apt-resource) others (_m monk-machine))
  "Merge R and OTHERS, apt resources, into one install."
  (monk-apt-resource
   :name (format "%s (+%d)" (oref r name) (length others))
   :packages (delete-dups (apply #'append (oref r packages)
                                 (mapcar (lambda (o) (oref o packages)) others)))
   :sudo t))

(cl-defmethod monk-resource-disaggregate ((_a monk-apt-resource) (member monk-apt-resource) changes)
  (let ((mine (seq-filter (lambda (name) (member name (oref member packages)))
                          (plist-get changes :install))))
    (and mine (list :install mine
                    :degree (monk-set-degree (length (oref member packages)) (length mine))))))

(monk-define-resource monk-apt monk-apt-resource
  "Ensure Debian PACKAGES are installed, in one call.
Independent apt resources on a machine aggregate into one install."
  ((name :type string :doc "Identifies the set.")
   (packages :type list :doc "Package names."))
  ()
  :classes (debian)
  :sudo t)

;;;; package: generic over the machine's package manager

(defun monk-package (name packages &rest args)
  "Ensure PACKAGES are installed through the machine's package manager.
NAME identifies the set.  Dispatches on the `pkg-mgr' fact of the
machine being compiled: homebrew gives `monk-brew', apt gives
`monk-apt'.  ARGS: :cask (brew only) and common keys.  A machine with
another manager gets a resource that reports skipped with the reason,
so the policy still compiles everywhere.  Ansible's `package'."
  (let ((manager (monk-machine-fact (monk-current-machine) 'pkg-mgr)))
    (pcase manager
      ("homebrew" (apply #'monk-brew name packages :classes nil args))
      ("apt" (apply #'monk-apt name packages :classes nil
                    (let ((rest (copy-sequence args))) (cl-remf rest :cask) rest)))
      (_ (apply #'monk-unsupported name (format "package: no supported package manager (pkg-mgr is %s)" manager)
                args)))))

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
