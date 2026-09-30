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
         :documentation "Treat the names as casks."))
  "A set of Homebrew formulae or casks that must be installed.")

(cl-defmethod monk-resource-available-p ((_r monk-brew-resource) (m monk-machine))
  (monk-program-available-p m monk-brew-program))

(cl-defmethod monk-resource-class-init ((_r monk-brew-resource) (m monk-machine))
  "Update Homebrew on M once per run, so no install call does it noisily."
  (if (monk-exec-ok-p (monk-exec m monk-brew-program "update" "--quiet"))
      "brew update"
    (monk-log 'warn "%s: brew update failed; installs go on with stale taps" (oref m name))
    nil))

(cl-defmethod monk-resource-aggregate ((r monk-brew-resource) others (_m monk-machine))
  "Merge R and OTHERS, brew resources of one kind (formula or cask), into one."
  (when (cl-every (lambda (o) (eq (null (oref o cask)) (null (oref r cask)))) others)
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
                   (oref r packages))))
    (when missing
      (list :install missing
            :degree (monk-set-degree (length (oref r packages)) (length missing))))))

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

(cl-defmethod monk-repair ((r monk-brew-resource) (m monk-machine) changes)
  (let ((process-environment (cons "HOMEBREW_NO_AUTO_UPDATE=1" process-environment))
        (arguments (append (list "install")
                           (and (oref r cask) '("--cask"))
                           (plist-get changes :install)))
        (trusted nil))
    (condition-case err
        (apply #'monk-exec-string m monk-brew-program arguments)
      (monk-exec-error
       ;; Homebrew refuses formulae from a tap nobody ran `brew trust'
       ;; on.  Trust it once and retry; a second refusal propagates.
       (let ((output (format "%s" (car (last err)))))
         (unless (and (string-match monk-brew-untrusted-tap-regexp output)
                      (monk--brew-trust m (match-string 1 output)))
           (signal (car err) (cdr err)))
         (setq trusted (match-string 1 output))
         (apply #'monk-exec-string m monk-brew-program arguments))))
    (monk-run-context-clear (list 'brew-installed (oref m name)))
    (format "installed %s%s" (string-join (plist-get changes :install) " ")
            (if trusted (format " (after brew trust %s)" trusted) ""))))

(monk-define-resource monk-brew monk-brew-resource
  "Ensure Homebrew PACKAGES are installed, in one call.
Independent brew resources on a machine aggregate into one install."
  ((name :type string :doc "Identifies the set.")
   (packages :type list :doc "Formula or cask names; aliases and taps count."))
  ((cask :type boolean :doc "Treat the names as casks."))
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
