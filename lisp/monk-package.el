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

(cl-defmethod monk--brew-installed ((r monk-brew-resource) (m monk-machine))
  "Return the installed formula or cask names on M, per the cask slot of R."
  (apply #'monk-exec-lines m monk-brew-program "list" "-1"
         (if (oref r cask) '("--cask") '("--formula"))))

(cl-defmethod monk-check ((r monk-brew-resource) (m monk-machine))
  (let* ((installed (monk--brew-installed r m))
         (missing (seq-remove
                   (lambda (name)
                     ;; Tapped formulae list under their short name.
                     (member (file-name-nondirectory name) installed))
                   (oref r packages))))
    (when missing
      (list :install missing
            :degree (monk-set-degree (length (oref r packages)) (length missing))))))

(cl-defmethod monk-repair ((r monk-brew-resource) (m monk-machine) changes)
  (apply #'monk-exec-string m monk-brew-program "install"
         (append (and (oref r cask) '("--cask")) (plist-get changes :install)))
  (format "installed %s" (string-join (plist-get changes :install) " ")))

(defun monk-brew (name packages &rest args)
  "Ensure Homebrew PACKAGES are installed.  NAME identifies the set.
ARGS: :cask and common keys."
  (monk-add-resource
   (apply #'monk-brew-resource
          :name name
          (monk--resource-plist args
                                :packages packages
                                :cask (plist-get args :cask)
                                :classes (append '(darwin) (plist-get args :classes))))))

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
  "tapped")

(defun monk-brew-tap (name &rest args)
  "Ensure Homebrew tap NAME is present.  ARGS: common keys."
  (monk-add-resource
   (apply #'monk-brew-tap-resource
          :name name
          (monk--resource-plist args
                                :classes (append '(darwin) (plist-get args :classes))))))

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

(defun monk-apt (name packages &rest args)
  "Ensure Debian PACKAGES are installed.  NAME identifies the set.
ARGS: common keys."
  (monk-add-resource
   (apply #'monk-apt-resource
          :name name
          (monk--resource-plist args
                                :packages packages
                                :sudo t
                                :classes (append '(debian) (plist-get args :classes))))))

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

(defun monk-osx-default (domain key value &rest args)
  "Ensure macOS default DOMAIN KEY is VALUE.
ARGS: :type (string int float bool array) and common keys."
  (monk-add-resource
   (apply #'monk-osx-default-resource
          :name (format "%s:%s" domain key)
          (monk--resource-plist args
                                :domain domain
                                :key key
                                :value value
                                :value-type (or (plist-get args :type) 'string)
                                :classes (append '(darwin) (plist-get args :classes))))))

(provide 'monk-package)
;;; monk-package.el ends here
