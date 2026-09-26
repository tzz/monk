;;; monk-file.el --- File resources for Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, files

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

;; Resources: `monk-directory', `monk-symlink', `monk-file',
;; `monk-line-in-file', `monk-absent'.
;;
;; All paths are interpreted on the target machine.  A leading `~'
;; expands to the home directory of the login user, or of root when
;; :sudo is set.

;;; Code:

(require 'monk)

(defconst monk-file-default-mode #o644
  "Mode for files when the policy gives none.")

(defconst monk-directory-default-mode #o755
  "Mode for directories when the policy gives none.")

(defconst monk-diff-program "diff"
  "Program used to produce unified diffs.")

(defconst monk-stat-owner-args-gnu '("-c" "%U %G")
  "Arguments that make GNU stat print owner and group.")

(defconst monk-stat-owner-args-bsd '("-f" "%Su %Sg")
  "Arguments that make BSD and macOS stat print owner and group.")

;;;; Shared helpers

(defun monk--mode-string (mode)
  "Return MODE as an octal string."
  (format "%04o" mode))

(cl-defgeneric monk-file-read (machine path &optional sudo)
  "Return the content of PATH on MACHINE as a string, or nil when absent.
When SUDO is non-nil, read as root.")

(cl-defgeneric monk-file-write (machine path content &optional sudo mode)
  "Write CONTENT to PATH on MACHINE.  Set MODE when non-nil.
When SUDO is non-nil, write as root.")

(cl-defgeneric monk--ownership-changes (machine full owner group sudo)
  "Return ownership changes for FULL on MACHINE against OWNER and GROUP.")

(cl-defgeneric monk--apply-ownership (machine path owner group sudo)
  "Set OWNER and GROUP on PATH of MACHINE.")

(cl-defmethod monk-file-read ((m monk-machine) path &optional sudo)
  (let ((full (monk-machine-path m path sudo)))
    (when (file-exists-p full)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents full))
        (buffer-string)))))

(cl-defmethod monk-file-write ((m monk-machine) path content &optional sudo mode)
  (let ((full (monk-machine-path m path sudo)))
    (let ((parent (file-name-directory full)))
      (when (and parent (not (file-directory-p parent)))
        (make-directory parent t)))
    (with-temp-file full
      (let ((coding-system-for-write 'utf-8-unix))
        (insert content)))
    (when mode
      (set-file-modes full mode))
    full))

(defun monk--unified-diff (old new label)
  "Return a unified diff between strings OLD and NEW labeled LABEL."
  (let ((old-file (make-temp-file "monk-old"))
        (new-file (make-temp-file "monk-new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (or old "")))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            (call-process monk-diff-program nil t nil "-u"
                          "--label" (concat "a/" label)
                          "--label" (concat "b/" label)
                          old-file new-file)
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(defun monk--parent-directories-to-create (full)
  "Return the missing ancestors of FULL, shallowest first."
  (let ((missing nil)
        (dir (file-name-directory (directory-file-name full))))
    (while (and dir (not (file-directory-p dir)))
      (push dir missing)
      (setq dir (file-name-directory (directory-file-name dir))))
    missing))

;;;; directory

(defclass monk-directory-resource (monk-resource)
  ((mode :initarg :mode
         :initform nil
         :documentation "Octal integer mode, or nil to leave the mode alone.")
   (owner :initarg :owner
          :initform nil
          :documentation "Owner user name, or nil to leave the owner alone.")
   (group :initarg :group
          :initform nil
          :documentation "Group name, or nil to leave the group alone."))
  "A directory that must exist.")

(cl-defmethod monk-check ((r monk-directory-resource) (m monk-machine))
  (let* ((full (monk-machine-path m (oref r name) (oref r sudo)))
         (changes nil))
    (cond
     ((not (file-exists-p full))
      (setq changes (plist-put changes :create t)))
     ((not (file-directory-p full))
      (signal 'monk-resource-error
              (list (format "%s exists and is not a directory" (oref r name)))))
     (t
      (when (and (oref r mode)
                 (/= (logand (file-modes full) #o7777) (oref r mode)))
        (setq changes (plist-put changes :mode
                                 (list (file-modes full) (oref r mode)))))
      (let ((ownership (monk--ownership-changes m full (oref r owner) (oref r group)
                                                (oref r sudo))))
        (when ownership
          (setq changes (append changes ownership))))))
    changes))

(cl-defmethod monk-repair ((r monk-directory-resource) (m monk-machine) changes)
  (let ((full (monk-machine-path m (oref r name) (oref r sudo)))
        (notes nil))
    (when (plist-get changes :create)
      (make-directory full t)
      (when (oref r mode)
        (set-file-modes full (oref r mode)))
      (push "created" notes))
    (when (plist-get changes :mode)
      (set-file-modes full (oref r mode))
      (push (format "mode %s" (monk--mode-string (oref r mode))) notes))
    (when (or (plist-get changes :owner) (plist-get changes :group)
              (and (plist-get changes :create) (or (oref r owner) (oref r group))))
      (monk--apply-ownership m (monk-machine-local-path full)
                             (oref r owner) (oref r group) (oref r sudo))
      (push "ownership" notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((r monk-directory-resource))
  (list (oref r name)))

(cl-defmethod monk-resource-owns-subtree-p ((_r monk-directory-resource))
  t)

(defun monk-directory (path &rest args)
  "Ensure directory PATH exists.  ARGS: :mode :owner :group and common keys."
  (monk-add-resource
   (apply #'monk-directory-resource
          :name path
          (monk--resource-plist args
                                :mode (plist-get args :mode)
                                :owner (plist-get args :owner)
                                :group (plist-get args :group)))))

;;;; ownership helpers

(cl-defmethod monk--ownership-changes ((m monk-machine) full owner group sudo)
  (when (or owner group)
    (let* ((stat-args (if (monk-class-p m 'darwin)
                          monk-stat-owner-args-bsd
                        monk-stat-owner-args-gnu))
           (current (split-string (apply #'monk-exec-string m "stat"
                                         (append stat-args
                                                 (list (monk-machine-local-path full)
                                                       :sudo sudo)))))
           (changes nil))
      (when (and owner (not (equal owner (car current))))
        (setq changes (plist-put changes :owner (list (car current) owner))))
      (when (and group (not (equal group (cadr current))))
        (setq changes (plist-put changes :group (list (cadr current) group))))
      changes)))

(cl-defmethod monk--apply-ownership ((m monk-machine) path owner group sudo)
  (monk-exec-string m "chown"
                    (concat (or owner "") (and group (concat ":" group)))
                    path :sudo sudo))

;;;; symlink

(defclass monk-symlink-resource (monk-resource)
  ((target :initarg :target
           :type string
           :documentation "Path the link points to.  A leading ~ expands on the target.")
   (force :initarg :force
          :initform t
          :documentation "Replace an existing file or link at the link path."))
  "A symbolic link at NAME that points to TARGET.")

(cl-defmethod monk--symlink-target ((r monk-symlink-resource) (m monk-machine))
  "Return the target of R as the link text on M.
A leading `~' is expanded on M, because `make-symbolic-link' stores
the text verbatim and a literal `~' in a link is dangling."
  (monk-machine-expand m (oref r target) (oref r sudo)))

(cl-defmethod monk-check ((r monk-symlink-resource) (m monk-machine))
  (let* ((full (monk-machine-path m (oref r name) (oref r sudo)))
         (target (monk--symlink-target r m))
         (current (file-symlink-p full)))
    (cond
     ((equal current target) nil)
     (current
      (list :retarget (list current target)))
     ((file-exists-p full)
      (unless (oref r force)
        (signal 'monk-resource-error
                (list (format "%s exists and is not a link" (oref r name)))))
      (list :replace (list (if (file-directory-p full) 'directory 'file) target)))
     (t
      (list :create target)))))

(cl-defmethod monk-repair ((r monk-symlink-resource) (m monk-machine) changes)
  (let ((full (monk-machine-path m (oref r name) (oref r sudo)))
        (target (monk--symlink-target r m)))
    (dolist (dir (monk--parent-directories-to-create full))
      (make-directory dir)
      (set-file-modes dir #o700))
    (cond
     ((plist-get changes :replace)
      (if (eq 'directory (car (plist-get changes :replace)))
          (delete-directory full t)
        (delete-file full)))
     ((plist-get changes :retarget)
      (delete-file full)))
    (make-symbolic-link target full)
    (format "-> %s" target)))

(cl-defmethod monk-resource-paths ((r monk-symlink-resource))
  "Return the link of R and its target.
The watcher wants both, so that a replaced target re-checks the link."
  (list (oref r name) (oref r target)))

(defun monk-symlink (path target &rest args)
  "Ensure PATH is a symbolic link to TARGET.  ARGS: :force and common keys."
  (monk-add-resource
   (apply #'monk-symlink-resource
          :name path
          (monk--resource-plist args
                                :target target
                                :force (if (plist-member args :force)
                                           (plist-get args :force)
                                         t)))))

;;;; file

(defclass monk-file-resource (monk-resource)
  ((content :initarg :content
            :initform nil
            :documentation "Desired content string.  Mutually exclusive with source.")
   (source :initarg :source
           :initform nil
           :documentation "Controller-side file to copy.  Mutually exclusive with content.")
   (mode :initarg :mode
         :initform nil
         :documentation "Octal integer mode, or nil for `monk-file-default-mode' on create.")
   (owner :initarg :owner :initform nil)
   (group :initarg :group :initform nil))
  "A file whose full content Monk owns.")

(cl-defmethod monk--file-desired-content ((r monk-file-resource))
  "Return the desired content of R."
  (cond
   ((oref r content) (oref r content))
   ((oref r source)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents (expand-file-name (oref r source))))
      (buffer-string)))
   (t (signal 'monk-resource-error
              (list (format "%s has neither :content nor :source" (oref r name)))))))

(cl-defmethod monk-check ((r monk-file-resource) (m monk-machine))
  (let* ((full (monk-machine-path m (oref r name) (oref r sudo)))
         (desired (monk--file-desired-content r))
         (current (monk-file-read m (oref r name) (oref r sudo)))
         (changes nil))
    (cond
     ((null current)
      (setq changes (plist-put changes :create t)))
     ((not (equal current desired))
      (setq changes (plist-put changes :content t))))
    (when (and current (oref r mode)
               (/= (logand (file-modes full) #o7777) (oref r mode)))
      (setq changes (plist-put changes :mode (list (file-modes full) (oref r mode)))))
    (when current
      (setq changes (append changes
                            (monk--ownership-changes m full (oref r owner) (oref r group)
                                                     (oref r sudo)))))
    changes))

(cl-defmethod monk-diff ((r monk-file-resource) (m monk-machine) changes)
  (when (or (plist-get changes :create) (plist-get changes :content))
    (monk--unified-diff (monk-file-read m (oref r name) (oref r sudo))
                        (monk--file-desired-content r)
                        (oref r name))))

(cl-defmethod monk-repair ((r monk-file-resource) (m monk-machine) changes)
  (let ((notes nil))
    (when (or (plist-get changes :create) (plist-get changes :content))
      (monk-file-write m (oref r name) (monk--file-desired-content r) (oref r sudo)
                       (or (oref r mode)
                           (and (plist-get changes :create) monk-file-default-mode)))
      (push (if (plist-get changes :create) "created" "content") notes))
    (when (plist-get changes :mode)
      (set-file-modes (monk-machine-path m (oref r name) (oref r sudo)) (oref r mode))
      (push (format "mode %s" (monk--mode-string (oref r mode))) notes))
    (when (or (plist-get changes :owner) (plist-get changes :group)
              (and (plist-get changes :create) (or (oref r owner) (oref r group))))
      (monk--apply-ownership m (monk-machine-local-path
                                (monk-machine-path m (oref r name) (oref r sudo)))
                             (oref r owner) (oref r group) (oref r sudo))
      (push "ownership" notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((r monk-file-resource))
  (list (oref r name)))

(defun monk-file (path &rest args)
  "Ensure PATH has the given content.
ARGS: :content STRING or :source LOCAL-PATH, plus :mode :owner :group
and common keys."
  (monk-add-resource
   (apply #'monk-file-resource
          :name path
          (monk--resource-plist args
                                :content (plist-get args :content)
                                :source (plist-get args :source)
                                :mode (plist-get args :mode)
                                :owner (plist-get args :owner)
                                :group (plist-get args :group)))))

;;;; line-in-file

(defclass monk-line-in-file-resource (monk-resource)
  ((path :initarg :path
         :type string
         :documentation "The file to edit.  The name slot carries a suffix.")
   (line :initarg :line
         :type string
         :documentation "The line that must be present, without newline.")
   (regexp :initarg :regexp
           :initform nil
           :documentation "Regexp that identifies the line to replace.
When nil, match the literal line.")
   (state :initarg :state
          :initform 'present
          :documentation "One of `present' or `absent'.")
   (insert-after :initarg :insert-after
                 :initform nil
                 :documentation "Regexp.  Insert after the last match, else at end.")
   (insert-before :initarg :insert-before
                  :initform nil
                  :documentation "Regexp.  Insert before the first match, else at end.")
   (create :initarg :create
           :initform nil
           :documentation "Create the file when it does not exist."))
  "A single line that must be present or absent in a file.")

(cl-defun monk-edit-line (current line &key regexp (state 'present)
                                  insert-after insert-before)
  "Return CURRENT text with LINE made present or absent.
REGEXP identifies the line to replace.  When nil, match LINE
literally.  STATE is `present' or `absent'.  INSERT-AFTER and
INSERT-BEFORE are regexps that place a new line.  Without them, or
without a match, append at the end."
  (let ((regexp (or regexp (concat "^" (regexp-quote line) "$"))))
    (with-temp-buffer
      (insert (or current ""))
      (pcase state
        ('absent
         (goto-char (point-min))
         (while (re-search-forward regexp nil t)
           (delete-region (line-beginning-position)
                          (min (point-max) (1+ (line-end-position))))
           (unless (bolp) (forward-line 1))))
        ('present
         (goto-char (point-min))
         (let ((found nil)
               (literal (concat "^" (regexp-quote line) "$")))
           (while (and (not found) (re-search-forward regexp nil t))
             (setq found t)
             (delete-region (line-beginning-position) (line-end-position))
             (insert line))
           ;; The regexp identifies the old line.  After one repair the
           ;; new line often no longer matches it, so a literal hit
           ;; also counts as present.
           (unless found
             (goto-char (point-min))
             (setq found (re-search-forward literal nil t)))
           (unless found
             (cond
              ((and insert-before
                    (progn (goto-char (point-min))
                           (re-search-forward insert-before nil t)))
               (goto-char (line-beginning-position))
               (insert line "\n"))
              ((and insert-after
                    (progn (goto-char (point-max))
                           (re-search-backward insert-after nil t)))
               (goto-char (line-end-position))
               (insert "\n" line))
              (t
               (goto-char (point-max))
               (unless (or (bobp) (bolp)) (insert "\n"))
               (insert line "\n"))))))
        (other (signal 'monk-resource-error
                       (list (format "Unknown line state %S" other)))))
      (buffer-string))))

(cl-defmethod monk--line-in-file-compute ((r monk-line-in-file-resource) current)
  "Return the new content of the file for R given CURRENT content."
  (monk-edit-line current (oref r line)
                  :regexp (oref r regexp)
                  :state (oref r state)
                  :insert-after (oref r insert-after)
                  :insert-before (oref r insert-before)))

(cl-defmethod monk-check ((r monk-line-in-file-resource) (m monk-machine))
  (let ((current (monk-file-read m (oref r path) (oref r sudo))))
    (cond
     ((and (null current) (eq (oref r state) 'absent)) nil)
     ((and (null current) (not (oref r create)))
      (signal 'monk-resource-error
              (list (format "%s does not exist and :create is nil" (oref r path)))))
     (t
      (let ((new (monk--line-in-file-compute r current)))
        (unless (equal new current)
          (list :content t :create (null current))))))))

(cl-defmethod monk-diff ((r monk-line-in-file-resource) (m monk-machine) _changes)
  (let ((current (monk-file-read m (oref r path) (oref r sudo))))
    (monk--unified-diff current (monk--line-in-file-compute r current) (oref r path))))

(cl-defmethod monk-repair ((r monk-line-in-file-resource) (m monk-machine) changes)
  (let ((current (monk-file-read m (oref r path) (oref r sudo))))
    (monk-file-write m (oref r path) (monk--line-in-file-compute r current)
                     (oref r sudo)
                     (and (plist-get changes :create) monk-file-default-mode))
    (format "%s line" (if (eq (oref r state) 'absent) "removed" "set"))))

(cl-defmethod monk-resource-paths ((r monk-line-in-file-resource))
  (list (oref r path)))

(defun monk-line-in-file (path line &rest args)
  "Ensure LINE is present in PATH, or absent when :state is `absent'.
ARGS: :regexp :state :insert-after :insert-before :create and common
keys.  The resource name is PATH followed by the regexp or line, so
one file can hold several line resources."
  (monk-add-resource
   (apply #'monk-line-in-file-resource
          :name (format "%s#%s" path (or (plist-get args :regexp) line))
          (monk--resource-plist args
                                :path path
                                :line line
                                :regexp (plist-get args :regexp)
                                :state (or (plist-get args :state) 'present)
                                :insert-after (plist-get args :insert-after)
                                :insert-before (plist-get args :insert-before)
                                :create (plist-get args :create)))))

;;;; absent

(defclass monk-absent-resource (monk-resource)
  ()
  "A path that must not exist.")

(cl-defmethod monk-check ((r monk-absent-resource) (m monk-machine))
  (let ((full (monk-machine-path m (oref r name) (oref r sudo))))
    (when (or (file-exists-p full) (file-symlink-p full))
      (list :remove (cond ((file-symlink-p full) 'symlink)
                          ((file-directory-p full) 'directory)
                          (t 'file))))))

(cl-defmethod monk-repair ((r monk-absent-resource) (m monk-machine) changes)
  (let ((full (monk-machine-path m (oref r name) (oref r sudo))))
    (if (eq 'directory (plist-get changes :remove))
        (delete-directory full t)
      (delete-file full))
    (format "removed %s" (plist-get changes :remove))))

(cl-defmethod monk-resource-paths ((r monk-absent-resource))
  (list (oref r name)))

(defun monk-absent (path &rest args)
  "Ensure PATH does not exist.  ARGS: common keys."
  (monk-add-resource
   (apply #'monk-absent-resource :name path (monk--resource-plist args))))

(provide 'monk-file)
;;; monk-file.el ends here
