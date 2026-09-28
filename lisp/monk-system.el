;;; monk-system.el --- Cron, git, user, sysctl, systemd resources  -*- lexical-binding: t; -*-

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

;; Resources: `monk-cron', `monk-git', `monk-user-group',
;; `monk-sysctl', `monk-systemd-unit', `monk-command'.

;;; Code:

(require 'monk)
(require 'vc-git)
(require 'monk-file)
(require 'format-spec)

(defconst monk-cron-marker-format "#Monk: %s"
  "Comment line that precedes a Monk-managed cron entry.")

(defconst monk-git-default-branch "master"
  "Branch used when a git resource gives none.")

(defconst monk-systemd-unit-directory "/etc/systemd/system/"
  "Directory for system-level unit files.")

(defconst monk-systemd-user-unit-directory "~/.config/systemd/user/"
  "Directory for user-level unit files.")

(defconst monk-sysctl-config-file "/etc/sysctl.d/90-monk.conf"
  "File that stores persistent sysctl settings.")

(defconst monk-systemd-enabled-states
  '("enabled" "enabled-runtime" "static" "indirect" "alias" "generated" "transient")
  "Outputs of `systemctl is-enabled' that count as enabled.
Units that are static or aliased cannot be enabled by hand, so
treating them as disabled would repair forever.")

(defconst monk-systemd-user-environment
  '("XDG_RUNTIME_DIR=/run/user/%u" "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%u/bus")
  "Environment for `systemctl --user' over a non-login session.
%u is replaced by the numeric uid on the target.")

;;;; cron

(defclass monk-cron-resource (monk-resource)
  ((job :initarg :job
        :type string
        :documentation "The command to run.")
   (schedule :initarg :schedule
             :type string
             :documentation "The five cron time fields as one string.")
   (user :initarg :user
         :initform nil
         :documentation "Crontab owner.  Nil means the login user.  Non-nil implies sudo.")
   (state :initarg :state
          :initform 'present))
  "One entry in a crontab, identified by a marker comment holding NAME.")

(cl-defmethod monk--cron-read ((r monk-cron-resource) (m monk-machine))
  "Return the current crontab text for R on M, or the empty string."
  (let ((exec (if (oref r user)
                  (monk-exec m "crontab" "-u" (oref r user) "-l" :sudo t)
                (monk-exec m "crontab" "-l"))))
    (if (monk-exec-ok-p exec)
        (monk-exec-output exec)
      ;; "no crontab for user" exits 1.
      "")))

(cl-defmethod monk--cron-compute ((r monk-cron-resource) current)
  "Return the crontab text after applying R to CURRENT."
  (let ((marker (format monk-cron-marker-format (oref r name)))
        (entry (format "%s %s" (oref r schedule) (oref r job))))
    (with-temp-buffer
      (insert current)
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (regexp-quote marker) "$") nil t)
        (goto-char (line-beginning-position))
        (let ((start (point)))
          (forward-line 1)
          ;; Remove the job line only when it is not another marker or
          ;; a comment.  A hand-deleted job leaves the marker alone.
          (unless (or (eobp) (looking-at "#"))
            (forward-line 1))
          (delete-region start (point))))
      (when (eq (oref r state) 'present)
        (goto-char (point-max))
        (unless (or (bobp) (bolp)) (insert "\n"))
        (insert marker "\n" entry "\n"))
      (buffer-string))))

(cl-defmethod monk-check ((r monk-cron-resource) (m monk-machine))
  (let* ((current (monk--cron-read r m))
         (new (monk--cron-compute r current)))
    (unless (equal current new)
      (list :crontab t))))

(cl-defmethod monk-diff ((r monk-cron-resource) (m monk-machine) _changes)
  (let ((current (monk--cron-read r m)))
    (monk--unified-diff current (monk--cron-compute r current)
                        (format "crontab:%s" (or (oref r user) "self")))))

(cl-defmethod monk-repair ((r monk-cron-resource) (m monk-machine) _changes)
  (let ((new (monk--cron-compute r (monk--cron-read r m))))
    (if (oref r user)
        (monk-exec-string m "crontab" "-u" (oref r user) "-" :sudo t :input new)
      (monk-exec-string m "crontab" "-" :input new))
    (format "%s entry" (if (eq (oref r state) 'absent) "removed" "set"))))

(defun monk-cron (name schedule job &rest args)
  "Ensure the crontab has entry NAME running JOB on SCHEDULE.
SCHEDULE is the five time fields as one string.  ARGS: :user :state
and common keys."
  (monk-add-resource
   (apply #'monk-cron-resource
          :name name
          (monk--resource-plist args
                                :schedule schedule
                                :job job
                                :user (plist-get args :user)
                                :state (or (plist-get args :state) 'present)))))

;;;; git

(defclass monk-git-resource (monk-resource)
  ((repo :initarg :repo
         :type string
         :documentation "Remote URL.")
   (branch :initarg :branch
           :initform nil
           :documentation "Branch to check out.  Nil means `monk-git-default-branch'.")
   (update :initarg :update
           :initform t
           :documentation "When non-nil, fast-forward an existing checkout on each run."))
  "A git checkout at NAME from REPO.")

(defun monk--git-in (directory &rest args)
  "Run git with ARGS in DIRECTORY through VC and return trimmed output.
DIRECTORY can be a TRAMP path; `vc-git--call' runs through
`process-file', so remote and sudo hops work, and `vc-git-program'
and the VC coding settings apply.  Signal `monk-exec-error' on a
non-zero exit."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((code (apply #'vc-git--call nil '(t t) (car args) (cdr args))))
        (unless (eql 0 code)
          (signal 'monk-exec-error
                  (list default-directory (cons vc-git-program args) code
                        (string-trim (buffer-string)))))
        (string-trim (buffer-string))))))

(cl-defgeneric monk--git (resource machine &rest args)
  "Run git with ARGS inside the checkout of RESOURCE on MACHINE.")

(cl-defmethod monk--git ((r monk-git-resource) (m monk-machine) &rest args)
  (apply #'monk--git-in (monk-machine-path m (oref r name) (oref r sudo)) args))

(cl-defmethod monk-check ((r monk-git-resource) (m monk-machine))
  (let ((full (monk-machine-path m (oref r name) (oref r sudo)))
        (branch (or (oref r branch) monk-git-default-branch)))
    (cond
     ((not (file-directory-p (expand-file-name ".git" full)))
      (when (and (file-exists-p full)
                 (directory-files full nil directory-files-no-dot-files-regexp))
        (signal 'monk-resource-error
                (list (format "%s exists, is not empty, and is not a git checkout"
                              (oref r name)))))
      (list :clone (list (oref r repo) branch)))
     (t
      (let ((changes nil)
            (current-branch (monk--git r m "rev-parse" "--abbrev-ref" "HEAD")))
        (unless (equal current-branch branch)
          (setq changes (plist-put changes :checkout (list current-branch branch))))
        (when (oref r update)
          ;; ls-remote reads the remote without touching the checkout,
          ;; so check stays side-effect free.
          (let* ((remote-head (car (split-string
                                    (monk--git r m "ls-remote" "origin"
                                               (format "refs/heads/%s" branch)))))
                 (local-head (monk--git r m "rev-parse" "HEAD")))
            (when (and remote-head (not (equal remote-head local-head)))
              (setq changes (plist-put changes :behind remote-head)))))
        changes)))))

(cl-defmethod monk-repair ((r monk-git-resource) (m monk-machine) changes)
  (let ((full (monk-machine-path m (oref r name) (oref r sudo)))
        (branch (or (oref r branch) monk-git-default-branch))
        (notes nil))
    (when (plist-get changes :clone)
      (dolist (dir (monk--parent-directories-to-create full))
        (make-directory dir))
      ;; `vc-git-clone' runs in `default-directory', so the parent of
      ;; the checkout on M decides where, and with what rights, it runs.
      (let ((default-directory (file-name-directory (directory-file-name full))))
        (unless (with-temp-buffer
                  (vc-git-clone (oref r repo) (monk-machine-local-path full) branch))
          (signal 'monk-exec-error
                  (list (oref m name) (list vc-git-program "clone" (oref r repo)) 1
                        (format "clone of %s failed" (oref r repo))))))
      (push "cloned" notes))
    (when (plist-get changes :checkout)
      (monk--git r m "checkout" "--quiet" branch)
      (push (format "checked out %s" branch) notes))
    (when (plist-get changes :behind)
      (monk--git r m "fetch" "--quiet" "origin")
      (monk--git r m "merge" "--quiet" "--ff-only" (format "origin/%s" branch))
      (push (format "fast-forwarded to %s"
                    (substring (plist-get changes :behind) 0 8))
            notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((r monk-git-resource))
  "Return the .git directory of R only.
Watching the whole checkout would fire on every edit inside it."
  (list (concat (file-name-as-directory (oref r name)) ".git")))

(defun monk-git (path repo &rest args)
  "Ensure a git checkout of REPO at PATH.  ARGS: :branch :update and common keys."
  (monk-add-resource
   (apply #'monk-git-resource
          :name path
          (monk--resource-plist args
                                :repo repo
                                :branch (plist-get args :branch)
                                :update (if (plist-member args :update)
                                            (plist-get args :update)
                                          t)))))

;;;; user-group

(defclass monk-user-group-resource (monk-resource)
  ((user :initarg :user :type string)
   (group :initarg :group :type string))
  "Membership of USER in supplementary GROUP.  Linux only.")

(cl-defmethod monk-check ((r monk-user-group-resource) (m monk-machine))
  (let ((groups (split-string (monk-exec-string m "id" "-Gn" (oref r user)))))
    (unless (member (oref r group) groups)
      (list :add-to-group (oref r group)))))

(cl-defmethod monk-repair ((r monk-user-group-resource) (m monk-machine) _changes)
  (monk-exec-string m "usermod" "-aG" (oref r group) (oref r user) :sudo t)
  (format "added %s to %s" (oref r user) (oref r group)))

(defun monk-user-group (user group &rest args)
  "Ensure USER is a member of GROUP.  ARGS: common keys."
  (monk-add-resource
   (apply #'monk-user-group-resource
          :name (format "%s:%s" user group)
          (monk--resource-plist args :user user :group group :sudo t))))

;;;; sysctl

(defclass monk-sysctl-resource (monk-resource)
  ((value :initarg :value :type string))
  "A kernel parameter NAME with VALUE, live and persisted.  Linux only.")

(cl-defmethod monk-check ((r monk-sysctl-resource) (m monk-machine))
  (let ((live (string-trim (monk-exec-string m "sysctl" "-n" (oref r name))))
        (changes nil))
    ;; sysctl prints tab-separated values for some keys.
    (unless (equal (replace-regexp-in-string "[ \t]+" " " live) (oref r value))
      (setq changes (plist-put changes :live (list live (oref r value)))))
    (let* ((persisted (monk-file-read m monk-sysctl-config-file t))
           (want (format "%s = %s" (oref r name) (oref r value))))
      (unless (and persisted (string-match-p (concat "^" (regexp-quote want) "$") persisted))
        (setq changes (plist-put changes :persist want))))
    changes))

(cl-defmethod monk-repair ((r monk-sysctl-resource) (m monk-machine) changes)
  (let ((notes nil))
    (when (plist-get changes :live)
      (monk-exec-string m "sysctl" "-w" (format "%s=%s" (oref r name) (oref r value))
                        :sudo t)
      (push "set live" notes))
    (when (plist-get changes :persist)
      (let ((current (monk-file-read m monk-sysctl-config-file t)))
        (monk-file-write m monk-sysctl-config-file
                         (monk-edit-line current (plist-get changes :persist)
                                         :regexp (concat "^" (regexp-quote (oref r name))
                                                         "\\s-*="))
                         t monk-file-default-mode))
      (push "persisted" notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((_r monk-sysctl-resource))
  (list monk-sysctl-config-file))

(defun monk-sysctl (name value &rest args)
  "Ensure kernel parameter NAME is VALUE.  ARGS: common keys."
  (monk-add-resource
   (apply #'monk-sysctl-resource
          :name name
          (monk--resource-plist args :value (format "%s" value) :sudo t))))

;;;; systemd-unit

(defclass monk-systemd-unit-resource (monk-resource)
  ((content :initarg :content
            :initform nil
            :documentation "Unit file content.  Nil means do not manage the file.")
   (enabled :initarg :enabled
            :initform t)
   (started :initarg :started
            :initform t)
   (user-unit :initarg :user-unit
              :initform nil
              :documentation "Manage a user unit with systemctl --user."))
  "A systemd unit NAME: its file, its enablement, and its active state.")

(cl-defgeneric monk--systemctl (resource machine &rest args)
  "Run systemctl with ARGS for RESOURCE on MACHINE.  Return a `monk-exec'.")

(cl-defmethod monk--systemctl ((r monk-systemd-unit-resource) (m monk-machine) &rest args)
  (if (oref r user-unit)
      (let* ((uid (monk-exec-string m "id" "-u"))
             (env (mapcar (lambda (spec) (format-spec spec (list (cons ?u uid))))
                          monk-systemd-user-environment)))
        (apply #'monk-exec m "env" (append env (list "systemctl" "--user") args)))
    (apply #'monk-exec m "systemctl" (append args (list :sudo t)))))

(cl-defmethod monk--systemd-unit-path ((r monk-systemd-unit-resource))
  "Return the unit file path for R."
  (concat (if (oref r user-unit)
              monk-systemd-user-unit-directory
            monk-systemd-unit-directory)
          (oref r name)))

(cl-defmethod monk-check ((r monk-systemd-unit-resource) (m monk-machine))
  (let ((changes nil)
        (sudo (not (oref r user-unit))))
    (when (oref r content)
      (unless (equal (oref r content)
                     (monk-file-read m (monk--systemd-unit-path r) sudo))
        (setq changes (plist-put changes :content t))))
    (let ((enabled (string-trim
                    (monk-exec-output (monk--systemctl r m "is-enabled" (oref r name)))))
          (active (string-trim
                   (monk-exec-output (monk--systemctl r m "is-active" (oref r name))))))
      (unless (eq (oref r enabled)
                  (and (member enabled monk-systemd-enabled-states) t))
        (setq changes (plist-put changes :enabled (list enabled (oref r enabled)))))
      (unless (eq (oref r started) (equal active "active"))
        (setq changes (plist-put changes :started (list active (oref r started))))))
    changes))

(cl-defmethod monk-diff ((r monk-systemd-unit-resource) (m monk-machine) changes)
  (when (plist-get changes :content)
    (monk--unified-diff (monk-file-read m (monk--systemd-unit-path r)
                                        (not (oref r user-unit)))
                        (oref r content)
                        (monk--systemd-unit-path r))))

(cl-defmethod monk-repair ((r monk-systemd-unit-resource) (m monk-machine) changes)
  (let ((notes nil))
    (when (plist-get changes :content)
      (monk-file-write m (monk--systemd-unit-path r) (oref r content)
                       (not (oref r user-unit)) monk-file-default-mode)
      (monk--systemctl r m "daemon-reload")
      (push "unit file" notes))
    (when (plist-get changes :enabled)
      (monk--systemctl r m (if (oref r enabled) "enable" "disable") (oref r name))
      (push (if (oref r enabled) "enabled" "disabled") notes))
    (when (or (plist-get changes :started)
              (and (plist-get changes :content) (oref r started)))
      (monk--systemctl r m (if (oref r started) "restart" "stop") (oref r name))
      (push (if (oref r started) "restarted" "stopped") notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-default-watch-action ((_r monk-systemd-unit-resource))
  'restart)

(cl-defmethod monk-action ((r monk-systemd-unit-resource) (m monk-machine) action)
  (pcase action
    ((or 'restart 'reload 'stop 'start)
     (let ((exec (monk--systemctl r m (symbol-name action) (oref r name))))
       (unless (monk-exec-ok-p exec)
         (signal 'monk-exec-error (list (oref r name) action (monk-exec-output exec))))
       (format "%s %s" action (oref r name))))
    ('daemon-reload
     (monk--systemctl r m "daemon-reload")
     "daemon-reload")
    (_ (cl-call-next-method))))

(cl-defmethod monk-resource-paths ((r monk-systemd-unit-resource))
  (and (oref r content) (list (monk--systemd-unit-path r))))

(defun monk-systemd-unit (name &rest args)
  "Ensure systemd unit NAME.
ARGS: :content :enabled :started :user-unit and common keys."
  (monk-add-resource
   (apply #'monk-systemd-unit-resource
          :name name
          (monk--resource-plist args
                                :content (plist-get args :content)
                                :enabled (if (plist-member args :enabled)
                                             (plist-get args :enabled)
                                           t)
                                :started (if (plist-member args :started)
                                             (plist-get args :started)
                                           t)
                                :user-unit (plist-get args :user-unit)))))

;;;; command

(defclass monk-command-resource (monk-resource)
  ((program :initarg :program :type string)
   (args :initarg :args :initform nil)
   (creates :initarg :creates
            :initform nil
            :documentation "Path.  When it exists, the command is kept.")
   (unless-command :initarg :unless-command
                   :initform nil
                   :documentation "List (PROGRAM . ARGS).  Zero exit means kept."))
  "A command that runs unless CREATES exists or UNLESS-COMMAND succeeds.
Without either guard the command is always kept and runs only as a
watch action, so `:watch' makes it a handler in the Ansible sense.")

(cl-defmethod monk-check ((r monk-command-resource) (m monk-machine))
  (cond
   ((and (null (oref r creates)) (null (oref r unless-command)))
    nil)
   ((and (oref r creates)
         (file-exists-p (monk-machine-path m (oref r creates) (oref r sudo))))
    nil)
   ((and (oref r unless-command)
         (monk-exec-ok-p (apply #'monk-exec m (car (oref r unless-command))
                                (append (cdr (oref r unless-command))
                                        (list :sudo (oref r sudo))))))
    nil)
   (t (list :run (cons (oref r program) (oref r args))))))

(cl-defmethod monk-repair ((r monk-command-resource) (m monk-machine) _changes)
  (let ((output (apply #'monk-exec-string m (oref r program)
                       (append (oref r args) (list :sudo (oref r sudo))))))
    (if (string-empty-p output) "ran" (truncate-string-to-width output 200))))

(cl-defmethod monk-default-watch-action ((_r monk-command-resource))
  'run)

(cl-defmethod monk-action ((r monk-command-resource) (m monk-machine) action)
  (pcase action
    ('run (monk-repair r m nil))
    (_ (cl-call-next-method))))

(cl-defmethod monk-resource-paths ((r monk-command-resource))
  (and (oref r creates) (list (oref r creates))))

(defun monk-command (name program &rest args)
  "Ensure the effect of PROGRAM.  NAME identifies the resource.
ARGS: :args LIST, :creates PATH, :unless (PROGRAM . ARGS), and common
keys.  Without :creates or :unless the command runs only when a
watched ref changed."
  (monk-add-resource
   (apply #'monk-command-resource
          :name name
          (monk--resource-plist args
                                :program program
                                :args (plist-get args :args)
                                :creates (plist-get args :creates)
                                :unless-command (plist-get args :unless)))))

(provide 'monk-system)
;;; monk-system.el ends here
