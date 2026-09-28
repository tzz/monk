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
(require 'monk-cell)
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

(defconst monk-launchd-agent-directory "~/Library/LaunchAgents/"
  "Directory for per-user launchd agents.")

(defconst monk-launchd-daemon-directory "/Library/LaunchDaemons/"
  "Directory for system launchd daemons.")

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
    (list (monk-diff-content current (monk--cron-compute r current)
                             (format "crontab:%s" (or (oref r user) "self"))))))

(cl-defmethod monk-repair ((r monk-cron-resource) (m monk-machine) _changes)
  (let ((new (monk--cron-compute r (monk--cron-read r m))))
    (if (oref r user)
        (monk-exec-string m "crontab" "-u" (oref r user) "-" :sudo t :input new)
      (monk-exec-string m "crontab" "-" :input new))
    (format "%s entry" (if (eq (oref r state) 'absent) "removed" "set"))))

(monk-define-resource monk-cron monk-cron-resource
  "Ensure the crontab has entry NAME running JOB on SCHEDULE."
  ((name :type string :doc "Identifies the entry; written as a comment above it.")
   (schedule :type string :doc "The five time fields as one string.")
   (job :type string :doc "The command."))
  ((user :type string :doc "Whose crontab; nil means the login user.  Needs sudo for others.")
   (state :type symbol :default present :choices (present absent))))

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

(monk-define-resource monk-git monk-git-resource
  "Ensure a git checkout of REPO at PATH."
  ((path :type path :doc "The checkout directory.")
   (repo :type string :doc "Remote URL."))
  ((branch :type string :doc "Branch to check out; default `monk-git-default-branch'.")
   (update :type boolean :default t :doc "Fast-forward an existing checkout on each run.")))

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

(monk-define-resource monk-user-group monk-user-group-resource
  "Ensure USER is a member of GROUP."
  ((user :type string :doc "Login name.")
   (group :type string :doc "Group name."))
  ()
  :sudo t
  :name-form (format "%s:%s" user group))

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

(monk-define-resource monk-sysctl monk-sysctl-resource
  "Ensure kernel parameter NAME is VALUE, live and in the sysctl config."
  ((name :type string :doc "The parameter, for example net.core.somaxconn.")
   (value :type any :doc "The value; printed with %s."))
  ()
  :sudo t
  :init ((setq args (plist-put args :value (format "%s" value)))))

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

(cl-defmethod monk-resource-available-p ((_r monk-systemd-unit-resource) (m monk-machine))
  "Systemd units need systemctl on M."
  (unless (monk-run-context-get (list 'systemctl (oref m name)))
    (monk-run-context-put (list 'systemctl (oref m name))
                          (if (monk-exec-ok-p (monk-exec m "sh" "-c" "command -v systemctl"))
                              'yes 'no)))
  (and (eq 'no (monk-run-context-get (list 'systemctl (oref m name))))
       "systemctl is not on this machine"))

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
    (list (monk-diff-content (monk-file-read m (monk--systemd-unit-path r)
                                             (not (oref r user-unit)))
                             (oref r content)
                             (monk--systemd-unit-path r)))))

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

(monk-define-resource monk-systemd-unit monk-systemd-unit-resource
  "Ensure systemd unit NAME: its file, its enablement, and its active state."
  ((name :type string :doc "Unit name with suffix, for example foo.service."))
  ((content :type string :doc "Unit file content.  Nil leaves the file alone.")
   (enabled :type boolean :default t)
   (started :type boolean :default t)
   (user-unit :type boolean :doc "A user unit, managed with systemctl --user.")))

;;;; launchd

(defclass monk-launchd-resource (monk-resource)
  ((plist :initarg :plist
          :initform nil
          :documentation "The job as an alist; see `monk-launchd-plist'.  Nil: do not manage the file.")
   (loaded :initarg :loaded
           :initform t
           :documentation "Whether the job must be loaded in launchd.")
   (daemon :initarg :daemon
           :initform nil
           :documentation "A system daemon under /Library/LaunchDaemons, else a user agent."))
  "A launchd job NAME (its label): its plist file and its loaded state.
The macOS counterpart of `monk-systemd-unit-resource'.")

(cl-defmethod monk-resource-available-p ((_r monk-launchd-resource) (m monk-machine))
  (monk-program-available-p m "launchctl"))

(defun monk-launchd-plist (label alist)
  "Return the XML property list for job LABEL from ALIST.
ALIST keys are launchd keys as symbols or strings; values are strings,
numbers, t, nil (false), lists of strings (arrays), or alists (dicts)."
  (cl-labels ((value (v)
                (cond
                 ((eq v t) "<true/>")
                 ((null v) "<false/>")
                 ((numberp v) (format "<integer>%s</integer>" v))
                 ((stringp v) (format "<string>%s</string>" (monk-launchd--escape v)))
                 ((and (consp v) (consp (car v)))
                  (concat "<dict>" (mapconcat (lambda (c) (concat (key (car c)) (value (cdr c)))) v "") "</dict>"))
                 ((listp v) (concat "<array>" (mapconcat #'value v "") "</array>"))
                 (t (format "<string>%s</string>" (monk-launchd--escape (format "%s" v))))))
              (key (k) (format "<key>%s</key>" (monk-launchd--escape (format "%s" k)))))
    (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
            "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
            "<plist version=\"1.0\">\n<dict>\n"
            (key "Label") (value label) "\n"
            (mapconcat (lambda (c) (concat (key (car c)) (value (cdr c)) "\n")) alist "")
            "</dict>\n</plist>\n")))

(defun monk-launchd--escape (text)
  "Return TEXT with XML special characters escaped."
  (replace-regexp-in-string
   "[&<>]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;"))) text t t))

(cl-defmethod monk--launchd-path ((r monk-launchd-resource))
  "Return the plist file path for R."
  (concat (if (oref r daemon) monk-launchd-daemon-directory monk-launchd-agent-directory)
          (oref r name) ".plist"))

(cl-defmethod monk--launchd-domain ((r monk-launchd-resource) (m monk-machine))
  "Return the launchctl domain target for R on M: system or gui/UID."
  (if (oref r daemon) "system"
    (format "gui/%s" (monk-with-run-cache (list 'uid (oref m name))
                       (monk-exec-string m "id" "-u")))))

(cl-defmethod monk--launchd-loaded-p ((r monk-launchd-resource) (m monk-machine))
  (monk-exec-ok-p (monk-exec m "launchctl" "print"
                             (format "%s/%s" (monk--launchd-domain r m) (oref r name))
                             :sudo (oref r daemon))))

(cl-defmethod monk-check ((r monk-launchd-resource) (m monk-machine))
  (let ((changes nil)
        (sudo (oref r daemon)))
    (when (oref r plist)
      (unless (equal (monk-launchd-plist (oref r name) (oref r plist))
                     (monk-file-read m (monk--launchd-path r) sudo))
        (setq changes (plist-put changes :content t))))
    (let ((loaded (monk--launchd-loaded-p r m)))
      (unless (eq (and loaded t) (and (oref r loaded) t))
        (setq changes (plist-put changes :loaded (list loaded (oref r loaded))))))
    changes))

(cl-defmethod monk-diff ((r monk-launchd-resource) (m monk-machine) changes)
  (when (plist-get changes :content)
    (list (monk-diff-content (monk-file-read m (monk--launchd-path r) (oref r daemon))
                             (monk-launchd-plist (oref r name) (oref r plist))
                             (monk--launchd-path r)))))

(cl-defmethod monk-repair ((r monk-launchd-resource) (m monk-machine) changes)
  (let* ((sudo (oref r daemon))
         (domain (monk--launchd-domain r m))
         (path (monk-machine-local-path (monk-machine-path m (monk--launchd-path r) sudo)))
         (notes nil))
    (when (plist-get changes :content)
      ;; launchd reads the file at load: write, then reload below.
      (monk-file-write m (monk--launchd-path r) (monk-launchd-plist (oref r name) (oref r plist)) sudo)
      (push "wrote plist" notes)
      (when (monk--launchd-loaded-p r m)
        (monk-exec m "launchctl" "bootout" (format "%s/%s" domain (oref r name)) :sudo sudo)
        (setf (plist-get changes :loaded) (list nil (oref r loaded)))))
    (when (plist-get changes :loaded)
      (if (oref r loaded)
          (progn (monk-exec-string m "launchctl" "bootstrap" domain path :sudo sudo)
                 (push "loaded" notes))
        (monk-exec-string m "launchctl" "bootout" (format "%s/%s" domain (oref r name)) :sudo sudo)
        (push "unloaded" notes)))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((r monk-launchd-resource))
  (list (monk--launchd-path r)))

(monk-define-resource monk-launchd monk-launchd-resource
  "Ensure launchd job LABEL: its plist file and its loaded state."
  ((label :type string :doc "The job label, for example org.monk.agent."))
  ((plist :type alist :doc "The job as an alist; see `monk-launchd-plist'.  Nil: file not managed.")
   (loaded :type boolean :default t)
   (daemon :type boolean :doc "A system daemon under /Library/LaunchDaemons, else a user agent.")))

;;;; service: generic over the machine's service manager

(defun monk-service (name &rest args)
  "Ensure service NAME through the machine's service manager.
Dispatches on the `service-mgr' fact: systemd gives `monk-systemd-unit'
\(NAME gets .service when it has no suffix), launchd gives
`monk-launchd' (NAME is the label).  ARGS: :enabled :started for
systemd, :loaded :plist for launchd, :user-unit or :daemon, and common
keys; keys the other manager does not take are dropped.  Ansible's
`service'."
  (let ((manager (monk-machine-fact (monk-current-machine) 'service-mgr))
        (rest (copy-sequence args)))
    (pcase manager
      ("systemd"
       (cl-remf rest :loaded) (cl-remf rest :plist) (cl-remf rest :daemon)
       (apply #'monk-systemd-unit
              (if (string-match-p "\\." name) name (concat name ".service"))
              rest))
      ("launchd"
       (cl-remf rest :enabled) (cl-remf rest :started) (cl-remf rest :user-unit) (cl-remf rest :content)
       (when (plist-member rest :started)
         (setq rest (plist-put rest :loaded (plist-get rest :started))))
       (apply #'monk-launchd name rest))
      (_ (apply #'monk-unsupported name
                (format "service: no supported service manager (service-mgr is %s)" manager)
                (monk--common-initargs args))))))

;;;; command

(defclass monk-command-resource (monk-resource)
  ((program :initarg :program :type string)
   (args :initarg :args :initform nil)
   (creates :initarg :creates
            :initform nil
            :documentation "Path.  When it exists, the command is kept.")
   (unless-command :initarg :unless-command
                   :initform nil
                   :documentation "List (PROGRAM . ARGS).  Zero exit means kept.")
   (verify :initarg :verify
           :initform nil
           :documentation "After the command ran: a list (PROGRAM . ARGS) that must exit
zero, or a function of the machine that returns nil when all is well
and a reason string otherwise.  Salt's check_cmd.  A non-zero exit or
a reason fails the resource, with the output as the comment.")
   (changed-when :initarg :changed-when
                 :initform nil
                 :documentation "Decides `repaired' against `kept' from the command's own output.
A function of the `monk-exec', or a CELL form over (rc), (stdout),
(stderr).  The command then runs on every pass, as a query, and its
result is what the predicate says.  Ansible's changed_when.")
   (failed-when :initarg :failed-when
                :initform nil
                :documentation "Decides failure instead of the exit code.  Same forms as
`changed-when'.  Ansible's failed_when."))
  "A command that runs unless CREATES exists or UNLESS-COMMAND succeeds.
Without either guard the command is always kept and runs only as a
watch action, so `:watch' makes it a handler in the Ansible sense.
With CHANGED-WHEN it runs every pass and reports what the predicate
decides.")

(defun monk-command-predicate (predicate exec)
  "Evaluate PREDICATE against EXEC, a `monk-exec'.  Return non-nil when it holds.
PREDICATE is a function of EXEC, or a CELL form where (rc) is the exit
code, (stdout) the output, and (stderr) the same output, because
Monk merges the streams."
  (cond
   ((functionp predicate) (funcall predicate exec))
   (t (let ((rc (monk-exec-exit-code exec))
            (out (string-trim (monk-exec-output exec))))
        (monk-cell-eval-with predicate
                             `((rc . ,rc) (stdout . ,out) (stderr . ,out)))))))

(cl-defmethod monk-check ((r monk-command-resource) (m monk-machine))
  (cond
   ;; A query command runs every pass; the predicate decides after.
   ((oref r changed-when)
    (list :run (cons (oref r program) (oref r args)) :query t))
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
  (let* ((exec (apply #'monk-exec m (oref r program)
                      (append (oref r args) (list :sudo (oref r sudo)))))
         (output (string-trim (monk-exec-output exec)))
         (failed (if (oref r failed-when)
                     (monk-command-predicate (oref r failed-when) exec)
                   (not (monk-exec-ok-p exec)))))
    (when failed
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (if (oref r failed-when)
                        (format "failed-when held: %s" (truncate-string-to-width output 200))
                      output))))
    (when (and (oref r changed-when)
               (not (monk-command-predicate (oref r changed-when) exec)))
      ;; Ran, nothing changed: tell the runner to record kept.
      (signal 'monk-command-unchanged
              (list (if (string-empty-p output) "ran, unchanged"
                      (truncate-string-to-width output 200)))))
    (if (string-empty-p output) "ran" (truncate-string-to-width output 200))))

(cl-defmethod monk-default-watch-action ((_r monk-command-resource))
  'run)

(cl-defmethod monk-action ((r monk-command-resource) (m monk-machine) action)
  (pcase action
    ('run (monk-repair r m nil))
    (_ (cl-call-next-method))))

(cl-defmethod monk-resource-paths ((r monk-command-resource))
  (and (oref r creates) (list (oref r creates))))

(cl-defmethod monk-resource-verify ((r monk-command-resource) (m monk-machine))
  "Run the :verify of R on M after a repair.  Return nil or a reason."
  (pcase (oref r verify)
    ('nil nil)
    ((and (pred functionp) fn) (funcall fn m))
    (`(,program . ,arguments)
     (let ((exec (apply #'monk-exec m program
                        (append arguments (list :sudo (oref r sudo))))))
       (unless (monk-exec-ok-p exec)
         (format "%s exited %d: %s" program (monk-exec-exit-code exec)
                 (string-trim (monk-exec-output exec))))))
    (other (format ":verify must be a function or (PROGRAM . ARGS), not %S" other))))

(monk-define-resource monk-command monk-command-resource
  "Ensure the effect of PROGRAM.
Without :creates, :unless, or :changed-when the command is always kept
and runs only as a watch action, so with :watch it is a handler."
  ((name :type string :doc "Identifies the resource.")
   (program :type string :doc "The program."))
  ((args :type list :doc "Arguments.")
   (creates :type path :doc "When this path exists, the command is kept.")
   (unless :type list :slot unless-command :doc "(PROGRAM . ARGS); zero exit means kept.")
   (verify :type any :doc "(PROGRAM . ARGS) that must exit zero after the command, or a function of the machine that returns nil or a reason.")
   (changed-when :type any :doc "Function of the exec, or CELL over (rc) (stdout) (stderr): the command runs every pass and is repaired only when this holds.")
   (failed-when :type any :doc "Function of the exec, or CELL over (rc) (stdout) (stderr): replaces the exit-code rule."))
  :exclusive ((changed-when creates) (changed-when unless)))

(provide 'monk-system)
;;; monk-system.el ends here
