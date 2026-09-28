;;; monk-sync.el --- rsync resource for Monk  -*- lexical-binding: t; -*-

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

;; `monk-rsync' keeps a destination in step with sources through rsync.
;; The check is `rsync --dry-run --itemize-changes': every line rsync
;; would act on is a change, and the degree is the share of files
;; that already match.  The repair is the same command without
;; `--dry-run'.  Emacs has no transfer primitive that does what rsync
;; does over ssh (delta transfer, --delete, one connection for a
;; tree), so this is a justified shell call.
;;
;;   (monk-rsync "dotfiles to work" '("~/.zshrc" "~/.zshenv") "work:"
;;               :follow-links t)
;;   (monk-rsync "tmux plugins" '("~/.tmux/plugins/") "work:.tmux/plugins/"
;;               :archive t :delete t)
;;
;; The destination is an rsync destination string: `host:path' or a
;; local path.  A trailing slash on a source means its contents, as
;; in rsync.  Sources with `~' expand on the machine that runs rsync.
;;
;; A destination host that does not answer makes rsync exit 255.
;; `:reachable HOST' turns that into a skip with the reason "HOST is
;; not reachable" instead of a failure: a laptop that is at home is
;; not a broken policy.  The probe is one TCP connect to the ssh port
;; through `make-network-process', cached per run.

;;; Code:

(require 'monk)

(defconst monk-rsync-program "rsync"
  "The rsync binary.")

(defconst monk-rsync-check-args '("--dry-run" "--itemize-changes" "--out-format=%i %n" "--stats")
  "Arguments that make rsync report what it would do, one line per item.
`--stats' adds the file count, which gives the degree.")

(defconst monk-rsync-stats-files-regexp "^Number of files: \\([0-9,]+\\)"
  "The `--stats' line that counts every file rsync looked at.")

(defconst monk-rsync-unreachable-exit 255
  "Exit code rsync gives when ssh could not connect.")

(defconst monk-rsync-item-regexp "\\`\\([<>ch.*][fdLDS][^ ]\\{7,9\\}\\|\\*deleting\\) +\\(.*\\)\\'"
  "One `--itemize-changes' line: the flags, then the path.")


(defcustom monk-rsync-reach-port 22
  "TCP port a `:reachable' probe connects to."
  :type 'natnum
  :group 'monk)

(defcustom monk-rsync-reach-timeout 2.0
  "Seconds a `:reachable' probe waits for the connect."
  :type 'number
  :group 'monk)

;;;; Reachability

(defun monk-host-reachable-p (host &optional port timeout)
  "Return non-nil when HOST accepts a TCP connect on PORT within TIMEOUT.
PORT defaults to `monk-rsync-reach-port', TIMEOUT to
`monk-rsync-reach-timeout'.  Uses `make-network-process', not ping:
ping needs raw sockets on some systems and a host can answer ping
while sshd is down.  Cached per run under (reachable HOST PORT)."
  (let* ((port (or port monk-rsync-reach-port))
         (timeout (or timeout monk-rsync-reach-timeout))
         (key (list 'reachable host port))
         (cached (monk-run-context-get key)))
    (if cached
        (eq cached 'yes)
      (let ((answer
             (condition-case nil
                 (let ((proc (make-network-process
                              :name (format "monk-reach-%s" host)
                              :host host :service port :nowait t :noquery t)))
                   (unwind-protect
                       (let ((deadline (+ (float-time) timeout)))
                         (while (and (eq (process-status proc) 'connect)
                                     (< (float-time) deadline))
                           (accept-process-output nil 0.05))
                         (eq (process-status proc) 'open))
                     (delete-process proc)))
               (error nil))))
        (monk-run-context-put key (if answer 'yes 'no))
        answer))))

;;;; Resource

(defclass monk-rsync-resource (monk-resource)
  ((sources :initarg :sources
            :type list
            :documentation "Source paths.  `~' expands on the running machine.")
   (destination :initarg :destination
                :type string
                :documentation "rsync destination: `host:path' or a local path.")
   (archive :initarg :archive
            :initform nil
            :documentation "Pass -a.")
   (recursive :initarg :recursive
              :initform nil
              :documentation "Pass -r.  Implied by :archive.")
   (follow-links :initarg :follow-links
                 :initform nil
                 :documentation "Pass -L: copy what links point to.")
   (delete :initarg :delete
           :initform nil
           :documentation "Pass --delete: remove destination files absent from the source.")
   (excludes :initarg :excludes
             :initform nil
             :documentation "Patterns for --exclude.")
   (options :initarg :options
            :initform nil
            :documentation "Extra rsync arguments, verbatim.")
   (reachable :initarg :reachable
              :initform nil
              :documentation "Host name to probe first.  Unreachable means skipped, not failed."))
  "A destination kept in step with sources through rsync.")

(cl-defmethod monk-resource-available-p ((r monk-rsync-resource) (m monk-machine))
  "Return nil when R can run on M: rsync exists and the :reachable host answers.
Both are availability, not policy: a mock run asks neither."
  (or (monk-program-available-p m monk-rsync-program)
      (and (oref r reachable)
           (not (monk-host-reachable-p (oref r reachable)))
           (format "%s is not reachable" (oref r reachable)))))

(defun monk--rsync-arguments (r m)
  "Return the rsync argument list for R on M, without the dry-run flags."
  (append (and (oref r archive) '("-a"))
          (and (oref r recursive) (not (oref r archive)) '("-r"))
          (and (oref r follow-links) '("-L"))
          (and (oref r delete) '("--delete"))
          (mapcar (lambda (p) (concat "--exclude=" p)) (oref r excludes))
          (oref r options)
          (mapcar (lambda (s) (monk-machine-expand m s)) (oref r sources))
          (list (monk-machine-expand m (oref r destination)))))

(defun monk-rsync-parse-itemize (output)
  "Return (CHANGED . TOTAL) from rsync OUTPUT with itemize and stats.
CHANGED is the list of \"FLAGS PATH\" lines for items rsync would
touch.  rsync itemizes changed items only, so TOTAL comes from the
`--stats' file count, for the degree; it is nil without stats.  A
pure attribute change on a directory (`.d..t......') is not counted:
rsync sets those times as a side effect of other changes."
  (let ((changed nil) (total nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-match monk-rsync-stats-files-regexp line)
        (setq total (string-to-number (string-replace "," "" (match-string 1 line)))))
       ((and (string-match monk-rsync-item-regexp line)
             (not (string-prefix-p ".d" (match-string 1 line))))
        (push line changed))))
    (cons (nreverse changed) total)))

(defun monk--rsync-run (r m dry-run)
  "Run rsync for R on M.  With DRY-RUN, itemize only.  Return the `monk-exec'."
  (apply #'monk-exec m monk-rsync-program
         (append (and dry-run monk-rsync-check-args)
                 (monk--rsync-arguments r m)
                 (list :sudo (oref r sudo)))))

(cl-defmethod monk-check ((r monk-rsync-resource) (m monk-machine))
  (let ((exec (monk--rsync-run r m t)))
    (cond
     ((= (monk-exec-exit-code exec) monk-rsync-unreachable-exit)
      (signal 'monk-resource-error
              (list (format "rsync could not reach %s: %s" (oref r destination)
                            (string-trim (monk-exec-output exec))))))
     ((not (monk-exec-ok-p exec))
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
     (t
      (pcase-let ((`(,changed . ,total) (monk-rsync-parse-itemize (monk-exec-output exec))))
        (when changed
          (list :sync changed
                :degree (if (and total (> total 0))
                            (max 0.0 (/ (float (- total (length changed))) total))
                          0.0))))))))

(cl-defmethod monk-repair ((r monk-rsync-resource) (m monk-machine) changes)
  (let ((exec (monk--rsync-run r m nil)))
    (unless (monk-exec-ok-p exec)
      (signal 'monk-exec-error
              (list (oref m name) (monk-exec-command exec) (monk-exec-exit-code exec)
                    (string-trim (monk-exec-output exec)))))
    (format "synced %d item%s to %s" (length (plist-get changes :sync))
            (if (= 1 (length (plist-get changes :sync))) "" "s")
            (oref r destination))))

(cl-defmethod monk-resource-paths ((r monk-rsync-resource))
  "The sources of R: a change under one of them is drift for the sync."
  (oref r sources))

(cl-defmethod monk-resource-owns-subtree-p ((_r monk-rsync-resource))
  t)

(defun monk-rsync (name sources destination &rest args)
  "Ensure DESTINATION mirrors SOURCES through rsync.  NAME identifies the sync.
SOURCES is a list of paths.  ARGS: :archive :recursive :follow-links
:delete :excludes :options :reachable HOST, and common keys."
  (monk-add-resource
   (apply #'monk-rsync-resource
          :name name
          (monk--resource-plist args
                                :sources sources
                                :destination destination
                                :archive (plist-get args :archive)
                                :recursive (plist-get args :recursive)
                                :follow-links (plist-get args :follow-links)
                                :delete (plist-get args :delete)
                                :excludes (plist-get args :excludes)
                                :options (plist-get args :options)
                                :reachable (plist-get args :reachable)))))

(provide 'monk-sync)
;;; monk-sync.el ends here
