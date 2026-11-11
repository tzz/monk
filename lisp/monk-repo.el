;;; monk-repo.el --- Deploy files from a repository  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, vc, files

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

;; Resources: `monk-repo-file' and `monk-repo-tree'.  A file, or a
;; directory of files, on the target machine mirrors a path inside a
;; repository at a revision.  The content comes from one of three
;; sources, chosen by the resource's arguments:
;;
;;   :checkout DIR   a working tree on the controller (a `monk-git'
;;                   checkout or any directory).  Edits that are not
;;                   committed count: what is on disk is deployed.
;;   :repo URL       no checkout.  Monk keeps a bare mirror of URL
;;                   under MONK_HOME/repos/, fetches it once per run,
;;                   and reads blobs with `git cat-file' through
;;                   vc-git.  Works offline after the first fetch.
;;   :repo URL on a known host (`monk-repo-api-hosts': github.com,
;;                   gitlab.com, codeberg.org, bitbucket.org) with
;;                   `monk-repo-prefer-api' set: the host's HTTP API
;;                   gives the blob and the tree, nothing is stored.
;;                   A token from auth-source applies when present.
;;
;; The content is compared and written by the `monk-file' machinery,
;; so the diff, the mode, and the ownership follow the same rules; the
;; mode defaults to what git recorded (100755 is executable).  A tree
;; resource deploys every regular file under the path and, with
;; :delete, removes files under DEST that the tree does not hold.

;;; Code:

(require 'monk)
(require 'monk-file)
(require 'monk-system)
(require 'monk-package)
(require 'vc-git)
(require 'url)
(require 'url-http)
(require 'json)
(require 'auth-source)

(defvar url-http-response-status)

(defgroup monk-repo nil
  "Files deployed from a repository."
  :group 'monk)

(defconst monk-repo-default-revision "HEAD"
  "The revision deployed when the policy names none.")

(defconst monk-repo-mirrors-directory-name "repos"
  "Directory under `monk-home-directory' that holds the bare mirrors.")

(defconst monk-repo-executable-mode #o755
  "Mode of a deployed file that git marks executable (100755).")

(defconst monk-repo-git-mode-executable "100755"
  "The git tree mode of an executable blob.")

(defconst monk-repo-git-mode-symlink "120000"
  "The git tree mode of a symbolic link.  Links are not deployed.")

(defconst monk-repo-git-type-blob "blob"
  "The git object type of a file.")

(defcustom monk-repo-prefer-api t
  "When non-nil, read from a hosting API instead of a mirror where one is known.
See `monk-repo-api-hosts'.  Off, every `:repo' goes through a bare
mirror under MONK_HOME, whatever the host."
  :type 'boolean)

(defcustom monk-repo-api-hosts
  '(("github.com" . github) ("gitlab.com" . gitlab)
    ("codeberg.org" . gitea) ("bitbucket.org" . bitbucket))
  "Hosts whose HTTP API Monk reads repositories through, and their flavor.
Each entry is (HOST . FLAVOR); FLAVOR is one of `github', `gitlab',
`gitea' (Codeberg, Forgejo, Gitea), `bitbucket'.  Add a private
GitLab or Gitea by host name; the token comes from auth-source for
that host."
  :type '(alist :key-type string :value-type symbol))

(defcustom monk-repo-api-timeout 30
  "Seconds to wait for one API reply."
  :type 'natnum)

(defcustom monk-repo-fetch t
  "When non-nil, fetch a mirror once per run before reading from it.
Off, the mirror is read as it is; a revision not yet mirrored is
fetched on demand."
  :type 'boolean)

(define-error 'monk-repo-error "Monk repository error" 'monk-resource-error)

;;;; Repository URLs

(defun monk-repo--parse-url (url)
  "Return (HOST OWNER NAME) for repository URL, or nil when it has no host.
Understands https://HOST/OWNER/NAME[.git], git@HOST:OWNER/NAME[.git],
and ssh://git@HOST/OWNER/NAME[.git].  Local paths and file:// URLs
have no host."
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\`\\(?:https?\\|ssh\\|git\\)://\\(?:[^@/]+@\\)?\\([^/:]+\\)\\(?::[0-9]+\\)?/\\(.+?\\)\\(?:\\.git\\)?/?\\'" url)
      (let ((host (match-string 1 url)) (path (match-string 2 url)))
        (and (string-match "\\`\\(.+\\)/\\([^/]+\\)\\'" path)
             (list host (match-string 1 path) (match-string 2 path)))))
     ;; scp-like: HOST:PATH, where PATH does not start with a slash (that
     ;; would be a URL scheme such as file:///, which has no host)
     ((string-match "\\`\\(?:[^@/]+@\\)?\\([^/:]+\\):\\([^/].*?\\)\\(?:\\.git\\)?/?\\'" url)
      (let ((host (match-string 1 url)) (path (match-string 2 url)))
        (and (string-match "\\`\\(.+\\)/\\([^/]+\\)\\'" path)
             (list host (match-string 1 path) (match-string 2 path)))))
     (t nil))))

(defun monk-repo--api-flavor (url)
  "Return the API flavor for URL, or nil when it goes through a mirror."
  (and monk-repo-prefer-api
       (let ((parsed (monk-repo--parse-url url)))
         (and parsed (cdr (assoc (car parsed) monk-repo-api-hosts))))))

;;;; Sources: one protocol, three implementations

(defun monk-repo--tree-prefix (path)
  "Return PATH as a directory prefix for entries under it; \"\" for the root."
  (if (string-empty-p path) "" (file-name-as-directory path)))

(defun monk-repo--join (path relative)
  "Return RELATIVE under PATH inside a repository; PATH may be the root."
  (concat (monk-repo--tree-prefix path) relative))

(cl-defgeneric monk-repo-source-blob (source path revision)
  "Return (CONTENT . EXECUTABLE-P) for PATH at REVISION in SOURCE, or nil.
CONTENT is a unibyte string; nil means PATH is not a file there.")

(cl-defgeneric monk-repo-source-files (source path revision)
  "Return the regular files under PATH at REVISION in SOURCE.
Each is (RELATIVE-PATH . EXECUTABLE-P), relative to PATH, sorted.
Symbolic links and submodules are left out.")

(cl-defgeneric monk-repo-source-describe (source revision)
  "Return a short string that names SOURCE at REVISION for reports.")

;; A working tree on the controller.

(cl-defstruct (monk-repo-checkout (:constructor monk-repo-checkout-create))
  "A source read from a working tree."
  directory)

(cl-defmethod monk-repo-source-blob ((source monk-repo-checkout) path _revision)
  (let ((file (expand-file-name path (monk-repo-checkout-directory source))))
    (when (and (file-regular-p file) (not (file-symlink-p file)))
      (cons (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (buffer-string))
            (file-executable-p file)))))

(cl-defmethod monk-repo-source-files ((source monk-repo-checkout) path _revision)
  (let* ((root (file-name-as-directory
                (expand-file-name path (monk-repo-checkout-directory source))))
         (files nil))
    (unless (file-directory-p root)
      (signal 'monk-repo-error (list (format "%s is not a directory in the checkout" path))))
    (dolist (file (directory-files-recursively root "" nil
                                               (lambda (dir)
                                                 (not (member (file-name-nondirectory dir)
                                                              vc-directory-exclusion-list)))))
      (when (and (file-regular-p file) (not (file-symlink-p file)))
        (push (cons (file-relative-name file root) (file-executable-p file)) files)))
    (sort files (lambda (a b) (string< (car a) (car b))))))

(cl-defmethod monk-repo-source-describe ((source monk-repo-checkout) _revision)
  (format "checkout %s" (abbreviate-file-name (monk-repo-checkout-directory source))))

;; A bare mirror under MONK_HOME, read through vc-git.

(cl-defstruct (monk-repo-mirror (:constructor monk-repo-mirror-create))
  "A source read from a bare mirror of a remote."
  url directory)

(defun monk-repo-mirror-path (url)
  "Return the directory of the bare mirror of URL under MONK_HOME.
One mirror per URL, named by a hash of the URL and its last path
component, so two forks of one project do not collide and the
directory still says what it holds."
  (let* ((parsed (monk-repo--parse-url url))
         (stem (or (nth 2 parsed) (file-name-base (directory-file-name url)))))
    (expand-file-name (format "%s-%s.git" (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" stem)
                              (substring (secure-hash 'sha256 url) 0 12))
                      (expand-file-name monk-repo-mirrors-directory-name (monk-home-directory)))))

(defun monk-repo--git-ok (directory &rest args)
  "Run git ARGS in DIRECTORY through vc-git.  Return the output or nil."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (and (eql 0 (apply #'vc-git--call nil '(t nil) (car args) (cdr args)))
           (buffer-string)))))

(defun monk-repo--git-binary (directory &rest args)
  "Run git ARGS in DIRECTORY through vc-git with binary coding.
Return the output as a unibyte string, or nil on a non-zero exit."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (let ((out (apply #'monk-repo--git-ok directory args)))
      (and out (if (multibyte-string-p out) (encode-coding-string out 'binary) out)))))

(defun monk-repo-mirror-ensure (url)
  "Return the mirror directory of URL: cloned when absent, fetched once a run.
The clone is `--mirror' (every ref, no working tree).  With
`monk-repo-fetch' off, an existing mirror is left as it is."
  (let ((directory (monk-repo-mirror-path url)))
    (monk-with-run-cache (list 'repo-mirror url)
      (cond
       ((not (file-directory-p directory))
        (make-directory (file-name-directory (directory-file-name directory)) t)
        (let ((default-directory (file-name-directory (directory-file-name directory))))
          (unless (vc-git--out-ok "clone" "--quiet" "--mirror" url
                                  (monk-machine-local-path directory))
            (signal 'monk-repo-error (list (format "clone of %s failed" url))))))
       (monk-repo-fetch
        (unless (monk-repo--git-ok directory "fetch" "--quiet" "--prune" "origin")
          ;; Offline: say so once, read what is mirrored.
          (monk-log 'warn "repo %s: fetch failed, reading the mirror as it is" url))))
      t)
    directory))

(defun monk-repo-mirror-resolve (directory revision)
  "Return the full commit id of REVISION in the mirror at DIRECTORY.
Signal `monk-repo-error' when the mirror does not know it."
  (let ((out (monk-repo--git-ok directory "rev-parse" "--verify" "--quiet"
                                (format "%s^{commit}" revision))))
    (unless out
      (signal 'monk-repo-error
              (list (format "revision %s is not in %s" revision (abbreviate-file-name directory)))))
    (string-trim out)))

(defun monk-repo--parse-ls-tree (output)
  "Return (PATH MODE TYPE) entries from `ls-tree -z' OUTPUT."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([0-7]+\\) \\([a-z]+\\) [0-9a-f]+\t\\(.*\\)\\'" line)
                    (list (match-string 3 line) (match-string 1 line) (match-string 2 line))))
                (split-string output "\0" t))))

(cl-defmethod monk-repo-source-blob ((source monk-repo-mirror) path revision)
  (let* ((directory (monk-repo-mirror-directory source))
         (commit (monk-repo-mirror-resolve directory revision))
         (entry (car (monk-repo--parse-ls-tree
                      (or (monk-repo--git-ok directory "ls-tree" "-z" "--full-tree" commit "--" path)
                          "")))))
    (when (and entry (equal (nth 2 entry) monk-repo-git-type-blob)
               (not (equal (nth 1 entry) monk-repo-git-mode-symlink)))
      (let ((content (monk-repo--git-binary directory "cat-file" "--filters"
                                            (format "%s:%s" commit path))))
        (unless content
          (signal 'monk-repo-error (list (format "cannot read %s at %s" path revision))))
        (cons content (equal (nth 1 entry) monk-repo-git-mode-executable))))))

(cl-defmethod monk-repo-source-files ((source monk-repo-mirror) path revision)
  (let* ((directory (monk-repo-mirror-directory source))
         (commit (monk-repo-mirror-resolve directory revision))
         (prefix (monk-repo--tree-prefix path))
         (output (apply #'monk-repo--git-ok directory "ls-tree" "-r" "-z" "--full-tree" commit
                        (and (not (string-empty-p path)) (list "--" path)))))
    (unless (and output (not (string-empty-p output)))
      (signal 'monk-repo-error (list (format "%s is not in %s at %s" path
                                             (monk-repo-mirror-url source) revision))))
    (let (files)
      (dolist (entry (monk-repo--parse-ls-tree output))
        (when (and (equal (nth 2 entry) monk-repo-git-type-blob)
                   (not (equal (nth 1 entry) monk-repo-git-mode-symlink))
                   (string-prefix-p prefix (car entry)))
          (push (cons (substring (car entry) (length prefix))
                      (equal (nth 1 entry) monk-repo-git-mode-executable))
                files)))
      (sort files (lambda (a b) (string< (car a) (car b)))))))

(cl-defmethod monk-repo-source-describe ((source monk-repo-mirror) revision)
  (format "%s at %s" (monk-repo-mirror-url source) revision))

;; A hosting API: GitHub, GitLab, Gitea/Forgejo/Codeberg, Bitbucket.

(cl-defstruct (monk-repo-api (:constructor monk-repo-api-create))
  "A source read through a hosting HTTP API."
  url flavor host owner name)

(defun monk-repo-api-token (host)
  "Return the auth-source secret for HOST, or nil.
Looked up as (:host HOST :user \"token\") first, then any user on HOST."
  (let ((auth-source-do-cache nil))
    (let ((found (or (car (auth-source-search :host host :user "token" :max 1))
                     (car (auth-source-search :host host :max 1)))))
      (when found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret))))))

(defun monk-repo--url-encode-path (path)
  "Return PATH with each segment URL encoded, slashes kept."
  (mapconcat #'url-hexify-string (split-string path "/") "/"))

(defun monk-repo-api--request (api route &optional raw)
  "GET ROUTE on API.  Return the parsed JSON body, or the raw body with RAW.
Return nil on 404.  Signal `monk-repo-error' on other failures."
  (let* ((token (monk-repo-api-token (monk-repo-api-host api)))
         (url-request-method "GET")
         (url-request-extra-headers
          (append (and token
                       (list (cons "Authorization"
                                   (pcase (monk-repo-api-flavor api)
                                     ('github (concat "Bearer " token))
                                     ('gitlab (concat "Bearer " token))
                                     ('gitea (concat "token " token))
                                     ('bitbucket (concat "Bearer " token))))))
                  (and raw (eq (monk-repo-api-flavor api) 'github)
                       '(("Accept" . "application/vnd.github.raw+json")))
                  (and (not raw) '(("Accept" . "application/json")))))
         (url-show-status nil)
         (buffer (url-retrieve-synchronously route t t monk-repo-api-timeout)))
    (unless buffer
      (signal 'monk-repo-error (list (format "no reply from %s" route))))
    (with-current-buffer buffer
      (unwind-protect
          (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status)
                            (save-excursion
                              (goto-char (point-min))
                              (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                                   (string-to-number (match-string 1)))))))
            (goto-char (point-min))
            (re-search-forward "^\r?$" nil t)
            (forward-line 1)
            (cond
             ((eql status 404) nil)
             ((and status (< status 300))
              (if raw
                  (let ((body (buffer-substring-no-properties (point) (point-max))))
                    (if (multibyte-string-p body) (encode-coding-string body 'binary) body))
                (json-parse-buffer :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil)))
             (t (signal 'monk-repo-error
                        (list (format "%s: HTTP %s%s" route status
                                      (if token "" " (no token in auth-source for this host)")))))))
        (kill-buffer buffer)))))

(defun monk-repo-api--base (api)
  "Return the API base URL for API's host and flavor."
  (let ((host (monk-repo-api-host api)))
    (pcase (monk-repo-api-flavor api)
      ('github (if (equal host "github.com") "https://api.github.com" (format "https://%s/api/v3" host)))
      ('gitlab (format "https://%s/api/v4" host))
      ('gitea (format "https://%s/api/v1" host))
      ('bitbucket (if (equal host "bitbucket.org") "https://api.bitbucket.org/2.0"
                    (format "https://%s/rest/api/1.0" host))))))

(defun monk-repo-api--project (api)
  "Return the owner/name part of API as each flavor wants it in a route."
  (pcase (monk-repo-api-flavor api)
    ('gitlab (url-hexify-string (format "%s/%s" (monk-repo-api-owner api) (monk-repo-api-name api))))
    (_ (format "%s/%s" (monk-repo-api-owner api) (monk-repo-api-name api)))))

(defun monk-repo--json-object-p (data)
  "Return non-nil when DATA, parsed JSON, is one object and not a list of them."
  (and (consp data) (consp (car data)) (symbolp (caar data))))

(defun monk-repo--json-list-p (data)
  "Return non-nil when DATA, parsed JSON, is a list of objects (or empty)."
  (or (null data)
      (and (consp data) (or (null (car data)) (monk-repo--json-object-p (car data))))))

(defun monk-repo-api--entries (api path revision)
  "Return (PATH TYPE EXECUTABLE-P) per entry at PATH, REVISION through API.
Nil on 404.  TYPE is `file', `dir', or `other'.  PATH is relative to
the repository."
  (let* ((base (monk-repo-api--base api))
         (project (monk-repo-api--project api))
         (encoded (monk-repo--url-encode-path path))
         (ref (url-hexify-string revision)))
    (pcase (monk-repo-api-flavor api)
      ('github
       (monk-repo-api--contents-entries
        (monk-repo-api--request
         api (format "%s/repos/%s/contents/%s?ref=%s" base project encoded ref))))
      ('gitlab
       ;; the tree endpoint lists a directory; a file answers with the
       ;; file endpoint, whose JSON has the mode
       (let ((data (monk-repo-api--request
                    api (format "%s/projects/%s/repository/tree?path=%s&ref=%s&per_page=100"
                                base project encoded ref))))
         ;; the tree endpoint answers [] for a file path as for a
         ;; missing one: ask the file endpoint before giving up
         (if (and data (monk-repo--json-list-p data))
             (mapcar (lambda (e) (list (alist-get 'path e)
                                       (pcase (alist-get 'type e) ("blob" 'file) ("tree" 'dir) (_ 'other))
                                       (equal (alist-get 'mode e) monk-repo-git-mode-executable)))
                     data)
           ;; the files endpoint wants the whole path as one encoded segment
           (let ((file (monk-repo-api--request
                        api (format "%s/projects/%s/repository/files/%s?ref=%s" base project
                                    (url-hexify-string path) ref))))
             (and file (list (list path 'file (equal (alist-get 'execute_filemode file) t))))))))
      ('gitea
       (monk-repo-api--contents-entries
        (monk-repo-api--request
         api (format "%s/repos/%s/contents/%s?ref=%s" base project encoded ref))))
      ('bitbucket
       (let ((data (monk-repo-api--request
                    api (format "%s/repositories/%s/src/%s/%s?format=meta&pagelen=100"
                                base project ref encoded))))
         (cond
          ((null data) nil)
          ((alist-get 'values data)
           (mapcar #'monk-repo-api--bitbucket-entry (alist-get 'values data)))
          (t (list (monk-repo-api--bitbucket-entry data)))))))))

(defun monk-repo-api--contents-entries (data)
  "Return entries from DATA, a GitHub or Gitea contents reply.
DATA is one object (a file) or a list of them (a directory)."
  (cl-flet ((entry (e) (list (alist-get 'path e)
                             (pcase (alist-get 'type e) ("file" 'file) ("dir" 'dir) (_ 'other))
                             nil)))
    (cond
     ((null data) nil)
     ((monk-repo--json-object-p data) (list (entry data)))
     (t (mapcar #'entry data)))))

(defun monk-repo-api--bitbucket-entry (e)
  "Return the entry for E, one Bitbucket src object."
  (list (alist-get 'path e)
        (pcase (alist-get 'type e) ("commit_file" 'file) ("commit_directory" 'dir) (_ 'other))
        (and (seq-some (lambda (a) (equal a "executable")) (alist-get 'attributes e)) t)))

(defun monk-repo-api--raw (api path revision)
  "Return the bytes of file PATH at REVISION through API, or nil on 404."
  (let* ((base (monk-repo-api--base api))
         (project (monk-repo-api--project api))
         (encoded (monk-repo--url-encode-path path))
         (ref (url-hexify-string revision)))
    (pcase (monk-repo-api-flavor api)
      ('github (monk-repo-api--request
                api (format "%s/repos/%s/contents/%s?ref=%s" base project encoded ref) t))
      ('gitlab (monk-repo-api--request
                api (format "%s/projects/%s/repository/files/%s/raw?ref=%s" base project
                            (url-hexify-string path) ref)
                t))
      ('gitea (monk-repo-api--request
               api (format "%s/repos/%s/raw/%s?ref=%s" base project encoded ref) t))
      ('bitbucket (monk-repo-api--request
                   api (format "%s/repositories/%s/src/%s/%s" base project ref encoded) t)))))

(cl-defmethod monk-repo-source-blob ((source monk-repo-api) path revision)
  (let ((entries (monk-repo-api--entries source path revision)))
    (when (and entries (null (cdr entries)) (eq (nth 1 (car entries)) 'file))
      (let ((content (monk-repo-api--raw source path revision)))
        (unless content
          (signal 'monk-repo-error (list (format "cannot read %s at %s" path revision))))
        (cons content (nth 2 (car entries)))))))

(cl-defmethod monk-repo-source-files ((source monk-repo-api) path revision)
  (let ((prefix (monk-repo--tree-prefix path))
        (files nil)
        (pending (list path)))
    (unless (monk-repo-api--entries source path revision)
      (signal 'monk-repo-error (list (format "%s is not in %s at %s" path
                                             (monk-repo-api-url source) revision))))
    (while pending
      (let ((dir (pop pending)))
        (dolist (entry (monk-repo-api--entries source dir revision))
          (pcase (nth 1 entry)
            ('file (push (cons (substring (car entry) (length prefix)) (nth 2 entry)) files))
            ('dir (push (car entry) pending))
            (_ nil)))))
    (sort files (lambda (a b) (string< (car a) (car b))))))

(cl-defmethod monk-repo-source-describe ((source monk-repo-api) revision)
  (format "%s at %s (%s API)" (monk-repo-api-url source) revision (monk-repo-api-flavor source)))

;;;; Choosing the source

(defun monk-repo-source (checkout repo)
  "Return the source for CHECKOUT or REPO, one of which is non-nil.
A CHECKOUT is read from disk.  A REPO on an API host goes through the
API (see `monk-repo-prefer-api'); any other REPO through a mirror
under MONK_HOME."
  (cond
   (checkout (monk-repo-checkout-create :directory (expand-file-name checkout)))
   ((monk-repo--api-flavor repo)
    (let ((parsed (monk-repo--parse-url repo)))
      (monk-repo-api-create :url repo :flavor (monk-repo--api-flavor repo)
                            :host (nth 0 parsed) :owner (nth 1 parsed) :name (nth 2 parsed))))
   (repo (monk-repo-mirror-create :url repo :directory (monk-repo-mirror-ensure repo)))
   (t (signal 'monk-repo-error (list "neither :checkout nor :repo given")))))

;;;; The file resource

(defclass monk-repo-file-resource (monk-file-resource)
  ((repo :initarg :repo :initform nil
         :documentation "Repository URL, read through a mirror or an API.")
   (checkout :initarg :checkout :initform nil
             :documentation "Working tree on the controller, read as it is.")
   (path :initarg :path :initform nil
         :documentation "Path inside the repository.  Nil: the destination's base name.")
   (revision :initarg :revision :initform nil
             :documentation "Commit, branch, or tag.  Nil: `monk-repo-default-revision'.")
   (repo-source :initform nil
                :documentation "The source object, made once per instance."))
  "A file whose content is a path in a repository at a revision.")

(cl-defmethod monk-repo--source-of ((r monk-repo-file-resource))
  "Return the source of R, built on first use."
  (or (oref r repo-source)
      (oset r repo-source (monk-repo-source (oref r checkout) (oref r repo)))))

(cl-defmethod monk-repo--revision ((r monk-repo-file-resource))
  (or (oref r revision) monk-repo-default-revision))

(cl-defmethod monk-repo--path ((r monk-repo-file-resource))
  (or (oref r path) (file-name-nondirectory (oref r name))))

(cl-defmethod monk-repo--fetch-blob ((r monk-repo-file-resource))
  "Return (CONTENT . EXECUTABLE-P) for R, cached for the run.
Signal `monk-repo-error' when the path is not a file there."
  (monk-with-run-cache (list 'repo-blob (oref r repo) (oref r checkout)
                             (monk-repo--revision r) (monk-repo--path r))
    (or (monk-repo-source-blob (monk-repo--source-of r) (monk-repo--path r) (monk-repo--revision r))
        (signal 'monk-repo-error
                (list (format "%s is not a file in %s" (monk-repo--path r)
                              (monk-repo-source-describe (monk-repo--source-of r)
                                                         (monk-repo--revision r))))))))

(cl-defmethod monk--file-desired-content ((r monk-repo-file-resource))
  "Return R's blob decoded as UTF-8, as `monk-file' diffs and writes it."
  (decode-coding-string (car (monk-repo--fetch-blob r)) 'utf-8-unix))

(cl-defmethod monk-check ((r monk-repo-file-resource) (_m monk-machine))
  "Check R as a file, with the mode from git when the policy gives none.
An executable blob lands executable.  Set once, before the file check
reads the slot."
  (when (and (null (oref r mode)) (cdr (monk-repo--fetch-blob r)))
    (oset r mode monk-repo-executable-mode))
  (monk-check-note :source (monk-repo-source-describe (monk-repo--source-of r)
                                                      (monk-repo--revision r)))
  (cl-call-next-method))

(monk-define-resource monk-repo-file monk-repo-file-resource
  "Ensure PATH holds a file from a repository at a revision."
  ((path :type path :doc "The destination file."))
  ((repo :type string :doc "Repository URL; a mirror under MONK_HOME, or a hosting API.")
   (checkout :type path :doc "Working tree on the controller instead of a URL.")
   (path :type string :slot path
         :doc "Path inside the repository.  Default: the destination's base name.")
   (revision :type string :aliases (rev) :doc "Commit, branch, or tag.  Default HEAD.")
   (mode :type mode :doc "Octal mode.  Default: 755 for a blob git marks executable, else as `monk-file'.")
   (owner :type string :doc "User name.")
   (group :type string :doc "Group name."))
  :one-of (repo checkout)
  :exclusive ((repo checkout)))

;;;; The tree resource

(defclass monk-repo-tree-resource (monk-resource)
  ((repo :initarg :repo :initform nil)
   (checkout :initarg :checkout :initform nil)
   (path :initarg :path :initform ""
         :documentation "Directory inside the repository.  Empty: the root.")
   (revision :initarg :revision :initform nil)
   (delete :initarg :delete :initform nil
           :documentation "When non-nil, remove files under the destination the tree lacks.")
   (mode :initarg :mode :initform nil
         :documentation "Mode for every deployed file; nil follows git.")
   (owner :initarg :owner :initform nil)
   (group :initarg :group :initform nil)
   (repo-source :initform nil))
  "A directory whose regular files mirror a directory of a repository.")

(cl-defmethod monk-repo--source-of ((r monk-repo-tree-resource))
  (or (oref r repo-source)
      (oset r repo-source (monk-repo-source (oref r checkout) (oref r repo)))))

(cl-defmethod monk-repo--revision ((r monk-repo-tree-resource))
  (or (oref r revision) monk-repo-default-revision))

(cl-defmethod monk-repo--tree-files ((r monk-repo-tree-resource))
  "Return the (RELATIVE . EXECUTABLE-P) files of R, once per run."
  (monk-with-run-cache (list 'repo-tree (oref r repo) (oref r checkout)
                             (monk-repo--revision r) (oref r path))
    (monk-repo-source-files (monk-repo--source-of r) (oref r path) (monk-repo--revision r))))

(cl-defmethod monk-repo--tree-blob ((r monk-repo-tree-resource) relative)
  "Return the content of RELATIVE in R's tree as a string."
  (let ((in-repo (monk-repo--join (oref r path) relative)))
    (decode-coding-string
     (car (or (monk-repo-source-blob (monk-repo--source-of r) in-repo (monk-repo--revision r))
              (signal 'monk-repo-error (list (format "%s vanished from the tree" in-repo)))))
     'utf-8-unix)))

(cl-defmethod monk-repo--tree-mode ((r monk-repo-tree-resource) executable)
  "Return the mode a file of R gets when EXECUTABLE says what git recorded."
  (or (oref r mode)
      (if executable monk-repo-executable-mode monk-file-default-mode)))

(defun monk-repo--local-files (full)
  "Return the regular files under FULL, relative, sorted, or nil when absent."
  (when (file-directory-p full)
    (sort (mapcar (lambda (f) (file-relative-name f full))
                  (seq-filter (lambda (f) (and (file-regular-p f) (not (file-symlink-p f))))
                              (directory-files-recursively full "" nil)))
          #'string<)))

(cl-defmethod monk-check ((r monk-repo-tree-resource) (m monk-machine))
  (let* ((full (monk-machine-path m (oref r name) (oref r sudo)))
         (files (monk-repo--tree-files r))
         (local (monk-repo--local-files full))
         (write nil) (extra nil) (modes nil))
    (monk-check-note :source (monk-repo-source-describe (monk-repo--source-of r)
                                                        (monk-repo--revision r))
                     :files (length files))
    (dolist (entry files)
      (let* ((relative (car entry))
             (target (expand-file-name relative full))
             (current (and (file-exists-p target) (monk-file-read m (concat (file-name-as-directory (oref r name)) relative) (oref r sudo))))
             (want (monk-repo--tree-mode r (cdr entry))))
        (cond
         ((null current) (push relative write))
         ((not (equal current (monk-repo--tree-blob r relative))) (push relative write))
         ((/= (logand (file-modes target) #o7777) want) (push relative modes)))))
    (when (oref r delete)
      (dolist (relative local)
        (unless (assoc relative files) (push relative extra))))
    (let ((total (max 1 (+ (length files) (length extra))))
          (wrong (+ (length write) (length modes) (length extra))))
      (and (> wrong 0)
           (list :write (nreverse write) :mode (nreverse modes) :delete (nreverse extra)
                 :degree (monk-set-degree total wrong))))))

(cl-defmethod monk-diff ((r monk-repo-tree-resource) (m monk-machine) changes)
  (append
   (mapcar (lambda (relative)
             (let ((dest (concat (file-name-as-directory (oref r name)) relative)))
               (monk-diff-content (monk-file-read m dest (oref r sudo))
                                  (monk-repo--tree-blob r relative) dest)))
           (plist-get changes :write))
   (mapcar (lambda (relative)
             (let ((dest (concat (file-name-as-directory (oref r name)) relative)))
               (monk-diff-content (monk-file-read m dest (oref r sudo)) nil dest)))
           (plist-get changes :delete))))

(cl-defmethod monk-repair ((r monk-repo-tree-resource) (m monk-machine) changes)
  (let ((files (monk-repo--tree-files r))
        (notes nil))
    (dolist (relative (plist-get changes :write))
      (let ((dest (concat (file-name-as-directory (oref r name)) relative)))
        (monk-file-write m dest (monk-repo--tree-blob r relative) (oref r sudo)
                         (monk-repo--tree-mode r (cdr (assoc relative files))))
        (when (or (oref r owner) (oref r group))
          (monk--apply-ownership m (monk-machine-local-path (monk-machine-path m dest (oref r sudo)))
                                 (oref r owner) (oref r group) (oref r sudo)))))
    (when (plist-get changes :write)
      (push (format "wrote %d" (length (plist-get changes :write))) notes))
    (dolist (relative (plist-get changes :mode))
      (set-file-modes (monk-machine-path m (concat (file-name-as-directory (oref r name)) relative) (oref r sudo))
                      (monk-repo--tree-mode r (cdr (assoc relative files)))))
    (when (plist-get changes :mode)
      (push (format "mode on %d" (length (plist-get changes :mode))) notes))
    (dolist (relative (plist-get changes :delete))
      (delete-file (monk-machine-path m (concat (file-name-as-directory (oref r name)) relative) (oref r sudo))))
    (when (plist-get changes :delete)
      (push (format "deleted %d" (length (plist-get changes :delete))) notes))
    (string-join (nreverse notes) ", ")))

(cl-defmethod monk-resource-paths ((r monk-repo-tree-resource))
  (list (oref r name)))

(cl-defmethod monk-resource-owns-subtree-p ((r monk-repo-tree-resource))
  "With :delete, R owns everything under its destination."
  (and (oref r delete) t))

(monk-define-resource monk-repo-tree monk-repo-tree-resource
  "Ensure the files under DEST mirror a directory of a repository."
  ((dest :type path :doc "The destination directory."))
  ((repo :type string :doc "Repository URL; a mirror under MONK_HOME, or a hosting API.")
   (checkout :type path :doc "Working tree on the controller instead of a URL.")
   (path :type string :default "" :doc "Directory inside the repository.  Default: the root.")
   (revision :type string :aliases (rev) :doc "Commit, branch, or tag.  Default HEAD.")
   (delete :type boolean :doc "Remove files under DEST that the tree does not hold.")
   (mode :type mode :doc "Mode for every file.  Default: 755 for blobs git marks executable, else 644.")
   (owner :type string :doc "User name.")
   (group :type string :doc "Group name."))
  :one-of (repo checkout)
  :exclusive ((repo checkout)))

(provide 'monk-repo)
;;; monk-repo.el ends here
