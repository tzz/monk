;;; monk-package-check.el --- Install the built tarball into a throwaway ELPA  -*- lexical-binding: t; -*-

;;; Commentary:

;; `make package-test' runs `monk-package-check' on dist/monk-VERSION.tar:
;; install it with package.el into a fresh `package-user-dir', which
;; byte-compiles every file and generates the autoloads the way MELPA
;; and GNU ELPA users get them, then fail when a file did not compile
;; or an entry point has no autoload.  Nothing of the user's own
;; packages is touched.

;;; Code:

(require 'package)
(require 'seq)

(defconst monk-package-check-entry-points
  '(monk-plan monk-apply monk-version monk-watch-start monk-watch-stop monk-connect
    monk-reconnect monk-agent-main monk-agent-poke monk-reload monk-testdrive
    monk-tutorial monk-ui-history monk-translate-import monk-embark-setup)
  "Commands a user reaches before any Monk library is loaded; each needs an autoload.")

(defun monk-package-check (tarball)
  "Install TARBALL into a throwaway ELPA directory and check the result."
  (setq package-user-dir (make-temp-file "monk-elpa" t))
  (package-initialize)
  (package-install-file (expand-file-name tarball))
  (let* ((desc (cadr (assq 'monk package-alist)))
         (dir (and desc (package-desc-dir desc)))
         (sources (and dir (seq-remove (lambda (f) (string-match-p "autoloads\\|-pkg" f))
                                        (directory-files dir t "\\.el\\'"))))
         (missing (seq-remove (lambda (f) (file-exists-p (concat f "c"))) sources))
         ;; the generated file, not `fboundp': compiling the package
         ;; in this same Emacs has already defined the functions
         (autoloads (and dir (with-temp-buffer
                               (insert-file-contents (expand-file-name "monk-autoloads.el" dir))
                               (buffer-string))))
         (no-autoload (seq-remove (lambda (f) (string-match-p (format "(autoload '%s " (regexp-quote (symbol-name f)))
                                                              (or autoloads "")))
                                  monk-package-check-entry-points)))
    (unless dir
      (message "monk did not install")
      (kill-emacs 1))
    (when missing
      (message "not compiled: %s" (mapconcat #'file-name-nondirectory missing ", "))
      (kill-emacs 1))
    (when no-autoload
      (message "no autoload: %s" (mapconcat #'symbol-name no-autoload ", "))
      (kill-emacs 1))
    (message "package ok: %s, %d files compiled, %d entry points autoloaded"
             (package-version-join (package-desc-version desc))
             (length sources) (length monk-package-check-entry-points))))

(provide 'monk-package-check)
;;; monk-package-check.el ends here
