EMACS ?= emacs
BATCH = $(EMACS) --batch --no-init-file -L lisp -L test

EL_FILES = $(wildcard lisp/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

VERSION = $(shell sed -n 's/^;; Version: *//p' lisp/monk.el)
PKG_DIR = dist/monk-$(VERSION)

.PHONY: all compile test check clean package package-test

all: compile

compile: $(ELC_FILES)

%.elc: %.el
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

test: compile
	$(BATCH) -l monk-test -f ert-run-tests-batch-and-exit

# checkdoc: fail on anything except the missing-docstring advice for
# methods, which have their docstring on the generic.
check: compile
	$(BATCH) --eval '(progn (require (quote checkdoc)) (setq checkdoc-diagnostic-buffer "*cd*") (dolist (f (list $(patsubst %,"%",$(EL_FILES)))) (checkdoc-file f)) (let ((bad (and (get-buffer "*cd*") (with-current-buffer "*cd*" (seq-remove (lambda (l) (or (string-empty-p l) (string-match-p "might as well have a documentation string" l))) (split-string (buffer-string) "\n")))))) (when bad (mapc (lambda (l) (message "%s" l)) bad) (kill-emacs 1))))'

clean:
	rm -f $(ELC_FILES)
	rm -rf dist

# A multi-file package tarball as package.el and MELPA build it: every
# lisp/*.el flat in monk-VERSION/, a generated monk-pkg.el from the
# headers of monk.el.  `make package-test' installs it into a throwaway
# package-user-dir, which compiles every file and generates the
# autoloads, and fails on a file that did not compile.
package:
	rm -rf $(PKG_DIR) && mkdir -p $(PKG_DIR)
	cp $(EL_FILES) README.md $(PKG_DIR)/
	$(BATCH) --eval '(progn (require (quote package)) (package-generate-description-file (with-temp-buffer (insert-file-contents "lisp/monk.el") (package-buffer-info)) "$(PKG_DIR)/monk-pkg.el"))'
	cd dist && tar cf monk-$(VERSION).tar monk-$(VERSION)
	@echo "dist/monk-$(VERSION).tar"

package-test: package
	$(BATCH) -l test/monk-package-check.el --eval '(monk-package-check "dist/monk-$(VERSION).tar")'
