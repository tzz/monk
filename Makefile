EMACS ?= emacs
BATCH = $(EMACS) --batch --no-init-file -L lisp -L test

EL_FILES = $(wildcard lisp/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

.PHONY: all compile test check clean

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
