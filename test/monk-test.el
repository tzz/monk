;;; monk-test.el --- Tests for Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run with: make test
;;
;; Tests use a real local machine object pointed at a temporary
;; directory.  Resources that need root or a package manager are
;; tested through their pure helpers and through a fake exec layer.

;;; Code:

(require 'ert)
(require 'monk)
(require 'monk-file)
(require 'monk-system)
(require 'monk-package)
(require 'monk-inventory)

(defconst monk-test-fake-facts
  '((kernel . "Linux") (arch . "x86_64") (nodename . "testbox.local")
    (hostname . "testbox") (os-family . "Debian") (distribution . "Ubuntu")
    (distribution-release . "noble") (distribution-version . "24.04")
    (home . "/home/tester") (user . "tester"))
  "Facts used by tests that do not probe the host.")

(defun monk-test-machine (&optional facts)
  "Return a local machine with FACTS pre-filled so probes do not run."
  (let ((m (monk-local-machine :name "testbox")))
    (oset m facts (or facts monk-test-fake-facts))
    (oset m classes (monk-hard-classes m))
    m))

(defmacro monk-test-with-tmpdir (var &rest body)
  "Bind VAR to a fresh temporary directory, run BODY, delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "monk-test" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun monk-test-converge (policy &rest options)
  "Converge POLICY on a test machine with `monk-show-diffs' on."
  (let ((monk-log-level nil))
    (apply #'monk-converge (monk-test-machine) policy options)))

(defun monk-test-result (run name)
  "Return the first result in RUN named NAME."
  (seq-find (lambda (r) (equal (monk-result-name r) name)) (monk-run-results run)))

(defun monk-test-status (run name)
  "Return the status of result NAME in RUN."
  (monk-result-status (monk-test-result run name)))

;;;; Core

(ert-deftest monk-test-result-changed-p ()
  (should (monk-result-changed-p (monk-result-create :status 'repaired)))
  (should (monk-result-changed-p (monk-result-create :status 'would-repair)))
  (should-not (monk-result-changed-p (monk-result-create :status 'kept)))
  (should-not (monk-result-changed-p (monk-result-create :status 'failed))))

(ert-deftest monk-test-remote-prefix ()
  (let ((local (monk-local-machine))
        (remote (monk-machine :name "r" :address "host.example" :user "u" :port 2222))
        (plain (monk-machine :name "p" :address "host.example")))
    (should (equal "" (monk-machine-remote-prefix local)))
    (should (equal "/sudo::" (monk-machine-remote-prefix local t)))
    (should (equal "/ssh:u@host.example#2222:" (monk-machine-remote-prefix remote)))
    (should (equal "/ssh:u@host.example#2222|sudo::" (monk-machine-remote-prefix remote t)))
    (should (equal "/ssh:host.example:" (monk-machine-remote-prefix plain)))
    (should (equal "/ssh:host.example:/etc/hosts" (monk-machine-path plain "/etc/hosts")))
    (should (equal (expand-file-name "~/x") (monk-machine-path local "~/x")))))

(ert-deftest monk-test-hard-classes ()
  (let ((m (monk-machine :name "Box-1" :vars '((mine . t) (observer . nil) (count . 3))
                         :groups '(macs home world))))
    (oset m facts monk-test-fake-facts)
    (let ((classes (monk-hard-classes m)))
      (dolist (c '(linux debian ubuntu noble x86_64 box_1 macs home world mine local))
        (should (memq c classes)))
      (should-not (memq 'observer classes))
      (should-not (memq 'count classes)))))

(ert-deftest monk-test-parse-os-release ()
  (let ((parsed (monk--parse-os-release
                 "NAME=\"Ubuntu\"\nID=ubuntu\nID_LIKE=debian\nVERSION_CODENAME=noble\n")))
    (should (equal "ubuntu" (alist-get 'id parsed)))
    (should (equal "debian" (alist-get 'id_like parsed)))
    (should (equal "noble" (alist-get 'version_codename parsed)))))

(ert-deftest monk-test-facts-on-local-host ()
  "Probes run on the real local host and fill the required keys."
  (let ((m (monk-local-machine))
        (monk-log-level nil))
    (monk-gather-facts m)
    (dolist (key '(kernel arch hostname os-family home user))
      (should (stringp (monk-machine-fact m key))))
    (should (monk-class-p m 'local))))

(ert-deftest monk-test-exec ()
  (let* ((m (monk-local-machine))
         (ok (monk-exec m "sh" "-c" "echo hi; exit 0"))
         (bad (monk-exec m "sh" "-c" "echo no >&2; exit 3"))
         (with-input (monk-exec m "cat" :input "piped")))
    (should (monk-exec-ok-p ok))
    (should (equal "hi\n" (monk-exec-output ok)))
    (should (eql 3 (monk-exec-exit-code bad)))
    (should (equal "piped" (monk-exec-output with-input)))
    (should (equal "hi" (monk-exec-string m "sh" "-c" "echo hi")))
    (should-error (monk-exec-string m "sh" "-c" "exit 1") :type 'monk-exec-error)
    (should-error (monk-exec m "true" :sudo) :type 'monk-exec-error)
    (should-error (monk-exec m "true" :bogus t) :type 'monk-exec-error)
    (should (equal "/" (monk-exec-string m "pwd" :directory "/")))))

(ert-deftest monk-test-compile-collects-in-order ()
  (let ((resources (monk-compile (monk-test-machine)
                                 (lambda (_m)
                                   (monk-directory "/tmp/a")
                                   (monk-directory "/tmp/b")))))
    (should (equal '("/tmp/a" "/tmp/b")
                   (mapcar (lambda (r) (oref r name)) resources)))))

(ert-deftest monk-test-compile-rejects-duplicates ()
  (should-error (monk-compile (monk-test-machine)
                              (lambda (_m)
                                (monk-directory "/tmp/a")
                                (monk-symlink "/tmp/a" "/tmp/b")))
                :type 'monk-policy-error))

(ert-deftest monk-test-constructor-outside-policy ()
  (should-error (monk-directory "/tmp/a") :type 'monk-policy-error))

(ert-deftest monk-test-guards-and-classes ()
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-directory (concat dir "yes") :classes '(debian))
                  (monk-directory (concat dir "no-class") :classes '(darwin))
                  (monk-directory (concat dir "no-guard") :only-if (lambda (_m) nil))
                  (monk-directory (concat dir "not-if") :not-if (lambda (_m) t))
                  (monk-directory (concat dir "both") :only-if (lambda (m) (monk-class-p m 'local)))))))
      (should (eq 'repaired (monk-test-status run (concat dir "yes"))))
      (should (eq 'skipped (monk-test-status run (concat dir "no-class"))))
      (should (eq 'skipped (monk-test-status run (concat dir "no-guard"))))
      (should (eq 'skipped (monk-test-status run (concat dir "not-if"))))
      (should (eq 'repaired (monk-test-status run (concat dir "both"))))
      (should (file-directory-p (concat dir "yes")))
      (should-not (file-exists-p (concat dir "no-class"))))))

(ert-deftest monk-test-dry-run-changes-nothing ()
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-directory (concat dir "d"))
                  (monk-file (concat dir "f") :content "x\n"))
                :dry-run t)))
      (should (eq 'would-repair (monk-test-status run (concat dir "d"))))
      (should (eq 'would-repair (monk-test-status run (concat dir "f"))))
      (should-not (file-exists-p (concat dir "d")))
      (should-not (file-exists-p (concat dir "f")))
      (should (monk-run-dry-run run))
      (should (= 1 (monk-run-passes run)))
      (should (string-match-p "\\+x" (monk-result-diff (monk-test-result run (concat dir "f"))))))))

(ert-deftest monk-test-convergence-second-run-is-kept ()
  (monk-test-with-tmpdir dir
    (let ((policy (lambda (_m)
                    (monk-directory (concat dir "d") :mode #o700)
                    (monk-symlink (concat dir "l") (concat dir "d"))
                    (monk-file (concat dir "f") :content "hello\n" :mode #o600)
                    (monk-line-in-file (concat dir "g") "world" :create t))))
      (let ((first (monk-test-converge policy)))
        (should (= 4 (monk-run-count first 'repaired)))
        (should (monk-run-ok-p first))
        ;; Pass two re-checks and repairs nothing, so passes = 2.
        (should (= 2 (monk-run-passes first)))
        (should-not (monk-run-oscillating-p first)))
      (let ((second (monk-test-converge policy)))
        (should (= 4 (monk-run-count second 'kept)))
        (should (= 0 (monk-run-count second 'repaired)))
        (should (= 1 (monk-run-passes second)))))))

(ert-deftest monk-test-conflicting-resources-flap ()
  "Two resources that fight over one file flap.
Both repair in pass one and again in pass two.  The run stops there,
names both, and is not converged."
  (monk-test-with-tmpdir dir
    (let* ((f (concat dir "f"))
           (run (monk-test-converge
                 (lambda (_m)
                   (monk-file f :content "hello\n")
                   (monk-line-in-file f "world" :create t))
                 :passes 5)))
      (should (= 2 (monk-run-passes run)))
      (should (monk-run-flapping-p run))
      (should-not (monk-run-converged-p run))
      (should (equal (list (cons "testbox" f) (cons "testbox" (concat f "#world")))
                     (monk-run-flapping run)))
      (should (string-match-p "flapping: testbox:" (monk-run-report-string run)))
      (should-not (string-match-p "did not converge" (monk-run-report-string run))))))

(ert-deftest monk-test-flapping-needs-consecutive-repairs ()
  "A resource that repairs once and holds is not flapping.
A chain where a different resource repairs in each pass is slow, not
flapping, and reaches the cap as oscillating instead."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))))
      (should-not (monk-run-flapping-p run))
      (should (monk-run-converged-p run)))
    ;; Policy order is c, b, a, with each guarded on the next one's
    ;; directory.  Pass 1: only a repairs.  Pass 2: b.  Pass 3: c.  No
    ;; key repeats, so this is slow convergence, not flapping.
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-command "c" "mkdir" :args (list (concat dir "c"))
                                :creates (concat dir "c")
                                :only-if (lambda (_m) (file-directory-p (concat dir "b"))))
                  (monk-command "b" "mkdir" :args (list (concat dir "b"))
                                :creates (concat dir "b")
                                :only-if (lambda (_m) (file-directory-p (concat dir "a"))))
                  (monk-directory (concat dir "a")))
                :passes 3)))
      (should (= 3 (monk-run-passes run)))
      (should-not (monk-run-flapping-p run))
      (should (monk-run-oscillating-p run))
      (should (string-match-p "did not converge" (monk-run-report-string run))))
    ;; With room for a fourth pass the chain settles: pass 4 repairs nothing.
    (dolist (d '("a" "b" "c")) (delete-directory (concat dir d)))
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-command "c" "mkdir" :args (list (concat dir "c"))
                                :creates (concat dir "c")
                                :only-if (lambda (_m) (file-directory-p (concat dir "b"))))
                  (monk-command "b" "mkdir" :args (list (concat dir "b"))
                                :creates (concat dir "b")
                                :only-if (lambda (_m) (file-directory-p (concat dir "a"))))
                  (monk-directory (concat dir "a"))))))
      (should (= 4 (monk-run-passes run)))
      (should (monk-run-converged-p run)))))

(ert-deftest monk-test-flapping-detection-pure ()
  "`monk-run-flapping' reads repaired keys per pass, newest pass first.
A repeat at any distance counts, not only in adjacent passes."
  (let ((run (monk-run-create :repaired-by-pass '((("m" . "x")) (("m" . "x") ("m" . "y")) (("m" . "y"))))))
    ;; Passes in order: y; x y; x.  y repeats in 1-2, x repeats in 2-3.
    (should (equal '(("m" . "y") ("m" . "x")) (monk-run-flapping run))))
  ;; Passes in order: a; b; c; a.  Only a repeats, three passes apart.
  (let ((run (monk-run-create :repaired-by-pass '((("m" . "a")) (("m" . "c")) (("m" . "b")) (("m" . "a"))))))
    (should (equal '(("m" . "a")) (monk-run-flapping run))))
  (should-not (monk-run-flapping (monk-run-create :repaired-by-pass '((("m" . "x")) (("m" . "y"))))))
  (should-not (monk-run-flapping (monk-run-create :repaired-by-pass '((("m" . "x")))))))

(ert-deftest monk-test-flapping-three-cycle ()
  "A -> B -> C -> not A: each undoes the next, so A repairs again in pass 4.
The run stops there and names A, not at the cap."
  (monk-test-with-tmpdir dir
    (let* ((a (concat dir "a")) (b (concat dir "b")) (c (concat dir "c"))
           (run (monk-test-converge
                 (lambda (_m)
                   ;; A: file a exists.  Repair creates it and removes b.
                   (monk-command "A" "sh" :args (list "-c" (format "touch %s; rm -f %s" a b))
                                 :creates a)
                   ;; B: file b exists.  Repair creates it and removes c.
                   (monk-command "B" "sh" :args (list "-c" (format "touch %s; rm -f %s" b c))
                                 :creates b)
                   ;; C: file c exists.  Repair creates it and removes a.
                   (monk-command "C" "sh" :args (list "-c" (format "touch %s; rm -f %s" c a))
                                 :creates c))
                 :passes 10)))
      ;; Pass 1: A B C all repair (a removed by C).  Pass 2: A repairs
      ;; again, which is already a repeat.  Detection fires at pass 2.
      (should (monk-run-flapping-p run))
      (should (member (cons "testbox" "A") (monk-run-flapping run)))
      (should (< (monk-run-passes run) 10)))))

(ert-deftest monk-test-flapping-long-cycle ()
  "One repair per pass: A in pass 1, B in 2, C in 3, A again in 4.
Detection fires at pass 4, three passes after the first repair, and
names only A.  A cap below 4 misses it and reports did-not-converge."
  (let* ((pass 0)
         (needs (lambda (name)
                  ;; Resource NAME needs repair in the pass whose index
                  ;; modulo 3 matches it: A in 1 and 4, B in 2, C in 3.
                  (= (mod (1- pass) 3) (cl-position name '("A" "B" "C") :test #'equal))))
         (policy (lambda (_m)
                   (dolist (name '("A" "B" "C"))
                     (let ((name name))
                       (monk-command name "true" :creates "/nonexistent"
                                     :only-if (lambda (_m) (funcall needs name)))))))
         (monk-progress-functions
          (list (lambda (event data)
                  (when (eq event 'run-start) (setq pass 1))
                  (when (eq event 'pass-end) (setq pass (1+ (plist-get data :pass)))))))
         (run (monk-test-converge policy :passes 10)))
    (should (= 4 (monk-run-passes run)))
    (should (equal (list (cons "testbox" "A")) (monk-run-flapping run)))
    (should-not (monk-run-converged-p run))
    (setq pass 0)
    (let ((short (monk-test-converge policy :passes 3)))
      (should-not (monk-run-flapping-p short))
      (should (monk-run-oscillating-p short))
      (should (string-match-p "did not converge" (monk-run-report-string short))))))

(ert-deftest monk-test-failure-stops-run-unless-ignored ()
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x")
                  (monk-directory (concat dir "after"))))))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x"))))
      (should-not (monk-test-result run (concat dir "after")))
      (should-not (monk-run-ok-p run)))
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x" :ignore-errors t)
                  (monk-directory (concat dir "after"))))))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x"))))
      (should (eq 'repaired (monk-test-status run (concat dir "after")))))))

(ert-deftest monk-test-watch-runs-action-once ()
  "A handler watched by two changed resources runs its action once."
  (monk-test-with-tmpdir dir
    (let* ((marker (concat dir "ran"))
           (policy (lambda (_m)
                     (monk-file (concat dir "a") :content "a")
                     (monk-file (concat dir "b") :content "b")
                     (monk-command "toucher" "sh"
                                   :args (list "-c" (format "echo 1 >> %s" marker))
                                   :watch (list (concat dir "a") (concat dir "b")))))
           (run (monk-test-converge policy :passes 1)))
      (should (= 1 (with-temp-buffer (insert-file-contents marker) (count-lines 1 (point-max)))))
      (should (eq 'repaired (monk-test-status run "toucher")))
      (should (string-match-p "watch .* changed: ran" (monk-result-comment (monk-test-result run "toucher"))))
      ;; Second run: nothing changed, the handler is kept and does not run.
      (let ((run (monk-test-converge policy :passes 1)))
        (should (eq 'kept (monk-test-status run "toucher")))
        (should (= 1 (with-temp-buffer (insert-file-contents marker) (count-lines 1 (point-max)))))))))

(ert-deftest monk-test-watch-orders-handler-after-watched ()
  "The handler is scheduled after the resources it watches, whatever the policy order."
  (monk-test-with-tmpdir dir
    (let* ((order nil)
           (run (monk-test-converge
                 (lambda (_m)
                   (monk-command "handler" "true" :watch (list (concat dir "a")))
                   (monk-file (concat dir "a") :content "a"))
                 :passes 1)))
      (setq order (mapcar #'monk-result-name (monk-run-results run)))
      (should (equal (list (concat dir "a") "handler") order)))))

(ert-deftest monk-test-require-skips-when-unsatisfied ()
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x" :ignore-errors t)
                  (monk-directory (concat dir "after") :require (list (concat dir "missing#x")))
                  (monk-directory (concat dir "free")))
                :passes 1)))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x"))))
      (should (eq 'skipped (monk-test-status run (concat dir "after"))))
      (should (string-match-p "require" (monk-result-comment (monk-test-result run (concat dir "after")))))
      (should (eq 'repaired (monk-test-status run (concat dir "free")))))))

(ert-deftest monk-test-onchanges-and-onfail ()
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m)
                     (monk-file (concat dir "a") :content "a")
                     (monk-line-in-file (concat dir "missing") "x" :ignore-errors t)
                     (monk-directory (concat dir "on-change") :onchanges (list (concat dir "a")))
                     (monk-directory (concat dir "on-fail") :onfail (list (concat dir "missing#x")))
                     (monk-directory (concat dir "on-no-fail") :onfail (list (concat dir "a")))))
           (run (monk-test-converge policy :passes 1)))
      (should (eq 'repaired (monk-test-status run (concat dir "on-change"))))
      (should (eq 'repaired (monk-test-status run (concat dir "on-fail"))))
      (should (eq 'skipped (monk-test-status run (concat dir "on-no-fail"))))
      (delete-directory (concat dir "on-change"))
      ;; Second run: a is kept, so on-change is skipped.
      (let ((run (monk-test-converge policy :passes 1)))
        (should (eq 'skipped (monk-test-status run (concat dir "on-change"))))))))

(ert-deftest monk-test-requisite-unknown-ref-signals ()
  "A ref that matches nothing is a policy error, raised before any node runs."
  (should-error
   (monk-test-converge (lambda (_m) (monk-directory "/tmp/a" :require '("nope"))))
   :type 'monk-requisite-error)
  (should-error
   (monk-test-converge (lambda (_m)
                         (monk-directory "/tmp/a")
                         (monk-directory "/tmp/b" :require '((:ref "/tmp/a" :min 2)))))
   :type 'monk-requisite-error)
  ;; A self-reference matches nothing.
  (should-error
   (monk-test-converge (lambda (_m) (monk-directory "/tmp/a" :require '("/tmp/a"))))
   :type 'monk-requisite-error))

(ert-deftest monk-test-requisite-cycle-signals ()
  (should-error
   (monk-test-converge
    (lambda (_m)
      (monk-directory "/tmp/x" :require '("/tmp/y"))
      (monk-directory "/tmp/y" :require '("/tmp/x"))))
   :type 'monk-requisite-error))

(ert-deftest monk-test-parse-ref ()
  (let ((plain (monk-parse-ref "a"))
        (scoped (monk-parse-ref '(kamen . "a")))
        (thresholds (monk-parse-ref '(:ref (unix . "a") :min 2 :degree 0.5))))
    (should (null (monk-ref-scope plain)))
    (should (equal "a" (monk-ref-name plain)))
    (should (eq 'kamen (monk-ref-scope scoped)))
    (should (eq 'unix (monk-ref-scope thresholds)))
    (should (= 2 (monk-ref-min thresholds)))
    (should (= 0.5 (monk-ref-degree thresholds)))
    (should-error (monk-parse-ref 42) :type 'monk-requisite-error)))

(ert-deftest monk-test-report-string ()
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
           (text (monk-run-report-string run)))
      (should (string-match-p "== testbox: 0 kept, 1 repaired" text))
      (should (string-match-p (concat "repaired +directory +" (regexp-quote dir) "d") text)))))

;;;; File resources

(ert-deftest monk-test-directory-mode-and-conflict ()
  (monk-test-with-tmpdir dir
    (let ((d (concat dir "d")))
      (make-directory d)
      (set-file-modes d #o755)
      (let ((run (monk-test-converge (lambda (_m) (monk-directory d :mode #o700)))))
        (should (eq 'repaired (monk-test-status run d)))
        (should (equal "mode 0700" (monk-result-comment (monk-test-result run d))))
        (should (= #o700 (logand (file-modes d) #o7777))))
      (with-temp-file (concat dir "f") (insert "x"))
      (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "f"))))))
        (should (eq 'failed (monk-test-status run (concat dir "f"))))))))

(ert-deftest monk-test-symlink-create-retarget-replace ()
  (monk-test-with-tmpdir dir
    (let ((link (concat dir "deep/er/link")))
      (let ((run (monk-test-converge (lambda (_m) (monk-symlink link "/t1")))))
        (should (eq 'repaired (monk-test-status run link)))
        (should (equal '(:create "/t1") (monk-result-changes (monk-test-result run link))))
        (should (equal "/t1" (file-symlink-p link)))
        (should (= #o700 (logand (file-modes (concat dir "deep")) #o7777))))
      (let ((run (monk-test-converge (lambda (_m) (monk-symlink link "/t2")))))
        (should (equal '(:retarget ("/t1" "/t2")) (monk-result-changes (monk-test-result run link))))
        (should (equal "/t2" (file-symlink-p link))))
      (delete-file link)
      (with-temp-file link (insert "plain"))
      (let ((run (monk-test-converge (lambda (_m) (monk-symlink link "/t3" :force nil)))))
        (should (eq 'failed (monk-test-status run link))))
      (let ((run (monk-test-converge (lambda (_m) (monk-symlink link "/t3")))))
        (should (equal '(:replace (file "/t3")) (monk-result-changes (monk-test-result run link))))
        (should (equal "/t3" (file-symlink-p link)))))))

(ert-deftest monk-test-file-content-source-and-diff ()
  (monk-test-with-tmpdir dir
    (let ((src (concat dir "src"))
          (dst (concat dir "dst")))
      (with-temp-file src (insert "one\ntwo\n"))
      (with-temp-file dst (insert "one\n"))
      (let* ((run (monk-test-converge (lambda (_m) (monk-file dst :source src))))
             (result (monk-test-result run dst)))
        (should (eq 'repaired (monk-result-status result)))
        (should (string-match-p "^\\+two$" (monk-result-diff result)))
        (should (equal "one\ntwo\n" (with-temp-buffer (insert-file-contents dst) (buffer-string))))))))

(ert-deftest monk-test-file-content-error-signals-when-neither ()
  "A file with neither content nor source fails at check time."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge (lambda (_m) (monk-file (concat dir "x") :ignore-errors t)))))
      (should (eq 'failed (monk-test-status run (concat dir "x")))))))

(ert-deftest monk-test-edit-line ()
  (should (equal "a\nb\n" (monk-edit-line "a\n" "b")))
  (should (equal "a\n" (monk-edit-line "a\n" "a")))
  (should (equal "a\nX\n" (monk-edit-line "a\nb\n" "X" :regexp "^b")))
  (should (equal "a\nc\n" (monk-edit-line "a\nb\nc\n" "b" :state 'absent)))
  (should (equal "a\nb\nc\n" (monk-edit-line "a\nc\n" "b" :insert-after "^a")))
  (should (equal "a\nb\nc\n" (monk-edit-line "a\nc\n" "b" :insert-before "^c")))
  (should (equal "a\nc\nb\n" (monk-edit-line "a\nc\n" "b" :insert-before "^zzz")))
  ;; After a repair the regexp may no longer match the new line.  The
  ;; literal line counts as present, so the file does not grow.
  (should (equal "x\nPermitRootLogin no\n"
                 (monk-edit-line "x\nPermitRootLogin no\n" "PermitRootLogin no"
                                 :regexp "^PermitRootLogin yes")))
  (should (equal "b\n" (monk-edit-line nil "b")))
  (should (equal "a\nb\n" (monk-edit-line "a" "b")))
  (should-error (monk-edit-line "" "b" :state 'bogus) :type 'monk-resource-error))

(ert-deftest monk-test-line-in-file-sudoers-shape ()
  "The two sudoers edits from the Ansible playbook, applied to a fixture."
  (monk-test-with-tmpdir dir
    (let ((sudoers (concat dir "sudoers")))
      (with-temp-file sudoers
        (insert "Defaults env_reset\nroot ALL=(ALL) ALL\n%admin ALL=(ALL) ALL\n"))
      (let ((run (monk-test-converge
                  (lambda (_m)
                    (monk-line-in-file sudoers "tzz ALL=(ALL) NOPASSWD: ALL"
                                       :regexp "^tzz" :insert-after "^%admin")
                    (monk-line-in-file sudoers "Defaults env_keep += \"SSH_AUTH_SOCK\""
                                       :regexp "^Defaults env_keep.*SSH_AUTH_SOCK"
                                       :insert-before "^Defaults")))))
        (should (= 2 (monk-run-count run 'repaired)))
        (should (equal (concat "Defaults env_keep += \"SSH_AUTH_SOCK\"\n"
                               "Defaults env_reset\nroot ALL=(ALL) ALL\n"
                               "%admin ALL=(ALL) ALL\ntzz ALL=(ALL) NOPASSWD: ALL\n")
                       (with-temp-buffer (insert-file-contents sudoers) (buffer-string))))))))

(ert-deftest monk-test-absent ()
  (monk-test-with-tmpdir dir
    (let ((f (concat dir "f")) (d (concat dir "d")) (l (concat dir "l")))
      (with-temp-file f (insert "x"))
      (make-directory d)
      (make-symbolic-link "/nowhere" l)
      (let ((run (monk-test-converge (lambda (_m) (monk-absent f) (monk-absent d) (monk-absent l)))))
        (should (= 3 (monk-run-count run 'repaired)))
        (should (equal '(:remove symlink) (monk-result-changes (monk-test-result run l))))
        (should-not (file-exists-p f))
        (should-not (file-exists-p d))
        (should-not (file-symlink-p l))))))

;;;; System resources (pure parts)

(ert-deftest monk-test-cron-compute ()
  (let ((r (monk-cron-create-for-test "backup" "1 2 * * *" "do-it")))
    (should (equal "#Monk: backup\n1 2 * * * do-it\n" (monk--cron-compute r "")))
    (should (equal "other\n#Monk: backup\n1 2 * * * do-it\n"
                   (monk--cron-compute r "other\n")))
    ;; Replacing an existing managed entry keeps one copy.
    (should (equal "other\n#Monk: backup\n1 2 * * * do-it\n"
                   (monk--cron-compute r "other\n#Monk: backup\n9 9 * * * old\n")))
    ;; A prefix-matching marker of another resource is left alone.
    (should (equal "#Monk: backup2\n9 9 * * * other\n#Monk: backup\n1 2 * * * do-it\n"
                   (monk--cron-compute r "#Monk: backup2\n9 9 * * * other\n")))
    ;; A marker whose job line was removed by hand does not eat the next entry.
    (should (equal "#Monk: other\n3 3 * * * o\n#Monk: backup\n1 2 * * * do-it\n"
                   (monk--cron-compute r "#Monk: backup\n#Monk: other\n3 3 * * * o\n")))
    (oset r state 'absent)
    (should (equal "other\n" (monk--cron-compute r "other\n#Monk: backup\n1 2 * * * do-it\n")))))

(defun monk-cron-create-for-test (name schedule job)
  "Return a `monk-cron' object without registering it."
  (monk-cron-resource :name name :schedule schedule :job job))

(ert-deftest monk-test-command-creates-and-unless ()
  (monk-test-with-tmpdir dir
    (let ((marker (concat dir "m")))
      (let ((run (monk-test-converge
                  (lambda (_m)
                    (monk-command "touch" "touch" :args (list marker) :creates marker)))))
        (should (eq 'repaired (monk-test-status run "touch")))
        (should (file-exists-p marker)))
      (let ((run (monk-test-converge
                  (lambda (_m)
                    (monk-command "touch" "touch" :args (list marker) :creates marker)))))
        (should (eq 'kept (monk-test-status run "touch"))))
      (let ((run (monk-test-converge
                  (lambda (_m)
                    (monk-command "u" "false" :unless '("true"))
                    (monk-command "bare" "true" :ignore-errors t)))))
        (should (eq 'kept (monk-test-status run "u")))
        (should (eq 'kept (monk-test-status run "bare")))))))

(ert-deftest monk-test-git-clone-and-update ()
  (skip-unless (executable-find "git"))
  (monk-test-with-tmpdir dir
    (let ((origin (concat dir "origin"))
          (clone (concat dir "clone"))
          (process-environment (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                                         "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                                       process-environment)))
      (make-directory origin)
      (let ((default-directory origin))
        (call-process "git" nil nil nil "init" "-q" "-b" "main")
        (with-temp-file "f" (insert "1"))
        (call-process "git" nil nil nil "add" "f")
        (call-process "git" nil nil nil "commit" "-q" "-m" "one"))
      (let ((run (monk-test-converge (lambda (_m) (monk-git clone origin :branch "main")))))
        (should (eq 'repaired (monk-test-status run clone)))
        (should (equal "cloned" (monk-result-comment (monk-test-result run clone)))))
      (let ((run (monk-test-converge (lambda (_m) (monk-git clone origin :branch "main")))))
        (should (eq 'kept (monk-test-status run clone))))
      (let ((default-directory origin))
        (with-temp-file "f" (insert "2"))
        (call-process "git" nil nil nil "commit" "-q" "-am" "two"))
      (let ((run (monk-test-converge (lambda (_m) (monk-git clone origin :branch "main")))))
        (should (eq 'repaired (monk-test-status run clone)))
        (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'"
                                (plist-get (monk-result-changes (monk-test-result run clone)) :behind))))
      (let ((run (monk-test-converge (lambda (_m) (monk-git clone origin :branch "main" :update nil)))))
        (should (eq 'kept (monk-test-status run clone)))))))

;;;; Package helpers

(ert-deftest monk-test-osx-default-normalize ()
  (should (equal "1" (monk--osx-default-normalize 'bool t)))
  (should (equal "0" (monk--osx-default-normalize 'bool nil)))
  (should (equal "512" (monk--osx-default-normalize 'int 512)))
  (should (equal '("-bool" "false") (monk--osx-default-args 'bool nil)))
  (should (equal '("-array" "a" "b") (monk--osx-default-args 'array '("a" "b"))))
  (should (equal "(\n    a,\n    b\n)"
                 (monk--osx-default-clean (monk--osx-default-normalize 'array '("a" "b")))))
  (should-error (monk--osx-default-args 'bogus 1) :type 'monk-resource-error))

(ert-deftest monk-test-platform-resources-skip-off-platform ()
  "brew, apt, and defaults carry platform classes and skip elsewhere."
  (let ((run (monk-test-converge
              (lambda (_m)
                (monk-brew "b" '("jq"))
                (monk-brew-tap "x/y")
                (monk-osx-default "d" "k" 1 :type 'int)))))
    (should (= 3 (monk-run-count run 'skipped))))
  (let* ((darwin (monk-test-machine '((kernel . "Darwin") (os-family . "Darwin") (home . "/tmp"))))
         (monk-log-level nil)
         (run (monk-converge darwin (lambda (_m) (monk-apt "a" '("jq"))))))
    (should (= 1 (monk-run-count run 'skipped)))))

(ert-deftest monk-test-apt-check-parses-dpkg-output ()
  "Fake dpkg-query output through `monk-exec' and check the missing list."
  (let ((m (monk-test-machine))
        (r (monk-apt-resource :name "a" :packages '("jq" "curl" "nope"))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 1
                                   :output "jq installed\ncurl not-installed\n"))))
      (should (equal '("curl" "nope") (plist-get (monk-check r m) :install)))
      (should (< (abs (- (/ 1.0 3) (plist-get (monk-check r m) :degree))) 0.001)))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 0
                                   :output "jq installed\ncurl installed\nnope installed\n"))))
      (should-not (monk-check r m)))))

(ert-deftest monk-test-brew-check-handles-tapped-names ()
  (let ((m (monk-test-machine))
        (r (monk-brew-resource :name "b" :packages '("jq" "DimitarPetrov/stegify/stegify"))))
    (cl-letf (((symbol-function 'monk-exec-lines)
               (lambda (&rest _) '("jq" "stegify"))))
      (should-not (monk-check r m)))
    (cl-letf (((symbol-function 'monk-exec-lines)
               (lambda (&rest _) '("jq"))))
      (should (equal '(:install ("DimitarPetrov/stegify/stegify") :degree 0.5)
                     (monk-check r m))))))

;;;; Inventory

(defconst monk-test-inventory
  '(world
    :vars ((observer . nil) (tier . "base"))
    :children
    ((home
      :vars ((mine . t))
      :children
      ((macs
        :vars ((tier . "mac"))
        :hosts
        ((foxy :vars ((home_leader . t)))
         (moi :address "moi.local" :user "tzz")))
       (unix
        :hosts
        ((kamen :address "kamen.local" :port 22)))))
     (work
      :vars ((mine . nil))
      :hosts
      ((laptop))))))

(ert-deftest monk-test-inventory-define-and-merge ()
  (let ((machines (monk-inventory-define monk-test-inventory)))
    (should (= 4 (length machines)))
    (let ((foxy (monk-inventory-find 'foxy))
          (kamen (monk-inventory-find "kamen"))
          (laptop (monk-inventory-find 'laptop)))
      (should (monk-machine-local-p foxy))
      (should (equal "mac" (monk-machine-var foxy 'tier)))
      (should (eq t (monk-machine-var foxy 'mine)))
      (should (eq t (monk-machine-var foxy 'home_leader)))
      (should (equal '(macs home world) (oref foxy groups)))
      (should (equal "kamen.local" (oref kamen address)))
      (should (equal "base" (monk-machine-var kamen 'tier)))
      (should (eq nil (monk-machine-var laptop 'mine)))
      (should (equal 'missing (monk-machine-var laptop 'nothing 'missing))))))

(ert-deftest monk-test-inventory-select ()
  (monk-inventory-define monk-test-inventory)
  (should (equal '("foxy") (mapcar (lambda (m) (oref m name)) (monk-inventory-select 'foxy))))
  (should (equal '("foxy" "moi") (mapcar (lambda (m) (oref m name)) (monk-inventory-select 'macs))))
  (should (= 3 (length (monk-inventory-select 'home))))
  (should (= 4 (length (monk-inventory-select 'world))))
  (should (= 3 (length (monk-inventory-select '(macs kamen foxy)))))
  (should-error (monk-inventory-select 'nobody) :type 'monk-inventory-error)
  (cl-letf (((symbol-function 'system-name) (lambda () "kamen.local")))
    (should (equal "kamen" (oref (car (monk-inventory-select 'self)) name))))
  (cl-letf (((symbol-function 'system-name) (lambda () "stranger")))
    (should-error (monk-inventory-select 'self) :type 'monk-inventory-error)))

(ert-deftest monk-test-inventory-classes-from-vars ()
  (monk-inventory-define monk-test-inventory)
  (let ((foxy (monk-inventory-find 'foxy)))
    (oset foxy facts monk-test-fake-facts)
    (oset foxy classes (monk-hard-classes foxy))
    (should (monk-class-p foxy 'mine))
    (should (monk-class-p foxy 'home_leader))
    (should (monk-class-p foxy 'macs))
    (should-not (monk-class-p foxy 'observer))))

(ert-deftest monk-test-inventory-rejects-duplicates ()
  (should-error (monk-inventory-define '(root :hosts ((a) (a)))) :type 'monk-inventory-error)
  (should-error (monk-inventory-define '("root" :hosts ((a)))) :type 'monk-inventory-error))

(ert-deftest monk-test-symlink-tilde-target-expands ()
  "A ~ target is stored expanded, and a second run is kept."
  (monk-test-with-tmpdir dir
    (let* ((link (concat dir "link"))
           (policy (lambda (_m) (monk-symlink link "~/target"))))
      ;; The test machine's home fact is /home/tester, not $HOME.
      (let ((run (monk-test-converge policy)))
        (should (eq 'repaired (monk-test-status run link)))
        (should (equal "/home/tester/target" (file-symlink-p link))))
      (let ((run (monk-test-converge policy)))
        (should (eq 'kept (monk-test-status run link)))))))

(ert-deftest monk-test-failed-then-kept-reads-kept ()
  "A resource that fails in pass one and is kept in pass two reports kept."
  (monk-test-with-tmpdir dir
    (let* ((f (concat dir "f"))
           (run (monk-test-converge
                 (lambda (_m)
                   ;; Needs the file, which the next resource creates.
                   (monk-line-in-file f "x" :ignore-errors t)
                   (monk-file f :content "x\n")))))
      (should (eq 'kept (monk-test-status run (concat f "#x"))))
      (should (eq 'repaired (monk-test-status run f)))
      (should (monk-run-ok-p run))
      (should (= 2 (monk-run-passes run))))))

(ert-deftest monk-test-fatal-failure-runs-one-pass ()
  "A fatal failure stops the run.  It does not re-run earlier repairs."
  (monk-test-with-tmpdir dir
    (let* ((marker (concat dir "count"))
           (run (monk-test-converge
                 (lambda (_m)
                   (monk-command "counter" "sh"
                                 :args (list "-c" (format "echo 1 >> %s" marker))
                                 :creates (concat dir "never"))
                   (monk-line-in-file (concat dir "missing") "x")))))
      (should (= 1 (monk-run-passes run)))
      (should-not (monk-run-oscillating-p run))
      (should (= 1 (with-temp-buffer (insert-file-contents marker) (count-lines 1 (point-max)))))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x")))))))

(ert-deftest monk-test-systemd-enabled-states ()
  (dolist (state '("enabled" "static" "indirect" "alias"))
    (should (member state monk-systemd-enabled-states)))
  (should-not (member "disabled" monk-systemd-enabled-states))
  (should-not (member "masked" monk-systemd-enabled-states)))

;;;; Orchestration across machines

(defun monk-test-machines (&rest names)
  "Return local test machines named NAMES, all in group `pool'."
  (mapcar (lambda (name)
            (let ((m (monk-local-machine :name name)))
              (oset m groups '(pool))
              (oset m facts monk-test-fake-facts)
              (oset m classes (monk-hard-classes m))
              m))
          names))

(ert-deftest monk-test-orchestrate-cross-machine-require ()
  "A resource on b waits for a resource on a, whatever the machine order."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b"))
           (monk-log-level nil)
           (run (monk-orchestrate
                 machines
                 (lambda (m)
                   (pcase (oref m name)
                     ("a" (monk-file (concat dir "leader") :content "x"))
                     ("b" (monk-directory (concat dir "follower")
                                          :require (list (cons 'a (concat dir "leader")))))))
                 :passes 1)))
      (should (equal '(("a" . "leader") ("b" . "follower"))
                     (mapcar (lambda (r) (cons (monk-result-machine r)
                                               (file-name-nondirectory (monk-result-name r))))
                             (monk-run-results run))))
      (should (eq 'repaired (monk-result-status (cadr (monk-run-results run)))))
      (should (equal '("a" "b") (mapcar (lambda (m) (oref m name)) (monk-run-machines run)))))))

(ert-deftest monk-test-orchestrate-group-threshold ()
  "A group ref with :min succeeds when enough machines succeed.
Machine a skips its member by guard, b repairs it, so one of two
holds: :min 1 passes and the strict ref does not."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b" "c"))
           (monk-log-level nil)
           (run (monk-orchestrate
                 machines
                 (lambda (m)
                   (pcase (oref m name)
                     ("c"
                      (monk-directory (concat dir "gate")
                                      :require (list (list :ref (cons 'pool "member") :min 1)))
                      (monk-directory (concat dir "strict")
                                      :require (list (cons 'pool "member"))))
                     ("a" (monk-command "member" "true" :creates (concat dir "nothing")
                                        :only-if (lambda (_) nil)))
                     ("b" (monk-command "member" "true" :creates (concat dir "nothing")))))
                 :passes 1)))
      (should (eq 'repaired (monk-test-status run (concat dir "gate"))))
      (should (eq 'skipped (monk-test-status run (concat dir "strict")))))))

(ert-deftest monk-test-orchestrate-unknown-machine-scope ()
  "A ref to a machine outside the run matches nothing and signals."
  (let ((machines (monk-test-machines "a"))
        (monk-log-level nil))
    (should-error
     (monk-orchestrate machines
                       (lambda (_m)
                         (monk-directory "/tmp/x" :require '((nobody . "/tmp/y")))))
     :type 'monk-requisite-error)))

(ert-deftest monk-test-orchestrate-group-ref-excludes-self ()
  "A group ref from a member of that group does not wait on itself."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b"))
           (monk-log-level nil)
           (run (monk-orchestrate
                 machines
                 (lambda (m)
                   (monk-directory (concat dir (oref m name))
                                   :require (list (list :ref (cons 'pool (concat dir "shared")) :min 1)))
                   (monk-command (concat dir "shared") "true" :creates (concat dir "none")))
                 :passes 1)))
      (should (eq 'repaired (monk-test-status run (concat dir "a"))))
      (should (eq 'repaired (monk-test-status run (concat dir "b")))))))

(ert-deftest monk-test-onchanges-keeps-status-across-passes ()
  "A node that ran in pass one and is skipped in pass two reports its pass-one status."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-file (concat dir "a") :content "a")
                  (monk-command "after" "true" :onchanges (list (concat dir "a")))))))
      (should (= 2 (monk-run-passes run)))
      (should (eq 'kept (monk-test-status run "after"))))))

(ert-deftest monk-test-run-end-sent-on-escaping-error ()
  "run-end reaches progress functions when an error escapes the pass loop.
A quit behaves the same way; ERT cannot catch a quit, so the test
uses an error from a progress hook, which the node handler does not
cover."
  (let* ((events nil)
         (monk-progress-functions
          (list (lambda (event _data)
                  (push event events)
                  (when (eq event 'node-start) (error "hook failure")))))
         (monk-log-level nil))
    (should-error
     (monk-test-converge
      (lambda (_m) (monk-command "boom" "true" :creates "/nonexistent")))
     :type 'error)
    (should (memq 'run-end events))))

;;;; Degree and partial

(ert-deftest monk-test-set-degree ()
  (should (= 1.0 (monk-set-degree 0 0)))
  (should (= 1.0 (monk-set-degree 4 0)))
  (should (= 0.5 (monk-set-degree 4 2)))
  (should (= 0.0 (monk-set-degree 4 4))))

(ert-deftest monk-test-partial-status-and-degree-threshold ()
  "A set resource that fails to repair reports partial with its degree.
A :degree threshold on a require lets a follower run anyway."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (run (cl-letf (((symbol-function 'monk-exec)
                         (lambda (_m program &rest _)
                           (if (equal program "dpkg-query")
                               (monk-exec-create :exit-code 0
                                                 :output "a installed\nb installed\nc missing\nd missing\n")
                             (monk-exec-create :exit-code 1 :output "apt broke")))))
                (monk-converge
                 m
                 (lambda (_m)
                   (monk-apt "pkgs" '("a" "b" "c" "d") :ignore-errors t)
                   (monk-command "needs-all" "true" :require '("pkgs"))
                   (monk-command "needs-half" "true" :require '((:ref "pkgs" :degree 0.5))))
                 :passes 1))))
    (should (eq 'partial (monk-test-status run "pkgs")))
    (should (= 0.5 (monk-result-degree (monk-test-result run "pkgs"))))
    (should (eq 'skipped (monk-test-status run "needs-all")))
    (should (eq 'kept (monk-test-status run "needs-half")))
    (should-not (monk-run-ok-p run))
    (should (string-match-p "partial .*pkgs \\[50%\\]" (monk-run-report-string run)))))

(ert-deftest monk-test-monk-expand ()
  (let ((m (monk-test-machine))
        (seen nil))
    (should (equal "/home/tester/x" (monk-machine-expand m "~/x")))
    (should (equal "/abs" (monk-machine-expand m "/abs")))
    (should (equal "rel" (monk-machine-expand m "rel")))
    (monk-compile m (lambda (_m) (setq seen (monk-expand "~/y"))))
    (should (equal "/home/tester/y" seen))
    (should-error (monk-expand "~/z") :type 'monk-policy-error)))

;;;; UI text paths

(ert-deftest monk-test-ui-badge-and-glyphs ()
  (require 'monk-ui)
  (let ((monk-ui-unicode nil))
    (should (equal "! failed" (substring-no-properties (monk-ui-badge 'failed))))
    (should (equal "= 3" (substring-no-properties (monk-ui-badge 'kept "3")))))
  (should (eq 'monk-ui-failed (monk-ui-face 'failed)))
  (should (eq 'default (monk-ui-face 'bogus))))

(ert-deftest monk-test-ui-text-bar ()
  (require 'monk-ui)
  (let* ((monk-ui-style 'text)
         ;; Width 15 holds a 10-cell bar plus " 8/10".
         (bar (monk-ui-bar (list (cons 'kept 6) (cons 'failed 2)) 10 nil 15)))
    ;; 2 failed cells, then 6 kept, then 2 track; label after.
    (should (equal "████████░░ 8/10" (substring-no-properties bar)))
    (should (eq 'monk-ui-failed (get-text-property 0 'face bar)))
    (should (eq 'monk-ui-kept (get-text-property 2 'face bar)))
    (should (eq 'monk-ui-track (get-text-property 9 'face bar))))
  (let ((monk-ui-style nil))
    (should (equal "x" (monk-ui-bar nil 0 "x")))))

(ert-deftest monk-test-ui-fit-label ()
  (require 'monk-ui)
  (should (equal "450/500 90%" (monk-ui--fit-label "450/500 90%  ~12s left" 12)))
  (should (equal "450/500…" (monk-ui--fit-label "450/500 90%" 8))))

(ert-deftest monk-test-ui-blend ()
  (require 'monk-ui)
  (should (equal "#808080" (monk-ui--blend "#ffffff" "#000000" 0.5)))
  (should (equal "#ff0000" (monk-ui--blend "#ff0000" "#000000" 1.0))))

(ert-deftest monk-test-ui-report-buffer-in-batch ()
  "The report function prints text in batch and the entries build."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
           (monk-ui-style 'text))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run run)
        (let ((entries (monk-ui--entries)))
          (should (= 1 (length entries)))
          (should (equal "testbox" (caar entries)))
          (should (equal "1" (aref (cadr (car entries)) 4))))
        (should (string-match-p "1/1" (substring-no-properties (monk-ui--header-line))))))))

;;;; Bridge parsing

(ert-deftest monk-test-bridge-format-args ()
  (require 'monk-bridge)
  (should (equal '("path=/x" "state=link" "force=yes" "groups=a,b")
                 (monk-bridge--format-args '((path . "/x") (state . "link") (force . t)
                                             (groups . ("a" "b")))))))

(ert-deftest monk-test-ansible-parse ()
  (require 'monk-bridge)
  (let* ((m (monk-test-machine))
         (r (monk-ansible-resource :name "x" :module "file" :args '((path . "/x"))))
         (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create
                  :exit-code 0
                  :output "--- before | old\n+++ after | new\nlocalhost | CHANGED => {\"changed\": true, \"diff\": {\"before\": \"a\", \"after\": \"b\"}, \"msg\": \"linked\"}\n"))))
      (let ((changes (monk-check r m)))
        (should (equal "linked" (plist-get changes :ansible)))
        (should (string-match-p "^-a$" (monk-diff r m changes))))
      (should (equal "file linked" (monk-repair r m nil))))
    ;; lineinfile returns diff as a list of before/after alists.
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create
                  :exit-code 0
                  :output "localhost | CHANGED => {\"changed\": true, \"diff\": [{\"before\": \"a\", \"after\": \"b\"}, {\"before\": \"c\", \"after\": \"d\"}]}\n"))))
      (let ((changes (monk-check r m)))
        (should (string-match-p "^-c$" (monk-diff r m changes)))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 0
                                   :output "localhost | SUCCESS => {\"changed\": false}\n"))))
      (should-not (monk-check r m)))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 0
                                   :output "localhost | SKIPPED => {\"skipped\": true}\n"))))
      (should (eq 'no-check-mode (plist-get (monk-check r m) :ansible))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 2
                                   :output "localhost | FAILED! => {\"msg\": \"boom\"}\n"))))
      (should-error (monk-check r m) :type 'monk-resource-error))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 127 :output "not found"))))
      (should-error (monk-check r m) :type 'monk-exec-error))))

(ert-deftest monk-test-salt-parse ()
  (require 'monk-bridge)
  (let* ((m (monk-test-machine))
         (r (monk-salt-resource :name "/x" :function "file.symlink" :args '((target . "/y"))))
         (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create
                  :exit-code 0
                  :output "{\"local\": {\"file_|-/x_|-/x_|-symlink\": {\"result\": null, \"changes\": {}, \"comment\": \"Symlink /x to /y is set for creation\"}}}"))))
      (should (stringp (plist-get (monk-check r m) :salt))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create
                  :exit-code 0
                  :output "{\"local\": {\"id\": {\"result\": true, \"changes\": {}, \"comment\": \"in place\"}}}"))))
      (should-not (monk-check r m))
      (should (equal "file.symlink: in place" (monk-repair r m nil))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create
                  :exit-code 1
                  :output "{\"local\": {\"id\": {\"result\": false, \"changes\": {}, \"comment\": \"nope\"}}}"))))
      (should-error (monk-check r m) :type 'monk-resource-error))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 1
                                   :output "{\"local\": [\"Rendering SLS failed\"]}"))))
      (should-error (monk-check r m) :type 'monk-resource-error))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (monk-exec-create :exit-code 0 :output "{\"local\": {}}"))))
      (should-error (monk-check r m) :type 'monk-resource-error))))

(provide 'monk-test)
;;; monk-test.el ends here
