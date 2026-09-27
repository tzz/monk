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
    (should (equal "/sudo:root@localhost:" (monk-machine-remote-prefix local t)))
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
  ;; macOS reports the host name in upper case.
  (cl-letf (((symbol-function 'system-name) (lambda () "KAMEN.local")))
    (should (equal "kamen" (oref (car (monk-inventory-select 'self)) name))))
  (should (equal "foxy" (oref (monk-inventory-find "FOXY") name)))
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

(ert-deftest monk-test-ui-select-window-rule ()
  "Command runs take focus, watch runs do not, and t/nil override."
  (require 'monk-ui)
  (let ((command (monk-run-create))
        (watch (monk-run-create :reasons '((("m" . "x") . "changed")))))
    (let ((monk-ui-select-window 'command))
      (should (monk-ui-select-window-p command))
      (should-not (monk-ui-select-window-p watch)))
    (let ((monk-ui-select-window t))
      (should (monk-ui-select-window-p watch)))
    (let ((monk-ui-select-window nil))
      (should-not (monk-ui-select-window-p command)))))

(ert-deftest monk-test-ui-report-buffer-in-batch ()
  "The report function prints text in batch and the entries build."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
           (monk-ui-style 'text))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run run)
        (monk-ui-refresh)
        (let ((entries (monk-ui--entries)))
          (should (= 1 (length entries)))
          (should (equal "testbox" (caar entries)))
          (should (equal "1" (aref (cadr (car entries)) 4))))
        (should (string-match-p "1/1" (substring-no-properties (monk-ui--header-line))))
        ;; The summary owns the header line; the column names are in the body.
        (should (equal '(:eval (monk-ui--header-line)) header-line-format))
        (should-not tabulated-list-use-header-line)
        (should (string-match-p "\\` ?Machine +State" (buffer-substring-no-properties
                                                         (point-min) (line-end-position))))))))

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

;;;; Plan and apply

(ert-deftest monk-test-plan-then-apply-as-planned ()
  "A plan predicts two changes; apply makes them; every verdict is as-planned."
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m)
                     (monk-directory (concat dir "d"))
                     (monk-file (concat dir "f") :content "x\n")
                     (monk-directory dir)))
           (m (monk-test-machine))
           (monk-log-level nil)
           (plan (monk-plan m policy))
           (untouched (not (file-exists-p (concat dir "d"))))
           (apply (monk-apply plan)))
      (should (monk-run-dry-run plan))
      (should (= 2 (monk-run-count plan 'would-repair)))
      (should (= 1 (monk-run-count plan 'kept)))
      (should untouched)
      (should-not (monk-run-dry-run apply))
      (should (eq plan (monk-run-plan apply)))
      (should (eq policy (monk-run-policy apply)))
      (should (= 2 (monk-run-count apply 'repaired)))
      (should (file-directory-p (concat dir "d")))
      (should (monk-run-as-planned-p apply))
      (should (equal '((as-planned . 3))
                     (monk-run-compare-counts (monk-run-compare plan apply))))
      (should (string-match-p "against plan: 3 as-planned" (monk-run-report-string apply))))))

(ert-deftest monk-test-plan-apply-detects-drift ()
  "The machine changes between plan and apply.
One planned change is already done (unfulfilled), one unplanned
change appears, and one resource fails."
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m)
                     (monk-directory (concat dir "planned"))
                     (monk-file (concat dir "stable") :content "same\n")
                     (monk-line-in-file (concat dir "fragile") "x" :ignore-errors t)))
           (m (monk-test-machine))
           (monk-log-level nil))
      (with-temp-file (concat dir "stable") (insert "same\n"))
      (with-temp-file (concat dir "fragile") (insert "old\n"))
      (let ((plan (monk-plan m policy)))
        ;; Drift: someone creates planned, edits stable, deletes fragile.
        (make-directory (concat dir "planned"))
        (with-temp-file (concat dir "stable") (insert "edited\n"))
        (delete-file (concat dir "fragile"))
        (let* ((apply (monk-apply plan))
               (comparison (monk-run-compare plan apply)))
          (should-not (monk-run-as-planned-p apply))
          (should (eq 'unfulfilled (alist-get (cons "testbox" (concat dir "planned")) comparison nil nil #'equal)))
          (should (eq 'unplanned (alist-get (cons "testbox" (concat dir "stable")) comparison nil nil #'equal)))
          (should (eq 'failed-planned (alist-get (cons "testbox" (concat dir "fragile#x")) comparison nil nil #'equal)))
          (let ((report (monk-run-report-string apply)))
            (should (string-match-p "unfulfilled +testbox:" report))
            (should (string-match-p "unplanned +testbox:" report))))))))

(ert-deftest monk-test-plan-compare-new-and-missing ()
  "Policies that differ between plan and apply produce new and missing."
  (let* ((plan (monk-run-create :dry-run t :results
                                (list (monk-result-create :machine "m" :name "a" :status 'kept)
                                      (monk-result-create :machine "m" :name "gone" :status 'would-repair))))
         (apply (monk-run-create :plan plan :results
                                 (list (monk-result-create :machine "m" :name "a" :status 'kept)
                                       (monk-result-create :machine "m" :name "extra" :status 'repaired)))))
    (should (equal '((("m" . "a") . as-planned) (("m" . "extra") . new) (("m" . "gone") . missing))
                   (monk-run-compare plan apply)))
    (should-not (monk-run-as-planned-p apply))))

(ert-deftest monk-test-plan-verdict-table ()
  (let ((kept (monk-result-create :status 'kept))
        (would (monk-result-create :status 'would-repair))
        (did (monk-result-create :status 'repaired))
        (bad (monk-result-create :status 'failed))
        (part (monk-result-create :status 'partial)))
    (should (eq 'as-planned (monk--plan-verdict kept kept)))
    (should (eq 'as-planned (monk--plan-verdict would did)))
    (should (eq 'unfulfilled (monk--plan-verdict would kept)))
    (should (eq 'unplanned (monk--plan-verdict kept did)))
    (should (eq 'failed-planned (monk--plan-verdict would bad)))
    (should (eq 'failed-planned (monk--plan-verdict kept part)))
    (should (eq 'as-planned (monk--plan-verdict bad bad)))
    (should (eq 'as-planned (monk--plan-verdict part bad)))
    (should (eq 'new (monk--plan-verdict nil did)))
    (should (eq 'missing (monk--plan-verdict would nil)))))

(ert-deftest monk-test-apply-requires-a-plan ()
  (let ((real (monk-run-create :dry-run nil)))
    (should-error (monk-apply real) :type 'monk-policy-error)))

(ert-deftest monk-test-apply-overrides-options-but-records-them ()
  "Options recorded on the plan repeat on apply; :plan is a link, not an option."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (plan (monk-plan m (lambda (_m) (monk-directory (concat dir "d"))) :passes 2))
           (apply (monk-apply plan :passes 4)))
      (should (equal 2 (plist-get (monk-run-options plan) :passes)))
      (should (eq t (plist-get (monk-run-options plan) :dry-run)))
      (should (equal 4 (plist-get (monk-run-options apply) :passes)))
      (should-not (plist-member (monk-run-options apply) :plan))
      (should (eq plan (monk-run-plan apply))))))

(ert-deftest monk-test-ui-plan-and-apply-views ()
  "The report buffer shows a Would column for a plan and a Plan column for an apply."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m) (monk-directory (concat dir "d"))))
           (m (monk-test-machine))
           (monk-log-level nil)
           (monk-ui-style 'text)
           (monk-ui-unicode nil)
           (plan (monk-plan m policy)))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run plan)
        (monk-ui-refresh)
        (should (equal "Would" (car (aref tabulated-list-format 4))))
        (should (= 8 (length tabulated-list-format)))
        (let ((header (substring-no-properties (monk-ui--header-line))))
          (should (string-match-p "PLAN" header))
          (should (string-match-p "to apply" header)))
        (let ((apply (monk-apply plan)))
          (setq monk-ui--run apply)
          (monk-ui-refresh)
          (should (= 9 (length tabulated-list-format)))
          (should (equal "Plan" (car (aref tabulated-list-format 7))))
          (let ((row (cadr (car (monk-ui--entries)))))
            (should (equal "= as planned" (substring-no-properties (aref row 7)))))
          (let ((header (substring-no-properties (monk-ui--header-line))))
            (should (string-match-p "APPLY +from plan" header))
            (should (string-match-p "= 1" header))))))))

(ert-deftest monk-test-ui-plan-cell-names-drift ()
  (require 'monk-ui)
  (let ((monk-ui-unicode nil))
    (should (equal "+ 2 o 1"
                   (substring-no-properties
                    (monk-ui--machine-plan-cell
                     "m" '((("m" . "a") . unplanned) (("m" . "b") . unplanned)
                           (("m" . "c") . unfulfilled) (("m" . "d") . as-planned)
                           (("other" . "e") . unplanned))))))))

;;;; Resource paths and partial runs

(ert-deftest monk-test-resource-paths ()
  (let* ((m (monk-test-machine))
         (resources (monk-compile
                     m (lambda (_m)
                         (monk-directory "~/d")
                         (monk-symlink "~/l" "~/t")
                         (monk-file "~/f" :content "x")
                         (monk-line-in-file "~/g" "x")
                         (monk-absent "~/a")
                         (monk-cron "c" "* * * * *" "true")
                         (monk-git "~/repo" "u")
                         (monk-command "k" "true" :creates "~/made")
                         (monk-command "bare" "true")))))
    (should (equal '(("~/d") ("~/l" "~/t") ("~/f") ("~/g") ("~/a") nil ("~/repo/.git") ("~/made") nil)
                   (mapcar #'monk-resource-paths resources)))))

(ert-deftest monk-test-select-nodes-closes-over-dependents ()
  "Selecting a resource also selects what watches or requires it, transitively."
  (let* ((m (monk-test-machine))
         (nodes (monk--nodes-for
                 (list m)
                 (lambda (_m)
                   (monk-file "/tmp/conf" :content "x")
                   (monk-command "svc" "true" :watch '("/tmp/conf"))
                   (monk-command "after-svc" "true" :require '("svc"))
                   (monk-command "unrelated" "true"))))
         (selected (monk-select-nodes nodes '(("testbox" . "/tmp/conf")))))
    (should (equal '("/tmp/conf" "svc" "after-svc")
                   (mapcar (lambda (n) (oref (monk-node-resource n) name)) selected)))
    (should-not (monk-node-selected (car (last nodes))))))

(ert-deftest monk-test-orchestrate-only ()
  "With :only, unselected nodes are skipped and count as satisfied for requires."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (policy (lambda (_m)
                     (monk-directory (concat dir "base"))
                     (monk-file (concat dir "conf") :content "x" :require (list (concat dir "base")))
                     (monk-directory (concat dir "other"))))
           (run (monk-orchestrate (list m) policy
                                  :only (list (cons "testbox" (concat dir "conf"))))))
      (should (eq 'skipped (monk-test-status run (concat dir "base"))))
      (should (equal monk-skip-not-selected
                     (monk-result-comment (monk-test-result run (concat dir "base")))))
      ;; conf requires base; base was not selected, so it counts as fine.
      (should (eq 'repaired (monk-test-status run (concat dir "conf"))))
      (should (eq 'skipped (monk-test-status run (concat dir "other"))))
      (should-not (file-exists-p (concat dir "other")))
      (should (equal (list (cons "testbox" (concat dir "conf")))
                     (plist-get (monk-run-options run) :only))))))

;;;; Watch

(require 'monk-watch)

(defmacro monk-test-with-watch (dir-var policy &rest body)
  "Start a watch over POLICY on a test machine in a temp dir bound to DIR-VAR."
  (declare (indent 2))
  `(monk-test-with-tmpdir ,dir-var
     (let ((monk-log-level nil)
           (monk-watch-debounce 0.05)
           (monk-watch-settle 0.0)
           (monk-watch-mode 'assisted)
           (monk-report-function #'ignore)
           (m (monk-test-machine)))
       (unwind-protect
           (cl-letf (((symbol-function 'file-notify-add-watch)
                      (lambda (&rest _) (cl-gensym "desc")))
                     ((symbol-function 'file-notify-rm-watch) #'ignore)
                     ((symbol-function 'file-notify-valid-p) (lambda (_) t))
                     ((symbol-value 'file-notify--library) 'test)
                     ((symbol-value 'monk-watch--delivery) 'yes))
             (monk-watch-start m ,policy)
             ,@body)
         (monk-watch-stop)))))

(defun monk-test-fire (file &optional action)
  "Inject a filenotify event for FILE and let the debounce timer run."
  (monk-watch--on-event (list 'desc (or action 'changed) file))
  (let ((deadline (+ (float-time) 2)))
    (while (and (monk-watch-pending monk-watch--current) (< (float-time) deadline))
      (sit-for 0.02))))

(ert-deftest monk-test-watch-index-and-lookup ()
  (monk-test-with-watch dir
      (lambda (_m)
        (monk-directory (concat dir "d"))
        (monk-file (concat dir "d/inner") :content "x")
        (monk-file (concat dir "f") :content "y"))
    (let ((w monk-watch--current))
      ;; Two parent directories: dir and dir/d.
      (should (= 2 (length (monk-watch-path-index w))))
      (should (equal (list (concat dir "f"))
                     (mapcar (lambda (n) (oref (monk-node-resource n) name))
                             (monk-watch--nodes-for-file w (concat dir "f")))))
      ;; A file inside a managed directory maps to the directory and, when
      ;; managed itself, to its own resource.
      (should (equal (list (concat dir "d") (concat dir "d/inner"))
                     (sort (mapcar (lambda (n) (oref (monk-node-resource n) name))
                                   (monk-watch--nodes-for-file w (concat dir "d/inner")))
                           #'string<)))
      (should-not (monk-watch--nodes-for-file w (concat dir "stranger"))))))

(ert-deftest monk-test-watch-assisted-plans-only-affected ()
  "Drift on one file yields a dry run restricted to that resource and dependents."
  (monk-test-with-watch dir
      (lambda (_m)
        (monk-file (concat dir "conf") :content "x")
        (monk-command "handler" "true" :watch (list (concat dir "conf")))
        (monk-file (concat dir "other") :content "y"))
    (let (runs)
      (add-hook 'monk-watch-functions (lambda (e d) (when (eq e 'run) (push (plist-get d :run) runs))))
      (with-temp-file (concat dir "conf") (insert "drifted"))
      (monk-test-fire (concat dir "conf"))
      (should (= 1 (length runs)))
      (let ((run (car runs)))
        (should (monk-run-dry-run run))
        (should (eq 'would-repair (monk-test-status run (concat dir "conf"))))
        (should (eq 'skipped (monk-test-status run (concat dir "other"))))
        (should (equal (list (cons "testbox" (concat dir "conf")))
                       (mapcar #'car (monk-run-reasons run))))
        (should (string-match-p "changed .*conf" (cdar (monk-run-reasons run))))
        (should (string-match-p "triggered by drift" (monk-run-report-string run))))
      ;; Nothing was repaired.
      (should (equal "drifted" (with-temp-buffer (insert-file-contents (concat dir "conf")) (buffer-string)))))))

(ert-deftest monk-test-watch-autopilot-repairs ()
  (monk-test-with-watch dir
      (lambda (_m) (monk-file (concat dir "conf") :content "x"))
    (setq monk-watch-mode 'autopilot)
    (with-temp-file (concat dir "conf") (insert "drifted"))
    (monk-test-fire (concat dir "conf"))
    (should (equal "x" (with-temp-buffer (insert-file-contents (concat dir "conf")) (buffer-string))))
    (should (= 1 (monk-watch-runs monk-watch--current)))))

(ert-deftest monk-test-watch-debounce-coalesces ()
  "A burst of events on several files makes one run with all keys."
  (monk-test-with-watch dir
      (lambda (_m)
        (monk-file (concat dir "a") :content "x")
        (monk-file (concat dir "b") :content "y"))
    (let (runs)
      (add-hook 'monk-watch-functions (lambda (e d) (when (eq e 'run) (push d runs))))
      (monk-watch--on-event (list 'desc 'changed (concat dir "a")))
      (monk-watch--on-event (list 'desc 'changed (concat dir "b")))
      (monk-watch--on-event (list 'desc 'changed (concat dir "a")))
      (monk-test-fire (concat dir "b"))
      (should (= 1 (length runs)))
      (should (= 2 (length (plist-get (car runs) :keys)))))))

(ert-deftest monk-test-watch-suppresses-own-writes ()
  "Events during a run and within the settle window are dropped."
  (monk-test-with-watch dir
      (lambda (_m) (monk-file (concat dir "conf") :content "x"))
    (let ((monk-watch--running t))
      (monk-watch--on-event (list 'desc 'changed (concat dir "conf")))
      (should-not (monk-watch-pending monk-watch--current)))
    (let ((monk-watch-settle 60))
      (setf (monk-watch-last-run-end monk-watch--current) (float-time))
      (monk-watch--on-event (list 'desc 'changed (concat dir "conf")))
      (should-not (monk-watch-pending monk-watch--current)))))

(ert-deftest monk-test-watch-poll-remote ()
  "Polling a remote machine turns would-repair into drift keys."
  (monk-test-with-tmpdir dir
    (let* ((monk-log-level nil)
           (monk-watch-settle 0.0)
           (monk-report-function #'ignore)
           (remote (monk-machine :name "far" :address "far.example"))
           (policy (lambda (_m) (monk-file (concat dir "conf") :content "x")))
           (probe (monk-run-create :dry-run t :results
                                   (list (monk-result-create :machine "far" :name (concat dir "conf")
                                                             :status 'would-repair)))))
      (oset remote facts monk-test-fake-facts)
      (oset remote classes (monk-hard-classes remote))
      (cl-letf (((symbol-function 'monk-plan) (lambda (&rest _) probe))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'cancel-timer) #'ignore)
                ((symbol-value 'monk-watch--delivery) 'no))
        (unwind-protect
            (progn
              (monk-watch-start remote policy)
              (should (null (monk-watch-path-index monk-watch--current)))
              (monk-watch--poll)
              (should (equal (list (cons "far" (concat dir "conf")))
                             (monk-watch-pending monk-watch--current)))
              (should (string-match-p "poll" (cdar (monk-watch-pending-reasons monk-watch--current)))))
          (monk-watch-stop))))))

(ert-deftest monk-test-watch-stop-clears ()
  (monk-test-with-watch dir (lambda (_m) (monk-file (concat dir "f") :content "x"))
    (should (monk-watch-active-p))
    (should (stringp (monk-watch-status)))
    (monk-watch-stop)
    (should-not (monk-watch-active-p))
    (should-not (monk-watch-status))))

(ert-deftest monk-test-watch-probe ()
  "The delivery probe returns a boolean, caches it, and cleans up after itself."
  (let ((before (directory-files temporary-file-directory nil "monk-probe"))
        (monk-watch--delivery nil)
        (monk-watch-probe-timeout 0.2))
    (should (memq (monk-watch-notifications-work-p) '(t nil)))
    (should (memq monk-watch--delivery '(yes no)))
    (should (equal before (directory-files temporary-file-directory nil "monk-probe"))))
  (let ((monk-watch--delivery 'no))
    (should-not (monk-watch-notifications-work-p)))
  (let ((monk-watch--delivery 'yes))
    (should (monk-watch-notifications-work-p))))

(ert-deftest monk-test-watch-live-inotify ()
  "With delivered notifications, a write to a managed file fires a run.
Skipped where the kernel delivers no events: the sandbox that runs
this suite reports inotify and delivers nothing."
  (skip-unless (let ((monk-watch--delivery nil)
                     (monk-watch-probe-timeout 0.3))
                 (monk-watch-notifications-work-p)))
  (monk-test-with-tmpdir dir
    (let ((monk-log-level nil)
          (monk-watch-debounce 0.05)
          (monk-watch-settle 0.0)
          (monk-watch-mode 'autopilot)
          (monk-report-function #'ignore)
          (m (monk-test-machine))
          (conf (concat dir "conf")))
      (with-temp-file conf (insert "x"))
      (unwind-protect
          (progn
            (monk-watch-start m (lambda (_m) (monk-file conf :content "x")))
            (should (= 1 (length (monk-watch-descriptors monk-watch--current))))
            (with-temp-file conf (insert "drifted"))
            (let ((deadline (+ (float-time) 3)))
              (while (and (< (monk-watch-runs monk-watch--current) 1) (< (float-time) deadline))
                (accept-process-output nil 0.05)
                (sit-for 0.02)))
            (should (= 1 (monk-watch-runs monk-watch--current)))
            (should (equal "x" (with-temp-buffer (insert-file-contents conf) (buffer-string)))))
        (monk-watch-stop)))))

;;;; Events and the agent

(require 'monk-event)
(require 'monk-agent)

(ert-deftest monk-test-event-result-round-trip ()
  "A result serializes to one JSON line and parses back to the same values."
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge
                 (lambda (_m)
                   (monk-file (concat dir "f") :content "x\n")
                   (monk-directory dir))))
           (events (monk-run-events run))
           (lines (mapcar #'monk-event-line events)))
      (should (= 4 (length events)))
      (should (equal '("run-start" "result" "result" "run-end")
                     (mapcar (lambda (e) (alist-get 'event e)) events)))
      (dolist (line lines)
        (should (string-suffix-p "\n" line))
        (should-not (string-match-p "\n." line))
        (should (monk-event-parse line)))
      (let* ((parsed (mapcar (lambda (l) (monk-event-parse (string-trim l))) lines))
             (back (monk-events-to-run parsed))
             (orig (monk-test-result run (concat dir "f")))
             (copy (monk-test-result back (concat dir "f"))))
        (should (equal '("testbox") (mapcar (lambda (m) (oref m name)) (monk-run-machines back))))
        (should (= 2 (length (monk-run-results back))))
        (should (eq 'repaired (monk-result-status copy)))
        (should (equal (monk-result-diff orig) (monk-result-diff copy)))
        (should (equal (monk-result-comment orig) (monk-result-comment copy)))
        (should (= 1.0 (monk-result-degree copy)))
        (should (equal '((create . t)) (monk-result-changes copy)))
        (should (equal (monk-run-passes run) (monk-run-passes back)))
        (should-not (monk-run-dry-run back))
        (should (string-match-p "\"status\":\"repaired\"" (nth 1 lines)))
        (should (string-match-p "\"changed\":true" (nth 1 lines)))
        (should (string-match-p "\"rgi\":1" (car lines)))))))

(ert-deftest monk-test-event-jsonable ()
  (should (equal "sym" (monk-event--jsonable 'sym)))
  (should (equal [1 "a" "(k . v)"] (monk-event--jsonable '(1 a (k . v)))))
  (should (equal "(k . v)" (monk-event--jsonable '(k . v))))
  (should (equal '((k . "v") (n . 2)) (monk-event--jsonable '((k . v) (n . 2)))))
  (should (equal '((create . t) (mode . [420 448]))
                 (monk-event--plist-to-alist '(:create t :mode (420 448)))))
  ;; Every event of a real run must serialize without error.
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge (lambda (_m) (monk-symlink (concat dir "l") "/t")))))
      (dolist (e (monk-run-events run))
        (should (stringp (monk-event--json e)))))))

(ert-deftest monk-test-event-run-end-fields ()
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge
                 (lambda (_m)
                   (monk-file (concat dir "f") :content "hello\n")
                   (monk-line-in-file (concat dir "f") "world" :create t))
                 :passes 5))
           (end (monk-event-run 'run-end run)))
      (should (eq t (alist-get 'ok end)))
      (should (eq :false (alist-get 'converged end)))
      (should (= 2 (length (alist-get 'flapping end))))
      ;; A parsed run-end serializes again: vectors, not lists, for arrays.
      (should (stringp (monk-event--json (monk-event-parse (monk-event--json end)))))
      (should (stringp (alist-get 'summary end))))))

(ert-deftest monk-test-agent-record-appends-ledger ()
  "The agent report function appends one line per event to the ledger."
  (monk-test-with-tmpdir dir
    (let* ((monk-agent-ledger-file (concat dir "state/events.ndjson"))
           (monk-log-level nil)
           (run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (monk-agent--report run)
        (monk-agent--report run))
      (let ((lines (with-temp-buffer
                     (insert-file-contents monk-agent-ledger-file)
                     (split-string (buffer-string) "\n" t))))
        (should (= 6 (length lines)))
        (should (cl-every #'monk-event-parse lines))))))

(ert-deftest monk-test-agent-observe-filter-splits-lines ()
  "Partial chunks are buffered until a newline completes the line."
  (let* ((seen nil)
         (monk-agent-observe-functions (list (lambda (e) (push (alist-get 'name e) seen))))
         (proc (start-process "monk-test-cat" nil "cat")))
    (unwind-protect
        (progn
          (monk-agent--observe-filter proc "{\"event\":\"result\",\"na")
          (should-not seen)
          (monk-agent--observe-filter proc "me\":\"a\"}\n{\"event\":\"result\",\"name\":\"b\"}\n{\"ev")
          (should (equal '("b" "a") seen))
          (monk-agent--observe-filter proc "ent\":\"result\",\"name\":\"c\"}\n")
          (should (equal '("c" "b" "a") seen)))
      (delete-process proc))))

(ert-deftest monk-test-agent-main-runs-and-stops ()
  "The agent converges on start, then leaves its loop when told to."
  (monk-test-with-tmpdir dir
    (let* ((monk-agent-ledger-file (concat dir "events.ndjson"))
           (monk-log-level nil)
           (monk-batch-policy (lambda (_m) (monk-directory (concat dir "d"))))
           (monk-agent-full-interval 3600)
           (monk-watch--delivery 'no))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function 'file-notify-add-watch) (lambda (&rest _) 'd))
                ((symbol-function 'file-notify-rm-watch) #'ignore)
                ((symbol-function 'file-notify-valid-p) (lambda (_) t))
                ((symbol-value 'file-notify--library) 'test)
                ;; Stop after the first wait.
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (monk-agent-stop) nil)))
        (monk-agent-main))
      (should (file-directory-p (concat dir "d")))
      (should (file-exists-p monk-agent-ledger-file))
      (should-not (monk-watch-active-p))
      (should-not monk-agent--full-timer))))

(ert-deftest monk-test-agent-units-text ()
  "Unit files carry the right sections, and the socket unit is inetd style."
  (let ((monk-agent-emacs "/usr/bin/emacs")
        (monk-agent-load-path "/opt/monk/lisp")
        (monk-agent-policy-file "/etc/monk/policy.el"))
    (let ((svc (monk-agent-service-unit)))
      (should (string-match-p "^ExecStart=/usr/bin/emacs --batch -L /opt/monk/lisp -l /etc/monk/policy.el -f monk-agent-main$" svc))
      (should (string-match-p "^Restart=on-failure$" svc))
      (should (string-match-p "^StateDirectory=monk$" svc))
      (should (string-match-p "^WantedBy=multi-user.target$" svc)))
    (let ((usvc (monk-agent-service-unit t)))
      (should-not (string-match-p "StateDirectory" usvc))
      (should (string-match-p "^WantedBy=default.target$" usvc)))
    (let ((sock (monk-agent-socket-unit)))
      (should (string-match-p "^ListenStream=/run/monk/agent.sock$" sock))
      (should (string-match-p "^Accept=yes$" sock)))
    (let ((tmpl (monk-agent-template-unit)))
      (should (string-match-p "^StandardInput=socket$" tmpl))
      (should (string-match-p "-f monk-agent-serve$" tmpl)))))

(ert-deftest monk-test-agent-units-policy-fragment ()
  "The policy fragment compiles to three resources with the socket watching its template."
  (let* ((m (monk-test-machine))
         (monk-agent-load-path "/opt/monk/lisp")
         (monk-agent-policy-file "/etc/monk/policy.el")
         (resources (monk-compile m (lambda (_m) (monk-agent-units)))))
    (should (equal '("monk-agent.service" "/etc/systemd/system/monk-agent@.service" "monk-agent.socket")
                   (mapcar (lambda (r) (oref r name)) resources)))
    (should (equal '("/etc/systemd/system/monk-agent@.service")
                   (oref (nth 2 resources) watch)))
    (should (eq 'daemon-reload (oref (nth 2 resources) watch-action)))
    (should (= 1 (length (monk-compile m (lambda (_m) (monk-agent-units :socket nil))))))))

(ert-deftest monk-test-agent-handle-status-and-tail ()
  (monk-test-with-tmpdir dir
    (let* ((monk-agent-ledger-file (concat dir "events.ndjson"))
           (monk-log-level nil)
           (run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (monk-agent-record run))
      (let* ((out (with-output-to-string (monk-agent-handle '((op . "status")))))
             (resp (monk-event-parse (string-trim out))))
        (should (eq t (alist-get 'ok resp)))
        (should (equal "status" (alist-get 'op resp)))
        (should (equal "run-end" (alist-get 'event (alist-get 'last_run resp))))
        (should (eq :false (alist-get 'agent_active resp))))
      (let* ((out (with-output-to-string (monk-agent-handle '((op . "tail") (n . 2)))))
             (lines (split-string out "\n" t)))
        ;; two ledger lines plus the ok line
        (should (= 3 (length lines)))
        (should (equal "tail" (alist-get 'op (monk-event-parse (car (last lines)))))))
      (let* ((out (with-output-to-string (monk-agent-handle '((op . "bogus")))))
             (resp (monk-event-parse (string-trim out))))
        (should (eq :false (alist-get 'ok resp)))
        (should (string-match-p "unknown op" (alist-get 'error resp)))))))

(ert-deftest monk-test-agent-handle-plan-streams-events ()
  "A plan request streams one result line per node and ends with run-end."
  (monk-test-with-tmpdir dir
    (let* ((monk-agent-ledger-file (concat dir "events.ndjson"))
           (monk-log-level nil)
           (monk-batch-policy (lambda (_m)
                                (monk-directory (concat dir "a"))
                                (monk-directory (concat dir "b"))))
           (fake (monk-test-machine))
           (out (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                          ((symbol-function 'monk-local-machine) (lambda (&rest _) fake)))
                  (with-output-to-string (monk-agent-handle '((op . "plan"))))))
           (events (mapcar #'monk-event-parse (split-string out "\n" t))))
      (should (equal '("result" "result" "run-end") (mapcar (lambda (e) (alist-get 'event e)) events)))
      (should (equal "would-repair" (alist-get 'status (car events))))
      (should-not (file-exists-p (concat dir "a")))
      ;; The controller side rebuilds a run from exactly these lines.
      (let ((run (monk-events-to-run events)))
        (should (monk-run-dry-run run))
        (should (= 2 (monk-run-count run 'would-repair)))))))

(ert-deftest monk-test-agent-query-parses-socat-output ()
  (let ((m (monk-test-machine)))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (should (equal "socat" program))
                 (should (member "UNIX-CONNECT:/run/monk/agent.sock" args))
                 (should (string-match-p "\"op\":\"status\"" (cadr (memq :input args))))
                 (monk-exec-create :exit-code 0
                                   :output "{\"op\":\"status\",\"ok\":true,\"host\":\"far\"}\n"))))
      (should (equal "far" (alist-get 'host (monk-agent-status m)))))))

;;;; etcd, against a fake gateway

(require 'monk-etcd)

(defvar monk-test-etcd--store nil "Alist of (KEY . VALUE) in the fake gateway.")
(defvar monk-test-etcd--leases 0)
(defvar monk-test-etcd--watchers nil "List of (PROC . PREFIX) streaming watchers.")

(defun monk-test-etcd--b64 (s) (base64-encode-string (encode-coding-string s 'utf-8) t))
(defun monk-test-etcd--unb64 (s) (decode-coding-string (base64-decode-string s) 'utf-8))

(defun monk-test-etcd--notify (kind key value)
  "Push a watch frame to every watcher whose prefix covers KEY."
  (dolist (w monk-test-etcd--watchers)
    (when (and (process-live-p (car w)) (string-prefix-p (cdr w) key))
      (process-send-string
       (car w)
       (concat (json-serialize
                `((result . ((events . [((type . ,(if (eq kind 'delete) "DELETE" "PUT"))
                                          (kv . ((key . ,(monk-test-etcd--b64 key))
                                                 ,@(and value `((value . ,(monk-test-etcd--b64 value)))))))])))))
               "\n")))))

(defun monk-test-etcd--handle (path body)
  "Return the JSON reply alist for PATH with BODY alist."
  (let* ((key (and (alist-get 'key body) (monk-test-etcd--unb64 (alist-get 'key body))))
         (end (and (alist-get 'range_end body) (monk-test-etcd--unb64 (alist-get 'range_end body)))))
    (pcase path
      ("/v3/kv/put"
       (let ((value (monk-test-etcd--unb64 (alist-get 'value body))))
         (setf (alist-get key monk-test-etcd--store nil nil #'equal) value)
         (monk-test-etcd--notify 'put key value)
         '((header . ((revision . "1"))))))
      ("/v3/kv/range"
       (let ((hits (seq-filter (lambda (c) (if end
                                               (and (string< (car c) end) (not (string< (car c) key)))
                                             (equal (car c) key)))
                               monk-test-etcd--store)))
         `((kvs . ,(vconcat (mapcar (lambda (c) `((key . ,(monk-test-etcd--b64 (car c)))
                                                  (value . ,(monk-test-etcd--b64 (cdr c)))))
                                    (sort (copy-sequence hits) (lambda (a b) (string< (car a) (car b))))))))))
      ("/v3/kv/deleterange"
       (let ((before (length monk-test-etcd--store)))
         (setq monk-test-etcd--store
               (seq-remove (lambda (c)
                             (let ((hit (if end (and (string< (car c) end) (not (string< (car c) key)))
                                          (equal (car c) key))))
                               (when hit (monk-test-etcd--notify 'delete (car c) nil))
                               hit))
                           monk-test-etcd--store))
         `((deleted . ,(- before (length monk-test-etcd--store))))))
      ("/v3/lease/grant" `((ID . ,(number-to-string (cl-incf monk-test-etcd--leases))) (TTL . ,(alist-get 'TTL body))))
      ("/v3/lease/keepalive" `((result . ((ID . ,(alist-get 'ID body)) (TTL . 30)))))
      (_ nil))))

(defun monk-test-etcd--filter (proc chunk)
  (let ((buf (concat (or (process-get proc 'partial) "") chunk)))
    (process-put proc 'partial buf)
    (when (string-match "\r\n\r\n" buf)
      (let* ((head (substring buf 0 (match-beginning 0)))
             (rest (substring buf (match-end 0)))
             (len (and (string-match "Content-Length: \\([0-9]+\\)" head)
                       (string-to-number (match-string 1 head))))
             (path (and (string-match "^POST \\([^ ]+\\)" head) (match-string 1 head))))
        (when (and len (>= (string-bytes rest) len))
          (process-put proc 'partial "")
          (let ((body (json-parse-string rest :object-type 'alist :array-type 'list
                                         :null-object nil :false-object :false)))
            (if (equal path "/v3/watch")
                (progn
                  (push (cons proc (monk-test-etcd--unb64
                                    (alist-get 'key (alist-get 'create_request body))))
                        monk-test-etcd--watchers)
                  (process-send-string proc "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
                  (process-send-string proc "{\"result\":{\"created\":true}}\n"))
              (let* ((reply (monk-test-etcd--handle path body))
                     (text (if reply (json-serialize reply :null-object nil :false-object :false)
                             "{\"message\":\"no such route\"}")))
                (process-send-string
                 proc (format "HTTP/1.1 %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                              (if reply "200 OK" "404 Not Found") (string-bytes text) text))
                (delete-process proc)))))))))

(defmacro monk-test-with-fake-etcd (&rest body)
  "Run BODY with `monk-etcd-url' pointing at a fake gateway on a free port."
  (declare (indent 0))
  `(let* ((monk-test-etcd--store nil)
          (monk-test-etcd--watchers nil)
          (monk-log-level nil)
          (server (make-network-process :name "monk-test-etcd" :server t :host "127.0.0.1"
                                        :service t :family 'ipv4 :noquery t
                                        :filter #'monk-test-etcd--filter))
          (monk-etcd-url (format "http://127.0.0.1:%d" (process-contact server :service))))
     (unwind-protect
         (progn ,@body)
       (dolist (w monk-test-etcd--watchers) (when (process-live-p (car w)) (delete-process (car w))))
       (delete-process server))))

(ert-deftest monk-test-etcd-prefix-end ()
  (should (equal "/_monk/events0" (monk-etcd--prefix-end "/_monk/events/")))
  (should (equal "b" (monk-etcd--prefix-end "a"))))

(ert-deftest monk-test-etcd-strings-mgmt-layout ()
  (monk-test-with-fake-etcd
    (monk-etcd-str-set "greeting" "hi")
    (should (equal "hi" (monk-etcd-str-get "greeting")))
    (should (equal "hi" (cdr (assoc "/_mgmt/strings/greeting" monk-test-etcd--store))))
    (monk-etcd-strmap-set "load" "a" "0.5")
    (monk-etcd-strmap-set "load" "b" "0.9")
    (should (equal '(("a" . "0.5") ("b" . "0.9")) (monk-etcd-strmap-get "load")))
    (should (equal "0.9" (cdr (assoc "/_mgmt/strings/load/b" monk-test-etcd--store))))
    (should-not (monk-etcd-get "/nothing"))
    (should (= 1 (monk-etcd-delete "/_mgmt/strings/greeting")))
    (should-not (monk-etcd-str-get "greeting"))))

(ert-deftest monk-test-etcd-export-collect ()
  (monk-test-with-fake-etcd
    (let* ((m (monk-test-machine))
           (resources (monk-compile m (lambda (_m)
                                        (monk-file "/etc/hosts.d/a" :content "1.2.3.4 a\n" :mode #o644)
                                        (monk-symlink "/x" "/y" :sudo t)))))
      (monk-etcd-export "a" monk-etcd-any-host (car resources))
      (monk-etcd-export "a" "b" (cadr resources))
      (let ((for-b (monk-etcd-collect "b"))
            (for-c (monk-etcd-collect "c")))
        (should (= 2 (length for-b)))
        (should (= 1 (length for-c)))
        (let ((file (seq-find (lambda (r) (equal (alist-get 'type r) "file")) for-b)))
          (should (equal "a" (alist-get 'from file)))
          (should (equal "/etc/hosts.d/a" (alist-get 'name file)))
          (should (equal "1.2.3.4 a\n" (alist-get 'content (alist-get 'params file))))
          (should (= 420 (alist-get 'mode (alist-get 'params file))))
          (should (eq :false (alist-get 'sudo (alist-get 'meta file)))))
        (should (equal '("symlink") (mapcar (lambda (r) (alist-get 'type r))
                                            (monk-etcd-collect "b" "symlink"))))))))

(ert-deftest monk-test-etcd-events-and-presence ()
  (monk-test-with-fake-etcd
    (monk-test-with-tmpdir dir
      (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))))
        (should (= 3 (monk-etcd-publish-run run)))
        (should (equal (list (monk-run-id run)) (monk-etcd-runs "testbox")))
        (let ((back (monk-etcd-run "testbox" (monk-run-id run))))
          (should (= 1 (monk-run-count back 'repaired))))
        (let ((lease (monk-etcd-announce "testbox" '((mode . "standalone")))))
          (should (numberp lease))
          (should (= 30 (monk-etcd-keepalive lease)))
          (should (equal "standalone"
                         (alist-get 'mode (cdr (assoc "testbox" (monk-etcd-agents)))))))))))

(ert-deftest monk-test-etcd-watch-stream ()
  "A watch delivers puts and deletes under its prefix and nothing outside it."
  (monk-test-with-fake-etcd
    (let* ((seen nil)
           (proc (monk-etcd-watch "/_monk/events/" (lambda (kind key value) (push (list kind key value) seen)))))
      (unwind-protect
          (progn
            (let ((deadline (+ (float-time) 2)))
              (while (and (null monk-test-etcd--watchers) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should monk-test-etcd--watchers)
            (monk-etcd-put "/_monk/events/h/r/000001" "{\"event\":\"result\"}")
            (monk-etcd-put "/_mgmt/strings/other" "x")
            (monk-etcd-delete "/_monk/events/h/r/000001")
            (let ((deadline (+ (float-time) 2)))
              (while (and (< (length seen) 2) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (equal '((put "/_monk/events/h/r/000001" "{\"event\":\"result\"}")
                             (delete "/_monk/events/h/r/000001" nil))
                           (reverse seen))))
        (delete-process proc)))))

(ert-deftest monk-test-etcd-http-error-signals ()
  (monk-test-with-fake-etcd
    (should-error (monk-etcd--request 'bogus '((x . 1))) :type 'monk-error)))

;;;; Test drive and tutorial

(require 'monk-testdrive)
(require 'monk-tutorial)

(ert-deftest monk-test-testdrive-step-records-outcomes ()
  "The step macro records PASS, FAIL with note, SKIP, and turns errors into FAIL."
  (let ((monk-testdrive--results nil)
        (monk-testdrive--layout nil))
    (with-current-buffer (monk-testdrive--buf)
      (let ((inhibit-read-only t)) (erase-buffer)))
    (monk-testdrive--step "s" "passes" 'PASS)
    (monk-testdrive--step "s" "fails" (cons 'FAIL "because"))
    (monk-testdrive--step "s" "skips" 'SKIP)
    (monk-testdrive--step "s" "errors" (error "boom"))
    (should (equal '(PASS FAIL SKIP FAIL)
                   (mapcar #'caddr (reverse monk-testdrive--results))))
    (should (equal "because" (nth 3 (nth 2 monk-testdrive--results))))
    (should (string-match-p "error: boom" (nth 3 (car monk-testdrive--results))))
    (with-current-buffer (monk-testdrive--buf)
      (should (string-match-p "^FAIL fails" (buffer-string)))
      (goto-char (point-min))
      (re-search-forward "^FAIL")
      (should (eq 'monk-testdrive-fail (get-text-property (match-beginning 0) 'face))))))

(ert-deftest monk-test-testdrive-checks-helper ()
  (should (eq 'PASS (monk-testdrive--checks '(t . "a") '(t . "b"))))
  (should (equal '(FAIL . "b") (monk-testdrive--checks '(t . "a") '(nil . "b") '(nil . "c")))))

(ert-deftest monk-test-testdrive-full-run-in-batch ()
  "The whole drive runs headless, cleans up its home, and reports no FAIL.
The live-notification step may SKIP where the kernel delivers nothing."
  (let ((home nil)
        (monk-report-function #'ignore))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (monk-testdrive))
    (setq home monk-testdrive--home)
    (should (null home))
    (let ((fails (seq-filter (lambda (r) (eq (caddr r) 'FAIL)) monk-testdrive--results)))
      (should (null fails)))
    (should (>= (cl-count 'PASS monk-testdrive--results :key #'caddr) 18))
    (with-current-buffer (monk-testdrive--buf)
      (should (string-match-p "\\`Monk test drive\n" (buffer-string)))
      (should (string-match-p "^[0-9]+ PASS, 0 FAIL" (buffer-string))))))

(ert-deftest monk-test-tutorial-forms-parse-and-run ()
  "Every lesson form reads and evaluates against a fresh sandbox."
  (let ((forms (monk-tutorial-forms)))
    (should (> (length forms) 20))
    (should (null (monk-tutorial-check)))
    (should (null monk-tutorial--home))))

(ert-deftest monk-test-tutorial-lessons-match-doc ()
  "docs/tutorial.md has the same ten lesson headings as the buffer."
  (let* ((doc (expand-file-name "../docs/tutorial.md"
                                (file-name-directory (locate-library "monk-tutorial"))))
         (headings (lambda (text prefix)
                     (let (out (start 0))
                       (while (string-match (concat "^" prefix "\\([0-9]+\\)\\. \\(.*\\)$") text start)
                         (push (cons (string-to-number (match-string 1 text))
                                     (string-trim (match-string 2 text)))
                               out)
                         (setq start (match-end 0)))
                       (nreverse out)))))
    (skip-unless (file-exists-p doc))
    (should (equal (funcall headings monk-tutorial--text ";;; \\* ")
                   (funcall headings (with-temp-buffer (insert-file-contents doc) (buffer-string))
                            "## ")))))

;;;; Reload

(require 'monk-reload)

(ert-deftest monk-test-reload-features-order-and-filter ()
  "Monk features come back oldest first; the reloader itself is left out."
  (let ((feats (monk-reload-features)))
    (should (memq 'monk feats))
    (should (memq 'monk-ui feats))
    (should-not (memq 'monk-reload feats))
    (should-not (memq 'monk-test feats))
    (should (< (cl-position 'monk feats) (cl-position 'monk-ui feats)))
    (should (cl-every (lambda (f) (string-prefix-p "monk" (symbol-name f))) feats))))

(ert-deftest monk-test-reload-runs-and-rekeys ()
  "A reload loads every feature, rebuilds keymaps, and re-installs them in live buffers."
  (let* ((old-map monk-ui-mode-map)
         (buf (generate-new-buffer "*monk-reload-test*"))
         (monk-log-level nil)
         (monk-watch--delivery 'no))
    (unwind-protect
        (progn
          (with-current-buffer buf (monk-ui-mode))
          (let ((n (let ((inhibit-message t)) (monk-reload))))
            (should (null monk-reload-last-failures))
            (should (= n (length (monk-reload-features)))))
          (should-not (eq old-map monk-ui-mode-map))
          (with-current-buffer buf
            (should (eq (current-local-map) monk-ui-mode-map)))
          (should (null monk-watch--delivery)))
      (kill-buffer buf))))

(ert-deftest monk-test-reload-restarts-watch ()
  "An active watch survives a reload with the same policy and mode."
  (monk-test-with-watch dir (lambda (_m) (monk-file (concat dir "f") :content "x"))
    (setq monk-watch-mode 'autopilot)
    (let ((policy (monk-watch-policy monk-watch--current))
          (inhibit-message t))
      (monk-reload)
      (should (monk-watch-active-p))
      (should (eq policy (monk-watch-policy monk-watch--current)))
      (should (eq 'autopilot monk-watch-mode)))))

;;;; kqueue-style watching

(ert-deftest monk-test-watch-kqueue-adds-file-watches ()
  "Where a directory watch misses writes, existing owned files are watched too,
and a file that appears later is adopted on its directory event."
  (monk-test-with-tmpdir dir
    (with-temp-file (concat dir "present") (insert "x"))
    (let ((monk-log-level nil)
          (monk-watch-debounce 0.05)
          (monk-watch-settle 0.0)
          (monk-report-function #'ignore)
          (added nil)
          (m (monk-test-machine)))
      (unwind-protect
          (cl-letf (((symbol-function 'file-notify-add-watch)
                     (lambda (path &rest _) (push path added) (cl-gensym "desc")))
                    ((symbol-function 'file-notify-rm-watch) #'ignore)
                    ((symbol-function 'file-notify-valid-p) (lambda (_) t))
                    ((symbol-value 'file-notify--library) 'kqueue)
                    ((symbol-value 'monk-watch--delivery) 'yes))
            (monk-watch-start m (lambda (_m)
                                  (monk-file (concat dir "present") :content "x")
                                  (monk-file (concat dir "later") :content "y")))
            ;; The directory and the existing file; not the absent one.
            (should (member (directory-file-name dir) (mapcar #'directory-file-name added)))
            (should (member (concat dir "present") added))
            (should-not (member (concat dir "later") added))
            ;; The absent file appears: its create event on the directory adopts it.
            (with-temp-file (concat dir "later") (insert "y"))
            (monk-watch--on-event (list 'desc 'created (concat dir "later")))
            (should (member (concat dir "later") added))
            (should (assoc (concat dir "later") (monk-watch-watched monk-watch--current))))
        (monk-watch-stop)))))

(ert-deftest monk-test-watch-inotify-does-not-add-file-watches ()
  (monk-test-with-tmpdir dir
    (with-temp-file (concat dir "present") (insert "x"))
    (let ((monk-log-level nil) (added nil) (m (monk-test-machine)))
      (unwind-protect
          (cl-letf (((symbol-function 'file-notify-add-watch)
                     (lambda (path &rest _) (push path added) (cl-gensym "desc")))
                    ((symbol-function 'file-notify-rm-watch) #'ignore)
                    ((symbol-function 'file-notify-valid-p) (lambda (_) t))
                    ((symbol-value 'file-notify--library) 'inotify)
                    ((symbol-value 'monk-watch--delivery) 'yes))
            (monk-watch-start m (lambda (_m) (monk-file (concat dir "present") :content "x")))
            (should (= 1 (length added)))
            (should-not (member (concat dir "present") added)))
        (monk-watch-stop)))))

;;;; fswatch backend

(ert-deftest monk-test-watch-choose-backend ()
  (cl-letf (((symbol-value 'file-notify--library) 'inotify))
    (let ((monk-watch-backend 'auto) (system-type 'gnu/linux))
      (should (eq 'filenotify (monk-watch--choose-backend))))
    (let ((monk-watch-backend 'auto) (system-type 'darwin))
      (cl-letf (((symbol-function 'executable-find) (lambda (p &rest _) (equal p "fswatch"))))
        (should (eq 'fswatch (monk-watch--choose-backend))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (should (eq 'filenotify (monk-watch--choose-backend)))))
    (let ((monk-watch-backend nil))
      (should-not (monk-watch--choose-backend)))
    (let ((monk-watch-backend 'fswatch))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (should-error (monk-watch--choose-backend) :type 'monk-error))))
  (cl-letf (((symbol-value 'file-notify--library) nil))
    (let ((monk-watch-backend 'auto) (system-type 'gnu/linux))
      (should-not (monk-watch--choose-backend)))
    (let ((monk-watch-backend 'filenotify))
      (should-error (monk-watch--choose-backend) :type 'monk-error))))

(ert-deftest monk-test-watch-fswatch-filter-splits-on-nul ()
  "Paths arrive NUL-separated, possibly split across chunks."
  (let* ((seen nil)
         (proc (start-process "monk-test-cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'monk-watch--on-event)
                   (lambda (e) (push (nth 2 e) seen))))
          (monk-watch--fswatch-filter proc "/a/one\0/a/tw")
          (should (equal '("/a/one") seen))
          (monk-watch--fswatch-filter proc "o\0\0/a/three\0")
          (should (equal '("/a/three" "/a/two" "/a/one") seen)))
      (delete-process proc))))

(ert-deftest monk-test-watch-fswatch-end-to-end ()
  "A fake fswatch that prints one path fires an autopilot run through the real process filter."
  (monk-test-with-tmpdir dir
    (let* ((bin (concat dir "bin/"))
           (conf (concat dir "conf"))
           (fake (concat bin "fswatch"))
           (monk-log-level nil)
           (monk-watch-debounce 0.05)
           (monk-watch-settle 0.0)
           (monk-watch-mode 'autopilot)
           (monk-watch-backend 'fswatch)
           (monk-report-function #'ignore)
           (m (monk-test-machine)))
      (make-directory bin)
      (with-temp-file conf (insert "x"))
      ;; The fake ignores its arguments, waits a moment, prints the
      ;; conf path NUL-terminated, then stays alive like fswatch does.
      (with-temp-file fake
        (insert "#!/bin/sh\nsleep 0.2\nprintf '%s\\0' " (shell-quote-argument conf) "\nsleep 30\n"))
      (set-file-modes fake #o755)
      (let ((exec-path (cons (directory-file-name bin) exec-path)))
        (unwind-protect
            (progn
              (monk-watch-start m (lambda (_m) (monk-file conf :content "x")))
              (should (eq 'fswatch (monk-watch-backend monk-watch--current)))
              (should (process-live-p (monk-watch-process monk-watch--current)))
              (with-temp-file conf (insert "drifted"))
              (let ((deadline (+ (float-time) 5)))
                (while (and (< (monk-watch-runs monk-watch--current) 1) (< (float-time) deadline))
                  (accept-process-output nil 0.05) (sit-for 0.02)))
              (should (= 1 (monk-watch-runs monk-watch--current)))
              (should (equal "x" (with-temp-buffer (insert-file-contents conf) (buffer-string)))))
          (let ((proc (and monk-watch--current (monk-watch-process monk-watch--current))))
            (monk-watch-stop)
            (should-not (and proc (process-live-p proc)))))))))

(ert-deftest monk-test-testdrive-focus-from-side-window ()
  "The tour can take the frame even when a side window is selected.
That is where the report leaves point, and `delete-other-windows'
refuses to run from a side window."
  ;; Split the main area first: `window-main-window' is then a parent,
  ;; which is what broke the tour on a real frame.
  (split-window-right)
  (let ((side (display-buffer (get-buffer-create "*monk-side-test*")
                              '(display-buffer-in-side-window (side . right)))))
    (unwind-protect
        (progn
          (select-window side)
          (should-not (window-live-p (window-main-window)))
          (should (window-parameter (selected-window) 'window-side))
          (monk-testdrive--focus-monk)
          (should (= 1 (length (window-list))))
          (should (equal monk-ui-buffer-name (buffer-name (window-buffer (selected-window))))))
      (kill-buffer "*monk-side-test*"))))

(ert-deftest monk-test-describe-changes ()
  (should (equal "unset → 512" (monk-describe-changes '(:before unset :after "512"))))
  (should (equal "0 → 1" (monk-describe-changes '(:before "0" :after "1"))))
  (should (equal "create → /t" (monk-describe-changes '(:create "/t"))))
  (should (equal "create" (monk-describe-changes '(:create t))))
  (should (equal "/old → /new" (monk-describe-changes '(:retarget ("/old" "/new")))))
  (should (equal "mode 0755 → 0700" (monk-describe-changes '(:mode (33261 448)))))
  (should (equal "install 2: a b" (monk-describe-changes '(:install ("a" "b") :degree 0.5))))
  (should (equal "content differs, mode 0644 → 0600"
                 (monk-describe-changes '(:content t :mode (420 384)))))
  (should (equal "clone u (main)" (monk-describe-changes '(:clone ("u" "main")))))
  (should (equal "crontab yes" (monk-describe-changes '(:crontab t)))))

(ert-deftest monk-test-osx-default-reports-before-after ()
  (let ((m (monk-test-machine))
        (r (monk-osx-default-resource :name "d:k" :domain "d" :key "k" :value 512 :value-type 'int)))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 1 :output "does not exist"))))
      (should (equal '(:before unset :after "512") (monk-check r m))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 0 :output "256\n"))))
      (should (equal '(:before "256" :after "512") (monk-check r m))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 0 :output "512\n"))))
      (should-not (monk-check r m)))))

(provide 'monk-test)
;;; monk-test.el ends here
