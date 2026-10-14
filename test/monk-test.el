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

(ert-deftest monk-test-exec-yields-and-times-out ()
  "A command runs as a process Monk waits on in steps: the yield hook
and timers run meanwhile, stderr is captured, and a command past its
timeout is killed with exit 124 and a note."
  (let* ((m (monk-local-machine))
         (yields 0)
         (timer-fired nil)
         (monk-exec-yield-functions (list (lambda () (cl-incf yields)))))
    ;; a timer fires during the command: the agent's socket would too
    (run-at-time 0.2 nil (lambda () (setq timer-fired t)))
    (let ((exec (monk-exec m "sh" "-c" "echo out; echo err >&2; sleep 0.6; exit 2")))
      (should (eql 2 (monk-exec-exit-code exec)))
      (should (string-match-p "out" (monk-exec-output exec)))
      (should (string-match-p "err" (monk-exec-output exec)))
      (should (> yields 2))
      (should timer-fired))
    ;; the limit
    (let* ((start (float-time))
           (exec (monk-exec m "sleep" "30" :timeout 1)))
      (should (eql monk-exec-timeout-exit (monk-exec-exit-code exec)))
      (should (string-match-p "stopped after 1s" (monk-exec-output exec)))
      (should (< (- (float-time) start) 5)))
    ;; the default limit is on, so a resource cannot hang a converge
    (should (natnump monk-exec-timeout))
    ;; a nap: one wait returns far later than asked; the deadline moves
    ;; out by the nap and the output says the machine slept
    (let ((monk-exec-sleep-gap-seconds 0.5) (naps 0))
      (cl-letf* ((real-accept (symbol-function 'accept-process-output))
                 ((symbol-function 'accept-process-output)
                  (lambda (&rest args)
                    (when (and (= naps 0) (processp (car args)))
                      (cl-incf naps)
                      ;; pretend the machine was away for 0.8 s
                      (sleep-for 0.8))
                    (apply real-accept args))))
        (let ((exec (monk-exec m "sh" "-c" "sleep 1.2; exit 0" :timeout 1)))
          ;; without the nap credit this would have hit the 1 s limit
          (should (monk-exec-ok-p exec))
          (should (monk-exec-slept-p exec))
          (should-not (monk-exec-timed-out-p exec))))
      ;; a command that fails across a nap runs once more; the second
      ;; attempt's output and exit stand, with a note
      (setq naps 0)
      (monk-test-with-tmpdir dir
        (let ((flag (concat dir "second")))
          (cl-letf* ((real-accept (symbol-function 'accept-process-output))
                     ((symbol-function 'accept-process-output)
                      (lambda (&rest args)
                        (when (and (= naps 0) (processp (car args)))
                          (cl-incf naps)
                          (sleep-for 0.8))
                        (apply real-accept args))))
            ;; first run: no flag yet -> exit 1 (across the nap); second run: flag -> ok
            (let ((exec (monk-exec m "sh" "-c" (format "if [ -e %s ]; then echo second; exit 0; else touch %s; sleep 0.3; exit 1; fi" flag flag))))
              (should (monk-exec-ok-p exec))
              (should (string-match-p "second" (monk-exec-output exec)))
              (should (string-match-p (regexp-quote monk-exec-retried-note) (monk-exec-output exec)))))
          ;; with the retry off, the first attempt is what you get
          (delete-file flag)
          (setq naps 0)
          (cl-letf* ((real-accept (symbol-function 'accept-process-output))
                     ((symbol-function 'accept-process-output)
                      (lambda (&rest args)
                        (when (and (= naps 0) (processp (car args)))
                          (cl-incf naps)
                          (sleep-for 0.8))
                        (apply real-accept args)))
                     (monk-exec-retry-after-sleep nil))
            (let ((exec (monk-exec m "sh" "-c" (format "if [ -e %s ]; then exit 0; else touch %s; sleep 0.3; exit 1; fi" flag flag))))
              (should (eql 1 (monk-exec-exit-code exec)))
              (should (monk-exec-slept-p exec)))))))
    (should (monk-exec-timed-out-p (monk-exec m "sleep" "5" :timeout 1)))
    ;; input still works through the pipe
    (should (equal "piped" (monk-exec-output (monk-exec m "cat" :input "piped"))))
    ;; a child that exits at once, before stdin is closed on it, is not
    ;; a failure: `defaults read' did this on a Mac
    (dotimes (_ 20)
      (let ((exec (monk-exec m "true")))
        (should (monk-exec-ok-p exec))))
    (dotimes (_ 20)
      (should (eql 3 (monk-exec-exit-code (monk-exec m "sh" "-c" "exit 3" :input "ignored")))))))

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
      (should (string-match-p "\\+x" (monk-result-diff-text (monk-test-result run (concat dir "f"))))))))

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

(ert-deftest monk-test-failure-does-not-stop-the-pass ()
  "A failed resource is a result; unrelated resources still run.
This is the Salt execution model.  Only dependents skip."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x")
                  (monk-directory (concat dir "after"))
                  (monk-directory (concat dir "dependent")
                                  :require (list (concat dir "missing#x")))))))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x"))))
      (should (eq 'repaired (monk-test-status run (concat dir "after"))))
      (should (eq 'skipped (monk-test-status run (concat dir "dependent"))))
      (should (string-match-p "require" (monk-result-comment
                                         (monk-test-result run (concat dir "dependent")))))
      (should-not (monk-run-ok-p run))
      (should-not (file-directory-p (concat dir "dependent"))))))

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
                  (monk-line-in-file (concat dir "missing") "x")
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
                     (monk-line-in-file (concat dir "missing") "x")
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
        (should (string-match-p "^\\+two$" (monk-result-diff-text result)))
        (should (equal "one\ntwo\n" (with-temp-buffer (insert-file-contents dst) (buffer-string))))))))

(ert-deftest monk-test-file-content-error-signals-when-neither ()
  "A file with neither content nor source is refused when the policy compiles."
  (monk-test-with-tmpdir dir
    (let ((err (should-error (monk-test-converge (lambda (_m) (monk-file (concat dir "x"))))
                             :type 'monk-argument-error)))
      (should (string-match-p "one of :content, :source is required" (cadr err))))))

(ert-deftest monk-test-declared-parameters ()
  "Constructors validate their keywords: unknown, type, choices, required, exclusive."
  (let ((m (monk-test-machine)))
    ;; unknown keyword, Ansible's words
    (let ((err (should-error (monk-compile m (lambda (_m) (monk-directory "~/d" :mdoe #o700)))
                             :type 'monk-argument-error)))
      (should (string-match-p "Unsupported parameter for monk-directory: :mdoe\\.  Supported parameters include: :group, :mode, :owner" (cadr err))))
    ;; type
    (should-error (monk-compile m (lambda (_m) (monk-directory "~/d" :mode "0700"))) :type 'monk-argument-error)
    (should-error (monk-compile m (lambda (_m) (monk-directory 7))) :type 'monk-argument-error)
    ;; choices
    (should-error (monk-compile m (lambda (_m) (monk-line-in-file "~/f" "x" :state 'gone))) :type 'monk-argument-error)
    ;; exclusive
    (should-error (monk-compile m (lambda (_m) (monk-file "~/f" :content "a" :source "/b"))) :type 'monk-argument-error)
    ;; an explicit nil for an optional typed parameter is "not given"
    (should (equal "master" (or (oref (car (monk-compile m (lambda (_m) (monk-git "~/r" "u" :branch nil)))) branch)
                                monk-git-default-branch)))
    ;; defaults fill in; common keys pass; docstring is generated
    (let ((r (car (monk-compile m (lambda (_m) (monk-symlink "~/l" "~/t" :classes '(x)))))))
      (should (eq t (oref r force)))
      (should (equal '(x) (oref r classes))))
    (should (string-match-p ":force BOOLEAN (default t)" (documentation 'monk-symlink)))
    (should (assq 'monk-file monk-resource-specs))))

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
                    (monk-command "bare" "true")))))
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

(defun monk-test-brew-info-json (formulae &optional casks)
  "Return brew info --json=v2 text.
FORMULAE are (NAME FULL ALIASES), CASKS are (TOKEN FULL OLD-TOKENS)."
  (json-serialize
   (list :formulae
         (vconcat (mapcar (lambda (f)
                            (list :name (nth 0 f) :full_name (nth 1 f)
                                  :aliases (vconcat (nth 2 f)) :oldnames []))
                          formulae))
         :casks
         (vconcat (mapcar (lambda (c)
                            (list :token (nth 0 c) :full_token (nth 1 c)
                                  :old_tokens (vconcat (nth 2 c))))
                          casks)))))

(defun monk-test-brew-fake-info (formulae &optional casks)
  "Return a fake `monk-exec' that answers brew info --installed from FORMULAE and CASKS."
  (lambda (_m _p &rest args)
    (should (equal '("info" "--json=v2" "--installed") args))
    (monk-exec-create :command args :exit-code 0
                      :output (monk-test-brew-info-json formulae casks))))

(ert-deftest monk-test-brew-check-handles-tapped-names ()
  (let ((m (monk-test-machine))
        (r (monk-brew-resource :name "b" :packages '("jq" "DimitarPetrov/stegify/stegify"))))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-brew-fake-info '(("jq" "jq" ()) ("stegify" "dimitarpetrov/stegify/stegify" ())))))
      (should-not (monk-check r m)))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-brew-fake-info '(("jq" "jq" ())))))
      (should (equal '(:install ("DimitarPetrov/stegify/stegify") :degree 0.5)
                     (monk-check r m))))))

(ert-deftest monk-test-brew-check-resolves-aliases-and-casks ()
  "rg is ripgrep, gpg1 is gnupg@1.4, swiftbar is a cask: none is missing.
Found live: all three installed on every pass and the run flapped."
  (let ((m (monk-test-machine))
        (r (monk-brew-resource :name "b" :packages '("rg" "gpg1" "swiftbar" "jq" "gone"))))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-brew-fake-info '(("ripgrep" "ripgrep" ("rg"))
                                           ("gnupg@1.4" "gnupg@1.4" ("gpg1"))
                                           ("jq" "jq" ()))
                                         '(("swiftbar" "swiftbar" ())))))
      (let ((changes (monk-check r m)))
        (should (equal '("gone") (plist-get changes :install)))
        (should (< (abs (- (plist-get changes :degree) 0.8)) 1e-9))))
    ;; the cask resource reads the same list
    (let ((c (monk-brew-resource :name "c" :packages '("swiftbar" "nocask") :cask t)))
      (cl-letf (((symbol-function 'monk-exec)
                 (monk-test-brew-fake-info nil '(("swiftbar" "swiftbar" ())))))
        (should (equal '("nocask") (plist-get (monk-check c m) :install)))))))

(ert-deftest monk-test-brew-check-falls-back-to-list ()
  "When brew info fails, brew list decides, without aliases."
  (let ((m (monk-test-machine))
        (r (monk-brew-resource :name "b" :packages '("rg" "jq")))
        (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m _p &rest args)
                 (monk-exec-create :command args :exit-code 1 :output "Error: boom")))
              ((symbol-function 'monk-exec-lines)
               (lambda (_m _p &rest args)
                 (if (member "--formula" args) '("ripgrep" "jq") nil))))
      (should (equal '("rg") (plist-get (monk-check r m) :install))))))

(ert-deftest monk-test-fact-probe-order ()
  "A new probe runs after the existing ones; re-registering keeps the slot."
  (let ((monk-fact-probes (copy-alist monk-fact-probes)))
    (monk-register-fact-probe 'zzz (lambda (_m) '((z . 1))))
    (should (eq 'zzz (car (car (last monk-fact-probes)))))
    (should (eq 'uname (car (car monk-fact-probes))))
    (monk-register-fact-probe 'uname #'ignore)
    (should (eq 'uname (car (car monk-fact-probes))))
    (should (eq #'ignore (alist-get 'uname monk-fact-probes)))))

(ert-deftest monk-test-ui-rerun-repeats-a-plan ()
  "g in a plan buffer plans again and shows the new result."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m) (monk-directory (concat dir "d"))))
           (m (monk-test-machine))
           (monk-log-level nil)
           (plan (monk-plan m policy))
           (shown nil))
      (should (= 1 (monk-run-count plan monk-status-would-repair)))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run plan)
        (cl-letf (((symbol-function 'monk-run-report) (lambda (run &rest _) (setq shown run))))
          (make-directory (concat dir "d"))
          (monk-ui-rerun))
        (should shown)
        (should (monk-run-dry-run shown))
        (should (= 0 (monk-run-count shown monk-status-would-repair)))
        (should (= 1 (monk-run-count shown monk-status-kept)))))))

(defconst monk-test-brew-untrusted-output
  "Error: Refusing to load formula dimitarpetrov/stegify/stegify from untrusted tap dimitarpetrov/stegify.
Run `brew trust --formula dimitarpetrov/stegify/stegify` or `brew trust dimitarpetrov/stegify` to trust it."
  "What brew install prints for a tap that was never trusted.")

(defun monk-test-fake-brew (script)
  "Return a fake `monk-exec' that answers from SCRIPT and logs calls.
SCRIPT maps the first brew argument to a list of (EXIT-CODE OUTPUT)
answers, consumed in order.  The log is stored in the symbol property
`calls' of `monk-test-fake-brew'."
  (put 'monk-test-fake-brew 'calls nil)
  (lambda (_m program &rest args)
    (push (cons program args) (get 'monk-test-fake-brew 'calls))
    (let* ((answers (assoc (car args) script))
           (answer (or (cadr answers) '(0 ""))))
      (when (cddr answers) (setcdr answers (cddr answers)))
      (monk-exec-create :command (cons program args)
                        :exit-code (car answer) :output (cadr answer)))))

(ert-deftest monk-test-brew-install-trusts-tap-and-retries ()
  "An untrusted-tap refusal runs brew trust once, then installs."
  (let* ((m (monk-test-machine))
         (r (monk-brew-resource :name "b" :packages '("DimitarPetrov/stegify/stegify")))
         (script (list (list "install" (list 1 monk-test-brew-untrusted-output) '(0 "ok"))
                       (list "trust" '(0 "")))))
    (cl-letf (((symbol-function 'monk-exec) (monk-test-fake-brew script)))
      (should (equal "installed DimitarPetrov/stegify/stegify (after brew trust dimitarpetrov/stegify)"
                     (monk-repair r m '(:install ("DimitarPetrov/stegify/stegify")))))
      (should (equal '(("brew" "install" "DimitarPetrov/stegify/stegify")
                       ("brew" "trust" "dimitarpetrov/stegify")
                       ("brew" "install" "DimitarPetrov/stegify/stegify"))
                     (reverse (get 'monk-test-fake-brew 'calls)))))))

(ert-deftest monk-test-brew-install-other-failure-propagates ()
  "A failure that is not an untrusted tap signals without a retry."
  (let* ((m (monk-test-machine))
         (r (monk-brew-resource :name "b" :packages '("nope")))
         (script (list (list "install" '(1 "Error: No available formula with the name \"nope\".")))))
    (cl-letf (((symbol-function 'monk-exec) (monk-test-fake-brew script)))
      (should-error (monk-repair r m '(:install ("nope"))) :type 'monk-exec-error)
      (should (= 1 (length (get 'monk-test-fake-brew 'calls)))))))

(ert-deftest monk-test-brew-install-trust-unsupported-propagates ()
  "When brew trust itself fails (old Homebrew), the original error signals."
  (let* ((m (monk-test-machine))
         (r (monk-brew-resource :name "b" :packages '("x/y/z")))
         (script (list (list "install" (list 1 monk-test-brew-untrusted-output))
                       (list "trust" '(1 "Error: Unknown command: trust")))))
    (cl-letf (((symbol-function 'monk-exec) (monk-test-fake-brew script)))
      (let ((err (should-error (monk-repair r m '(:install ("x/y/z"))) :type 'monk-exec-error)))
        (should (string-match-p "untrusted tap" (format "%s" (car (last err))))))
      (should (= 2 (length (get 'monk-test-fake-brew 'calls)))))))

(ert-deftest monk-test-brew-tap-trusts-after-tapping ()
  "Tapping also trusts; the comment says which happened."
  (let ((m (monk-test-machine))
        (r (monk-brew-tap-resource :name "a/b")))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-fake-brew (list (list "tap" '(0 "")) (list "trust" '(0 ""))))))
      (should (equal "tapped and trusted" (monk-repair r m '(:tap "a/b"))))
      (should (equal '(("brew" "tap" "a/b") ("brew" "trust" "a/b"))
                     (reverse (get 'monk-test-fake-brew 'calls)))))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-fake-brew (list (list "tap" '(0 ""))
                                          (list "trust" '(1 "Error: Unknown command: trust"))))))
      (should (equal "tapped" (monk-repair r m '(:tap "a/b")))))))

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
                   (monk-line-in-file f "x")
                   (monk-file f :content "x\n")))))
      (should (eq 'kept (monk-test-status run (concat f "#x"))))
      (should (eq 'repaired (monk-test-status run f)))
      (should (monk-run-ok-p run))
      (should (= 2 (monk-run-passes run))))))

(ert-deftest monk-test-failure-still-confirms-repairs ()
  "A failure does not cut the run short: repairs still get a second pass.
The failed resource is checked again in that pass and stays failed."
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge
                 (lambda (_m)
                   (monk-directory (concat dir "made"))
                   (monk-line-in-file (concat dir "missing") "x")))))
      (should (= 2 (monk-run-passes run)))
      (should-not (monk-run-oscillating-p run))
      (should (eq 'repaired (monk-test-status run (concat dir "made"))))
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
                         (lambda (_m program &rest args)
                           (cond
                            ((equal program "dpkg-query")
                             (monk-exec-create :exit-code 0
                                               :output "a installed\nb installed\nc missing\nd missing\n"))
                            ;; the availability probe and apt-get update succeed
                            ((or (equal program "sh") (member "update" args))
                             (monk-exec-create :exit-code 0 :output ""))
                            (t (monk-exec-create :exit-code 1 :output "apt broke"))))))
                (monk-converge
                 m
                 (lambda (_m)
                   (monk-apt "pkgs" '("a" "b" "c" "d"))
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
        (should (string-match-p "\\` *Machine +State" (buffer-substring-no-properties
                                                         (point-min) (line-end-position))))
        ;; the tree: connection -> machine -> status -> resource
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "^. local (1)\n  . testbox " text))
          (should (string-match-p "repaired (1)" text))
          (should (string-match-p (regexp-quote (concat dir "d")) text)))
        ;; a machine row carries its name for the keys; a kept group starts folded
        (goto-char (point-min))
        (monk-ui-next-machine)
        (should (equal "testbox" (monk-ui-machine-at-point)))
        ;; a fold survives a refresh
        (monk-ui-toggle-fold)
        (should (monk-ui--fold-closed-p (point)))
        (monk-ui-refresh)
        (should (equal "testbox" (monk-ui-machine-at-point)))
        (should (monk-ui--fold-closed-p (point)))))))

(ert-deftest monk-test-ui-tree-groups-by-connection-and-scope ()
  "Machines group under their connection; results under a machine group by scope."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((local (monk-test-machine))
           (far (monk-local-machine :name "far"))
           (monk-ui-style 'text)
           (monk-log-level nil))
      (oset far address "far.invalid") (oset far method "rpc")
      (oset far facts monk-test-fake-facts) (oset far classes (monk-hard-classes far))
      (should (eq 'local (monk-machine-connection local)))
      (should (eq 'rpc (monk-machine-connection far)))
      (should (equal "/rpc:far.invalid:" (monk-machine-remote-prefix far)))
      ;; a run with one user and one sudo result on the local machine, and
      ;; a stand-in result on far: built by hand, no connection is made
      (let ((run (monk-run-create :machines (list local far) :dry-run t :passes 1)))
        (setf (monk-run-results run)
              (list (monk-result-create :machine "testbox" :name (concat dir "a") :type "directory"
                                        :status monk-status-kept :scope monk-scope-user)
                    (monk-result-create :machine "testbox" :name (concat dir "b") :type "directory"
                                        :status monk-status-would-repair :scope monk-scope-sudo)
                    (monk-result-create :machine "far" :name "/x" :type "directory"
                                        :status monk-status-kept :scope monk-scope-user)))
        (with-temp-buffer
          (monk-ui-mode)
          (setq monk-ui--run run)
          (monk-ui-refresh)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match "^. local (1)" text))
            (should (string-match "^. rpc (1)" text))
            (should (< (string-match "local (1)" text) (string-match "rpc (1)" text)))
            ;; two scopes on testbox: scope headings appear
            (should (string-match-p "^      . user (1)" text))
            (should (string-match-p "^      . sudo (1)" text))
            ;; one scope on far: no scope heading, straight to the status group
            (let ((far-part (substring text (string-match "rpc (1)" text))))
              (should-not (string-match-p "user (1)" far-part))
              (should (string-match-p "kept (1)" far-part))))
          ;; n/p walk the machine rows
          (goto-char (point-min))
          (monk-ui-next-machine)
          (should (equal "testbox" (monk-ui-machine-at-point)))
          (monk-ui-next-machine)
          (should (equal "far" (monk-ui-machine-at-point)))
          (monk-ui-previous-machine)
          (should (equal "testbox" (monk-ui-machine-at-point)))
          ;; S-TAB folds every machine to its row
          (monk-ui-fold-all)
          (should (monk-ui--fold-closed-p (point)))
          (monk-ui-fold-all t)
          ;; point on a result line stays on that result across a redraw
          (goto-char (point-min))
          (search-forward (concat dir "b"))
          (beginning-of-line)
          (let ((key (monk-result-key (get-text-property (point) 'monk-ui-result))))
            (monk-ui-refresh)
            (should (equal key (monk-result-key (get-text-property (line-beginning-position) 'monk-ui-result)))))
          ;; nested folds keep their own state: open kept, fold the
          ;; machine, unfold it: would-repair is still open, kept still open,
          ;; and a group folded by hand stays folded
          (goto-char (point-min))
          (re-search-forward "would-repair (1)") (beginning-of-line)
          (monk-ui-toggle-fold)
          (should (monk-ui--fold-closed-p (point)))
          (goto-char (point-min))
          (monk-ui-next-machine)
          (monk-ui-toggle-fold)
          (should (monk-ui--fold-closed-p (point)))
          (monk-ui-toggle-fold)
          (should-not (monk-ui--fold-closed-p (point)))
          (goto-char (point-min))
          (re-search-forward "would-repair (1)") (beginning-of-line)
          (should (monk-ui--fold-closed-p (point)))
          (should (invisible-p (1+ (line-end-position))))
          (monk-ui-toggle-fold)
          (should-not (invisible-p (1+ (line-end-position))))
          ;; point inside a folded kept body while the cursor is drawn on
          ;; the skipped heading below: TAB acts on skipped, not kept
          (goto-char (point-min))
          (re-search-forward "kept (1)") (beginning-of-line)
          (monk-ui-toggle-fold)
          (let ((kept-h (point)))
            (forward-line 1)
            (should (invisible-p (point)))
            (monk-ui-toggle-fold)
            (should (monk-ui--fold-closed-p kept-h))
            ;; point moved to the next visible heading (sudo here) and
            ;; TAB toggled that one, not kept
            (let ((next-h (monk-ui--fold-heading-at (line-beginning-position))))
              (should next-h)
              (should-not (eq next-h kept-h))
              (should (monk-ui--fold-closed-p next-h))
              (monk-ui-toggle-fold)
              (should-not (monk-ui--fold-closed-p next-h)))
            (goto-char kept-h)
            (monk-ui-toggle-fold)
            (should-not (monk-ui--fold-closed-p kept-h)))
          ;; the enclosing heading of a line inside a folded kept body
          ;; is kept, not the machine (the walk used to stop one level up)
          (goto-char (point-min))
          (re-search-forward "kept (1)") (beginning-of-line)
          (let ((kept-h (point)))
            (monk-ui-toggle-fold)
            (forward-line 1)
            (should (eq (monk-ui--enclosing-heading (point)) kept-h))
            (goto-char kept-h)
            (monk-ui-toggle-fold)
            (should-not (monk-ui--fold-closed-p kept-h)))
          ;; TAB on the last status heading at column 0, and on the blank
          ;; line after the tree (acts on the heading above)
          (goto-char (point-min))
          (re-search-forward "skipped\\|kept (1)" nil t)
          (goto-char (point-max))
          (should (bolp))
          (let ((heading (monk-ui--enclosing-heading (point))))
            (should heading)
            (monk-ui-toggle-fold)
            (should (monk-ui--fold-closed-p heading))
            (monk-ui-toggle-fold)
            (should-not (monk-ui--fold-closed-p heading)))
          ;; point on a status heading stays on it; TAB at column 0 folds it
          (goto-char (point-min))
          (re-search-forward "kept (1)")
          (beginning-of-line)
          (let ((id (monk-ui--fold-id-at (point))))
            (should id)
            (monk-ui-toggle-fold)
            (should (monk-ui--fold-closed-p (monk-ui--fold-heading-at (point))))
            (monk-ui-refresh)
            (should (eq id (monk-ui--fold-id-at (line-beginning-position))))
            (should (monk-ui--fold-closed-p (monk-ui--fold-heading-at (line-beginning-position))))))))))

(ert-deftest monk-test-ui-display-modes-and-quit-restores-layout ()
  "`monk-ui-display' picks the window; q after full-frame restores the layout."
  (require 'monk-ui)
  (skip-unless (not noninteractive))
  (let ((run (monk-run-create :machines (list (monk-test-machine)) :dry-run t :passes 1))
        (monk-ui-style 'text))
    (delete-other-windows)
    (split-window-right)
    (let ((windows-before (length (window-list))))
      (let ((monk-ui-display 'full-frame))
        (monk-ui-report run)
        (should (= 1 (length (window-list))))
        (should (eq (current-buffer) (get-buffer monk-ui-buffer-name)))
        (monk-ui-quit)
        (should (= windows-before (length (window-list)))))
      (let ((monk-ui-display 'pop))
        (monk-ui-report run)
        (should (get-buffer-window monk-ui-buffer-name))
        (quit-window)))))

(ert-deftest monk-test-ui-fold-walk-terminates ()
  "TAB twice on every line of a large tree ends, keeps the buffer size,
and never grows undo: one TAB left 24 MB of undo and a hung Emacs
(2026-10-12), from a heading walk that did not move backward."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m)
                     (dotimes (i 30) (monk-directory (format "%sd%d" dir i)))
                     (monk-directory (concat dir "x") :when '(= 1 2))))
           (m (monk-test-machine))
           (monk-ui-style 'text)
           (monk-log-level nil))
      (monk-converge m policy)
      (with-current-buffer (monk-ui-show (monk-converge m policy))
        (should (eq buffer-undo-list t))
        (let ((size (buffer-size)) (lines (count-lines (point-min) (point-max))) (steps 0))
          (goto-char (point-min))
          (while (and (not (eobp)) (< steps 500))
            (cl-incf steps)
            (with-timeout (2 (ert-fail (format "TAB hung on %S" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
              (ignore-errors (monk-ui-toggle-fold))
              (ignore-errors (monk-ui-toggle-fold)))
            ;; move like C-n does: past hidden text
            (let ((line-move-visual nil)) (ignore-errors (next-line 1)))
            (when (invisible-p (point)) (goto-char (next-single-char-property-change (point) 'invisible))))
          (should (< steps 500))
          (should (= size (buffer-size)))
          (should (= lines (count-lines (point-min) (point-max)))))))))

(ert-deftest monk-test-ui-unasked-runs-do-not-pop-the-window ()
  "A run nobody asked for updates *monk* but never brings it up.
In batch there are no windows, so this checks the decision itself."
  (require 'monk-ui)
  (let* ((buffer (monk-ui-show (monk-run-create :machines (list (monk-test-machine)) :dry-run t :passes 1)))
         (shown nil))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer) (lambda (&rest _) (setq shown 'pop) 'window))
              ((symbol-function 'pop-to-buffer) (lambda (&rest _) (setq shown 'full) nil))
              ((symbol-function 'selected-window) (lambda () 'window)))
      (dolist (mode '(full-frame pop))
        (let ((monk-ui-display mode))
          ;; unasked under when-visible: nothing is shown
          (setq shown nil)
          (let ((monk-ui-show-unasked-runs 'when-visible))
            (should-not (monk-ui-display-buffer buffer t))
            (should-not shown))
          ;; unasked under nil: nothing either
          (let ((monk-ui-show-unasked-runs nil))
            (should-not (monk-ui-display-buffer buffer t))
            (should-not shown))
          ;; unasked under t: shown as any run
          (let ((monk-ui-show-unasked-runs t))
            (should (monk-ui-display-buffer buffer t))
            (should shown))
          ;; asked: shown
          (setq shown nil)
          (should (monk-ui-display-buffer buffer nil))
          (should shown))))))

(ert-deftest monk-test-ui-display-buffer-in-batch ()
  "In batch, `monk-ui-display-buffer' returns a window for every mode and keeps an existing one."
  (require 'monk-ui)
  (let ((buffer (monk-ui-show (monk-run-create :machines (list (monk-test-machine)) :dry-run t :passes 1))))
    (dolist (mode '(full-frame pop same-window auto))
      (let ((monk-ui-display mode))
        (should (window-live-p (monk-ui-display-buffer buffer)))
        (should (eq (monk-ui-display-buffer buffer) (get-buffer-window buffer)))))))

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

;;;; Watch: symlinked directories

(ert-deftest monk-test-watch-index-resolves-symlinked-directories ()
  "An event on the real path of a file under a symlinked directory finds its node.
Found live: ~/autodist was a link, fswatch reported the target tree,
and the edit was silently unmanaged."
  (monk-test-with-tmpdir dir
    (make-directory (concat dir "real/files") t)
    (make-symbolic-link (concat dir "real") (concat dir "link"))
    (with-temp-file (concat dir "real/files/zshrc") (insert "x"))
    (let* ((m (monk-test-machine))
           (nodes (monk--nodes-for (list m)
                                   (lambda (_m)
                                     (monk-file (concat dir "link/files/zshrc") :content "x"))))
           (w (monk-watch-create :machines (list m) :policy #'ignore
                                 :path-index (monk-watch--build-index nodes))))
      ;; indexed under the real directory
      (should (assoc (file-name-as-directory (file-truename (concat dir "real/files")))
                     (monk-watch-path-index w)))
      ;; both spellings of the event map to the node
      (should (= 1 (length (monk-watch--nodes-for-file w (concat dir "real/files/zshrc")))))
      (should (= 1 (length (monk-watch--nodes-for-file w (concat dir "link/files/zshrc")))))
      ;; a managed symlink keeps its own name as the indexed path
      (let* ((nodes2 (monk--nodes-for (list m) (lambda (_m) (monk-symlink (concat dir "sl") (concat dir "real")))))
             (w2 (monk-watch-create :machines (list m) :policy #'ignore
                                    :path-index (monk-watch--build-index nodes2))))
        (should (seq-some (lambda (e) (assoc (concat (directory-file-name (file-truename dir)) "/sl") (cdr e)))
                          (monk-watch-path-index w2)))))))

;;;; Watch: what fswatch is pointed at

(ert-deftest monk-test-watch-fswatch-paths-flat-and-trees ()
  "Owned files give their parent, flat; owned subtrees give themselves, walked.
A directory resource is a file in this sense: its parent is watched
flat, its contents are not.  An rsync source tree is walked.  A parent
inside a walked tree is dropped, and ~ itself is never walked."
  (require 'monk-sync)
  (monk-test-with-tmpdir dir
    (make-directory (concat dir "home/.config/app") t)
    (make-directory (concat dir "home/src/plugins") t)
    (with-temp-file (concat dir "home/.zshrc") (insert "x"))
    (with-temp-file (concat dir "home/.config/app/conf") (insert "x"))
    (let* ((m (monk-test-machine (append (list (cons 'home (concat dir "home"))) monk-test-fake-facts)))
           (nodes (monk--nodes-for (list m)
                                   (lambda (_m)
                                     (monk-file "~/.zshrc" :content "x")
                                     (monk-file "~/.config/app/conf" :content "x")
                                     (monk-directory "~/src/plugins")
                                     (monk-rsync "push" '("~/.zshrc" "~/src/plugins/") "/tmp/o/" :archive t))))
           (w (monk-watch-create :machines (list m) :policy #'ignore
                                 :path-index (monk-watch--build-index nodes))))
      (pcase-let ((`(,flat . ,trees) (monk-watch--fswatch-paths w)))
        ;; ~ (for .zshrc), ~/.config/app (for conf), ~/src (parent of the
        ;; plugins directory resource: a flat watch there sees the
        ;; directory replaced or re-moded); the rsync walks plugins itself
        (should (equal (list (directory-file-name (concat dir "home"))
                             (concat dir "home/.config/app")
                             (concat dir "home/src"))
                       flat))
        (should (equal (list (concat dir "home/src/plugins")) trees))))))

;;;; Watch selection by shared paths

(ert-deftest monk-test-select-nodes-follows-shared-paths ()
  "A resource that owns a changed resource's path is selected with it."
  (require 'monk-sync)
  (let* ((m (monk-test-machine))
         (nodes (monk--nodes-for (list m)
                                 (lambda (_m)
                                   (monk-file "~/autodist/zshrc" :content "x\n")
                                   (monk-symlink "~/.zshrc" "~/autodist/zshrc")
                                   (monk-rsync "push" '("~/.zshrc") "/tmp/x/" :follow-links t)
                                   (monk-directory "~/unrelated"))))
         (selected (monk-select-nodes nodes (list (cons "testbox" "~/autodist/zshrc")))))
    (should (equal '("~/autodist/zshrc" "~/.zshrc" "push")
                   (mapcar (lambda (n) (oref (monk-node-resource n) name)) selected)))))

;;;; CLI progress

(ert-deftest monk-test-cli-tty-detection-and-format ()
  "Off a terminal nothing is rewritten; the helpers pad and elide by display width."
  (require 'monk-cli)
  ;; the test runner's stderr is a pipe or a file, never a tty
  (should-not (monk-cli-stderr-tty-p))
  (should (equal "abc   " (monk-cli-pad "abc" 6)))
  (should (equal "abcdef" (monk-cli-pad "abcdef" 3)))
  (let ((monk-cli--utf8 t))
    (should (equal "abcdefgh" (monk-cli-elide "abcdefgh" 8)))
    (should (equal "abc…ijk" (monk-cli-elide "abcdefghijk" 7)))
    (should (= 7 (string-width (monk-cli-elide "~/.config/very/long/path/name" 7)))))
  (let ((monk-cli--utf8 nil))
    (should (equal "ab...jk" (monk-cli-elide "abcdefghijk" 7))))
  ;; no tty: glyphs still, but no SGR
  (let ((monk-cli--tty nil) (monk-cli--utf8 t) (monk-cli-color 'auto))
    (should (equal "✓" (monk-cli-glyph monk-status-kept)))
    (should-not (string-match-p "\e" (monk-cli-glyph monk-status-failed))))
  (let ((monk-cli--tty t) (monk-cli--utf8 nil) (monk-cli-color t))
    (should (equal "\e[31m!!\e[0m" (monk-cli-glyph monk-status-failed))))
  (let ((monk-cli--tty t) (monk-cli-color 'auto))
    (cl-letf (((symbol-function 'getenv) (lambda (v) (cond ((equal v "NO_COLOR") "1") ((equal v "TERM") "xterm")))))
      (should-not (monk-cli-color-p)))
    (cl-letf (((symbol-function 'getenv) (lambda (v) (cond ((equal v "TERM") "dumb")))))
      (should-not (monk-cli-color-p)))
    (cl-letf (((symbol-function 'getenv) (lambda (v) (cond ((equal v "TERM") "xterm")))))
      (should (monk-cli-color-p)))))

(ert-deftest monk-test-cli-progress-prints-changes-and-sizes-columns ()
  "Without a tty the hook prints changed and failed results as aligned lines."
  (require 'monk-cli)
  (monk-test-with-tmpdir dir
    (let* ((lines nil)
           (monk-log-level nil)
           (machines (monk-test-machines "a" "much-longer-name")))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) lines))))
        (unwind-protect
            (progn
              (monk-cli-enable)
              (should-not monk-cli--tty)
              (monk-orchestrate machines
                                (lambda (m)
                                  (monk-directory (concat dir (oref m name)))
                                  (monk-directory dir)
                                  (monk-line-in-file (concat dir "absent-" (oref m name)) "x"))
                                :passes 1))
          (monk-cli-disable)))
      (setq lines (nreverse lines))
      ;; machine column sized to the longest name: both rows align
      (let ((rows (seq-filter (lambda (l) (string-match-p "directory" l)) lines)))
        (should (= 2 (length rows)))
        (should (= (string-search "directory" (car rows)) (string-search "directory" (cadr rows)))))
      (should (= 16 monk-cli--machine-width))
      (should (= 2 (seq-count (lambda (l) (string-match-p "line-in-file.*absent" l)) lines)))
      ;; kept results do not print: the shared dir itself was kept on both machines
      (should-not (seq-some (lambda (l) (string-match-p (concat (regexp-quote (directory-file-name dir)) "  ") l)) lines))
      (should (= 2 (seq-count (lambda (l) (string-match-p "directory" l)) lines)))
      ;; nothing rewrote the line
      (should-not (seq-some (lambda (l) (or (string-search "\r" l) (string-search "\e[2K" l))) lines))
      (should (string-match-p "2 failed  1 pass" (monk-cli-summary-line (monk-orchestrate machines (lambda (m) (monk-line-in-file (concat dir "gone-" (oref m name)) "x")) :passes 1)))))))

;;;; rsync and launchd

(ert-deftest monk-test-rsync-parse-itemize ()
  (require 'monk-sync)
  (pcase-let ((`(,changed . ,total)
               (monk-rsync-parse-itemize
                (concat ">f+++++++++ a\n.d..t...... d/\n>f.st...... d/b\n*deleting   gone\n"
                        "sending incremental file list\n\nNumber of files: 1,204 (reg: 3, dir: 2)\n"))))
    (should (= 1204 total))
    (should (equal '(">f+++++++++ a" ">f.st...... d/b" "*deleting   gone") changed)))
  (should (null (cdr (monk-rsync-parse-itemize ">f+++++++++ a\n"))))
  ;; protocol 31 prints 12 flag columns; found live on macOS, where
  ;; every line was dropped and a mode change went unseen
  ;; and `T' alone (identical content, time would be stamped) is not
  ;; drift: without -t it repeats on every run and flapped live
  (should (equal '("<f..Tp...... config" ".d...p...... config.d/")
                 (car (monk-rsync-parse-itemize
                       "<f..Tp...... config\n.d...p...... config.d/\n<f..T....... config.d/w\n.d..T....... old/\n"))))
  ;; a mode-only or owner-only difference on a directory is drift; time-only is not
  (should (equal '(".d...p..... config.d/" ".d.....o... other/")
                 (car (monk-rsync-parse-itemize ".d...p..... config.d/\n.d..t...... d/\n.d.....o... other/\n"))))
  ;; a mode-only difference on a symlink is not drift: Linux cannot set
  ;; it and a Mac source reports it on every run (found live, tpm)
  (should (equal '(".L..tp...... a -> b" "cL++++++++++ new -> x")
                 (car (monk-rsync-parse-itemize
                       (concat ".L...p...... tmux-resurrect/run_tests -> lib/tmux-test/run_tests\n"
                               ".L..tp...... a -> b\n"
                               "cL++++++++++ new -> x\n"))))))

(ert-deftest monk-test-rsync-archive-omits-ownership ()
  ":archive is -rlptD, never -o -g: a user cannot chown and the run would flap."
  (require 'monk-sync)
  (let* ((m (monk-test-machine))
         (r (monk-rsync-resource :name "x" :sources '("/s/") :destination "/d/" :archive t :delete t :timeout nil)))
    (should (equal '("-rlptD" "--delete" "/s/" "/d/") (monk--rsync-arguments r m)))
    (should (equal '("-r" "-L" "-p" "-t" "/s/" "/d/")
                   (monk--rsync-arguments
                    (monk-rsync-resource :name "y" :sources '("/s/") :destination "/d/"
                                         :recursive t :follow-links t :perms t :timeout nil)
                    m)))))

(ert-deftest monk-test-rsync-perms-converges-with-old-mtimes ()
  "A source older than its identical copy does not repair forever."
  (skip-unless (executable-find "rsync"))
  (require 'monk-sync)
  (monk-test-with-tmpdir dir
    (let* ((src (concat dir "src/")) (dst (concat dir "dst/"))
           (policy (lambda (_m) (monk-rsync "t" (list src) dst :recursive t :follow-links t :perms t))))
      (make-directory src) (make-directory dst)
      (with-temp-file (concat src "f") (insert "x\n"))
      (copy-file (concat src "f") (concat dst "f"))
      (set-file-times (concat src "f") (encode-time '(0 0 0 1 1 2020 nil nil t)))
      (let ((run (monk-test-converge policy)))
        (should (eq 'repaired (monk-test-status run "t")))
        (should (monk-run-converged-p run))
        (should (= 2 (monk-run-passes run))))
      (should (eq 'kept (monk-test-status (monk-test-converge policy :passes 1) "t"))))))

(ert-deftest monk-test-rsync-perms-carries-directory-mode ()
  "With :perms, a 0755 destination directory becomes 0700 like the source."
  (skip-unless (executable-find "rsync"))
  (require 'monk-sync)
  (monk-test-with-tmpdir dir
    (let* ((src (concat dir "src/config.d"))
           (dst (concat dir "dst/"))
           (policy (lambda (_m) (monk-rsync "cfg" (list src) dst :recursive t :follow-links t :perms t))))
      (make-directory src t)
      (make-directory (concat dst "config.d") t)
      (with-temp-file (concat src "/work") (insert "x\n"))
      (with-temp-file (concat dst "config.d/work") (insert "x\n"))
      (set-file-modes src #o700)
      (set-file-modes (concat dst "config.d") #o755)
      (let ((run (monk-test-converge policy :passes 1)))
        (should (eq 'repaired (monk-test-status run "cfg")))
        (should (= #o700 (logand #o777 (file-modes (concat dst "config.d"))))))
      (should (eq 'kept (monk-test-status (monk-test-converge policy :passes 1) "cfg")))
      ;; without :perms the same difference is invisible, which is the Makefile's behavior
      (set-file-modes (concat dst "config.d") #o755)
      (should (eq 'kept (monk-test-status
                         (monk-test-converge (lambda (_m) (monk-rsync "cfg" (list src) dst :recursive t)) :passes 1)
                         "cfg"))))))

(ert-deftest monk-test-rsync-local-roundtrip ()
  "rsync to a local destination: would-repair, repaired with a degree, then kept."
  (skip-unless (executable-find "rsync"))
  (require 'monk-sync)
  (monk-test-with-tmpdir dir
    (let* ((src (concat dir "src/"))
           (dst (concat dir "dst/"))
           (policy (lambda (_m)
                     (monk-rsync "mirror" (list src) dst :archive t :delete t)))
           (m (monk-test-machine))
           (monk-log-level nil))
      (make-directory (concat src "d") t)
      (with-temp-file (concat src "a") (insert "a\n"))
      (with-temp-file (concat src "d/b") (insert "b\n"))
      (make-directory dst)
      (with-temp-file (concat dst "stale") (insert "x\n"))
      (let ((plan (monk-plan m policy)))
        (should (eq 'would-repair (monk-test-status plan "mirror")))
        ;; a, d/ (created), d/b, and the deletion of stale
        (should (= 4 (length (plist-get (monk-result-changes (monk-test-result plan "mirror")) :sync))))
        (should (file-exists-p (concat dst "stale"))))
      (let ((run (monk-converge m policy)))
        (should (eq 'repaired (monk-test-status run "mirror")))
        ;; rsync names the deletion first, then the three copies; three shown
        (should (string-match-p "stale, a, d/ -> .*dst/ (\\+1 more)" (monk-result-comment (monk-test-result run "mirror"))))
        (should (equal "b\n" (with-temp-buffer (insert-file-contents (concat dst "d/b")) (buffer-string))))
        (should-not (file-exists-p (concat dst "stale")))
        (should (= 2 (monk-run-passes run))))
      (should (eq 'kept (monk-test-status (monk-test-converge policy) "mirror"))))))

(ert-deftest monk-test-rsync-follow-links-file-list ()
  "Several file sources with -L land as files in the destination directory."
  (skip-unless (executable-find "rsync"))
  (require 'monk-sync)
  (monk-test-with-tmpdir dir
    (let ((real (concat dir "real"))
          (link (concat dir "link"))
          (dst (concat dir "out/")))
      (with-temp-file real (insert "r\n"))
      (make-symbolic-link real link)
      (make-directory dst)
      (let ((run (monk-test-converge (lambda (_m) (monk-rsync "files" (list link) dst :follow-links t)))))
        (should (eq 'repaired (monk-test-status run "files")))
        (should-not (file-symlink-p (concat dst "link")))
        (should (equal "r\n" (with-temp-buffer (insert-file-contents (concat dst "link")) (buffer-string))))))))

(ert-deftest monk-test-rsync-unreachable-is-skipped-with-probe ()
  ":reachable turns a dead host into a skip; without it, rsync's 255 is a failure."
  (require 'monk-sync)
  (let ((m (monk-test-machine)) (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-host-reachable-p) (lambda (&rest _) nil)))
      (let ((run (monk-converge m (lambda (_m) (monk-rsync "s" '("/x") "dead:x" :reachable "dead")) :passes 1)))
        (should (eq 'skipped (monk-test-status run "s")))
        (should (equal "dead is not reachable" (monk-result-comment (monk-test-result run "s"))))))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (if (equal program "rsync")
                     (monk-exec-create :exit-code 255 :output "ssh: connect to host dead port 22: No route")
                   (ignore args)
                   (monk-exec-create :exit-code 0 :output "")))))
      (let ((run (monk-converge m (lambda (_m) (monk-rsync "s" '("/x") "dead:x")) :passes 1)))
        (should (eq 'failed (monk-test-status run "s")))
        (should (string-match-p "could not reach dead:x (ssh exit 255)" (monk-result-comment (monk-test-result run "s"))))))
    ;; a ProxyCommand helper missing on this host: exit 124 from
    ;; timeout(1) inside the ssh config, with the helper's own message;
    ;; the comment names the cause, not the 14-path argv
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (if (equal program "rsync")
                     (monk-exec-create :exit-code 124
                                       :output (concat "timeout: failed to run command \u2018hostlookup\u2019: No such file or directory\n"
                                                       "rsync: connection unexpectedly closed (0 bytes received so far) [sender]\n"
                                                       "rsync error: unexplained error (code 255) at io.c(232) [sender=3.4.1]\n"))
                   (ignore args)
                   (monk-exec-create :exit-code 0 :output "")))))
      (let* ((run (monk-converge m (lambda (_m) (monk-rsync "s" '("/x") "work:x")) :passes 1))
             (comment (monk-result-comment (monk-test-result run "s"))))
        (should (eq 'failed (monk-test-status run "s")))
        (should (string-match-p "could not reach work:x (ssh exit 124, a ProxyCommand or Match exec timed out)" comment))
        (should (string-match-p "hostlookup" comment))
        (should-not (string-match-p "rsync error" comment))
        (should-not (string-match-p "--itemize" comment))))
    ;; Monk's own 600 s kill is not an ssh helper timeout: the comment
    ;; says the transfer hung and quotes the last ssh lines
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (if (equal program "rsync")
                     (monk-exec-create :exit-code monk-exec-timeout-exit
                                       :output "debug1: Authenticating to work:22 as 't'\nmonk: rsync stopped after 600s (monk-exec-timeout)\n")
                   (ignore args)
                   (monk-exec-create :exit-code 0 :output "")))))
      (let* ((run (monk-converge m (lambda (_m) (monk-rsync "s" '("/x") "work:x")) :passes 1))
             (comment (monk-result-comment (monk-test-result run "s"))))
        (should (eq 'failed (monk-test-status run "s")))
        (should (string-match-p "hung for 600s before any transfer" comment))
        (should (string-match-p "Authenticating to work" comment))
        (should-not (string-match-p "ProxyCommand" comment))))
    ;; and when the machine slept meanwhile, the comment says that instead
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (if (equal program "rsync")
                     (monk-exec-create :exit-code monk-exec-timeout-exit
                                       :output (concat "monk: rsync stopped after 600s (monk-exec-timeout)\n"
                                                       monk-exec-slept-note "\n"))
                   (ignore args)
                   (monk-exec-create :exit-code 0 :output "")))))
      (let* ((run (monk-converge m (lambda (_m) (monk-rsync "s" '("/x") "work:x")) :passes 1))
             (comment (monk-result-comment (monk-test-result run "s"))))
        ;; interrupted: skipped with the reason, and the run is ok
        (should (eq 'skipped (monk-test-status run "s")))
        (should (monk-run-ok-p run))
        (should-not (monk-run-unreachable run))
        (should (string-match-p "the machine slept during the transfer to work:x (stopped after 600s); next run" comment))))))

(ert-deftest monk-test-host-reachable-p ()
  "A listening local port is reachable; a closed one is not; answers are cached per run."
  (require 'monk-sync)
  (let* ((server (make-network-process :name "monk-test-listen" :server t :host "127.0.0.1"
                                       :service t :noquery t))
         (port (process-contact server :service))
         (monk--current-run (monk-run-create)))
    (unwind-protect
        (progn
          (should (monk-host-reachable-p "127.0.0.1" port 1.0))
          (delete-process server)
          ;; cached: still yes inside this run
          (should (monk-host-reachable-p "127.0.0.1" port 1.0))
          (let ((monk--current-run (monk-run-create)))
            (should-not (monk-host-reachable-p "127.0.0.1" port 0.5))))
      (ignore-errors (delete-process server)))))

(ert-deftest monk-test-launchd-plist-and-check ()
  "The plist renders; check compares content and load state through faked launchctl."
  (let* ((m (monk-test-machine))
         (job '((ProgramArguments . ("/usr/bin/emacs" "--batch" "-f" "monk-agent-main"))
                (RunAtLoad . t) (KeepAlive . t) (ThrottleInterval . 30)
                (EnvironmentVariables . ((PATH . "/usr/bin:/bin")))))
         (xml (monk-launchd-plist "org.example.job" job)))
    (should (string-match-p "<key>Label</key><string>org.example.job</string>" xml))
    (should (string-match-p "<key>ProgramArguments</key><array><string>/usr/bin/emacs</string>" xml))
    (should (string-match-p "<key>KeepAlive</key><true/>" xml))
    (should (string-match-p "<key>ThrottleInterval</key><integer>30</integer>" xml))
    (should (string-match-p "<key>EnvironmentVariables</key><dict><key>PATH</key>" xml))
    (should (string-match-p "&lt;x&gt;" (monk-launchd-plist "l" '((Comment . "<x>")))))
    (monk-test-with-tmpdir dir
      (let ((monk-launchd-agent-directory (concat dir "agents/"))
            (r (monk-launchd-resource :name "org.example.job" :plist job))
            (loaded nil) (calls nil))
        (make-directory (concat dir "agents"))
        (cl-letf (((symbol-function 'monk-exec)
                   (lambda (_m program &rest args)
                     (push (cons program (seq-take-while #'stringp args)) calls)
                     (pcase program
                       ("launchctl"
                        (pcase (car args)
                          ("print" (monk-exec-create :exit-code (if loaded 0 113) :output ""))
                          ("bootstrap" (setq loaded t) (monk-exec-create :exit-code 0 :output ""))
                          ("bootout" (setq loaded nil) (monk-exec-create :exit-code 0 :output ""))))
                       ("id" (monk-exec-create :exit-code 0 :output "501\n"))
                       (_ (monk-exec-create :exit-code 0 :output ""))))))
          (let ((monk--current-run (monk-run-create)))
            (should (equal '(:content t :loaded (nil t)) (monk-check r m)))
            (should (equal "wrote plist, loaded" (monk-repair r m (monk-check r m))))
            (should (equal xml (with-temp-buffer
                                 (insert-file-contents (concat dir "agents/org.example.job.plist"))
                                 (buffer-string))))
            (should (member '("launchctl" "bootstrap" "gui/501") 
                            (mapcar (lambda (c) (seq-take c 3)) calls)))
            (should-not (monk-check r m))
            ;; content change while loaded: bootout then bootstrap
            (setq calls nil)
            (oset r plist (cons '(Nice . 5) job))
            (should (equal "wrote plist, loaded" (monk-repair r m (monk-check r m))))
            (should (equal '("bootout" "bootstrap")
                           (delq nil (mapcar (lambda (c) (and (equal (car c) "launchctl")
                                                              (member (cadr c) '("bootout" "bootstrap"))
                                                              (cadr c)))
                                             (reverse calls)))))))))))

(ert-deftest monk-test-agent-launchd-job ()
  (require 'monk-agent)
  (let* ((monk-agent-emacs "/opt/homebrew/bin/emacs")
         (monk-agent-load-path "/Users/x/source/monk/lisp")
         (job (monk-agent-launchd-job "/Users/x/policy.el")))
    (should (equal '("/opt/homebrew/bin/emacs" "--batch" "-L" "/Users/x/source/monk/lisp"
                     "-l" "/Users/x/policy.el" "-f" "monk-agent-main")
                   (alist-get 'ProgramArguments job)))
    (should (eq t (alist-get 'KeepAlive job)))
    (should (string-suffix-p "agent.log" (alist-get 'StandardOutPath job)))))

;;;; Salt bridge: high aggregation, import, completion, matchers

(defconst monk-test-salt-low-sls-json
  "{\"local\": [
  {\"state\": \"file\", \"name\": \"/tmp/w/a.txt\", \"__sls__\": \"demo\", \"__env__\": \"base\",
   \"__id__\": \"/tmp/w/a.txt\", \"contents\": \"hello\\n\", \"order\": 10000, \"fun\": \"managed\"},
  {\"state\": \"cmd\", \"name\": \"touch /tmp/w/b\", \"__sls__\": \"demo\", \"__env__\": \"base\",
   \"__id__\": \"touch-b\", \"creates\": \"/tmp/w/b\", \"order\": 10001, \"fun\": \"run\",
   \"require\": [{\"file\": \"/tmp/w/a.txt\"}]},
  {\"state\": \"cmd\", \"name\": \"echo last\", \"__sls__\": \"demo\", \"__env__\": \"base\",
   \"__id__\": \"last-one\", \"order\": 1010100, \"fun\": \"run\"}]}"
  "What state.show_low_sls demo printed on the sandbox (from the Salt review).")

(ert-deftest monk-test-salt-split-key-and-id ()
  (require 'monk-bridge)
  (should (equal '("file" . "managed") (monk-salt-split-key "file_|-/x_|-/x_|-managed")))
  (should (equal '("cmd" . "run") (monk-salt-split-key "cmd_|-touch-b_|-touch /tmp/w/b_|-run")))
  (should (equal "weird" (monk-salt-result-id "cmd_|-a_|-b_|-run" '((__id__ . "weird")))))
  (should (equal "a" (monk-salt-result-id "cmd_|-a_|-b_|-run" nil)))
  (should (null (monk-salt-result-id "bare" nil))))

(ert-deftest monk-test-salt-import-sls ()
  "Low chunks become monk-salt forms with requisites, in Salt's order."
  (require 'monk-bridge)
  (let ((m (monk-test-machine)))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m _p &rest args)
                 (should (member "state.show_low_sls" args))
                 (should (member "--file-root=/tmp/states" args))
                 (monk-exec-create :exit-code 0 :output monk-test-salt-low-sls-json))))
      (let ((forms (monk-salt-import-sls m "demo" "/tmp/states")))
        (should (= 3 (length forms)))
        (should (equal '(monk-salt "/tmp/w/a.txt" "file.managed" '((contents . "hello\n")))
                       (nth 0 forms)))
        (should (equal '(monk-salt "touch /tmp/w/b" "cmd.run" '((creates . "/tmp/w/b"))
                                   :id "touch-b" :require '("/tmp/w/a.txt"))
                       (nth 1 forms)))
        (should (equal "echo last" (nth 1 (nth 2 forms))))))))

(ert-deftest monk-test-salt-high-data-and-aggregate ()
  "Independent salt resources merge into one state.high with Salt requisites."
  (require 'monk-bridge)
  (let* ((a (monk-salt-resource :name "/x" :function "file.symlink" :args '((target . "/y"))))
         (b (monk-salt-resource :name "svc" :function "service.running" :args nil :watch '("/x")))
         (high (monk-salt-high-data (list a b))))
    (should (equal '(/x . ((file . (((name . "/x")) ((target . "/y")) ((order . 10000)) "symlink"))))
                   (car high)))
    (should (equal '(svc . ((service . (((name . "svc")) ((order . 10001))
                                        ((watch . (((file . "/x"))))) "running"))))
                   (cadr high)))
    (should (string-match-p "\"/x\":{\"file\":\\[{\"name\":\"/x\"}"
                            (monk-bridge--json-string high)))
    (let ((agg (monk-resource-aggregate a (list b) (monk-test-machine))))
      (should agg)
      (should (equal (list a b) (oref agg members)))
      (should (null (monk-resource-aggregate a (list (monk-salt-resource :name "s" :function "f.g" :sudo t))
                                             (monk-test-machine)))))))

(ert-deftest monk-test-salt-high-check-and-disaggregate ()
  "One state.high test call answers for every member; failures stay per member."
  (require 'monk-bridge)
  (let* ((m (monk-test-machine))
         (a (monk-salt-resource :name "/x" :function "file.symlink" :args '((target . "/y"))))
         (b (monk-salt-resource :name "/z" :function "file.directory" :args nil))
         (c (monk-salt-resource :name "bad" :function "test.fail_without_changes" :args nil))
         (agg (monk-resource-aggregate a (list b c) m))
         (calls nil)
         (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m _p &rest args)
                 (push args calls)
                 (monk-exec-create
                  :exit-code 0
                  :output (concat "[WARNING ] noise on stderr\n"
                                  "{\"local\": {"
                                  "\"file_|-/x_|-/x_|-symlink\": {\"result\": null, \"changes\": {}, \"comment\": \"Symlink /x to /y is set for creation\", \"__id__\": \"/x\", \"__run_num__\": 0},"
                                  "\"file_|-/z_|-/z_|-directory\": {\"result\": true, \"changes\": {}, \"comment\": \"in place\", \"__id__\": \"/z\", \"__run_num__\": 1},"
                                  "\"test_|-bad_|-bad_|-fail_without_changes\": {\"result\": false, \"changes\": {}, \"comment\": \"Failure!\", \"__id__\": \"bad\", \"__run_num__\": 2}}}")))))
      (let ((changes (monk-check agg m)))
        (should (= 1 (length calls)))
        (should (member "state.high" (car calls)))
        (should (member "test=True" (car calls)))
        (should (equal '("/x" "bad") (mapcar #'car (plist-get changes :salt-high))))
        (should (equal "Symlink /x to /y is set for creation"
                       (plist-get (monk-resource-disaggregate agg a changes) :salt)))
        (should (null (monk-resource-disaggregate agg b changes)))
        (should (string-match-p "Failure" (plist-get (monk-resource-disaggregate agg c changes) :salt)))))))

(ert-deftest monk-test-salt-state-functions-cached ()
  (require 'monk-bridge)
  (let ((m (monk-test-machine)) (calls 0))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (&rest _)
                 (cl-incf calls)
                 (monk-exec-create :exit-code 0 :output "{\"local\": [\"file.managed\", \"file.symlink\"]}"))))
      (let ((monk--current-run (monk-run-create)))
        (should (equal '("file.managed" "file.symlink") (monk-salt-state-functions m)))
        (should (equal '("file.managed" "file.symlink") (monk-salt-state-functions m)))
        (should (= 1 calls))))))

(ert-deftest monk-test-salt-matcher-to-cell ()
  (require 'monk-bridge)
  (dolist (case '(("G@os_family:Debian" . (= (fact os-family) "Debian"))
                  ("G@os_family:MacOS" . (= (fact os-family) "Darwin"))
                  ("G@kernel:Darwin" . (class darwin))
                  ("G@os:Deb*" . (match (fact distribution) "\\`Deb.*\\'"))
                  ("P@os:(Ubuntu|Debian)" . (match (fact distribution) "\\`(Ubuntu|Debian)"))
                  ("I@role:web" . (= (var role) "web"))
                  ("J@role:w.b" . (match (var role) "\\`w.b"))
                  ("L@a,b" . (in (name) (list "a" "b")))
                  ("E@web.*" . (match (name) "\\`web.*"))
                  ("N@webs" . (group webs))
                  ("web*" . (match (name) "\\`web.*\\'"))
                  ("kamen" . (= (name) "kamen"))
                  ("G@os_family:Debian and not L@a,b"
                   . (and (= (fact os-family) "Debian") (not (in (name) (list "a" "b")))))
                  ("( G@os:Ubuntu or G@os:Debian ) and I@role:web"
                   . (and (or (= (fact distribution) "Ubuntu") (= (fact distribution) "Debian"))
                          (= (var role) "web")))
                  ("G@os_family:Debian not N@skip"
                   . (and (= (fact os-family) "Debian") (not (group skip))))))
    (should (equal (cdr case) (monk-salt-matcher-to-cell (car case)))))
  (should-error (monk-salt-matcher-to-cell "S@10.0.0.0/8") :type 'monk-guard-error)
  (should-error (monk-salt-matcher-to-cell "G@nokey") :type 'monk-guard-error)
  (should-error (monk-salt-matcher-to-cell "( G@a:b") :type 'monk-guard-error)
  ;; the translation evaluates
  (let ((machines (monk-test-cell-machines)))
    (should (equal "kamen" (monk-test-cell-names (monk-salt-matcher-to-cell "G@os_family:Debian") machines)))
    (should (equal "foxy,moi" (monk-test-cell-names (monk-salt-matcher-to-cell "G@kernel:Darwin and not L@kamen") machines)))))

(ert-deftest monk-test-bridge-parse-json-skips-noise ()
  (require 'monk-bridge)
  (should (equal '((local . 1)) (monk-bridge--parse-json "[ERROR   ] tls import failed\n{\"local\": 1}")))
  (should (null (monk-bridge--parse-json "garbage"))))

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
                     (monk-line-in-file (concat dir "fragile") "x")))
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
        (should (equal "Would" (car (aref monk-ui--columns 4))))
        (should (= 8 (length monk-ui--columns)))
        (let ((header (substring-no-properties (monk-ui--header-line))))
          (should (string-match-p "PLAN" header))
          (should (string-match-p "to apply" header)))
        (let ((apply (monk-apply plan)))
          (setq monk-ui--run apply)
          (monk-ui-refresh)
          (should (= 9 (length monk-ui--columns)))
          (should (equal "Plan" (car (aref monk-ui--columns 7))))
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
                     (plist-get (monk-run-options run) :only)))
      ;; the summaries name what a partial run left out instead of
      ;; counting it as skipped
      (should (= 2 (monk-run-unselected-count run)))
      (should (string-match-p "0 skipped, 2 not selected" (monk-run-summary run)))
      (require 'monk-cli)
      (let ((line (substring-no-properties (monk-cli-summary-line run))))
        (should (string-match-p "1 repaired" line))
        (should-not (string-match-p "skipped" line))
        (should-not (string-match-p "dry run" line))
        (should (string-match-p "partial: 1 of 3" line)))
      ;; a plan is marked as one, first
      (let* ((plan (monk-orchestrate (list m) policy :dry-run t))
             (line (substring-no-properties (monk-cli-summary-line plan))))
        (should (string-match-p "\\`\\(◌\\|PLAN\\) dry run" line))))))

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
      ;; A file inside a managed directory maps to its own resource only:
      ;; the directory manages its mode and owner, which a file written
      ;; inside it cannot change, so scratch files under it are not drift.
      (should (equal (list (concat dir "d/inner"))
                     (mapcar (lambda (n) (oref (monk-node-resource n) name))
                             (monk-watch--nodes-for-file w (concat dir "d/inner")))))
      (should-not (monk-watch--nodes-for-file w (concat dir "d/scratch")))
      ;; the directory itself still is
      (should (equal (list (concat dir "d"))
                     (mapcar (lambda (n) (oref (monk-node-resource n) name))
                             (monk-watch--nodes-for-file w (concat dir "d")))))
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
        (should (equal (monk-result-diff-text orig) (monk-result-diff-text copy)))
        (should (equal (monk-result-comment orig) (monk-result-comment copy)))
        (should (= 1.0 (monk-result-degree copy)))
        ;; changes come back as the plist every consumer expects
        (should (equal '(:create t) (monk-result-changes copy)))
        (should (equal (monk-run-passes run) (monk-run-passes back)))
        (should-not (monk-run-dry-run back))
        (should (string-match-p "\"status\":\"repaired\"" (nth 1 lines)))
        (should (string-match-p "\"changed\":true" (nth 1 lines)))
        (should (string-match-p (format "\"rgi\":%d" monk-event-version) (car lines)))))))

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

;;;; CELL and CEL

(defun monk-test-cell-machines ()
  "Return three machines with distinct facts, vars, and groups for guard tests."
  (let ((mac (monk-local-machine :name "foxy"))
        (moi (monk-machine :name "moi" :address "moi.local"))
        (pi (monk-machine :name "kamen" :address "kamen.local")))
    (oset mac groups '(macs home))
    (oset mac vars '((home_leader . t) (disk . 50)))
    (oset mac facts '((kernel . "Darwin") (arch . "arm64") (os-family . "Darwin")))
    (oset moi groups '(macs home))
    (oset moi facts '((kernel . "Darwin") (arch . "x86_64") (os-family . "Darwin")))
    (oset pi groups '(unix home))
    (oset pi vars '((disk . 200)))
    (oset pi facts '((kernel . "Linux") (arch . "aarch64") (os-family . "Debian")
                     (distribution . "Raspbian")))
    (dolist (m (list mac moi pi))
      (oset m classes (monk-hard-classes m)))
    (list mac moi pi)))

(defun monk-test-cell-names (guard machines)
  "Return the names of MACHINES that GUARD matches, as one string."
  (mapconcat (lambda (m) (oref m name)) (monk-cell-select guard machines) ","))

(ert-deftest monk-test-cell-eval-operators ()
  "Every CELL operator evaluates against a machine."
  (pcase-let ((`(,mac ,_moi ,pi) (monk-test-cell-machines)))
    (should (monk-cell-eval mac '(class darwin)))
    (should-not (monk-cell-eval mac '(class linux)))
    (should (monk-cell-eval mac '(group macs)))
    (should (equal "arm64" (monk-cell-eval mac '(fact arch))))
    (should (equal 50 (monk-cell-eval mac '(var disk))))
    (should (equal "foxy" (monk-cell-eval mac '(name))))
    (should (monk-cell-eval mac '(local)))
    (should-not (monk-cell-eval pi '(local)))
    (should (monk-cell-eval mac '(has (var disk))))
    (should-not (monk-cell-eval mac '(has (fact distribution))))
    (should (monk-cell-eval pi '(has (fact distribution))))
    (should (monk-cell-eval pi '(and (class linux) (> (var disk) 100))))
    (should (monk-cell-eval mac '(or (class linux) (= (fact arch) "arm64"))))
    (should (monk-cell-eval mac '(not (class linux))))
    (should (equal "big" (monk-cell-eval pi '(if (>= (var disk) 200) "big" "small"))))
    (should (monk-cell-eval mac '(/= (fact arch) "x86_64")))
    (should (monk-cell-eval mac '(in "arm64" (list "arm64" "aarch64"))))
    (should (monk-cell-eval mac '(in "macs" (groups))))
    (should (monk-cell-eval pi '(starts-with (name) "ka")))
    (should (monk-cell-eval pi '(ends-with (fact distribution) "bian")))
    (should (monk-cell-eval pi '(contains (fact distribution) "asp")))
    (should (monk-cell-eval pi '(match (fact kernel) "^Lin")))
    (let ((case-fold-search t))
      (should-not (monk-cell-eval pi '(match (fact kernel) "^lin"))))
    (should (equal 4 (monk-cell-eval mac '(size "abcd"))))
    (should (equal 2 (monk-cell-eval mac '(size (groups)))))
    (should (equal 7 (monk-cell-eval mac '(+ 3 4))))
    (should (equal "ab" (monk-cell-eval mac '(+ "a" "b"))))
    (should (equal 1 (monk-cell-eval mac '(% 7 3))))
    (should (equal "x" (monk-cell-eval mac "x")))
    (should (null (monk-cell-eval mac '(fact missing))))))

(ert-deftest monk-test-cell-eval-short-circuit ()
  "and/or stop at the first deciding part, so a bad later part never runs."
  (let ((m (car (monk-test-cell-machines))))
    (should-not (monk-cell-eval m '(and (class linux) (frob))))
    (should (monk-cell-eval m '(or (class darwin) (frob))))
    (should-error (monk-cell-eval m '(and (class darwin) (frob))) :type 'monk-guard-error)))

(ert-deftest monk-test-cell-errors ()
  "Bad forms signal `monk-guard-error' with a message that names the problem."
  (let ((m (car (monk-test-cell-machines))))
    (dolist (case '(((frob 1) . "Unknown CELL operator")
                    ((fact) . "takes 1 argument")
                    ((= 1) . "takes 2 arguments")
                    ((has (name)) . "has needs")
                    ((< "a" 1) . "cannot compare")
                    ((size 3) . "needs a string or list")
                    ((/ 1 0) . "division by zero")
                    (frob . "Not a CELL form")))
      (let ((err (should-error (monk-cell-eval m (car case)) :type 'monk-guard-error)))
        (should (string-match-p (cdr case) (cadr err)))))
    (should-error (monk-cell-check '(and (class a) (nope))) :type 'monk-guard-error)
    (should (equal '(and (class a)) (monk-cell-check '(and (class a)))))))

(ert-deftest monk-test-cell-select-and-matches ()
  "Selection filters machines; matches keeps errors per machine."
  (let ((machines (monk-test-cell-machines)))
    (should (equal "foxy,moi" (monk-test-cell-names '(class darwin) machines)))
    (should (equal "kamen" (monk-test-cell-names '(> (var disk) 100) machines)))
    (should (equal "foxy" (monk-test-cell-names '(local) machines)))
    (should (equal "foxy,moi,kamen" (monk-test-cell-names '(group home) machines)))
    ;; moi has no disk var: (> nil 100) signals, so moi does not match
    ;; in select and carries the error in matches.
    (should (equal "kamen" (monk-test-cell-names '(> (var disk) 100) machines)))
    (let ((matches (monk-cell-matches '(> (var disk) 100) machines)))
      (should (= 3 (length matches)))
      (should-not (cdr (assoc (car machines) matches)))
      (should (monk-cell-error-p (cdr (assoc (nth 1 machines) matches))))
      (should (eq t (cdr (assoc (nth 2 machines) matches)))))))

(ert-deftest monk-test-cell-explain ()
  "Explanations name the deciding clause."
  (pcase-let ((`(,mac ,_moi ,pi) (monk-test-cell-machines)))
    (should (equal "class linux is absent"
                   (monk-cell-explain mac '(and (class darwin) (class linux)))))
    (should (string-match-p "every part is false"
                            (monk-cell-explain mac '(or (class linux) (= (fact arch) "x86_64")))))
    (should (equal "(class darwin) is true, so (not (class darwin)) is false"
                   (monk-cell-explain mac '(not (class darwin)))))
    (should (equal "'darwin' in classes is true, so !'darwin' in classes is false"
                   (monk-cell-explain mac '(not (class darwin)) 'cel)))
    (should (string-match-p "(var disk) = 200"
                            (monk-cell-explain pi '(> (var disk) 100))))
    (should (string-match-p "is true" (monk-cell-explain pi '(and (class linux) (group unix)))))))

(ert-deftest monk-test-cel-parse ()
  "CEL text parses to the expected CELL forms."
  (dolist (case '(("'darwin' in classes" . (class darwin))
                  ("'macs' in groups" . (group macs))
                  ("'arch' in facts" . (has (fact arch)))
                  ("facts.arch == 'arm64'" . (= (fact arch) "arm64"))
                  ("facts['arch'] != \"x86_64\"" . (/= (fact arch) "x86_64"))
                  ("vars.disk > 100 && !local" . (and (> (var disk) 100) (not (local))))
                  ("a in classes || b in classes" . error)
                  ("'a' in classes || 'b' in classes || 'c' in classes"
                   . (or (class a) (class b) (class c)))
                  ("has(vars.disk)" . (has (var disk)))
                  ("size(classes) >= 3" . (>= (size (classes)) 3))
                  ("name.startsWith('ka')" . (starts-with (name) "ka"))
                  ("facts.kernel.matches('^Lin')" . (match (fact kernel) "^Lin"))
                  ("local ? 'here' : 'there'" . (if (local) "here" "there"))
                  ("facts.arch in ['arm64', 'aarch64']" . (in (fact arch) (list "arm64" "aarch64")))
                  ("1 + 2 * 3" . (+ 1 (* 2 3)))
                  ("(1 + 2) * 3" . (* (+ 1 2) 3))
                  ("-vars.disk" . (- 0 (var disk)))
                  ("-5" . -5)
                  ("true && !false" . (and t (not nil)))
                  ("'it\\'s'" . "it's")
                  ("" . error)
                  ("facts.arch ==" . error)
                  ("foo == 1" . error)
                  ("facts.a.b" . (get (fact a) b))
                  ("facts.local.monk.role == 'ws'" . (= (get (fact local) monk role) "ws"))
                  ("classes.arch" . error)
                  ("frob(1)" . error)
                  ("name.frob(1)" . error)
                  ("1 2" . error)
                  ("facts.arch == 'x' @" . error)))
    (if (eq (cdr case) 'error)
        (should-error (monk-cel-parse (car case)) :type 'monk-cel-error)
      (should (equal (cdr case) (monk-cel-parse (car case)))))))

(ert-deftest monk-test-cel-roundtrip ()
  "CELL prints as CEL with minimal parentheses and parses back equal."
  (dolist (case '(((and (class darwin) (= (fact arch) "arm64"))
                   . "'darwin' in classes && facts.arch == 'arm64'")
                  ((or (and (class a) (class b)) (class c))
                   . "'a' in classes && 'b' in classes || 'c' in classes")
                  ((and (or (class a) (class b)) (class c))
                   . "('a' in classes || 'b' in classes) && 'c' in classes")
                  ((not (and (class a) (class b))) . "!('a' in classes && 'b' in classes)")
                  ((- 1 (- 2 3)) . "1 - (2 - 3)")
                  ((- (- 1 2) 3) . "1 - 2 - 3")
                  ((* (+ 1 2) 3) . "(1 + 2) * 3")
                  ((if (class a) (if (class b) 1 2) 3) . "'a' in classes ? ('b' in classes ? 1 : 2) : 3")
                  ((in "x" (list "x" "y")) . "'x' in ['x', 'y']")
                  ((starts-with (name) "it's") . "name.startsWith('it\\'s')")
                  ((has (var disk)) . "has(vars.disk)")
                  ((not (not (local))) . "!!local")
                  ((= (get (fact local) monk role) "ws") . "facts.local.monk.role == 'ws'")))
    (should (equal (cdr case) (monk-cell-to-cel (car case))))
    (should (equal (car case) (monk-cel-parse (cdr case))))))

(ert-deftest monk-test-cell-read ()
  "`monk-cell-read' takes CELL in parentheses and CEL otherwise."
  (should (equal '(class a) (monk-cell-read "  (class a) ")))
  (should (equal '(class a) (monk-cell-read "'a' in classes")))
  (should-error (monk-cell-read "(nope a)") :type 'monk-guard-error)
  (should-error (monk-cell-read "nope") :type 'monk-cel-error))

(ert-deftest monk-test-when-guard-skips-with-reason ()
  "A :when guard skips a resource and the comment names the false clause."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-directory (concat dir "linux") :when '(class linux))
                  (monk-directory (concat dir "cel") :when "'darwin' in classes && vars.x == 1")
                  (monk-directory (concat dir "both") :classes '(linux) :when '(= (fact arch) "x86_64"))
                  (monk-directory (concat dir "yes") :when "'linux' in classes && facts.arch == 'x86_64'")
                  (monk-directory (concat dir "fn") :only-if (lambda (_m) nil))))))
      (should (eq monk-status-repaired (monk-test-status run (concat dir "linux"))))
      (should (eq monk-status-skipped (monk-test-status run (concat dir "cel"))))
      (should (equal "class darwin is absent"
                     (monk-result-comment (monk-test-result run (concat dir "cel")))))
      (should (eq monk-status-repaired (monk-test-status run (concat dir "both"))))
      (should (eq monk-status-repaired (monk-test-status run (concat dir "yes"))))
      (should (equal monk-skip-only-if
                     (monk-result-comment (monk-test-result run (concat dir "fn")))))
      (should-not (file-directory-p (concat dir "cel"))))))

(ert-deftest monk-test-when-guard-checked-at-compile ()
  "A bad :when guard fails the compile with the resource name."
  (let ((err (should-error (monk-compile (monk-test-machine)
                                         (lambda (_m) (monk-directory "~/x" :when "nope == 1")))
                           :type 'monk-policy-error)))
    (should (string-match-p "~/x.*bad :when guard.*Unknown name nope" (cadr err))))
  (should-error (monk-compile (monk-test-machine)
                              (lambda (_m) (monk-directory "~/x" :when '(frob))))
                :type 'monk-policy-error))

(ert-deftest monk-test-resource-guard-combines-classes-and-when ()
  ":classes and :when form one CELL guard; the compile stores the parsed form."
  (let* ((resources (monk-compile (monk-test-machine)
                                  (lambda (_m)
                                    (monk-directory "~/a" :classes '(x y))
                                    (monk-directory "~/b" :when "'x' in classes")
                                    (monk-directory "~/c" :classes '(x) :when '(local))
                                    (monk-directory "~/d"))))
         (guard (lambda (i) (monk-resource-guard (nth i resources)))))
    (should (equal '(and (class x) (class y)) (funcall guard 0)))
    (should (equal '(class x) (funcall guard 1)))
    (should (equal '(and (class x) (local)) (funcall guard 2)))
    (should (null (funcall guard 3)))
    (should (equal '(class x) (oref (nth 1 resources) when)))))

(ert-deftest monk-test-machine-describe ()
  "The description lists what guards can see, sorted by key."
  (let* ((m (car (monk-test-cell-machines)))
         (d (monk-machine-describe m))
         (text (monk-machine-describe-string m "  ")))
    (should (memq 'darwin (alist-get 'classes d)))
    (should (equal '(macs home) (alist-get 'groups d)))
    (should (equal '(arch kernel os-family) (mapcar #'car (alist-get 'facts d))))
    (should (equal '(disk home_leader) (mapcar #'car (alist-get 'vars d))))
    (should (string-match-p "^  classes: .*darwin" text))
    (should (string-match-p "^    arch +\"arm64\"" text))))

(ert-deftest monk-test-report-verbose-describes-machine ()
  "The verbose text report includes the machine facts under its header."
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
           (verbose (monk-run-report-string run t))
           (terse (monk-run-report-string run)))
      (should (string-match-p "   classes: " verbose))
      (should (string-match-p "kernel +\"Linux\"" verbose))
      (should-not (string-match-p "classes: " terse)))))

(ert-deftest monk-test-ui-apply-from-detail-buffer ()
  "a in a plan's detail buffer applies the plan and redraws the detail."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m) (monk-directory (concat dir "d"))))
           (m (monk-test-machine))
           (monk-log-level nil)
           (plan (monk-plan m policy))
           (detail (format monk-ui-detail-buffer-format "testbox")))
      (with-current-buffer (monk-ui--buffer)
        (monk-ui-mode)
        (setq monk-ui--run plan)
        (monk-ui-refresh)
        (monk-ui-detail-buffer "testbox"))
      (with-current-buffer detail
        (should (eq (key-binding "a") 'monk-ui-apply))
        (should (eq monk-ui--run plan))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (save-window-excursion (monk-ui-apply))))
      (should (file-directory-p (concat dir "d")))
      ;; *monk* now shows the apply, and the detail buffer followed it
      (with-current-buffer (monk-ui--buffer)
        (should-not (monk-run-dry-run monk-ui--run))
        (should (eq plan (monk-run-plan monk-ui--run))))
      (with-current-buffer detail
        (should-not (monk-run-dry-run monk-ui--run))
        (goto-char (point-min))
        (should (search-forward " APPLY " nil t))
        (should (search-forward "repaired" nil t)))
      (kill-buffer detail)
      (kill-buffer (monk-ui--buffer)))))

;;;; Generic package and service; derived groups; cross-host CELL

(ert-deftest monk-test-package-and-service-dispatch ()
  "monk-package and monk-service pick the resource type from the manager facts."
  (let* ((mac (monk-test-machine (append '((pkg-mgr . "homebrew") (service-mgr . "launchd")) monk-test-fake-facts)))
         (deb (monk-test-machine (append '((pkg-mgr . "apt") (service-mgr . "systemd")) monk-test-fake-facts)))
         (odd (monk-test-machine (append '((pkg-mgr . nil) (service-mgr . nil)) monk-test-fake-facts)))
         (policy (lambda (_m)
                   (monk-package "tools" '("jq"))
                   (monk-service "sshd" :started t))))
    (should (equal '(monk-brew-resource monk-launchd-resource)
                   (mapcar #'eieio-object-class-name (monk-compile mac policy))))
    (should (equal '(monk-apt-resource monk-systemd-unit-resource)
                   (mapcar #'eieio-object-class-name (monk-compile deb policy))))
    (should (equal "sshd.service" (oref (cadr (monk-compile deb policy)) name)))
    (should (eq t (oref (cadr (monk-compile mac policy)) loaded)))
    (let ((rs (monk-compile odd policy)))
      (should (equal '(monk-unsupported-resource monk-unsupported-resource)
                     (mapcar #'eieio-object-class-name rs)))
      (should (string-match-p "no supported package manager" (monk-resource-available-p (car rs) odd))))
    ;; the manager facts become classes
    (should (memq 'homebrew (oref mac classes)))
    (should (memq 'systemd (oref deb classes)))))

(ert-deftest monk-test-probe-managers ()
  "The managers probe reads one sh -c and prefers brew and launchd on Darwin."
  (let ((m (monk-test-machine)))
    (cl-letf (((symbol-function 'monk-exec-string)
               (lambda (_m &rest _) "apt-get\nsystemctl\n")))
      (should (equal '((pkg-mgr . "apt") (service-mgr . "systemd")) (monk--probe-managers m))))
    (let ((mac (monk-test-machine '((kernel . "Darwin")))))
      (cl-letf (((symbol-function 'monk-exec-string)
                 (lambda (_m &rest _) "brew\napt-get\n")))
        (should (equal '((pkg-mgr . "homebrew") (service-mgr . "launchd")) (monk--probe-managers mac)))))
    (cl-letf (((symbol-function 'monk-exec-string) (lambda (_m &rest _) "")))
      (should (equal '((pkg-mgr . nil) (service-mgr . nil)) (monk--probe-managers m))))))

(ert-deftest monk-test-derived-groups ()
  "monk-group and monk-keyed-groups put machines into groups from their facts."
  (let ((monk-inventory nil) (monk-inventory-derived nil))
    (monk-inventory-define '(w :hosts ((foxy) (moi :address "m") (kamen :address "k"))))
    (dolist (spec '(("foxy" "Darwin" "MacOS" 50) ("moi" "Darwin" "MacOS" 300) ("kamen" "Linux" "Raspbian" 8)))
      (let ((m (monk-inventory-find (car spec))))
        (oset m facts `((kernel . ,(nth 1 spec)) (distribution . ,(nth 2 spec)) (disk . ,(nth 3 spec))))
        (oset m classes (monk-hard-classes m))))
    (monk-group 'big-disk '(> (fact disk) 100))
    (monk-group 'macs-derived "facts.kernel == 'Darwin'")
    (monk-keyed-groups '(fact distribution) "distro")
    (should (equal '(w macs-derived distro_macos) (oref (monk-inventory-find 'foxy) groups)))
    (should (equal '(w big-disk macs-derived distro_macos) (oref (monk-inventory-find 'moi) groups)))
    (should (equal '(w distro_raspbian) (oref (monk-inventory-find 'kamen) groups)))
    ;; groups are classes and selectors
    (should (memq 'distro_raspbian (oref (monk-inventory-find 'kamen) classes)))
    (should (equal '("moi") (mapcar (lambda (m) (oref m name)) (monk-inventory-select 'big-disk))))
    ;; facts arriving later apply the rules through the hook
    (let ((late (monk-inventory-find 'foxy)))
      (oset late facts '((kernel . "Darwin") (distribution . "MacOS") (disk . 500)))
      (run-hook-with-args 'monk-facts-ready-functions late)
      (should (memq 'big-disk (oref late groups))))
    (monk-inventory-clear-derived)
    (should (equal '(w) (oref (monk-inventory-find 'moi) groups)))))

(ert-deftest monk-test-cell-cross-host-reads ()
  "machine-fact, machine-var, and machine-class read another inventory machine."
  (let ((monk-inventory nil))
    (monk-inventory-define '(w :hosts ((db :address "d" :vars ((role . "primary"))) (web))))
    (let ((db (monk-inventory-find 'db)) (web (monk-inventory-find 'web)))
      (oset db facts '((kernel . "Linux") (address . "10.0.0.5")))
      (oset db classes (monk-hard-classes db))
      (oset web facts '((kernel . "Linux")))
      (oset web classes (monk-hard-classes web))
      (should (equal "10.0.0.5" (monk-cell-eval web '(machine-fact "db" address))))
      (should (equal "primary" (monk-cell-eval web '(machine-var "db" role))))
      (should (monk-cell-eval web '(machine-class "db" linux)))
      (should (monk-cell-eval web '(= (machine-fact "db" address) "10.0.0.5")))
      (should-error (monk-cell-eval web '(machine-fact "nope" x)) :type 'monk-guard-error)
      (should (equal '(machine-fact "db" address) (monk-cell-check '(machine-fact "db" address)))))))

;;;; facts.d and nested access

(ert-deftest monk-test-facts-d-local-fact ()
  "JSON, INI, and executable .fact files become (fact local); CELL reaches inside."
  (monk-test-with-tmpdir dir
    (let ((d (concat dir "facts.d/")))
      (make-directory d)
      (with-temp-file (concat d "monk.fact") (insert "{\"role\": \"workstation\", \"tier\": 2}"))
      (with-temp-file (concat d "site.fact") (insert "[general]\nregion = eu\n[rack]\nrow=3\n"))
      (with-temp-file (concat d "dyn.fact") (insert "#!/bin/sh\necho '{\"load\": 0.5}'\n"))
      (set-file-modes (concat d "dyn.fact") #o755)
      (with-temp-file (concat d "bad.fact") (insert "not json = and not ini\n"))
      (let* ((monk-facts-d-directories (list d))
             (m (monk-local-machine :name "fd")))
        (cl-letf (((symbol-function 'monk--probe-uname) (lambda (_m) '((kernel . "Linux") (arch . "x") (hostname . "h") (nodename . "h"))))
                  ((symbol-function 'monk--probe-os) (lambda (_m) nil))
                  ((symbol-function 'monk--probe-env) (lambda (_m) nil)))
          (let ((monk-fact-cache-functions nil)) (monk-gather-facts m)))
        (let ((local (monk-machine-fact m 'local)))
          (should (equal "workstation" (cdr (assq 'role (cdr (assq 'monk local))))))
          (should (equal 2 (cdr (assq 'tier (cdr (assq 'monk local))))))
          (should (equal "eu" (cdr (assq 'region (cdr (assq 'general (cdr (assq 'site local))))))))
          (should (equal "3" (cdr (assq 'row (cdr (assq 'rack (cdr (assq 'site local))))))))
          (should (equal 0.5 (cdr (assq 'load (cdr (assq 'dyn local))))))
          ;; unparseable is kept as its own (empty) alist, not an error
          (should (assq 'bad local)))
        (should (monk-cell-eval m '(= (get (fact local) monk role) "workstation")))
        (should (monk-cell-eval m (monk-cell-guard "facts.local.monk.role == 'workstation'")))
        (should-not (monk-cell-eval m (monk-cell-guard "facts.local.monk.role == 'other'")))
        (should (monk-cell-eval m '(= (get (fact local) site rack row) "3")))
        (should (null (monk-cell-eval m '(get (fact local) nope deeper))))
        (should (equal "facts.local.monk.role" (monk-cell-to-cel '(get (fact local) monk role))))))))

;;;; Fact cache

(ert-deftest monk-test-fact-cache-round-trip-and-ttl ()
  "Live facts are stored; a fresh cache spares gathering; a stale one does not."
  (monk-test-with-history
    (let* ((m (monk-local-machine :name "cached"))
           (gathers 0)
           (monk-log-level nil))
      (cl-letf (((symbol-function 'monk--probe-uname)
                 (lambda (_m) (cl-incf gathers) '((kernel . "Linux") (arch . "x") (nodename . "n") (hostname . "n"))))
                ((symbol-function 'monk--probe-os) (lambda (_m) '((os-family . "Debian"))))
                ((symbol-function 'monk--probe-env) (lambda (_m) '((home . "/h") (user . "u")))))
        ;; first compile gathers and stores
        (monk--nodes-for (list m) (lambda (_m) (monk-directory "~/d")))
        (should (= 1 gathers))
        (should (eq 'live (oref m facts-source)))
        (should (monk-machine-facts-fresh-p m))
        (should (string-empty-p (monk-ui-facts-badge m)))
        (should (equal "Linux" (alist-get 'kernel (car (monk-history-load-facts m)))))
        ;; a new machine object for the same host loads from the cache, no gather
        (let ((m2 (monk-local-machine :name "cached")))
          (monk--nodes-for (list m2) (lambda (_m) (monk-directory "~/d")))
          (should (= 1 gathers))
          (should (eq 'cache (oref m2 facts-source)))
          (should (monk-machine-facts-fresh-p m2))
          (should (memq 'debian (oref m2 classes)))
          (should (string-prefix-p "·" (monk-ui-facts-badge m2))))
        ;; past the TTL the cache is ignored and facts are gathered again
        (let ((m3 (monk-local-machine :name "cached"))
              (monk-fact-ttl 0))
          (monk--nodes-for (list m3) (lambda (_m) (monk-directory "~/d")))
          (should (= 2 gathers))
          (should (eq 'live (oref m3 facts-source))))
        ;; a stale entry shows as stale when loaded by force
        (let ((m4 (monk-local-machine :name "cached")))
          (let ((monk-fact-ttl most-positive-fixnum)) (monk-facts-load m4))
          (let ((monk-fact-ttl 0))
            (should-not (monk-machine-facts-fresh-p m4))
            (should (string-prefix-p "!" (monk-ui-facts-badge m4)))))
        ;; flush forgets both
        (monk-facts-flush (list m))
        (should (null (oref m facts)))
        (should (null (monk-history-load-facts m)))
        (should (equal "?" (substring-no-properties (monk-ui-facts-badge m))))))))

(ert-deftest monk-test-fact-cache-serves-unreachable-machine ()
  "A machine that is off compiles with its last known facts, marked stale."
  (monk-test-with-history
    (let* ((monk-log-level nil)
           (dead (monk-test-dead-machine "dead")))
      ;; seed the cache as if it had answered yesterday
      (oset dead gathered (time-subtract (current-time) (* 2 86400)))
      (monk-history-store-facts dead)
      (let ((again (monk-machine :name "dead" :address "dead.invalid")))
        (let ((run (monk-test-with-dead-exec '("dead")
                     (monk-orchestrate (list again)
                                       (lambda (_m) (monk-apt "pkgs" '("jq") :classes nil))
                                       :passes 1))))
          ;; classes came from the cache: debian from the fake facts
          (should (memq 'debian (oref again classes)))
          (should (eq 'cache (oref again facts-source)))
          (should-not (monk-machine-facts-fresh-p again))
          (should (string-prefix-p "!" (monk-ui-facts-badge again)))
          (should (equal '("dead") (mapcar #'car (monk-run-unreachable run)))))))))

;;;; Structured diffs

(ert-deftest monk-test-structured-diff-content-and-attributes ()
  "A file whose content and mode both differ carries two entries; each renders."
  (monk-test-with-tmpdir dir
    (let* ((f (concat dir "f")))
      (with-temp-file f (insert "old\n"))
      (set-file-modes f #o644)
      (let* ((run (monk-plan (monk-test-machine) (lambda (_m) (monk-file f :content "new\n" :mode #o600))))
             (r (monk-test-result run f))
             (entries (monk-diff-entries (monk-result-diff r)))
             (text (monk-result-diff-text r)))
        (should (= 2 (length entries)))
        (should (eq 'content (monk-diff-entry-kind (car entries))))
        (should (eq 'attributes (monk-diff-entry-kind (cadr entries))))
        (should (equal '((mode . 420)) (monk-diff-entry-before (cadr entries))))
        (should (string-match-p "^-old$" text))
        (should (string-match-p "^\\+new$" text))
        (should (string-match-p "mode: 0644 → 0600" text))
        ;; the event carries Ansible's shape and comes back equal
        (let* ((event (monk-event-result r run 1 0))
               (data (alist-get 'diff event))
               (back (monk-event-diff-to-entries data)))
          (should (= 2 (length data)))
          (should (equal "old\n" (alist-get 'before (aref data 0))))
          (should (equal "content" (alist-get 'kind (aref data 0))))
          (should (equal "attributes" (alist-get 'kind (aref data 1))))
          (should (equal text (monk-diff-render back))))
        ;; a mode-only change on a directory has only the attributes entry
        (let* ((d (concat dir "d")))
          (make-directory d) (set-file-modes d #o755)
          (let ((dr (monk-test-result (monk-plan (monk-test-machine) (lambda (_m) (monk-directory d :mode #o700))) d)))
            (should (= 1 (length (monk-diff-entries (monk-result-diff dr)))))
            (should (string-match-p "mode: 0755 → 0700" (monk-result-diff-text dr)))))))))

(ert-deftest monk-test-diff-accepts-plain-string ()
  "An older or foreign diff string still renders and round-trips."
  (let ((r (monk-result-create :machine "m" :name "x" :type "t" :status 'repaired
                               :diff "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-a\n+b\n")))
    (should (equal (monk-result-diff r) (monk-result-diff-text r)))
    (should (equal (monk-result-diff-text r)
                   (monk-diff-render (monk-event-diff-to-entries (monk-event-diff (monk-result-diff r))))))))

;;;; changed-when, failed-when, until, no-log

(ert-deftest monk-test-command-changed-when ()
  "A query command runs every pass; the predicate decides kept or repaired."
  (monk-test-with-tmpdir dir
    (let* ((marker (concat dir "flag"))
           (policy (lambda (_m)
                     ;; prints "changed" the first time, "same" after, and records the calls
                     (monk-command "q" "sh"
                                   :args (list "-c" (format "echo run >> %s.log; if [ -e %s ]; then echo same; else touch %s; echo changed; fi"
                                                            marker marker marker))
                                   :changed-when '(= (stdout) "changed"))))
           (first (monk-test-converge policy)))
      (should (eq 'repaired (monk-test-status first "q")))
      (should (equal "changed" (monk-result-comment (monk-test-result first "q"))))
      ;; pass two ran it again and saw "same" -> kept; converged in 2 passes
      (should (= 2 (monk-run-passes first)))
      (should (= 2 (with-temp-buffer (insert-file-contents (concat marker ".log")) (count-lines 1 (point-max)))))
      (let ((second (monk-test-converge policy :passes 1)))
        (should (eq 'kept (monk-test-status second "q")))
        (should (equal "same" (monk-result-comment (monk-test-result second "q")))))
      ;; a function predicate works too, and sees the exec
      (should (eq 'kept (monk-test-status
                         (monk-test-converge (lambda (_m) (monk-command "f" "true"
                                                                        :changed-when (lambda (exec) (not (monk-exec-ok-p exec)))))
                                             :passes 1)
                         "f"))))))

(ert-deftest monk-test-command-failed-when ()
  "failed-when replaces the exit-code rule in both directions."
  (let ((run (monk-test-converge
              (lambda (_m)
                ;; exits 1 but that is fine
                (monk-command "ok-nonzero" "sh" :args '("-c" "echo fine; exit 1") :creates "~/never"
                              :failed-when '(contains (stdout) "ERROR"))
                ;; exits 0 but says ERROR
                (monk-command "bad-zero" "sh" :args '("-c" "echo ERROR: disk") :creates "~/never"
                              :failed-when '(contains (stdout) "ERROR"))
                ;; rc form
                (monk-command "rc" "sh" :args '("-c" "exit 3") :creates "~/never"
                              :failed-when '(> (rc) 5)))
              :passes 1)))
    (should (eq 'repaired (monk-test-status run "ok-nonzero")))
    (should (eq 'failed (monk-test-status run "bad-zero")))
    (should (string-match-p "failed-when held: ERROR: disk" (monk-result-comment (monk-test-result run "bad-zero"))))
    (should (eq 'repaired (monk-test-status run "rc")))))

(ert-deftest monk-test-command-predicate-errors-and-exclusive ()
  (should-error (monk-compile (monk-test-machine)
                              (lambda (_m) (monk-command "x" "true" :creates "~/a" :changed-when '(= (rc) 0))))
                :type 'monk-argument-error)
  ;; a broken predicate fails the resource, with the reason
  (let ((run (monk-test-converge (lambda (_m) (monk-command "p" "true" :changed-when '(frob))) :passes 1)))
    (should (eq 'failed (monk-test-status run "p")))
    (should (string-match-p "Unknown CELL operator frob" (monk-result-comment (monk-test-result run "p"))))))

(ert-deftest monk-test-retry-until-predicate ()
  "An :until function stops the retries when it says so."
  (monk-test-with-tmpdir dir
    (let* ((tries 0)
           (waits nil))
      (cl-letf (((symbol-function 'sit-for) (lambda (secs &rest _) (push secs waits) t)))
        (let ((run (monk-test-converge
                    (lambda (_m)
                      (monk-command "poll" "sh"
                                    :args (list "-c" (format "echo $(( $(cat %s/n 2>/dev/null || echo 0) + 1 )) > %s/n; cat %s/n" dir dir dir))
                                    :changed-when '(= (stdout) "3")
                                    :retry (list :attempts 5 :interval 0
                                                 :until (lambda (result) (eq (monk-result-status result) 'repaired)))))
                    :passes 1)))
          (ignore tries)
          (should (eq 'repaired (monk-test-status run "poll")))
          (should (string-match-p "attempt 3 of 5" (monk-result-comment (monk-test-result run "poll")))))))))

(ert-deftest monk-test-no-log-censors-everything ()
  "A :no-log resource's changes, diff, and comment never reach an event or report."
  (monk-test-with-tmpdir dir
    (let* ((secret "hunter2-very-secret")
           (monk-log-level nil)
           (run (monk-test-converge
                 (lambda (_m)
                   (monk-file (concat dir "creds") :content (concat secret "\n") :no-log t)
                   (monk-line-in-file (concat dir "absent") secret :no-log t))
                 :passes 1))
           (r (monk-test-result run (concat dir "creds")))
           (f (seq-find (lambda (r) (string-prefix-p (concat dir "absent#no-log-") (monk-result-name r)))
                        (monk-run-results run))))
      (should (eq 'repaired (monk-result-status r)))
      (should (monk-result-censored r))
      (should (equal monk-censored (monk-result-comment r)))
      (should (null (monk-result-diff r)))
      (should (equal '(:censored t) (monk-result-changes r)))
      (should (eq 'failed (monk-result-status f)))
      (should (string-match-p "censored" (monk-result-comment f)))
      ;; nothing leaks through the event, the text report, or the describe line
      (let ((text (mapconcat #'monk-event-line (monk-run-events run) "")))
        (should-not (string-search secret text))
        (should (string-search "\"censored\":true" text)))
      (should-not (string-search secret (monk-run-report-string run t)))
      (should (equal monk-censored (monk-describe-changes (monk-result-changes r))))
      ;; the file itself was written
      (should (equal (concat secret "\n")
                     (with-temp-buffer (insert-file-contents (concat dir "creds")) (buffer-string))))
      ;; and the round trip keeps the flag
      (should (monk-result-censored
               (monk-event-to-result (monk-event-parse (monk-event-line (monk-event-result r run 1 0)))))))))

;;;; Unreachable machines

(defun monk-test-dead-machine (name &optional ignore)
  "Return a remote machine NAME whose every command fails like a dead ssh host."
  (let ((m (monk-machine :name name :address (concat name ".invalid")
                         :ignore-unreachable ignore)))
    (oset m facts monk-test-fake-facts)
    (oset m classes (monk-hard-classes m))
    m))

(defmacro monk-test-with-dead-exec (dead-names &rest body)
  "Run BODY with `monk-exec' failing with ssh exit 255 for machines in DEAD-NAMES."
  (declare (indent 1))
  `(cl-letf* ((real (symbol-function 'monk-exec))
              ((symbol-function 'monk-exec)
               (lambda (m program &rest args)
                 (if (member (oref m name) ,dead-names)
                     (monk-exec-create :command (cons program args) :exit-code 255
                                       :output "ssh: connect to host dead.invalid port 22: No route to host")
                   (apply real m program args))))
              ((symbol-function 'file-attributes)
               (let ((real-fa (symbol-function 'file-attributes)))
                 (lambda (file &optional id)
                   (if (and (stringp file) (string-match-p "dead\\.invalid" file))
                       (signal 'file-error (list "Opening remote file" "No route to host" file))
                     (funcall real-fa file id))))))
     ,@body))

(ert-deftest monk-test-unreachable-error-p ()
  (should (monk-unreachable-error-p '(monk-exec-error "h" ("ssh") 255 "x")))
  (should (monk-unreachable-error-p '(file-error "Opening" "No route to host" "/ssh:h:/x")))
  (should (monk-unreachable-error-p '(file-error "ssh: connect to host h port 22: Connection refused")))
  (should-not (monk-unreachable-error-p '(monk-exec-error "h" ("brew") 1 "Error: nope")))
  (should-not (monk-unreachable-error-p '(file-error "Opening" "Permission denied" "/x")))
  (should-not (monk-unreachable-error-p '(monk-resource-error "bad"))))

(ert-deftest monk-test-unreachable-machine-marks-all-its-nodes ()
  "A machine that does not answer gets unreachable results, once per resource,
the other machine converges, cross-machine dependents skip with the host named,
the run is not ok, and the exit code says 4."
  (monk-test-with-tmpdir dir
    (let* ((live (car (monk-test-machines "live")))
           (dead (monk-test-dead-machine "dead"))
           (monk-log-level nil)
           (run (monk-test-with-dead-exec '("dead")
                  (monk-orchestrate
                   (list dead live)
                   (lambda (m)
                     (monk-command (format "probe-%s" (oref m name)) "true" :creates "~/never")
                     (monk-command (format "second-%s" (oref m name)) "true" :creates "~/never")
                     (when (equal (oref m name) "live")
                       (monk-directory (concat dir "after-dead") :require '((dead . "probe-dead")))
                       (monk-directory (concat dir "free"))))
                   :passes 2))))
      (should (eq monk-status-unreachable (monk-result-status (monk-run-result run '("dead" . "probe-dead")))))
      (should (eq monk-status-unreachable (monk-result-status (monk-run-result run '("dead" . "second-dead")))))
      ;; real TRAMP answered for the :creates check; either text is right, one line
      (let ((c (monk-result-comment (monk-run-result run '("dead" . "second-dead")))))
        (should (string-match-p "No route to host\\|Tramp failed to connect" c))
        (should-not (string-search "\n" c)))
      (should (eq monk-status-repaired (monk-result-status (monk-run-result run (cons "live" (concat dir "free"))))))
      (should (eq monk-status-skipped (monk-result-status (monk-run-result run (cons "live" (concat dir "after-dead"))))))
      (should (string-match-p "dead is unreachable" (monk-result-comment (monk-run-result run (cons "live" (concat dir "after-dead"))))))
      (should (equal '("dead") (mapcar #'car (monk-run-unreachable run))))
      (should-not (monk-run-ok-p run))
      (should (= 4 (monk-run-exit-code run)))
      (should (= 2 (monk-run-count run monk-status-unreachable)))
      (should (string-match-p "2 unreachable" (monk-run-summary run))))))

(ert-deftest monk-test-unreachable-ignored-machine-keeps-run-ok ()
  (let* ((dead (monk-test-dead-machine "laptop" t))
         (monk-log-level nil)
         (run (monk-test-with-dead-exec '("laptop")
                (monk-orchestrate (list dead)
                                  (lambda (_m) (monk-command "p" "true" :creates "~/never"))
                                  :passes 1))))
    (should (eq monk-status-unreachable (monk-result-status (monk-run-result run '("laptop" . "p")))))
    (should (monk-run-ok-p run))
    (should (= 0 (monk-run-exit-code run)))))

(ert-deftest monk-test-unreachable-at-fact-time ()
  "A machine that dies before facts are gathered still gets nodes, all unreachable."
  (let* ((dead (monk-machine :name "dead" :address "dead.invalid"))
         (monk-log-level nil)
         (run (monk-test-with-dead-exec '("dead")
                (monk-orchestrate (list dead) (lambda (_m) (monk-command "p" "true" :creates "~/never")) :passes 1))))
    (should (equal '("dead") (mapcar #'car (monk-run-unreachable run))))
    (should (eq monk-status-unreachable (monk-result-status (monk-run-result run '("dead" . "p")))))
    (should (= 4 (monk-run-exit-code run)))))

(ert-deftest monk-test-unreachable-exit-code-combines ()
  (monk-test-with-tmpdir dir
    (let* ((live (car (monk-test-machines "live")))
           (dead (monk-test-dead-machine "dead"))
           (monk-log-level nil)
           (run (monk-test-with-dead-exec '("dead")
                  (monk-orchestrate (list dead live)
                                    (lambda (m)
                                      (monk-command "p" "true" :creates "~/never")
                                      (when (equal (oref m name) "live")
                                        (monk-line-in-file (concat dir "absent") "x")))
                                    :passes 1))))
      (should (= 6 (monk-run-exit-code run)))
      (should (equal 6 (alist-get 'exit_code (monk-event-run 'run-end run))))
      (should (equal ["dead"] (alist-get 'unreachable (monk-event-run 'run-end run)))))))

(ert-deftest monk-test-inventory-ignore-unreachable-var ()
  (let ((monk-inventory nil))
    (monk-inventory-define '(w :hosts ((a :vars ((ignore_unreachable . t))) (b))))
    (should (monk-machine-ignores-unreachable-p (monk-inventory-find 'a)))
    (should-not (monk-machine-ignores-unreachable-p (monk-inventory-find 'b)))))

;;;; Salt items: tags, result hook, watch suppression, folds

(ert-deftest monk-test-event-tags ()
  "Every event carries a /-joined tag; names never go into tags."
  (should (equal "monk/run/r1/prog/host/3" (monk-event-tag "run" "r1" "prog" "host" 3)))
  (should (equal "monk/drift/h" (monk-event-tag "drift" "h" nil)))
  (should (equal "monk/x/a_b" (monk-event-tag "x" "a/b")))
  (should (monk-event-tag-matches-p "monk/run" "monk/run/r1/end"))
  (should-not (monk-event-tag-matches-p "monk/run" "monk/runs/r1"))
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
           (events (monk-run-events run))
           (id (monk-run-id run)))
      (should (equal (list (format "monk/run/%s/new" id)
                           (format "monk/run/%s/prog/testbox/0" id)
                           (format "monk/run/%s/end" id))
                     (mapcar (lambda (e) (alist-get 'tag e)) events)))
      (should (eql 0 (alist-get 'seq (nth 1 events))))
      (should (equal (format "monk/drift/testbox")
                     (alist-get 'tag (monk-event-drift '("testbox" . "~/x") "/x" 'changed))))
      (should (equal "changed" (alist-get 'change (monk-event-drift '("testbox" . "~/x") "/x" 'changed))))
      (should (equal (format "monk/run/%s/pass/2" id) (alist-get 'tag (monk-event-pass run 2 1)))))))

(ert-deftest monk-test-result-functions-hook ()
  "Each finished result reaches `monk-result-functions' with the run."
  (monk-test-with-tmpdir dir
    (let* ((seen nil)
           (monk-result-functions
            (list (lambda (result run) (push (cons (monk-result-name result) (and run t)) seen))
                  (lambda (&rest _) (error "boom"))))
           (monk-log-level nil))
      (monk-test-converge (lambda (_m)
                            (monk-directory (concat dir "a"))
                            (monk-directory (concat dir "b")))
                          :passes 1)
      (should (equal (list (cons (concat dir "a") t) (cons (concat dir "b") t))
                     (reverse seen))))))

(ert-deftest monk-test-watch-suppressed-during-any-run ()
  "Events during a run that the watch did not start are ignored."
  (require 'monk-watch)
  (let ((w (monk-watch-create :machines nil :policy #'ignore)))
    (setf (monk-watch-last-run-end w) 0.0)
    (let ((monk-watch-settle 0.0))
      (should-not (monk-watch--suppressed-p w))
      (let ((monk--current-run (monk-run-create)))
        (should (monk-watch--suppressed-p w))))))

(ert-deftest monk-test-ui-detail-folds-kept ()
  "kept and skipped groups start folded; repaired starts open."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-directory (concat dir "d"))
                  (monk-directory (concat dir "e") :classes '(mars))))))
      ;; second run: d kept
      (setq run (monk-test-converge
                 (lambda (_m)
                   (monk-directory (concat dir "d"))
                   (monk-directory (concat dir "new"))
                   (monk-directory (concat dir "e") :classes '(mars)))))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run run)
        (monk-ui-detail-buffer "testbox")
        (with-current-buffer (format monk-ui-detail-buffer-format "testbox")
          (goto-char (point-min))
          (search-forward "repaired (1)")
          (forward-line 1)
          (should-not (get-text-property (point) 'invisible))
          (goto-char (point-min))
          (search-forward "kept (1)")
          (should (looking-at ".*TAB to expand"))
          (forward-line 1)
          (should (get-text-property (point) 'invisible))
          (goto-char (point-min))
          (search-forward "skipped (1)")
          (forward-line 1)
          (should (get-text-property (point) 'invisible)))
        (kill-buffer (format monk-ui-detail-buffer-format "testbox"))))))

(ert-deftest monk-test-history-events-and-seq ()
  "Results carry seq and pass; the run-end event is recorded and queryable by prefix."
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (let* ((run (monk-test-converge (lambda (_m)
                                        (monk-directory (concat dir "a"))
                                        (monk-directory (concat dir "b")))))
             (rows (monk-history-results :run (monk-run-id run))))
        (should (equal '(0 1) (mapcar (lambda (r) (alist-get 'seq r)) rows)))
        (should (equal (list (monk-run-passes run) (monk-run-passes run))
                       (mapcar (lambda (r) (alist-get 'pass r)) rows)))
        (let ((events (monk-history-events "monk/run")))
          (should (= 1 (length events)))
          (should (equal (format "monk/run/%s/end" (monk-run-id run)) (alist-get 'tag (car events))))
          (should (equal "run-end" (alist-get 'event (alist-get 'data (car events))))))
        (should (null (monk-history-events "monk/drift")))
        (monk-history-record-event (monk-event-drift '("testbox" . "~/x") "/x" 'changed))
        (should (= 1 (length (monk-history-events "monk/drift/testbox"))))))))

;;;; Salt items: prereq, listen, retry, verify, order, available, mock, fatal, handled, aggregate

(ert-deftest monk-test-prereq-runs-only-when-target-would-change ()
  "A :prereq resource runs before its target and only when the target would change."
  (monk-test-with-tmpdir dir
    (let* ((log (concat dir "log"))
           (policy (lambda (_m)
                     (monk-command "before" "sh"
                                   :args (list "-c" (format "echo before >> %s" log))
                                   :creates (concat dir "never")
                                   :prereq (list (concat dir "conf")))
                     (monk-file (concat dir "conf") :content "v1\n")))
           (first (monk-test-converge policy :passes 1))
           (second (monk-test-converge policy :passes 1)))
      ;; order: before ran ahead of conf
      (should (equal (list "before" (concat dir "conf"))
                     (list (monk-result-name (car (monk-run-results first)))
                           (monk-result-name (cadr (monk-run-results first))))))
      (should (eq 'repaired (monk-test-status first "before")))
      (should (eq 'repaired (monk-test-status first (concat dir "conf"))))
      (should (equal "before\n" (with-temp-buffer (insert-file-contents log) (buffer-string))))
      ;; second run: conf is kept, so before is skipped with the prereq reason
      (should (eq 'skipped (monk-test-status second "before")))
      (should (equal monk-skip-prereq (monk-result-comment (monk-test-result second "before"))))
      (should (equal "before\n" (with-temp-buffer (insert-file-contents log) (buffer-string)))))))

(ert-deftest monk-test-prereq-ignores-a-target-that-is-itself-skipped ()
  "Salt #68438: a target skipped by its own guard does not fire the prereq."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-directory (concat dir "before") :prereq (list (concat dir "conf")))
                  (monk-file (concat dir "conf") :content "x\n" :classes '(mars)))
                :passes 1)))
      (should (eq 'skipped (monk-test-status run (concat dir "before"))))
      (should (eq 'skipped (monk-test-status run (concat dir "conf")))))))

(ert-deftest monk-test-prereq-dry-run ()
  "In a plan, a prereq resource is would-repair only when its target would change."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine)) (monk-log-level nil)
           (policy (lambda (_m)
                     (monk-directory (concat dir "before") :prereq (list (concat dir "conf")))
                     (monk-file (concat dir "conf") :content "x\n")))
           (plan (monk-plan m policy)))
      (should (eq 'would-repair (monk-test-status plan (concat dir "before"))))
      (should-not (file-directory-p (concat dir "before"))))))

(ert-deftest monk-test-listen-fires-once-after-convergence ()
  "Two watched files change across two passes; the listener acts once."
  (monk-test-with-tmpdir dir
    (let* ((count (concat dir "restarts"))
           (policy (lambda (_m)
                     (monk-file (concat dir "a.conf") :content "a\n")
                     ;; b depends on a so it repairs in the same pass but later;
                     ;; either way, the listener must fire exactly once.
                     (monk-file (concat dir "b.conf") :content "b\n" :require (list (concat dir "a.conf")))
                     (monk-command "svc" "sh"
                                   :args (list "-c" (format "echo r >> %s" count))
                                   :listen (list (concat dir "a.conf") (concat dir "b.conf")))))
           (run (monk-test-converge policy)))
      (should (eq 'repaired (monk-test-status run "svc")))
      (should (string-match-p "listen: .*a.conf.*b.conf" (monk-result-comment (monk-test-result run "svc"))))
      (should (= 1 (with-temp-buffer (insert-file-contents count) (count-lines 1 (point-max)))))
      ;; nothing changed: the listener stays kept
      (let ((again (monk-test-converge policy)))
        (should (eq 'kept (monk-test-status again "svc")))
        (should (= 1 (with-temp-buffer (insert-file-contents count) (count-lines 1 (point-max)))))))))

(ert-deftest monk-test-listen-gates-like-require ()
  "A listener whose ref failed is skipped, and does not fire."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x")
                  (monk-command "svc" "true" :listen (list (concat dir "missing#x")))))))
      (should (eq 'skipped (monk-test-status run "svc"))))))

(ert-deftest monk-test-retry-until-success-with-backoff ()
  "A resource that fails twice then succeeds is repaired on the third try."
  (monk-test-with-tmpdir dir
    (let* ((tries 0) (waits nil)
           (monk-log-level nil))
      (cl-letf (((symbol-function 'sit-for) (lambda (secs &rest _) (push secs waits) t)))
        (let ((run (monk-test-converge
                    (lambda (_m)
                      (monk-command "flaky" "sh"
                                    :args (list "-c" (format "test -e %s/ok" dir))
                                    :creates (concat dir "never")
                                    :retry '(:attempts 3 :interval 1 :backoff 2))))))
          (ignore tries)
          (should (eq 'failed (monk-test-status run "flaky")))
          (should (string-match-p "attempt 3 of 3" (monk-result-comment (monk-test-result run "flaky"))))
          (should (equal '(1.0 2.0) (reverse waits)))))
      (with-temp-file (concat dir "ok") (insert ""))
      (let ((run (monk-test-converge
                  (lambda (_m)
                    (monk-command "flaky" "sh"
                                  :args (list "-c" (format "test -e %s/ok" dir))
                                  :creates (concat dir "never")
                                  :retry '(:attempts 3 :interval 0))))))
        (should (eq 'repaired (monk-test-status run "flaky")))
        (should-not (string-match-p "attempt" (or (monk-result-comment (monk-test-result run "flaky")) "")))))))

(ert-deftest monk-test-retry-defaults-and-cap ()
  (let ((r (monk-command-resource :name "x" :program "true" :retry t)))
    (should (equal 2 (plist-get (monk--retry-plan r) :attempts)))
    (should (equal 30 (plist-get (monk--retry-plan r) :interval)))
    (should (eq 'success (plist-get (monk--retry-plan r) :until))))
  (let ((r (monk-command-resource :name "x" :program "true" :retry '(:attempts 5 :interval 1000))))
    (should (equal 5 (plist-get (monk--retry-plan r) :attempts)))
    (cl-letf (((symbol-function 'sit-for) (lambda (secs &rest _) secs)))
      (should (= monk-retry-max-interval (monk--retry-wait (monk--retry-plan r) 2)))))
  (should (null (monk--retry-plan (monk-command-resource :name "x" :program "true")))))

(ert-deftest monk-test-retry-dry-run-only-reports ()
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine)) (monk-log-level nil) (waits nil))
      (cl-letf (((symbol-function 'sit-for) (lambda (secs &rest _) (push secs waits) t)))
        (let ((plan (monk-plan m (lambda (_m) (monk-directory (concat dir "d") :retry '(:attempts 4))))))
          (should (eq 'would-repair (monk-test-status plan (concat dir "d"))))
          (should (string-match-p "retry up to 4" (monk-result-comment (monk-test-result plan (concat dir "d")))))
          (should (null waits)))))))

(ert-deftest monk-test-verify-fails-a-command-after-it-ran ()
  "A :verify that exits non-zero turns a successful command into a failure."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-command "bad" "true" :creates (concat dir "never")
                                :verify '("sh" "-c" "echo nope; exit 3"))
                  (monk-command "good" "true" :creates (concat dir "never")
                                :verify (lambda (_m) nil))
                  (monk-command "fn" "true" :creates (concat dir "never")
                                :verify (lambda (_m) "state is wrong")))
                :passes 1)))
      (should (eq 'failed (monk-test-status run "bad")))
      (should (string-match-p "verify failed: sh exited 3: nope" (monk-result-comment (monk-test-result run "bad"))))
      (should (eq 'repaired (monk-test-status run "good")))
      (should (eq 'failed (monk-test-status run "fn")))
      (should (string-match-p "state is wrong" (monk-result-comment (monk-test-result run "fn")))))))

(ert-deftest monk-test-order-first-and-last ()
  "`:order' breaks ties; requisites still win."
  (monk-test-with-tmpdir dir
    (let* ((run (monk-test-converge
                 (lambda (_m)
                   (monk-directory (concat dir "b"))
                   (monk-directory (concat dir "z") :order 'last)
                   (monk-directory (concat dir "a") :order 'first)
                   (monk-directory (concat dir "after-z") :require (list (concat dir "z"))))
                 :passes 1))
           (names (mapcar (lambda (r) (file-name-nondirectory (monk-result-name r)))
                          (monk-run-results run))))
      (should (equal '("a" "b" "z" "after-z") names))))
  (should-error (monk-test-converge (lambda (_m) (monk-directory "~/x" :order 'middle)))
                :type 'monk-policy-error))

(ert-deftest monk-test-available-p-skips-with-reason ()
  "A resource type that is not available here is skipped, not failed."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (run (cl-letf (((symbol-function 'monk-exec)
                         (lambda (_m program &rest args)
                           (if (and (equal program "sh") (seq-some (lambda (a) (string-match-p "command -v" a)) args))
                               (monk-exec-create :exit-code 1 :output "")
                             (monk-exec-create :exit-code 0 :output "")))))
                (monk-converge m (lambda (_m)
                                   (monk-brew "b" '("jq") :classes nil)
                                   (monk-apt "a" '("jq") :classes nil))
                               :passes 1))))
    (should (eq 'skipped (monk-test-status run "b")))
    (should (equal "brew is not on this machine" (monk-result-comment (monk-test-result run "b"))))
    (should (eq 'skipped (monk-test-status run "a")))
    (should (monk-run-ok-p run))))

(ert-deftest monk-test-mock-run-calls-nothing ()
  "A mock run orders the graph and evaluates requisites without touching the host."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine)) (monk-log-level nil)
           (checked nil)
           (run (cl-letf (((symbol-function 'monk-check)
                           (lambda (&rest _) (setq checked t) nil)))
                  (monk-orchestrate (list m)
                                    (lambda (_m)
                                      (monk-directory (concat dir "a"))
                                      (monk-directory (concat dir "b") :require (list (concat dir "a")))
                                      (monk-directory (concat dir "c") :onchanges (list (concat dir "a"))))
                                    :mock t))))
      (should-not checked)
      (should (monk-run-mock run))
      (should (= 1 (monk-run-passes run)))
      (should (equal (list monk-comment-mocked monk-comment-mocked "onchanges: nothing changed")
                     (mapcar #'monk-result-comment (monk-run-results run))))
      (should-not (file-directory-p (concat dir "a"))))))

(ert-deftest monk-test-fatal-stops-only-its-machine ()
  "A :fatal failure skips the rest of that machine; other machines finish."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b"))
           (monk-log-level nil)
           (run (monk-orchestrate
                 machines
                 (lambda (m)
                   (when (equal (oref m name) "a")
                     (monk-line-in-file (concat dir "missing") "x" :fatal t))
                   (monk-directory (concat dir (oref m name) "-after")))
                 :passes 1)))
      (should (eq monk-status-failed (monk-result-status (monk-run-result run (cons "a" (concat dir "missing#x"))))))
      (should (eq monk-status-skipped (monk-result-status (monk-run-result run (cons "a" (concat dir "a-after"))))))
      (should (equal monk-skip-fatal (monk-result-comment (monk-run-result run (cons "a" (concat dir "a-after"))))))
      (should (eq monk-status-repaired (monk-result-status (monk-run-result run (cons "b" (concat dir "b-after"))))))
      (should-not (file-directory-p (concat dir "a-after"))))))

(ert-deftest monk-test-handled-onfail-rescues ()
  "A successful :onfail handler with :handled t makes the run ok."
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing") "x")
                  (monk-directory (concat dir "rescue") :onfail (list (concat dir "missing#x")) :handled t))
                :passes 1)))
      (should (eq 'failed (monk-test-status run (concat dir "missing#x"))))
      (should (eq 'repaired (monk-test-status run (concat dir "rescue"))))
      (should (equal (list (cons "testbox" (concat dir "missing#x"))) (monk-run-rescued run)))
      (should (monk-run-ok-p run)))
    ;; without :handled the failure still counts
    (let ((run (monk-test-converge
                (lambda (_m)
                  (monk-line-in-file (concat dir "missing2") "x")
                  (monk-directory (concat dir "rescue2") :onfail (list (concat dir "missing2#x"))))
                :passes 1)))
      (should-not (monk-run-ok-p run)))))

(ert-deftest monk-test-run-context-cache ()
  "The run context caches per run and is gone after it."
  (should (null (monk-run-context-get 'x)))
  (let ((calls 0))
    (monk-test-with-tmpdir dir
      (monk-test-converge
       (lambda (_m)
         (monk-command "c1" "true" :creates (concat dir "n")
                       :verify (lambda (_m) (monk-with-run-cache '(k) (cl-incf calls) nil)))
         (monk-command "c2" "true" :creates (concat dir "n")
                       :verify (lambda (_m) (monk-with-run-cache '(k) (cl-incf calls) nil))))
       :passes 1))
    ;; nil results are not cached (the macro uses `or'), so both compute;
    ;; with a non-nil value the second call hits the cache
    (should (= 2 calls)))
  (let ((calls 0))
    (monk-test-with-tmpdir dir
      (monk-test-converge
       (lambda (_m)
         (monk-command "c1" "true" :creates (concat dir "n")
                       :verify (lambda (_m) (monk-with-run-cache '(k) (cl-incf calls) 'seen) nil))
         (monk-command "c2" "true" :creates (concat dir "n")
                       :verify (lambda (_m) (monk-with-run-cache '(k) (cl-incf calls) 'seen) nil)))
       :passes 1))
    (should (= 1 calls)))
  (should (null monk--current-run)))

(ert-deftest monk-test-class-init-runs-once-per-machine ()
  "apt-get update runs once per run per machine, before the first apt resource."
  (let* ((machines (monk-test-machines "a" "b"))
         (monk-log-level nil)
         (updates nil)
         (run (cl-letf (((symbol-function 'monk-exec)
                         (lambda (m program &rest args)
                           (cond
                            ((member "update" args)
                             (push (oref m name) updates)
                             (monk-exec-create :exit-code 0 :output ""))
                            ((equal program "dpkg-query")
                             (monk-exec-create :exit-code 0 :output "jq installed\ncurl installed\n"))
                            (t (monk-exec-create :exit-code 0 :output ""))))))
                (monk-orchestrate machines
                                  (lambda (_m)
                                    (monk-apt "one" '("jq") :classes nil)
                                    (monk-apt "two" '("curl") :classes nil))
                                  :passes 1))))
    (ignore run)
    (should (equal '("a" "b") (sort updates #'string<)))
    (should (= 2 (length updates)))))

(ert-deftest monk-test-aggregate-merges-independent-apt-resources ()
  "Two apt resources with no requisite between them install in one call."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (installs nil)
         (monk-apt-update nil)
         (run (cl-letf (((symbol-function 'monk-exec)
                         (lambda (_m program &rest args)
                           (cond
                            ((equal program "dpkg-query")
                             (monk-exec-create :exit-code 0 :output "a installed\n"))
                            ((member "install" args)
                             (push (seq-take-while #'stringp (cdr (member "-q" args))) installs)
                             (monk-exec-create :exit-code 0 :output ""))
                            (t (monk-exec-create :exit-code 0 :output ""))))))
                (monk-converge m (lambda (_m)
                                   (monk-apt "set1" '("a" "b") :classes nil)
                                   (monk-apt "set2" '("c") :classes nil)
                                   (monk-directory "/tmp" :require '("set1"))
                                   (monk-apt "set3" '("d") :classes nil :require '("/tmp")))
                               :passes 1))))
    ;; set1 and set2 merged; set3 depends transitively on set1 so it ran alone
    (should (= 2 (length installs)))
    (should (equal '("b" "c") (sort (copy-sequence (car (last installs))) #'string<)))
    (should (equal '("d") (car installs)))
    (should (eq 'repaired (monk-test-status run "set1")))
    (should (equal '(:install ("b") :degree 0.5) (monk-result-changes (monk-test-result run "set1"))))
    (should (eq 'repaired (monk-test-status run "set2")))
    (should (string-match-p "aggregated with 1 other" (monk-result-comment (monk-test-result run "set2"))))
    (should (eq 'repaired (monk-test-status run "set3")))))

;;;; History

(defmacro monk-test-with-history (&rest body)
  "Run BODY with a fresh history database in a temporary MONK_HOME."
  (declare (indent 0))
  `(monk-test-with-tmpdir home
     (require 'monk-history)
     (let ((old-home (getenv "MONK_HOME")))
       (unwind-protect
           (progn
             (setenv "MONK_HOME" home)
             (monk-history-close)
             (let ((monk-history-file nil))
               ,@body))
         (monk-history-close)
         (setenv "MONK_HOME" old-home)))))

(ert-deftest monk-test-history-location ()
  "MONK_HOME wins; root goes to /etc/monk; others to XDG."
  (let ((old (getenv "MONK_HOME")))
    (unwind-protect
        (progn
          (setenv "MONK_HOME" "/tmp/mh")
          (should (equal "/tmp/mh/" (monk-home-directory)))
          (setenv "MONK_HOME" nil)
          (cl-letf (((symbol-function 'user-uid) (lambda () 0)))
            (should (equal "/etc/monk/" (monk-home-directory))))
          (cl-letf (((symbol-function 'user-uid) (lambda () 1000)))
            (should (string-suffix-p "/monk/" (monk-home-directory)))
            (should (string-match-p "\\.local/share/monk/$\\|XDG" (monk-home-directory)))))
      (setenv "MONK_HOME" old))))

(ert-deftest monk-test-history-records-runs ()
  "Every finished run lands in the database with its results."
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (let* ((policy (lambda (_m)
                       (monk-directory (concat dir "d"))
                       (monk-line-in-file (concat dir "missing") "x")))
             (m (monk-test-machine))
             (monk-log-level nil)
             (plan (monk-plan m policy))
             (apply (monk-apply plan))
             (runs (monk-history-runs)))
        (should (= 2 (length runs)))
        ;; newest first
        (should (equal (monk-run-id apply) (alist-get 'id (car runs))))
        (should (equal (monk-run-id plan) (alist-get 'plan_id (car runs))))
        (should (eq 0 (alist-get 'dry_run (car runs))))
        (should (eq 1 (alist-get 'dry_run (cadr runs))))
        (should (eq 1 (alist-get 'repaired (car runs))))
        (should (eq 1 (alist-get 'failed (car runs))))
        (should (equal "testbox" (alist-get 'hosts (car runs))))
        (should (= 2 (monk-history-run-count)))
        (should (= 1 (monk-history-run-count :dry-run t)))
        (let ((results (monk-history-results :run (monk-run-id apply))))
          (should (= 2 (length results)))
          (should (member "repaired" (mapcar (lambda (r) (alist-get 'status r)) results)))
          (should (member "failed" (mapcar (lambda (r) (alist-get 'status r)) results))))
        ;; filters
        ;; the plan fails the same check, so two failed rows exist
        (should (= 2 (length (monk-history-results :status 'failed))))
        (should (= 1 (length (monk-history-results :status 'failed :dry-run nil))))
        (should (= 2 (length (monk-history-results :name (concat dir "d")))))
        (should (= 1 (length (monk-history-results :name (concat dir "d") :changed t :dry-run nil))))
        (should (= 1 (length (monk-history-repairs "testbox" (concat dir "d")))))
        (should (null (monk-history-runs :host "nobody")))
        (should (= 2 (length (monk-history-runs :host "testbox"))))
        (should (= 2 (length (monk-history-runs :since "1h"))))
        (should (null (monk-history-runs :until "1h")))
        (should (= 1 (length (monk-history-runs :limit 1))))
        (should-error (monk-history-runs :bogus 1) :type 'monk-error)
        ;; recording twice does not duplicate
        (monk-history-record apply)
        (should (= 2 (monk-history-run-count)))
        (should (= 2 (length (monk-history-results :run (monk-run-id apply)))))))))

(ert-deftest monk-test-history-run-roundtrip ()
  "A run read back from history has the same results and summary."
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (let* ((m (monk-test-machine))
             (monk-log-level nil)
             (run (monk-converge m (lambda (_m) (monk-file (concat dir "f") :content "hi\n"))))
             (back (monk-history-run (monk-run-id run))))
        (should back)
        (should (equal (monk-run-summary run) (monk-run-summary back)))
        (let ((r (car (monk-run-results back))))
          (should (eq monk-status-repaired (monk-result-status r)))
          (should (equal "file" (monk-result-type r)))
          ;; changes come back as a plist that the describer accepts
          (should (keywordp (car (monk-result-changes r))))
          (should (stringp (monk-describe-changes (monk-result-changes r))))
          (should (monk-result-diff r))
          (should (equal (monk-run-id run) (monk-run-id back))))
        (should (null (monk-history-run "nope")))))))

(ert-deftest monk-test-history-mode-off-records-nothing ()
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (monk-history-mode -1)
      (unwind-protect
          (progn
            (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))
            (should (= 0 (monk-history-run-count))))
        (monk-history-mode 1)))))

(ert-deftest monk-test-history-delete-before ()
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))
      (should (= 1 (monk-history-run-count)))
      (should (= 0 (monk-history-delete-before "1h")))
      (should (= 1 (monk-history-delete-before (current-time))))
      (should (= 0 (monk-history-run-count)))
      (should (null (monk-history-results))))))

(ert-deftest monk-test-history-agent-ops ()
  "history-runs and history-results answer over the agent handler with filters."
  (require 'monk-agent)
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (let ((monk-log-level nil))
        (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))
        (monk-plan (monk-test-machine) (lambda (_m) (monk-directory (concat dir "e")))))
      (let* ((out (with-output-to-string
                    (monk-agent-handle '((op . "history-runs") (dry_run . :false)))))
             (response (monk-event-parse (string-trim out))))
        (should (eq t (alist-get 'ok response)))
        (should (= 1 (alist-get 'count response)))
        (should (equal "testbox" (alist-get 'hosts (aref (alist-get 'rows response) 0)))))
      (let* ((out (with-output-to-string
                    (monk-agent-handle `((op . "history-results") (changed . t) (limit . 10)))))
             (response (monk-event-parse (string-trim out))))
        (should (= 2 (alist-get 'count response)))
        (should (member (alist-get 'status (aref (alist-get 'rows response) 0))
                        '("repaired" "would-repair"))))
      ;; filters plist from JSON keeps underscores mapped
      (should (equal '(:host "h" :dry-run nil :limit 5)
                     (monk-history-filters-from-json '((host . "h") (dry_run . :false) (limit . 5))))))))

(ert-deftest monk-test-ui-history-buffer ()
  "h lists runs; RET opens one; c lists its changes; r shows a timeline."
  (require 'monk-ui)
  (monk-test-with-history
    (monk-test-with-tmpdir dir
      (let* ((m (monk-test-machine))
             (monk-log-level nil)
             (run (monk-converge m (lambda (_m) (monk-directory (concat dir "d"))))))
        (save-window-excursion (monk-ui-history))
        (with-current-buffer monk-ui-history-buffer-name
          (should (derived-mode-p 'monk-ui-history-mode))
          (should (eq (key-binding "q") 'monk-ui-quit))
          (should (= 1 (length tabulated-list-entries)))
          (should (equal (monk-run-id run) (tabulated-list-get-id (point-min))))
          (goto-char (point-min))
          (save-window-excursion
            (monk-ui-history-open-run)
            (should (derived-mode-p 'monk-ui-mode))
            (should (equal (monk-run-summary run) (monk-run-summary monk-ui--run))))
          (save-window-excursion
            (monk-ui-history-changes)
            (goto-char (point-min))
            (should (search-forward "1 change" nil t))
            (should (search-forward (concat dir "d") nil t)))
          (save-window-excursion
            (monk-ui-history-resource "testbox" (concat dir "d"))
            (goto-char (point-min))
            (should (search-forward "repaired" nil t))))
        (dolist (b (buffer-list))
          (when (string-prefix-p "*monk" (buffer-name b)) (kill-buffer b)))))))

;;;; Origins and jump to source

(ert-deftest monk-test-resource-origin-from-file ()
  "A policy loaded from a file stamps each resource with that file."
  (monk-test-with-tmpdir dir
    (let ((policy-file (concat dir "policy.el")))
      (with-temp-file policy-file
        (insert ";; -*- lexical-binding: t; -*-\n"
                "(defun monk-test-origin-policy (_m)\n"
                "  (monk-directory \"~/from-file\"))\n"))
      (load policy-file nil t)
      (let* ((m (monk-test-machine))
             (resources (monk-compile m #'monk-test-origin-policy))
             (origin (oref (car resources) origin)))
        (should (equal (cdr origin) "~/from-file"))
        (should (equal (file-name-sans-extension (car origin))
                       (file-name-sans-extension policy-file)))
        (should (equal (monk-origin-file origin) policy-file))
        ;; the result carries it through a run
        (let* ((monk-log-level nil)
               (run (monk-plan m #'monk-test-origin-policy))
               (result (car (monk-run-results run))))
          (should (equal origin (monk-result-origin result)))
          (save-window-excursion
            (monk-result-find-definition result)
            (should (equal (buffer-file-name) policy-file))
            (should (looking-at ".*monk-directory \"~/from-file\"")))
          (kill-buffer (get-file-buffer policy-file))))
      (fmakunbound 'monk-test-origin-policy))))

(ert-deftest monk-test-lambda-policy-origin-is-visited-file ()
  "A lambda policy compiled while visiting a file is credited to that file."
  (monk-test-with-tmpdir dir
    (let ((file (concat dir "scratch.el")))
      (with-temp-file file (insert "(monk-directory \"~/x\")\n"))
      (with-current-buffer (find-file-noselect file)
        (let ((resources (monk-compile (monk-test-machine)
                                       (lambda (_m) (monk-directory "~/x")))))
          (should (equal (cons file "~/x") (oref (car resources) origin)))))
      (kill-buffer (get-file-buffer file)))))

(ert-deftest monk-test-origin-without-file ()
  "Without a file, the origin has no path and jumping is a user error."
  (let* ((load-file-name nil)
         (resources (with-temp-buffer
                      (monk-compile (monk-test-machine) (lambda (_m) (monk-directory "~/x"))))))
    (should (null (car (oref (car resources) origin))))
    (should-error (monk-find-origin (oref (car resources) origin)) :type 'user-error)))

(ert-deftest monk-test-inventory-origin ()
  "Machines remember the file that defined the inventory."
  (monk-test-with-tmpdir dir
    (let ((file (concat dir "inventory.el")))
      (with-temp-file file
        (insert ";; inventory\n"
                "(monk-inventory-define\n"
                " '(world :hosts ((alpha)\n"
                "                 (beta :address \"b\")\n"
                "                 (betamax))))\n"))
      (let ((monk-inventory nil))
        (load file nil t)
        (should (equal (cons file "alpha") (oref (monk-inventory-find 'alpha) origin)))
        (save-window-excursion
          (monk-machine-find-definition (monk-inventory-find 'beta))
          (should (equal (buffer-file-name) file))
          ;; on the line of (beta ...), not the first line and not betamax
          (should (= 4 (line-number-at-pos)))
          (should (looking-at " *(beta :address")))
        (kill-buffer (get-file-buffer file))))))

(ert-deftest monk-test-event-carries-source ()
  "The event record carries the policy file under native.source and restores it."
  (require 'monk-event)
  (let* ((result (monk-result-create :machine "m" :name "~/x" :type "directory"
                                     :status monk-status-kept
                                     :origin '("/tmp/policy" . "~/x")))
         (run (monk-run-create :machines nil :results (list result) :passes 1))
         (event (monk-event-result result run 1))
         (back (monk-event-to-result (monk-event-parse (monk-event-line event)))))
    (should (equal "/tmp/policy" (alist-get 'source (alist-get 'native event))))
    (should (equal '("/tmp/policy" . "~/x") (monk-result-origin back)))))

(ert-deftest monk-test-ui-find-definition-from-buffers ()
  "`.' in the detail, inspector, and query buffers finds the thing at point."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let ((policy-file (concat dir "policy.el"))
          (inventory-file (concat dir "inventory.el")))
      (with-temp-file policy-file
        (insert ";; -*- lexical-binding: t; -*-\n"
                "(defun monk-test-jump-policy (_m) (monk-directory \"~/jump\"))\n"))
      (with-temp-file inventory-file
        (insert "(monk-inventory-define '(world :hosts ((testbox))))\n"))
      (load policy-file nil t)
      (let ((monk-inventory nil) (monk-log-level nil))
        (load inventory-file nil t)
        (let* ((m (monk-inventory-find 'testbox))
               (run (progn (oset m facts monk-test-fake-facts)
                           (oset m classes (monk-hard-classes m))
                           (monk-plan m #'monk-test-jump-policy))))
          (with-temp-buffer
            (monk-ui-mode)
            (setq monk-ui--run run)
            (monk-ui-refresh)
            ;; machine row in *monk*
            (goto-char (point-min))
            (monk-ui-next-machine)
            (save-window-excursion
              (monk-ui-find-definition)
              (should (equal (buffer-file-name) inventory-file)))
            ;; result line in the detail buffer
            (monk-ui-detail-buffer "testbox")
            (with-current-buffer (format monk-ui-detail-buffer-format "testbox")
              (goto-char (point-min))
              (should (search-forward "~/jump" nil t))
              (save-window-excursion
                (monk-ui-find-definition)
                (should (equal (buffer-file-name) policy-file))
                (should (looking-at ".*~/jump")))
              ;; machine heading in the same buffer
              (goto-char (point-min))
              (should (search-forward "testbox: " nil t))
              (save-window-excursion
                (monk-ui-find-definition)
                (should (equal (buffer-file-name) inventory-file))))
            (kill-buffer (format monk-ui-detail-buffer-format "testbox")))
          ;; query buffer
          (save-window-excursion (monk-ui-query "'linux' in classes" (list m)))
          (with-current-buffer monk-ui-query-buffer-name
            (goto-char (point-min))
            (should (search-forward "✓ testbox" nil t))
            (save-window-excursion
              (monk-ui-find-definition)
              (should (equal (buffer-file-name) inventory-file))))
          (kill-buffer monk-ui-query-buffer-name)))
      (dolist (f (list policy-file inventory-file))
        (when (get-file-buffer f) (kill-buffer (get-file-buffer f))))
      (fmakunbound 'monk-test-jump-policy))))

;;;; UI: folds, inspector, query

(ert-deftest monk-test-ui-fold ()
  "A fold starts closed, TAB opens it, and the indicator follows."
  (require 'monk-ui)
  (with-temp-buffer
    (monk-ui-detail-mode)
    (let ((inhibit-read-only t) heading)
      (insert "top\n")
      (setq heading (monk-ui-insert-fold 'x "Section" (lambda () (insert "body line\n"))))
      (insert "after\n")
      (goto-char heading)
      (should (get-text-property (1+ (line-end-position)) 'invisible))
      (should (equal (get-text-property heading 'display) monk-ui-fold-indicator-closed))
      (goto-char heading)
      (monk-ui-toggle-fold)
      (should-not (get-text-property (1+ (line-end-position)) 'invisible))
      (should (equal (get-text-property heading 'display) monk-ui-fold-indicator-open))
      (monk-ui-toggle-fold)
      (should (get-text-property (1+ (line-end-position)) 'invisible))
      ;; the text after the fold stays visible either way
      (goto-char (point-max))
      (should-not (get-text-property (line-beginning-position 0) 'invisible)))))

(ert-deftest monk-test-ui-detail-has-machine-fold ()
  "The per-machine detail buffer carries a folded machine section."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d"))))))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run run)
        (save-window-excursion
          (monk-ui-show-machine "testbox"))
        (with-current-buffer (format monk-ui-detail-buffer-format "testbox")
          (should (derived-mode-p 'monk-ui-detail-mode))
          (should (eq (key-binding "q") 'monk-ui-quit))
          (goto-char (point-min))
          (should (search-forward "testbox: " nil t))
          (should (search-forward "\"Linux\"" nil t))
          (should (get-text-property (point) 'invisible)))
        (kill-buffer (format monk-ui-detail-buffer-format "testbox"))))))

(ert-deftest monk-test-ui-inspect-machine ()
  "The inspector shows the machine of the run, unfolded."
  (require 'monk-ui)
  (monk-test-with-tmpdir dir
    (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "d")))))
          (name (format monk-ui-detail-buffer-format "testbox facts")))
      (with-temp-buffer
        (monk-ui-mode)
        (setq monk-ui--run run)
        (save-window-excursion (monk-ui-inspect-machine "testbox"))
        (with-current-buffer name
          (goto-char (point-min))
          (should (search-forward "\"Linux\"" nil t))
          (should-not (get-text-property (point) 'invisible)))
        (kill-buffer name)))))

(ert-deftest monk-test-ui-query ()
  "The query buffer lists matches, explanations, and parse errors."
  (require 'monk-ui)
  (let ((machines (monk-test-cell-machines)))
    (save-window-excursion
      (monk-ui-query "'darwin' in classes && facts.arch == 'arm64'" machines))
    (with-current-buffer monk-ui-query-buffer-name
      (should (derived-mode-p 'monk-ui-query-mode))
      (should (eq (key-binding "q") 'monk-ui-quit))
      (goto-char (point-min))
      (should (search-forward "1 of 3 machines match" nil t))
      (should (search-forward "✓ foxy" nil t))
      (should (search-forward "– moi" nil t))
      (should (search-forward "facts.arch == 'arm64' is false" nil t))
      (should (search-forward "– kamen" nil t))
      (should (search-forward "class darwin is absent" nil t))
      ;; the CELL translation is shown
      (goto-char (point-min))
      (should (search-forward "(and (class darwin) (= (fact arch) \"arm64\"))" nil t)))
    (save-window-excursion (monk-ui-query "vars.disk > 100" machines))
    (with-current-buffer monk-ui-query-buffer-name
      (goto-char (point-min))
      (should (search-forward "1 of 3" nil t))
      (should (search-forward "? moi" nil t))
      (should (search-forward "cannot compare" nil t)))
    (save-window-excursion (monk-ui-query "frob == 1" machines))
    (with-current-buffer monk-ui-query-buffer-name
      (goto-char (point-min))
      (should (search-forward "Unknown name frob" nil t)))
    (kill-buffer monk-ui-query-buffer-name)))

;;;; Ansible C: tags, serial, max-fail-percentage, check-policy, agent, direct modules

(ert-deftest monk-test-tags-select ()
  "Ansible's evaluate_tags table."
  ;; no selection: everything runs
  (should (monk-tags-select-p nil nil nil))
  (should (monk-tags-select-p '(ssh) nil nil))
  ;; only
  (should (monk-tags-select-p '(ssh) '(ssh) nil))
  (should-not (monk-tags-select-p '(ssh) '(dns) nil))
  (should-not (monk-tags-select-p nil '(dns) nil))
  (should (monk-tags-select-p '(always) '(dns) nil))
  (should-not (monk-tags-select-p '(never) '(all) nil))
  (should (monk-tags-select-p '(never slow) '(slow) nil))
  (should (monk-tags-select-p '(ssh) '(all) nil))
  (should (monk-tags-select-p nil '(all) nil))
  (should (monk-tags-select-p '(ssh) '(tagged) nil))
  (should-not (monk-tags-select-p nil '(tagged) nil))
  (should (monk-tags-select-p nil '(untagged) nil))
  ;; skip
  (should-not (monk-tags-select-p '(ssh) nil '(ssh)))
  (should (monk-tags-select-p '(always) nil '(all)))
  (should-not (monk-tags-select-p '(always) nil '(all always)))
  (should-not (monk-tags-select-p '(ssh) nil '(tagged)))
  (should (monk-tags-select-p nil nil '(tagged)))
  ;; only then skip
  (should-not (monk-tags-select-p '(ssh slow) '(ssh) '(slow))))

(ert-deftest monk-test-tags-feed-only ()
  "Tag selection narrows the run; requisites still pull dependencies in."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (policy (lambda (_m)
                     (monk-directory (concat dir "base") :tags '(base))
                     (monk-file (concat dir "conf") :content "x" :require (list (concat dir "base")) :tags '(ssh))
                     (monk-directory (concat dir "other") :tags '(dns))
                     (monk-directory (concat dir "plain"))
                     (monk-directory (concat dir "must") :tags '(always))))
           (run (monk-orchestrate (list m) policy :tags '(ssh))))
      (should (eq 'repaired (monk-test-status run (concat dir "conf"))))
      (should (eq 'skipped (monk-test-status run (concat dir "base"))))
      (should (eq 'skipped (monk-test-status run (concat dir "other"))))
      (should (eq 'skipped (monk-test-status run (concat dir "plain"))))
      (should (eq 'repaired (monk-test-status run (concat dir "must"))))
      (should (equal '(ssh) (plist-get (monk-run-options run) :tags))))
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (run (monk-orchestrate (list m)
                                  (lambda (_m)
                                    (monk-directory (concat dir "s1") :tags '(slow))
                                    (monk-directory (concat dir "s2")))
                                  :skip-tags '(slow))))
      (should (eq 'skipped (monk-test-status run (concat dir "s1"))))
      (should (eq 'repaired (monk-test-status run (concat dir "s2")))))
    ;; a selection that matches nothing runs nothing, not everything
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (run (monk-orchestrate (list m)
                                  (lambda (_m) (monk-directory (concat dir "n1")))
                                  :tags '(nothing-has-this))))
      (should (eq 'skipped (monk-test-status run (concat dir "n1")))))))

(ert-deftest monk-test-serial-batches ()
  (should (= 1 (monk-serial-count "25%" 4)))
  (should (= 1 (monk-serial-count "10%" 4)))
  (should (= 3 (monk-serial-count 3 4)))
  (should-error (monk-serial-count "x" 4) :type 'monk-policy-error)
  (let ((ms '(a b c d e f g)))
    (should (equal '((a b c d e f g)) (monk-serial-batches ms nil)))
    (should (equal '((a b) (c d) (e f) (g)) (monk-serial-batches ms 2)))
    (should (equal '((a) (b c) (d e f g)) (monk-serial-batches ms '(1 2 0))))
    (should (equal '((a) (b c) (d e f g)) (monk-serial-batches ms '(1 "30%" "100%"))))
    (should (equal '((a) (b c) (d e) (f g)) (monk-serial-batches ms '(1 2))))))

(ert-deftest monk-test-serial-run-stops-over-threshold ()
  "Batches run in order; one that fails past the threshold stops the rest."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b" "c" "d"))
           (monk-log-level nil)
           (order nil)
           (policy (lambda (m)
                     (when (equal (oref m name) "a")
                       (monk-line-in-file (concat dir "missing") "x"))
                     (monk-command (concat "touch-" (oref m name)) "sh"
                                   :args (list "-c" (format "echo %s >> %s" (oref m name) (concat dir "order")))
                                   :changed-when (lambda (_e) t))
                     (monk-directory (concat dir (oref m name)))))
           (run (monk-orchestrate machines policy :serial 1 :max-fail-percentage 50 :passes 1)))
      (ignore order)
      (should (equal '(("a") ("b") ("c") ("d")) (monk-run-batches run)))
      (should (= 1 (monk-run-stopped-at-batch run)))
      (should-not (monk-run-ok-p run))
      (should (eq monk-status-failed (monk-result-status (monk-run-result run (cons "a" (concat dir "missing#x"))))))
      (should (file-directory-p (concat dir "a")))
      (should-not (file-directory-p (concat dir "b")))
      (should (equal monk-skip-batch (monk-result-comment (monk-run-result run (cons "b" (concat dir "b"))))))
      (should (equal monk-skip-batch (monk-result-comment (monk-run-result run (cons "d" (concat dir "d"))))))
      (should (equal "a\n" (with-temp-buffer (insert-file-contents (concat dir "order")) (buffer-string)))))
    ;; under the threshold every batch runs, and a plan never stops
    (let* ((machines (monk-test-machines "a" "b" "c" "d"))
           (monk-log-level nil)
           (policy (lambda (m)
                     (when (equal (oref m name) "a")
                       (monk-line-in-file (concat dir "missing") "x"))
                     (monk-directory (concat dir "2-" (oref m name)))))
           (run (monk-orchestrate machines policy :serial 2 :max-fail-percentage 50 :passes 1)))
      (should (equal '(("a" "b") ("c" "d")) (monk-run-batches run)))
      (should-not (monk-run-stopped-at-batch run))
      (should (file-directory-p (concat dir "2-d"))))
    (let* ((machines (monk-test-machines "a" "b"))
           (monk-log-level nil)
           (run (monk-orchestrate machines
                                  (lambda (m)
                                    (when (equal (oref m name) "a")
                                      (monk-line-in-file (concat dir "missing") "x"))
                                    (monk-directory (concat dir "3-" (oref m name))))
                                  :serial 1 :max-fail-percentage 0 :dry-run t)))
      (should-not (monk-run-stopped-at-batch run))
      (should (eq monk-status-would-repair (monk-result-status (monk-run-result run (cons "b" (concat dir "3-b")))))))))

(ert-deftest monk-test-serial-cross-batch-require ()
  "A resource in batch two can require one from batch one."
  (monk-test-with-tmpdir dir
    (let* ((machines (monk-test-machines "a" "b"))
           (monk-log-level nil)
           (run (monk-orchestrate machines
                                  (lambda (m)
                                    (if (equal (oref m name) "a")
                                        (monk-directory (concat dir "first"))
                                      (monk-directory (concat dir "second")
                                                      :require (list (cons 'a (concat dir "first"))))))
                                  :serial 1)))
      (should (eq monk-status-repaired (monk-result-status (monk-run-result run (cons "a" (concat dir "first"))))))
      (should (eq monk-status-repaired (monk-result-status (monk-run-result run (cons "b" (concat dir "second"))))))
      (should (monk-run-ok-p run)))))

(ert-deftest monk-test-check-policy ()
  "Compile only: lists resources and tags, reports errors and bad refs, no connection."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (gathers 0)
           (report (cl-letf (((symbol-function 'monk-gather-facts) (lambda (_m) (cl-incf gathers))))
                     (monk-check-policy m (lambda (_m)
                                            (monk-directory (concat dir "a") :tags '(base))
                                            (monk-file (concat dir "b") :content "x" :tags '(ssh base)))
                                        :tags '(ssh)))))
      (should (= 0 gathers))
      (should (monk-check-policy-ok-p report))
      (should (= 2 (length (plist-get report :nodes))))
      (should (equal '(base ssh) (plist-get report :tags)))
      (should (equal (list (cons "testbox" (concat dir "b"))) (plist-get report :selected)))
      (let ((text (monk-check-policy-string report)))
        (should (string-match-p "== testbox: 2 resource" text))
        (should (string-match-p "\\[ssh base\\]" text))
        (should (string-match-p "^tags: base ssh" text))))
    (let ((report (monk-check-policy (monk-test-machine)
                                     (lambda (_m) (monk-directory (concat dir "a") :require '("nope"))))))
      (should-not (monk-check-policy-ok-p report))
      (should (string-match-p "nope" (car (plist-get report :unresolved)))))
    (let ((report (monk-check-policy (monk-test-machine)
                                     (lambda (_m) (monk-directory (concat dir "a") :when '(frob))))))
      (should-not (monk-check-policy-ok-p report))
      (should (equal "testbox" (caar (plist-get report :errors))))
      (should (string-match-p "error: testbox:" (monk-check-policy-string report))))))

(ert-deftest monk-test-agent-splay-and-policy-repo ()
  (require 'monk-agent)
  (let ((monk-agent-splay 0))
    (should (= 0 (monk-agent-splay-seconds))))
  (let ((monk-agent-splay 5))
    (dotimes (_ 20) (should (<= 0 (monk-agent-splay-seconds) 5))))
  (should-not (let ((monk-agent-policy-repo nil)) (monk-agent-update-policy-repo)))
  (monk-test-with-tmpdir dir
    (let ((monk-log-level nil))
      ;; a directory that is not a repo: the update fails softly
      (let ((monk-agent-policy-repo dir))
        (should-not (monk-agent-update-policy-repo)))
      ;; a real repo with an upstream: same, then changed
      (let* ((upstream (concat dir "up/"))
             (clone (concat dir "clone")))
        (make-directory upstream)
        (monk--git-in upstream "init" "-q" "-b" "main")
        (monk--git-in upstream "-c" "user.email=t@example.invalid" "-c" "user.name=t"
                      "commit" "-q" "--allow-empty" "-m" "one")
        (monk--git-in dir "clone" "-q" upstream clone)
        (let ((monk-agent-policy-repo clone))
          (should (eq 'same (monk-agent-update-policy-repo)))
          (monk--git-in upstream "-c" "user.email=t@example.invalid" "-c" "user.name=t"
                        "commit" "-q" "--allow-empty" "-m" "two")
          (should (eq 'changed (monk-agent-update-policy-repo)))
          (should (equal (monk--git-in upstream "rev-parse" "HEAD") monk-agent--policy-revision)))))))

(ert-deftest monk-test-agent-only-if-changed-skips ()
  "A timed converge runs only after the policy repo moved when asked."
  (require 'monk-agent)
  (let ((converges 0)
        (monk-agent-only-if-changed t)
        (monk-agent-policy-repo "/nonexistent-repo")
        (monk-agent-splay 0)
        (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-converge) (lambda (&rest _) (cl-incf converges) (monk-run-create)))
              ((symbol-function 'monk-run-report) #'identity)
              ((symbol-function 'monk-agent-update-policy-repo) (lambda () 'same)))
      (monk-agent--full-converge #'ignore)      ; start: always
      (should (= 1 converges))
      (monk-agent--full-converge #'ignore t)    ; timed, unchanged: skipped
      (should (= 1 converges))
      (cl-letf (((symbol-function 'monk-agent-update-policy-repo) (lambda () 'changed))
                ((symbol-function 'monk-cli-print-line) #'ignore))
        (setq monk-agent--policy-revision "0123456789abcdef")
        (monk-agent--full-converge #'ignore t)
        (should (= 2 converges)))
      (let ((monk-agent-only-if-changed nil))
        (monk-agent--full-converge #'ignore t)
        (should (= 3 converges))))))

(ert-deftest monk-test-ansible-direct-normalize-and-diff ()
  (require 'monk-bridge)
  ;; rc != 0 without failed is failed; missing changed is false
  (let ((out (monk-ansible-normalize '((msg . "x")) 1)))
    (should (eq t (alist-get 'failed out)))
    (should (eq :false (alist-get 'changed out))))
  (should (eq :false (alist-get 'failed (monk-ansible-normalize '((changed . t)) 0))))
  ;; secrets are masked and internal keys dropped
  (let ((out (monk-ansible-normalize '((msg . "token is hunter2") (_ansible_new_secrets . ("hunter2"))
                                       (_ansible_no_log . t) (changed . :false))
                                     0)))
    (should (equal "token is ********" (alist-get 'msg out)))
    (should-not (assq '_ansible_new_secrets out))
    (should-not (assq '_ansible_no_log out)))
  ;; diff shapes
  (let ((content (monk-ansible-result-diff
                  '((diff (before . "a\n") (after . "b\n") (before_header . "/x (content)"))) "/x")))
    (should (= 1 (length content)))
    (should (eq 'content (monk-diff-entry-kind (car content))))
    (should (string-match-p "^-a$" (monk-diff-render content))))
  (let ((attrs (monk-ansible-result-diff
                '((diff (before (path . "/x") (mode . "0644")) (after (path . "/x") (mode . "0600")))) "/x")))
    (should (eq 'attributes (monk-diff-entry-kind (car attrs))))
    ;; the unchanged path is dropped from after so only mode renders
    (should (equal '((mode . "0600")) (monk-diff-entry-after (car attrs))))
    (should (string-match-p "mode: 0644 → 0600" (monk-diff-render attrs))))
  (should (= 2 (length (monk-ansible-result-diff
                        '((diff ((before . "a") (after . "b")) ((before . "c") (after . "d")))) "/x"))))
  (should (eq 'rendered (monk-diff-entry-kind (car (monk-ansible-result-diff '((diff . "--- x\n+++ y\n")) "/x")))))
  (should-not (monk-ansible-result-diff '((changed . t)) "/x"))
  ;; the module args JSON
  (should (equal "{\"ANSIBLE_MODULE_ARGS\":{\"path\":\"/x\",\"create\":true,\"_ansible_check_mode\":true,\"_ansible_diff\":true}}"
                 (monk-ansible-module-args '((path . "/x") (create . t)) t)))
  (should (equal "{\"ANSIBLE_MODULE_ARGS\":{\"path\":\"/x\"}}"
                 (monk-ansible-module-args '((path . "/x")) nil))))

(ert-deftest monk-test-ansible-direct-run ()
  "Direct module runs through a faked exec: check, diff, repair, failure, garbage."
  (require 'monk-bridge)
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (monk-ansible-library "/fake/lib")
         (monk-ansible--payload (cons "/fake/zip" "abc"))
         (sent nil) (env nil)
         (r (car (monk-compile m (lambda (_m)
                                   (monk-ansible "x" "lineinfile" '((path . "/x") (line . "b=2"))))))))
    (should (equal '((path . "/x") (line . "b=2")) (oref r args)))
    (cl-letf (((symbol-function 'monk-ansible-payload-on) (lambda (_m) "/home/u/.cache/monk/z.zip"))
              ((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (setq sent (plist-get (cdr (member "ansible.modules.lineinfile" args)) :input)
                       env (getenv "PYTHONPATH"))
                 (should (equal "python3" program))
                 (monk-exec-create
                  :exit-code 0
                  :output "\n{\"changed\": true, \"msg\": \"line added\", \"diff\": {\"before\": \"\", \"after\": \"b=2\\n\", \"before_header\": \"/x (content)\", \"after_header\": \"/x (content)\"}}\n"))))
      (let ((changes (monk-check r m)))
        (should (equal "line added" (plist-get changes :ansible)))
        (should (string-match-p "_ansible_check_mode\":true" sent))
        (should (equal "/home/u/.cache/monk/z.zip" env))
        (should (string-match-p "^\\+b=2$" (monk-diff-render (monk-diff r m changes)))))
      (should (equal "lineinfile line added" (monk-repair r m nil)))
      (should-not (string-match-p "_ansible_check_mode" sent)))
    (cl-letf (((symbol-function 'monk-ansible-payload-on) (lambda (_m) "/z.zip"))
              ((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 0 :output "\n{\"changed\": false}\n"))))
      (should-not (monk-check r m)))
    (cl-letf (((symbol-function 'monk-ansible-payload-on) (lambda (_m) "/z.zip"))
              ((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 0 :output "\n{\"changed\": false, \"skipped\": true, \"msg\": \"would run\"}\n"))))
      (should (eq 'no-check-mode (plist-get (monk-check r m) :ansible))))
    (cl-letf (((symbol-function 'monk-ansible-payload-on) (lambda (_m) "/z.zip"))
              ((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 1 :output "\n{\"failed\": true, \"msg\": \"missing required arguments: path\"}\n"))))
      (should-error (monk-check r m) :type 'monk-resource-error)
      (should-error (monk-repair r m nil) :type 'monk-resource-error))
    (cl-letf (((symbol-function 'monk-ansible-payload-on) (lambda (_m) "/z.zip"))
              ((symbol-function 'monk-exec)
               (lambda (&rest _) (monk-exec-create :exit-code 1 :output "Traceback: boom"))))
      (should-error (monk-check r m) :type 'monk-exec-error))))

(ert-deftest monk-test-ansible-payload-build ()
  "The payload zip is built from the library once, named by content."
  (require 'monk-bridge)
  (skip-unless (executable-find "zip"))
  (monk-test-with-tmpdir dir
    (let ((lib (concat dir "lib/")))
      (dolist (member monk-ansible-payload-members)
        (let ((full (concat lib member)))
          (if (string-suffix-p ".py" member)
              (progn (make-directory (file-name-directory full) t)
                     (with-temp-file full (insert "# stub\n")))
            (make-directory full t)
            (with-temp-file (concat full "/x.py") (insert "x = 1\n"))
            (make-directory (concat full "/__pycache__") t)
            (with-temp-file (concat full "/__pycache__/x.pyc") (insert "junk")))))
      (let ((monk-ansible-library lib)
            (monk-ansible--payload nil)
            (process-environment (cons (concat monk-home-environment-variable "=" dir "home") process-environment)))
        (pcase-let ((`(,path . ,sha) (monk-ansible-payload)))
          (should (file-exists-p path))
          (should (string-prefix-p (concat dir "home/ansible-modlib-") path))
          (should (= 64 (length sha)))
          (let ((mtime (file-attribute-modification-time (file-attributes path))))
            (should (equal (cons path sha) (monk-ansible-payload)))
            (should (equal mtime (file-attribute-modification-time (file-attributes path)))))
          (with-temp-buffer
            (call-process "unzip" nil t nil "-l" path)
            (should (search-backward "ansible/modules/x.py" nil t))
            (goto-char (point-min))
            (should-not (search-forward "__pycache__" nil t))))
        ;; a machine gets the zip under its cache once
        (let* ((m (monk-test-machine))
               (monk--current-run (monk-run-create)))
          (let ((monk-ansible-payload-cache (concat dir "target/.cache/monk")))
            (let ((remote (monk-ansible-payload-on m)))
              (should (file-exists-p remote))
              (should (string-prefix-p (concat dir "target/.cache/monk/ansible-modlib-") remote))
              (should (equal remote (monk-ansible-payload-on m))))))))
    (let ((monk-ansible-library nil))
      (should-error (monk-ansible-payload) :type 'monk-policy-error))))

;;;; Translate: import and export through a fake model

(require 'monk-translate)

(defun monk-test-fake-ask (answers)
  "Return an ask function that replies with ANSWERS in turn; records prompts in a list."
  (let ((queue (copy-sequence answers)) (log nil))
    (cons (lambda (prompt callback &rest keys)
            (push (list prompt (plist-get keys :system)) log)
            (let ((next (pop queue)))
              (if (eq next :fail)
                  (funcall callback nil "no model")
                (funcall callback next nil))))
          (lambda () (reverse log)))))

(ert-deftest monk-test-translate-catalog ()
  "The catalog comes from the specs: every declared constructor, its parameters, the common keys."
  (let ((text (monk-translate-catalog)))
    (should (string-match-p "^(monk-directory PATH &key \\.\\.\\.)" text))
    (should (string-match-p "^  :mode <mode>" text))
    (should (string-match-p "^(monk-line-in-file PATH LINE &key" text))
    (should (string-match-p ":state <symbol> default present one of present absent" text))
    (should (string-match-p "COMMON KEYS" text))
    (should (string-match-p ":when GUARD" text))
    (should (string-match-p "monk-ansible NAME MODULE ARGS-ALIST" text))))

(ert-deftest monk-test-translate-strip-and-read ()
  (should (equal "(defun x ())" (monk-translate-strip-fence "```elisp\n(defun x ())\n```")))
  (should (equal "(defun x ())" (monk-translate-strip-fence "(defun x ())")))
  (should (equal '(defun monk-translate--imported-policy (m) (monk-directory "~/a"))
                 (monk-translate-read-policy ";; -*- lexical-binding: t; -*-\n(defun monk-translate--imported-policy (m) (monk-directory \"~/a\"))\n")))
  (should-error (monk-translate-read-policy "(defun other (m))") :type 'monk-policy-error)
  (should-error (monk-translate-read-policy "(defun (") :type 'monk-policy-error))

(ert-deftest monk-test-translate-check-import ()
  (let ((m (monk-test-machine)) (monk-log-level nil))
    (pcase-let ((`(,report . ,errors)
                 (monk-translate-check-import
                  "(defun monk-translate--imported-policy (m) (monk-directory \"~/a\" :tags '(x)) (monk-file \"~/a/f\" :content \"hi\" :require '(\"~/a\")))" m)))
      (should-not errors)
      (should (= 2 (length (plist-get report :nodes))))
      (should (equal '(x) (plist-get report :tags))))
    (pcase-let ((`(,_ . ,errors)
                 (monk-translate-check-import
                  "(defun monk-translate--imported-policy (m) (monk-directory \"~/a\" :mdoe 1))" m)))
      (should (string-match-p "Unsupported parameter" errors)))
    (pcase-let ((`(,_ . ,errors)
                 (monk-translate-check-import
                  "(defun monk-translate--imported-policy (m) (monk-directory \"~/a\" :require '(\"nope\")))" m)))
      (should (string-match-p "nope" errors)))))

(ert-deftest monk-test-translate-import-repairs-then-stops ()
  "A bad answer goes back with the errors, up to the round limit; then the buffer says so."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (bad "(defun monk-translate--imported-policy (m) (monk-directory \"~/a\" :mdoe 1))")
         (good ";; -*- lexical-binding: t; -*-\n(defun monk-translate--imported-policy (m)\n  (monk-directory \"~/a\" :mode #o700))\n")
         (fake (monk-test-fake-ask (list bad bad good)))
         (monk-translate-ask-function (car fake))
         (monk-translate-repair-rounds 3))
    (cl-letf (((symbol-function 'monk-ui-check-policy) (lambda (&rest _) nil))
              ((symbol-function 'pop-to-buffer) #'set-buffer))
      (with-current-buffer (monk-translate-import 'ansible "- name: a\n  file: path=~/a state=directory mode=0700\n" m)
        (should (equal good (buffer-string)))
        (should (= 2 monk-translate--round))
        (let ((log (funcall (cdr fake))))
          (should (= 3 (length log)))
          ;; the first prompt carries the catalog and the source
          (should (string-match-p "MONK RESOURCE CATALOG" (car (nth 0 log))))
          (should (string-match-p "state=directory" (car (nth 0 log))))
          (should (string-match-p "imported-policy" (cadr (nth 0 log))))
          ;; repairs carry the error and the previous answer
          (should (string-match-p "Unsupported parameter" (car (nth 1 log))))
          (should (string-match-p ":mdoe 1" (car (nth 1 log))))))
      ;; out of rounds: the buffer keeps the last answer and a note
      (let* ((fake (monk-test-fake-ask (list bad bad bad bad bad)))
             (monk-translate-ask-function (car fake))
             (monk-translate-repair-rounds 2))
        (with-current-buffer (monk-translate-import 'salt "x:\n  file.directory\n" m)
          (should (= 2 monk-translate--round))
          (should (string-match-p "does not compile after 2 repair round" (buffer-string)))
          (should (= 3 (length (funcall (cdr fake)))))))
      ;; the model fails: the buffer says so, nothing signals
      (let* ((fake (monk-test-fake-ask (list :fail)))
             (monk-translate-ask-function (car fake)))
        (with-current-buffer (monk-translate-import 'salt "x:\n  file.directory\n" m)
          (should (string-match-p "did not answer: no model" (buffer-string)))))
      (let ((monk-translate-ask-function nil))
        (cl-letf (((symbol-function 'require) (lambda (f &rest _) (not (eq f 'efrit-ask)))))
          (should-error (monk-translate-import 'salt "x" m) :type 'user-error))))))

(ert-deftest monk-test-translate-resource-data ()
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (data (monk-translate-policy-data
                m (lambda (_m)
                    (monk-directory "~/a" :mode #o700 :tags '(base))
                    (monk-line-in-file "~/a/f" "x=1" :create t :require '("~/a")
                                       :when "facts.arch == 'x86_64'" :sudo t)
                    (monk-command "reload" "true" :watch '("~/a/f#x=1") :watch-action 'run)))))
    (should (= 3 (length data)))
    (should (equal "directory" (alist-get 'type (nth 0 data))))
    (should (equal "~/a" (alist-get 'name (nth 0 data))))
    (should (equal #o700 (alist-get 'mode (alist-get 'args (nth 0 data)))))
    (should (equal '(base) (alist-get 'tags (nth 0 data))))
    (should-not (assq 'when (nth 0 data)))
    (should (equal "facts.arch == 'x86_64'" (alist-get 'when (nth 1 data))))
    (should (equal '("~/a") (alist-get 'require (nth 1 data))))
    (should (eq t (alist-get 'sudo (nth 1 data))))
    (should (eq t (alist-get 'create (alist-get 'args (nth 1 data)))))
    (should (equal '("~/a/f#x=1") (alist-get 'watch (nth 2 data))))
    (should (eq 'run (alist-get 'watch-action (nth 2 data))))
    ;; the classes sugar shows up in the CEL too
    (let ((d (monk-translate-policy-data m (lambda (_m) (monk-directory "~/b" :classes '(linux))))))
      (should (equal "'linux' in classes" (alist-get 'when (car d)))))
    (should-error (monk-translate-policy-data m (lambda (_m) (monk-directory "~/a" :require '("nope"))))
                  :type 'monk-policy-error)))

(ert-deftest monk-test-translate-export-and-round-trip ()
  "Export sends data and lands YAML; a fake import of that YAML compiles to the same names."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (policy (lambda (_m)
                   (monk-directory "~/a" :mode #o700 :tags '(base))
                   (monk-file "~/a/f" :content "hi\n" :require '("~/a"))))
         (yaml "---\n- hosts: all\n  tasks:\n    - name: \"~/a\"\n      file: {path: \"~/a\", state: directory, mode: \"0700\"}\n      tags: [base]\n    - name: \"~/a/f\"\n      copy: {dest: \"~/a/f\", content: \"hi\\n\"}\n")
         (fake (monk-test-fake-ask (list (concat "```yaml\n" yaml "```"))))
         (monk-translate-ask-function (car fake))
         (checked nil))
    (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer)
              ((symbol-function 'monk-translate-check-export) (lambda (d f) (setq checked (list d (file-exists-p f))) 'unchecked)))
      (with-current-buffer (monk-translate-export 'ansible m policy)
        (should (equal yaml (buffer-string)))
        (should (equal '(ansible t) checked))
        (should (eq monk-translate--dialect 'ansible))
        (let ((prompt (car (car (funcall (cdr fake))))))
          (should (string-match-p "(type . \"directory\")" prompt))
          (should (string-match-p "(tags base)" prompt))
          (should (string-match-p "testbox" prompt)))))
    ;; round trip: the fake "model" hands back a policy with the same names
    (let* ((back ";; -*- lexical-binding: t; -*-\n(defun monk-translate--imported-policy (m)\n  (monk-directory \"~/a\" :mode #o700 :tags '(base))\n  (monk-file \"~/a/f\" :content \"hi\\n\" :require '(\"~/a\")))\n")
           (fake (monk-test-fake-ask (list back)))
           (monk-translate-ask-function (car fake))
           (before (monk-translate-policy-data m policy)))
      (cl-letf (((symbol-function 'monk-ui-check-policy) (lambda (&rest _) nil))
                ((symbol-function 'pop-to-buffer) #'set-buffer))
        (with-current-buffer (monk-translate-import 'ansible yaml m)
          (should (= 0 monk-translate--round))
          (should (equal before (monk-translate-policy-data m 'monk-translate--imported-policy))))))))

(ert-deftest monk-test-translate-check-export-tools ()
  "Without the tools the check is `unchecked'; with a fake tool, its verdict is read."
  (monk-test-with-tmpdir dir
    (let ((file (concat dir "x.yml")))
      (with-temp-file file (insert "- hosts: all\n"))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (should (eq 'unchecked (monk-translate-check-export 'ansible file)))
        (should (eq 'unchecked (monk-translate-check-export 'salt file))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) t))
                ((symbol-function 'call-process) (lambda (&rest _) (insert "ERROR! bad yaml") 4)))
        (should (equal "ERROR! bad yaml" (monk-translate-check-export 'ansible file))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) t))
                ((symbol-function 'call-process) (lambda (&rest _) 0)))
        (should-not (monk-translate-check-export 'salt file))))))

;;;; Version and stale libraries

(ert-deftest monk-test-version-and-stale ()
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" monk-version))
  (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\.[0-9]+\\'" monk-build))
  (should (string-match-p (format "Monk %s build %s from " monk-version monk-build) (monk-version-string)))
  (dolist (feature '(monk monk-file monk-ui monk-cell))
    (should (alist-get feature monk--load-times)))
  ;; nothing is stale right after loading
  (should-not (monk-stale-libraries))
  (should-not (string-match-p "STALE" (monk-version-string)))
  ;; a source newer than its load time is reported
  (let ((monk--load-times (copy-alist monk--load-times)))
    (setf (alist-get 'monk-file monk--load-times) (time-subtract (current-time) (* 400 86400)))
    (let ((stale (monk-stale-libraries)))
      (should (= 1 (length stale)))
      (should (eq 'monk-file (caar stale)))
      (should (> (cdar stale) 0)))
    (should (string-match-p "STALE: monk-file \\+[0-9]+s" (monk-version-string)))))

;;;; Resource index and Embark

(require 'monk-embark)

(ert-deftest monk-test-resource-index ()
  "Compiling fills the index; names resolve to origins, machines, and dependents."
  (monk-test-with-tmpdir dir
    (let* ((monk-resource-index (make-hash-table :test #'equal))
           (m (monk-test-machine))
           (monk-log-level nil)
           (policy-file (concat dir "p.el")))
      (with-temp-file policy-file
        (insert ";; -*- lexical-binding: t; -*-\n"
                "(defun monk-test--indexed-policy (_m)\n"
                "  (monk-directory \"~/ix\" :tags '(base))\n"
                "  (monk-file \"~/ix/f\" :content \"x\" :require '(\"~/ix\"))\n"
                "  (monk-command \"ix-reload\" \"true\" :watch '(\"~/ix/f\") :creates \"~/never\"))\n"))
      (load policy-file nil t)
      (unwind-protect
          (progn
            (should (= 0 (hash-table-count monk-resource-index)))
            (should-not (monk-resource-origin-by-name "~/ix"))
            ;; the batch policy fills it on demand
            (let ((monk-batch-policy 'monk-test--indexed-policy))
              (should (equal policy-file (monk-origin-file (monk-resource-origin-by-name "~/ix")))))
            (should (equal '("ix-reload" "~/ix" "~/ix/f") (monk-resource-names)))
            ;; the on-demand compile used the local host; index it for testbox too
            (should (= 1 (length (plist-get (gethash "~/ix" monk-resource-index) :machines))))
            (monk-check-policy m 'monk-test--indexed-policy)
            (should (member "testbox" (plist-get (gethash "~/ix" monk-resource-index) :machines)))
            (should (equal '(base) (plist-get (gethash "~/ix" monk-resource-index) :tags)))
            (should (equal '(("~/ix/f" . require)) (monk-resource-dependents-by-name "~/ix")))
            (should (equal '(("ix-reload" . watch)) (monk-resource-dependents-by-name "~/ix/f")))
            (should-not (monk-resource-dependents-by-name "ix-reload"))
            ;; a second machine adds to :machines, not a new entry
            (monk-check-policy (car (monk-test-machines "other")) 'monk-test--indexed-policy)
            (should (= 3 (length (plist-get (gethash "~/ix" monk-resource-index) :machines))))
            (should (member "other" (plist-get (gethash "~/ix" monk-resource-index) :machines))))
        (fmakunbound 'monk-test--indexed-policy)))))

(ert-deftest monk-test-embark-targets ()
  "The finders see a resource string in Lisp, a result line in a report, a machine row."
  (let ((monk-resource-index (make-hash-table :test #'equal))
        (monk-log-level nil))
    (puthash "ix-reload" (list :type "command") monk-resource-index)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(monk-file \"~/ix/f\" :require '(\"ix-reload\" \"plain-word\"))")
      ;; a path-like string
      (goto-char 15)
      (pcase (monk-embark-resource-target)
        (`(monk-resource ,name . (,start . ,end))
         (should (equal "~/ix/f" name))
         (should (equal "~/ix/f" (buffer-substring start end))))
        (other (ert-fail (format "no target on the path: %S" other))))
      ;; an indexed name
      (goto-char (point-min))
      (goto-char (- (search-forward "ix-reload") 3))
      (should (equal "ix-reload" (cadr (monk-embark-resource-target))))
      ;; an unknown plain string is not a resource
      (goto-char (point-min))
      (goto-char (- (search-forward "plain-word") 3))
      (should-not (monk-embark-resource-target))
      ;; outside any string: nothing
      (goto-char 2)
      (should-not (monk-embark-resource-target))
      ;; a path in a comment, and a line-in-file name with a # part
      (goto-char (point-max))
      (insert "\n;; see ~/.zshrc and ~/etc/motd#welcome here\n")
      (goto-char (- (search-backward "~/.zshrc") -3))
      (should (equal "~/.zshrc" (cadr (monk-embark-resource-target))))
      (goto-char (- (search-forward "#welcome") 2))
      (should (equal "~/etc/motd#welcome" (cadr (monk-embark-resource-target))))
      ;; a plain word in a comment is not one
      (goto-char (- (search-forward "here") 2))
      (should-not (monk-embark-resource-target))
      ;; the active region wins when it names a resource
      (goto-char (point-min))
      (search-forward "plain-word")
      (set-mark (match-beginning 0))
      (activate-mark)
      (let ((transient-mark-mode t))
        (should-not (monk-embark-resource-target))
        (puthash "plain-word" (list :type "x") monk-resource-index)
        (should (equal "plain-word" (cadr (monk-embark-resource-target)))))
      (deactivate-mark))
    ;; a result line in a report buffer
    (monk-test-with-tmpdir dir
      (let ((run (monk-test-converge (lambda (_m) (monk-directory (concat dir "t"))))))
        (with-current-buffer (monk-ui-detail-buffer* run "testbox")
          (goto-char (point-min))
          (search-forward (concat dir "t"))
          (pcase (monk-embark-resource-target)
            (`(monk-resource ,name . ,_)
             (should (equal (concat dir "t") name))
             (should (monk-result-p (get-text-property 0 'monk-result name))))
            (other (ert-fail (format "no result target: %S" other)))))
        (with-current-buffer (monk-ui-show run)
          (goto-char (point-min))
          (monk-ui-next-machine)
          (pcase (monk-embark-machine-target)
            (`(monk-machine ,name . ,_) (should (equal "testbox" name)))
            (other (ert-fail (format "no machine target: %S" other)))))))))

(defun monk-ui-detail-buffer* (run name)
  "Return the detail buffer of machine NAME for RUN, built from a report buffer."
  (with-current-buffer (monk-ui-show run)
    (monk-ui-detail-buffer name)))

(ert-deftest monk-test-embark-actions ()
  "Find, plan-only, dependents, and setup with a fake Embark."
  (monk-test-with-tmpdir dir
    (let* ((monk-resource-index (make-hash-table :test #'equal))
           (monk-log-level nil)
           (policy-file (concat dir "p.el"))
           (monk-batch-policy 'monk-test--embark-policy))
      (with-temp-file policy-file
        (insert ";; -*- lexical-binding: t; -*-\n"
                (format "(defun monk-test--embark-policy (_m)\n  (monk-directory %S)\n  (monk-directory %S :require '(%S))\n  (monk-directory %S))\n"
                        (concat dir "a") (concat dir "b") (concat dir "a") (concat dir "c"))))
      (load policy-file nil t)
      (unwind-protect
          (progn
            ;; find: opens the policy file at the literal
            (save-window-excursion
              (monk-embark-find-resource (concat dir "b"))
              (should (equal policy-file buffer-file-name))
              (should (looking-at-p (format ".*%S" (concat dir "b"))))
              (kill-buffer))
            ;; plan only b and its dependents: a is not selected (b requires a, not the reverse)
            (let ((reported nil) (tb (monk-test-machine)))
              (cl-letf (((symbol-function 'monk-run-report) (lambda (run &rest _) (setq reported run) run))
                        ((symbol-function 'monk-embark--machines-for) (lambda (_name) (list tb))))
                (monk-embark-plan-resource (concat dir "b"))
                (should (monk-run-dry-run reported))
                (should (eq monk-status-would-repair (monk-result-status (monk-run-result reported (cons "testbox" (concat dir "b"))))))
                (should (eq monk-status-skipped (monk-result-status (monk-run-result reported (cons "testbox" (concat dir "c"))))))
                (should-not (file-directory-p (concat dir "b")))
                (monk-embark-apply-resource (concat dir "b"))
                (should-not (monk-run-dry-run reported))
                (should (file-directory-p (concat dir "b")))
                (should-not (file-directory-p (concat dir "c")))))
            ;; dependents buffer
            (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
              (monk-embark-resource-dependents (concat dir "a"))
              (with-current-buffer "*monk-dependents*"
                (should (string-match-p (regexp-quote (concat dir "b")) (buffer-string)))
                (should (string-match-p "require" (buffer-string)))
                (kill-buffer)))
            ;; setup registers with Embark when it is there
            (progn
              (defvar embark-target-finders nil) (defvar embark-keymap-alist nil)
              (defvar embark-general-map (make-sparse-keymap))
              (setq embark-target-finders nil embark-keymap-alist nil)
              (provide 'embark)
              (unwind-protect
                  (progn
                    (setq embark-target-finders '(some-other-finder))
                    (should (monk-embark-setup))
                    (should (eq 'monk-embark-resource-target (car embark-target-finders)))
                    (should (equal '(monk-embark-resource-target monk-embark-machine-target some-other-finder)
                                   embark-target-finders))
                    (should (eq 'monk-embark-resource-map (alist-get 'monk-resource embark-keymap-alist)))
                    (should (eq #'monk-embark-find-resource (lookup-key monk-embark-resource-map (kbd "."))))
                    (monk-embark-setup)
                    (should (= 3 (length embark-target-finders))))
                (setq features (delq 'embark features)))))
        (fmakunbound 'monk-test--embark-policy)))))

(ert-deftest monk-test-format-duration ()
  "Fixed width, zero padded, so a redrawn line does not shift."
  (should (equal "00h 00m 00s" (monk-format-duration 0)))
  (should (equal "00h 00m 07s" (monk-format-duration 7.4)))
  (should (equal "00h 31m 12s" (monk-format-duration (+ (* 31 60) 12))))
  (should (equal "26h 00m 01s" (monk-format-duration (+ 86400 7200 1))))
  (should (equal "0d 00h 00m 00s" (monk-format-duration 0 t)))
  (should (equal "1d 02h 00m 01s" (monk-format-duration (+ 86400 7200 1) t)))
  (should (equal "00h 00m 00s" (monk-format-duration -5)))
  (should (= (length (monk-format-duration 59)) (length (monk-format-duration 3599)))))

;;;; brew versions, app-link, detach

(require 'monk-detach)

(defun monk-test-brew-table (table)
  "Return a `monk-exec' replacement answering brew from TABLE: ((ARGS-PREFIX . (CODE . OUTPUT)) ...)."
  (lambda (_m program &rest args)
    (let* ((args (seq-take-while (lambda (a) (not (keywordp a))) args))
           (hit (and (equal program "brew")
                     (seq-find (lambda (cell) (equal (seq-take args (length (car cell))) (car cell))) table))))
      (if hit
          (monk-exec-create :command (cons program args) :exit-code (cadr hit) :output (cddr hit))
        (monk-exec-create :command (cons program args) :exit-code 127 :output (format "fake: no %s %S" program args))))))

(ert-deftest monk-test-brew-versioned-formulae ()
  (let ((m (monk-test-machine)) (monk-log-level nil) (monk--current-run (monk-run-create)))
    (cl-letf (((symbol-function 'monk-exec)
               (monk-test-brew-table
                '((("search" "--formula" "d12frosted/emacs-plus/emacs-plus@")
                   . (0 . "==> Formulae\nd12frosted/emacs-plus/emacs-plus@29\nd12frosted/emacs-plus/emacs-plus@30\nd12frosted/emacs-plus/emacs-plus@33\nd12frosted/emacs-plus/emacs-plus@32\n"))
                  (("search" "--formula" "python@") . (0 . "python@3.12\npython@3.9\npython@3.13\n"))
                  (("search" "--formula" "nope@") . (1 . "Error: No formulae found."))
                  (("--prefix" "emacs-plus@33") . (0 . "/opt/homebrew/opt/emacs-plus@33\n"))
                  (("--prefix" "ghost") . (1 . "Error: No available formula"))
                  (("outdated" "--quiet" "emacs-plus@33") . (0 . "emacs-plus@33\n"))
                  (("outdated" "--quiet" "jq") . (0 . ""))))))
      (should (equal '(33 32 30 29) (monk-brew-versioned-formulae m "emacs-plus" "d12frosted/emacs-plus")))
      (should (equal "emacs-plus@33" (monk-brew-latest-formula m "emacs-plus" "d12frosted/emacs-plus")))
      (should (equal "emacs-plus@33" (monk-brew-latest-formula m "emacs-plus" "d12frosted/emacs-plus" 32)))
      (should-not (monk-brew-latest-formula m "emacs-plus" "d12frosted/emacs-plus" 34))
      (should (equal '("3.13" "3.12" "3.9") (monk-brew-versioned-formulae m "python")))
      (should-not (monk-brew-versioned-formulae m "nope"))
      (should (equal "/opt/homebrew/opt/emacs-plus@33/" (monk-brew-opt-prefix m "emacs-plus@33")))
      (should-not (monk-brew-opt-prefix m "ghost"))
      (should (monk-brew-outdated-p m "emacs-plus@33"))
      (should-not (monk-brew-outdated-p m "jq")))))

(ert-deftest monk-test-brew-failure-summary ()
  (should (equal "exit 1: Error: Fetching d12frosted/emacs-plus failed! | fatal: unable to access 'https://x/': Could not resolve host"
                 (monk--brew-failure-summary
                  (monk-exec-create :exit-code 1 :output "Updating Homebrew...\nError: Fetching d12frosted/emacs-plus failed!\nfatal: unable to access 'https://x/': Could not resolve host\n"))))
  (should (equal "exit 2: last line only" (monk--brew-failure-summary (monk-exec-create :exit-code 2 :output "noise\nlast line only\n")))))

(ert-deftest monk-test-app-link ()
  "The link follows brew's opt path; a copy in the way is replaced; a missing bundle errors."
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (monk-log-level nil)
           (opt (concat dir "opt/emacs-plus@32/"))
           (apps (concat dir "Applications/")))
      (make-directory (concat opt "Emacs.app/Contents") t)
      (make-directory apps t)
      (cl-letf (((symbol-function 'monk-exec)
                 (monk-test-brew-table `((("--prefix" "emacs-plus@32") . (0 . ,opt))
                                        (("--prefix" "ghost") . (1 . "Error: no formula"))))))
        (let ((r (car (monk-compile m (lambda (_m)
                                        (monk-app-link (concat apps "Emacs.app") "emacs-plus@32:Emacs.app" :classes nil))))))
          (should (equal (list :create (concat opt "Emacs.app")) (monk-check r m)))
          (should (string-match-p "Emacs.app -> " (monk-repair r m (monk-check r m))))
          (should (equal (concat opt "Emacs.app") (file-symlink-p (concat apps "Emacs.app"))))
          (should-not (monk-check r m))
          ;; a stale link to an old keg
          (delete-file (concat apps "Emacs.app"))
          (make-symbolic-link (concat dir "old/Emacs.app") (concat apps "Emacs.app"))
          (should (plist-get (monk-check r m) :retarget))
          (monk-repair r m (monk-check r m))
          (should-not (monk-check r m))
          ;; a real copy in the way is replaced
          (delete-file (concat apps "Emacs.app"))
          (make-directory (concat apps "Emacs.app/Contents") t)
          (should (equal '(directory) (seq-take (plist-get (monk-check r m) :replace) 1)))
          (monk-repair r m (monk-check r m))
          (should (file-symlink-p (concat apps "Emacs.app"))))
        ;; the bundle is not there: the check says so instead of linking into nothing
        (let ((r (car (monk-compile m (lambda (_m)
                                        (monk-app-link (concat apps "Nope.app") "emacs-plus@32:Nope.app" :classes nil))))))
          (should-error (monk-check r m) :type 'monk-resource-error))
        (let ((r (car (monk-compile m (lambda (_m)
                                        (monk-app-link (concat apps "G.app") "ghost:G.app" :classes nil))))))
          (should-error (monk-check r m) :type 'monk-resource-error))))))

(ert-deftest monk-test-detach-script ()
  "The script waits for the pid, runs the batch Emacs with the load path, logs, notifies."
  (monk-test-with-tmpdir dir
    (let ((process-environment (cons (concat monk-home-environment-variable "=" dir "home") process-environment))
          (monk-detach-emacs "/usr/bin/true")
          (monk-detach-wait-seconds 4))
      (let ((script (monk-detach-write "job" (concat dir "policy.el") 'my-fn 4242)))
        (should (file-executable-p script))
        (should (string-prefix-p (concat dir "home/") script))
        (let ((text (with-temp-buffer (insert-file-contents script) (buffer-string))))
          (should (string-match-p "WAIT_PID=4242" text))
          (should (string-match-p "kill -0" text))
          (should (string-match-p "/usr/bin/true --batch --no-init-file -L " text))
          (should (string-match-p "-f my-fn" text))
          (should (string-match-p (regexp-quote (concat "-l " (shell-quote-argument (concat dir "policy.el")))) text))
          (should (string-match-p "detach-job.log" text))
          (should (string-match-p "osascript\\|notify-send" text))))
      ;; the wait loop releases when the pid dies: a sleep as the "Emacs"
      (skip-unless (executable-find "sh"))
      (let* ((sleeper (start-process "sleeper" nil "sleep" "30"))
             (pid (process-id sleeper))
             (script (monk-detach-write "job2" (concat dir "policy.el") 'ignore pid))
             (t0 (float-time))
             (runner (start-process "runner" nil script)))
        (unwind-protect
            (progn
              (sleep-for 0.5)
              (should (process-live-p runner))
              (delete-process sleeper)
              (let ((deadline (+ (float-time) 8)))
                (while (and (process-live-p runner) (< (float-time) deadline))
                  (accept-process-output nil 0.1)))
              (should-not (process-live-p runner))
              (should (< (- (float-time) t0) 8))
              (let ((log (with-temp-buffer (insert-file-contents (monk-detach-log-path "job2")) (buffer-string))))
                (should (string-match-p "start (waiting for pid" log))
                (should (string-match-p "end rc=0" log))))
          (ignore-errors (delete-process sleeper))
          (ignore-errors (delete-process runner)))))))

(ert-deftest monk-test-detach-running-emacsen ()
  "Other Emacs processes are found by command; this one is not listed."
  (let ((me (emacs-pid)))
    (cl-letf (((symbol-function 'list-system-processes) (lambda () (list me 100 200 300)))
              ((symbol-function 'process-attributes)
               (lambda (pid)
                 (pcase pid
                   (100 '((comm . "Emacs") (args . "/Applications/Emacs.app/Contents/MacOS/Emacs --batch -f monk-agent-main")))
                   (200 '((comm . "bash") (args . "bash -c emacsclient -n foo")))
                   (300 '((comm . "emacs-31.1") (args . "emacs-31.1 -nw")))
                   (_ '((comm . "Emacs") (args . "me")))))))
      (let ((found (monk-detach-running-emacsen)))
        (should (equal '(100 300) (mapcar #'car found)))
        (let ((text (monk-detach-describe-emacsen found)))
          (should (string-match-p "pid 100 +Emacs --batch -f monk-agent-main" text))
          (should (string-match-p "pid 300 +emacs-31.1 -nw" text))
          (should-not (string-match-p "/Applications/" text)))))))

(ert-deftest monk-test-brew-rebuild ()
  "An installed formula with a rebuild reason is rebuilt; reinstall falls back to install --overwrite."
  (let* ((m (monk-test-machine))
         (monk-log-level nil)
         (monk--current-run (monk-run-create))
         (calls nil)
         (reinstall-ok t))
    (cl-letf (((symbol-function 'monk-exec)
               (lambda (_m program &rest args)
                 (let ((args (seq-take-while (lambda (a) (not (keywordp a))) args)))
                   (push (cons program args) calls)
                   (pcase (car args)
                     ("info" (monk-exec-create :exit-code 0 :output "{\"formulae\": [{\"name\": \"emacs-plus@32\", \"full_name\": \"d12frosted/emacs-plus/emacs-plus@32\", \"aliases\": [], \"oldnames\": []}], \"casks\": []}"))
                     ("reinstall" (monk-exec-create :exit-code (if reinstall-ok 0 1) :output (if reinstall-ok "ok" "Error: emacs-plus@32 not installed")))
                     (_ (monk-exec-create :exit-code 0 :output "ok")))))))
      ;; no rule: installed is kept
      (let ((r (monk-brew-resource :name "e" :packages '("emacs-plus@32"))))
        (should-not (monk-check r m)))
      ;; a rule that says nothing: kept
      (let ((r (monk-brew-resource :name "e" :packages '("emacs-plus@32") :rebuild (lambda (_m _n) nil))))
        (should-not (monk-check r m)))
      ;; a reason: :rebuild with the reason, degree 0, repair runs reinstall
      (let* ((r (monk-brew-resource :name "e" :packages '("emacs-plus@32")
                                    :rebuild (lambda (_m n) (format "%s moved" n))))
             (changes (monk-check r m)))
        (should (equal '(("emacs-plus@32" . "emacs-plus@32 moved")) (plist-get changes :rebuild)))
        (should-not (plist-get changes :install))
        (should (= 0.0 (plist-get changes :degree)))
        (should (string-match-p "rebuild emacs-plus@32: emacs-plus@32 moved" (monk-describe-changes changes)))
        (setq calls nil)
        (should (equal "rebuilt emacs-plus@32 (emacs-plus@32 moved)" (monk-repair r m changes)))
        (should (equal '(("brew" "reinstall" "emacs-plus@32")) (reverse calls)))
        ;; reinstall refuses: install --overwrite, as the old Makefile did
        (setq calls nil reinstall-ok nil)
        (monk-repair r m changes)
        (should (equal '(("brew" "reinstall" "emacs-plus@32") ("brew" "install" "--overwrite" "emacs-plus@32"))
                       (reverse calls)))
        (setq reinstall-ok t))
      ;; t means always
      (let ((r (monk-brew-resource :name "e" :packages '("emacs-plus@32") :rebuild t)))
        (should (equal "rebuild requested" (cdar (plist-get (monk-check r m) :rebuild)))))
      ;; missing and rebuild in one resource: install first, then rebuild the other
      (let* ((r (monk-brew-resource :name "two" :packages '("jq" "emacs-plus@32") :rebuild t))
             (changes (monk-check r m)))
        (should (equal '("jq") (plist-get changes :install)))
        (should (equal '("emacs-plus@32") (mapcar #'car (plist-get changes :rebuild))))
        (should (= 0.0 (plist-get changes :degree)))
        (setq calls nil)
        (should (string-match-p "installed jq; rebuilt emacs-plus@32" (monk-repair r m changes)))
        (should (equal '(("brew" "install" "jq") ("brew" "reinstall" "emacs-plus@32")) (reverse calls))))
      ;; a rebuild resource does not aggregate with plain ones
      (let ((plain (monk-brew-resource :name "a" :packages '("x")))
            (reb (monk-brew-resource :name "b" :packages '("y") :rebuild t)))
        (should-not (monk-resource-aggregate plain (list reb) m))
        (should-not (monk-resource-aggregate reb (list plain) m))
        (should (monk-resource-aggregate plain (list (monk-brew-resource :name "c" :packages '("z"))) m))))))

(ert-deftest monk-test-check-evidence-kept ()
  "A check's notes ride along with a kept result, into the history and events."
  (monk-test-with-tmpdir dir
    (let ((monk-log-level nil))
      (cl-letf (((symbol-function 'monk-rsync-parse-itemize) (lambda (_o) (cons nil 12)))
                ((symbol-function 'monk--rsync-run)
                 (lambda (_r _m _d) (monk-exec-create :command '("rsync" "--dry-run" "a" "b") :exit-code 0 :output "x\ny\n"))))
        (let* ((run (monk-test-converge (lambda (_m) (monk-rsync "s" (list (concat dir "a")) (concat dir "b/")))
                                        :passes 1))
               (r (monk-test-result run "s"))
               (evidence (plist-get (monk-result-changes r) :evidence)))
          (should (eq 'kept (monk-result-status r)))
          (should evidence)
          (should (= 0 (plist-get evidence :exit)))
          (should (= 12 (plist-get evidence :files)))
          (should (= 0 (plist-get evidence :itemized)))
          (should (= 2 (plist-get evidence :lines)))
          (should (string-match-p "--dry-run" (plist-get evidence :rsync)))
          ;; evidence alone is not a change
          (should-not (monk-result-changed-p r))
          (should (equal "" (monk-describe-changes (monk-result-changes r))))
          ;; and it reaches the event record
          (should (alist-get 'evidence (alist-get 'changes (monk-event-result r run)))))))))

(ert-deftest monk-test-agent-reload-drops-the-monk-reload-message ()
  "The agent prints its own reload line; `monk-reload's \"monk: ...\" message
must not reach stderr, where it lands behind the terminal's idle line.
The message is sent as (message \"%s\" summary), so the filter must look
at the formatted text, not the format string (2026-10-11)."
  (require 'monk-agent)
  (let ((seen nil)
        (monk-agent-policy-file nil)
        (monk-agent-source-watch nil)
        (monk-agent--server t)
        (monk-log-level nil))
    (cl-letf* ((real (symbol-function 'message))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format-message fmt args) seen))
                  nil))
               ((symbol-function 'monk-stale-libraries) (lambda () '((monk-sync . 3))))
               ((symbol-function 'monk-reload)
                (lambda (&rest _) (message "%s" "monk: 1.0 build x, reloaded 2 libraries") nil))
               ((symbol-function 'monk-agent--load-new-libraries) (lambda () nil)))
      (ignore real)
      (should (string-match-p "reloaded monk-sync" (monk-agent--reload-if-stale)))
      (should-not (seq-find (lambda (m) (string-prefix-p "monk: " m)) seen)))))

(ert-deftest monk-test-agent-reloads-stale-source-in-place ()
  "A timed converge reloads edited libraries and the policy file; nothing restarts."
  (require 'monk-agent)
  (monk-test-with-tmpdir dir
    (let* ((policy-file (concat dir "policy.el"))
           (monk-agent-policy-file policy-file)
           (monk-agent--policy-file-mtime nil)
           (reloaded nil) (loaded nil)
           (monk-log-level nil))
      (with-temp-file policy-file (insert ";; -*- lexical-binding: t; -*-\n(defun monk-test--agent-pol (_m) nil)\n"))
      (cl-letf (((symbol-function 'monk-stale-libraries) (lambda () '((monk-sync . 120))))
                ((symbol-function 'monk-reload) (lambda (&rest _) (setq reloaded t)))
                ((symbol-function 'load) (lambda (file &rest _) (push file loaded) t)))
        ;; first timed converge: library stale, policy never loaded by the agent -> both
        (let ((note (monk-agent--reload-if-stale)))
          (should reloaded)
          (should (equal (list policy-file) loaded))
          (should (string-match-p "reloaded monk-sync, reloaded policy policy.el" note)))
        ;; nothing changed since: no reload
        (setq reloaded nil loaded nil)
        (cl-letf (((symbol-function 'monk-stale-libraries) (lambda () nil)))
          (should-not (monk-agent--reload-if-stale))
          (should-not reloaded)
          (should-not loaded)
          ;; the policy file is edited: only it reloads
          (sleep-for 0.05)
          (set-file-times policy-file (time-add (current-time) 2))
          (let ((note (monk-agent--reload-if-stale)))
            (should (equal (list policy-file) loaded))
            (should-not reloaded)
            (should (equal "reloaded policy policy.el" note))))
        ;; a failing reload is a note, not an error, and the line keeps saying so
        (cl-letf (((symbol-function 'monk-stale-libraries) (lambda () '((monk-ui . 5))))
                  ((symbol-function 'monk-reload) (lambda (&rest _) (error "boom"))))
          (should (string-match-p "reload FAILED" (monk-agent--reload-if-stale)))
          (should (string-match-p "STALE CODE: monk-ui" (substring-no-properties (monk-agent--stale-note)))))
        ;; stale with no failed attempt is an edit pending, not a failure
        (setq monk-agent--reload-failed nil)
        (cl-letf (((symbol-function 'monk-stale-libraries) (lambda () '((monk . 3)))))
          (let ((note (substring-no-properties (monk-agent--stale-note))))
            (should (string-match-p "source edited: monk" note))
            (should-not (string-match-p "failed" note))))))))

(ert-deftest monk-test-libraries-load-alone-in-batch ()
  "Each library loads in a bare Emacs with only its own requires.
The agent found `parse-iso8601-time-string' undefined (2026-10-09):
the test Emacs had parse-time from somewhere else.  So each library
is loaded in a fresh subprocess and asked for one function."
  (skip-unless (executable-find (expand-file-name invocation-name invocation-directory)))
  (dolist (case '(("monk-history" . "(monk-history-load-facts (monk-local-machine :name \"x\"))")
                  ("monk-event" . "(monk-run-id-time \"r20261009T142254.000\")")
                  ("monk-agent" . "monk-agent-main")
                  ("monk-sync" . "monk-rsync")
                  ("monk-detach" . "monk-detach")
                  ("monk-embark" . "monk-embark-setup")
                  ("monk-translate" . "monk-translate-catalog")
                  ("monk-cli" . "monk-cli-enable")))
    (with-temp-buffer
      (let ((code (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" (file-name-directory (locate-library "monk"))
                                "-l" (car case)
                                "--eval" (format "(let ((monk-log-level nil) (process-environment (cons \"MONK_HOME=%s\" process-environment))) (condition-case e (progn %s (princ \"ok\")) (void-function (princ (format \"VOID %%S\" e))) (error (princ \"ok-other\"))))"
                                                 (make-temp-file "monk-alone" t)
                                                 (if (string-prefix-p "(" (cdr case)) (cdr case) (format "(fboundp '%s)" (cdr case)))))))
        (should (= 0 code))
        (should-not (string-match-p "VOID" (buffer-string)))))))

(ert-deftest monk-test-reload-keeps-agent-wiring ()
  "A reload keeps a batch caller's report function, its terminal progress, and the watch counters."
  (require 'monk-reload) (require 'monk-cli)
  (monk-test-with-watch dir
      (lambda (_m) (monk-file (concat dir "f") :content "y"))
    (let ((w monk-watch--current))
      (setf (monk-watch-runs w) 7 (monk-watch-events w) 11)
      (setf (monk-watch-started w) (time-subtract (current-time) 3600))
      (let ((monk-report-function #'ignore)
            (monk-log-level nil))
        (add-hook 'monk-progress-functions #'monk-cli--progress)
        (unwind-protect
            (progn
              (monk-reload)
              (should (eq monk-report-function #'ignore))
              (should (memq #'monk-cli--progress monk-progress-functions))
              (should (not (eq w monk-watch--current)))
              (should (= 7 (monk-watch-runs monk-watch--current)))
              (should (= 11 (monk-watch-events monk-watch--current)))
              (should (> (float-time (time-since (monk-watch-started monk-watch--current))) 3000)))
          (remove-hook 'monk-progress-functions #'monk-cli--progress))))))

(ert-deftest monk-test-ui-load-does-not-steal-report-function ()
  "Loading monk-ui again leaves a non-default report function alone."
  (let ((monk-report-function #'ignore))
    (load (locate-library "monk-ui") nil t)
    (should (eq monk-report-function #'ignore)))
  (let ((monk-report-function #'monk-run-report-text))
    (load (locate-library "monk-ui") nil t)
    (should (eq monk-report-function #'monk-ui-report))))

;;;; Scopes and owners

(ert-deftest monk-test-scopes-and-owner ()
  "Scope is derived per resource; owner is on the run; both reach events and history."
  (monk-test-with-tmpdir dir
    (let ((monk-log-level nil))
      (should (equal (user-login-name) (car (monk-owner))))
      (should (equal (user-login-name) (monk-owner-string (cons (user-login-name) nil))))
      (should (equal "root" (monk-owner-string '("root" . t))))
      (should (equal "root (tzz)" (monk-owner-string '("tzz" . t))))
      (let* ((run (monk-test-converge (lambda (_m)
                                        (monk-directory (concat dir "u"))
                                        (monk-command "s" "true" :creates (concat dir "u") :sudo t))
                                      :passes 1))
             (u (monk-test-result run (concat dir "u")))
             (s (monk-test-result run "s")))
        (skip-unless (not (monk-root-p)))
        (should (eq 'user (monk-result-scope u)))
        (should (eq 'sudo (monk-result-scope s)))
        (should (equal (user-login-name) (car (monk-run-owner run))))
        (let ((event (monk-event-result s run)))
          (should (equal "sudo" (alist-get 'scope event)))
          (should (equal (user-login-name) (alist-get 'owner event))))
        (should (equal (user-login-name) (alist-get 'owner (monk-event-run 'run-end run))))
        (monk-test-with-history
          (monk-history-record run)
          (let ((rows (monk-history-results :run (monk-run-id run) :scope 'sudo)))
            (should (= 1 (length rows)))
            (should (equal "s" (alist-get 'name (car rows)))))
          (should (= 1 (length (monk-history-runs :owner (user-login-name)))))
          (should-not (monk-history-runs :owner "nobody-else"))
          (should (eq 'sudo (monk-result-scope
                             (monk-history-result-to-result
                              (car (monk-history-results :run (monk-run-id run) :scope 'sudo)))))))))))

(ert-deftest monk-test-policy-scope-checks ()
  "A root policy refuses a user process; a user resource refuses a root process."
  (let ((monk-policy-scopes nil) (monk-log-level nil) (m (monk-test-machine)))
    (defun monk-test--root-policy (_m) (monk-directory "/tmp/x"))
    (defun monk-test--user-policy (_m) (monk-directory "~/x"))
    (unwind-protect
        (progn
          (should-error (monk-policy-scope 'monk-test--root-policy 'galactic) :type 'monk-policy-error)
          (monk-policy-scope 'monk-test--root-policy 'root)
          (should (eq 'root (monk-policy-scope 'monk-test--root-policy)))
          (should-not (monk-policy-scope 'monk-test--user-policy))
          (if (monk-root-p)
              (progn
                (should-error (monk-compile m 'monk-test--user-policy) :type 'monk-policy-error)
                (should (monk-compile m 'monk-test--root-policy)))
            (should-error (monk-compile m 'monk-test--root-policy) :type 'monk-policy-error)
            (should (monk-compile m 'monk-test--user-policy))
            ;; pretend to be root: a plain resource is refused, a sudo one and a root policy pass
            (cl-letf (((symbol-function 'monk-root-p) (lambda () t)))
              (should-error (monk-compile m 'monk-test--user-policy) :type 'monk-policy-error)
              (should (monk-compile m 'monk-test--root-policy))
              (should (monk-compile m (lambda (_m) (monk-directory "/etc/x" :sudo t))))
              (should (eq 'root (monk-resource-scope (car (monk-compile m 'monk-test--root-policy))))))))
      (fmakunbound 'monk-test--root-policy)
      (fmakunbound 'monk-test--user-policy))))

;;;; Agent socket and connect

(ert-deftest monk-test-agent-local-socket-and-connect ()
  "An agent serves its socket; a client subscribes, steers a plan, gets the events."
  (require 'monk-connect)
  (skip-unless (featurep 'make-network-process))
  (monk-test-with-tmpdir dir
    (let* ((home (concat dir "home"))
           (process-environment (cons (concat monk-home-environment-variable "=" home) process-environment))
           (monk-log-level nil)
           (monk-batch-policy (lambda (_m) (monk-directory (concat dir "agent-made"))))
           (monk-agent--server nil) (monk-agent--subscribers nil)
           (monk-connect--process nil) (monk-connect--status nil)
           (monk-connect--waiting nil) (monk-connect--run-events nil)
           (monk-connect--live-run nil) (monk-connect--live-results nil))
      (cl-letf (((symbol-function 'monk-agent-machine) (lambda () (monk-test-machine)))
                ((symbol-function 'monk-agent-record) #'ignore)
                ((symbol-function 'monk-agent--announce) #'ignore)
                ((symbol-function 'monk-agent--sudo-state) (lambda () "cached")))
        (unwind-protect
            (progn
              (should (monk-agent-serve-local))
              (should (file-exists-p (monk-agent-local-socket-path)))
              (should (= #o600 (file-modes (monk-agent-local-socket-path))))
              (let ((info (monk-agent-read-pid-file)))
                (should (= (emacs-pid) (alist-get 'pid info)))
                (should (equal (monk-agent-local-socket-path) (alist-get 'socket info))))
              (should (equal (monk-agent-local-socket-path) (monk-connect-socket)))
              ;; connect, within this process: the server and client share the loop
              (monk-connect)
              (let ((deadline (+ (float-time) 3)))
                (while (and (null monk-connect--status) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should monk-connect--status)
              (should (equal "cached" (alist-get 'sudo monk-connect--status)))
              (should (equal monk-build (alist-get 'build monk-connect--status)))
              (should (= 1 (length monk-agent--subscribers)))
              (should (monk-connect-active-p))
              (should (string-match-p "AGENT" (substring-no-properties (monk-connect-status-line))))
              ;; a local watch is refused while connected
              (should-error (monk-watch-start (monk-test-machine) #'ignore) :type 'user-error)
              ;; steer a plan; the reply is the run rebuilt from events
              (let ((got nil))
                (monk-connect-request "plan" nil (lambda (run) (setq got run)))
                (let ((deadline (+ (float-time) 5)))
                  (while (and (null got) (< (float-time) deadline))
                    (accept-process-output nil 0.05)))
                (should got)
                (should (monk-run-dry-run got))
                ;; one result per resource: no doubling through request + broadcast
                (should (= 1 (length (monk-run-results got))))
                (should (= 1 (seq-count (lambda (e) (equal (alist-get 'event e) "run-end")) monk-connect--run-events)))
                (should (eq 'would-repair (monk-result-status (car (monk-run-results got)))))
                (should (equal (concat dir "agent-made") (monk-result-name (car (monk-run-results got)))))
                (should-not (file-directory-p (concat dir "agent-made")))
                (should-not monk-connect--waiting))
              ;; a plan then a run: the second request is not blocked by the first
              (let ((got nil))
                (monk-connect-request "plan" nil (lambda (r) (push (cons 'plan r) got)))
                (let ((deadline (+ (float-time) 5)))
                  (while (and (< (length got) 1) (< (float-time) deadline)) (accept-process-output nil 0.05)))
                (should-not monk-connect--waiting)
                (monk-connect-request "run" nil (lambda (r) (push (cons 'run r) got)))
                (let ((deadline (+ (float-time) 5)))
                  (while (and (< (length got) 2) (< (float-time) deadline)) (accept-process-output nil 0.05)))
                (should (= 2 (length got)))
                (should (eq 'run (caar got)))
                (should-not (monk-run-dry-run (cdar got)))
                (should (file-directory-p (concat dir "agent-made")))
                (should-not monk-connect--waiting))
              ;; an op the agent rejects settles the wait and says so
              (let ((said nil))
                (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
                  (monk-connect-request "no-such-op" nil #'ignore)
                  (let ((deadline (+ (float-time) 3)))
                    (while (and monk-connect--waiting (< (float-time) deadline)) (accept-process-output nil 0.05))))
                (should-not monk-connect--waiting)
                (should (string-match-p "could not send no-such-op: unknown op" said)))
              ;; during a run the agent still answers read-only requests,
              ;; and queues a plan until the run ends
              (let* ((machine-at nil) (run-end-at nil) (plan-done nil)
                     (monk-batch-policy
                      (lambda (_m)
                        ;; :unless false -> the check itself sleeps, in plan and apply
                        (monk-command "slow" "true" :unless '("sh" "-c" "sleep 1.5; exit 1"))
                        (monk-directory (concat dir "agent-made-2")))))
                (monk-connect-request "run" nil (lambda (_r) (setq run-end-at (float-time))))
                ;; 0.4 s in: the run is inside its sleep
                (run-at-time 0.4 nil
                             (lambda ()
                               (monk-connect--send `((op . "machine") (client . "t")))
                               (monk-connect--send `((op . "plan") (client . "t")))))
                (cl-letf* ((real-handle (symbol-function 'monk-connect--handle))
                           ((symbol-function 'monk-connect--handle)
                            (lambda (event)
                              (when (equal (alist-get 'op event) "machine")
                                (setq machine-at (float-time)))
                              (when (and (equal (alist-get 'event event) "run-end")
                                         run-end-at (eq t (alist-get 'dry_run event)))
                                (setq plan-done t))
                              (funcall real-handle event))))
                  (let ((deadline (+ (float-time) 15)))
                    (while (and (not plan-done) (< (float-time) deadline))
                      (accept-process-output nil 0.05))))
                (should machine-at)
                (should run-end-at)
                ;; the facts answer came back before the run ended
                (should (< machine-at run-end-at))
                ;; and the queued plan ran after it
                (should plan-done)
                (should-not monk-connect--waiting))
              ;; a dropped wait frees the next request
              (setq monk-connect--waiting (cons "plan" #'ignore))
              (should-error (monk-connect-request "plan") :type 'user-error)
              (should (string-match-p "asked for a plan" (substring-no-properties (monk-connect--ui-badge))))
              ;; k in *monk* drops it
              (with-current-buffer (monk-ui--buffer)
                (should (eq (key-binding "k") 'monk-ui-drop-request))
                (monk-ui-drop-request))
              (should-not monk-connect--waiting)
              (should-not (string-match-p "asked for" (substring-no-properties (monk-connect--ui-badge))))
              ;; status op answers
              (let ((status nil))
                (monk-connect-status (lambda (s) (setq status s)))
                (should (equal (system-name) (alist-get 'host status))))
              ;; a run the agent starts on its own carries its origin and does
              ;; not bring *monk* up; one this session asked for does
              (let ((popped nil))
                (cl-letf (((symbol-function 'monk-ui-display-buffer)
                           (lambda (_b &optional unasked) (push (if unasked 'unasked 'asked) popped) nil))
                          ((symbol-function 'monk-ui--spinner-start) #'ignore))
                  (let ((noninteractive nil))
                    (monk-connect--run-start '((event . "run-start") (run . "r20261012T170000.001") (hosts . ["testbox"])
                                               (dry_run . t) (origin . "timed")))
                    (should (equal '(unasked) popped))
                    (setq monk-connect--asked t)
                    (monk-connect--run-start '((event . "run-start") (run . "r20261012T170000.002") (hosts . ["testbox"])
                                               (dry_run . t) (origin . "timed")))
                    (should (equal '(asked unasked) popped))
                    (should-not monk-connect--asked)
                    ;; the agent names this session in the origin: asked too
                    (monk-connect--run-start `((event . "run-start") (run . "r20261012T170000.003") (hosts . ["testbox"])
                                               (dry_run . t)
                                               (origin . ,(format "plan for %s" (monk-connect--client-name)))))
                    (should (equal '(asked asked unasked) popped))))
                (setq monk-connect--live-run nil monk-connect--run-events nil))
              ;; the subscribe reply carried the agent's machine: i needs no request
              (should monk-connect--machine)
              (let ((m monk-connect--machine))
                (should (equal "testbox" (oref m name)))
                (should (equal (alist-get 'os monk-test-fake-facts) (alist-get 'os (oref m facts))))
                (should (memq 'linux (oref m classes)))
                ;; the machine op still answers, for scripts
                (let ((got nil))
                  (monk-connect-request "machine" nil (lambda (r) (setq got r)))
                  (let ((deadline (+ (float-time) 3)))
                    (while (and (null got) (< (float-time) deadline)) (accept-process-output nil 0.05)))
                  (should (equal "testbox" (alist-get 'name got))))
                ;; a fact gather on the agent pushes the new machine; a
                ;; live-gathered one has a symbol source and a time, both
                ;; must serialize (they did not, 2026-10-12)
                (let ((before monk-connect--machine))
                  (setq monk-agent--machine (monk-test-machine))
                  (oset monk-agent--machine facts (cons '(pushed . "yes") monk-test-fake-facts))
                  (oset monk-agent--machine facts-source 'live)
                  (oset monk-agent--machine gathered (current-time))
                  (run-hook-with-args 'monk-facts-ready-functions monk-agent--machine)
                  (let ((deadline (+ (float-time) 3)))
                    (while (and (eq before monk-connect--machine) (< (float-time) deadline))
                      (accept-process-output nil 0.05)))
                  (should-not (eq before monk-connect--machine))
                  (should (equal "yes" (alist-get 'pushed (oref monk-connect--machine facts))))
                  (should (eq 'live (oref monk-connect--machine facts-source)))
                  (should (oref monk-connect--machine gathered)))
                ;; i while connected shows the pushed copy at once, no wait
                (save-window-excursion
                  (with-current-buffer (monk-ui-show (monk-run-create :machines (list m) :dry-run t :passes 1))
                    (goto-char (point-min))
                    (monk-ui-next-machine)
                    (monk-ui-inspect-machine)))
                (should-not monk-connect--waiting)
                (should (get-buffer "*monk: testbox facts*"))
                (with-current-buffer "*monk: testbox facts*"
                  (should (string-match-p "linux" (buffer-string)))
                  (should (string-match-p "pushed" (buffer-string))))
                (kill-buffer "*monk: testbox facts*"))
              ;; a second monk-connect while connected shows the window, no error
              (let ((proc monk-connect--process))
                (cl-letf (((symbol-function 'monk-connect--show-last-run) #'ignore))
                  (should (eq proc (monk-connect))))
                (should (eq proc monk-connect--process)))
              ;; the header names the host and the agent's version and build
              (should (string-match-p (concat "AGENT " (regexp-quote (system-name)) " "
                                              (regexp-quote monk-version) " "
                                              (regexp-quote (monk-connect--short-build monk-build)))
                                      (substring-no-properties (monk-connect--ui-badge))))
              (should (equal ".9" (monk-connect--short-build (concat (format-time-string "%F") ".9"))))
              (should (equal "2020-01-01.3" (monk-connect--short-build "2020-01-01.3")))
              ;; the agent goes away: the session is LOST, not silently local
              (monk-agent-stop-local)
              (let ((deadline (+ (float-time) 3)))
                (while (and (monk-connect-active-p) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (accept-process-output nil 0.1)
              (should-not (monk-connect-active-p))
              (should (monk-connect-lost-p))
              (should (string-match-p "AGENT LOST" (substring-no-properties (monk-connect--ui-badge))))
              ;; g in *monk* asks to reconnect; a "no" is a user-error, no local run
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (should-error (monk-ui--agent-key-p "plan") :type 'user-error))
              ;; a "yes" reconnects when the agent is back
              (should (monk-agent-serve-local))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'monk-connect--show-last-run) #'ignore))
                (should (monk-ui--agent-key-p "plan")))
              (should (monk-connect-active-p))
              (should-not monk-connect--lost)
              ;; disconnect drops the subscriber on the agent side
              (monk-disconnect)
              (accept-process-output nil 0.2)
              (should-not (monk-connect-active-p))
              (should-not (monk-connect-lost-p))
              (should-not monk-agent--subscribers))
          (ignore-errors (monk-disconnect))
          (monk-agent-stop-local)
          (should-not (file-exists-p (monk-agent-local-socket-path)))
          (should-not (file-exists-p (monk-agent-pid-file))))))))

(ert-deftest monk-test-ui-apply-with-nothing-to-change-asks-nothing ()
  "A plan with 0 changes and 0 failures applies without the yes-or-no question.
Asked \"Apply 0 changes?\" (2026-10-11): the question has one answer."
  (monk-test-with-tmpdir dir
    (let* ((policy (lambda (_m) (monk-directory dir)))
           (m (monk-test-machine))
           (monk-log-level nil)
           (plan (monk-plan m policy))
           (asked nil))
      (should (= 0 (monk-run-count plan monk-status-would-repair)))
      (with-current-buffer (monk-ui--buffer)
        (monk-ui-mode)
        (setq monk-ui--run plan)
        (monk-ui-refresh)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq asked t) t)))
          (save-window-excursion (monk-ui-apply)))
        (should-not asked)
        (should-not (monk-run-dry-run monk-ui--run))
        (should (eq plan (monk-run-plan monk-ui--run)))))))

(ert-deftest monk-test-connect-refuses-other-build-and-reconnects ()
  "A session connects only to an agent on its own build; monk-reconnect
pokes the agent and connects once the builds agree; reconnect-all walks
every agent this session used."
  (require 'monk-connect)
  (skip-unless (featurep 'make-network-process))
  (monk-test-with-tmpdir dir
    (let* ((home (concat dir "home"))
           (process-environment (cons (concat monk-home-environment-variable "=" home) process-environment))
           (monk-log-level nil)
           (monk-agent--server nil) (monk-agent--subscribers nil)
           (monk-connect--process nil) (monk-connect--status nil) (monk-connect--prefix nil)
           (monk-connect--known-prefixes nil) (monk-connect--waiting nil)
           (monk-connect--run-events nil) (monk-connect--live-run nil) (monk-connect--lost nil)
           (agent-build "0.0.0-old")
           (poked 0))
      (cl-letf (((symbol-function 'monk-agent-machine) (lambda () (monk-test-machine)))
                ((symbol-function 'monk-agent--sudo-state) (lambda () "cached"))
                ((symbol-function 'monk-connect--show-last-run) #'ignore)
                ;; the agent reports its own build; the poke "reloads" it
                ((symbol-function 'monk-agent-status-alist)
                 (lambda () `((host . "h") (owner . "o") (version . "v") (build . ,agent-build) (sudo . "cached"))))
                ((symbol-function 'signal-process)
                 (lambda (_pid _sig) (cl-incf poked) (setq agent-build monk-build) 0)))
        (unwind-protect
            (progn
              (should (monk-agent-serve-local))
              ;; mismatch: refused, nothing left open
              (should-error (monk-connect) :type 'user-error)
              (should-not (monk-connect-active-p))
              (should-not monk-connect--process)
              ;; the agent is still serving and has no stale subscriber
              (accept-process-output nil 0.2)
              (should-not monk-agent--subscribers)
              ;; reconnect: poke, probe with status (no subscribe), connect once
              (let ((monk-connect-reload-wait 5) (joins 0))
                (cl-letf* ((real-handle (symbol-function 'monk-agent-handle))
                           ((symbol-function 'monk-agent-handle)
                            (lambda (request)
                              (when (equal (alist-get 'op request) "subscribe") (cl-incf joins))
                              (funcall real-handle request))))
                  (monk-reconnect))
                (should (= 1 joins)))
              (should (= 1 poked))
              (should (monk-connect-active-p))
              (should (equal monk-build (alist-get 'build monk-connect--status)))
              (should (equal '(nil) monk-connect--known-prefixes))
              ;; the agent falls behind again; reconnect-all brings it back
              (setq agent-build "0.0.0-older")
              (let ((monk-connect-reload-wait 5) (said nil))
                (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
                  (monk-reconnect-all))
                (should (string-match-p "Reconnected: local" said)))
              (should (= 2 poked))
              (should (monk-connect-active-p))
              ;; with the check off, a mismatched agent connects and the badge says so
              (setq agent-build "0.0.0-off")
              (monk-disconnect)
              (let ((monk-connect-require-same-build nil))
                (monk-connect)
                (should (monk-connect-active-p))
                (should (string-match-p "0.0.0-off ≠" (substring-no-properties (monk-connect--ui-badge))))))
          (ignore-errors (monk-disconnect))
          (monk-agent-stop-local))))))

(ert-deftest monk-test-watch-names-noisy-directories ()
  "Unmanaged events are counted per directory; the stop line and status name the busiest."
  (require 'monk-watch)
  (monk-test-with-tmpdir dir
    (let* ((m (monk-test-machine))
           (noisy (concat dir "noisy/"))
           (quiet (concat dir "quiet/"))
           (monk-log-level 'info)
           (monk-watch--unmanaged-seen nil) (monk-watch--unmanaged-count 0)
           (monk-watch--unmanaged-by-directory nil)
           (w (monk-watch-create :machines (list m) :policy #'ignore :path-index nil)))
      (make-directory noisy) (make-directory quiet)
      (let ((monk-watch--current w))
        (dotimes (i 5) (monk-watch--on-event (list nil 'changed (format "%sf%d" noisy i))))
        (monk-watch--on-event (list nil 'changed (concat quiet "g")))
        (should (= 6 monk-watch--unmanaged-count))
        (should (equal (list (cons noisy 5) (cons quiet 1)) (monk-watch-noisy-directories)))
        (should (string-match-p "unmanaged mostly from .*noisy/ (5), .*quiet/ (1)" (monk-watch-status)))
        ;; the counts survive a carry-over (a reload)
        (let ((w2 (monk-watch-create :machines (list m) :policy #'ignore :path-index nil)))
          (setq monk-watch--unmanaged-count 0 monk-watch--unmanaged-by-directory nil)
          (monk-watch--carry-over w2 6 0 (current-time) 6 (list (cons noisy 5) (cons quiet 1)))
          (should (= 6 monk-watch--unmanaged-count))
          (should (equal (cons noisy 5) (car (monk-watch-noisy-directories)))))))))

(ert-deftest monk-test-rsync-arguments-carry-timeouts ()
  "rsync gets --timeout always and an ssh with connect and keepalive limits when a side is remote."
  (require 'monk-sync)
  (let* ((m (monk-test-machine))
         (local (car (monk-compile m (lambda (_m) (monk-rsync "l" '("/a/") "/b/")))))
         (remote (car (monk-compile m (lambda (_m) (monk-rsync "r" '("host:src/") "/b/")))))
         (monk-rsync-default-transport '("ssh" "-o" "ConnectTimeout=15")))
    (let ((args (monk--rsync-arguments local m)))
      (should (member "--timeout=60" args))
      (should-not (member "-e" args)))
    (let ((args (monk--rsync-arguments remote m)))
      (should (member "--timeout=60" args))
      (should (equal "ssh -o ConnectTimeout=15" (cadr (member "-e" args)))))
    ;; per resource: another value, or none
    (let ((slow (car (monk-compile m (lambda (_m) (monk-rsync "s" '("host:src/") "/b/" :timeout 300))))))
      (should (member "--timeout=300" (monk--rsync-arguments slow m))))
    (let ((none (car (monk-compile m (lambda (_m) (monk-rsync "n" '("host:src/") "/b/" :timeout 0 :transport 'none))))))
      (should-not (seq-some (lambda (a) (string-prefix-p "--timeout" a)) (monk--rsync-arguments none m)))
      (should-not (member "-e" (monk--rsync-arguments none m))))
    ;; the transport per resource: a string, or a list of words
    (let ((str (car (monk-compile m (lambda (_m) (monk-rsync "t" '("host:src/") "/b/" :transport "ssh -p 2222"))))))
      (should (equal "ssh -p 2222" (cadr (member "-e" (monk--rsync-arguments str m))))))
    (let ((lst (car (monk-compile m (lambda (_m) (monk-rsync "u" '("host:src/") "/b/" :transport '("ssh" "-J" "jump")))))))
      (should (equal "ssh -J jump" (cadr (member "-e" (monk--rsync-arguments lst m))))))
    ;; a word with a space cannot be passed through -e
    (should-error (monk-rsync-transport-string '("ssh" "-o" "ProxyCommand=nc x")) :type 'monk-resource-error)
    (should (= 60 monk-rsync-default-timeout))))

(ert-deftest monk-test-object-rebuild-after-class-grew ()
  "An object built before a class gained a slot is rebuilt by slot name.
Reproduces 2026-10-11: `monk-machine' gained `method' in a reload and
the agent's machine read `started' time as its `groups'."
  (eval '(defclass monk-test-grow () ((a :initarg :a :initform nil) (c :initarg :c :initform nil))) t)
  (monk-object-note-layout 'monk-test-grow)
  (let ((old (monk-test-grow :a 1 :c 3)))
    ;; the class grows in the middle, as monk-machine did
    (eval '(defclass monk-test-grow () ((a :initarg :a :initform nil)
                                        (b :initarg :b :initform 'default-b)
                                        (c :initarg :c :initform nil)))
          t)
    (monk-object-note-layout 'monk-test-grow)
    (should (monk-object-stale-p old))
    ;; reading the old object through the new class is wrong: c is now b
    (should (eq 3 (slot-value old 'b)))
    (let ((new (monk-object-rebuild old)))
      (should-not (monk-object-stale-p new))
      (should (eq 1 (slot-value new 'a)))
      (should (eq 'default-b (slot-value new 'b)))
      (should (eq 3 (slot-value new 'c)))
      ;; a current object comes back as itself
      (should (eq new (monk-object-rebuild new))))
    ;; monk-machine itself has a layout on record, so the agent path works
    (should (assoc (cons 'monk-machine (length (eieio-class-slots 'monk-machine))) monk-object--layouts))
    (let ((m (monk-test-machine)))
      (should (eq m (monk-machine-rebuild m))))))

(ert-deftest monk-test-reload-rebuilds-machines-it-keeps ()
  "`monk-reload' rebuilds the agent machine, the inventory, and the watch machines."
  (require 'monk-reload)
  (let* ((m (monk-test-machine))
         (monk-inventory (list m))
         (monk-agent--machine m)
         (rebuilt nil)
         (monk-log-level nil))
    (cl-letf (((symbol-function 'monk-machine-rebuild)
               (lambda (x) (push x rebuilt) x))
              ((symbol-function 'load) (lambda (&rest _) t))
              ((symbol-function 'monk-reload--refresh-buffers) (lambda () 0))
              ((symbol-function 'monk-reload--clear-caches) (lambda () 0))
              ((symbol-function 'message) #'ignore))
      (monk-reload)
      (should (memq m rebuilt)))))

(ert-deftest monk-test-connect-targets-and-remote-plumbing ()
  "`monk-connect' offers this host and every addressed inventory machine.
A typed METHOD:HOST or TRAMP prefix works too; the remote socket is
read from the pid file over TRAMP and reached through nc or socat."
  (require 'monk-connect)
  (let* ((far (monk-local-machine :name "far"))
         (box (monk-local-machine :name "box"))
         (monk-inventory nil))
    (oset far address "far.invalid") (oset far method "rpc")
    (oset box address "box.invalid")
    (setq monk-inventory (list (monk-test-machine) far box))
    (let ((targets (monk-connect-targets)))
      (should (equal '("local" . nil) (car targets)))
      (should (equal "/rpc:far.invalid:" (cdr (assoc "far (rpc far.invalid)" targets))))
      (should (equal "/ssh:box.invalid:" (cdr (assoc "box (ssh box.invalid)" targets))))
      (should (= 3 (length targets))))
    ;; the answers a person can type
    (dolist (case '(("local" . nil)
                    ("far (rpc far.invalid)" . "/rpc:far.invalid:")
                    ("rpc:elsewhere" . "/rpc:elsewhere:")
                    ("pi@kamen.local" . "/ssh:pi@kamen.local:")
                    ("/sshx:host:" . "/sshx:host:")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (car case))))
        (should (equal (cdr case) (monk-connect-read-target)))))
    ;; the pipe command: nc first, socat second, the socket appended right
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (and (equal name "socat") "/usr/bin/socat"))))
      (should (equal '("socat" "-" "UNIX-CONNECT:/tmp/agent.sock")
                     (monk-connect-pipe-command nil "/tmp/agent.sock"))))
    (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (and (equal name "nc") "/usr/bin/nc"))))
      (should (equal '("nc" "-U" "/tmp/agent.sock") (monk-connect-pipe-command nil "/tmp/agent.sock"))))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should-error (monk-connect-pipe-command nil "/tmp/agent.sock") :type 'user-error))
    ;; the remote pid file: read through the prefix, pid checked there
    (monk-test-with-tmpdir dir
      (let ((pidfile (concat dir "agent.pid")))
        (with-temp-file pidfile
          (insert (monk-event-line '((pid . 4242) (socket . "/home/u/.local/share/monk/agent.sock")))))
        (cl-letf (((symbol-function 'expand-file-name)
                   (lambda (name &optional default)
                     (if (and default (equal default "/ssh:box.invalid:")) pidfile name)))
                  ((symbol-function 'process-file)
                   (lambda (program &rest args)
                     (should (equal program "kill"))
                     (should (member "4242" args))
                     0)))
          (should (equal "/home/u/.local/share/monk/agent.sock"
                         (monk-connect-remote-socket "/ssh:box.invalid:"))))
        ;; a dead pid there is no agent
        (cl-letf (((symbol-function 'expand-file-name)
                   (lambda (name &optional default)
                     (if (and default (equal default "/ssh:box.invalid:")) pidfile name)))
                  ((symbol-function 'process-file) (lambda (&rest _) 1)))
          (should-error (monk-connect-remote-socket "/ssh:box.invalid:") :type 'user-error))))))

(ert-deftest monk-test-history-migrates-under-open-connection ()
  "A schema version raised by a reload is applied to the connection already open."
  (monk-test-with-history
    (let ((db (monk-history-db)))
      ;; pretend the connection was migrated by older code: drop a column's worth of knowledge
      (setq monk-history--db-schema (1- monk-history-schema-version))
      (sqlite-execute db "CREATE TABLE IF NOT EXISTS probe (x INTEGER)")
      (let ((monk-history-schema (append monk-history-schema
                                         '("CREATE TABLE IF NOT EXISTS probe2 (y INTEGER)"))))
        ;; same version as before: nothing re-applied
        (setq monk-history--db-schema monk-history-schema-version)
        (monk-history-db)
        (should-not (sqlite-select db "SELECT name FROM sqlite_master WHERE name = 'probe2'"))
        ;; version moved on: the schema runs again on the open connection
        (let ((monk-history-schema-version (1+ monk-history-schema-version)))
          (monk-history-db)
          (should (sqlite-select db "SELECT name FROM sqlite_master WHERE name = 'probe2'"))
          (should (eql monk-history--db-schema monk-history-schema-version)))))
    ;; the real migration adds owner and scope to an old database
    (let ((db (monk-history-db)))
      (sqlite-execute db "ALTER TABLE runs RENAME TO runs_old")
      (sqlite-execute db "CREATE TABLE runs (id TEXT PRIMARY KEY, started TEXT NOT NULL, ended TEXT, dry_run INTEGER NOT NULL, hosts TEXT NOT NULL, policy TEXT, passes INTEGER, plan_id TEXT, ok INTEGER, kept INTEGER, repaired INTEGER, would_repair INTEGER, partial INTEGER, failed INTEGER, skipped INTEGER, unreachable INTEGER, flapping TEXT, reasons TEXT)")
      (setq monk-history--db-schema nil)
      (monk-history-db)
      (should (member "owner" (mapcar #'cadr (sqlite-select db "PRAGMA table_info(runs)"))))
      (should (member "scope" (mapcar #'cadr (sqlite-select db "PRAGMA table_info(results)")))))))

(ert-deftest monk-test-agent-reload-loads-new-libraries-and-starts-socket ()
  "A timed converge loads a library that did not exist at start, and serves the socket."
  (require 'monk-agent)
  (skip-unless (featurep 'make-network-process))
  (monk-test-with-tmpdir dir
    (let* ((process-environment (cons (concat monk-home-environment-variable "=" dir "home") process-environment))
           (monk-agent-policy-file nil)
           (monk-agent--server nil) (monk-agent--subscribers nil)
           (monk-log-level nil)
           (required nil)
           (hidden t))
      (unwind-protect
          (cl-letf (((symbol-function 'monk-stale-libraries) (lambda () nil))
                    ;; monk-connect looks absent until it is "loaded" once
                    ((symbol-function 'featurep)
                     (let ((real (symbol-function 'featurep)))
                       (lambda (f &rest r) (and (not (and hidden (eq f 'monk-connect))) (apply real f r)))))
                    ((symbol-function 'require)
                     (let ((real (symbol-function 'require)))
                       (lambda (f &rest r) (push f required) (when (eq f 'monk-connect) (setq hidden nil))
                         (apply real f r)))))
            (let ((note (monk-agent--reload-if-stale)))
              (should (memq 'monk-connect required))
              (should (string-match-p "loaded new monk-connect" note))
              (should (string-match-p "socket started" note))
              (should monk-agent--server)
              (should (file-exists-p (monk-agent-local-socket-path)))
              ;; a second pass changes nothing
              (should-not (monk-agent--reload-if-stale))))
        (monk-agent-stop-local)))))

(ert-deftest monk-test-agent-source-watch-and-sigusr1 ()
  "An edit under a source directory reloads and converges after the debounce; so does SIGUSR1."
  (require 'monk-agent)
  (monk-test-with-tmpdir dir
    (let* ((converges nil)
           (monk-agent-source-debounce 0.05)
           (monk-agent-policy-file (concat dir "policy.el"))
           (monk-agent--policy #'ignore)
           (monk-agent--source-descriptors nil)
           (monk-agent--source-timer nil)
           (monk-agent--converging nil) (monk-agent--converge-again nil)
           (monk-log-level nil))
      (with-temp-file monk-agent-policy-file (insert ";; -*- lexical-binding: t; -*-\n"))
      (cl-letf (((symbol-function 'monk-agent--full-converge-1)
                 (lambda (_policy timed) (push timed converges)))
                ((symbol-function 'monk-cli-print-line) #'ignore)
                ((symbol-function 'locate-library) (lambda (&rest _) (concat dir "lisp/monk.el"))))
        (make-directory (concat dir "lisp") t)
        (should (equal (list (file-truename (concat dir "lisp/")) (file-truename dir))
                       (monk-agent-source-directories)))
        ;; a symlinked tree resolves to the real one: that is where events come from
        (make-symbolic-link (concat dir "lisp") (concat dir "link"))
        (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) (concat dir "link/monk.el"))))
          (should (equal (file-truename (concat dir "lisp/")) (car (monk-agent-source-directories)))))
        ;; events are injected: delivery itself is the kernel's business
        (monk-agent--source-event (list 'd 'changed (concat dir "lisp/monk-sync.el")))
        (monk-agent--source-event (list 'd 'changed (concat dir "lisp/monk-ui.el")))
        (should monk-agent--source-timer)
        (should-not converges)
        (let ((deadline (+ (float-time) 2)))
          (while (and (null converges) (< (float-time) deadline)) (sit-for 0.02)))
        ;; two edits, one converge, timed
        (should (equal '(t) converges))
        ;; a non-.el file does nothing
        (monk-agent--source-event (list 'd 'changed (concat dir "lisp/notes.txt")))
        (should-not monk-agent--source-timer)
        ;; the fswatch filter feeds NUL-separated paths through the same handler
        (let ((proc (start-process "fake" nil "sleep" "5")))
          (unwind-protect
              (progn
                (monk-agent--source-fswatch-filter proc (concat dir "lisp/a.el\0" dir "lisp/b.txt\0" dir "lisp/c."))
                (should monk-agent--source-timer)
                (should (equal (concat dir "lisp/c.") (process-get proc 'monk-partial)))
                (cancel-timer monk-agent--source-timer) (setq monk-agent--source-timer nil))
            (delete-process proc)))
        ;; SIGUSR1 handler
        (monk-agent--on-sigusr1)
        (should (equal '(t t) converges))
        ;; a request during a converge runs once after it, not nested
        (setq converges nil)
        (cl-letf (((symbol-function 'monk-agent--full-converge-1)
                   (lambda (_policy timed)
                     (push timed converges)
                     (when (= 1 (length converges))
                       (monk-agent--full-converge #'ignore t)
                       (should (= 1 (length converges)))))))
          (monk-agent--full-converge #'ignore t)
          (should (= 2 (length converges))))))))

(ert-deftest monk-test-agent-poke-finds-process ()
  "`monk-agent-poke' signals the pid file's agent, else a Monk agent process, else errors."
  (require 'monk-agent)
  (let ((sent nil))
    (cl-letf (((symbol-function 'signal-process) (lambda (pid sig) (push (cons pid sig) sent) 0))
              ((symbol-function 'monk-agent-read-pid-file) (lambda () '((pid . 4242)))))
      (should (= 4242 (monk-agent-poke)))
      (should (equal '((4242 . sigusr1)) sent)))
    (setq sent nil)
    (cl-letf (((symbol-function 'signal-process) (lambda (pid sig) (push (cons pid sig) sent) 0))
              ((symbol-function 'monk-agent-read-pid-file) (lambda () nil))
              ((symbol-function 'list-system-processes) (lambda () '(7 8 9)))
              ((symbol-function 'process-attributes)
               (lambda (p) (pcase p
                             (8 '((args . "emacs --batch -l monk-tzz-sync -f monk-tzz-agent")))
                             (_ '((args . "bash")))))))
      (should (= 8 (monk-agent-poke)))
      (should (equal '((8 . sigusr1)) sent)))
    (cl-letf (((symbol-function 'monk-agent-read-pid-file) (lambda () nil))
              ((symbol-function 'list-system-processes) (lambda () nil)))
      (should-error (monk-agent-poke) :type 'user-error))))

(ert-deftest monk-test-watch-repath-follows-retargeted-symlink ()
  "When a watched path's symlink is retargeted, the repath tick restarts the watch on the new place."
  (monk-test-with-tmpdir dir
    (make-directory (concat dir "a") t)
    (make-directory (concat dir "b") t)
    (make-symbolic-link (concat dir "a") (concat dir "cur"))
    (let ((monk-log-level nil)
          (monk-watch-repath-interval 3600)   ; the tick is called by hand
          (monk-report-function #'ignore)
          (m (monk-test-machine)))
      (unwind-protect
          (cl-letf (((symbol-function 'file-notify-add-watch) (lambda (&rest _) (cl-gensym "desc")))
                    ((symbol-function 'file-notify-rm-watch) #'ignore)
                    ((symbol-function 'file-notify-valid-p) (lambda (_) t))
                    ((symbol-value 'file-notify--library) 'test)
                    ((symbol-value 'monk-watch--delivery) 'yes))
            (monk-watch-start m (lambda (_m) (monk-file (concat dir "cur/f") :content "x")))
            (let ((first monk-watch--current)
                  (sig (monk-watch-path-signature monk-watch--current)))
              (should (equal (list (concat dir "a/f")) sig))
              (should (monk-watch-repath-timer first))
              (setf (monk-watch-runs first) 3)
              ;; nothing moved: the same watch stays
              (monk-watch--repath)
              (should (eq first monk-watch--current))
              ;; retarget: a tick notices and restarts, counters carried
              (delete-file (concat dir "cur"))
              (make-symbolic-link (concat dir "b") (concat dir "cur"))
              (monk-watch--repath)
              (should-not (eq first monk-watch--current))
              (should (equal (list (concat dir "b/f")) (monk-watch-path-signature monk-watch--current)))
              (should (= 3 (monk-watch-runs monk-watch--current)))
              (should (monk-watch--nodes-for-file monk-watch--current (concat dir "b/f")))
              (should-not (monk-watch--nodes-for-file monk-watch--current (concat dir "a/f")))
              (with-current-buffer monk-watch-log-buffer-name
                (should (string-match-p "paths moved" (buffer-string))))))
        (monk-watch-stop)))))

(ert-deftest monk-test-agent-source-repath ()
  "The source watch re-resolves its directories and restarts when the symlink moved."
  (require 'monk-agent)
  (monk-test-with-tmpdir dir
    (make-directory (concat dir "v1") t)
    (make-directory (concat dir "v2") t)
    (make-symbolic-link (concat dir "v1") (concat dir "monk"))
    (let ((monk-agent-policy-file nil)
          (monk-agent--source-dirs nil)
          (monk-agent--source-repath-timer nil)
          (monk-watch-repath-interval 3600)
          (starts 0)
          (monk-log-level nil))
      (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) (concat dir "monk/monk.el")))
                ((symbol-function 'monk-cli-print-line) #'ignore)
                ((symbol-function 'monk-agent-source-watch-start)
                 (lambda () (cl-incf starts) (setq monk-agent--source-dirs (monk-agent-source-directories)) 'fake)))
        (monk-agent-source-watch-start)
        (should (equal (list (file-truename (concat dir "v1/"))) monk-agent--source-dirs))
        (monk-agent--source-repath)
        (should (= 1 starts))
        (delete-file (concat dir "monk"))
        (make-symbolic-link (concat dir "v2") (concat dir "monk"))
        (monk-agent--source-repath)
        (should (= 2 starts))
        (should (equal (list (file-truename (concat dir "v2/"))) monk-agent--source-dirs))))))

(provide 'monk-test)
;;; monk-test.el ends here
