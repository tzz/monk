;;; monk-event.el --- Result events as JSON Lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, data

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

;; One JSON object per line, one line per resource outcome.  This is
;; the format a standalone agent writes to its ledger and a controller
;; reads back.  It is also the shape every other configuration tool can
;; be normalized to: Salt {result, changes, comment, duration}, Ansible
;; {changed, failed, msg, diff}, Chef {status, diff, duration}, CFEngine
;; kept/repaired/notkept.  See docs/mgmt-review-and-interop.md.
;;
;; The record is stable: fields are added, never renamed.  `rgi' is the
;; format version.  `status' uses Monk's vocabulary plus `unknown';
;; `native' carries a foreign tool's own status verbatim.

;;; Code:

(require 'monk)
(require 'json)

(defconst monk-event-version 2
  "Value of the rgi field.  Bump when a field changes meaning.")

(defconst monk-event-kinds '(result run-start run-end drift pass)
  "Values of the event field.")

;;;; Tags
;;
;; Every event carries a tag: a `/'-joined path, like Salt's
;; `salt/job/<jid>/ret/<id>' and `salt/beacon/<id>/<beacon>/...'.
;; Consumers filter by prefix: an etcd watch on `monk/run/', a socket
;; client that wants `monk/drift/<host>'.  Resource names never go in
;; a tag, because paths contain `/'; the name is in the data, and the
;; per-result tag uses a sequence number, like Salt's `__run_num__'.
;;
;;   monk/run/<run>/new                 run-start: hosts, policy, dry_run
;;   monk/run/<run>/pass/<n>            pass end: repaired count
;;   monk/run/<run>/prog/<host>/<seq>   one result
;;   monk/run/<run>/end                 run-end: summary, ok, flapping
;;   monk/drift/<host>                  a watch saw drift
;;   monk/agent/<host>/start            an agent came up
;;
;; Run ids sort by time, like Salt's jid.

(defconst monk-event-tag-base "monk"
  "First element of every Monk event tag.")

(defun monk-event-tag (&rest parts)
  "Return the tag for PARTS, joined with `/' under `monk-event-tag-base'.
Numbers are printed; nil parts are dropped; a `/' inside a part is
replaced by `_' so the tag stays a path."
  (mapconcat (lambda (part)
               (replace-regexp-in-string "/" "_" (format "%s" part) t t))
             (cons monk-event-tag-base (delq nil parts))
             "/"))

(defun monk-event-tag-matches-p (prefix tag)
  "Return non-nil when TAG is PREFIX or is under PREFIX as a path."
  (or (equal tag prefix)
      (string-prefix-p (concat prefix "/") tag)))

(defconst monk-event-time-format "%FT%T.%3NZ"
  "ISO 8601 UTC with milliseconds.")

(defun monk-event--time (&optional time)
  "Return TIME, default now, in `monk-event-time-format'."
  (format-time-string monk-event-time-format time t))

(defun monk-event--json (object)
  "Serialize OBJECT to one line of JSON.
Alists with symbol keys become objects; nil becomes null; vectors and
lists become arrays."
  (json-serialize object :null-object nil :false-object :false))

(defun monk-event--plist-to-alist (plist)
  "Return PLIST as an alist with keyword names as symbols.
Values that JSON cannot carry become their printed representation."
  (let (out)
    (while plist
      (let ((key (intern (string-remove-prefix ":" (symbol-name (pop plist)))))
            (value (pop plist)))
        (push (cons key (monk-event--jsonable value)) out)))
    (nreverse out)))

(defun monk-event-alist-to-plist (alist)
  "Return ALIST, as `monk-event--plist-to-alist' made it, as a changes plist.
Keys become keywords.  Vectors from JSON become lists.  Anything that
is not an alist returns unchanged."
  (if (and (listp alist) (consp (car-safe alist)) (symbolp (caar alist)))
      (let (out)
        (dolist (cell alist)
          (setq out (plist-put out (intern (concat ":" (symbol-name (car cell))))
                               (if (vectorp (cdr cell)) (append (cdr cell) nil) (cdr cell)))))
        out)
    alist))

(defun monk-event--jsonable (value)
  "Return VALUE in a form `json-serialize' accepts."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((stringp value) value)
   ((numberp value) value)
   ((symbolp value) (symbol-name value))
   ((and (proper-list-p value)
         (cl-every (lambda (c) (and (consp c) (symbolp (car c)))) value))
    (mapcar (lambda (c) (cons (car c) (monk-event--jsonable (cdr c)))) value))
   ((proper-list-p value) (vconcat (mapcar #'monk-event--jsonable value)))
   ((vectorp value) (vconcat (mapcar #'monk-event--jsonable value)))
   ;; A dotted pair or another object JSON cannot carry.
   (t (format "%S" value))))

(defun monk-event-result (result run &optional pass seq)
  "Return the event alist for RESULT inside RUN at PASS as item SEQ.
SEQ is the position of the result in the run's schedule, from 0; it
goes in the tag.  Nil SEQ looks the result up in the run."
  (let ((seq (or seq (monk-run-result-seq run result))))
    `((rgi . ,monk-event-version)
      (event . "result")
      (tag . ,(monk-event-tag "run" (monk-run-id run) "prog" (monk-result-machine result) seq))
      (ts . ,(monk-event--time))
      (host . ,(monk-result-machine result))
      (run . ,(monk-run-id run))
      (pass . ,pass)
      (seq . ,seq)
    (type . ,(monk-result-type result))
    (name . ,(monk-result-name result))
    (status . ,(symbol-name (monk-result-status result)))
    (changed . ,(if (monk-result-changed-p result) t :false))
    (degree . ,(monk-result-degree result))
    (changes . ,(and (monk-result-changes result)
                     (monk-event--plist-to-alist (monk-result-changes result))))
    (diff . ,(monk-event-diff (monk-result-diff result)))
    (comment . ,(monk-result-comment result))
    (duration_ms . ,(* 1000.0 (monk-result-duration result)))
      (dry_run . ,(if (monk-run-dry-run run) t :false))
      (censored . ,(if (monk-result-censored result) t :false))
      (native . ((tool . "monk")
                 (source . ,(car (monk-result-origin result))))))))

(defun monk-event-diff (diff)
  "Return DIFF as JSON data: a list of objects with before/after and headers.
Ansible's shape, one object per entry.  A rendered string entry keeps
its text under `rendered'."
  (and diff
       (vconcat
        (mapcar (lambda (e)
                  (pcase (monk-diff-entry-kind e)
                    ('rendered `((rendered . ,(monk-diff-entry-after e))))
                    ('attributes
                     `((before . ,(monk-event--jsonable (monk-diff-entry-before e)))
                       (after . ,(monk-event--jsonable (monk-diff-entry-after e)))
                       (before_header . ,(monk-diff-entry-before-header e))
                       (after_header . ,(monk-diff-entry-after-header e))
                       (kind . "attributes")))
                    (_ `((before . ,(monk-diff-entry-before e))
                         (after . ,(monk-diff-entry-after e))
                         (before_header . ,(monk-diff-entry-before-header e))
                         (after_header . ,(monk-diff-entry-after-header e))
                         (kind . "content")))))
                (monk-diff-entries diff)))))

(defun monk-event-diff-to-entries (data)
  "Return `monk-diff-entry' objects from DATA as `monk-event-diff' wrote it.
A plain string, from an older event, becomes one rendered entry."
  (cond
   ((null data) nil)
   ((stringp data) (list (monk-diff-entry-create :after data :kind 'rendered)))
   (t (mapcar (lambda (o)
                (cond
                 ((alist-get 'rendered o)
                  (monk-diff-entry-create :after (alist-get 'rendered o) :kind 'rendered))
                 ((equal (alist-get 'kind o) "attributes")
                  (monk-diff-entry-create :before (alist-get 'before o) :after (alist-get 'after o)
                                          :before-header (alist-get 'before_header o)
                                          :after-header (alist-get 'after_header o)
                                          :kind 'attributes))
                 (t (monk-diff-entry-create :before (alist-get 'before o) :after (alist-get 'after o)
                                            :before-header (alist-get 'before_header o)
                                            :after-header (alist-get 'after_header o)
                                            :kind 'content))))
              (append data nil)))))

(defun monk-run-result-seq (run result)
  "Return the position of RESULT among the results of RUN, or nil."
  (cl-position result (monk-run-results run)
               :test (lambda (a b) (equal (monk-result-key a) (monk-result-key b)))))

(defun monk-event-pass (run pass repaired)
  "Return the event alist for the end of PASS of RUN with REPAIRED changes."
  `((rgi . ,monk-event-version)
    (event . "pass")
    (tag . ,(monk-event-tag "run" (monk-run-id run) "pass" pass))
    (ts . ,(monk-event--time))
    (run . ,(monk-run-id run))
    (pass . ,pass)
    (repaired . ,repaired)))

(defun monk-event-run (kind run)
  "Return the event alist for the start or end of RUN.  KIND is a symbol."
  `((rgi . ,monk-event-version)
    (event . ,(symbol-name kind))
    (tag . ,(monk-event-tag "run" (monk-run-id run) (if (eq kind 'run-start) "new" "end")))
    (ts . ,(monk-event--time))
    (run . ,(monk-run-id run))
    (hosts . ,(vconcat (mapcar (lambda (m) (oref m name)) (monk-run-machines run))))
    (dry_run . ,(if (monk-run-dry-run run) t :false))
    ,@(when (eq kind 'run-end)
        `((passes . ,(monk-run-passes run))
          (ok . ,(if (monk-run-ok-p run) t :false))
          (exit_code . ,(monk-run-exit-code run))
          (unreachable . ,(vconcat (mapcar #'car (monk-run-unreachable run))))
          (converged . ,(if (monk-run-converged-p run) t :false))
          (flapping . ,(vconcat (mapcar (lambda (k) (format "%s:%s" (car k) (cdr k)))
                                        (monk-run-flapping run))))
          (summary . ,(monk-run-summary run))))))

(defun monk-event-drift (key path action)
  "Return the event alist for drift on KEY at PATH after ACTION."
  `((rgi . ,monk-event-version)
    (event . "drift")
    (tag . ,(monk-event-tag "drift" (car key)))
    (ts . ,(monk-event--time))
    (host . ,(car key))
    (name . ,(cdr key))
    (path . ,path)
    ;; Salt's beacon shape: id, path, change.
    (change . ,(format "%s" action))
    (action . ,(format "%s" action))))

(defun monk-event-agent-start (host)
  "Return the event alist for an agent that came up on HOST."
  `((rgi . ,monk-event-version)
    (event . "agent-start")
    (tag . ,(monk-event-tag "agent" host "start"))
    (ts . ,(monk-event--time))
    (host . ,host)))

(defun monk-event-line (event)
  "Return EVENT, an alist, as one JSON line with a trailing newline."
  (concat (monk-event--json event) "\n"))

(defun monk-run-id (run)
  "Return a stable identifier for RUN: its start time to the millisecond."
  (format-time-string "r%Y%m%dT%H%M%S.%3N" (monk-run-start-time run) t))

(defun monk-run-id-time (id)
  "Return the start time that run ID encodes, as a time value."
  (unless (string-match "\\`r\\([0-9]\\{8\\}\\)T\\([0-9]\\{6\\}\\)\\.\\([0-9]\\{3\\}\\)\\'" id)
    (signal 'monk-error (list "Not a run id" id)))
  (let* ((millis (string-to-number (match-string 3 id)))
         (seconds (time-convert
                   (parse-iso8601-time-string
                    (format "%s-%s-%sT%s:%s:%sZ"
                            (substring id 1 5) (substring id 5 7) (substring id 7 9)
                            (substring id 10 12) (substring id 12 14) (substring id 14 16)))
                   'integer)))
    ;; An exact (TICKS . HZ) pair: a float here loses the last digit
    ;; when the id is printed again.
    (time-convert (cons (+ (* seconds 1000) millis) 1000) t)))

(defun monk-run-events (run)
  "Return all events of RUN as a list of alists: start, results, end."
  (append (list (monk-event-run 'run-start run))
          (let ((seq -1))
            (mapcar (lambda (r) (monk-event-result r run (monk-run-passes run) (cl-incf seq)))
                    (monk-run-results run)))
          (list (monk-event-run 'run-end run))))

(defun monk-run-write-events (run file)
  "Append the events of RUN to FILE as JSON Lines."
  (with-temp-buffer
    (dolist (event (monk-run-events run))
      (insert (monk-event-line event)))
    (let ((coding-system-for-write 'utf-8-unix))
      (append-to-file (point-min) (point-max) file))))

;;;; Reading

(defun monk-event-parse (line)
  "Parse LINE, one JSON object, into an alist.  Return nil on failure.
Arrays become vectors, so a parsed event can be serialized again:
`json-serialize' reads a list of strings as a malformed alist."
  (condition-case nil
      (json-parse-string line :object-type 'alist :array-type 'array
                         :null-object nil :false-object :false)
    (error nil)))

(defun monk-event-to-result (event)
  "Return a `monk-result' built from a result EVENT alist."
  (monk-result-create
   :machine (alist-get 'host event)
   :name (alist-get 'name event)
   :type (alist-get 'type event)
   :status (intern (or (alist-get 'status event) "unknown"))
   :degree (or (alist-get 'degree event) 0.0)
   :changes (monk-event-alist-to-plist (alist-get 'changes event))
   :comment (alist-get 'comment event)
   :diff (monk-event-diff-to-entries (alist-get 'diff event))
   :duration (/ (or (alist-get 'duration_ms event) 0) 1000.0)
   :origin (let ((source (alist-get 'source (alist-get 'native event))))
             (and source (cons source (alist-get 'name event))))
   :censored (eq t (alist-get 'censored event))))

(defun monk-events-to-run (events)
  "Rebuild a `monk-run' from EVENTS, a list of alists from one run.
Machines are stand-ins named from the events; facts are empty."
  (let* ((results (mapcar #'monk-event-to-result
                          (seq-filter (lambda (e) (equal (alist-get 'event e) "result"))
                                      events)))
         (end (seq-find (lambda (e) (equal (alist-get 'event e) "run-end")) events))
         (start (seq-find (lambda (e) (equal (alist-get 'event e) "run-start")) events))
         (hosts (delete-dups (append (append (alist-get 'hosts start) nil)
                                     (mapcar #'monk-result-machine results)))))
    (monk-run-create
     :machines (mapcar (lambda (h) (monk-machine :name h :address h)) hosts)
     :results results
     :passes (or (alist-get 'passes end) 1)
     :dry-run (eq t (alist-get 'dry_run (or end start))))))

(provide 'monk-event)
;;; monk-event.el ends here
