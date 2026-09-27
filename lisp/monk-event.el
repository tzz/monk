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

(defconst monk-event-version 1
  "Value of the rgi field.  Bump when a field changes meaning.")

(defconst monk-event-kinds '(result run-start run-end drift)
  "Values of the event field.")

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

(defun monk-event-result (result run &optional pass)
  "Return the event alist for RESULT inside RUN at PASS."
  `((rgi . ,monk-event-version)
    (event . "result")
    (ts . ,(monk-event--time))
    (host . ,(monk-result-machine result))
    (run . ,(monk-run-id run))
    (pass . ,pass)
    (type . ,(monk-result-type result))
    (name . ,(monk-result-name result))
    (status . ,(symbol-name (monk-result-status result)))
    (changed . ,(if (monk-result-changed-p result) t :false))
    (degree . ,(monk-result-degree result))
    (changes . ,(and (monk-result-changes result)
                     (monk-event--plist-to-alist (monk-result-changes result))))
    (diff . ,(monk-result-diff result))
    (comment . ,(monk-result-comment result))
    (duration_ms . ,(* 1000.0 (monk-result-duration result)))
    (dry_run . ,(if (monk-run-dry-run run) t :false))
    (native . ((tool . "monk")))))

(defun monk-event-run (kind run)
  "Return the event alist for the start or end of RUN.  KIND is a symbol."
  `((rgi . ,monk-event-version)
    (event . ,(symbol-name kind))
    (ts . ,(monk-event--time))
    (run . ,(monk-run-id run))
    (hosts . ,(vconcat (mapcar (lambda (m) (oref m name)) (monk-run-machines run))))
    (dry_run . ,(if (monk-run-dry-run run) t :false))
    ,@(when (eq kind 'run-end)
        `((passes . ,(monk-run-passes run))
          (ok . ,(if (monk-run-ok-p run) t :false))
          (converged . ,(if (monk-run-converged-p run) t :false))
          (flapping . ,(vconcat (mapcar (lambda (k) (format "%s:%s" (car k) (cdr k)))
                                        (monk-run-flapping run))))
          (summary . ,(monk-run-summary run))))))

(defun monk-event-drift (key path action)
  "Return the event alist for drift on KEY at PATH after ACTION."
  `((rgi . ,monk-event-version)
    (event . "drift")
    (ts . ,(monk-event--time))
    (host . ,(car key))
    (name . ,(cdr key))
    (path . ,path)
    (action . ,(format "%s" action))))

(defun monk-event-line (event)
  "Return EVENT, an alist, as one JSON line with a trailing newline."
  (concat (monk-event--json event) "\n"))

(defun monk-run-id (run)
  "Return a stable identifier for RUN: its start time to the millisecond."
  (format-time-string "r%Y%m%dT%H%M%S.%3N" (monk-run-start-time run) t))

(defun monk-run-events (run)
  "Return all events of RUN as a list of alists: start, results, end."
  (append (list (monk-event-run 'run-start run))
          (mapcar (lambda (r) (monk-event-result r run (monk-run-passes run)))
                  (monk-run-results run))
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
   :changes (alist-get 'changes event)
   :comment (alist-get 'comment event)
   :diff (alist-get 'diff event)
   :duration (/ (or (alist-get 'duration_ms event) 0) 1000.0)))

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
