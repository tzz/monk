;;; monk-history.el --- Persistent run history in SQLite  -*- lexical-binding: t; -*-

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

;; Every finished run is written to a SQLite database: one row per
;; run and one row per result.  Plans, applies, and watch runs all
;; count.  The columns are the fields of the event record in
;; monk-event.el, so the ledger and the database say the same thing.
;;
;; The database lives under `monk-home-directory':
;;
;;   $MONK_HOME when set,
;;   /etc/monk when Emacs runs as root,
;;   $XDG_DATA_HOME/monk, else ~/.local/share/monk, otherwise.
;;
;; `monk-history-file' overrides the file itself.  Recording starts
;; when this file loads and stops with `monk-history-mode'.
;;
;; Queries:
;;
;;   (monk-history-runs :host "kamen" :since "7d" :limit 20)
;;   (monk-history-results :name "~/.zshrc" :status "repaired")
;;
;; Both accept the same filters and return lists of alists with the
;; column names as symbols.  `monk-history-repairs' is the resource
;; timeline: every time a resource changed, with what changed.
;;
;; The agent answers `history-runs' and `history-results' over its
;; socket with the same filters, so a controller can ask a remote
;; machine for its history without reading its database.
;;
;; This file needs Emacs built with SQLite: `sqlite-available-p'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'monk)
(require 'monk-event)

(unless (sqlite-available-p)
  (error "Monk history needs an Emacs built with SQLite support"))

;;;; Location

(defconst monk-home-environment-variable "MONK_HOME"
  "Environment variable that names the Monk state directory.")

(defconst monk-home-root-directory "/etc/monk"
  "Monk state directory when Emacs runs as root.")

(defun monk-home-directory ()
  "Return the directory for Monk state on this host.
$MONK_HOME wins.  Root uses `monk-home-root-directory'.  Anyone else
uses $XDG_DATA_HOME/monk or ~/.local/share/monk."
  (file-name-as-directory
   (or (getenv monk-home-environment-variable)
       (if (zerop (user-uid))
           monk-home-root-directory
         (expand-file-name "monk" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))))

(defcustom monk-history-file nil
  "Path of the history database.
Nil means history.sqlite under `monk-home-directory'."
  :type '(choice (const nil) file)
  :group 'monk)

(defun monk-history-path ()
  "Return the history database path."
  (expand-file-name (or monk-history-file
                        (concat (monk-home-directory) "history.sqlite"))))

;;;; Schema

(defconst monk-history-schema-version 2
  "Schema version stored in the `meta' table.")

(defconst monk-history-schema
  '("CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS runs (
       id TEXT PRIMARY KEY,
       started TEXT NOT NULL,
       ended TEXT,
       dry_run INTEGER NOT NULL,
       hosts TEXT NOT NULL,
       policy TEXT,
       passes INTEGER,
       plan_id TEXT,
       ok INTEGER,
       kept INTEGER, repaired INTEGER, would_repair INTEGER,
       partial INTEGER, failed INTEGER, skipped INTEGER,
       flapping TEXT,
       reasons TEXT)"
    "CREATE TABLE IF NOT EXISTS results (
       run_id TEXT NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
       host TEXT NOT NULL,
       name TEXT NOT NULL,
       type TEXT,
       status TEXT NOT NULL,
       degree REAL,
       changes TEXT,
       comment TEXT,
       diff TEXT,
       duration_ms REAL,
       source TEXT,
       seq INTEGER,
       pass INTEGER)"
    "CREATE TABLE IF NOT EXISTS events (
       ts TEXT NOT NULL,
       tag TEXT NOT NULL,
       run_id TEXT,
       data TEXT)"
    "CREATE INDEX IF NOT EXISTS events_tag ON events(tag)"
    "CREATE INDEX IF NOT EXISTS events_ts ON events(ts)"
    "CREATE INDEX IF NOT EXISTS results_run ON results(run_id)"
    "CREATE INDEX IF NOT EXISTS results_key ON results(host, name)"
    "CREATE INDEX IF NOT EXISTS runs_started ON runs(started)")
  "Statements that create the schema.  Each is idempotent.")

(defconst monk-history-run-columns
  '(id started ended dry_run hosts policy passes plan_id ok
    kept repaired would_repair partial failed skipped flapping reasons)
  "Columns of the runs table, in table order.")

(defconst monk-history-result-columns
  '(run_id host name type status degree changes comment diff duration_ms source seq pass)
  "Columns of the results table, in table order.")

;;;; Connection

(defvar monk-history--db nil
  "Open connection, or nil.")

(defvar monk-history--db-path nil
  "Path `monk-history--db' was opened on.")

(defun monk-history-db ()
  "Return the open database, opening or creating it when needed."
  (let ((path (monk-history-path)))
    (unless (and monk-history--db (equal path monk-history--db-path)
                 (sqlitep monk-history--db))
      (monk-history-close)
      (make-directory (file-name-directory path) t)
      (setq monk-history--db (sqlite-open path)
            monk-history--db-path path)
      (sqlite-execute monk-history--db "PRAGMA journal_mode = WAL")
      (sqlite-execute monk-history--db "PRAGMA foreign_keys = ON")
      (dolist (statement monk-history-schema)
        (sqlite-execute monk-history--db statement))
      (monk-history--migrate monk-history--db)
      (sqlite-execute monk-history--db
                      "INSERT OR REPLACE INTO meta (key, value) VALUES ('schema', ?)"
                      (list (number-to-string monk-history-schema-version))))
    monk-history--db))

(defun monk-history--migrate (db)
  "Bring an older DB up to `monk-history-schema-version'.
Columns are added, never removed, so an old Monk still reads the file."
  (let ((columns (mapcar #'cadr (sqlite-select db "PRAGMA table_info(results)"))))
    (dolist (column '("seq" "pass"))
      (unless (member column columns)
        (sqlite-execute db (format "ALTER TABLE results ADD COLUMN %s INTEGER" column))))))

(defun monk-history-close ()
  "Close the database connection, if open."
  (when (and monk-history--db (sqlitep monk-history--db))
    (sqlite-close monk-history--db))
  (setq monk-history--db nil
        monk-history--db-path nil))

;;;; Recording

(defun monk-history--json (value)
  "Return VALUE as a JSON string, or nil for nil."
  (and value (monk-event--json (monk-event--jsonable value))))

(defun monk-history--bool (value)
  "Return 1 for non-nil VALUE, else 0."
  (if value 1 0))

(defun monk-history-record (run)
  "Write RUN and its results to the database.  Return the run id.
A run id that exists already is replaced, so recording is idempotent."
  (let ((db (monk-history-db))
        (id (monk-run-id run))
        (seq -1))
    (sqlite-transaction db)
    (condition-case err
        (progn
          (sqlite-execute db "DELETE FROM results WHERE run_id = ?" (list id))
          (sqlite-execute
           db
           (format "INSERT OR REPLACE INTO runs (%s) VALUES (%s)"
                   (mapconcat #'symbol-name monk-history-run-columns ", ")
                   (mapconcat (lambda (_) "?") monk-history-run-columns ", "))
           (list id
                 (monk-event--time (monk-run-start-time run))
                 (and (monk-run-end-time run) (monk-event--time (monk-run-end-time run)))
                 (monk-history--bool (monk-run-dry-run run))
                 (mapconcat (lambda (m) (oref m name)) (monk-run-machines run) " ")
                 (let ((policy (monk-run-policy run)))
                   (and (symbolp policy) policy (symbol-name policy)))
                 (monk-run-passes run)
                 (and (monk-run-plan run) (monk-run-id (monk-run-plan run)))
                 (monk-history--bool (monk-run-ok-p run))
                 (monk-run-count run monk-status-kept)
                 (monk-run-count run monk-status-repaired)
                 (monk-run-count run monk-status-would-repair)
                 (monk-run-count run monk-status-partial)
                 (monk-run-count run monk-status-failed)
                 (monk-run-count run monk-status-skipped)
                 (and (monk-run-flapping-p run)
                      (mapconcat (lambda (k) (format "%s:%s" (car k) (cdr k)))
                                 (monk-run-flapping run) " "))
                 (monk-history--json
                  (mapcar (lambda (cell) (list (cons 'host (caar cell)) (cons 'name (cdar cell))
                                               (cons 'reason (cdr cell))))
                          (monk-run-reasons run)))))
          (dolist (result (monk-run-results run))
            (cl-incf seq)
            (sqlite-execute
             db
             (format "INSERT INTO results (%s) VALUES (%s)"
                     (mapconcat #'symbol-name monk-history-result-columns ", ")
                     (mapconcat (lambda (_) "?") monk-history-result-columns ", "))
             (list id
                   (monk-result-machine result)
                   (monk-result-name result)
                   (monk-result-type result)
                   (symbol-name (monk-result-status result))
                   (monk-result-degree result)
                   (monk-history--json (and (monk-result-changes result)
                                            (monk-event--plist-to-alist (monk-result-changes result))))
                   (monk-result-comment result)
                   (monk-result-diff result)
                   (* 1000.0 (monk-result-duration result))
                   (car (monk-result-origin result))
                   seq
                   (monk-run-passes run))))
          (monk-history--record-event db (monk-event-run 'run-end run) id)
          (sqlite-commit db))
      (error
       (sqlite-rollback db)
       (signal (car err) (cdr err))))
    id))

(defun monk-history--record-event (db event &optional run-id)
  "Insert EVENT, an alist with tag and ts, into the events table of DB."
  (sqlite-execute db "INSERT INTO events (ts, tag, run_id, data) VALUES (?, ?, ?, ?)"
                  (list (alist-get 'ts event) (alist-get 'tag event) run-id
                        (monk-event--json event))))

(defun monk-history-record-event (event)
  "Record EVENT, an alist from monk-event.el, in the events table."
  (monk-history--record-event (monk-history-db) event (alist-get 'run event)))

(defun monk-history-events (&optional prefix limit)
  "Return recorded events whose tag is under PREFIX, newest first, at most LIMIT.
Each is an alist (ts tag run_id data), with data parsed."
  (let* ((where (if prefix " WHERE tag = ? OR tag LIKE ?" ""))
         (params (and prefix (list prefix (concat prefix "/%"))))
         (rows (sqlite-select (monk-history-db)
                              (format "SELECT ts, tag, run_id, data FROM events%s ORDER BY ts DESC%s"
                                      where (if limit (format " LIMIT %d" limit) ""))
                              params)))
    (mapcar (lambda (row)
              (list (cons 'ts (nth 0 row)) (cons 'tag (nth 1 row)) (cons 'run_id (nth 2 row))
                    (cons 'data (and (nth 3 row) (monk-event-parse (nth 3 row))))))
            rows)))

(defun monk-history--on-progress (event data)
  "Record the run when EVENT is `run-end'.  DATA carries :run."
  (when (eq event 'run-end)
    (condition-case err
        (monk-history-record (plist-get data :run))
      (error
       (monk-log 'warn "history: could not record run: %s" (error-message-string err))))))

(define-minor-mode monk-history-mode
  "Record every finished run in the history database."
  :global t
  :group 'monk
  (if monk-history-mode
      (add-hook 'monk-progress-functions #'monk-history--on-progress)
    (remove-hook 'monk-progress-functions #'monk-history--on-progress)))

;;;; Filters

(defconst monk-history-filter-keys
  '(:host :name :type :status :run :since :until :dry-run :changed :limit :offset)
  "Filters that `monk-history-runs' and `monk-history-results' accept.")

(defun monk-history-since-time (since)
  "Return SINCE as an ISO timestamp string.
SINCE is a timestamp string already, a time value, or a duration
such as \"7d\", \"12h\", \"30m\" back from now."
  (cond
   ((null since) nil)
   ((and (stringp since) (string-match "\\`\\([0-9]+\\)\\([smhdw]\\)\\'" since))
    (let* ((n (string-to-number (match-string 1 since)))
           (unit (cdr (assoc (match-string 2 since)
                             '(("s" . 1) ("m" . 60) ("h" . 3600) ("d" . 86400) ("w" . 604800))))))
      (monk-event--time (time-subtract (current-time) (* n unit)))))
   ((stringp since) since)
   (t (monk-event--time since))))

(defun monk-history--where (filters table)
  "Return (SQL . PARAMS) for FILTERS, a plist, against TABLE.
TABLE is `runs' or `results'.  Unknown keys signal."
  (let (clauses params)
    (cl-loop for (key value) on filters by #'cddr
             do (unless (memq key monk-history-filter-keys)
                  (signal 'monk-error (list (format "Unknown history filter %s" key))))
             ;; :dry-run nil is a real filter (applies only); the
             ;; others are absent when nil.
             when (and (not (memq key '(:limit :offset)))
                       (or value (eq key :dry-run)))
             do (pcase key
                  (:host (push (if (eq table 'runs)
                                   "(' ' || hosts || ' ') LIKE ?"
                                 "host = ?")
                               clauses)
                         (push (if (eq table 'runs) (format "%% %s %%" value) value) params))
                  (:name (unless (eq table 'runs)
                           (push "name = ?" clauses) (push value params)))
                  (:type (unless (eq table 'runs)
                           (push "type = ?" clauses) (push value params)))
                  (:status (unless (eq table 'runs)
                             (push "status = ?" clauses)
                             (push (format "%s" value) params)))
                  (:run (push (if (eq table 'runs) "id = ?" "run_id = ?") clauses)
                        (push value params))
                  (:since (push (if (eq table 'runs) "started >= ?"
                                  "run_id IN (SELECT id FROM runs WHERE started >= ?)")
                                clauses)
                          (push (monk-history-since-time value) params))
                  (:until (push (if (eq table 'runs) "started <= ?"
                                  "run_id IN (SELECT id FROM runs WHERE started <= ?)")
                                clauses)
                          (push (monk-history-since-time value) params))
                  (:dry-run (push (if (eq table 'runs) "dry_run = ?"
                                    "run_id IN (SELECT id FROM runs WHERE dry_run = ?)")
                                  clauses)
                            (push (monk-history--bool (eq value t)) params))
                  (:changed (unless (eq table 'runs)
                              (push "status IN ('repaired', 'would-repair', 'partial')" clauses)))))
    (cons (if clauses (concat " WHERE " (string-join (nreverse clauses) " AND ")) "")
          (nreverse params))))

(defun monk-history--rows-to-alists (columns rows)
  "Return ROWS, lists of values, as alists keyed by COLUMNS."
  (mapcar (lambda (row) (cl-mapcar #'cons columns row)) rows))

(defun monk-history--select (table columns order filters)
  "Return rows of TABLE with COLUMNS in ORDER that match FILTERS, as alists."
  (pcase-let* ((`(,where . ,params) (monk-history--where filters table))
               (limit (plist-get filters :limit))
               (offset (plist-get filters :offset))
               (sql (format "SELECT %s FROM %s%s ORDER BY %s%s%s"
                            (mapconcat #'symbol-name columns ", ")
                            table where order
                            (if limit (format " LIMIT %d" limit) "")
                            (if offset (format " OFFSET %d" offset) ""))))
    (monk-history--rows-to-alists
     columns (sqlite-select (monk-history-db) sql params))))

;;;; Queries

(defun monk-history-runs (&rest filters)
  "Return runs, newest first, that match FILTERS.
FILTERS: :host NAME, :run ID, :since WHEN, :until WHEN, :dry-run
BOOL, :limit N, :offset N.  WHEN is a timestamp or a duration such
as \"7d\".  Each run is an alist over `monk-history-run-columns'."
  (monk-history--select 'runs monk-history-run-columns "started DESC" filters))

(defun monk-history-results (&rest filters)
  "Return results, newest run first, that match FILTERS.
FILTERS as for `monk-history-runs' plus :name, :type, :status, and
:changed t for repaired, would-repair, and partial only.  Each
result is an alist over `monk-history-result-columns' plus `started'
and `dry_run' from its run."
  (pcase-let* ((`(,where . ,params) (monk-history--where filters 'results))
               (limit (plist-get filters :limit))
               (offset (plist-get filters :offset))
               (columns (append monk-history-result-columns '(started dry_run)))
               (sql (format "SELECT %s FROM results JOIN runs ON runs.id = results.run_id%s ORDER BY runs.started DESC, results.rowid%s%s"
                            (mapconcat (lambda (c)
                                         (if (memq c '(started dry_run))
                                             (format "runs.%s" c)
                                           (format "results.%s" c)))
                                       columns ", ")
                            (replace-regexp-in-string
                             "\\_<\\(host\\|name\\|type\\|status\\|run_id\\)\\_>" "results.\\1" where)
                            (if limit (format " LIMIT %d" limit) "")
                            (if offset (format " OFFSET %d" offset) ""))))
    (monk-history--rows-to-alists columns (sqlite-select (monk-history-db) sql params))))

(defun monk-history-repairs (host name &rest filters)
  "Return every time resource NAME on HOST changed, newest first.
FILTERS as for `monk-history-results'.  Plans are excluded unless
:dry-run t is given."
  (apply #'monk-history-results :host host :name name :changed t
         (append (unless (plist-member filters :dry-run) '(:dry-run nil)) filters)))

(defun monk-history-run-count (&rest filters)
  "Return how many runs match FILTERS."
  (pcase-let ((`(,where . ,params) (monk-history--where filters 'runs)))
    (caar (sqlite-select (monk-history-db) (concat "SELECT COUNT(*) FROM runs" where) params))))

(defun monk-history-delete-before (when)
  "Delete runs started before WHEN and their results.  Return the count.
WHEN as :since in `monk-history-runs'."
  (let ((db (monk-history-db))
        (stamp (monk-history-since-time when)))
    (sqlite-execute db "DELETE FROM results WHERE run_id IN (SELECT id FROM runs WHERE started < ?)"
                    (list stamp))
    (sqlite-execute db "DELETE FROM runs WHERE started < ?" (list stamp))))

;;;; Rebuilding a run from history

(defun monk-history-run (id)
  "Return the run ID from history as a `monk-run', or nil.
Machines are stand-ins named from the row; facts are empty."
  (let ((row (car (monk-history-runs :run id))))
    (when row
      (let* ((results (mapcar #'monk-history-result-to-result (monk-history-results :run id)))
             (hosts (split-string (alist-get 'hosts row) " " t)))
        ;; The stored start time has no sub-second part; the run id
        ;; does.  Take the start time back from the id so the id of
        ;; the rebuilt run equals the stored one.
        (monk-run-create
         :machines (mapcar (lambda (h) (monk-machine :name h)) hosts)
         :results results
         :passes (alist-get 'passes row)
         :dry-run (eq 1 (alist-get 'dry_run row))
         :start-time (monk-run-id-time id)
         :end-time (and (alist-get 'ended row)
                        (parse-iso8601-time-string (alist-get 'ended row))))))))

(defun monk-history-result-to-result (row)
  "Return a `monk-result' from history ROW."
  (monk-result-create
   :machine (alist-get 'host row)
   :name (alist-get 'name row)
   :type (alist-get 'type row)
   :status (intern (alist-get 'status row))
   :degree (or (alist-get 'degree row) 0.0)
   :changes (and (alist-get 'changes row)
                 (condition-case nil
                     (monk-event-alist-to-plist
                      (json-parse-string (alist-get 'changes row) :object-type 'alist
                                         :null-object nil :false-object :false))
                   (error nil)))
   :comment (alist-get 'comment row)
   :diff (alist-get 'diff row)
   :duration (/ (or (alist-get 'duration_ms row) 0) 1000.0)
   :origin (and (alist-get 'source row)
                (cons (alist-get 'source row) (alist-get 'name row)))))

;;;; JSON for the agent socket

(defun monk-history-filters-from-json (request)
  "Return the filters plist that REQUEST, an alist from JSON, asks for.
Keys use underscores in JSON: host, name, type, status, run, since,
until, dry_run, changed, limit, offset."
  (let (filters)
    (dolist (pair '((host . :host) (name . :name) (type . :type) (status . :status)
                    (run . :run) (since . :since) (until . :until)
                    (dry_run . :dry-run) (changed . :changed)
                    (limit . :limit) (offset . :offset)))
      (let ((cell (assq (car pair) request)))
        (when cell
          (setq filters (plist-put filters (cdr pair)
                                   (pcase (cdr cell) (:false nil) (v v)))))))
    filters))

(defun monk-history-rows-json (rows)
  "Return ROWS, alists from a query, as a vector `json-serialize' accepts."
  (vconcat (mapcar (lambda (row)
                     (mapcar (lambda (cell)
                               (cons (car cell) (monk-event--jsonable (cdr cell))))
                             row))
                   rows)))

(monk-history-mode 1)

(provide 'monk-history)
;;; monk-history.el ends here
