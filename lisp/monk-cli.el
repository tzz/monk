;;; monk-cli.el --- Terminal progress for batch Monk  -*- lexical-binding: t; -*-

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

;; What `emacs --batch' shows on a terminal while a run is in progress.
;; `monk-ui.el' is for a live Emacs; this is for the agent and the
;; batch entry points, where output is a byte stream.
;;
;; When standard error is a terminal, one status line at the bottom is
;; rewritten in place: a spinner, the pass, how far through the nodes,
;; and the machine, type, and name of the resource in progress.  Every
;; result that changed or failed is printed above it as a permanent
;; line the moment it finishes.  Kept and skipped results only move the
;; counter.  At run end the status line is replaced by the summary.
;;
;;   ⠹ pass 1  7/11  kamen     rsync        home files
;;
;; Columns are sized once per run from the longest machine name and
;; type name in the run, so the line does not jump between resources.
;; When standard error is not a terminal (a pipe, a log file, launchd),
;; nothing is rewritten: the permanent lines and the summary print as
;; before, and no spinner bytes reach the log.
;;
;; Color is on when the terminal claims to support it; `monk-cli-color'
;; overrides.  `NO_COLOR' in the environment turns it off, as the
;; convention says.

;;; Code:

(require 'cl-lib)
(require 'monk)

(defgroup monk-cli nil
  "Terminal output of batch Monk."
  :group 'monk
  :prefix "monk-cli-")

(defcustom monk-cli-color 'auto
  "Whether to color terminal output: `auto', t, or nil."
  :type '(choice (const auto) (const t) (const nil)))

(defcustom monk-cli-spinner-interval 0.1
  "Seconds between spinner frames while a resource runs."
  :type 'number)

(defconst monk-cli-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames for a UTF-8 terminal: the 2x3 braille roll.")

(defcustom monk-cli-idle-interval 0.25
  "Seconds between frames of the watching line, shown between runs.
Slower than the run spinner: nothing is happening, it only says the
agent is alive.  Nil turns the idle line off."
  :type '(choice (const nil) number))

(defconst monk-cli-ascii-spinner-frames ["|" "/" "-" "\\"]
  "Spinner frames for a terminal without UTF-8.")

(defconst monk-cli-status-glyphs
  `((,monk-status-kept . "✓") (,monk-status-repaired . "●") (,monk-status-would-repair . "◐")
    (,monk-status-partial . "◔") (,monk-status-failed . "✗") (,monk-status-skipped . "–")
    (,monk-status-unreachable . "⊘"))
  "One glyph per status, as in `monk-ui'.")

(defconst monk-cli-status-ascii
  `((,monk-status-kept . "ok") (,monk-status-repaired . "**") (,monk-status-would-repair . "~~")
    (,monk-status-partial . "~!") (,monk-status-failed . "!!") (,monk-status-skipped . "--")
    (,monk-status-unreachable . "xx"))
  "ASCII stand-ins for the glyphs.")

(defconst monk-cli-status-colors
  `((,monk-status-kept . "32") (,monk-status-repaired . "34") (,monk-status-would-repair . "35")
    (,monk-status-partial . "33") (,monk-status-failed . "31") (,monk-status-skipped . "90")
    (,monk-status-unreachable . "35"))
  "SGR color per status: green, blue, magenta, yellow, red, grey.")

(defconst monk-cli-max-name-width 60
  "Longest resource name shown before the middle is elided.")

(defconst monk-cli-min-machine-width 6
  "Machine column never narrower than this.")

;;;; Terminal facts

(defvar monk-cli--tty nil
  "Non-nil when standard error is a terminal.  Set by `monk-cli-enable'.")

(defvar monk-cli--utf8 nil
  "Non-nil when the terminal takes UTF-8.")

(defun monk-cli-stderr-tty-p ()
  "Return non-nil when standard error of this Emacs is a terminal.
Emacs has no `isatty', and a subprocess started by `call-process'
does not inherit our stderr in batch, so `test -t 2' cannot answer.
What can: the file that /dev/fd/2 resolves to is a character device
on a terminal (`c' in its mode string) and a pipe or a regular file
otherwise.  The device name is not used, because macOS spells it
/dev/ttysNNN and Linux /dev/pts/N, and macOS makes /dev/stderr a
link to /dev/fd/2.  In a live Emacs this is always nil."
  (and noninteractive
       (let ((modes (condition-case nil
                        (file-attribute-modes
                         (file-attributes
                          (file-truename (if (file-exists-p "/dev/fd/2") "/dev/fd/2" "/dev/stderr"))))
                      (error nil))))
         (and (stringp modes) (eq (aref modes 0) ?c)))))

(defun monk-cli-utf8-p ()
  "Return non-nil when the locale says UTF-8."
  (let ((lang (or (getenv "LC_ALL") (getenv "LC_CTYPE") (getenv "LANG") "")))
    (and (string-match-p "[Uu][Tt][Ff]-?8" lang) t)))

(defun monk-cli-color-p ()
  "Return non-nil when output may carry SGR sequences."
  (pcase monk-cli-color
    ('nil nil)
    ('t t)
    (_ (and monk-cli--tty
            (null (getenv "NO_COLOR"))
            (let ((term (or (getenv "TERM") "")))
              (and (not (equal term "dumb")) (not (string-empty-p term))))))))

(defvar monk-cli--columns nil
  "Terminal width found at enable time, or nil.")

(defun monk-cli-columns ()
  "Return the terminal width: $COLUMNS, else `stty size' on the tty, else 80.
Asked once per `monk-cli-enable'; the shell keeps $COLUMNS current
only for its own children, so a resize during a long run is missed."
  (or monk-cli--columns
      (setq monk-cli--columns
            (or (let ((c (getenv "COLUMNS")))
                  (and c (> (string-to-number c) 0) (string-to-number c)))
                (and monk-cli--tty
                     (let ((out (with-output-to-string
                                  (with-current-buffer standard-output
                                    (call-process "sh" nil t nil "-c"
                                                  "stty size 2>/dev/null </dev/tty")))))
                       (and (string-match "[0-9]+ \\([0-9]+\\)" out)
                            (let ((n (string-to-number (match-string 1 out))))
                              (and (> n 0) n)))))
                80))))

;;;; Formatting

(defun monk-cli--color (code text)
  "Wrap TEXT in SGR CODE when color is on."
  (if (and code (monk-cli-color-p))
      (format "\e[%sm%s\e[0m" code text)
    text))

(defun monk-cli-glyph (status)
  "Return the glyph for STATUS, colored when possible."
  (monk-cli--color (alist-get status monk-cli-status-colors)
                   (or (alist-get status (if monk-cli--utf8 monk-cli-status-glyphs monk-cli-status-ascii))
                       (format "%s" status))))

(defun monk-cli-elide (text width)
  "Return TEXT at most WIDTH wide, with the middle replaced by an ellipsis."
  (let ((w (string-width text)))
    (if (<= w width)
        text
      (let* ((mark (if monk-cli--utf8 "…" "..."))
             (keep (- width (string-width mark)))
             (head (/ (1+ keep) 2))
             (tail (- keep head)))
        (concat (truncate-string-to-width text head)
                mark
                (substring text (- (length text) tail)))))))

(defun monk-cli-pad (text width)
  "Return TEXT padded with spaces to WIDTH display columns."
  (let ((w (string-width text)))
    (if (>= w width) text (concat text (make-string (- width w) ?\s)))))

;;;; State of the run in progress

(defvar monk-cli--machine-width monk-cli-min-machine-width
  "Width of the machine column for the current run.")

(defvar monk-cli--type-width 8
  "Width of the type column for the current run.")

(defvar monk-cli--total 0
  "Nodes in the current run.")

(defvar monk-cli--done 0
  "Nodes finished in the current pass.")

(defvar monk-cli--pass 1
  "Current pass number.")

(defvar monk-cli--frame 0
  "Spinner frame index.")

(defvar monk-cli--current nil
  "The node in progress, or nil.")

(defvar monk-cli--timer nil
  "Timer that advances the spinner between progress events.")

(defvar monk-cli--idle-timer nil
  "Timer that animates the watching line between runs.")

(defvar monk-cli--idle-text-function nil
  "Function that returns the text after the spinner on the watching line.")

(defvar monk-cli--dry-run nil
  "Non-nil while the run in progress is a dry run; the status line marks it.")

(defvar monk-cli--status-shown nil
  "Non-nil when a status line is on the terminal and must be erased first.")

(defun monk-cli--write (text)
  "Write TEXT to the terminal, bypassing `message' and its newline."
  (send-string-to-terminal text))

(defun monk-cli--erase-status ()
  "Clear the status line, when one is shown."
  (when monk-cli--status-shown
    (monk-cli--write "\r\e[2K")
    (setq monk-cli--status-shown nil)))

(defun monk-cli--status-text ()
  "Return the status line for the node in progress."
  (let* ((spinner (monk-cli-spinner-frame))
         (width (length (number-to-string monk-cli--total)))
         (counter (format (format "%%spass %%d  %%%dd/%%d" width)
                          (if monk-cli--dry-run (concat (monk-cli-plan-mark) " ") "")
                          monk-cli--pass monk-cli--done monk-cli--total))
         (node monk-cli--current)
         (fixed (+ 2 (string-width counter) 2 monk-cli--machine-width 2 monk-cli--type-width 2))
         (name-width (max 10 (min monk-cli-max-name-width (- (monk-cli-columns) fixed 1)))))
    (if (null node)
        (format "%s %s" spinner counter)
      (format "%s %s  %s  %s  %s"
              spinner counter
              (monk-cli-pad (oref (monk-node-machine node) name) monk-cli--machine-width)
              (monk-cli--color "90" (monk-cli-pad (monk-resource-type (monk-node-resource node))
                                                  monk-cli--type-width))
              (monk-cli-elide (oref (monk-node-resource node) name) name-width)))))

(defun monk-cli--draw-status ()
  "Rewrite the status line in place."
  (when monk-cli--tty
    (monk-cli--write (concat "\r\e[2K" (monk-cli--status-text)))
    (setq monk-cli--status-shown t)))

(defun monk-cli--tick ()
  "Advance the spinner and redraw."
  (cl-incf monk-cli--frame)
  (monk-cli--draw-status))

(defconst monk-cli-plan-glyph "◌"
  "Marks a dry run on a UTF-8 terminal: a dotted circle, nothing filled in.
Next to the result glyphs it reads as the outline of a run that did
not touch anything.  Purple, like would-repair.")

(defconst monk-cli-plan-ascii "PLAN"
  "Marks a dry run on a terminal without UTF-8.")

(defun monk-cli-plan-mark ()
  "Return the dry-run mark, colored, for the terminal in use."
  (monk-cli--color "35" (if monk-cli--utf8 monk-cli-plan-glyph monk-cli-plan-ascii)))

(defun monk-cli-spinner-frame ()
  "Return the current spinner frame, colored, for the terminal in use."
  (let ((frames (if monk-cli--utf8 monk-cli-spinner-frames monk-cli-ascii-spinner-frames)))
    (monk-cli--color "36" (aref frames (% monk-cli--frame (length frames))))))

;;;; The watching line
;;
;; Between runs the agent sits in `accept-process-output'.  On a tty
;; the last line shows the same braille roll, slower, with what the
;; watch is doing, rewritten in place like the run status line, so a
;; glance at the terminal says the agent is alive and since when.
;; Permanent lines go above it as before.

(defun monk-cli--draw-idle ()
  "Rewrite the watching line in place."
  (when (and monk-cli--tty (not monk-cli--current))
    (cl-incf monk-cli--frame)
    (monk-cli--write (concat "\r\e[2K" (monk-cli-spinner-frame) " "
                             (monk-cli--color "90" (or (and monk-cli--idle-text-function
                                                            (funcall monk-cli--idle-text-function))
                                                       "watching"))))
    (setq monk-cli--status-shown t)))

(defun monk-cli-idle-start (text-function)
  "Show the watching line, with TEXT-FUNCTION giving its text, until a run starts.
Does nothing off a tty or with `monk-cli-idle-interval' nil."
  (monk-cli-idle-stop)
  (setq monk-cli--idle-text-function text-function)
  (when (and monk-cli--tty monk-cli-idle-interval)
    (setq monk-cli--idle-timer
          (run-at-time 0 monk-cli-idle-interval #'monk-cli--draw-idle))))

(defun monk-cli-idle-stop ()
  "Take the watching line down."
  (when monk-cli--idle-timer
    (cancel-timer monk-cli--idle-timer)
    (setq monk-cli--idle-timer nil))
  (monk-cli--erase-status))

(defun monk-cli-print-result (result)
  "Print RESULT as one permanent line above the status line."
  (monk-cli--erase-status)
  (let* ((status (monk-result-status result))
         (name (monk-cli-elide (monk-result-name result) monk-cli-max-name-width))
         (comment (monk-result-comment result))
         (line (format "%s %s  %s  %s%s"
                       (monk-cli-glyph status)
                       (monk-cli-pad (monk-result-machine result) monk-cli--machine-width)
                       (monk-cli--color "90" (monk-cli-pad (monk-result-type result) monk-cli--type-width))
                       name
                       (if comment (monk-cli--color "90" (concat "  " comment)) ""))))
    (if monk-cli--tty
        (monk-cli--write (concat line "\n"))
      (message "%s" line))))

(defun monk-cli-print-line (text)
  "Print TEXT as a permanent line, keeping the status line below it."
  (monk-cli--erase-status)
  (if monk-cli--tty
      (monk-cli--write (concat text "\n"))
    (message "%s" text))
  (cond (monk-cli--current (monk-cli--draw-status))
        (monk-cli--idle-timer (monk-cli--draw-idle))))

;;;; Progress hook

(defun monk-cli--size-columns (nodes)
  "Set the machine and type widths from NODES, once per run."
  (setq monk-cli--machine-width
        (max monk-cli-min-machine-width
             (apply #'max 0 (mapcar (lambda (n) (string-width (oref (monk-node-machine n) name))) nodes)))
        monk-cli--type-width
        (max 6 (apply #'max 0 (mapcar (lambda (n) (string-width (monk-resource-type (monk-node-resource n))))
                                      nodes)))))

(defun monk-cli--progress (event data)
  "Handle progress EVENT with DATA on the terminal."
  (pcase event
    ('run-start
     (let ((nodes (plist-get data :nodes)))
       (when monk-cli--idle-timer
         (cancel-timer monk-cli--idle-timer)
         (setq monk-cli--idle-timer nil)
         (monk-cli--erase-status))
       (monk-cli--size-columns nodes)
       (setq monk-cli--total (length nodes)
             monk-cli--done 0
             monk-cli--pass 1
             monk-cli--frame 0
             monk-cli--current nil
             monk-cli--dry-run (and (plist-get data :run) (monk-run-dry-run (plist-get data :run))))
       (when monk-cli--tty
         (setq monk-cli--timer
               (run-at-time monk-cli-spinner-interval monk-cli-spinner-interval #'monk-cli--tick)))))
    ('node-start
     (setq monk-cli--current (plist-get data :node))
     (monk-cli--draw-status))
    ('node-end
     (cl-incf monk-cli--done)
     (setq monk-cli--current nil)
     (let ((result (plist-get data :result)))
       (when (or (monk-result-changed-p result) (monk-result-failed-p result)
                 (eq (monk-result-status result) monk-status-unreachable))
         (monk-cli-print-result result)))
     (monk-cli--draw-status))
    ('pass-end
     (setq monk-cli--pass (1+ (plist-get data :pass))
           monk-cli--done 0))
    ('run-end
     (when monk-cli--timer
       (cancel-timer monk-cli--timer)
       (setq monk-cli--timer nil))
     (monk-cli--erase-status)
     (setq monk-cli--current nil)
     ;; back to the watching line; the summary line the caller prints
     ;; next goes above it, as `monk-cli-print-line' redraws it
     (when (and monk-cli--idle-text-function monk-cli-idle-interval monk-cli--tty)
       (setq monk-cli--idle-timer
             (run-at-time monk-cli-idle-interval monk-cli-idle-interval #'monk-cli--draw-idle))))))

(defun monk-cli-summary-line (run)
  "Return the one-line summary of RUN with colored counts.
A partial run, one a watch or :only restricted, says how many
resources it left out instead of counting them as skipped: those are
not a verdict on anything."
  (let* ((parts nil)
         (unselected (monk-run-unselected-count run)))
    ;; a dry run says so first: on a clean host its counts are all
    ;; kept and read exactly like an apply otherwise
    (when (monk-run-dry-run run)
      (push (concat (monk-cli-plan-mark) " dry run") parts))
    (dolist (status (list monk-status-repaired monk-status-would-repair monk-status-partial
                          monk-status-failed monk-status-unreachable monk-status-kept monk-status-skipped))
      (let ((n (- (monk-run-count run status) (if (eq status monk-status-skipped) unselected 0))))
        (when (> n 0)
          (push (format "%s %d %s" (monk-cli-glyph status) n status) parts))))
    (concat (string-join (nreverse parts) "  ")
            (if (> unselected 0)
                (monk-cli--color "90" (format "  partial: %d of %d" (- (length (monk-run-results run)) unselected)
                                              (length (monk-run-results run))))
              "")
            (monk-cli--color "90" (format "  %d pass%s" (monk-run-passes run)
                                          (if (= 1 (monk-run-passes run)) "" "es")))
            (if (monk-run-flapping-p run)
                (monk-cli--color "33" (format "  flapping: %s"
                                              (mapconcat #'cdr (monk-run-flapping run) ", ")))
              ""))))

;;;; Enable

(defun monk-cli-enable ()
  "Turn on terminal progress for this batch Emacs.
Detects the terminal once.  Safe to call more than once."
  (setq monk-cli--tty (monk-cli-stderr-tty-p)
        monk-cli--utf8 (monk-cli-utf8-p)
        monk-cli--columns nil
        monk-batch-print-function #'monk-cli-print-line)
  (add-hook 'monk-progress-functions #'monk-cli--progress)
  monk-cli--tty)

(defun monk-cli-disable ()
  "Turn terminal progress off."
  (remove-hook 'monk-progress-functions #'monk-cli--progress)
  (monk-cli-idle-stop)
  (setq monk-cli--idle-text-function nil)
  (when monk-cli--timer (cancel-timer monk-cli--timer))
  (setq monk-cli--timer nil monk-cli--tty nil
        monk-batch-print-function (lambda (line) (message "%s" line))))

(monk-note-load 'monk-cli)
(provide 'monk-cli)
;;; monk-cli.el ends here
