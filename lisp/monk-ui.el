;;; monk-ui.el --- Graphical report and progress for Monk  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, unix, multimedia

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

;; Visuals for Monk runs.
;;
;; - Status badges: a glyph and a word per status, with a face.  On a
;;   TTY the glyphs fall back to ASCII.
;; - Stacked SVG bars: one lozenge per machine with layers for kept,
;;   repaired, partial, and failed.  Text fallback with block glyphs.
;; - A spinner for nodes that are running.
;; - `monk-ui-mode': a `tabulated-list-mode' buffer with one row per
;;   machine that updates while the run is in progress, and a detail
;;   buffer per machine with diffs.
;; - A header line with the run summary bar.
;;
;; Loading this file sets `monk-report-function' and adds a progress
;; display to `monk-progress-functions'.  All drawing degrades to text
;; when SVG is not available, and to nothing in batch mode.
;;
;; The bar and spinner drawing derive from the author's nngmail and
;; efrit-agent-spinner code.

;;; Code:

(require 'monk)
(require 'svg)
(require 'color)
(require 'tabulated-list)

(defgroup monk-ui nil
  "Graphical report for Monk."
  :group 'monk
  :prefix "monk-ui-")

;;;; Constants

(defconst monk-ui-buffer-name "*monk*"
  "The report buffer.")

(defconst monk-ui-detail-buffer-format "*monk: %s*"
  "Format for per-machine detail buffers.")

(defconst monk-ui-bar-width 24
  "Width of a progress bar in character cells.")

(defconst monk-ui-spinner-steps 12
  "Frames in one spinner revolution.")

(defconst monk-ui-spinner-interval 0.1
  "Seconds between spinner frames.")

(defconst monk-ui-text-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille frames for the text spinner.")

(defconst monk-ui-ascii-spinner-frames ["|" "/" "-" "\\"]
  "ASCII frames for the text spinner.")

(defconst monk-ui-font-scale 0.62
  "Height of bar labels as a fraction of the line height.")

(defconst monk-ui-glyph-advance 0.62
  "Advance of one monospace glyph as a fraction of its font size.")

(defconst monk-ui-min-bar-height 8
  "Smallest bar height in pixels.")

(defconst monk-ui-status-order
  (list monk-status-failed monk-status-partial monk-status-repaired
        monk-status-would-repair monk-status-kept monk-status-skipped)
  "Bar layer order, bottom to top, and sort order for detail buffers.")

;;;; Customization

(defcustom monk-ui-style 'auto
  "How to draw bars and spinners.
`auto' uses SVG when the display supports it.  `svg' forces SVG.
`text' forces block glyphs.  nil draws no bars."
  :type '(choice (const auto) (const svg) (const text) (const nil)))

(defcustom monk-ui-unicode t
  "When non-nil, status glyphs use Unicode.  Nil uses ASCII."
  :type 'boolean)

;;;; Faces

(defface monk-ui-kept
  '((((background dark)) :foreground "#3fb950")
    (t :foreground "#1a7f37"))
  "Resource already held.")

(defface monk-ui-repaired
  '((((background dark)) :foreground "#58a6ff")
    (t :foreground "#0969da"))
  "Resource was changed.")

(defface monk-ui-would-repair
  '((((background dark)) :foreground "#a371f7")
    (t :foreground "#8250df"))
  "Resource would change in a dry run.")

(defface monk-ui-partial
  '((((background dark)) :foreground "#d29922")
    (t :foreground "#9a6700"))
  "Resource holds in part.")

(defface monk-ui-failed
  '((((background dark)) :foreground "#f85149")
    (t :foreground "#cf222e"))
  "Resource failed.")

(defface monk-ui-skipped
  '((((background dark)) :foreground "#6e7681")
    (t :foreground "#8c959f"))
  "Resource was skipped.")

(defface monk-ui-running
  '((((background dark)) :foreground "#88c0d0")
    (t :foreground "#5e81ac"))
  "Resource in progress.  Also the spinner color.")

(defface monk-ui-pending
  '((((background dark)) :foreground "#484f58")
    (t :foreground "#afb8c1"))
  "Resource not yet run.")

(defface monk-ui-track
  '((((background dark)) :background "#30363d" :foreground "#30363d")
    (t :background "#d0d7de" :foreground "#d0d7de"))
  "Empty part of a bar.")

(defface monk-ui-bar-label
  '((((background dark)) :foreground "#f0f6fc" :background "#0d1117")
    (t :foreground "#1f2328" :background "#ffffff"))
  "Label inside a bar.  Background is the halo color.")

(defface monk-ui-machine
  '((t :inherit bold))
  "Machine name.")

(defface monk-ui-header
  '((((background dark)) :background "#161b22" :foreground "#c9d1d9" :extend t)
    (t :background "#f6f8fa" :foreground "#24292f" :extend t))
  "Header line of the report.")

(defface monk-ui-separator
  '((((background dark)) :overline "#30363d" :height 0.4 :extend t)
    (t :overline "#d0d7de" :height 0.4 :extend t))
  "Hairline between sections.")

(defconst monk-ui-status-faces
  `((,monk-status-kept . monk-ui-kept)
    (,monk-status-repaired . monk-ui-repaired)
    (,monk-status-would-repair . monk-ui-would-repair)
    (,monk-status-partial . monk-ui-partial)
    (,monk-status-failed . monk-ui-failed)
    (,monk-status-skipped . monk-ui-skipped)
    (,monk-status-running . monk-ui-running)
    (,monk-status-pending . monk-ui-pending))
  "Face for each status.")

(defconst monk-ui-status-glyphs
  `((,monk-status-kept "✓" "=")
    (,monk-status-repaired "●" "*")
    (,monk-status-would-repair "◐" "~")
    (,monk-status-partial "◒" "%")
    (,monk-status-failed "✗" "!")
    (,monk-status-skipped "–" "-")
    (,monk-status-running "▶" ">")
    (,monk-status-pending "·" "."))
  "Unicode and ASCII glyph for each status.")

(defun monk-ui-face (status)
  "Return the face for STATUS."
  (or (alist-get status monk-ui-status-faces) 'default))

(defun monk-ui-glyph (status)
  "Return the glyph string for STATUS, Unicode or ASCII."
  (let ((entry (alist-get status monk-ui-status-glyphs)))
    (if (and monk-ui-unicode (char-displayable-p (string-to-char (car entry))))
        (car entry)
      (cadr entry))))

(defun monk-ui-badge (status &optional text)
  "Return a badge for STATUS: glyph, space, then TEXT or the status name."
  (propertize (concat (monk-ui-glyph status) " " (or text (symbol-name status)))
              'face (monk-ui-face status)
              'help-echo (symbol-name status)))

;;;; Color helpers

(defun monk-ui--hex (face &optional attribute)
  "Return ATTRIBUTE of FACE as #rrggbb, resolving inheritance.
ATTRIBUTE defaults to :foreground.  Fall back to the default face,
then to mid gray."
  (let* ((attr (or attribute :foreground))
         (name (let ((v (face-attribute face attr nil t)))
                 (if (stringp v) v (face-attribute 'default attr nil t))))
         (rgb (and (stringp name) (color-name-to-rgb name))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) "#808080")))

(defun monk-ui--parse-hex (color)
  "Return COLOR, a #rrggbb string, as three floats from 0 to 1."
  (if (and (stringp color)
           (string-match "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'"
                         color))
      (mapcar (lambda (i) (/ (string-to-number (match-string i color) 16) 255.0)) '(1 2 3))
    '(0.5 0.5 0.5)))

(defun monk-ui--blend (color bg alpha)
  "Return COLOR over BG at ALPHA as #rrggbb.
Opacity is baked into the color because only librsvg honors
stroke-opacity."
  (let ((c (monk-ui--parse-hex color))
        (b (monk-ui--parse-hex bg)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (* 255 (+ (* alpha x) (* (- 1 alpha) y))))) c b))))

;;;; Capability

(defun monk-ui-svg-p ()
  "Return non-nil when SVG drawing is possible and allowed."
  (and (not noninteractive)
       (display-graphic-p)
       (image-type-available-p 'svg)
       (memq monk-ui-style '(auto svg))))

(defun monk-ui--latch-text (err)
  "Log ERR and switch `monk-ui-style' to text for the rest of the session."
  (monk-log 'warn "SVG drawing failed (%s); using text" (error-message-string err))
  (setq monk-ui-style 'text))

;;;; Bars

(defvar monk-ui--bar-cache (make-hash-table :test #'equal)
  "Cache of rendered bars keyed by their drawing parameters.")

(defun monk-ui-reset-cache ()
  "Drop cached images, for example after a theme or font change."
  (interactive)
  (clrhash monk-ui--bar-cache))

(defun monk-ui--fit-label (label max-cols)
  "Shorten LABEL to MAX-COLS columns by dropping trailing fields.
Fields are separated by two spaces."
  (let ((parts (split-string label "  " t)))
    (while (and (cdr parts)
                (> (string-width (mapconcat #'identity parts "  ")) max-cols))
      (setq parts (butlast parts)))
    (truncate-string-to-width (mapconcat #'identity parts "  ") max-cols nil nil "…")))

(defun monk-ui--bar-svg (width layers label tooltip)
  "Return a propertized string with one SVG lozenge WIDTH cells wide.
LAYERS is a list of (FRACTION . COLOR) drawn in order, each from the
left edge, so put the widest first.  LABEL is centered inside.
TOOLTIP is the help-echo."
  (let* ((ch (frame-char-width))
         (lh (frame-char-height))
         (px (* width ch))
         (h (max monk-ui-min-bar-height (- lh 2)))
         (top (/ (- lh h) 2.0))
         (r (/ h 2.0))
         (svg (svg-create px lh)))
    (svg-rectangle svg 0 top px h :rx r :ry r :fill (monk-ui--hex 'monk-ui-track :background))
    (dolist (layer layers)
      (let ((f (max 0.0 (min 1.0 (car layer)))))
        (when (> f 0)
          (svg-rectangle svg 0 top (max h (round (* f px))) h :rx r :ry r :fill (cdr layer)))))
    (when (and label (not (string-empty-p label)))
      (let* ((font-size (* monk-ui-font-scale lh))
             (glyph (* monk-ui-glyph-advance font-size))
             (inner (- px h))
             (max-chars (max 4 (floor (/ inner glyph))))
             (label (monk-ui--fit-label label max-chars))
             (family (format "'%s', ui-monospace, Menlo, monospace"
                             (or (face-attribute 'default :family) "monospace")))
             (text-width (min inner (* glyph (string-width label))))
             (y (+ top (* h 0.5) (* font-size 0.35))))
        (dolist (pass '(halo text))
          (apply #'svg-text svg label
                 :x (/ px 2.0) :y y
                 :font-size font-size :font-family family :text-anchor "middle"
                 :textLength text-width :lengthAdjust "spacingAndGlyphs"
                 (if (eq pass 'halo)
                     (list :fill "none"
                           :stroke (monk-ui--hex 'monk-ui-bar-label :background)
                           :stroke-width 1.2 :stroke-opacity 0.6 :stroke-linejoin "round")
                   (list :fill (monk-ui--hex 'monk-ui-bar-label)))))))
    (propertize (make-string width ?\s)
                'display (svg-image svg :ascent 'center :scale 1)
                'help-echo tooltip)))

(defun monk-ui--bar-text (width layers label)
  "Return a text bar WIDTH cells wide.
LAYERS is a list of (FRACTION . FACE), each from the left edge, so
put the widest first.  LABEL follows the bar."
  (let* ((label-width (if (and label (not (string-empty-p label)))
                          (+ 1 (string-width label))
                        0))
         (cells (max 4 (- width label-width)))
         (row (make-vector cells nil)))
    (dolist (layer layers)
      (let ((filled (round (* (max 0.0 (min 1.0 (car layer))) cells))))
        (dotimes (i filled)
          (aset row i (cdr layer)))))
    (concat
     (mapconcat (lambda (face)
                  (if face
                      (propertize "█" 'face face)
                    (propertize "░" 'face 'monk-ui-track)))
                row "")
     (if (> label-width 0) (concat " " label) ""))))

(defun monk-ui-bar (counts total &optional label width)
  "Return a bar showing COUNTS out of TOTAL.
COUNTS is an alist of (STATUS . N).  Layers stack from the left in
`monk-ui-status-order' so the failures sit on the left edge.  LABEL
defaults to a done count.  WIDTH defaults to `monk-ui-bar-width'."
  (let* ((width (or width monk-ui-bar-width))
         (done (apply #'+ (mapcar #'cdr counts)))
         (label (or label (if (> total 0) (format "%d/%d" done total) "")))
         (total (max 1 total))
         ;; Cumulative fractions in status order.  `push' leaves the
         ;; last, widest layer first, which is the paint order: each
         ;; narrower layer covers the left part of the one below.
         (layers nil)
         (acc 0))
    (dolist (status monk-ui-status-order)
      (let ((n (alist-get status counts 0)))
        (when (> n 0)
          (setq acc (+ acc n))
          (push (cons (/ (float acc) total) status) layers))))
    (let ((tooltip (mapconcat (lambda (s) (format "%s %d" (car s) (cdr s))) counts ", ")))
      (pcase (if (eq monk-ui-style 'auto) (if (monk-ui-svg-p) 'svg 'text) monk-ui-style)
        ('nil label)
        ('svg
         (let ((key (list 'bar width layers label (frame-char-width) (frame-char-height)
                          (frame-parameter nil 'background-mode))))
           (or (gethash key monk-ui--bar-cache)
               (condition-case err
                   (puthash key
                            (monk-ui--bar-svg
                             width
                             (mapcar (lambda (l) (cons (car l) (monk-ui--hex (monk-ui-face (cdr l)))))
                                     layers)
                             label tooltip)
                            monk-ui--bar-cache)
                 (error
                  (monk-ui--latch-text err)
                  (monk-ui-bar counts total label width))))))
        (_
         (propertize (monk-ui--bar-text width
                                        (mapcar (lambda (l) (cons (car l) (monk-ui-face (cdr l))))
                                                layers)
                                        label)
                     'help-echo tooltip))))))

;;;; Spinner

(defvar monk-ui--spinner-index 0
  "Current spinner frame, shared by all displays.")

(defvar monk-ui--spinner-timer nil
  "Timer that advances the spinner.")

(defun monk-ui--arc (svg cx cy r start sweep stroke width)
  "Draw on SVG an arc of radius R around CX,CY from angle START over SWEEP degrees."
  (let* ((a0 (* float-pi (/ start 180.0)))
         (a1 (* float-pi (/ (+ start sweep) 180.0)))
         (x0 (+ cx (* r (cos a0)))) (y0 (+ cy (* r (sin a0))))
         (x1 (+ cx (* r (cos a1)))) (y1 (+ cy (* r (sin a1))))
         (large (if (> sweep 180) 1 0)))
    (svg-node svg 'path
              :d (format "M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f" x0 y0 r r large x1 y1)
              :fill "none" :stroke stroke :stroke-width width
              :stroke-linecap "round")))

(defun monk-ui--spinner-svg (size color index bg)
  "Return spinner frame INDEX as an SVG, SIZE pixels square, in COLOR over BG."
  (let* ((svg (svg-create size size))
         (c (/ size 2.0))
         (w (max 1.2 (/ size 10.0)))
         (r (- c w 1))
         (angle (* index (/ 360.0 monk-ui-spinner-steps))))
    (svg-circle svg c c r :fill "none" :stroke-width w
                :stroke (monk-ui--blend color bg 0.15))
    (monk-ui--arc svg c c r angle 200 (monk-ui--blend color bg 0.4) w)
    (monk-ui--arc svg c c r (+ angle 200) 70 color w)
    svg))

(defun monk-ui-spinner (&optional index)
  "Return a one-cell string showing spinner frame INDEX.
SVG when possible, else a Braille or ASCII glyph."
  (let ((index (mod (or index monk-ui--spinner-index) monk-ui-spinner-steps)))
    (if (monk-ui-svg-p)
        (let* ((size (- (frame-char-height) 2))
               (color (monk-ui--hex 'monk-ui-running))
               (bg (monk-ui--hex 'default :background))
               (key (list 'spin size color bg index)))
          (or (gethash key monk-ui--bar-cache)
              (condition-case err
                  (puthash key
                           (propertize " "
                                       'display (svg-image (monk-ui--spinner-svg size color index bg)
                                                           :ascent 'center :scale 1))
                           monk-ui--bar-cache)
                (error (monk-ui--latch-text err) (monk-ui-spinner index)))))
      (let ((frames (if (and monk-ui-unicode (char-displayable-p ?⠋))
                        monk-ui-text-spinner-frames
                      monk-ui-ascii-spinner-frames)))
        (propertize (aref frames (mod index (length frames))) 'face 'monk-ui-running)))))

(defun monk-ui--spinner-tick ()
  "Advance the spinner and redraw live report buffers."
  (setq monk-ui--spinner-index (1+ monk-ui--spinner-index))
  (condition-case err
      (monk-ui--refresh-live)
    (error
     (monk-log 'warn "spinner tick failed: %s" (error-message-string err))
     (monk-ui--spinner-stop))))

(defun monk-ui--spinner-start ()
  "Start the spinner timer if it is not running."
  (unless (or noninteractive monk-ui--spinner-timer)
    (setq monk-ui--spinner-timer
          (run-at-time 0 monk-ui-spinner-interval #'monk-ui--spinner-tick))))

(defun monk-ui--spinner-stop ()
  "Stop the spinner timer."
  (when monk-ui--spinner-timer
    (cancel-timer monk-ui--spinner-timer)
    (setq monk-ui--spinner-timer nil)))

;;;; Run model for the display

(defvar monk-ui--live-run nil
  "The run in progress, or nil.")

(defvar monk-ui--live-nodes nil
  "Nodes of the run in progress, in schedule order.")

(defvar monk-ui--live-current nil
  "The node being checked or repaired, or nil.")

(defvar monk-ui--live-pass 0
  "Pass number of the run in progress.")

(defvar-local monk-ui--run nil
  "The run this report buffer shows.")

(defun monk-ui--node-result (node)
  "Return the result of NODE in the live run.
Pass two clears node results before it starts, so fall back to the
merged results of the run to keep bars from dropping to zero."
  (or (monk-node-result node)
      (and monk-ui--live-run
           (seq-find (lambda (r) (equal (monk-result-key r) (monk-node-key node)))
                     (monk-run-results monk-ui--live-run)))))

(defun monk-ui--node-status (node)
  "Return the display status of NODE during a live run."
  (cond
   ((eq node monk-ui--live-current) monk-status-running)
   ((monk-ui--node-result node) (monk-result-status (monk-ui--node-result node)))
   (t monk-status-pending)))

(defun monk-ui--results (&optional machine-name)
  "Return the results of the live or finished run, for MACHINE-NAME when given.
During a live run, node results are current; the run holds only the
merged results of finished passes."
  (let ((results (if monk-ui--live-nodes
                     (delq nil (mapcar #'monk-ui--node-result monk-ui--live-nodes))
                   (let ((run (or monk-ui--live-run monk-ui--run)))
                     (and run (monk-run-results run))))))
    (if machine-name
        (seq-filter (lambda (r) (equal (monk-result-machine r) machine-name)) results)
      results)))

(defun monk-ui--machine-counts (machine-name)
  "Return an alist of (STATUS . N) for MACHINE-NAME in the live or finished run."
  (let ((counts nil))
    (if monk-ui--live-nodes
        (dolist (node monk-ui--live-nodes)
          (when (equal (oref (monk-node-machine node) name) machine-name)
            (let ((status (monk-ui--node-status node)))
              (unless (memq status (list monk-status-pending monk-status-running))
                (cl-incf (alist-get status counts 0))))))
      (dolist (result (and monk-ui--run (monk-run-results monk-ui--run)))
        (when (equal (monk-result-machine result) machine-name)
          (cl-incf (alist-get (monk-result-status result) counts 0)))))
    counts))

(defun monk-ui--machine-total (machine-name)
  "Return the number of nodes or results for MACHINE-NAME."
  (if monk-ui--live-nodes
      (seq-count (lambda (n) (equal (oref (monk-node-machine n) name) machine-name))
                 monk-ui--live-nodes)
    (seq-count (lambda (r) (equal (monk-result-machine r) machine-name))
               (and monk-ui--run (monk-run-results monk-ui--run)))))

(defun monk-ui--machine-verdict (machine-name counts)
  "Return the badge status for MACHINE-NAME from COUNTS."
  (cond
   ((and monk-ui--live-current
         (equal (oref (monk-node-machine monk-ui--live-current) name) machine-name))
    monk-status-running)
   ((and monk-ui--live-nodes
         (< (apply #'+ (mapcar #'cdr counts)) (monk-ui--machine-total machine-name)))
    monk-status-pending)
   ((> (alist-get monk-status-failed counts 0) 0) monk-status-failed)
   ((> (alist-get monk-status-partial counts 0) 0) monk-status-partial)
   ((> (alist-get monk-status-would-repair counts 0) 0) monk-status-would-repair)
   ((> (alist-get monk-status-repaired counts 0) 0) monk-status-repaired)
   ((> (alist-get monk-status-kept counts 0) 0) monk-status-kept)
   (t monk-status-skipped)))

;;;; Report buffer

(defvar monk-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'monk-ui-show-machine)
    (define-key map (kbd "d") #'monk-ui-show-machine)
    (define-key map (kbd "g") #'monk-ui-refresh)
    (define-key map (kbd "t") #'monk-ui-toggle-style)
    map)
  "Keymap for `monk-ui-mode'.")

(define-derived-mode monk-ui-mode tabulated-list-mode "Monk"
  "Report of a Monk run, one row per machine."
  (setq tabulated-list-format
        (vector (list "Machine" 18 t)
                (list "State" 14 t)
                (list "Progress" (+ 2 monk-ui-bar-width) nil)
                (list "Kept" 5 nil :right-align t)
                (list "Changed" 8 nil :right-align t)
                (list "Failed" 7 nil :right-align t)
                (list "Skipped" 8 nil :right-align t)
                (list "Time" 6 nil :right-align t))
        tabulated-list-padding 1)
  (setq-local header-line-format '(:eval (monk-ui--header-line)))
  (add-hook 'window-configuration-change-hook #'monk-ui-reset-cache nil t)
  (tabulated-list-init-header))

(defun monk-ui--machine-names ()
  "Return the machine names of the live or finished run."
  (let ((run (or monk-ui--live-run monk-ui--run)))
    (and run (mapcar (lambda (m) (oref m name)) (monk-run-machines run)))))

(defun monk-ui--machine-duration (machine-name)
  "Return the total seconds spent on MACHINE-NAME."
  (apply #'+ (mapcar #'monk-result-duration (monk-ui--results machine-name))))

(defun monk-ui--entries ()
  "Return `tabulated-list-entries' for the current run."
  (mapcar
   (lambda (name)
     (let* ((counts (monk-ui--machine-counts name))
            (total (monk-ui--machine-total name))
            (verdict (monk-ui--machine-verdict name counts))
            (changed (+ (alist-get monk-status-repaired counts 0)
                        (alist-get monk-status-would-repair counts 0)))
            (failed (+ (alist-get monk-status-failed counts 0)
                       (alist-get monk-status-partial counts 0))))
       (list name
             (vector (propertize name 'face 'monk-ui-machine)
                     (if (eq verdict monk-status-running)
                         (concat (monk-ui-spinner) " "
                                 (propertize "running" 'face 'monk-ui-running))
                       (monk-ui-badge verdict))
                     (monk-ui-bar counts total)
                     (number-to-string (alist-get monk-status-kept counts 0))
                     (number-to-string changed)
                     (number-to-string failed)
                     (number-to-string (alist-get monk-status-skipped counts 0))
                     (format "%.1fs" (monk-ui--machine-duration name))))))
   (monk-ui--machine-names)))

(defun monk-ui--header-line ()
  "Return the header line: run summary and a total bar."
  (let* ((run (or monk-ui--live-run monk-ui--run))
         (names (monk-ui--machine-names))
         (counts nil)
         (total 0))
    (dolist (name names)
      (setq total (+ total (monk-ui--machine-total name)))
      (dolist (cell (monk-ui--machine-counts name))
        (cl-incf (alist-get (car cell) counts 0) (cdr cell))))
    (let ((done (apply #'+ (mapcar #'cdr counts))))
      (concat
       (propertize " " 'face 'monk-ui-header)
       (if monk-ui--live-run
           (concat (monk-ui-spinner) " ")
         "")
       (propertize (format "Monk %s" (cond ((null run) "")
                                            ((monk-run-dry-run run) "dry run")
                                            (t "run")))
                   'face '(monk-ui-header bold))
       "  "
       (monk-ui-bar counts total (format "%d/%d  pass %d" done total
                                         (if monk-ui--live-run
                                             monk-ui--live-pass
                                           (or (and run (monk-run-passes run)) 0)))
                    (min 40 (max 10 (- (window-width) 40))))
       "  "
       (mapconcat (lambda (status)
                    (let ((n (alist-get status counts 0)))
                      (if (> n 0) (monk-ui-badge status (number-to-string n)) "")))
                  monk-ui-status-order " ")
       (if (and run (not monk-ui--live-run) (monk-run-flapping-p run))
           (concat "  " (propertize (format "%s flapping %d"
                                            (monk-ui-glyph monk-status-partial)
                                            (length (monk-run-flapping run)))
                                    'face 'monk-ui-partial
                                    'help-echo (mapconcat
                                                (lambda (key) (format "%s:%s" (car key) (cdr key)))
                                                (monk-run-flapping run) "\n")))
         "")))))

(defun monk-ui-refresh ()
  "Redraw the report buffer."
  (interactive)
  (when (derived-mode-p 'monk-ui-mode)
    (setq tabulated-list-entries (monk-ui--entries))
    (tabulated-list-print t)))

(defun monk-ui--refresh-live ()
  "Redraw every visible report buffer during a live run.
The run executes synchronously in the main thread, so timers and
redisplay do not run on their own.  `redisplay' forces a frame update
here; without it the user sees nothing until the run returns."
  (let ((buffer (get-buffer monk-ui-buffer-name)))
    (when (and buffer (get-buffer-window buffer t))
      (with-current-buffer buffer
        (monk-ui-refresh)
        (force-mode-line-update))
      (redisplay t))))

(defun monk-ui-toggle-style ()
  "Switch between SVG and text drawing."
  (interactive)
  (setq monk-ui-style (if (eq monk-ui-style 'text) 'auto 'text))
  (monk-ui-reset-cache)
  (monk-ui-refresh)
  (message "Monk drawing: %s" monk-ui-style))

(defun monk-ui--buffer ()
  "Return the report buffer, creating it in `monk-ui-mode'."
  (let ((buffer (get-buffer-create monk-ui-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-ui-mode)
        (monk-ui-mode)))
    buffer))

;;;; Detail buffer

(defun monk-ui--insert-result (result)
  "Insert one RESULT into the detail buffer."
  (let ((status (monk-result-status result)))
    (insert (monk-ui-badge status)
            (make-string (max 1 (- 14 (length (symbol-name status)))) ?\s)
            (propertize (format "%-14s" (monk-result-type result)) 'face 'monk-ui-skipped)
            (monk-result-name result))
    (when (eq status monk-status-partial)
      (insert " " (monk-ui-bar (list (cons monk-status-kept
                                            (round (* 100 (monk-result-degree result)))))
                                100
                                (format "%d%%" (round (* 100 (monk-result-degree result))))
                                10)))
    (when (monk-result-comment result)
      (insert (propertize (concat "  " (monk-result-comment result)) 'face 'monk-ui-skipped)))
    (insert "\n")
    (when (and (monk-result-changes result) (not (monk-result-diff result)))
      (insert (propertize (format "    %S\n" (monk-result-changes result))
                          'face 'monk-ui-skipped)))
    (when (monk-result-diff result)
      (let ((start (point)))
        (insert (monk-result-diff result))
        (unless (bolp) (insert "\n"))
        (monk-ui--fontify-diff start (point))))))

(defun monk-ui--fontify-diff (start end)
  "Apply diff faces between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((face (cond ((looking-at "^\\+\\+\\+\\|^---") 'diff-header)
                        ((looking-at "^@@") 'diff-hunk-header)
                        ((looking-at "^\\+") 'diff-added)
                        ((looking-at "^-") 'diff-removed)
                        (t nil))))
        (when face
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'face face))))
      (forward-line 1))))

(defun monk-ui-show-machine (&optional machine-name)
  "Show the results for MACHINE-NAME, or the machine at point."
  (interactive)
  (let* ((name (or machine-name (tabulated-list-get-id)
                   (user-error "No machine at point")))
         (run (or monk-ui--live-run monk-ui--run))
         (results (monk-ui--results name))
         (buffer (get-buffer-create (format monk-ui-detail-buffer-format name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize (format " %s " name) 'face '(monk-ui-header monk-ui-machine))
                (propertize (format " %s\n" (if run (monk-run-summary run name) ""))
                            'face 'monk-ui-header))
        (insert (propertize "\n" 'face 'monk-ui-separator))
        (dolist (status monk-ui-status-order)
          (let ((group (seq-filter (lambda (r) (eq (monk-result-status r) status)) results)))
            (when group
              (insert "\n" (monk-ui-badge status (format "%s (%d)" status (length group))) "\n")
              (mapc #'monk-ui--insert-result group))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;;; Progress hook and report entry

(defun monk-ui--progress (event data)
  "Handle progress EVENT with DATA.  See `monk-progress-functions'."
  (unless noninteractive
    (pcase event
      ('run-start
       (setq monk-ui--live-run (plist-get data :run)
             monk-ui--live-nodes (plist-get data :nodes)
             monk-ui--live-current nil
             monk-ui--live-pass 1)
       (let ((buffer (monk-ui--buffer)))
         (with-current-buffer buffer
           (setq monk-ui--run monk-ui--live-run))
         (display-buffer buffer)
         (monk-ui-reset-cache)
         (monk-ui--refresh-live))
       (monk-ui--spinner-start))
      ('node-start
       (setq monk-ui--live-current (plist-get data :node))
       (setq monk-ui--spinner-index (1+ monk-ui--spinner-index))
       (monk-ui--refresh-live))
      ('node-end
       (setq monk-ui--live-current nil)
       (monk-ui--refresh-live))
      ('pass-end
       (setq monk-ui--live-pass (1+ (plist-get data :pass)))
       (monk-ui--refresh-live))
      ('run-end
       (monk-ui--spinner-stop)
       (setq monk-ui--live-current nil
             monk-ui--live-nodes nil
             monk-ui--live-run nil)
       (with-current-buffer (monk-ui--buffer)
         (setq monk-ui--run (plist-get data :run))
         (monk-ui-refresh)
         (force-mode-line-update))))))

(defun monk-ui-report (run &optional _verbose)
  "Show RUN in the report buffer.  In batch mode, print text instead."
  (if noninteractive
      (monk-run-report-text run)
    (let ((buffer (monk-ui--buffer)))
      (with-current-buffer buffer
        (setq monk-ui--run run)
        (monk-ui-refresh))
      (display-buffer buffer)
      (when (or (not (monk-run-ok-p run)) (not (monk-run-converged-p run)))
        (message "Monk: %s%s"
                 (monk-run-summary run)
                 (cond ((monk-run-flapping-p run)
                        (format " (flapping: %s)"
                                (mapconcat #'cdr (monk-run-flapping run) ", ")))
                       ((monk-run-oscillating-p run) " (did not converge)")
                       (t "")))))))

(add-hook 'monk-progress-functions #'monk-ui--progress)
(setq monk-report-function #'monk-ui-report)

(provide 'monk-ui)
;;; monk-ui.el ends here
