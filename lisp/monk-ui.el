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
;; - Plan and apply: a dry run is a plan, shown with its own colors and
;;   a `would' column.  `a' in a plan buffer applies it.  The apply
;;   report carries a Plan column that says whether each machine did
;;   what the plan said, and the detail buffer pairs each result with
;;   its planned counterpart.
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

(defconst monk-ui-query-buffer-name "*monk-query*"
  "Buffer that shows which machines a CEL or CELL guard matches.")

(defconst monk-ui-fold-indicator-open "▾"
  "Marker before an unfolded section heading.")

(defconst monk-ui-fold-indicator-closed "▸"
  "Marker before a folded section heading.")

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
  (list monk-status-unreachable monk-status-failed monk-status-partial monk-status-repaired
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

(defcustom monk-ui-fold-statuses (list monk-status-kept monk-status-skipped)
  "Statuses whose group starts folded in a detail buffer.
Salt's `state_output: changes': failures and changes open, the rest
one line each.  TAB on the group heading unfolds it."
  :type '(repeat symbol))

(defcustom monk-ui-select-window 'command
  "When to move point into the report buffer after a run.
`command' selects the window when the run came from an interactive
command, so `a' and `RET' work at once.  Watch-triggered runs do not
steal focus.  t always selects.  nil only displays."
  :type '(choice (const command) (const t) (const nil)))

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

(defface monk-ui-unreachable
  '((((background dark)) :foreground "#d2a8ff" :slant italic)
    (t :foreground "#8250df" :slant italic))
  "Machine did not answer.  Purple, apart from red: not a policy failure.")

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

(defface monk-ui-plan-header
  '((((background dark)) :background "#1f1b2e" :foreground "#d2a8ff" :extend t)
    (t :background "#fbefff" :foreground "#6639ba" :extend t))
  "Header line of a plan (dry run).  Purple, to differ from an apply.")

(defface monk-ui-plan-badge
  '((((background dark)) :foreground "#0d1117" :background "#a371f7" :weight bold)
    (t :foreground "#ffffff" :background "#8250df" :weight bold))
  "The PLAN word in the header.")

(defface monk-ui-apply-badge
  '((((background dark)) :foreground "#0d1117" :background "#58a6ff" :weight bold)
    (t :foreground "#ffffff" :background "#0969da" :weight bold))
  "The APPLY word in the header.")

(defface monk-ui-as-planned
  '((t :inherit monk-ui-kept))
  "Apply matched the plan.")

(defface monk-ui-unplanned
  '((t :inherit monk-ui-partial :weight bold))
  "Apply changed something the plan did not predict.")

(defface monk-ui-unfulfilled
  '((t :inherit monk-ui-would-repair :weight bold))
  "The plan predicted a change that apply did not make.")

(defface monk-ui-watch
  '((((background dark)) :foreground "#0d1117" :background "#3fb950" :weight bold)
    (t :foreground "#ffffff" :background "#1a7f37" :weight bold))
  "The WATCH indicator when a watch is active.")

(defface monk-ui-autopilot
  '((((background dark)) :foreground "#0d1117" :background "#d29922" :weight bold)
    (t :foreground "#ffffff" :background "#9a6700" :weight bold))
  "The WATCH indicator in autopilot mode.")

(defface monk-ui-separator
  '((((background dark)) :overline "#30363d" :height 0.4 :extend t)
    (t :overline "#d0d7de" :height 0.4 :extend t))
  "Hairline between sections.")

(defface monk-ui-fold
  '((((background dark)) :foreground "#8b949e" :weight bold)
    (t :foreground "#57606a" :weight bold))
  "Heading of a section that TAB folds and unfolds.")

(defface monk-ui-key
  '((((background dark)) :foreground "#79c0ff")
    (t :foreground "#0550ae"))
  "A fact or variable name in the machine inspector.")

(defface monk-ui-class
  '((((background dark)) :foreground "#0d1117" :background "#8b949e")
    (t :foreground "#ffffff" :background "#6e7781"))
  "One class in the machine inspector.")

(defface monk-ui-facts-fresh
  '((((background dark)) :foreground "#8b949e")
    (t :foreground "#6e7781"))
  "Age of facts that are within `monk-fact-ttl'.")

(defface monk-ui-facts-stale
  '((((background dark)) :foreground "#d29922" :weight bold)
    (t :foreground "#9a6700" :weight bold))
  "Age badge of stale facts.
Facts older than `monk-fact-ttl', or from the cache of a machine that
did not answer.  What the guards saw may not be what is.")

(defun monk-ui-facts-age-string (seconds)
  "Return SECONDS as a short age: 3m, 2h, 5d."
  (cond ((< seconds 60) (format "%ds" (round seconds)))
        ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
        ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
        (t (format "%dd" (round (/ seconds 86400))))))

(defun monk-ui-facts-badge (m)
  "Return a short badge that says how old and how trustworthy M's facts are.
Empty when the facts were gathered live in this session: nothing to
say.  `·3h' in grey for fresh cached facts, `!2d' in amber for stale
ones, `?' when there are no facts at all."
  (let ((age (monk-machine-facts-age m)))
    (cond
     ((null (oref m facts)) (propertize "?" 'face 'monk-ui-facts-stale
                                        'help-echo "no facts gathered"))
     ((eq (oref m facts-source) 'live) "")
     ((null age) "")
     ((monk-machine-facts-fresh-p m)
      (propertize (concat "·" (monk-ui-facts-age-string age)) 'face 'monk-ui-facts-fresh
                  'help-echo (format "facts from the cache, %s old, within monk-fact-ttl"
                                     (monk-ui-facts-age-string age))))
     (t (propertize (concat "!" (monk-ui-facts-age-string age)) 'face 'monk-ui-facts-stale
                    'help-echo (format "facts from the cache, %s old, past monk-fact-ttl: stale"
                                       (monk-ui-facts-age-string age)))))))

(defface monk-ui-match
  '((t :inherit monk-ui-kept :weight bold))
  "A machine that a query matched.")

(defface monk-ui-no-match
  '((t :inherit monk-ui-skipped))
  "A machine that a query did not match.")

(defconst monk-ui-status-faces
  `((,monk-status-kept . monk-ui-kept)
    (,monk-status-repaired . monk-ui-repaired)
    (,monk-status-would-repair . monk-ui-would-repair)
    (,monk-status-partial . monk-ui-partial)
    (,monk-status-failed . monk-ui-failed)
    (,monk-status-unreachable . monk-ui-unreachable)
    (,monk-status-skipped . monk-ui-skipped)
    (,monk-status-running . monk-ui-running)
    (,monk-status-pending . monk-ui-pending))
  "Face for each status.")

(defconst monk-ui-plan-faces
  `((,monk-plan-as-planned . monk-ui-as-planned)
    (,monk-plan-unplanned . monk-ui-unplanned)
    (,monk-plan-unfulfilled . monk-ui-unfulfilled)
    (,monk-plan-failed . monk-ui-failed)
    (,monk-plan-new . monk-ui-unplanned)
    (,monk-plan-missing . monk-ui-unfulfilled))
  "Face for each plan comparison verdict.")

(defconst monk-ui-plan-glyphs
  `((,monk-plan-as-planned "✓" "=")
    (,monk-plan-unplanned "⚠" "+")
    (,monk-plan-unfulfilled "○" "o")
    (,monk-plan-failed "✗" "!")
    (,monk-plan-new "?" "?")
    (,monk-plan-missing "∅" "0"))
  "Unicode and ASCII glyph for each plan verdict.")

(defconst monk-ui-status-glyphs
  `((,monk-status-kept "✓" "=")
    (,monk-status-repaired "●" "*")
    (,monk-status-would-repair "◐" "~")
    (,monk-status-partial "◒" "%")
    (,monk-status-failed "✗" "!")
    (,monk-status-unreachable "⊘" "x")
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

(defun monk-ui-plan-badge (verdict &optional text)
  "Return a badge for plan VERDICT: glyph, space, then TEXT or the verdict name."
  (let ((entry (alist-get verdict monk-ui-plan-glyphs)))
    (propertize (concat (if (and monk-ui-unicode (char-displayable-p (string-to-char (car entry))))
                            (car entry)
                          (cadr entry))
                        " " (or text (symbol-name verdict)))
                'face (or (alist-get verdict monk-ui-plan-faces) 'default)
                'help-echo (symbol-name verdict))))

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

(defvar-local monk-ui--detail-machine nil
  "Machine name a detail buffer shows, so a rerun can redraw it.")

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
   ((> (alist-get monk-status-unreachable counts 0) 0) monk-status-unreachable)
   ((> (alist-get monk-status-failed counts 0) 0) monk-status-failed)
   ((> (alist-get monk-status-partial counts 0) 0) monk-status-partial)
   ((> (alist-get monk-status-would-repair counts 0) 0) monk-status-would-repair)
   ((> (alist-get monk-status-repaired counts 0) 0) monk-status-repaired)
   ((> (alist-get monk-status-kept counts 0) 0) monk-status-kept)
   (t monk-status-skipped)))

;;;; Report buffer

(defun monk-ui-quit ()
  "Bury this Monk buffer.
The window shows its previous buffer again, or closes when it was
made for this one.  The buffer stays, so the next report reuses it."
  (interactive)
  (quit-window))

(defvar monk-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'monk-ui-quit)
    (define-key map (kbd "RET") #'monk-ui-show-machine)
    (define-key map (kbd "d") #'monk-ui-show-machine)
    (define-key map (kbd "g") #'monk-ui-rerun)
    (define-key map (kbd "t") #'monk-ui-toggle-style)
    (define-key map (kbd "a") #'monk-ui-apply)
    (define-key map (kbd "p") #'monk-ui-show-plan)
    (define-key map (kbd "w") #'monk-ui-show-watch-log)
    (define-key map (kbd "W") #'monk-ui-toggle-watch-mode)
    (define-key map (kbd "i") #'monk-ui-inspect-machine)
    (define-key map (kbd "/") #'monk-ui-query)
    (define-key map (kbd "h") #'monk-ui-history)
    (define-key map (kbd ".") #'monk-ui-find-definition)
    (define-key map (kbd "M-.") #'monk-ui-find-definition)
    (define-key map (kbd "?") #'monk-ui-help)
    map)
  "Keymap for `monk-ui-mode'.")

(defun monk-ui--format ()
  "Return the `tabulated-list-format' for the run in this buffer.
A plan says `Would' where an apply says `Changed'.  An apply that has
a plan gains a Plan column."
  (let ((run monk-ui--run))
    (vconcat
     (list (list "Machine" 18 t)
           (list "State" 14 t)
           (list "Progress" (+ 2 monk-ui-bar-width) nil)
           (list "Kept" 5 nil :right-align t)
           (list (if (and run (monk-run-dry-run run)) "Would" "Changed") 8 nil :right-align t)
           (list "Failed" 7 nil :right-align t)
           (list "Skipped" 8 nil :right-align t))
     (and run (monk-run-plan run) (not (monk-run-dry-run run))
          (list (list "Plan" 16 t)))
     (list (list "Time" 6 nil :right-align t)))))

(define-derived-mode monk-ui-mode tabulated-list-mode "Monk"
  "Report of a Monk run, one row per machine.
\\{monk-ui-mode-map}"
  (setq tabulated-list-format (monk-ui--format)
        tabulated-list-padding 1)
  ;; The header line carries Monk's summary: the PLAN or APPLY badge,
  ;; the total bar, the badges, the flapping and watch indicators.
  ;; tabulated-list would overwrite it with the column names, so tell
  ;; it to draw those as the first body line instead.  Found on the
  ;; first live tour: no header line was ever visible.
  (setq-local tabulated-list-use-header-line nil)
  (add-hook 'window-configuration-change-hook #'monk-ui-reset-cache nil t)
  (tabulated-list-init-header)
  (setq-local header-line-format '(:eval (monk-ui--header-line))))

(defun monk-ui-help ()
  "Show the keys of the report buffer."
  (interactive)
  (message "%s" (substitute-command-keys
                 "\\[monk-ui-show-machine] detail  \\[monk-ui-inspect-machine] facts  \\[monk-ui-find-definition] source  \\[monk-ui-query] query  \\[monk-ui-history] history  \\[monk-ui-apply] apply plan  \\[monk-ui-show-plan] show plan  \\[monk-ui-show-watch-log] watch log  \\[monk-ui-toggle-watch-mode] watch mode  \\[monk-ui-toggle-style] svg/text  \\[monk-ui-rerun] rerun (C-u: redraw only)  \\[monk-ui-quit] quit")))

(declare-function monk-watch-status "monk-watch")
(declare-function monk-watch-toggle-mode "monk-watch")
(declare-function monk-watch-active-p "monk-watch")
(defvar monk-watch-mode)
(defvar monk-watch-log-buffer-name)

(defun monk-ui--watch-indicator ()
  "Return the WATCH badge for the header, or an empty string."
  (if (and (featurep 'monk-watch) (monk-watch-active-p))
      (concat
       (propertize (if (eq monk-watch-mode 'autopilot) " AUTOPILOT " " WATCH ")
                   'face (if (eq monk-watch-mode 'autopilot) 'monk-ui-autopilot 'monk-ui-watch)
                   'help-echo (concat (monk-watch-status)
                                      (substitute-command-keys
                                       "\n\\[monk-ui-show-watch-log] log, \\[monk-ui-toggle-watch-mode] mode")))
       " ")
    ""))

(defun monk-ui-show-watch-log ()
  "Show the watch event log."
  (interactive)
  (unless (featurep 'monk-watch)
    (user-error "Load monk-watch first"))
  (pop-to-buffer (get-buffer-create monk-watch-log-buffer-name)))

(defun monk-ui-toggle-watch-mode ()
  "Switch the watch between assisted and autopilot."
  (interactive)
  (unless (featurep 'monk-watch)
    (user-error "Load monk-watch first"))
  (monk-watch-toggle-mode)
  (force-mode-line-update t))

(defun monk-ui--machine-names ()
  "Return the machine names of the live or finished run."
  (let ((run (or monk-ui--live-run monk-ui--run)))
    (and run (mapcar (lambda (m) (oref m name)) (monk-run-machines run)))))

(defun monk-ui--machine-duration (machine-name)
  "Return the total seconds spent on MACHINE-NAME."
  (apply #'+ (mapcar #'monk-result-duration (monk-ui--results machine-name))))

(defun monk-ui--comparison ()
  "Return the plan comparison for the run in this buffer, or nil."
  (let ((run monk-ui--run))
    (and run (monk-run-plan run) (not (monk-run-dry-run run))
         (not monk-ui--live-run)
         (monk-run-compare (monk-run-plan run) run))))

(defun monk-ui--machine-plan-cell (name comparison)
  "Return the Plan column text for machine NAME from COMPARISON."
  (let ((counts nil))
    (dolist (cell comparison)
      (when (equal (caar cell) name)
        (cl-incf (alist-get (cdr cell) counts 0))))
    (if (cl-every (lambda (c) (eq (car c) monk-plan-as-planned)) counts)
        (monk-ui-plan-badge monk-plan-as-planned "as planned")
      ;; Verdicts in a fixed order, so two rows read alike.
      (mapconcat #'identity
                 (delq nil
                       (mapcar (lambda (v)
                                 (let ((n (alist-get v counts 0)))
                                   (and (> n 0) (not (eq v monk-plan-as-planned))
                                        (monk-ui-plan-badge v (number-to-string n)))))
                               monk-plan-verdicts))
                 " "))))

(defun monk-ui--entries ()
  "Return `tabulated-list-entries' for the current run."
  (let ((comparison (monk-ui--comparison)))
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
               (vconcat
                (list (monk-ui--machine-cell name)
                      (if (eq verdict monk-status-running)
                          (concat (monk-ui-spinner) " "
                                  (propertize "running" 'face 'monk-ui-running))
                        (monk-ui-badge verdict))
                      (monk-ui-bar counts total)
                      (number-to-string (alist-get monk-status-kept counts 0))
                      (number-to-string changed)
                      (number-to-string failed)
                      (number-to-string (alist-get monk-status-skipped counts 0)))
                (and (= (length tabulated-list-format) 9)
                     (list (if comparison (monk-ui--machine-plan-cell name comparison) "")))
                (list (format "%.1fs" (monk-ui--machine-duration name)))))))
     (monk-ui--machine-names))))

(defun monk-ui--machine-cell (name)
  "Return the first column for machine NAME: the name and its badges.
`tabulated-list-print-col' keeps a cell's own `help-echo' only when
the first character has one, else it wraps the whole cell in
\"Machine: ...\".  So the name carries the tooltips of its badges."
  (let* ((m (monk-ui--run-machine name))
         (facts (if m (monk-ui-facts-badge m) ""))
         (batch (monk-ui-batch-badge name))
         (tips (delq nil (list (and (not (string-empty-p facts)) (get-text-property 0 'help-echo facts))
                               (and (not (string-empty-p batch)) (get-text-property 1 'help-echo batch))))))
    (concat (propertize name 'face 'monk-ui-machine
                        'help-echo (if tips (string-join tips "\n") (format "Machine: %s" name)))
            (if (string-empty-p facts) "" (concat " " facts))
            batch)))

(defun monk-ui-batch-badge (name)
  "Return a badge with the batch number of machine NAME, or the empty string.
Only a run split by :serial has batches.  A batch that never started
because an earlier one failed past the threshold is marked."
  (let* ((run (or monk-ui--live-run monk-ui--run))
         (batches (and run (monk-run-batches run)))
         (index (and batches (seq-position batches name (lambda (batch n) (member n batch))))))
    (if (null index)
        ""
      (let* ((number (1+ index))
             (stopped (monk-run-stopped-at-batch run))
             (skipped (and stopped (> number stopped))))
        (concat " "
                (propertize (format "b%d%s" number (if skipped "×" ""))
                            'face (if skipped 'monk-ui-skipped 'monk-ui-key)
                            'help-echo (cond
                                        (skipped (format "Batch %d of %d: never started, batch %d failed past :max-fail-percentage"
                                                         number (length batches) stopped))
                                        (t (format "Batch %d of %d (:serial)" number (length batches))))))))))

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
    (let* ((done (apply #'+ (mapcar #'cdr counts)))
           (plan (and run (monk-run-dry-run run)))
           (comparison (monk-ui--comparison))
           (header-face (if plan 'monk-ui-plan-header 'monk-ui-header)))
      (concat
       (propertize " " 'face header-face)
       (monk-ui--watch-indicator)
       (if monk-ui--live-run
           (concat (monk-ui-spinner) " ")
         "")
       (propertize (if plan " PLAN " " APPLY ")
                   'face (if plan 'monk-ui-plan-badge 'monk-ui-apply-badge)
                   'help-echo (if plan
                                  "Dry run: nothing was changed.  Press a to apply."
                                "Changes were made."))
       (if (and run (monk-run-plan run))
           (propertize " from plan" 'face header-face)
         "")
       (if (and run (monk-run-reasons run))
           (propertize (format " drift: %s" (mapconcat #'cdr (monk-run-reasons run) ", "))
                       'face header-face
                       'help-echo (mapconcat (lambda (c) (format "%s:%s  %s" (caar c) (cdar c) (cdr c)))
                                             (monk-run-reasons run) "\n"))
         "")
       "  "
       (monk-ui-bar counts total (format "%d/%d  pass %d" done total
                                         (if monk-ui--live-run
                                             monk-ui--live-pass
                                           (or (and run (monk-run-passes run)) 0)))
                    (min 40 (max 10 (- (window-width) 40))))
       "  "
       (mapconcat #'identity
                  (delq nil
                        (mapcar (lambda (status)
                                  (let ((n (alist-get status counts 0)))
                                    (and (> n 0) (monk-ui-badge status (number-to-string n)))))
                                monk-ui-status-order))
                  " ")
       (if (and run (monk-run-stopped-at-batch run))
           (concat "  " (propertize (format "%s stopped at batch %d/%d"
                                            (monk-ui-glyph monk-status-failed)
                                            (monk-run-stopped-at-batch run)
                                            (length (monk-run-batches run)))
                                    'face 'monk-ui-failed
                                    'help-echo "More machines failed in that batch than :max-fail-percentage allows; later batches were skipped."))
         "")
       (if (and run (not monk-ui--live-run) (monk-run-flapping-p run))
           (concat "  " (propertize (format "%s flapping %d"
                                            (monk-ui-glyph monk-status-partial)
                                            (length (monk-run-flapping run)))
                                    'face 'monk-ui-partial
                                    'help-echo (mapconcat
                                                (lambda (key) (format "%s:%s" (car key) (cdr key)))
                                                (monk-run-flapping run) "\n")))
         "")
       (if comparison
           (let ((vcounts (monk-run-compare-counts comparison)))
             (concat "  │ "
                     (mapconcat #'identity
                                (delq nil
                                      (mapcar (lambda (v)
                                                (let ((n (alist-get v vcounts 0)))
                                                  (and (> n 0)
                                                       (monk-ui-plan-badge v (number-to-string n)))))
                                              monk-plan-verdicts))
                                " ")))
         "")
       (if (and plan (not monk-ui--live-run))
           (propertize (substitute-command-keys "  \\[monk-ui-apply] to apply")
                       'face header-face)
         "")))))

(defun monk-ui-rerun (&optional redraw-only)
  "Run the policy of this buffer again and show the new result.
A plan is planned again.  An apply is applied again, against the
same plan when it had one.  With REDRAW-ONLY, the prefix argument,
only redraw the buffer from the stored run."
  (interactive "P")
  (cond
   (redraw-only (monk-ui-refresh))
   ((or (null monk-ui--run) (null (monk-run-policy monk-ui--run)))
    (user-error "No run to repeat here; redrawing")
    (monk-ui-refresh))
   (monk-ui--live-run
    (user-error "A run is in progress"))
   (t
    (let* ((run monk-ui--run)
           (options (copy-sequence (monk-run-options run))))
      (when (monk-run-plan run)
        (setq options (plist-put options :plan (monk-run-plan run))))
      (monk-ui--show-follow-up
       (apply #'monk-orchestrate (monk-run-machines run) (monk-run-policy run)
              :dry-run (monk-run-dry-run run) options))))))

(defun monk-ui-refresh ()
  "Redraw the report buffer.
The column set depends on the run kind, so rebuild the header when it
changed."
  (interactive)
  (when (derived-mode-p 'monk-ui-mode)
    (let ((format (monk-ui--format)))
      (unless (equal format tabulated-list-format)
        (setq tabulated-list-format format)
        (tabulated-list-init-header)))
    (setq tabulated-list-entries (monk-ui--entries))
    (tabulated-list-print t)
    ;; `tabulated-list-init-header' with the header line off does not
    ;; touch `header-line-format', but be sure nothing else did either.
    (setq header-line-format '(:eval (monk-ui--header-line)))))

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

(defun monk-ui--insert-result (result &optional planned run)
  "Insert one RESULT into the detail buffer.
With PLANNED, the matching result from the plan, add a plan verdict
and the planned status when it differs.  RUN supplies drift reasons."
  (let ((status (monk-result-status result))
        (start (point)))
    (insert (monk-ui-badge status)
            (make-string (max 1 (- 14 (length (symbol-name status)))) ?\s)
            (propertize (format "%-14s" (monk-result-type result)) 'face 'monk-ui-skipped)
            (monk-result-name result))
    (put-text-property start (point) 'monk-ui-result result)
    (let ((reason (and run
                       (alist-get (monk-result-key result) (monk-run-reasons run)
                                  nil nil #'equal))))
      (when reason
        (insert "  " (propertize (concat "drift: " reason) 'face 'monk-ui-partial))))
    (when planned
      (let ((verdict (monk--plan-verdict planned result)))
        (unless (eq verdict monk-plan-as-planned)
          (insert "  " (monk-ui-plan-badge verdict
                                           (format "%s (plan said %s)" verdict
                                                   (monk-result-status planned)))))))
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
      (insert (propertize (format "    %s\n" (monk-describe-changes (monk-result-changes result)))
                          'face 'monk-ui-skipped)))
    (let ((text (monk-result-diff-text result)))
      (when text
        (let ((start (point)))
          (insert text)
          (unless (bolp) (insert "\n"))
          (monk-ui--fontify-diff start (point)))))))

(defun monk-ui--fontify-diff (start end)
  "Apply diff faces between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((face (cond ((looking-at "^\\+\\+\\+\\|^---") 'diff-header)
                        ((looking-at "^@@") 'diff-hunk-header)
                        ((looking-at "^\\+") 'diff-added)
                        ((looking-at "^-") 'diff-removed)
                        ((looking-at "^[a-z]+: .* → ") 'diff-changed)
                        (t nil))))
        (when face
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'face face))))
      (forward-line 1))))

;;;; Folded sections

(defvar monk-ui-fold-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'monk-ui-toggle-fold)
    (define-key map (kbd "<tab>") #'monk-ui-toggle-fold)
    (define-key map (kbd "RET") #'monk-ui-toggle-fold)
    (define-key map [mouse-1] #'monk-ui-toggle-fold)
    map)
  "Keymap on a fold heading.")

(defun monk-ui--fold-heading-at (pos)
  "Return the start of the fold heading that owns POS, or nil."
  (let ((id (get-text-property pos 'monk-ui-fold)))
    (when id
      (if (and (> pos (point-min)) (eq (get-text-property (1- pos) 'monk-ui-fold) id))
          (previous-single-property-change pos 'monk-ui-fold)
        pos))))

(defun monk-ui--fold-body (heading)
  "Return (START . END) of the body under fold HEADING."
  (let* ((id (get-text-property heading 'monk-ui-fold))
         (start (next-single-property-change heading 'monk-ui-fold nil (point-max)))
         (end (or (text-property-any start (point-max) 'monk-ui-fold-end id) (point-max))))
    (cons start end)))

(defun monk-ui--fold-set (heading closed)
  "Fold the body under HEADING when CLOSED is non-nil, else unfold it.
The indicator glyph is a `display' property on the first character of
the heading, so the text and its properties never change."
  (pcase-let ((`(,start . ,end) (monk-ui--fold-body heading))
              (inhibit-read-only t))
    (put-text-property start end 'invisible (and closed 'monk-ui-fold))
    (put-text-property heading (1+ heading) 'display
                       (if closed monk-ui-fold-indicator-closed monk-ui-fold-indicator-open))))

(defun monk-ui--fold-closed-p (heading)
  "Return non-nil when the body under HEADING is folded."
  (get-text-property (car (monk-ui--fold-body heading)) 'invisible))

(defun monk-ui-toggle-fold ()
  "Fold or unfold the section at point."
  (interactive)
  (let ((heading (or (monk-ui--fold-heading-at (point))
                     (monk-ui--fold-heading-at (line-beginning-position))
                     (user-error "No foldable section at point"))))
    (monk-ui--fold-set heading (not (monk-ui--fold-closed-p heading)))))

(defun monk-ui-insert-fold (id heading body-function &optional open)
  "Insert HEADING, then the output of BODY-FUNCTION, as fold ID.
The body starts folded unless OPEN is non-nil.  BODY-FUNCTION inserts
at point.  A folded body is invisible; TAB on the heading toggles it."
  (let ((heading-start (point)))
    (insert (propertize (concat monk-ui-fold-indicator-closed " " heading "\n")
                        'face 'monk-ui-fold
                        'monk-ui-fold id
                        'keymap monk-ui-fold-map
                        'help-echo "TAB: fold or unfold"))
    (put-text-property heading-start (1+ heading-start) 'display
                       (if open monk-ui-fold-indicator-open monk-ui-fold-indicator-closed))
    (let ((body-start (point)))
      (funcall body-function)
      (insert (propertize "\n" 'monk-ui-fold-end id))
      (unless open
        (put-text-property body-start (point) 'invisible 'monk-ui-fold)))
    heading-start))

(defun monk-ui--thing-at-point ()
  "Return (KIND . OBJECT) for the line at point: `result' or `machine', or nil."
  (let ((result (or (get-text-property (point) 'monk-ui-result)
                    (get-text-property (line-beginning-position) 'monk-ui-result)))
        (machine (or (get-text-property (point) 'monk-ui-machine)
                     (get-text-property (line-beginning-position) 'monk-ui-machine))))
    (cond (result (cons 'result result))
          (machine (cons 'machine machine))
          ((and (derived-mode-p 'monk-ui-mode) (tabulated-list-get-id))
           (let ((m (monk-ui--run-machine (tabulated-list-get-id))))
             (and m (cons 'machine m)))))))

(defun monk-ui-find-definition (&optional other-window)
  "Jump to the source of the thing at point.
On a result line, open the policy file at the resource.  On a machine
row or heading, open the inventory at the host.  With OTHER-WINDOW,
the prefix argument, show the file in another window.  `M-,' returns."
  (interactive "P")
  (pcase (monk-ui--thing-at-point)
    (`(result . ,result) (monk-result-find-definition result other-window))
    (`(machine . ,m) (monk-machine-find-definition m other-window))
    (_ (user-error "Nothing at point has a definition to jump to"))))

(defvar monk-ui-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'monk-ui-quit)
    (define-key map (kbd "TAB") #'monk-ui-toggle-fold)
    (define-key map (kbd "<tab>") #'monk-ui-toggle-fold)
    (define-key map (kbd ".") #'monk-ui-find-definition)
    (define-key map (kbd "M-.") #'monk-ui-find-definition)
    (define-key map (kbd "a") #'monk-ui-apply)
    (define-key map (kbd "g") #'monk-ui-rerun)
    (define-key map (kbd "p") #'monk-ui-show-plan)
    (define-key map (kbd "?") #'monk-ui-detail-help)
    map)
  "Keymap for `monk-ui-detail-mode'.")

(defun monk-ui-detail-help ()
  "Show the keys of a detail buffer."
  (interactive)
  (message "%s" (substitute-command-keys
                 "\\[monk-ui-toggle-fold] fold  \\[monk-ui-find-definition] source  \\[monk-ui-apply] apply plan  \\[monk-ui-rerun] rerun  \\[monk-ui-show-plan] show plan  \\[monk-ui-quit] quit")))

(define-derived-mode monk-ui-detail-mode special-mode "Monk-Detail"
  "Per-machine results, or the machine inspector.
\\{monk-ui-detail-mode-map}")

;;;; Machine inspector

(defun monk-ui--insert-machine-description (m)
  "Insert the classes, groups, facts, and vars of machine M at point."
  (let ((description (monk-machine-describe m)))
    (insert "  " (propertize "classes " 'face 'monk-ui-key))
    (dolist (class (alist-get 'classes description))
      (insert (propertize (format " %s " class) 'face 'monk-ui-class) " "))
    (insert "\n  " (propertize "groups  " 'face 'monk-ui-key)
            (mapconcat #'symbol-name (alist-get 'groups description) " ") "\n")
    (insert "  " (propertize "facts from " 'face 'monk-ui-key)
            (pcase (oref m facts-source)
              ('live "this session, gathered live")
              ('cache (let ((age (monk-machine-facts-age m)))
                        (concat "the cache, " (monk-ui-facts-age-string (or age 0)) " old, "
                                (if (monk-machine-facts-fresh-p m)
                                    (propertize "fresh" 'face 'monk-ui-facts-fresh)
                                  (propertize "stale" 'face 'monk-ui-facts-stale)))))
              (_ (propertize "nowhere yet" 'face 'monk-ui-facts-stale)))
            "\n")
    (dolist (section '(facts vars))
      (insert "  " (propertize (format "%s" section) 'face 'monk-ui-key) "\n")
      (if (null (alist-get section description))
          (insert (propertize "    none\n" 'face 'monk-ui-skipped))
        (dolist (cell (alist-get section description))
          (insert (format "    %-24s" (propertize (symbol-name (car cell)) 'face 'monk-ui-key))
                  (format "%S" (cdr cell)) "\n"))))))

(defun monk-ui-insert-machine-fold (m &optional open)
  "Insert a folded section with the description of machine M.
OPEN unfolds it.  Return the heading position.  The heading carries
M in the `monk-ui-machine' property, so `.' jumps to the inventory."
  (let ((heading (monk-ui-insert-fold (intern (format "machine-%s" (oref m name)))
                                      (format "%s: %d classes, %d facts%s, %d vars  (TAB to expand)"
                                              (oref m name)
                                              (length (oref m classes))
                                              (length (oref m facts))
                                              (pcase (oref m facts-source)
                                                ('live "")
                                                ('cache (format " %s" (monk-ui-facts-badge m)))
                                                (_ (if (oref m facts) "" " ?")))
                                              (length (oref m vars)))
                                      (lambda () (monk-ui--insert-machine-description m))
                                      open)))
    (put-text-property heading (save-excursion (goto-char heading) (line-end-position))
                       'monk-ui-machine m)
    heading))

(defun monk-ui--run-machine (name)
  "Return the machine object called NAME in the run of this buffer, or nil."
  (let ((run (or monk-ui--live-run monk-ui--run)))
    (and run (seq-find (lambda (m) (equal (oref m name) name)) (monk-run-machines run)))))

(defun monk-ui-inspect-machine (&optional machine-name)
  "Show classes, groups, facts, and vars of MACHINE-NAME, or the machine at point.
The buffer is `*monk: NAME facts*'.  Press `q' to close it."
  (interactive)
  (let* ((name (or machine-name (tabulated-list-get-id)
                   (user-error "No machine at point")))
         (m (or (monk-ui--run-machine name)
                (and (featurep 'monk-inventory) (monk-inventory-find name))
                (user-error "No machine %s in this run" name)))
         (buffer (get-buffer-create (format monk-ui-detail-buffer-format
                                            (concat name " facts")))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (monk-ui-detail-mode)
        (insert (propertize (format " %s " name) 'face '(monk-ui-header monk-ui-machine))
                (propertize " what guards can see\n" 'face 'monk-ui-header))
        (insert (propertize "\n" 'face 'monk-ui-separator))
        (monk-ui-insert-machine-fold m t)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))


;;;; Query: which machines match a guard

(defvar monk-inventory)
(declare-function monk-inventory-find "monk-inventory" (name))

(defvar monk-ui-query-history nil
  "Minibuffer history for `monk-ui-query'.")

(defvar-local monk-ui--query-text nil
  "The guard text shown in the query buffer.")

(defvar-local monk-ui--query-machines nil
  "The machines the query buffer evaluates against.")

(defun monk-ui--query-render (text machines gather)
  "Fill the current buffer with the matches of guard TEXT over MACHINES.
GATHER passes to `monk-cell-matches'."
  (let* ((inhibit-read-only t)
         (notation (if (string-prefix-p "(" (string-trim text)) 'cell 'cel))
         (form (condition-case err
                   (monk-cell-read text)
                 (monk-guard-error (cons 'error (cadr err)))))
         (matches (unless (eq (car-safe form) 'error)
                    (monk-cell-matches form machines gather)))
         (hits (seq-count (lambda (cell) (and (cdr cell) (not (monk-cell-error-p (cdr cell)))))
                          matches)))
    (erase-buffer)
    (insert (propertize " QUERY " 'face 'monk-ui-plan-badge)
            (propertize (format " %s\n" text) 'face 'monk-ui-plan-header))
    (insert (propertize "\n" 'face 'monk-ui-separator))
    (if (eq (car-safe form) 'error)
        (insert (propertize (format "\n  %s\n" (cdr form)) 'face 'monk-ui-failed)
                (propertize "\n  Edit the guard with e.\n" 'face 'monk-ui-skipped))
      (insert (format "\n  %-6s %s\n  %-6s %s\n\n"
                      (propertize "CELL" 'face 'monk-ui-key) (monk-cell-format form)
                      (propertize "CEL" 'face 'monk-ui-key) (monk-cell-to-cel form)))
      (insert (format "  %d of %d machine%s match\n\n"
                      hits (length matches) (if (= 1 (length matches)) "" "s")))
      (dolist (cell matches)
        (let* ((m (car cell))
               (result (cdr cell))
               (failed (monk-cell-error-p result)))
          (insert (propertize
                   (cond (failed (propertize "  ? " 'face 'monk-ui-failed))
                         (result (propertize "  ✓ " 'face 'monk-ui-match))
                         (t (propertize "  – " 'face 'monk-ui-no-match)))
                   'monk-ui-machine m)
                  (propertize (format "%-20s" (oref m name))
                              'face (cond (failed 'monk-ui-failed)
                                          (result 'monk-ui-match)
                                          (t 'monk-ui-no-match))
                              'monk-ui-machine m)
                  (propertize (cond (failed (format "%s" (cadr result)))
                                    ((null (oref m facts)) "no facts yet; g gathers")
                                    (t (monk-cell-explain m form notation)))
                              'face 'monk-ui-skipped)
                  "\n")
          (monk-ui-insert-machine-fold m))))
    (goto-char (point-min))))

(defvar monk-ui-query-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'monk-ui-quit)
    (define-key map (kbd "e") #'monk-ui-query-edit)
    (define-key map (kbd "g") #'monk-ui-query-gather)
    (define-key map (kbd "TAB") #'monk-ui-toggle-fold)
    (define-key map (kbd "<tab>") #'monk-ui-toggle-fold)
    (define-key map (kbd ".") #'monk-ui-find-definition)
    (define-key map (kbd "M-.") #'monk-ui-find-definition)
    map)
  "Keymap for `monk-ui-query-mode'.")

(define-derived-mode monk-ui-query-mode special-mode "Monk-Query"
  "Which machines a CEL or CELL guard matches.
\\{monk-ui-query-mode-map}")

(defun monk-ui-query (text &optional machines)
  "Show which of MACHINES match guard TEXT in `monk-ui-query-buffer-name'.
TEXT is CEL, or CELL when it starts with a parenthesis.  MACHINES
defaults to `monk-inventory'.  Machines without gathered facts show
their inventory data only; press `g' in the buffer to gather facts
over the network.  Press `e' to edit the guard, `q' to close."
  (interactive
   (list (read-string "Guard (CEL, or CELL in parentheses): " nil 'monk-ui-query-history)))
  (unless (or machines (bound-and-true-p monk-inventory))
    (user-error "The inventory is empty; load a policy file first"))
  (let ((buffer (get-buffer-create monk-ui-query-buffer-name)))
    (with-current-buffer buffer
      (monk-ui-query-mode)
      (setq monk-ui--query-text text
            monk-ui--query-machines (or machines monk-inventory))
      (monk-ui--query-render text monk-ui--query-machines nil))
    (pop-to-buffer buffer)))

(defun monk-ui-query-edit ()
  "Edit the guard of this query buffer and evaluate it again."
  (interactive)
  (let ((text (read-string "Guard: " monk-ui--query-text 'monk-ui-query-history)))
    (setq monk-ui--query-text text)
    (monk-ui--query-render text monk-ui--query-machines nil)))

(defun monk-ui-query-gather ()
  "Gather facts for machines in this query that have none, then evaluate again."
  (interactive)
  (monk-ui--query-render monk-ui--query-text monk-ui--query-machines t))

;;;; Check policy: compile only

(defconst monk-ui-check-buffer-name "*monk-check*"
  "Buffer of `monk-ui-check-policy'.")

(defvar monk-ui-check-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'monk-ui-quit)
    (define-key map (kbd "TAB") #'monk-ui-toggle-fold)
    (define-key map (kbd "<tab>") #'monk-ui-toggle-fold)
    (define-key map (kbd ".") #'monk-ui-find-definition)
    (define-key map (kbd "M-.") #'monk-ui-find-definition)
    map)
  "Keymap for `monk-ui-check-mode'.")

(define-derived-mode monk-ui-check-mode special-mode "Monk-Check"
  "A policy compiled with no connection: resources, tags, and errors.
\\{monk-ui-check-mode-map}")

(defun monk-ui-check-policy (machines policy &rest options)
  "Compile POLICY for MACHINES without a connection and show the outcome.
OPTIONS are as for `monk-check-policy'.  Each machine's resources are
listed with their type, tags, and, under :tags or :skip-tags, whether
the selection would run them.  Errors and unresolved requisites come
first.  `.' jumps to a resource's definition, `q' closes."
  (let* ((report (apply #'monk-check-policy machines policy options))
         (buffer (get-buffer-create monk-ui-check-buffer-name))
         (selected (plist-get report :selected))
         (selecting (or (plist-get options :tags) (plist-get options :skip-tags)))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (monk-ui-check-mode)
      (erase-buffer)
      (insert (propertize (if (monk-check-policy-ok-p report) " CHECK OK " " CHECK FAILED ")
                          'face (if (monk-check-policy-ok-p report) 'monk-ui-apply-badge 'monk-ui-failed))
              (format "  %d machine(s), %d resource(s), %d tag(s)%s\n"
                      (length (plist-get report :machines)) (length (plist-get report :nodes))
                      (length (plist-get report :tags))
                      (if selecting (format ", %d selected by tags %S skip %S" (length selected)
                                            (plist-get options :tags) (plist-get options :skip-tags))
                        "")))
      (dolist (cell (plist-get report :errors))
        (insert (propertize (format "  %s error: " (monk-ui-glyph monk-status-failed)) 'face 'monk-ui-failed)
                (car cell) ": " (cdr cell) "\n"))
      (dolist (text (plist-get report :unresolved))
        (insert (propertize (format "  %s unresolved: " (monk-ui-glyph monk-status-failed)) 'face 'monk-ui-failed)
                text "\n"))
      (when (plist-get report :tags)
        (insert "  " (propertize "tags " 'face 'monk-ui-key))
        (dolist (tag (plist-get report :tags))
          (insert (propertize (format " %s " tag) 'face 'monk-ui-class) " "))
        (insert "\n"))
      (dolist (name (plist-get report :machines))
        (let ((nodes (seq-filter (lambda (n) (equal (oref (monk-node-machine n) name) name))
                                 (plist-get report :nodes))))
          (insert "\n" (propertize name 'face 'monk-ui-machine 'monk-ui-machine (and nodes (monk-node-machine (car nodes))))
                  (format "  %d resource(s)\n" (length nodes)))
          (dolist (node nodes)
            (let* ((r (monk-node-resource node))
                   (key (monk-node-key node))
                   (in (or (not selecting) (member key selected)))
                   (start (point)))
              (insert (format "  %s %-14s %s"
                              (if selecting (monk-ui-glyph (if in monk-status-kept monk-status-skipped)) " ")
                              (monk-resource-type r) (oref r name)))
              (when (oref r tags)
                (insert "  ")
                (dolist (tag (oref r tags))
                  (insert (propertize (format " %s " tag) 'face 'monk-ui-class) " ")))
              (insert "\n")
              (put-text-property start (point) 'monk-ui-result
                                 (monk-result-create :machine name :name (oref r name)
                                                     :type (monk-resource-type r) :origin (oref r origin)))
              (unless in
                (add-face-text-property start (point) 'monk-ui-skipped))))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    report))

(defun monk-ui-show-machine (&optional machine-name)
  "Show the results for MACHINE-NAME, or the machine at point.
In an apply with a plan, results that differ from the plan come first."
  (interactive)
  (pop-to-buffer (monk-ui-detail-buffer (or machine-name (tabulated-list-get-id)
                                            (user-error "No machine at point")))))

(defun monk-ui-detail-buffer (name)
  "Fill and return the detail buffer for machine NAME, without showing it.
Reads the run of the current buffer, so call it from `*monk*'."
  (let* ((run (or monk-ui--live-run monk-ui--run))
         (plan (and run (not monk-ui--live-run) (monk-run-plan run)))
         (results (monk-ui--results name))
         (machine (monk-ui--run-machine name))
         (header-face (if (and run (monk-run-dry-run run)) 'monk-ui-plan-header 'monk-ui-header))
         (buffer (get-buffer-create (format monk-ui-detail-buffer-format name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (monk-ui-detail-mode)
        ;; The detail buffer knows its run, so a, g, and p work here
        ;; as they do in *monk*.
        (setq monk-ui--run run
              monk-ui--detail-machine name)
        (insert (propertize (if (and run (monk-run-dry-run run)) " PLAN " " APPLY ")
                            'face (if (and run (monk-run-dry-run run))
                                      'monk-ui-plan-badge 'monk-ui-apply-badge))
                (propertize (format " %s " name) 'face (list header-face 'monk-ui-machine))
                (propertize (format " %s\n" (if run (monk-run-summary run name) ""))
                            'face header-face))
        (insert (propertize "\n" 'face 'monk-ui-separator))
        (when machine
          (monk-ui-insert-machine-fold machine))
        (when plan
          (let ((off (seq-remove
                      (lambda (r) (eq (monk--plan-verdict (monk-run-result plan (monk-result-key r)) r)
                                      monk-plan-as-planned))
                      results)))
            (when off
              (insert "\n" (monk-ui-plan-badge monk-plan-unplanned
                                               (format "differs from plan (%d)" (length off)))
                      "\n")
              (dolist (r off)
                (monk-ui--insert-result r (monk-run-result plan (monk-result-key r)) run)))))
        ;; The status groups stay complete.  A row already shown in the
        ;; drift section repeats here without its diff, marked as such.
        (dolist (status monk-ui-status-order)
          (let ((group (seq-filter (lambda (r) (eq (monk-result-status r) status)) results)))
            (when group
              (insert "\n")
              (monk-ui-insert-fold
               (intern (format "status-%s" status))
               (concat (monk-ui-badge status (format "%s (%d)" status (length group)))
                       (if (memq status monk-ui-fold-statuses)
                           (propertize "  (TAB to expand)" 'face 'monk-ui-skipped)
                         ""))
               (lambda ()
                 (dolist (r group)
                   (let ((planned (and plan (monk-run-result plan (monk-result-key r)))))
                     (if (and planned (not (eq (monk--plan-verdict planned r) monk-plan-as-planned)))
                         (insert (propertize
                                  (concat (monk-ui-badge status)
                                          (make-string (max 1 (- 14 (length (symbol-name status)))) ?\s)
                                          (propertize (format "%-14s" (monk-result-type r)) 'face 'monk-ui-skipped)
                                          (monk-result-name r)
                                          (propertize "  see above" 'face 'monk-ui-skipped))
                                  'monk-ui-result r)
                                 "\n")
                       (monk-ui--insert-result r planned run)))))
               (not (memq status monk-ui-fold-statuses))))))
        (goto-char (point-min))))
    buffer))

(defun monk-ui-show-plan ()
  "Show the plan that the apply in this buffer came from."
  (interactive)
  (let ((plan (and monk-ui--run (monk-run-plan monk-ui--run))))
    (unless plan
      (user-error "This run has no plan"))
    (let ((buffer (get-buffer-create (format monk-ui-detail-buffer-format "plan"))))
      (with-current-buffer buffer
        (monk-ui-mode)
        (setq monk-ui--run plan)
        (monk-ui-refresh))
      (pop-to-buffer buffer))))

(defun monk-ui-apply ()
  "Apply the plan shown in this buffer after confirmation.
The confirmation names the machines and the count of changes.  The
apply report replaces the plan in this buffer and compares against it."
  (interactive)
  (let ((plan monk-ui--run))
    (unless (and plan (monk-run-dry-run plan))
      (user-error "This buffer does not show a plan"))
    (when monk-ui--live-run
      (user-error "A run is in progress"))
    (let ((would (monk-run-count plan monk-status-would-repair))
          (failed (+ (monk-run-count plan monk-status-failed)
                     (monk-run-count plan monk-status-partial)))
          (names (mapconcat (lambda (m) (oref m name)) (monk-run-machines plan) ", ")))
      (when (yes-or-no-p
             (format "Apply %d change%s on %s%s? "
                     would (if (= would 1) "" "s") names
                     (if (> failed 0) (format " (%d failed in the plan)" failed) "")))
        (monk-ui--show-follow-up (monk-apply plan))))))

(defun monk-ui--show-follow-up (run)
  "Report RUN, then redraw a detail buffer that asked for it.
From `*monk*' the report replaces the buffer.  From a detail buffer
the report goes to `*monk*' as usual and the detail buffer shows the
same machine in the new run, so the eye stays where it was."
  (let ((machine monk-ui--detail-machine))
    (monk-run-report run)
    ;; In batch the report is text; fill *monk* anyway, so a drive or
    ;; a detail buffer reads the new run from it.
    (with-current-buffer (monk-ui-show run)
      (when machine
        (monk-ui-show-machine machine)))))

;;;; History

(defconst monk-ui-history-buffer-name "*monk-history*"
  "Buffer that lists past runs.")

(defconst monk-ui-history-limit 200
  "How many runs the history list shows at most.")

(declare-function monk-history-runs "monk-history")
(declare-function monk-history-results "monk-history")
(declare-function monk-history-repairs "monk-history")
(declare-function monk-history-run "monk-history")
(declare-function monk-history-run-count "monk-history")
(declare-function monk-history-path "monk-history")

(defvar-local monk-ui--history-filters nil
  "Filters the history buffer was built with.")

(defvar monk-ui-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'monk-ui-quit)
    (define-key map (kbd "RET") #'monk-ui-history-open-run)
    (define-key map (kbd "r") #'monk-ui-history-resource)
    (define-key map (kbd "c") #'monk-ui-history-changes)
    (define-key map (kbd "f") #'monk-ui-history-filter-host)
    (define-key map (kbd "g") #'monk-ui-history-refresh)
    (define-key map (kbd "?") #'monk-ui-history-help)
    map)
  "Keymap for `monk-ui-history-mode'.")

(define-derived-mode monk-ui-history-mode tabulated-list-mode "Monk-History"
  "Past Monk runs, newest first.
\\{monk-ui-history-mode-map}"
  (setq tabulated-list-format
        [("Started" 20 t) ("Kind" 6 t) ("Hosts" 18 t) ("Kept" 5 nil :right-align t)
         ("Changed" 8 nil :right-align t) ("Failed" 7 nil :right-align t)
         ("Skipped" 8 nil :right-align t) ("Passes" 7 nil :right-align t) ("Note" 20 nil)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun monk-ui-history-help ()
  "Show the keys of the history buffer."
  (interactive)
  (message "%s" (substitute-command-keys
                 "\\[monk-ui-history-open-run] open run  \\[monk-ui-history-changes] changes in run  \\[monk-ui-history-resource] resource timeline  \\[monk-ui-history-filter-host] filter host  \\[monk-ui-history-refresh] refresh  \\[monk-ui-quit] quit")))

(defun monk-ui--history-entry (row)
  "Return a `tabulated-list' entry for history ROW."
  (let* ((get (lambda (k) (alist-get k row)))
         (dry (eq 1 (funcall get 'dry_run)))
         (changed (+ (or (funcall get 'repaired) 0) (or (funcall get 'would_repair) 0)))
         (failed (+ (or (funcall get 'failed) 0) (or (funcall get 'partial) 0)))
         (note (cond ((funcall get 'flapping)
                      (propertize (concat "flapping " (funcall get 'flapping)) 'face 'monk-ui-partial))
                     ((funcall get 'reasons)
                      (propertize "watch" 'face 'monk-ui-watch))
                     ((funcall get 'plan_id)
                      (propertize "from plan" 'face 'monk-ui-as-planned))
                     (t ""))))
    (list (funcall get 'id)
          (vector (replace-regexp-in-string "T" " " (substring (funcall get 'started) 0 19))
                  (if dry
                      (propertize "plan" 'face 'monk-ui-plan-badge)
                    (propertize "apply" 'face 'monk-ui-apply-badge))
                  (propertize (funcall get 'hosts) 'face 'monk-ui-machine)
                  (number-to-string (or (funcall get 'kept) 0))
                  (propertize (number-to-string changed)
                              'face (if (> changed 0)
                                        (if dry 'monk-ui-would-repair 'monk-ui-repaired)
                                      'default))
                  (propertize (number-to-string failed)
                              'face (if (> failed 0) 'monk-ui-failed 'default))
                  (number-to-string (or (funcall get 'skipped) 0))
                  (number-to-string (or (funcall get 'passes) 0))
                  note))))

(defun monk-ui-history (&rest filters)
  "Show past runs in `monk-ui-history-buffer-name'.
FILTERS as for `monk-history-runs'.  Interactively, show everything."
  (interactive)
  (require 'monk-history)
  (let ((buffer (get-buffer-create monk-ui-history-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'monk-ui-history-mode)
        (monk-ui-history-mode))
      (setq monk-ui--history-filters filters)
      (monk-ui-history-refresh))
    (pop-to-buffer buffer)))

(defun monk-ui-history-refresh ()
  "Reread the history list from the database."
  (interactive)
  (let* ((filters monk-ui--history-filters)
         (rows (apply #'monk-history-runs :limit monk-ui-history-limit filters))
         (total (apply #'monk-history-run-count filters)))
    (setq tabulated-list-entries (mapcar #'monk-ui--history-entry rows))
    (tabulated-list-print t)
    (setq header-line-format
          (concat (propertize " HISTORY " 'face 'monk-ui-apply-badge)
                  (propertize (format " %d run%s%s in %s"
                                      total (if (= total 1) "" "s")
                                      (if (plist-get filters :host)
                                          (format " on %s" (plist-get filters :host))
                                        "")
                                      (abbreviate-file-name (monk-history-path)))
                              'face 'monk-ui-header)))))

(defun monk-ui-history-filter-host (host)
  "Show only runs that include HOST.  Empty HOST shows all."
  (interactive
   (list (completing-read "Host (empty for all): "
                          (delete-dups
                           (mapcan (lambda (row) (split-string (alist-get 'hosts row) " " t))
                                   (monk-history-runs :limit monk-ui-history-limit))))))
  (setq monk-ui--history-filters
        (if (string-empty-p host) nil (list :host host)))
  (monk-ui-history-refresh))

(defun monk-ui-history-open-run ()
  "Open the run at point in the report view."
  (interactive)
  (let* ((id (or (tabulated-list-get-id) (user-error "No run at point")))
         (run (or (monk-history-run id) (user-error "Run %s is gone" id)))
         (buffer (get-buffer-create (format monk-ui-detail-buffer-format id))))
    (with-current-buffer buffer
      (monk-ui-mode)
      (setq monk-ui--run run)
      (monk-ui-refresh))
    (pop-to-buffer buffer)))

(defun monk-ui-history-changes ()
  "List what changed in the run at point."
  (interactive)
  (let* ((id (or (tabulated-list-get-id) (user-error "No run at point")))
         (rows (monk-history-results :run id :changed t)))
    (monk-ui--history-results-buffer (format "changes in %s" id) rows)))

(defun monk-ui-history-resource (host name)
  "Show every time resource NAME on HOST changed, newest first.
Interactively, complete over resources seen in history."
  (interactive
   (let* ((rows (monk-history-results :changed t :limit 2000))
          (keys (delete-dups (mapcar (lambda (r) (format "%s %s" (alist-get 'host r) (alist-get 'name r)))
                                     rows)))
          (choice (completing-read "Host and resource: " keys nil t)))
     (split-string choice " " t nil)))
  (let ((name (if (listp name) (string-join name " ") name)))
    (monk-ui--history-results-buffer
     (format "%s on %s" name host)
     (monk-history-repairs host name :limit 500))))

(defun monk-ui--history-results-buffer (title rows)
  "Show history result ROWS under TITLE in a detail buffer."
  (let ((buffer (get-buffer-create (format monk-ui-detail-buffer-format title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (monk-ui-detail-mode)
        (insert (propertize " HISTORY " 'face 'monk-ui-apply-badge)
                (propertize (format " %s: %d change%s\n" title (length rows)
                                    (if (= 1 (length rows)) "" "s"))
                            'face 'monk-ui-header))
        (insert (propertize "\n" 'face 'monk-ui-separator))
        (if (null rows)
            (insert (propertize "\n  nothing changed\n" 'face 'monk-ui-skipped))
          (dolist (row rows)
            (let ((result (monk-history-result-to-result row)))
              (insert (propertize
                       (format "%s  " (replace-regexp-in-string "T" " " (substring (alist-get 'started row) 0 19)))
                       'face 'monk-ui-skipped))
              (when (eq 1 (alist-get 'dry_run row))
                (insert (propertize " plan " 'face 'monk-ui-plan-badge) " "))
              (monk-ui--insert-result result))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(declare-function monk-history-result-to-result "monk-history")

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

(defun monk-ui-show (run)
  "Fill the report buffer with RUN and return the buffer.
Works in batch too, for tests and drives that read the buffer."
  (let ((buffer (monk-ui--buffer)))
    (with-current-buffer buffer
      (setq monk-ui--run run)
      (monk-ui-refresh))
    buffer))

(defun monk-ui-select-window-p (run)
  "Return non-nil when the report for RUN must take the selected window.
See `monk-ui-select-window'.  A run with watch reasons came from a
watch, not from a command."
  (pcase monk-ui-select-window
    ('command (null (monk-run-reasons run)))
    ('nil nil)
    (_ t)))

(defun monk-ui-report (run &optional _verbose)
  "Show RUN in the report buffer.  In batch mode, print text instead."
  (if noninteractive
      (monk-run-report-text run)
    (let* ((buffer (monk-ui-show run))
           (window (display-buffer buffer)))
      (when (and (window-live-p window) (monk-ui-select-window-p run))
        (select-window window))
      (cond
       ((or (not (monk-run-ok-p run)) (not (monk-run-converged-p run)))
        (message "Monk: %s%s"
                 (monk-run-summary run)
                 (cond ((monk-run-flapping-p run)
                        (format " (flapping: %s)"
                                (mapconcat #'cdr (monk-run-flapping run) ", ")))
                       ((monk-run-oscillating-p run) " (did not converge)")
                       (t ""))))
       ((and (monk-run-plan run) (not (monk-run-as-planned-p run)))
        (let ((counts (monk-run-compare-counts (monk-run-compare (monk-run-plan run) run))))
          (message "Monk: apply differed from plan: %s"
                   (mapconcat (lambda (v)
                                (let ((n (alist-get v counts 0)))
                                  (if (and (> n 0) (not (eq v monk-plan-as-planned)))
                                      (format "%d %s" n v)
                                    "")))
                              monk-plan-verdicts " "))))
       ((monk-run-plan run)
        (message "Monk: applied as planned"))
       ((monk-run-dry-run run)
        (message "Monk: plan ready, %d would change; press a in *monk* to apply"
                 (monk-run-count run monk-status-would-repair)))))))

(add-hook 'monk-progress-functions #'monk-ui--progress)
(setq monk-report-function #'monk-ui-report)

(provide 'monk-ui)
;;; monk-ui.el ends here
