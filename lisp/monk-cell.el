;;; monk-cell.el --- CELL guard language and CEL translator  -*- lexical-binding: t; -*-

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

;; CELL is CEL in Lisp: a small guard language over the classes,
;; facts, inventory variables, and groups of a machine.  A CELL form is
;; data, not code.  It has no side effects, it always terminates, and
;; Monk can evaluate it on the controller without a connection to the
;; host.  Because it is data, Monk can also say which part of a guard
;; was false, print it in reports, and compare it between plans.
;;
;; CELL forms:
;;
;;   (class SYM)            machine has class SYM
;;   (group SYM)            machine is in inventory group SYM
;;   (fact KEY)             value of fact KEY, or nil
;;   (var KEY)              value of inventory variable KEY, or nil
;;   (name)                 inventory name of the machine
;;   (local)                t for the local host
;;   (classes) (groups) (facts) (vars)   the whole list or alist
;;   (has (fact KEY)) (has (var KEY))    the key is present
;;   (and ...) (or ...) (not X) (if C A B)
;;   (= A B) (/= A B) (< A B) (<= A B) (> A B) (>= A B)
;;   (in X LIST)            X is an element of LIST
;;   (starts-with S P) (ends-with S P) (contains S P) (match S REGEXP)
;;   (size X)               length of a string or list
;;   (list ...)             a literal list
;;   (+ A B) (- A B) (* A B) (/ A B) (% A B)
;;
;; Strings, numbers, t, and nil evaluate to themselves.  Anything else
;; signals `monk-guard-error'.
;;
;; CEL is Google's Common Expression Language.  `monk-cel-parse' turns
;; a CEL string into a CELL form, so people who know CEL can write
;; guards as strings:
;;
;;   "'darwin' in classes && facts.arch == 'arm64'"
;;   ==> (and (class darwin) (= (fact arch) "arm64"))
;;
;; In CEL, `classes' and `groups' are lists, `facts' and `vars' are
;; maps, `name' is a string, and `local' is a bool.  The CEL functions
;; has(), size(), startsWith(), endsWith(), contains(), and matches()
;; are supported.  `monk-cell-to-cel' prints a CELL form back as CEL.
;;
;; `monk-cell-select' returns the machines of a list that match, and
;; `monk-cell-query' is the interactive form of that for the inventory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function monk-machine-fact "monk" (m key &optional default))
(declare-function monk-machine-var "monk" (m key &optional default))
(declare-function monk-machine-local-p "monk" (m))
(declare-function monk-class-p "monk" (m class))
(declare-function monk-machine-in-group-p "monk" (m group))
(declare-function monk-gather-facts "monk" (m))
(declare-function monk-log "monk" (level format &rest args))
(declare-function eieio-oref "eieio-core" (obj slot))
(defvar monk-inventory)

;; `monk-guard-error' and `monk-cel-error' are defined in monk.el,
;; under `monk-error', before it requires this file.

;;;; Machine access

(defun monk-cell--slot (m slot)
  "Return SLOT of machine M through EIEIO."
  (eieio-oref m slot))

;;;; Evaluation

(defconst monk-cell-operators
  '(class group fact var name local classes groups facts vars has
    and or not if = /= < <= > >= in starts-with ends-with contains match
    size list + - * / %)
  "Every operator a CELL form can use.")

(defun monk-cell--arity (form min &optional max)
  "Signal unless FORM has between MIN and MAX arguments.  MAX nil: no bound."
  (let ((n (length (cdr form))))
    (when (or (< n min) (and max (> n max)))
      (signal 'monk-guard-error
              (list (format "%s takes %s argument%s, got %d"
                            (car form)
                            (if max (if (= min max) min (format "%d to %d" min max))
                              (format "at least %d" min))
                            (if (and max (= min max 1)) "" "s")
                            n))))))

(defun monk-cell--key (form)
  "Return the key argument of FORM as a symbol."
  (let ((key (cadr form)))
    (cond ((symbolp key) key)
          ((stringp key) (intern key))
          (t (signal 'monk-guard-error
                     (list (format "%s needs a symbol or string, got %S" (car form) key)))))))

(defun monk-cell--compare (op a b)
  "Compare A and B with OP, which is one of < <= > >=."
  (cond
   ((and (numberp a) (numberp b)) (funcall op a b))
   ((and (stringp a) (stringp b))
    (pcase op
      ('< (string< a b))
      ('<= (not (string< b a)))
      ('> (string< b a))
      ('>= (not (string< a b)))))
   (t (signal 'monk-guard-error
              (list (format "%s cannot compare %S and %S" op a b))))))

(defun monk-cell--string (op value)
  "Return VALUE when it is a string.  Signal for OP otherwise."
  (unless (stringp value)
    (signal 'monk-guard-error (list (format "%s needs a string, got %S" op value))))
  value)

(defun monk-cell--arith (op a b)
  "Apply arithmetic OP to A and B.  `+' also joins two strings."
  (cond
   ((and (eq op '+) (stringp a) (stringp b)) (concat a b))
   ((and (numberp a) (numberp b))
    (pcase op
      ('+ (+ a b)) ('- (- a b)) ('* (* a b))
      ('/ (if (zerop b) (signal 'monk-guard-error (list "division by zero")) (/ a b)))
      ('% (if (zerop b) (signal 'monk-guard-error (list "division by zero")) (mod a b)))))
   (t (signal 'monk-guard-error (list (format "%s cannot combine %S and %S" op a b))))))

(defun monk-cell-eval (m form)
  "Evaluate CELL FORM against machine M.  Return its value.
Signal `monk-guard-error' for a form that is not CELL."
  (cond
   ((or (stringp form) (numberp form) (eq form t) (null form)) form)
   ((not (consp form))
    (signal 'monk-guard-error
            (list (format "Not a CELL form: %S (use (fact %s) or (class %s)?)"
                          form form form))))
   (t
    (pcase (car form)
      ('class (monk-cell--arity form 1 1) (and (monk-class-p m (monk-cell--key form)) t))
      ('group (monk-cell--arity form 1 1) (and (monk-machine-in-group-p m (monk-cell--key form)) t))
      ('fact (monk-cell--arity form 1 1) (monk-machine-fact m (monk-cell--key form)))
      ('var (monk-cell--arity form 1 1) (monk-machine-var m (monk-cell--key form)))
      ('name (monk-cell--arity form 0 0) (monk-cell--slot m 'name))
      ('local (monk-cell--arity form 0 0) (and (monk-machine-local-p m) t))
      ('classes (monk-cell--arity form 0 0) (monk-cell--slot m 'classes))
      ('groups (monk-cell--arity form 0 0) (monk-cell--slot m 'groups))
      ('facts (monk-cell--arity form 0 0) (monk-cell--slot m 'facts))
      ('vars (monk-cell--arity form 0 0) (monk-cell--slot m 'vars))
      ('has
       (monk-cell--arity form 1 1)
       (pcase (cadr form)
         (`(fact ,_) (and (assq (monk-cell--key (cadr form)) (monk-cell--slot m 'facts)) t))
         (`(var ,_) (and (assq (monk-cell--key (cadr form)) (monk-cell--slot m 'vars)) t))
         (other (signal 'monk-guard-error
                        (list (format "has needs (fact KEY) or (var KEY), got %S" other))))))
      ('and (let ((value t) (rest (cdr form)))
              (while (and value rest)
                (setq value (monk-cell-eval m (pop rest))))
              value))
      ('or (let ((value nil) (rest (cdr form)))
             (while (and (not value) rest)
               (setq value (monk-cell-eval m (pop rest))))
             value))
      ('not (monk-cell--arity form 1 1) (not (monk-cell-eval m (cadr form))))
      ('if (monk-cell--arity form 2 3)
           (if (monk-cell-eval m (nth 1 form))
               (monk-cell-eval m (nth 2 form))
             (monk-cell-eval m (nth 3 form))))
      ('= (monk-cell--arity form 2 2)
          (equal (monk-cell-eval m (nth 1 form)) (monk-cell-eval m (nth 2 form))))
      ('/= (monk-cell--arity form 2 2)
           (not (equal (monk-cell-eval m (nth 1 form)) (monk-cell-eval m (nth 2 form)))))
      ((and (or '< '<= '> '>=) op)
       (monk-cell--arity form 2 2)
       (monk-cell--compare op (monk-cell-eval m (nth 1 form)) (monk-cell-eval m (nth 2 form))))
      ('in (monk-cell--arity form 2 2)
           (let ((item (monk-cell-eval m (nth 1 form)))
                 (set (monk-cell-eval m (nth 2 form))))
             (unless (listp set)
               (signal 'monk-guard-error (list (format "in needs a list, got %S" set))))
             (and (or (member item set)
                      (and (stringp item) (memq (intern item) set))
                      (and (symbolp item) (member (symbol-name item) set)))
                  t)))
      ('starts-with (monk-cell--arity form 2 2)
                    (string-prefix-p (monk-cell--string 'starts-with (monk-cell-eval m (nth 2 form)))
                                     (monk-cell--string 'starts-with (monk-cell-eval m (nth 1 form)))))
      ('ends-with (monk-cell--arity form 2 2)
                  (string-suffix-p (monk-cell--string 'ends-with (monk-cell-eval m (nth 2 form)))
                                   (monk-cell--string 'ends-with (monk-cell-eval m (nth 1 form)))))
      ('contains (monk-cell--arity form 2 2)
                 (and (string-search (monk-cell--string 'contains (monk-cell-eval m (nth 2 form)))
                                     (monk-cell--string 'contains (monk-cell-eval m (nth 1 form))))
                      t))
      ('match (monk-cell--arity form 2 2)
              (and (string-match-p (monk-cell--string 'match (monk-cell-eval m (nth 2 form)))
                                   (monk-cell--string 'match (monk-cell-eval m (nth 1 form))))
                   t))
      ('size (monk-cell--arity form 1 1)
             (let ((value (monk-cell-eval m (cadr form))))
               (if (or (stringp value) (listp value))
                   (length value)
                 (signal 'monk-guard-error (list (format "size needs a string or list, got %S" value))))))
      ('list (mapcar (lambda (x) (monk-cell-eval m x)) (cdr form)))
      ((and (or '+ '- '* '/ '%) op)
       (monk-cell--arity form 2 2)
       (monk-cell--arith op (monk-cell-eval m (nth 1 form)) (monk-cell-eval m (nth 2 form))))
      (op (signal 'monk-guard-error
                  (list (format "Unknown CELL operator %S in %S" op form))))))))

(defun monk-cell-check (form)
  "Signal `monk-guard-error' unless FORM is well-formed CELL.
This walks the form without a machine, so it catches unknown operators
and wrong arities at policy load time."
  (cond
   ((or (stringp form) (numberp form) (eq form t) (null form)) form)
   ((not (consp form))
    (signal 'monk-guard-error (list (format "Not a CELL form: %S" form))))
   ((not (memq (car form) monk-cell-operators))
    (signal 'monk-guard-error (list (format "Unknown CELL operator %S in %S" (car form) form))))
   (t
    (pcase (car form)
      ((or 'class 'group 'fact 'var) (monk-cell--arity form 1 1) (monk-cell--key form))
      ((or 'name 'local 'classes 'groups 'facts 'vars) (monk-cell--arity form 0 0))
      ('has (monk-cell--arity form 1 1)
            (unless (memq (car-safe (cadr form)) '(fact var))
              (signal 'monk-guard-error
                      (list (format "has needs (fact KEY) or (var KEY), got %S" (cadr form))))))
      ((or 'not 'size) (monk-cell--arity form 1 1) (monk-cell-check (cadr form)))
      ('if (monk-cell--arity form 2 3) (mapc #'monk-cell-check (cdr form)))
      ((or 'and 'or 'list) (mapc #'monk-cell-check (cdr form)))
      (_ (monk-cell--arity form 2 2) (mapc #'monk-cell-check (cdr form))))
    form)))

;;;; Explaining

(defun monk-cell-explain (m form)
  "Return why FORM is false or true on machine M, as one line.
For a false `and', name the first false part.  For a false `or', say
that every part was false.  For a comparison, show the values."
  (let ((value (monk-cell-eval m form)))
    (pcase form
      (`(and . ,parts)
       (if value
           (format "%s is true" (monk-cell-format form))
         (monk-cell-explain m (seq-find (lambda (p) (not (monk-cell-eval m p))) parts))))
      (`(or . ,parts)
       (if value
           (monk-cell-explain m (seq-find (lambda (p) (monk-cell-eval m p)) parts))
         (format "%s: every part is false" (monk-cell-format form))))
      (`(not ,inner)
       (if value
           (format "%s is false, so %s is true" (monk-cell-format inner) (monk-cell-format form))
         (format "%s is true, so %s is false" (monk-cell-format inner) (monk-cell-format form))))
      (`(,(and op (or '= '/= '< '<= '> '>= 'in 'starts-with 'ends-with 'contains 'match)) ,a ,b)
       (ignore op)
       (format "%s is %s: %s = %S, %s = %S"
               (monk-cell-format form) (if value "true" "false")
               (monk-cell-format a) (monk-cell-eval m a)
               (monk-cell-format b) (monk-cell-eval m b)))
      (`(class ,c) (format "class %s is %s" c (if value "present" "absent")))
      (`(group ,g) (format "group %s is %s" g (if value "present" "absent")))
      (_ (format "%s is %S" (monk-cell-format form) value)))))

(defun monk-cell-format (form)
  "Return FORM printed as CELL, without a quote."
  (let ((print-quoted t) (print-length nil) (print-level nil))
    (prin1-to-string form)))

;;;; Guards: CELL forms or CEL strings

(defun monk-cell-guard (guard)
  "Return GUARD as a checked CELL form.
GUARD is a CELL form or a CEL string.  Signal `monk-guard-error' when
it is neither, or `monk-cel-error' when the CEL does not parse."
  (monk-cell-check (if (stringp guard) (monk-cel-parse guard) guard)))

(defun monk-cell-read (text)
  "Return the guard in TEXT as a CELL form.
TEXT that starts with an open parenthesis is CELL, read with `read'.
Anything else is CEL."
  (let ((trimmed (string-trim text)))
    (if (string-prefix-p "(" trimmed)
        (monk-cell-check (car (read-from-string trimmed)))
      (monk-cel-parse trimmed))))

;;;; Selecting machines

(defun monk-cell-select (guard &optional machines)
  "Return the machines in MACHINES for which GUARD is true.
GUARD is a CELL form or a CEL string.  MACHINES defaults to
`monk-inventory'.  A machine that signals during evaluation, for
example because a fact is missing, does not match."
  (let ((form (monk-cell-guard guard)))
    (seq-filter (lambda (m)
                  (condition-case nil
                      (monk-cell-eval m form)
                    (monk-guard-error nil)))
                (or machines monk-inventory))))

(defun monk-cell-matches (guard &optional machines gather)
  "Return an alist (MACHINE . RESULT) for GUARD over MACHINES.
RESULT is the value, or a `monk-guard-error' condition when evaluation
signaled.  When GATHER is non-nil, gather facts first on machines
that have none.  MACHINES defaults to `monk-inventory'."
  (let ((form (monk-cell-guard guard)))
    (mapcar (lambda (m)
              (when (and gather (null (monk-cell--slot m 'facts)))
                (condition-case err
                    (monk-gather-facts m)
                  (error (monk-log 'warn "%s: cannot gather facts: %s"
                                   (monk-cell--slot m 'name) (error-message-string err)))))
              (cons m (condition-case err
                          (monk-cell-eval m form)
                        (monk-guard-error err))))
            (or machines monk-inventory))))

;;;; CEL tokenizer

(defconst monk-cel--token-regexp
  (rx (or (group-n 1 (or "&&" "||" "==" "!=" "<=" ">=" "<" ">" "!" "?" ":" "." ","
                         "(" ")" "[" "]" "+" "-" "*" "/" "%"))
          (group-n 2 (or (seq "\"" (* (or (seq "\\" anything) (not (any "\"\\")))) "\"")
                         (seq "'" (* (or (seq "\\" anything) (not (any "'\\")))) "'")))
          (group-n 3 (seq (+ digit) (? "." (+ digit))))
          (group-n 4 (seq (or alpha "_") (* (or alnum "_"))))))
  "One CEL token: operator, string, number, or identifier.")

(defun monk-cel--unescape (text)
  "Return the body of CEL string literal TEXT with escapes resolved."
  (let ((body (substring text 1 -1)))
    (replace-regexp-in-string
     "\\\\\\(.\\)"
     (lambda (whole)
       (pcase (aref whole 1)
         (?n "\n") (?t "\t") (?r "\r") (c (string c))))
     body t t)))

(defun monk-cel-tokenize (text)
  "Return the tokens of CEL TEXT as a list of (TYPE . VALUE).
TYPE is `op', `string', `number', or `ident'."
  (let ((pos 0) (tokens nil) (end (length text)))
    (while (< pos end)
      (cond
       ((memq (aref text pos) '(?\s ?\t ?\n))
        (setq pos (1+ pos)))
       ((eq (string-match monk-cel--token-regexp text pos) pos)
        (push (cond ((match-beginning 1) (cons 'op (match-string 1 text)))
                    ((match-beginning 2) (cons 'string (monk-cel--unescape (match-string 2 text))))
                    ((match-beginning 3) (cons 'number (string-to-number (match-string 3 text))))
                    (t (cons 'ident (match-string 4 text))))
              tokens)
        (setq pos (match-end 0)))
       (t (signal 'monk-cel-error
                  (list (format "Unexpected character %S at %d in %S"
                                (string (aref text pos)) pos text))))))
    (nreverse tokens)))

;;;; CEL parser

(defvar monk-cel--tokens nil
  "Remaining tokens during a parse.")

(defvar monk-cel--source nil
  "The CEL text being parsed, for error messages.")

(defun monk-cel--peek ()
  "Return the next token without consuming it."
  (car monk-cel--tokens))

(defun monk-cel--next ()
  "Consume and return the next token."
  (pop monk-cel--tokens))

(defun monk-cel--op-p (value)
  "Return non-nil when the next token is operator VALUE."
  (let ((token (monk-cel--peek)))
    (and (eq (car token) 'op) (equal (cdr token) value))))

(defun monk-cel--expect (value)
  "Consume operator VALUE or signal."
  (unless (monk-cel--op-p value)
    (signal 'monk-cel-error
            (list (format "Expected %S but found %S in %S"
                          value (cdr (monk-cel--peek)) monk-cel--source))))
  (monk-cel--next))

(defun monk-cel--error (format &rest args)
  "Signal `monk-cel-error' with FORMAT and ARGS, naming the source."
  (signal 'monk-cel-error (list (concat (apply #'format format args)
                                        (format " in %S" monk-cel--source)))))

(defun monk-cel-parse (text)
  "Parse CEL TEXT into a CELL form.  Signal `monk-cel-error' on bad input."
  (let ((monk-cel--tokens (monk-cel-tokenize text))
        (monk-cel--source text))
    (when (null monk-cel--tokens)
      (monk-cel--error "Empty expression"))
    (let ((form (monk-cel--ternary)))
      (when monk-cel--tokens
        (monk-cel--error "Unexpected %S after expression" (cdr (monk-cel--peek))))
      (monk-cell-check form))))

(defun monk-cel--ternary ()
  "Parse COND ? A : B."
  (let ((condition (monk-cel--or)))
    (if (monk-cel--op-p "?")
        (progn
          (monk-cel--next)
          (let ((then (monk-cel--ternary)))
            (monk-cel--expect ":")
            (list 'if condition then (monk-cel--ternary))))
      condition)))

(defun monk-cel--or ()
  "Parse A || B || C."
  (let ((parts (list (monk-cel--and))))
    (while (monk-cel--op-p "||")
      (monk-cel--next)
      (push (monk-cel--and) parts))
    (if (cdr parts) (cons 'or (nreverse parts)) (car parts))))

(defun monk-cel--and ()
  "Parse A && B && C."
  (let ((parts (list (monk-cel--relation))))
    (while (monk-cel--op-p "&&")
      (monk-cel--next)
      (push (monk-cel--relation) parts))
    (if (cdr parts) (cons 'and (nreverse parts)) (car parts))))

(defconst monk-cel--relations
  '(("==" . =) ("!=" . /=) ("<" . <) ("<=" . <=) (">" . >) (">=" . >=))
  "CEL comparison operators and their CELL names.")

(defun monk-cel--relation ()
  "Parse A == B, A in B, and the other comparisons."
  (let ((left (monk-cel--additive)))
    (cond
     ((and (eq (car (monk-cel--peek)) 'ident) (equal (cdr (monk-cel--peek)) "in"))
      (monk-cel--next)
      (monk-cel--in left (monk-cel--additive)))
     ((and (eq (car (monk-cel--peek)) 'op) (assoc (cdr (monk-cel--peek)) monk-cel--relations))
      (let ((op (cdr (assoc (cdr (monk-cel--next)) monk-cel--relations))))
        (list op left (monk-cel--additive))))
     (t left))))

(defun monk-cel--in (item set)
  "Return the CELL form for ITEM in SET.
A string in `classes' or `groups' becomes the (class SYM) or (group
SYM) form, so the report can name the class."
  (pcase (list item set)
    (`(,(and (pred stringp) s) (classes)) (list 'class (intern s)))
    (`(,(and (pred stringp) s) (groups)) (list 'group (intern s)))
    (`(,(and (pred stringp) s) (facts)) (list 'has (list 'fact (intern s))))
    (`(,(and (pred stringp) s) (vars)) (list 'has (list 'var (intern s))))
    (_ (list 'in item set))))

(defun monk-cel--additive ()
  "Parse A + B - C."
  (let ((left (monk-cel--multiplicative)))
    (while (or (monk-cel--op-p "+") (monk-cel--op-p "-"))
      (let ((op (intern (cdr (monk-cel--next)))))
        (setq left (list op left (monk-cel--multiplicative)))))
    left))

(defun monk-cel--multiplicative ()
  "Parse A * B / C % D."
  (let ((left (monk-cel--unary)))
    (while (or (monk-cel--op-p "*") (monk-cel--op-p "/") (monk-cel--op-p "%"))
      (let ((op (intern (cdr (monk-cel--next)))))
        (setq left (list op left (monk-cel--unary)))))
    left))

(defun monk-cel--unary ()
  "Parse !A and -A."
  (cond
   ((monk-cel--op-p "!") (monk-cel--next) (list 'not (monk-cel--unary)))
   ((monk-cel--op-p "-")
    (monk-cel--next)
    (let ((operand (monk-cel--unary)))
      (if (numberp operand) (- operand) (list '- 0 operand))))
   (t (monk-cel--postfix (monk-cel--primary)))))

(defconst monk-cel--methods
  '(("startsWith" . starts-with) ("endsWith" . ends-with)
    ("contains" . contains) ("matches" . match))
  "CEL string methods and their CELL names.")

(defun monk-cel--postfix (base)
  "Parse member access, method calls, and indexing after BASE."
  (let ((form base))
    (while (or (monk-cel--op-p ".") (monk-cel--op-p "["))
      (if (monk-cel--op-p "[")
          (progn
            (monk-cel--next)
            (let ((index (monk-cel--ternary)))
              (monk-cel--expect "]")
              (setq form (monk-cel--member form index))))
        (monk-cel--next)
        (let ((token (monk-cel--next)))
          (unless (eq (car token) 'ident)
            (monk-cel--error "Expected a name after ."))
          (if (monk-cel--op-p "(")
              (setq form (monk-cel--method form (cdr token) (monk-cel--arguments)))
            (setq form (monk-cel--member form (cdr token)))))))
    form))

(defun monk-cel--member (base key)
  "Return the CELL form for BASE.KEY or BASE[KEY]."
  (unless (or (stringp key) (symbolp key))
    (monk-cel--error "Map key must be a name or string, got %S" key))
  (let ((sym (if (stringp key) (intern key) key)))
    (pcase base
      ('(facts) (list 'fact sym))
      ('(vars) (list 'var sym))
      (_ (monk-cel--error "Only facts and vars have members, not %s" (monk-cell-format base))))))

(defun monk-cel--method (receiver name arguments)
  "Return the CELL form for RECEIVER.NAME(ARGUMENTS)."
  (let ((op (cdr (assoc name monk-cel--methods))))
    (unless op
      (monk-cel--error "Unknown method %s" name))
    (unless (= 1 (length arguments))
      (monk-cel--error "%s takes one argument" name))
    (list op receiver (car arguments))))

(defun monk-cel--arguments ()
  "Parse ( A, B, ... ) and return the list of forms."
  (monk-cel--expect "(")
  (let ((arguments nil))
    (unless (monk-cel--op-p ")")
      (push (monk-cel--ternary) arguments)
      (while (monk-cel--op-p ",")
        (monk-cel--next)
        (push (monk-cel--ternary) arguments)))
    (monk-cel--expect ")")
    (nreverse arguments)))

(defconst monk-cel--identifiers
  '(("classes" . (classes)) ("groups" . (groups)) ("facts" . (facts))
    ("vars" . (vars)) ("name" . (name)) ("local" . (local))
    ("true" . t) ("false" . nil) ("null" . nil))
  "CEL identifiers Monk defines and their CELL forms.")

(defun monk-cel--primary ()
  "Parse a literal, an identifier, a function call, a list, or ( A )."
  (let ((token (monk-cel--next)))
    (pcase token
      (`(string . ,s) s)
      (`(number . ,n) n)
      (`(op . "(")
       (let ((inner (monk-cel--ternary)))
         (monk-cel--expect ")")
         inner))
      (`(op . "[")
       (let ((items nil))
         (unless (monk-cel--op-p "]")
           (push (monk-cel--ternary) items)
           (while (monk-cel--op-p ",")
             (monk-cel--next)
             (push (monk-cel--ternary) items)))
         (monk-cel--expect "]")
         (cons 'list (nreverse items))))
      (`(ident . ,name)
       (cond
        ((monk-cel--op-p "(")
         (monk-cel--call name (monk-cel--arguments)))
        ((assoc name monk-cel--identifiers)
         (cdr (assoc name monk-cel--identifiers)))
        (t (monk-cel--error "Unknown name %s (use facts.%s, vars.%s, or '%s' in classes)"
                            name name name name))))
      (`nil (monk-cel--error "Unexpected end of expression"))
      (_ (monk-cel--error "Unexpected %S" (cdr token))))))

(defun monk-cel--call (name arguments)
  "Return the CELL form for global function NAME(ARGUMENTS)."
  (pcase name
    ("has"
     (unless (and (= 1 (length arguments))
                  (memq (car-safe (car arguments)) '(fact var)))
       (monk-cel--error "has() takes one facts.KEY or vars.KEY argument"))
     (list 'has (car arguments)))
    ("size"
     (unless (= 1 (length arguments))
       (monk-cel--error "size() takes one argument"))
     (list 'size (car arguments)))
    (_ (monk-cel--error "Unknown function %s" name))))

;;;; CELL to CEL

(defconst monk-cell--to-cel-relations
  '((= . "==") (/= . "!=") (< . "<") (<= . "<=") (> . ">") (>= . ">=")
    (+ . "+") (- . "-") (* . "*") (/ . "/") (% . "%"))
  "CELL binary operators and their CEL spelling.")

(defun monk-cell--cel-string (s)
  "Return S as a single-quoted CEL string literal."
  (concat "'" (replace-regexp-in-string "['\\\\]" "\\\\\\&" s) "'"))

(defconst monk-cell--cel-precedence
  '((if . 1) (or . 2) (and . 3)
    (= . 4) (/= . 4) (< . 4) (<= . 4) (> . 4) (>= . 4) (in . 4)
    (+ . 5) (- . 5) (* . 6) (/ . 6) (% . 6) (not . 7))
  "CEL binding strength of each CELL operator.  Higher binds tighter.
Anything not listed is an atom or a call and never needs parentheses.")

(defun monk-cell--cel-precedence (form)
  "Return the precedence of FORM, or 8 for atoms and calls."
  (or (and (consp form) (alist-get (car form) monk-cell--cel-precedence)) 8))

(defun monk-cell--cel-child (form parent-precedence)
  "Print FORM as CEL, in parentheses when it binds weaker than PARENT-PRECEDENCE."
  (let ((text (monk-cell-to-cel form)))
    (if (< (monk-cell--cel-precedence form) parent-precedence)
        (concat "(" text ")")
      text)))

(defun monk-cell-to-cel (form)
  "Return CELL FORM printed as a CEL expression.
The output parses back with `monk-cel-parse' to an equal form.
Parentheses appear only where CEL precedence needs them."
  (let ((self (monk-cell--cel-precedence form)))
    (pcase form
      ((pred stringp) (monk-cell--cel-string form))
      ((pred numberp) (number-to-string form))
      ('t "true")
      ('nil "false")
      (`(class ,c) (format "%s in classes" (monk-cell--cel-string (symbol-name c))))
      (`(group ,g) (format "%s in groups" (monk-cell--cel-string (symbol-name g))))
      (`(fact ,k) (format "facts.%s" k))
      (`(var ,k) (format "vars.%s" k))
      (`(,(and single (or 'name 'local 'classes 'groups 'facts 'vars))) (symbol-name single))
      (`(has ,inner) (format "has(%s)" (monk-cell-to-cel inner)))
      (`(size ,inner) (format "size(%s)" (monk-cell-to-cel inner)))
      (`(not ,inner) (concat "!" (monk-cell--cel-child inner self)))
      (`(and . ,parts)
       (mapconcat (lambda (p) (monk-cell--cel-child p (1+ self))) parts " && "))
      (`(or . ,parts)
       (mapconcat (lambda (p) (monk-cell--cel-child p (1+ self))) parts " || "))
      (`(if ,c ,a ,b)
       (format "%s ? %s : %s"
               (monk-cell--cel-child c (1+ self))
               (monk-cell--cel-child a (1+ self))
               (monk-cell--cel-child b self)))
      (`(in ,item ,set)
       (format "%s in %s" (monk-cell--cel-child item (1+ self)) (monk-cell--cel-child set (1+ self))))
      (`(list . ,items) (format "[%s]" (mapconcat #'monk-cell-to-cel items ", ")))
      (`(,(and op (or 'starts-with 'ends-with 'contains 'match)) ,receiver ,argument)
       (format "%s.%s(%s)"
               (monk-cell--cel-child receiver 8)
               (car (rassq op monk-cel--methods))
               (monk-cell-to-cel argument)))
      (`(,(and op (pred (lambda (o) (assq o monk-cell--to-cel-relations)))) ,a ,b)
       ;; Left-associative: the right child needs parentheses at equal
       ;; precedence, the left child does not.
       (format "%s %s %s"
               (monk-cell--cel-child a self)
               (cdr (assq op monk-cell--to-cel-relations))
               (monk-cell--cel-child b (1+ self))))
      (_ (signal 'monk-guard-error (list (format "Cannot print %S as CEL" form)))))))

(provide 'monk-cell)
;;; monk-cell.el ends here
