;;;; Canonical, operator-aware Prolog term rendering.

(in-package #:cl-prolog-kit)

(defconstant +compound-argument-priority+ 999)

(defvar *write-seen* nil
  "When bound to an EQ hash-table, the term writer marks each cons on the
current path and emits `...' on revisiting one, so a cyclic term (which the
occurs_check=false flag lets a user build) prints in bounded time instead of
recursing forever.  Path-scoped (marked on descent, unmarked on return), so a
shared but acyclic subterm still prints in full.")

(declaim (ftype function %write-prolog-term))

(defun %writer-operator-name (functor)
  (case functor
    (and '|,|)
    (or '|;|)
    (not '|\\+|)
    (otherwise functor)))

(defun %writer-operator-definition (functor arity)
  (let ((specifiers (ecase arity
                      (1 '(:fx :fy))
                      (2 '(:xfx :xfy :yfx)))))
    (find-if (lambda (definition)
               (member (operator-definition-specifier definition)
                       specifiers :test #'eq))
             (%operator-table-find *standard-operator-table*
                                   (%writer-operator-name functor)))))

(defun %write-prolog-variable (variable stream)
  (let ((name (symbol-name variable)))
    (write-string (if (> (length name) 1) (subseq name 1) "_") stream)))

(defun %write-quoted-prolog-atom (name stream)
  (write-char #\' stream)
  (loop for character across name
        do (when (member character '(#\' #\\) :test #'char=)
             (write-char character stream))
           (write-char character stream))
  (write-char #\' stream))

(defun %write-prolog-string (string stream quotedp)
  "Write a Prolog string: raw characters for write/print, or a `\"...\"'
literal with \" and \\ escaped for writeq/write_canonical."
  (if quotedp
      (progn
        (write-char #\" stream)
        (loop for character across string
              do (when (member character '(#\" #\\) :test #'char=)
                   (write-char #\\ stream))
                 (write-char character stream))
        (write-char #\" stream))
      (write-string string stream)))

(defun %unquoted-prolog-atom-name-p (name)
  "True when NAME reads back as this atom without quotes, per ISO 13211-1 6.4.2.

Besides a plain atom name, that covers a graphic token -- a non-empty run of
graphic characters, so `+', `=..' and `\\+' print bare -- the solo chars `!' and
`;', and the bracket pairs `[]' and `{}'.  Deliberately excluded: `,' and `|',
which would be read back as separators, and a lone `.', which ISO 6.4.8 makes
the end token when layout or end of input follows it, so it has no bare reading
at all (a longer run such as `..' does)."
  (or (%plain-prolog-atom-name-p name)
      (string= name "!")
      (string= name ";")
      (string= name "[]")
      (string= name "{}")
      (and (plusp (length name))
           (not (string= name "."))
           (every #'%prolog-graphic-character-p name))))

(defun %write-prolog-atom (atom stream quotedp)
  ;; %ATOM-TEXT, not SYMBOL-NAME: an atom interned verbatim carries its text
  ;; as-is, so downcasing here would print `'FooBar'' as `foobar' -- and the
  ;; result would then read back as a different atom.
  (let ((name (%atom-text atom)))
    (cond
      ((%unquoted-prolog-atom-name-p name) (write-string name stream))
      (quotedp (%write-quoted-prolog-atom name stream))
      (t (write-string name stream)))))

(defparameter +numbervars-functor+ (%intern-prolog-atom "$VAR")
  "The ISO 8.14.2 numbervars functor: the atom whose text is `$VAR'.  Its text
is upper case, so it is a verbatim atom and a different atom from `$var'.")

(defun %numbered-variable-index (term)
  ;; Compared on text rather than symbol name, so the distinct lower-case atom
  ;; `'$var'(0)' is written as itself instead of as a numbered variable.
  (when (and (consp term)
             (symbolp (first term))
             (%same-atom-text-p (first term) +numbervars-functor+)
             (consp (rest term))
             (null (cddr term))
             (typep (second term) '(integer 0)))
    (second term)))

(defun %write-numbered-variable (index stream)
  (multiple-value-bind (suffix letter) (floor index 26)
    (write-char (code-char (+ (char-code #\A) letter)) stream)
    (unless (zerop suffix)
      (princ suffix stream))))

(defun %write-prolog-number (number stream)
  (etypecase number
    (integer (princ number stream))
    (float
     (let ((representation (string-downcase (write-to-string number))))
       (loop for character across representation
             do (write-char (if (find character "dfsle" :test #'char=)
                                #\e
                                character)
                            stream))))))

(defun %write-prolog-list (term stream quotedp numbervarsp ignore-opsp)
  (write-char #\[ stream)
  ;; Mark each spine cons in *WRITE-SEEN* (path-scoped) so a cyclic list tail
  ;; such as X = [a|X] emits `|...]' instead of looping forever.  The head cons
  ;; is already marked by %WRITE-PROLOG-TERM; the ones we add here are removed
  ;; on return to keep an acyclic shared tail printable in full.
  (let ((marked '()))
    (unwind-protect
         (loop with tail = term
               with firstp = t
               do (cond
                    ((null tail) (return))
                    ((not (consp tail))
                     (write-char #\| stream)
                     (%write-prolog-term tail stream +compound-argument-priority+
                                         quotedp numbervarsp ignore-opsp)
                     (return))
                    ((and (not firstp) *write-seen* (gethash tail *write-seen*))
                     (write-string "|..." stream)
                     (return))
                    (t
                     (unless firstp (write-char #\, stream))
                     (when (and *write-seen* (not firstp))
                       (setf (gethash tail *write-seen*) t)
                       (push tail marked))
                     (%write-prolog-term (car tail) stream
                                         +compound-argument-priority+
                                         quotedp numbervarsp ignore-opsp)
                     (setf firstp nil
                           tail (cdr tail)))))
      (dolist (cell marked) (remhash cell *write-seen*))))
  (write-char #\] stream))

(defun %write-prolog-prefix-operator
    (term definition stream context-priority quotedp numbervarsp ignore-opsp)
  (let* ((priority (operator-definition-priority definition))
         (parenthesize (> priority context-priority))
         (argument-priority (if (eq :fx (operator-definition-specifier definition))
                                (1- priority)
                                priority)))
    (when parenthesize (write-char #\( stream))
    (write-string (%operator-lexeme definition) stream)
    (write-char #\Space stream)
    (%write-prolog-term (second term) stream argument-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-binary-operator
    (term definition stream context-priority quotedp numbervarsp ignore-opsp)
  (let* ((priority (operator-definition-priority definition))
         (specifier (operator-definition-specifier definition))
         (parenthesize (> priority context-priority))
         (left-priority (if (eq specifier :yfx) priority (1- priority)))
         (right-priority (if (eq specifier :xfy) priority (1- priority))))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream left-priority
                        quotedp numbervarsp ignore-opsp)
    (format stream " ~A " (%operator-lexeme definition))
    (%write-prolog-term (third term) stream right-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-conditional
    (term stream context-priority softp quotedp numbervarsp ignore-opsp)
  "Render (SOFT-)IF-THEN-ELSE at the ISO priorities of the -> / *-> / ; xfy
operators it stands for: `;' at 1100, `->'/`*->' at 1050, and the condition
argument one below that so a bare `->'/`*->' on the left needs no parens."
  (let* ((semicolon-priority 1100)
         (arrow-priority 1050)
         (condition-priority (1- arrow-priority))
         (parenthesize (> semicolon-priority context-priority)))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream condition-priority
                        quotedp numbervarsp ignore-opsp)
    (write-string (if softp " *-> " " -> ") stream)
    (%write-prolog-term (third term) stream arrow-priority
                        quotedp numbervarsp ignore-opsp)
    (write-string " ; " stream)
    (%write-prolog-term (fourth term) stream semicolon-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-functor (atom stream quotedp)
  "Write ATOM as a compound term's functor, quoting anything but a plain atom
name.

Stricter than %WRITE-PROLOG-ATOM on purpose: this reader does not yet apply ISO
6.3.3's rule that a name is a functor only when `(' follows it with no layout
between, so it reads a bare `+(1,2)' as the prefix operator `+' applied to
`(1,2)'.  Emitting `'+'(1,2)' keeps write_canonical/1 output re-readable, which
ISO 8.14.2 requires of it."
  (let ((name (%atom-text atom)))
    (cond
      ((%plain-prolog-atom-name-p name) (write-string name stream))
      (quotedp (%write-quoted-prolog-atom name stream))
      (t (write-string name stream)))))

(defun %write-prolog-compound (term stream quotedp numbervarsp ignore-opsp)
  (%write-prolog-functor (first term) stream quotedp)
  (write-char #\( stream)
  (loop for argument in (rest term)
        for firstp = t then nil
        do (unless firstp (write-char #\, stream))
           (%write-prolog-term argument stream +compound-argument-priority+
                               quotedp numbervarsp ignore-opsp))
  (write-char #\) stream))

(defun %write-prolog-cons
    (term stream context-priority quotedp numbervarsp ignore-opsp)
  "Write a cons (compound / list / operator / numbervar) TERM.  Operator and
compound dispatch (which need the term's arity) is gated on %PROPER-LIST-P so a
cyclic or partial list cannot hang `length'; such terms fall through to the
cycle-safe list writer."
  (cond
    ((and numbervarsp (%numbered-variable-index term))
     (%write-numbered-variable (%numbered-variable-index term) stream))
    (t
     (let ((properp (%proper-list-p term)))
       (cond
         ((and properp (not ignore-opsp)
               (member (first term) '(if-then-else soft-if-then-else) :test #'eq)
               (= (length term) 4))
          (%write-prolog-conditional term stream context-priority
                                     (eq (first term) 'soft-if-then-else)
                                     quotedp numbervarsp ignore-opsp))
         ((and properp (not ignore-opsp) (symbolp (first term))
               (= (length term) 2))
          (let ((definition (%writer-operator-definition (first term) 1)))
            (if definition
                (%write-prolog-prefix-operator term definition stream
                                               context-priority
                                               quotedp numbervarsp ignore-opsp)
                (%write-prolog-compound term stream quotedp numbervarsp
                                        ignore-opsp))))
         ((and properp (not ignore-opsp) (symbolp (first term))
               (= (length term) 3))
          (let ((definition (%writer-operator-definition (first term) 2)))
            (if definition
                (%write-prolog-binary-operator term definition stream
                                               context-priority
                                               quotedp numbervarsp ignore-opsp)
                (%write-prolog-compound term stream quotedp numbervarsp
                                        ignore-opsp))))
         ((and properp (symbolp (first term)))
          (%write-prolog-compound term stream quotedp numbervarsp ignore-opsp))
         (t (%write-prolog-list term stream quotedp numbervarsp ignore-opsp)))))))

(defun %write-prolog-term
    (term stream context-priority quotedp numbervarsp ignore-opsp)
  (cond
    ((null term) (write-string "[]" stream))
    ((logic-var-p term) (%write-prolog-variable term stream))
    ((numberp term) (%write-prolog-number term stream))
    ((symbolp term) (%write-prolog-atom term stream quotedp))
    ((stringp term) (%write-prolog-string term stream quotedp))
    ((atom term) (error "Cannot write non-Prolog atomic value ~S." term))
    ((and *write-seen* (gethash term *write-seen*))
     ;; Cyclic revisit on the current path: stop rather than loop forever.
     (write-string "..." stream))
    (t
     (when *write-seen* (setf (gethash term *write-seen*) t))
     (unwind-protect
          (%write-prolog-cons term stream context-priority
                              quotedp numbervarsp ignore-opsp)
       (when *write-seen* (remhash term *write-seen*))))))

(defun %write-prolog-term-with-options
    (term stream &key (quoted t) (numbervars nil) (ignore-ops nil))
  ;; Only a cons can carry a cycle, so allocate the seen-set lazily -- writing
  ;; an atom/number/string (the common streamed-output case) allocates nothing.
  (let ((*write-seen* (if (consp term)
                          (make-hash-table :test #'eq)
                          *write-seen*)))
    (%write-prolog-term term stream +maximum-operator-priority+
                        quoted numbervars ignore-ops)))

(defun write-prolog-term (term &optional (stream *standard-output*))
  "Write TERM to STREAM in canonical, parseable Prolog syntax and return TERM."
  (%write-prolog-term-with-options term stream)
  term)

(defun prolog-term-string (term)
  "Return the canonical, parseable Prolog representation of TERM."
  (with-output-to-string (stream)
    (write-prolog-term term stream)))
