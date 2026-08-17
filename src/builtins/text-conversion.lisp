;;;; Shared text-conversion primitives for the atom/character/number
;;;; builtins in atom-ops.lisp and atom-number-conversion.lisp: resource
;;;; limits, character/code list conversion, and number-text parsing.

(in-package #:cl-prolog-kit)

(progn
  (defparameter *max-prolog-derived-atom-candidates*
    *max-prolog-numeric-lexeme-length*
    "Maximum number of atom candidates a single relational builtin may derive.")

  (defun %check-resource-limit
      (actual limit resource environment operation message)
    (when (> actual limit)
      (%raise-resource-error resource environment operation message))
    actual)

  (defun %check-text-resource-limit
      (text limit resource environment operation message)
    (%check-resource-limit (length text) limit resource environment operation message)
    text)

  (defun %check-atom-text-limit (atom environment operation)
    (%check-resource-limit
     (%atom-text-length atom) *max-prolog-quoted-lexeme-length* "ATOM_LENGTH"
     environment operation "atom text exceeds the configured length limit")
    (%atom-text atom))

  (defun %integer-within-decimal-digit-limit-p (integer digit-limit)
    (let* ((magnitude (abs integer))
           (bit-limit (ceiling (* digit-limit 3322) 1000))
           (bits (integer-length magnitude)))
      (or (zerop magnitude)
          (< bits bit-limit)
          (and (= bits bit-limit)
               (< magnitude (expt 10 digit-limit))))))

  (defun %check-integer-text-limit (integer environment operation)
    (let ((digit-limit (- *max-prolog-numeric-lexeme-length*
                          (if (minusp integer) 1 0))))
      (unless (and (plusp digit-limit)
                   (%integer-within-decimal-digit-limit-p integer digit-limit))
        (%raise-resource-error
         "INTEGER_SIZE" environment operation
         "integer decimal representation exceeds the configured length limit")))
    integer))

(defun %text-atom (text &optional environment (operation (%iso-atom "ATOM")))
  (%check-text-resource-limit
   text *max-prolog-quoted-lexeme-length* "ATOM_LENGTH" environment operation
   "atom text exceeds the configured length limit")
  (%intern-prolog-atom text))

(defun %character-atom-p (term)
  (and (%term-atom-p term) (= 1 (%atom-text-length term))))

(define-term-guard %ensure-atom-value (value argument)
  :instantiation (format nil "~A must be instantiated" argument)
  :accept (%term-atom-p value)
  :type "ATOM"
  :type-message (format nil "~A must be an atom" argument))

(defun %ensure-proper-instantiated-list
    (value environment operation argument
     &key element-checker
          (limit *max-prolog-quoted-lexeme-length*)
          (resource "LIST_LENGTH"))
  (let ((visited (make-hash-table :test #'eq))
        (tail value)
        (count 0))
    (loop
      (cond
        ((null tail) (return count))
        ((logic-var-p tail)
         (%raise-instantiation-error
          environment operation (format nil "~A must be instantiated" argument)))
        ((not (consp tail))
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a proper list" argument)))
        ((gethash tail visited)
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a finite proper list" argument)))
        (t
         (setf (gethash tail visited) t)
         (when (logic-var-p (car tail))
           (%raise-instantiation-error
            environment operation (format nil "~A must be instantiated" argument)))
         (when element-checker
           (funcall element-checker (car tail)))
         (incf count)
         (%check-resource-limit
          count limit resource environment operation
          "list exceeds the configured length limit")
         (setf tail (cdr tail)))))))

(defun %atom-character-list
    (atom environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "ATOM_LENGTH"))
  (let ((text (%atom-text atom)))
    (%check-text-resource-limit
     text limit resource environment operation
     "atom text exceeds the configured length limit")
    (map 'list (lambda (character)
                 (%text-atom (string character) environment operation))
         text)))

(defun %atom-code-list
    (atom environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "ATOM_LENGTH"))
  (let ((text (%atom-text atom)))
    (%check-text-resource-limit
     text limit resource environment operation
     "atom text exceeds the configured length limit")
    (map 'list #'char-code text)))

(defun %character-list-text
    (characters environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "LIST_LENGTH"))
  (let* ((count
           (%ensure-proper-instantiated-list
            characters environment operation "character list"
            :limit limit
            :resource resource
            :element-checker
            (lambda (character)
              (unless (%character-atom-p character)
                (%raise-type-error
                 "CHARACTER" character environment operation
                 "atom_chars/2 and number_chars/2 require character atoms")))))
         (text (make-string count)))
    (loop for character in characters
          for index from 0
          do (setf (char text index) (char (%atom-text character) 0)))
    text))

(defun %code-character (code environment operation)
  (unless (integerp code)
    (%raise-type-error "INTEGER" code environment operation
                       "character codes must be integers"))
  ;; ISO 13211-1 8.16.6.3: an integer that is no character code is a
  ;; representation_error(character_code), not a domain error -- the value is
  ;; meaningful, it just has no character to name.
  (unless (and (<= 0 code) (< code char-code-limit))
    (%raise-representation-error "CHARACTER_CODE" environment operation
                                 "integer is not a character code"))
  (code-char code))

(defun %code-list-text
    (codes environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "LIST_LENGTH"))
  (let* ((count
           (%ensure-proper-instantiated-list
            codes environment operation "code list"
            :limit limit
            :resource resource
            :element-checker
            (lambda (code) (%code-character code environment operation))))
         (text (make-string count)))
    (loop for code in codes
          for index from 0
          do (setf (char text index)
                   (%code-character code environment operation)))
    text))

(defun %number-text (number environment operation)
  (cond
    ((integerp number)
     (%check-integer-text-limit number environment operation)
     (write-to-string number :base 10 :radix nil :readably t))
    ((floatp number)
     (let ((text (write-to-string number :base 10 :radix nil :readably t)))
       (%check-text-resource-limit
        text *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH"
        environment operation
        "numeric text exceeds the configured length limit")
       (map-into text
                 (lambda (character)
                   (if (find character "sSfFdDlL") #\e character))
                 text)
       ;; Reject implementation-specific non-finite and reader forms.
       (%text-number text environment operation)
       text))
    ((realp number)
     (%raise-domain-error "PROLOG_NUMBER" number environment operation
                          "ratios are not Prolog numeric terms"))
    (t
     (%raise-type-error "NUMBER" number environment operation
                        "first argument must be a Prolog integer or float"))))

(defun %read-number-token (text environment operation)
  "Read TEXT as one Prolog number token, per ISO 13211-1 8.16.7/8.16.8.

The standard says the characters are read as a *number token*, which is the
reader's own grammar: leading layout, a sign, and the `0'c'/`0x'/`0o'/`0b'
notations all belong to it.  Delegating to the reader is what keeps this
agreeing with what the same text means in source."
  ;; The exponent-magnitude bound is this engine's own guard against a short
  ;; text that would demand an enormous float; it has to run before the reader
  ;; sees the text, since the reader would simply fail on it.
  (let ((exponent (position-if (lambda (character)
                                 (member character '(#\e #\E) :test #'char=))
                               text)))
    (when exponent
      (let ((digits (count-if #'digit-char-p text :start exponent)))
        (when (> digits (length (princ-to-string
                                 *max-prolog-arithmetic-exponent-magnitude*)))
          (%raise-resource-error
           "EXPONENT_MAGNITUDE" environment operation
           "numeric exponent exceeds the configured magnitude limit"))
        (let ((value (ignore-errors
                      (parse-integer text :start (1+ exponent) :junk-allowed t))))
          (when (and value
                     (> (abs value) *max-prolog-arithmetic-exponent-magnitude*))
            (%raise-resource-error
             "EXPONENT_MAGNITUDE" environment operation
             "numeric exponent exceeds the configured magnitude limit"))))))
  ;; A number token ends where the number ends: ISO 13211-1 8.16.7 admits
  ;; *leading* layout before it but nothing after, so `'3 '' is a syntax error
  ;; even though the tokenizer would happily skip the trailing space.
  (when (and (plusp (length text))
             (member (char text (1- (length text)))
                     '(#\Space #\Tab #\Return #\Newline)
                     :test #'char=))
    (%raise-syntax-error-for text environment operation))
  (let ((tokens (handler-case (%tokenize-prolog text)
                  (prolog-parser-resource-error (condition)
                    (%raise-parser-resource-error condition environment operation))
                  (error ()
                    (%raise-syntax-error-for text environment operation)))))
    ;; Exactly one number token, optionally signed, and nothing else.  Reading
    ;; the text as a *term* instead would also accept `1.', which is a number
    ;; followed by an end token rather than a number token.
    (let* ((index 0)
           (sign (let ((token (aref tokens 0)))
                   (when (and (eq :operator (%token-kind token))
                              (member (%token-value token) '("+" "-")
                                      :test #'string=))
                     (incf index)
                     (if (string= (%token-value token) "-") -1 1)))))
      (let ((number (aref tokens index))
            (end (aref tokens (min (1+ index) (1- (length tokens))))))
        (unless (and (eq :number (%token-kind number))
                     (eq :eof (%token-kind end)))
          (%raise-syntax-error-for text environment operation))
        (if (eql sign -1) (- (%token-value number)) (%token-value number))))))

(defun %text-number (text environment operation)
  (%check-text-resource-limit
   text *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH"
   environment operation "numeric text exceeds the configured length limit")
  (%read-number-token text environment operation))

(defun %text-of (value environment operation
                 &key (accept :any)
                      (instantiation "text argument must be instantiated")
                      (type "ATOM")
                      (type-message
                       "expected an atom, string, number, or char/code list"))
  "Return the text (a CL string) of an atomic-or-text VALUE.

ACCEPT selects the admissible shapes beyond a string or atom: :ATOMIC also
accepts a number, :TEXT also accepts a code or character list, and :ANY (the
default) accepts both.  A NIL INSTANTIATION lets an unbound VALUE fall through
to the TYPE-MESSAGE type_error instead of raising instantiation_error.  Shared
by the string builtins, the text-accepting conversions (atom_string,
number_string, term_string, ...), format/1,2,3 and the case-folding builtins."
  (let ((numbers (member accept '(:atomic :any)))
        (lists (member accept '(:text :any))))
    (cond
      ((logic-var-p value)
       (if instantiation
           (%raise-instantiation-error environment operation instantiation)
           (%raise-type-error type value environment operation type-message)))
      ((stringp value) value)
      ((%term-atom-p value) (%atom-text value))
      ((and numbers (or (integerp value) (floatp value)))
       (%number-text value environment operation))
      ((and lists (consp value))
       (if (every #'integerp value)
           (%code-list-text value environment operation)
           (%character-list-text value environment operation)))
      (t (%raise-type-error type value environment operation type-message)))))
