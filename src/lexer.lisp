;;;; Lexer resource limits and shared representations: the parse-error and
;;;; parser-resource-error conditions, finite resource-limit parameters and
;;;; checks, symbol interning under those limits, and the %token/%parser
;;;; structs the tokenizer (lexer-tokenizer.lisp) builds on. Operator-lexeme
;;;; table computation lives in lexer-operator-lexemes.lisp.

(in-package #:cl-prolog-kit)

(progn
  (define-condition prolog-parse-error (error)
    ((description :initarg :description :reader prolog-parse-error-description))
    (:report (lambda (condition stream)
               (write-string (prolog-parse-error-description condition) stream)))
    (:documentation "A syntax error detected while reading Prolog source text."))

  (define-condition prolog-parser-resource-error (error)
    ((resource :initarg :resource
               :reader prolog-parser-resource-error-resource)
     (limit :initarg :limit
            :reader prolog-parser-resource-error-limit)
     (observed :initarg :observed
               :reader prolog-parser-resource-error-observed)
     (position :initarg :position
               :reader prolog-parser-resource-error-position))
    (:report
     (lambda (condition stream)
       (format stream
               "Prolog parser resource ~A exceeded limit ~D at position ~D (observed ~D)."
               (prolog-parser-resource-error-resource condition)
               (prolog-parser-resource-error-limit condition)
               (prolog-parser-resource-error-position condition)
               (prolog-parser-resource-error-observed condition))))
    (:documentation
     "A finite parser resource limit was exceeded while reading Prolog text."))

  (defparameter *max-prolog-source-characters* 1048576)
  (defparameter *max-prolog-delimiter-depth* 256)
  (defparameter *max-prolog-parser-depth* 256)
  (defparameter *max-prolog-tokens* 65536)
  (defparameter *max-prolog-identifier-length* 1024)
  (defparameter *max-prolog-quoted-lexeme-length* 65536)
  (defparameter *max-prolog-numeric-lexeme-length* 4096)
  (defparameter *max-prolog-interned-symbols* 65536)

  (defvar *parser-interned-symbols* (make-hash-table :test #'equal))

  (defun %parser-resource-error (resource limit observed position)
    (error 'prolog-parser-resource-error
           :resource resource
           :limit limit
           :observed observed
           :position position))

  (defun %check-parser-limit (resource limit observed position)
    (when (and limit (> observed limit))
      (%parser-resource-error resource limit observed position)))

  (defun %reserve-parser-symbol (name package position)
    (when *max-prolog-interned-symbols*
      (let ((key (cons (package-name package) name)))
        (unless (gethash key *parser-interned-symbols*)
          (%check-parser-limit "INTERNED_SYMBOLS"
                               *max-prolog-interned-symbols*
                               (1+ (hash-table-count
                                    *parser-interned-symbols*))
                               position)
          (setf (gethash key *parser-interned-symbols*) t)))))

  (defun %intern-parser-symbol (name package &optional (position 0))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (if status
          symbol
          (progn
            (%reserve-parser-symbol name package position)
            (intern name package))))))

(defun %parse-error (control &rest arguments)
  (error 'prolog-parse-error
         :description (apply #'format nil control arguments)))

(defun %read-prolog-term-source (stream)
  "Read through one top-level term terminator without consuming the next term."
  (let ((delimiters '())
        (state :code)
        (previous nil)
        (position 0))
    (labels ((record-character (character out)
               (incf position)
               (%check-parser-limit "SOURCE_CHARACTERS"
                                    *max-prolog-source-characters*
                                    position
                                    position)
               (write-char character out))
             (expected-closer (character)
               (cdr (assoc character
                           '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))
                           :test #'char=)))
             (open-delimiter (character)
               (%check-parser-limit "DELIMITER_DEPTH"
                                    *max-prolog-delimiter-depth*
                                    (1+ (length delimiters))
                                    position)
               (push (expected-closer character) delimiters))
             (close-delimiter (character)
               (unless delimiters
                 (%parse-error
                  "Unexpected closing Prolog delimiter ~C at source position ~D."
                  character position))
               (unless (char= character (first delimiters))
                 (%parse-error
                  "Mismatched Prolog delimiter ~C at source position ~D; expected ~C."
                  character position (first delimiters)))
               (pop delimiters)))
      (with-output-to-string (out)
        (loop for character = (read-char stream nil nil)
              do (unless character
                   (unless (member state '(:code :line-comment))
                     (%parse-error
                      "Unexpected end of Prolog input while reading ~A."
                      state))
                   (when delimiters
                     (%parse-error
                      "Unexpected end of Prolog input; expected closing delimiter ~C."
                      (first delimiters)))
                   (return))
                 (record-character character out)
                 (ecase state
                   (:code
                    (cond
                      ;; `0'c' is a character-code constant (ISO 6.4.4), not the
                      ;; start of a quoted atom, so its `'' must not put the
                      ;; splitter into :QUOTED and swallow the rest of the term.
                      ((and (char= character #\') (eql previous #\0))
                       (let ((next (read-char stream nil nil)))
                         (when next
                           (record-character next out)
                           (cond
                             ((char= next #\\)
                              (let ((escaped (read-char stream nil nil)))
                                (when escaped (record-character escaped out))))
                             ;; `0''' spells the quote character itself.
                             ((and (char= next #\')
                                   (eql (peek-char nil stream nil nil) #\'))
                              (record-character (read-char stream) out)))
                           (setf character next))))
                      ((char= character #\') (setf state :quoted))
                      ((char= character #\") (setf state :dquoted))
                      ((char= character #\%) (setf state :line-comment))
                      ((and (char= character #\/)
                            (eql (peek-char nil stream nil nil) #\*))
                       (record-character (read-char stream) out)
                       (setf state :block-comment))
                      ((member character '(#\( #\[ #\{))
                       (open-delimiter character))
                      ((member character '(#\) #\] #\}))
                       (close-delimiter character))
                      ((and (null delimiters)
                            (char= character #\.)
                            (not (and previous
                                      (digit-char-p previous)
                                      (let ((next
                                              (peek-char nil stream nil nil)))
                                        (and next (digit-char-p next))))))
                       (return))))
                   (:quoted
                    (cond
                      ((char= character #\\)
                       (setf state :quoted-escape))
                      ((char= character #\')
                       (if (eql (peek-char nil stream nil nil) #\')
                           (progn
                             (record-character (read-char stream) out)
                             (setf previous #\'))
                           (setf state :code)))))
                   (:quoted-escape (setf state :quoted))
                   (:dquoted
                    (cond
                      ((char= character #\\)
                       (setf state :dquoted-escape))
                      ((char= character #\")
                       (if (eql (peek-char nil stream nil nil) #\")
                           (progn
                             (record-character (read-char stream) out)
                             (setf previous #\"))
                           (setf state :code)))))
                   (:dquoted-escape (setf state :dquoted))
                   (:line-comment
                    (when (char= character #\Newline)
                      (setf state :code)))
                   (:block-comment
                    (when (and (char= character #\*)
                               (eql (peek-char nil stream nil nil) #\/))
                      (record-character (read-char stream) out)
                      (setf state :code))))
                 (setf previous character))))))

(defun %prolog-source-string (source)
  (etypecase source
    (string
      (%check-parser-limit
        "SOURCE_CHARACTERS"
        *max-prolog-source-characters*
        (length source)
        (length source))
      source)
    (stream (%read-prolog-term-source source))))

(defstruct (%token (:constructor %token (kind &optional value position)))
  kind
  value
  (position 0))

(defstruct (%parser (:constructor %parser (tokens operator-table)))
  tokens
  operator-table
  (position 0)
  (depth 0))

(defvar *parsing-dcg-body-p* nil)

(defvar *active-char-conversions* nil
  "Hash table of char_conversion/2 mappings applied while tokenizing.

NIL disables conversion.  Callers with a rulebase bind this around parsing
when the char_conversion flag is on; quoted tokens are never converted, as
ISO requires.")
