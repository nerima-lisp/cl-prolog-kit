;;;; ISO stream read/write builtins.

(in-package #:cl-prolog-kit)

(defun %io-read-term-values (entry operator-table mode environment operation)
  (let ((input (prolog-stream-stream entry)))
    (if (eq (peek-char t input nil :eof) :eof)
        (progn
          (setf (prolog-stream-end-of-stream entry) :past)
          (values (%iso-atom "end_of_file") '() '() '() t))
        (handler-case
            (multiple-value-bind (term variables names singletons)
                (%io-parse-term-with-variables input operator-table)
              (setf (prolog-stream-end-of-stream entry) :not)
              (values term variables names singletons t))
          (prolog-parser-resource-error (condition)
            (%raise-parser-resource-error condition environment operation))
          (prolog-parse-error (condition)
            (if (%io-option-name-p mode "error")
                (%raise-syntax-error condition environment operation)
                (values nil nil nil nil nil)))))))

(defun %io-unify-read-results (term value options variables names singletons
                               environment emit)
  (multiple-value-bind (term-environment term-ok)
      (unify term value environment)
    (when term-ok
      (multiple-value-bind (variables-environment variables-ok)
          (unify (%io-option "variables" options variables)
                 variables term-environment)
        (when variables-ok
          (multiple-value-bind (names-environment names-ok)
              (unify (%io-option "variable_names" options names)
                     names variables-environment)
            (when names-ok
              (multiple-value-bind (singletons-environment singletons-ok)
                  (unify (%io-option "singletons" options singletons)
                         singletons names-environment)
                (when singletons-ok (funcall emit singletons-environment))))))))))

(defun %io-read-term-goal (rulebase entry term options environment emit
                           &optional (operation (%io-operation "READ_TERM")))
  (let* ((parsed (%io-read-options options environment operation))
         (mode (%io-syntax-errors-mode parsed environment operation))
         (*active-char-conversions* (%rulebase-active-char-conversions rulebase))
         (*double-quotes* (%double-quotes-mode rulebase)))
    (multiple-value-bind (value variables names singletons readablep)
        (%io-read-term-values entry (rulebase-operator-table rulebase) mode
                              environment operation)
      (when readablep
        (%io-unify-read-results term value parsed variables names singletons
                                environment emit)))))

(%define-io-dual-builtin (read_term (term options) :input "READ_TERM")
    (rulebase environment depth emit)
  (%io-read-term-goal rulebase entry term options environment emit))

(%define-io-dual-builtin (read (term) :input "READ")
    (rulebase environment depth emit)
  (%io-read-term-goal rulebase entry term '() environment emit operation))

(defun %io-write-term-goal (entry term options environment emit
                            &optional (operation (%io-operation "WRITE_TERM")))
  (let* (
         (parsed (%io-write-options options environment operation
                                    '("quoted" "ignore_ops" "numbervars")))
         (ignore-ops (%io-boolean
                      (%io-option "ignore_ops" parsed (%iso-atom "false"))
                      environment operation))
         (quoted (%io-boolean (%io-option "quoted" parsed (%iso-atom "false"))
                              environment operation))
         (numbervars
           (%io-boolean (%io-option "numbervars" parsed (%iso-atom "false"))
                        environment operation)))
    (let ((value (logic-substitute term environment))
          (stream (prolog-stream-stream entry)))
      (%write-prolog-term-with-options value stream
                                       :quoted quoted
                                       :numbervars numbervars
                                       :ignore-ops ignore-ops))
    (funcall emit environment)))

(%define-io-dual-builtin (write_term (term options) :output "WRITE_TERM")
    (rulebase environment depth emit)
  (%io-write-term-goal entry term options environment emit))

(defun %io-write-facade-goal (entry term quoted environment emit operation)
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom (if quoted "true" "false")))
         (list (%iso-atom "numbervars") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (write (term) :output "WRITE")
    (rulebase environment depth emit)
  (%io-write-facade-goal entry term nil environment emit operation))

(%define-io-dual-builtin (writeq (term) :output "WRITEQ")
    (rulebase environment depth emit)
  (%io-write-facade-goal entry term t environment emit operation))

(defun %io-write-canonical-goal (entry term environment emit operation)
  ;; ISO write_canonical/1,2: quoted, operator-free, no numbervars, so the
  ;; output reads back as the same term under any operator table.
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom "true"))
         (list (%iso-atom "ignore_ops") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (write_canonical (term) :output "WRITE_CANONICAL")
    (rulebase environment depth emit)
  (%io-write-canonical-goal entry term environment emit operation))

(defun %io-newline-goal (entry environment emit)
  (terpri (prolog-stream-stream entry))
  (funcall emit environment))

(%define-io-dual-builtin (nl () :output "NL")
    (rulebase environment depth emit)
  (%io-newline-goal entry environment emit))

(%define-io-dual-builtin (flush_output () :output "FLUSH_OUTPUT")
    (rulebase environment depth emit)
  (%io-flush-goal entry environment emit))

(defun %io-flush-goal (entry environment emit)
  (finish-output (prolog-stream-stream entry))
  (funcall emit environment))

(defun %io-require-stream-type (entry type environment operation)
  ;; ISO 13211-1 8.12/8.13 name the stream's *actual* type as the culprit -- a
  ;; byte read from a text stream is permission_error(input, text_stream, S) --
  ;; because that is the property that makes the operation illegal.
  (unless (eq (prolog-stream-type entry) type)
    (%raise-permission-error
     (if (eq (prolog-stream-direction entry) :input) "INPUT" "OUTPUT")
     (if (eq (prolog-stream-type entry) :text) "TEXT_STREAM" "BINARY_STREAM")
     (%io-public-designator entry) environment operation
     (if (eq type :text)
         "Character operation requires a text stream"
         "Byte operation requires a binary stream")))
  entry)

(defun %io-read-character (entry environment operation &key peek)
  "Read (or, if PEEK, look ahead at) ENTRY's next character, updating its
end-of-stream property the way every ISO character/code input predicate
must. Returns the character, or NIL at end of stream."
  (%io-require-stream-type entry :text environment operation)
  (let ((character (if peek
                       (peek-char nil (prolog-stream-stream entry) nil nil)
                       (read-char (prolog-stream-stream entry) nil nil))))
    (setf (prolog-stream-end-of-stream entry)
          (cond (character :not)
                (peek :at)
                (t :past)))
    character))

(defun %io-require-in-character (term environment operation)
  "Check get_char/peek_char's output argument per ISO 13211-1 8.12.1.3.

A bound argument that is neither a one-character atom nor `end_of_file' is a
type_error(in_character, C); leaving it to unification would fail silently."
  (let ((value (logic-substitute term environment)))
    (unless (or (logic-var-p value)
                (%character-atom-p value)
                (and (%term-atom-p value)
                     (string= "end_of_file" (%atom-text value))))
      (%raise-type-error "IN_CHARACTER" value environment operation
                         "expected a character or end_of_file"))))

(defun %io-character-input (entry environment operation &key peek)
  (let ((character (%io-read-character entry environment operation :peek peek)))
    (if character (%character-atom character) (%iso-atom "end_of_file"))))

(%define-io-dual-builtin (get_char (character) :input "GET_CHAR")
    (rulebase environment depth emit)
  (%io-require-in-character character environment operation)
  (%unify-emit character (%io-character-input entry environment operation)
               environment emit))

(%define-io-dual-builtin (peek_char (character) :input "PEEK_CHAR")
    (rulebase environment depth emit)
  (%io-require-in-character character environment operation)
  (%unify-emit character
               (%io-character-input entry environment operation :peek t)
               environment emit))

(defun %io-write-text-unit (entry term environment operation converter)
  "Write TERM's single text unit (a character or character-code atom, per
CONVERTER) to ENTRY, requiring a text stream. Shared by PUT_CHAR
(CONVERTER = %IO-CHARACTER) and PUT_CODE (CONVERTER = %IO-CODE-CHARACTER,
src/builtins/io-code.lisp)."
  (%io-require-stream-type entry :text environment operation)
  (write-char (funcall converter term environment operation)
              (prolog-stream-stream entry)))

(defun %io-write-character (entry term environment operation)
  (%io-write-text-unit entry term environment operation #'%io-character))

(defun %io-read-byte (entry environment operation &key peek)
  (%io-require-stream-type entry :binary environment operation)
  (let* ((stream (prolog-stream-stream entry))
         (position (and peek (ignore-errors (file-position stream)))))
    (when (and peek (not (integerp position)))
      (%raise-permission-error
       "INPUT" "BINARY_STREAM" (%io-public-designator entry)
       environment operation "Binary stream does not support peeking"))
    (let ((byte (read-byte stream nil nil)))
      (when peek
        (unless (ignore-errors (file-position stream position))
          (%raise-permission-error
           "INPUT" "BINARY_STREAM" (%io-public-designator entry)
           environment operation "Binary stream position could not be restored")))
      (setf (prolog-stream-end-of-stream entry)
            (cond (byte :not)
                  (peek :at)
                  (t :past)))
      (or byte -1))))

(defun %io-byte (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    ;; ISO 13211-1 8.13.3.3 uses one class for both shapes: anything that is
    ;; not a byte is type_error(byte, B).
    (unless (and (integerp value) (<= 0 value 255))
      (%raise-type-error "BYTE" value environment operation
                         "Byte must be an integer between 0 and 255"))
    value))

(defun %io-byte-input-goal (entry byte environment emit operation peek)
  ;; ISO 13211-1 8.13.1.3: a bound argument that is no in-byte is a
  ;; type_error(in_byte, B), checked before the stream's type.
  (let ((value (logic-substitute byte environment)))
    (unless (or (logic-var-p value)
                (and (integerp value) (<= -1 value 255)))
      (%raise-type-error "IN_BYTE" value environment operation
                         "expected a byte or -1 for end of file")))
  (%unify-emit byte (%io-read-byte entry environment operation :peek peek)
               environment emit))

(%define-io-dual-builtin (put_char (character) :output "PUT_CHAR")
    (rulebase environment depth emit)
  (%io-write-character entry character environment operation)
  (funcall emit environment))

(%define-io-dual-builtin (get_byte (byte) :input "GET_BYTE")
    (rulebase environment depth emit)
  (%io-byte-input-goal entry byte environment emit operation nil))

(%define-io-dual-builtin (peek_byte (byte) :input "PEEK_BYTE")
    (rulebase environment depth emit)
  (%io-byte-input-goal entry byte environment emit operation t))

(defun %io-write-byte-goal (entry byte environment emit operation)
  ;; ISO 13211-1 8.13.3.3 checks the argument before the stream: `put_byte(a)'
  ;; on a text stream is a type_error(byte, a), not a permission error about
  ;; the stream, because the argument is wrong whatever the stream is.
  (let ((value (%io-byte byte environment operation)))
    (%io-require-stream-type entry :binary environment operation)
    (write-byte value (prolog-stream-stream entry)))
  (funcall emit environment))
(%define-io-dual-builtin (put_byte (byte) :output "PUT_BYTE")
    (rulebase environment depth emit)
  (%io-write-byte-goal entry byte environment emit operation))

(defun %io-at-end-p (entry environment operation)
  (if (eq (prolog-stream-end-of-stream entry) :past)
      t
      (let ((at-end-p
              (ecase (prolog-stream-type entry)
                (:text
                 (eq (peek-char nil (prolog-stream-stream entry) nil :eof) :eof))
                (:binary
                 (= -1 (%io-read-byte entry environment operation :peek t))))))
        (setf (prolog-stream-end-of-stream entry)
              (if at-end-p :at :not))
        at-end-p)))

(%define-io-dual-builtin (at_end_of_stream () :input "AT_END_OF_STREAM")
    (rulebase environment depth emit)
  (when (%io-at-end-p entry environment operation)
    (funcall emit environment)))
