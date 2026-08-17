;;;; format/1,2,3, tab/1,2, and print/1,2.
;;;;
;;;; A pragmatic SWI-compatible format covering the directives real code
;;;; relies on: ~w ~p ~q ~a ~s ~d ~D ~f ~e ~g ~r ~R ~c ~n ~~ and column
;;;; control ~t ~| ~+ (fill character set with ~`ct, argument-supplied counts
;;;; with ~*).  The format text may be an atom, a code list, a character list,
;;;; or a Common Lisp string; arguments are a proper list (a non-list argument
;;;; is treated as a single-element list, as SWI does).  Malformed directives
;;;; and out-of-range values raise catchable ISO errors.

(in-package #:cl-prolog-kit)

(defun %format-source-text (term environment operation)
  "Resolve a format-string argument to a Lisp string."
  (%text-of (logic-substitute term environment) environment operation
            :accept :text
            :instantiation "format string must be instantiated"
            :type-message
            "format string must be an atom, code list, or char list"))

(defun %format-arguments (term environment)
  "Resolve the format arguments; a non-list value becomes a single argument."
  (let ((value (logic-substitute term environment)))
    (if (%proper-list-p value) value (list value))))

(defun %format-write-string (term &key quoted (numbervars t))
  "Render TERM the way write/writeq would, into a fresh string."
  (with-output-to-string (stream)
    (%write-prolog-term-with-options term stream
                                     :quoted quoted :numbervars numbervars)))

(defun %format-atomic-text (term environment operation)
  "Text of an atomic argument for ~a: atom name or number text."
  (%text-of term environment operation
            :accept :atomic :instantiation nil
            :type "ATOMIC" :type-message "~~a expects an atom or number"))

(defun %format-string-text (term environment operation)
  "Text of a ~s argument: an atom, a code list, or a character list."
  (%text-of term environment operation
            :accept :text :instantiation nil
            :type-message "~~s expects an atom, code list, or char list"))

(defun %format-grouped-integer (integer)
  "Render INTEGER in base 10 with comma-separated thousands groups."
  (let* ((negative (minusp integer))
         (digits (write-to-string (abs integer) :base 10))
         (length (length digits)))
    (with-output-to-string (out)
      (when negative (write-char #\- out))
      (loop for index from 0 below length
            for from-end = (- length index)
            do (write-char (char digits index) out)
               (when (and (> from-end 1) (zerop (mod (1- from-end) 3)))
                 (write-char #\, out))))))

(defun %format-decimal-shifted (integer places)
  "Render INTEGER with PLACES fractional digits (SWI ~Nd), inserting a point."
  (if (zerop places)
      (write-to-string integer :base 10)
      (let* ((negative (minusp integer))
             (digits (write-to-string (abs integer) :base 10))
             (padded (if (<= (length digits) places)
                         (concatenate 'string
                                      (make-string (1+ (- places (length digits)))
                                                   :initial-element #\0)
                                      digits)
                         digits))
             (split (- (length padded) places)))
        (format nil "~:[~;-~]~A.~A" negative
                (subseq padded 0 split) (subseq padded split)))))

(defun %format-radix (integer radix upcase environment operation)
  "Render INTEGER in RADIX (2..36); UPCASE selects letter case."
  (unless (<= 2 radix 36)
    (%raise-domain-error "RADIX" radix environment operation
                         "format ~r radix must be between 2 and 36"))
  (let ((text (write-to-string integer :base radix)))
    (if upcase (string-upcase text) (string-downcase text))))

(defun %format-float (directive places value)
  "Render VALUE with the CL float DIRECTIVE (#\\f #\\e #\\g) and PLACES digits.
For ~e/~g the Common Lisp exponent marker (e.g. `d' for double-floats) is
normalized to `e', matching the Prolog/C convention; ~g's alignment padding is
trimmed so the result reads like SWI's ~g."
  (let ((text (format nil (format nil "~~,~D~C" places directive) value)))
    (if (char= directive #\f)
        text
        ;; The mantissa contains only digits, sign and point, so the sole
        ;; letter is the exponent marker; force it to `e'.
        (string-right-trim
         " "
         (map 'string
              (lambda (character)
                (if (alpha-char-p character) #\e character))
              text)))))

(defun %format-string-error (text environment operation message)
  "Raise a catchable ISO domain_error(format_string, ...) for a malformed
format string, so untrusted format text can only trigger catchable errors.
The culprit is an uninterned symbol so untrusted text is never interned."
  (%raise-domain-error "FORMAT_STRING" (make-symbol text)
                       environment operation message))

(defun %read-format-directive (text index next-argument environment operation)
  "Parse a format directive body starting at INDEX (just past the ~).
Returns (VALUES NUMERIC-ARGUMENT FILL-CHARACTER DIRECTIVE NEXT-INDEX).
A `*' column argument consumes the next format argument via NEXT-ARGUMENT.
Malformed directives raise a catchable ISO error."
  (let ((length (length text))
        (numeric-argument nil)
        (fill-character #\Space))
    (cond
      ((>= index length)
       (%format-string-error text environment operation
                             "format directive is truncated"))
      ((char= (char text index) #\*)
       (let ((value (funcall next-argument)))
         (unless (integerp value)
           (%raise-type-error "INTEGER" value environment operation
                              "format ~* column argument must be an integer"))
         (setf numeric-argument value))
       (incf index))
      ((char= (char text index) #\`)
       (incf index)
       (when (>= index length)
         (%format-string-error text environment operation
                               "format ` directive is truncated"))
       (setf fill-character (char text index))
       (setf numeric-argument (char-code fill-character))
       (incf index))
      (t
       (let ((start index))
         (loop while (and (< index length) (digit-char-p (char text index)))
               do (incf index))
         (when (> index start)
           (setf numeric-argument (parse-integer text :start start :end index))))))
    (when (>= index length)
      (%format-string-error text environment operation
                            "format directive is missing its command character"))
    (values numeric-argument fill-character (char text index) (1+ index))))

(defun %render-format-string (text arguments environment operation)
  "Return the string produced by TEXT and ARGUMENTS.  Supports column
control (~t fill, ~| absolute column, ~+ relative column)."
  (let ((length (length text))
        (index 0)
        (remaining arguments)
        (output (make-string-output-stream))
        (line-column 0)      ; committed column on the current line
        (segment-column 0)   ; column where the pending segment began
        (pending-text-length 0)
        (pending-fill-count 0)
        (pieces '()))        ; reversed: strings and (:fill . char)
    (labels ((next-argument ()
               (when (null remaining)
                 (%raise-domain-error "FORMAT_ARGUMENTS" (%iso-atom "format")
                                      environment operation
                                      "not enough arguments for format directives"))
               (pop remaining))
             (commit-plain ()
               ;; Flush the pending segment with zero-width fills.
               (dolist (piece (reverse pieces))
                 (when (stringp piece) (write-string piece output)))
               (incf line-column pending-text-length)
               (setf pieces '()
                     pending-text-length 0
                     pending-fill-count 0
                     segment-column line-column))
             (commit-to-column (target)
               (let* ((text-length pending-text-length)
                      (fills pending-fill-count)
                      (deficit (max 0 (- target (+ segment-column text-length)))))
                 (%check-builtin-output-length
                  deficit "COLUMN_WIDTH" environment operation
                  "format column fill exceeds the configured output limit")
                 (if (zerop fills)
                     (progn
                       (dolist (piece (reverse pieces))
                         (when (stringp piece) (write-string piece output)))
                       ;; No fill point: pad on the right to reach the column.
                       (dotimes (i deficit) (write-char #\Space output)))
                     (let ((base (floor deficit fills))
                           (extra (mod deficit fills))
                           (fill-index 0))
                       (dolist (piece (reverse pieces))
                         (if (stringp piece)
                             (write-string piece output)
                             (let ((width (+ base (if (< fill-index extra) 1 0))))
                               (incf fill-index)
                               (dotimes (i width)
                                 (write-char (cdr piece) output)))))))
                 (setf line-column (max target (+ segment-column text-length)))
                 (setf pieces '()
                       pending-text-length 0
                       pending-fill-count 0
                       segment-column line-column)))
             (add-text (string)
               (incf pending-text-length (length string))
               (push string pieces))
             (add-fill (character)
               (incf pending-fill-count)
               (push (cons :fill character) pieces))
             (newline (count)
               (%check-builtin-output-length
                (max 1 count) "NEWLINE_COUNT" environment operation
                "format ~n count exceeds the configured output limit")
               (commit-plain)
               (dotimes (i (max 1 count)) (write-char #\Newline output))
               (setf line-column 0 segment-column 0)))
      (loop while (< index length) do
        (let ((character (char text index)))
          (if (char/= character #\~)
              (let ((literal-end (or (position #\~ text :start index) length)))
                (add-text (subseq text index literal-end))
                (setf index literal-end))
              (multiple-value-bind (numeric-argument fill-character directive next-index)
                  (%read-format-directive text (1+ index) #'next-argument
                                          environment operation)
                (setf index next-index)
                (case directive
                  ((#\w) (add-text (%format-write-string (next-argument))))
                  ((#\p) (add-text (%format-write-string (next-argument) :quoted t)))
                  ((#\q) (add-text (%format-write-string (next-argument) :quoted t)))
                  ((#\a) (add-text (%format-atomic-text (next-argument)
                                                        environment operation)))
                  ((#\s) (add-text (%format-string-text (next-argument)
                                                        environment operation)))
                  ((#\d)
                   (let ((value (next-argument)))
                     (unless (integerp value)
                       (%raise-type-error "INTEGER" value environment operation
                                          "~~d expects an integer"))
                     (add-text (%format-decimal-shifted value (or numeric-argument 0)))))
                  ((#\D)
                   (let ((value (next-argument)))
                     (unless (integerp value)
                       (%raise-type-error "INTEGER" value environment operation
                                          "~~D expects an integer"))
                     (add-text (%format-grouped-integer value))))
                  ((#\f #\e #\g)
                   (let ((value (next-argument))
                         (places (or numeric-argument 6)))
                     (unless (or (integerp value) (floatp value))
                       (%raise-type-error "NUMBER" value environment operation
                                          "~f/~e/~g expect a number"))
                     (add-text (%format-float directive places
                                              (float value 1.0d0)))))
                  ((#\r #\R)
                   (let ((value (next-argument)))
                     (unless (integerp value)
                       (%raise-type-error "INTEGER" value environment operation
                                          "~r expects an integer"))
                     (add-text (%format-radix value (or numeric-argument 8)
                                              (char= directive #\R)
                                              environment operation))))
                  ((#\c)
                   (let ((code (next-argument))
                         (count (or numeric-argument 1)))
                     (unless (integerp code)
                       (%raise-type-error "INTEGER" code environment operation
                                          "~c expects a character code"))
                     (%check-builtin-output-length
                      (max 0 count) "REPEAT_COUNT" environment operation
                      "format ~c count exceeds the configured output limit")
                     (add-text (make-string (max 0 count)
                                            :initial-element
                                            (%code-character code environment operation)))))
                  ((#\n) (newline (or numeric-argument 1)))
                  ((#\~) (add-text "~"))
                  ((#\t) (add-fill fill-character))
                  ((#\|) (commit-to-column (or numeric-argument
                                               (+ segment-column pending-text-length))))
                  ((#\+) (commit-to-column (+ segment-column
                                              (or numeric-argument 0))))
                  (t (%format-string-error
                      text environment operation
                      (format nil "unknown format directive ~~~C" directive))))))))
      (commit-plain)
      (get-output-stream-string output))))

(defun %format-goal (entry format-term arguments-term environment emit operation)
  (let* ((text (%format-source-text format-term environment operation))
         (arguments (%format-arguments arguments-term environment))
         (rendered (%render-format-string text arguments environment operation)))
    (write-string rendered (prolog-stream-stream entry))
    (funcall emit environment)))

(define-builtin (format format-term) (rulebase environment depth emit)
  (%format-goal (%io-current-output-entry rulebase)
                format-term nil environment emit (%io-operation "FORMAT")))

(define-builtin (format format-term arguments) (rulebase environment depth emit)
  (%format-goal (%io-current-output-entry rulebase)
                format-term arguments environment emit (%io-operation "FORMAT")))

(define-builtin (format stream format-term arguments) (rulebase environment depth emit)
  (let ((operation (%io-operation "FORMAT")))
    (%format-goal (%io-stream-entry rulebase stream :output environment operation)
                  format-term arguments environment emit operation)))

;;; tab/1, tab/2

(defun %tab-goal (entry count-term environment emit operation)
  (let ((count (%evaluate-arithmetic-expression count-term environment)))
    (unless (integerp count)
      (%raise-type-error "INTEGER" count environment operation
                         "tab expects an integer count"))
    (when (plusp count)
      (%check-builtin-output-length
       count "REPEAT_COUNT" environment operation
       "tab count exceeds the configured output limit")
      (write-string (make-string count :initial-element #\Space)
                    (prolog-stream-stream entry)))
    (funcall emit environment)))

(define-builtin (tab count) (rulebase environment depth emit)
  (%tab-goal (%io-current-output-entry rulebase) count environment emit
             (%io-operation "TAB")))

(define-builtin (tab stream count) (rulebase environment depth emit)
  (let ((operation (%io-operation "TAB")))
    (%tab-goal (%io-stream-entry rulebase stream :output environment operation)
               count environment emit operation)))

;;; print/1, print/2 -- like write/1 with quoting and numbervars, since no
;;; portray/1 hook exists.

(defun %print-goal (entry term environment emit)
  (%write-prolog-term-with-options (logic-substitute term environment)
                                   (prolog-stream-stream entry)
                                   :quoted t :numbervars t)
  (funcall emit environment))

(define-builtin (print term) (rulebase environment depth emit)
  (%print-goal (%io-current-output-entry rulebase) term environment emit))

(define-builtin (print stream term) (rulebase environment depth emit)
  (%print-goal (%io-stream-entry rulebase stream :output environment
                                 (%io-operation "PRINT"))
               term environment emit))
