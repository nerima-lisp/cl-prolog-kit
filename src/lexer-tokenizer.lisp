;;;; The character-stream tokenizer: reading one term's raw source text,
;;;; then scanning it into %token structs against an operator table's
;;;; lexemes (lexer-operator-lexemes.lisp), under lexer.lisp's resource
;;;; limits.
(in-package #:cl-prolog-kit)

(defmacro %scan-prolog-quoted-lexeme (delimiter unterminated-message)
  `(progn
    (take)
    (setf raw-mode t)
    (unwind-protect (let ((content-length 0))
        (labels ((write-content (character out)
                   (incf content-length)
                   (%check-parser-limit
                "QUOTED_LEXEME_LENGTH"
                *max-prolog-quoted-lexeme-length*
                content-length
                position)
                   (write-char character out)))
          (with-output-to-string (out)
            (loop (unless (peek)
                (%parse-error ,unterminated-message)) (let ((character (take)))
                (cond
                  ((and (char= character ,delimiter) (peek) (char= (peek) ,delimiter))
                    (take)
                    (write-content ,delimiter out))
                  ((char= character ,delimiter) (return))
                  ((and (char= character #\\) (peek))
                    (let ((decoded (scan-escape)))
                      (when decoded
                        (write-content decoded out))))
                  (t (write-content character out))))))))
      (setf raw-mode nil))))

(defun %tokenize-prolog (source &optional (operator-table *standard-operator-table*))
  (let* ((text (%prolog-source-string source))
         (length (length text))
         (position 0)
         (operator-lexemes (%operator-table-lexemes operator-table))
         (word-operators (car operator-lexemes))
         (symbolic-tokens (cdr operator-lexemes))
         (conversions *active-char-conversions*)
         (raw-mode nil)
         (token-count 0)
         (delimiters '())
         (delimiter-depth 0)
         (tokens '()))
        (labels ((peek (&optional (offset 0))
               (let ((index (+ position offset)))
                 (when (< index length)
                   (let ((character (char text index)))
                     (if (and conversions (not raw-mode))
                         (or (gethash character conversions) character)
                         character)))))
             (take ()
               (prog1 (peek) (incf position)))
             (expected-closer (operator)
               (cond ((string= operator "(") ")")
                     ((string= operator "[") "]")
                     ((string= operator "{") "}")))
             (note-delimiter
  (operator start)
  (let ((closer (expected-closer operator)))
    (cond
      (closer
        (%check-parser-limit
          "DELIMITER_DEPTH"
          *max-prolog-delimiter-depth*
          (1+ delimiter-depth)
          start)
        (progn
          (push closer delimiters)
          (incf delimiter-depth)))
      ((member operator '(")" "]" "}") :test #'string=)
        (unless delimiters
          (%parse-error
            "Unexpected closing Prolog delimiter ~A at source position ~D."
            operator
            start))
        (unless (string= operator (first delimiters))
          (%parse-error
            "Mismatched Prolog delimiter ~A at source position ~D; expected ~A."
            operator
            start
            (first delimiters)))
        (progn
          (pop delimiters)
          (decf delimiter-depth))))))
             (emit (kind &optional value (start position))
               (unless (eq kind :eof)
                 (%check-parser-limit "TOKEN_COUNT"
                                      *max-prolog-tokens*
                                      (1+ token-count)
                                      start)
                 (incf token-count))
               (when (eq kind :operator)
                 (note-delimiter value start))
               (push (%token kind value start) tokens))
             (skip-line ()
               (loop while (and (peek) (not (char= (peek) #\Newline)))
                     do (take)))
             (skip-block ()
               (incf position 2)
               (loop until (and (peek) (peek 1)
                                (char= (peek) #\*)
                                (char= (peek 1) #\/))
                     do (unless (peek)
                          (%parse-error "Unterminated Prolog block comment."))
                        (take))
               (incf position 2))
             (scan-name ()
               (let ((start position))
                 (with-output-to-string (out)
                   (loop while (and (peek) (%identifier-character-p (peek)))
                         do (%check-parser-limit
                             "IDENTIFIER_LENGTH"
                             *max-prolog-identifier-length*
                             (1+ (- position start))
                             position)
                            (write-char (take) out)))))
             (scan-radix-escape (radix terminated-p)
               "Read the digits of a `\\xHH\\' or `\\OOO\\' escape as a character."
               (let ((digits (with-output-to-string (out)
                               (loop while (and (peek) (digit-char-p (peek) radix))
                                     do (write-char (take) out)))))
                 (when (zerop (length digits))
                   (%parse-error "Malformed Prolog character escape at position ~D."
                                 position))
                 ;; ISO 6.4.2.1 closes both forms with `\', which SWI also
                 ;; accepts without; tolerate the missing one either way.
                 (when (and terminated-p (eql (peek) #\\)) (take))
                 (let ((code (parse-integer digits :radix radix)))
                   (unless (< code char-code-limit)
                     (%parse-error "Prolog character escape ~S is out of range."
                                   digits))
                   (code-char code))))
             (scan-escape ()
               "Decode the escape sequence after a `\\', per ISO 6.4.2.1.

Returns NIL for a `\\'-newline continuation, which contributes no character."
               (let ((character (take)))
                 (case character
                   (#\a (code-char 7))
                   (#\b #\Backspace)
                   (#\f #\Page)
                   (#\n #\Newline)
                   (#\r #\Return)
                   (#\t #\Tab)
                   (#\v (code-char 11))
                   (#\e (code-char 27))
                   (#\0 (code-char 0))
                   (#\x (scan-radix-escape 16 t))
                   (#\Newline nil)
                   (t (if (digit-char-p character 8)
                          (progn (decf position) (scan-radix-escape 8 t))
                          ;; \\ \' \" \` and anything else stand for themselves.
                          character)))))
             (scan-quoted
  ()
  (%scan-prolog-quoted-lexeme #\' "Unterminated quoted Prolog atom."))
             (scan-string () (%scan-prolog-quoted-lexeme #\" "Unterminated Prolog string."))
             (block-comment-ahead-p ()
               (and (eql (peek) #\/) (eql (peek 1) #\*)))
             (scan-graphic-run ()
               "Consume the maximal run of graphic characters at POSITION.

Stops before a `/*' so an adjacent block comment still opens where it would
have without the run, as in `a +/* note */ b'."
               (let ((start position))
                 (loop while (and (%prolog-graphic-character-p (peek))
                                  (not (and (> position start)
                                            (block-comment-ahead-p))))
                       do (%check-parser-limit
                           "IDENTIFIER_LENGTH"
                           *max-prolog-identifier-length*
                           (1+ (- position start))
                           position)
                          (take))
                 (subseq text start position)))
             (end-token-follows-p ()
               "True when a lone `.' just consumed ends a clause: ISO 6.4.8
requires layout text, a comment, or end of input after the end token."
               (let ((next (peek)))
                 (or (null next)
                     (member next '(#\Space #\Tab #\Return #\Newline))
                     (char= next #\%))))
             (radix-digit-p (character radix)
               (and character (digit-char-p character radix)))
             (scan-radix-integer (radix)
               "Read the digits of an `0x'/`0o'/`0b' integer constant."
               (incf position 2)
               (let ((start position))
                 (loop while (radix-digit-p (peek) radix)
                       do (%check-parser-limit
                           "NUMERIC_LEXEME_LENGTH"
                           *max-prolog-numeric-lexeme-length*
                           (1+ (- position start))
                           position)
                          (take))
                 (parse-integer (subseq text start position) :radix radix)))
             (scan-character-code ()
               "Read an `0'c' character-code constant, per ISO 6.4.4."
               (incf position 2)
               (let ((character (peek)))
                 (cond
                   ((null character)
                    (%parse-error "Unterminated Prolog character-code constant."))
                   ;; `0''' is the quote itself, doubled as inside a quoted atom.
                   ((char= character #\')
                    (take)
                    (when (eql (peek) #\') (take))
                    (char-code #\'))
                   ((char= character #\\)
                    (take)
                    (let ((decoded (scan-escape)))
                      (unless decoded
                        (%parse-error
                         "A `\\'-newline continuation is not a character code."))
                      (char-code decoded)))
                   (t (take) (char-code character)))))
             (scan-number ()
               ;; ISO 6.4.4's non-decimal integer constants all begin with `0'.
               (when (and (char= (peek) #\0) (peek 1))
                 (let ((marker (char-downcase (peek 1))))
                   (case marker
                     (#\' (return-from scan-number (scan-character-code)))
                     (#\x (when (radix-digit-p (peek 2) 16)
                            (return-from scan-number (scan-radix-integer 16))))
                     (#\o (when (radix-digit-p (peek 2) 8)
                            (return-from scan-number (scan-radix-integer 8))))
                     (#\b (when (radix-digit-p (peek 2) 2)
                            (return-from scan-number (scan-radix-integer 2)))))))
               (let ((start position))
                 (labels ((take-number-character ()
                            (%check-parser-limit
                             "NUMERIC_LEXEME_LENGTH"
                             *max-prolog-numeric-lexeme-length*
                             (1+ (- position start))
                             position)
                            (take)))
                   (loop while (and (peek) (digit-char-p (peek)))
                         do (take-number-character))
                   (let ((float-p nil))
                     (when (and (peek) (peek 1)
                                (char= (peek) #\.)
                                (digit-char-p (peek 1)))
                       (setf float-p t)
                       (take-number-character)
                       (loop while (and (peek) (digit-char-p (peek)))
                             do (take-number-character)))
                     (when (and (peek) (member (peek) '(#\e #\E)))
                       (setf float-p t)
                       (take-number-character)
                       (when (and (peek) (member (peek) '(#\+ #\-)))
                         (take-number-character))
                       (unless (and (peek) (digit-char-p (peek)))
                         (%parse-error "Malformed Prolog exponent."))
                       (loop while (and (peek) (digit-char-p (peek)))
                             do (take-number-character)))
                     (let ((lexeme (subseq text start position)))
                       (if float-p
                           (let ((*read-default-float-format* 'double-float))
                             (read-from-string lexeme))
                           (parse-integer lexeme))))))))
      (loop while (peek)
            do (let ((start position))
                 (cond
                   ((member (peek) '(#\Space #\Tab #\Return #\Newline))
                    (take))
                   ((char= (peek) #\%)
                    (skip-line))
                   ((and (char= (peek) #\/)
                         (peek 1)
                         (char= (peek 1) #\*))
                    (skip-block))
                   ((char= (peek) #\')
                    (emit :quoted-atom (scan-quoted) start))
                   ((char= (peek) #\")
                    (emit :string (scan-string) start))
                   ((digit-char-p (peek))
                    (emit :number (scan-number) start))
                   ((or (upper-case-p (peek)) (char= (peek) #\_))
                    (emit :variable (scan-name) start))
                   ((lower-case-p (peek))
                    (let ((name (scan-name)))
                      (if (member name word-operators :test #'string=)
                          (emit :operator name start)
                          (emit :atom name start))))
                   ;; A run of graphic characters is one token, per ISO 6.4.2 --
                   ;; not the longest declared operator that happens to prefix
                   ;; it, which would split `===' into `==' and `='.
                   ((%prolog-graphic-character-p (peek))
                    (let ((run (scan-graphic-run)))
                      (cond
                        ((and (string= run ".") (end-token-follows-p))
                         (emit :operator "." start))
                        ((member run symbolic-tokens :test #'string=)
                         (emit :operator run start))
                        ;; An undeclared graphic token is simply an atom.
                        (t (emit :atom run start)))))
                   (t
                    (let ((operator
                            (find-if
                             (lambda (candidate)
                               (and (<= (+ position (length candidate)) length)
                                    (loop for index from 0 below (length candidate)
                                          always (eql (char candidate index)
                                                      (peek index)))))
                             symbolic-tokens)))
                      (if operator
                          (progn
                            (incf position (length operator))
                            (emit :operator operator start))
                          (let ((character (take)))
                            (if (char= character #\!)
                                (emit :atom "!" start)
                                (%parse-error
                                 "Unexpected Prolog character ~S at source position ~D."
                                 character start)))))))))
      (when delimiters
        (%parse-error
         "Unexpected end of Prolog input; expected closing delimiter ~A."
         (first delimiters)))
      (emit :eof nil position)
      (coerce (nreverse tokens) 'vector))))

