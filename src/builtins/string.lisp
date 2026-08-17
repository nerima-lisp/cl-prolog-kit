;;;; SWI-compatible string builtins over the CL-string term type: conversions
;;;; (atom_string, string_to_atom, number_string, string_chars, string_codes,
;;;; term_string), string_concat/text_concat, string_length, sub_string, and
;;;; split_string.  Text inputs are coerced through %TEXT-OF, so any of atom /
;;;; string / number / code list / char list is accepted where SWI accepts text.

(in-package #:cl-prolog-kit)

;;; string_length/2

(define-builtin (string_length string length) (rulebase environment depth emit)
  (let ((text (%text-of (logic-substitute string environment)
                        environment (%iso-atom "STRING_LENGTH"))))
    (%unify-emit length (length text) environment emit)))

;;; atom_string/2, string_to_atom/2, number_string/2

(define-iso-builtin (atom_string atom (string :raw)) "ATOM_STRING"
  (if (logic-var-p resolved-atom)
      ;; string → atom
      (%unify-emit atom (%text-atom (%text-of (logic-substitute string environment)
                                              environment operation))
                   environment emit)
      ;; atom (or any text) → string
      (%unify-emit string (%text-of resolved-atom environment operation)
                   environment emit)))

(define-iso-builtin (string_to_atom string (atom :raw)) "STRING_TO_ATOM"
  (if (logic-var-p resolved-string)
      ;; atom → string
      (%unify-emit string (%text-of (logic-substitute atom environment)
                                    environment operation)
                   environment emit)
      ;; string → atom
      (%unify-emit atom (%text-atom (%text-of resolved-string environment operation))
                   environment emit)))

(define-iso-builtin (number_string number (string :raw)) "NUMBER_STRING"
  (if (logic-var-p resolved-number)
      ;; string → number
      (%unify-emit number
                   (%text-number (string-trim '(#\Space #\Tab #\Newline)
                                              (%text-of (logic-substitute string environment)
                                                        environment operation))
                                 environment operation)
                   environment emit)
      (progn
        (unless (or (integerp resolved-number) (floatp resolved-number))
          (%raise-type-error "NUMBER" resolved-number environment operation
                             "number_string/2 first argument must be a number"))
        (%unify-emit string (%number-text resolved-number environment operation)
                     environment emit))))

;;; string_chars/2, string_codes/2

(defun %string-list-builtin (string list environment emit operation list->text text->list)
  "Shared body for string_chars/2 and string_codes/2."
  (let ((string-value (logic-substitute string environment)))
    (if (logic-var-p string-value)
        (%unify-emit string
                     (funcall list->text (logic-substitute list environment)
                              environment operation)
                     environment emit)
        (%unify-emit list
                     (funcall text->list (%text-of string-value environment operation))
                     environment emit))))

(define-builtin (string_chars string chars) (rulebase environment depth emit)
  (%string-list-builtin
   string chars environment emit (%iso-atom "STRING_CHARS")
   #'%character-list-text
   (lambda (text) (map 'list (lambda (c) (%text-atom (string c))) text))))

(define-builtin (string_codes string codes) (rulebase environment depth emit)
  (%string-list-builtin
   string codes environment emit (%iso-atom "STRING_CODES")
   #'%code-list-text
   (lambda (text) (map 'list #'char-code text))))

;;; term_string/2

(define-iso-builtin (term_string term (string :raw)) "TERM_STRING"
  (if (logic-var-p resolved-term)
      (%unify-emit term
                   (%parse-term-from-text
                    (%text-of (logic-substitute string environment)
                              environment operation)
                    rulebase environment operation)
                   environment emit)
      (%unify-emit string (prolog-term-string resolved-term) environment emit)))

;;; string_concat/3 and text_concat/3

(defun %concat-goal (left right whole environment emit operation make-result)
  "Relational concatenation shared by string_concat/3 and text_concat/3.
MAKE-RESULT wraps a CL string as the emitted term (string or atom)."
  (let ((left-value (logic-substitute left environment))
        (right-value (logic-substitute right environment))
        (whole-value (logic-substitute whole environment)))
    (cond
      ((and (not (logic-var-p left-value)) (not (logic-var-p right-value)))
       (%unify-emit whole
                    (funcall make-result
                             (concatenate 'string
                                          (%text-of left-value environment operation)
                                          (%text-of right-value environment operation)))
                    environment emit))
      ((logic-var-p whole-value)
       (%raise-instantiation-error
        environment operation
        "concatenation needs the first two or the whole argument instantiated"))
      (t
       (let ((whole-text (%text-of whole-value environment operation)))
         (cond
           ((not (logic-var-p left-value))
            (let ((prefix (%text-of left-value environment operation)))
              (when (and (<= (length prefix) (length whole-text))
                         (string= prefix whole-text :end2 (length prefix)))
                (%unify-emit right (funcall make-result (subseq whole-text (length prefix)))
                             environment emit))))
           ((not (logic-var-p right-value))
            (let* ((suffix (%text-of right-value environment operation))
                   (split (- (length whole-text) (length suffix))))
              (when (and (>= split 0)
                         (string= suffix whole-text :start2 split))
                (%unify-emit left (funcall make-result (subseq whole-text 0 split))
                             environment emit))))
           (t
            ;; both ends unbound: enumerate every split point.
            (loop for split from 0 to (length whole-text)
                  do (%term-unify-sequence
                      (list (cons left (funcall make-result (subseq whole-text 0 split)))
                            (cons right (funcall make-result (subseq whole-text split))))
                      environment emit)))))))))

(define-builtin (string_concat left right whole) (rulebase environment depth emit)
  (let ((operation (%iso-atom "STRING_CONCAT")))
    (%concat-goal left right whole environment emit operation
                  ;; Cap the result length like the atom/text builtins, so a
                  ;; feedback loop (double(S,SS):-string_concat(S,S,SS)) cannot
                  ;; grow a string without bound.
                  (lambda (text)
                    (%check-text-resource-limit
                     text *max-prolog-quoted-lexeme-length* "STRING_LENGTH"
                     environment operation
                     "string exceeds the configured length limit")
                    text))))

(define-builtin (text_concat left right whole) (rulebase environment depth emit)
  (%concat-goal left right whole environment emit (%iso-atom "TEXT_CONCAT")
                (lambda (text) (%text-atom text))))

;;; sub_string/5 -- like sub_atom/5 but the sub-term is a string.

(define-iso-builtin (sub_string string before length (after :raw) sub) "SUB_STRING"
  (let* ((text (%text-of resolved-string environment operation))
         (total (length text)))
    (flet ((emit-slice (b l)
             (let ((piece (subseq text b (+ b l))))
               (%term-unify-sequence
                (list (cons before b) (cons length l)
                      (cons after (- total b l)) (cons sub piece))
                environment emit))))
      (cond
        ;; sub given as text: find all occurrences.
        ((and (logic-var-p resolved-before) (not (logic-var-p resolved-sub)))
         (let* ((piece (%text-of resolved-sub environment operation))
                (l (length piece)))
           (loop for b from 0 to (- total l)
                 when (string= piece text :start2 b :end2 (+ b l))
                   do (emit-slice b l))))
        ;; before and length both known.
        ((and (integerp resolved-before) (integerp resolved-length))
         (when (and (<= 0 resolved-before) (<= 0 resolved-length)
                    (<= (+ resolved-before resolved-length) total))
           (emit-slice resolved-before resolved-length)))
        ;; otherwise enumerate all (before, length) slices.
        (t
         (loop for b from 0 to total
               when (or (not (integerp resolved-before)) (= b resolved-before))
                 do (loop for l from 0 to (- total b)
                          when (or (not (integerp resolved-length)) (= l resolved-length))
                            do (emit-slice b l))))))))

;;; split_string/4

(defun %split-string-fields (text separators)
  "Split TEXT into fields at any character in SEPARATORS.  Empty SEPARATORS
yields the whole TEXT as a single field."
  (if (zerop (length separators))
      (list text)
      (let ((fields '()) (start 0))
        (dotimes (index (length text))
          (when (find (char text index) separators)
            (push (subseq text start index) fields)
            (setf start (1+ index))))
        (push (subseq text start) fields)
        (nreverse fields))))

(define-iso-builtin (split_string string separators pad (parts :raw)) "SPLIT_STRING"
  (let* ((text (%text-of resolved-string environment operation))
         (sep (%text-of resolved-separators environment operation))
         (pad (%text-of resolved-pad environment operation))
         (pad-bag (coerce pad 'list))
         (fields (mapcar (lambda (field) (string-trim pad-bag field))
                         (%split-string-fields text sep))))
    (%unify-emit parts fields environment emit)))
