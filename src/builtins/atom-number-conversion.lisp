;;;; Atom/number <-> character/code list conversion builtins:
;;;; atom_chars/2, atom_codes/2, char_code/2, number_chars/2,
;;;; number_codes/2, atom_number/2.

(in-package #:cl-prolog-kit)

(defmacro define-atom-list-conversion (name list-to-text atom-to-list)
  `(define-iso-builtin (,name atom list) ,(string-upcase (symbol-name name))
     (cond
       ((not (logic-var-p resolved-atom))
        (%ensure-atom-value resolved-atom environment operation "first argument")
        (%unify-emit
         list (,atom-to-list resolved-atom environment operation)
         environment emit))
       ((not (logic-var-p resolved-list))
        (%unify-emit
         atom
         (%text-atom
          (,list-to-text resolved-list environment operation)
          environment operation)
         environment emit))
       (t
        (%raise-instantiation-error environment operation
                                    "one argument must be instantiated")))))

(define-atom-list-conversion atom_chars %character-list-text %atom-character-list)

(define-atom-list-conversion atom_codes %code-list-text %atom-code-list)

(define-iso-builtin (char_code character code) "CHAR_CODE"
  (cond
    ((not (logic-var-p resolved-character))
     (unless (%character-atom-p resolved-character)
       (%raise-type-error "CHARACTER" resolved-character environment operation
                          "char_code/2 requires a one-character atom"))
     (unless (or (logic-var-p resolved-code) (integerp resolved-code))
       (%raise-type-error "INTEGER" resolved-code environment operation
                          "char_code/2 code must be an integer"))
     (%unify-emit code (char-code (char (%atom-text resolved-character) 0))
                  environment emit))
    ((not (logic-var-p resolved-code))
     (let ((value (%code-character resolved-code environment operation)))
       (%unify-emit
        character (%text-atom (string value) environment operation)
        environment emit)))
    (t
     (%raise-instantiation-error environment operation
                                 "char_code/2 requires one instantiated argument"))))

(defmacro define-number-list-conversion (name list-to-text text-to-list)
  `(define-iso-builtin (,name number list) ,(string-upcase (symbol-name name))
     (cond
       ;; Both bound: ISO 13211-1 8.16.7/8.16.8 relate a number to *a* text that
       ;; denotes it, not to the one this engine would print.  So `3.3E+0' and
       ;; `3.3' both name 3.3, and the comparison has to be numeric.
       ((and (not (logic-var-p resolved-number))
             (not (logic-var-p resolved-list))
             (realp resolved-number))
        (when (eql resolved-number
                   (%text-number
                    (,list-to-text
                     resolved-list environment operation
                     *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH")
                    environment operation))
          (funcall emit environment)))
       ((not (logic-var-p resolved-number))
        (unless (realp resolved-number)
          (%raise-type-error "NUMBER" resolved-number environment operation
                             "first argument must be a number"))
        (%unify-emit
         list
         (,text-to-list
          (%text-atom
           (%number-text resolved-number environment operation)
           environment operation)
          environment operation
          *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH")
         environment emit))
       ((not (logic-var-p resolved-list))
        (%unify-emit
         number
         (%text-number
          (,list-to-text
           resolved-list environment operation
           *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH")
          environment operation)
         environment emit))
       (t
        (%raise-instantiation-error environment operation
                                    "one argument must be instantiated")))))

(define-number-list-conversion number_chars %character-list-text %atom-character-list)

(define-number-list-conversion number_codes %code-list-text %atom-code-list)

(define-iso-builtin (atom_number atom number) "ATOM_NUMBER"
  (unless (logic-var-p resolved-atom)
    (%ensure-atom-value resolved-atom environment operation "first argument"))
  (unless (or (logic-var-p resolved-number) (realp resolved-number))
    (%raise-type-error "NUMBER" resolved-number environment operation
                       "second argument must be a number"))
  (unless (logic-var-p resolved-atom)
    (%check-atom-text-limit resolved-atom environment operation))
  (cond
    ((not (logic-var-p resolved-atom))
     ;; Unlike number_chars/2 and number_codes/2, invalid atom text fails.  The
     ;; parse runs outside the handler so that an error raised further along the
     ;; continuation is not mistaken for this atom's and silently swallowed.
     (let ((parsed (handler-case (%text-number (%atom-text resolved-atom)
                                               environment operation)
                     (prolog-syntax-error () nil)
                     (prolog-domain-error () nil))))
       (when parsed
         (%unify-emit number parsed environment emit))))
    ((not (logic-var-p resolved-number))
     (%unify-emit
      atom
      (%text-atom
       (%number-text resolved-number environment operation)
       environment operation)
      environment emit))
    (t
     (%raise-instantiation-error environment operation
                                 "one argument must be instantiated"))))
