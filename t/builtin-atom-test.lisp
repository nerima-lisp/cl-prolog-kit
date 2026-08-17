;;;; Atom, character, and numeric text builtin contract.

(in-package #:cl-prolog-kit.tests)

(defmacro deftest-bidirectional-queries (name (rulebase-form) &body cases)
  "Define NAME from paired query cases that share a predicate shape.

Each case is (PREDICATE FORWARD-INPUT FORWARD-OUTPUT FORWARD-EXPECTED
                     REVERSE-INPUT REVERSE-OUTPUT REVERSE-EXPECTED).
The macro expands each case into two DEFTEST-QUERIES specs."
  `(deftest-queries ,name (,rulebase-form)
     ,@(loop for case in cases
             append (destructuring-bind (predicate forward-input forward-output
                                          forward-expected reverse-input
                                          reverse-output reverse-expected)
                        case
                      `(((,predicate ,forward-input ,forward-output)
                         :ordered ,forward-expected)
                        ((,predicate ,reverse-input ,reverse-output)
                         :ordered ,reverse-expected))))))

(deftest-queries atom-builtins ((make-rulebase))
  ((cl-prolog-kit::atom_length cl-prolog-kit::hello ?length) :ordered (((?length . 5))))
  ((cl-prolog-kit::atom_length cl-prolog-kit::hello 5) :succeeds)
  ((cl-prolog-kit::atom_length cl-prolog-kit::hello 4) :fails)
  ((cl-prolog-kit::atom_concat cl-prolog-kit::hello cl-prolog-kit::world ?whole)
   :ordered (((?whole . cl-prolog-kit::helloworld))))
  ((cl-prolog-kit::atom_concat ?left ?right cl-prolog-kit::abc)
   :ordered (((?left . cl-prolog-kit::||) (?right . cl-prolog-kit::abc))
       ((?left . cl-prolog-kit::a) (?right . cl-prolog-kit::bc))
       ((?left . cl-prolog-kit::ab) (?right . cl-prolog-kit::c))
       ((?left . cl-prolog-kit::abc) (?right . cl-prolog-kit::||))))
  ((cl-prolog-kit::atom_concat cl-prolog-kit::a ?right cl-prolog-kit::abc)
   :ordered (((?right . cl-prolog-kit::bc))))
  ((cl-prolog-kit::atom_concat ?left cl-prolog-kit::bc cl-prolog-kit::abc)
   :ordered (((?left . cl-prolog-kit::a))))
  ((cl-prolog-kit::atom_concat ?left cl-prolog-kit::bcdef cl-prolog-kit::abc)
   :fails)
  ((cl-prolog-kit::atom_concat cl-prolog-kit::left ?right ?whole)
   :signals)
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc ?before ?length ?after ?sub)
   :succeeds)
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc 1 1 ?after ?sub)
   :ordered (((?after . 1) (?sub . cl-prolog-kit::b))))
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc ?before 2 ?after ?sub)
   :ordered (((?before . 0) (?after . 1) (?sub . cl-prolog-kit::ab))
       ((?before . 1) (?after . 0) (?sub . cl-prolog-kit::bc))))
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc ?before ?length 1 ?sub)
   :ordered (((?before . 0) (?length . 2) (?sub . cl-prolog-kit::ab))
       ((?before . 1) (?length . 1) (?sub . cl-prolog-kit::b))
       ((?before . 2) (?length . 0) (?sub . cl-prolog-kit::||))))
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc ?before 3 0 ?sub)
   :ordered (((?before . 0) (?sub . cl-prolog-kit::abc))))
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc 1 ?length ?after ?sub)
   :ordered (((?length . 0) (?after . 2) (?sub . cl-prolog-kit::||))
       ((?length . 1) (?after . 1) (?sub . cl-prolog-kit::b))
       ((?length . 2) (?after . 0) (?sub . cl-prolog-kit::bc))))
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc 2 ?length 2 ?sub)
   :fails)
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc ?before 5 0 ?sub)
   :fails)
  ((cl-prolog-kit::sub_atom cl-prolog-kit::abc 3 3 ?after ?sub)
   :fails))

(deftest-bidirectional-queries atom-builtins-text ((make-rulebase))
  (cl-prolog-kit::atom_chars cl-prolog-kit::abc ?chars
   (((?chars cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c)))
   ?atom (cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c)
   (((?atom . cl-prolog-kit::abc))))
  ;; The codes of the atom's text, `abc', not of the upcased symbol name that
  ;; happens to represent it: ISO 13211-1 8.16.5.
  (cl-prolog-kit::atom_codes cl-prolog-kit::abc ?codes
   (((?codes 97 98 99)))
   ?atom (97 98 99)
   (((?atom . cl-prolog-kit::abc))))
  (cl-prolog-kit::char_code cl-prolog-kit::a ?code
   (((?code . 97)))
   ?character 97
   (((?character . cl-prolog-kit::a))))
  (cl-prolog-kit::number_chars 42 ?chars
   (((?chars cl-prolog-kit::|4| cl-prolog-kit::|2|)))
   ?number (cl-prolog-kit::|4| cl-prolog-kit::|2|)
   (((?number . 42))))
  (cl-prolog-kit::number_codes -17 ?codes
   (((?codes 45 49 55)))
   ?number (45 49 55)
   (((?number . -17))))
  (cl-prolog-kit::atom_number cl-prolog-kit::|42| ?number
   (((?number . 42)))
   ?atom 42
   (((?atom . cl-prolog-kit::|42|)))))

(deftest-queries atom-number-builtins ((make-rulebase))
  ((cl-prolog-kit::atom_number cl-prolog-kit::|-0.125| ?number)
   :ordered (((?number . -0.125d0))))
  ((cl-prolog-kit::atom_number cl-prolog-kit::|42| 42) :succeeds)
  ((cl-prolog-kit::atom_number cl-prolog-kit::|42| 43) :fails)
  ((cl-prolog-kit::atom_number cl-prolog-kit::bad ?number) :fails))

(defun parse-number-codes (codes)
  (let ((solutions (query-prolog
                    (make-rulebase)
                    `(cl-prolog-kit::number_codes ?number ,codes))))
    (cdr (assoc '?number (first solutions)))))

(defun number-codes-error-type (codes)
  (query-error-summary
   (make-rulebase) `(cl-prolog-kit::number_codes ?number ,codes)))

(defun round-trip-number-codes (number)
  (let* ((encoded (query-prolog
                   (make-rulebase)
                   `(cl-prolog-kit::number_codes ,number ?codes)))
         (codes (cdr (assoc '?codes (first encoded)))))
    (parse-number-codes codes)))

(defun number-output-error-type (number)
  (query-error-summary
   (make-rulebase) `(cl-prolog-kit::number_codes ,number ?codes)))

(deftest-table atom-number-text-grammar ()
  (:equal 0 (parse-number-codes '(48)))
  (:equal 17 (parse-number-codes '(43 49 55)))
  (:equal -17 (parse-number-codes '(45 49 55)))
  (:equal 12.5d0 (parse-number-codes '(49 50 46 53)))
  (:equal 1250.0d0 (parse-number-codes '(49 46 50 53 69 43 51)))
  (:equal 0.0125d0 (parse-number-codes '(49 46 50 53 101 45 50)))
  ;; ISO 13211-1 8.16.8.3: codes that do not spell a number are a syntax_error,
  ;; the same class the reader raises for unreadable text.
  (:equal 'prolog-syntax-error (number-codes-error-type '(49 47 50)))
  (:equal 'prolog-syntax-error (number-codes-error-type '(49 46)))
  (:equal 'prolog-syntax-error (number-codes-error-type '(46 53)))
  (:equal 'prolog-syntax-error (number-codes-error-type '(49 101)))
  (:equal 'prolog-syntax-error (number-codes-error-type '(49 50 120)))
  (:equal 'prolog-resource-error
          (number-codes-error-type
           (map 'list #'char-code
                (format nil "1e~A" (make-string 40 :initial-element #\9)))))
  ;; An exponent within the magnitude bound but beyond what a double can hold is
  ;; rejected by the reader as an ill-formed number token.  The bound above is
  ;; what stops unbounded work; this one only has to be refused, not classified.
  (:equal 'prolog-syntax-error
          (number-codes-error-type
           (map 'list #'char-code "1.0e4095"))))

(deftest-table atom-number-text-round-trips ()
  (:equal 42 (round-trip-number-codes 42))
  (:equal 12.5d0 (round-trip-number-codes 12.5d0))
  (:equal -0.125d0 (round-trip-number-codes -0.125d0))
  (:equal 1.25d20 (round-trip-number-codes 1.25d20))
  (:equal 1.25d-20 (round-trip-number-codes 1.25d-20))
  (:equal 'prolog-domain-error
          (number-output-error-type 1/2)))

(deftest-table atom-list-conversion-rejects-malformed-lists ()
  (:signals-condition prolog-type-error
   (let ((cycle (list (cl-prolog-kit::%text-atom "a"))))
     (setf (cdr cycle) cycle)
     (cl-prolog-kit::%character-list-text
      cycle nil (cl-prolog-kit::%iso-atom "ATOM_CHARS")))
   "Cyclic lists must raise a Prolog type error")
  (:signals-condition prolog-instantiation-error
   (cl-prolog-kit::%character-list-text
    (cons (cl-prolog-kit::%text-atom "a") '?tail)
    nil (cl-prolog-kit::%iso-atom "ATOM_CHARS"))
   "A variable list tail must raise an instantiation error")
  (:signals-condition prolog-type-error
   (cl-prolog-kit::%character-list-text
    (cons (cl-prolog-kit::%text-atom "a") (cl-prolog-kit::%text-atom "b"))
    nil (cl-prolog-kit::%iso-atom "ATOM_CHARS"))
   "A non-cons, non-nil tail must raise a type error")
  (:signals-condition prolog-instantiation-error
   (cl-prolog-kit::%character-list-text
    (list (cl-prolog-kit::%text-atom "a") '?element)
    nil (cl-prolog-kit::%iso-atom "ATOM_CHARS"))
   "An unbound list element must raise an instantiation error"))

(deftest resource-limit-check-rejects-values-past-the-configured-limit ()
  (signals-prolog-condition prolog-resource-error
    (cl-prolog-kit::%check-resource-limit
     5 3 "TEST_RESOURCE" nil (cl-prolog-kit::%iso-atom "TEST") "over limit")
    "A value past the configured limit must raise a resource error"))

(defun atom-builtin-error-summary (goal)
  (query-error-summary (make-rulebase) goal :with-data t))

(deftest-table atom-builtins-report-iso-errors ()
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::atom_length ?atom ?length)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_length 1 ?length)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "NOT_LESS_THAN_ZERO" -1))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_length atom -1)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::atom_concat ?a right ?whole)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 3))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_concat 3 right whole)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::sub_atom ?atom 0 1 ?after ?sub)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" 1.5))
          (atom-builtin-error-summary '(cl-prolog-kit::sub_atom abc 1.5 1 ?after ?sub)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 42))
          (atom-builtin-error-summary '(cl-prolog-kit::sub_atom abc 0 1 ?after 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::atom_chars ?atom ?chars)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_chars ?atom (ab))))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_codes ?atom (atom))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::char_code ?character ?code)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (atom-builtin-error-summary '(cl-prolog-kit::char_code ab ?code)))
  ;; ISO 13211-1 8.16.6.3: an integer that names no character is a
  ;; representation_error, which reports only the flag, not the culprit.
  (:equal '(prolog-representation-error ("REPRESENTATION_ERROR" "CHARACTER_CODE"))
          (atom-builtin-error-summary '(cl-prolog-kit::char_code ?character -1)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog-kit::number_chars atom ?chars)))
  ;; ISO 13211-1 8.16.8.3 classifies unreadable numeric text as a syntax_error,
  ;; whose culprit is the offending text itself.
  (:equal '(prolog-syntax-error ("SYNTAX_ERROR" "bad"))
          (atom-builtin-error-summary '(cl-prolog-kit::number_codes ?number (98 97 100))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog-kit::atom_number ?atom ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_number 1 ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_number ?atom atom)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "PROLOG_NUMBER" 1/2))
          (atom-builtin-error-summary '(cl-prolog-kit::atom_number ?atom 1/2)))
  ;; A short text can still demand an enormous float.  The exponent bound is
  ;; checked before the reader sees the text, in two steps: too many exponent
  ;; digits to be worth parsing, then a parsed exponent past the limit.
  (:equal '(prolog-resource-error ("RESOURCE_ERROR" "EXPONENT_MAGNITUDE"))
          (atom-builtin-error-summary
           (list 'cl-prolog-kit::atom_number (prolog-atom "1.0e5000") '?number)))
  (:equal '(prolog-resource-error ("RESOURCE_ERROR" "EXPONENT_MAGNITUDE"))
          (atom-builtin-error-summary
           (list 'cl-prolog-kit::atom_number (prolog-atom "1.0e500000") '?number))))

(deftest number-text-rejects-a-number-that-is-not-real ()
  "%NUMBER-TEXT is the single place a number becomes its Prolog text.  Its
callers all screen their argument first, so the two rejections below are only
reachable from Lisp -- but they are what makes the function total: a ratio is a
number that is not a Prolog one (domain), while a complex is not even a real
number to begin with (type)."
  (flet ((summary (number)
           (handler-case
               (progn (cl-prolog-kit::%number-text
                       number nil (cl-prolog-kit::%io-operation "NUMBER_TEXT"))
                      nil)
             (prolog-runtime-error (condition)
               (list (type-of condition)
                     (normalize-error-data
                      (second (prolog-exception-term condition))))))))
    (is-equal '(prolog-domain-error ("DOMAIN_ERROR" "PROLOG_NUMBER" 3/2))
              (summary 3/2))
    (is-equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" #C(1 2)))
              (summary (complex 1 2)))))
