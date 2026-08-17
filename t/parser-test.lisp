;;;; Conventional Prolog source parser tests.

(in-package #:cl-prolog-kit.tests)

(deftest prolog-term-parser ()
  (is-equal (list 'cl-prolog-kit::person (cl-prolog-kit:prolog-atom "Mary Jane") 12 1.5d0)
            (read-prolog-term "person('Mary Jane', 12, 1.5)."))
  (is-equal '(cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c)
            (read-prolog-term "[a,b,c]"))
  (is-equal '(cl-prolog-kit::a cl-prolog-kit::b . cl-prolog-kit::?TAIL)
             (read-prolog-term "[a,b|Tail]"))
  (is-equal 1000.0d0 (read-prolog-term "1e3"))
  (is-equal 0.015d0 (read-prolog-term "1.5e-2"))
  (signals-error (read-prolog-term "[a|X,Y]"))
  (let ((term (read-prolog-term "pair(X, X, _, _)")))
    (is (eq (second term) (third term)))
    (is (not (eq (fourth term) (fifth term)))))
  (let ((nil-atom (read-prolog-term "nil"))
        (car-atom (read-prolog-term "car")))
    (is (not (null nil-atom)))
    ;; Quoting an atom cannot change which atom it is (ISO 13211-1 6.4.2), but
    ;; case is part of the name, so `'NIL'' denotes a different atom.
    (is (eq nil-atom (read-prolog-term "'nil'")))
    (is (not (eq nil-atom (read-prolog-term "'NIL'"))))
    (is (eq (symbol-package nil-atom)
            (find-package '#:cl-prolog-kit.user-atoms)))
    (is (not (eq car-atom 'cl:car)))
    (is-equal '() (read-prolog-term "[]"))
    ;; ISO 6.3.5 makes `[]' an atom, which this engine represents as NIL, so
    ;; its quoted spelling denotes the same atom.
    (is-equal '() (read-prolog-term "'[]'"))))

(deftest prolog-stream-term-reader-is-incremental ()
  (with-input-from-string
      (stream (format nil
                      "first(1.5, 'not.a.term'). % between terms~%second([a,b])."))
    (is-equal '(cl-prolog-kit.user-atoms::first 1.5d0 cl-prolog-kit::not.a.term)
              (read-prolog-term stream))
    (is-equal '(cl-prolog-kit.user-atoms::second (cl-prolog-kit::a cl-prolog-kit::b))
              (read-prolog-term stream)))
  (with-input-from-string (stream "value % comment at end of file")
    (is-equal 'cl-prolog-kit::value (read-prolog-term stream))))

(deftest parser-reads-an-operator-as-an-atom-where-it-has-no-operand ()
  "ISO 13211-1 6.3.3.1 admits an atom that is an operator as an argument and
6.3.4.3 admits it bracketed, so the Order argument of `compare/3', the
specifier of `sort/4' and the name argument of `op/3' can all be written."
  (is-equal (list 'cl-prolog-kit::f (prolog-atom "+") 1)
            (read-prolog-term "f(+, 1)"))
  (is-equal (list (prolog-atom "+") (prolog-atom "-"))
            (read-prolog-term "[+, -]"))
  (is-equal (prolog-atom "@<") (read-prolog-term "(@<)"))
  (is-equal (list 'cl-prolog-kit::f (prolog-atom "-")) (read-prolog-term "f(-)"))
  ;; A prefix operator with an operand still applies to it.
  (is-equal -1 (read-prolog-term "- 1"))
  (is-equal '(cl-prolog-kit::- 1 2) (read-prolog-term "1 - 2"))
  (is-equal '(cl-prolog-kit::not cl-prolog-kit:fail) (read-prolog-term "\\+ fail"))
  ;; ...and a closing delimiter where a term belongs is still a syntax error,
  ;; rather than the atom of that name.
  (signals-error (read-prolog-term "f(, 1)"))
  (signals-error (read-prolog-term "f(a, )")))

(deftest parser-reads-a-graphic-run-as-one-token ()
  "ISO 13211-1 6.4.2 makes a maximal run of graphic characters one token, and an
undeclared one is an atom -- which is what lets `:- op(700, xfx, ===).' name an
operator before it exists.  Matching the longest declared operator instead
would split `===' into `==' and `='."
  (is-equal "===" (prolog-atom-text (read-prolog-term "(===)")))
  (is-equal "@#$" (prolog-atom-text (read-prolog-term "(@#$)")))
  (is-equal '(cl-prolog-kit::op 700 cl-prolog-kit::xfx #.(cl-prolog-kit:prolog-atom "==="))
            (read-prolog-term "op(700, xfx, ===)"))
  ;; A declared operator run still lexes as that operator, and the end token is
  ;; still a lone `.' followed by layout or end of input.
  (is-equal '(cl-prolog-kit::|=..| cl-prolog-kit::?X (cl-prolog-kit::a))
            (read-prolog-term "X =.. [a]."))
  ;; A block comment still opens where it would without the run.
  (is-equal '(cl-prolog-kit::+ cl-prolog-kit::a cl-prolog-kit::b)
            (read-prolog-term "a +/* note */ b")))

(deftest prolog-clause-parser ()
  (let ((fact (read-prolog-clause "parent(tom, bob)."))
        (rule (read-prolog-clause "ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).")))
    (is-equal '(cl-prolog-kit::parent cl-prolog-kit::tom cl-prolog-kit::bob) (clause-head fact))
    (is-equal '() (clause-body fact))
    (is-equal '(cl-prolog-kit::ancestor cl-prolog-kit::?X cl-prolog-kit::?Y) (clause-head rule))
    (is-equal '((cl-prolog-kit::parent cl-prolog-kit::?X cl-prolog-kit::?Z)
                (cl-prolog-kit::ancestor cl-prolog-kit::?Z cl-prolog-kit::?Y))
              (clause-body rule)))
  (let ((rule (read-prolog-clause "ready :- true.")))
    (is-equal '(cl-prolog-kit::ready) (clause-head rule))
    (is-equal '(cl-prolog-kit::true) (clause-body rule))))

(deftest prolog-operator-parser ()
  (is-equal '(cl-prolog-kit::or (cl-prolog-kit::and cl-prolog-kit::p cl-prolog-kit::q) cl-prolog-kit::r)
            (read-prolog-term "p, q ; r"))
  (is-equal '(cl-prolog-kit::if-then-else cl-prolog-kit::p cl-prolog-kit::q cl-prolog-kit::r)
            (read-prolog-term "p -> q ; r"))
  (is-equal '(cl-prolog-kit::soft-if-then-else cl-prolog-kit::p cl-prolog-kit::q cl-prolog-kit::r)
            (read-prolog-term "p *-> q ; r"))
  (is-equal '(not (= cl-prolog-kit::?X 1)) (read-prolog-term "\\+ X = 1"))
  (is-equal '(is cl-prolog-kit::?X (+ 1 (* 2 3))) (read-prolog-term "X is 1 + 2 * 3"))
  (signals-error (read-prolog-term "2 ** 3 ** 4"))
  (is-equal '(cl-prolog-kit::^ 2 (cl-prolog-kit::^ 3 4))
            (read-prolog-term "2 ^ 3 ^ 4"))
  (is-equal '(cl-prolog-kit::rem (cl-prolog-kit::div (cl-prolog-kit::// 20 3) 2) 2)
            (read-prolog-term "20 // 3 div 2 rem 2"))
  (is-equal '(cl-prolog-kit::mod 17 5) (read-prolog-term "17 mod 5"))
  (is-equal '(cl-prolog-kit::@< cl-prolog-kit::a cl-prolog-kit::b)
            (read-prolog-term "a @< b"))
  (is-equal '(cl-prolog-kit:|\\=| cl-prolog-kit::?X cl-prolog-kit::?Y)
            (read-prolog-term "X \\= Y"))
  (is-equal '(cl-prolog-kit::=.. cl-prolog-kit::?X
              (cl-prolog-kit::foo cl-prolog-kit::a cl-prolog-kit::b))
             (read-prolog-term "X =.. [foo,a,b]"))
  (dolist (operator '("=" "\\=" "==" "\\==" "=:=" "=\\=" "=.."
                      "=<" ">=" "<" ">" "is"))
    (signals-error
     (read-prolog-term (format nil "X ~A Y ~A Z" operator operator))))
  (signals-error (read-prolog-term "X = Y < Z"))
  (signals-error (read-prolog-term "X = \\+ p"))
  (is-equal '(cl-prolog-kit::pair (+ 1 2) (* 3 4))
            (read-prolog-term "pair(1 + 2, 3 * 4)")))

(deftest prolog-operator-table-drives-parser ()
  (dolist (case '(("1 + 2 * 3" (+ 1 (* 2 3)))
                  ("8 - 3 - 1" (- (- 8 3) 1))
                  ("2 ^ 3 ^ 4" (cl-prolog-kit::^ 2 (cl-prolog-kit::^ 3 4)))
                  ("\\+ X = 1" (not (= cl-prolog-kit::?X 1)))))
    (destructuring-bind (source expected) case
      (is-equal expected (read-prolog-term source))))
  (dolist (definition
           (cl-prolog-kit::%operator-table-current
            cl-prolog-kit::*standard-operator-table*))
    (let ((lexeme (cl-prolog-kit::%operator-lexeme definition)))
      (if (cl-prolog-kit::%plain-prolog-atom-name-p lexeme)
          (is (member lexeme (cl-prolog-kit::%standard-operator-lexemes t)
                      :test #'string=))
          (is (member lexeme (cl-prolog-kit::%symbolic-token-lexemes)
                      :test #'string=))))))

(deftest operator-lexeme-generation-does-not-mutate-delimiters ()
  (dotimes (index 20)
    (declare (ignorable index))
    (is-equal 8
              (length (cl-prolog-kit::%compute-symbolic-token-lexemes
                       (cl-prolog-kit::%make-operator-table '())))))
  (is-equal 3
            (length (parse-prolog
                     "fact(a). rule(X) :- fact(X). ?- rule(X)."))))
(deftest quoted-question-atoms-use-distinct-atom-namespace ()
  (let* ((atom (read-prolog-term "'?x'."))
         (printed (prolog-term-string atom))
         (round-trip
           (read-prolog-term (concatenate 'string printed "."))))
    (is (eq (find-package '#:cl-prolog-kit.user-atoms)
            (symbol-package atom)))
    (is (not (logic-var-p atom)))
    (is (not (eq atom 'cl-prolog-kit::?x)))
    (is-equal "'?x'" printed)
    (is (eq atom round-trip)))
  (let* ((rulebase (consult-prolog "p('?x')."))
         (atom-query (read-prolog-term "p('?x')."))
         (other-query (read-prolog-term "p(a).")))
    (is-equal '(nil) (query-prolog rulebase atom-query))
    (is (prolog-succeeds-p rulebase atom-query))
    (is (not (prolog-succeeds-p rulebase other-query)))))

(deftest quoted-atoms-decode-escape-sequences ()
  "ISO 13211-1 6.4.2.1: a `\\'-escape in a quoted token denotes the character it
names, so `'a\\nb'' holds a newline rather than the letter n."
  (is-equal "it's" (prolog-atom-text (read-prolog-term "'it''s'.")))
  (is-equal (format nil "a~Cb" #\Newline)
            (prolog-atom-text (read-prolog-term "'a\\nb'.")))
  (is-equal (format nil "a~Cb" #\Tab)
            (prolog-atom-text (read-prolog-term "'a\\tb'.")))
  (is-equal "a\\b" (prolog-atom-text (read-prolog-term "'a\\\\b'.")))
  (is-equal "a'b" (prolog-atom-text (read-prolog-term "'a\\'b'.")))
  ;; \xHH\ and \OOO\ name a character by code, in hex and octal.
  (is-equal "aAb" (prolog-atom-text (read-prolog-term "'a\\x41\\b'.")))
  (is-equal "aAb" (prolog-atom-text (read-prolog-term "'a\\101\\b'.")))
  ;; A `\'-newline continuation contributes nothing, so the atom is one word.
  (is-equal "ab" (prolog-atom-text
                  (read-prolog-term (format nil "'a\\~Cb'." #\Newline))))
  ;; The same decoding applies inside a "..." literal.
  (is-equal (list (char-code #\Newline))
            (read-prolog-term "\"\\n\".")))

(deftest numeric-literals-cover-the-iso-notations ()
  "ISO 13211-1 6.4.4: a character-code constant and radix constants."
  (is-equal 97 (read-prolog-term "0'a."))
  (is-equal 39 (read-prolog-term "0''."))
  (is-equal 39 (read-prolog-term "0'''."))
  (is-equal (char-code #\Newline) (read-prolog-term "0'\\n."))
  (is-equal 65 (read-prolog-term "0'\\x41\\."))
  (is-equal 255 (read-prolog-term "0xff."))
  (is-equal 255 (read-prolog-term "0xFF."))
  (is-equal 15 (read-prolog-term "0o17."))
  (is-equal 5 (read-prolog-term "0b101."))
  ;; A `0' that begins no such constant is still the plain integer zero.
  (is-equal 0 (read-prolog-term "0."))
  (is-equal 0.5d0 (read-prolog-term "0.5."))
  (is-equal '(cl-prolog-kit::f 0 cl-prolog-kit::x) (read-prolog-term "f(0, x)"))
  ;; ...and the notations survive the stream splitter, not just a string.
  (with-input-from-string (stream "code(0'a). code(0'').")
    (is-equal '(cl-prolog-kit::code 97) (read-prolog-term stream))
    (is-equal '(cl-prolog-kit::code 39) (read-prolog-term stream))))

(deftest bitwise-operators-are-declared ()
  "ISO 13211-1 6.3.4.4 table 7 declares the bitwise operators, without which
their evaluable functors have no written form."
  (dolist (case '(("X is 1 << 3" 8)
                  ("X is 8 >> 3" 1)
                  ("X is 12 /\\ 10" 8)
                  ("X is 12 \\/ 10" 14)
                  ("X is 12 xor 10" 6)
                  ("X is \\ 0" -1)))
    (destructuring-bind (source expected) case
      (let ((solutions (query-prolog (make-rulebase) (read-prolog-term source))))
        (is-equal expected (solution-binding 'cl-prolog-kit::?X (first solutions))
                  source)))))

(deftest raw-term-source-reader-tracks-comments-quotes-and-decimal-points ()
  (dolist (text (list "X is 4/2."
                       "foo /* comment */ bar."
                       "X is 3.14."
                       "'a\\b'."
                       "'it''s'."
                       "\"a\\b\"."
                       "\"it\"\"s\"."))
    (is-equal text
              (with-input-from-string (stream text)
                (cl-prolog-kit::%read-prolog-term-source stream)))))

(deftest prolog-source-parser-and-consult ()
  (let* ((source (format nil "% family~% parent(tom,bob). /* rule */~% child(X) :- parent(tom,X).~% ?- child(X)."))
         (forms (parse-prolog source)))
    (is-equal 3 (length forms))
    (is (clause-p (first forms)))
    (is (clause-p (second forms)))
    (is-equal '(cl-prolog-kit::child cl-prolog-kit::?X) (third forms)))
  (let ((rulebase (consult-prolog "edge(a,b). edge(b,c).")))
    (assert-query rulebase (cl-prolog-kit::edge cl-prolog-kit::a ?x)
                  :ordered (((?x . cl-prolog-kit::b)))))
  (signals-error (consult-prolog "?- true."))
  (let ((rulebase (make-rulebase)))
    (signals-error (consult-prolog "kept. ?- kept." rulebase))
    (is-equal '() (rulebase-visible-clauses rulebase))))

(defun %parser-resource-condition (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (prolog-parser-resource-error (condition)
      condition)))

(defun %assert-parser-resource-error (thunk resource-name)
  "Run THUNK, assert it signals a PROLOG-PARSER-RESOURCE-ERROR whose
resource is RESOURCE-NAME, and return the condition for further checks
against its :OBSERVED/:LIMIT/:POSITION, which vary per resource kind."
  (let ((condition (%parser-resource-condition thunk)))
    (is condition)
    (is-equal resource-name (prolog-parser-resource-error-resource condition))
    condition))

(defun %prolog-parse-error-p (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (cl-prolog-kit::prolog-parse-error ()
      t)))

(defun %prolog-parse-error-condition (thunk)
    (handler-case
        (progn
          (funcall thunk)
          nil)
      (cl-prolog-kit::prolog-parse-error (condition)
        condition)))

  (defun %assert-prolog-parse-error (thunk expected-description)
    (let ((condition (%prolog-parse-error-condition thunk)))
      (is (typep condition 'cl-prolog-kit::prolog-parse-error))
      (when condition
        (is-equal expected-description
                  (cl-prolog-kit::prolog-parse-error-description condition)))
      condition))

  (deftest lexer-rejects-malformed-source-for-string-and-stream-input ()
    (dolist (case
             (list
              (list "unterminated block comment"
                    "/* unterminated"
                    "Unterminated Prolog block comment."
                    "Unexpected end of Prolog input while reading BLOCK-COMMENT.")
              (list "unterminated quoted atom"
                    "'unterminated"
                    "Unterminated quoted Prolog atom."
                    "Unexpected end of Prolog input while reading QUOTED.")
              (list "unterminated string"
                    "\"unterminated"
                    "Unterminated Prolog string."
                    "Unexpected end of Prolog input while reading DQUOTED.")
              (list "malformed character escape"
                    "'\\xz'"
                    "Malformed Prolog character escape at position 3."
                    "Malformed Prolog character escape at position 3.")
              (list "out-of-range character escape"
                    "'\\x110000z'"
                    "Prolog character escape \"110000\" is out of range."
                    "Prolog character escape \"110000\" is out of range.")
              (list "truncated character-code constant"
                    "0'"
                    "Unterminated Prolog character-code constant."
                    "Unterminated Prolog character-code constant.")
              (list "malformed exponent"
                    "1e+"
                    "Malformed Prolog exponent."
                    "Malformed Prolog exponent.")))
      (destructuring-bind (name source string-description stream-description) case
        (declare (ignore name))
        (%assert-prolog-parse-error
         (lambda () (read-prolog-term source))
         string-description)
        (with-input-from-string (stream source)
          (%assert-prolog-parse-error
           (lambda () (read-prolog-term stream))
           stream-description)))))

  (deftest prolog-parse-error-report-writes-its-description ()
    (let ((condition (make-condition 'cl-prolog-kit::prolog-parse-error
                                     :description "unexpected end of input")))
      (is-equal "unexpected end of input" (princ-to-string condition))))

(deftest prolog-parser-resource-error-report-includes-every-slot ()
  (let* ((*max-prolog-source-characters* 1)
         (condition (%assert-parser-resource-error
                     (lambda () (read-prolog-term "ab")) "SOURCE_CHARACTERS")))
    (is-equal
     "Prolog parser resource SOURCE_CHARACTERS exceeded limit 1 at position 2 (observed 2)."
     (princ-to-string condition))))

(deftest prolog-parser-enforces-source-and-token-limits ()
  (let ((*max-prolog-source-characters* 1))
    (is-equal (quote cl-prolog-kit::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-source-characters* 1)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "ab")) "SOURCE_CHARACTERS")))
    (is-equal 1 (prolog-parser-resource-error-limit condition))
    (is-equal 2 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition)))
  (with-input-from-string (stream "a.")
    (let ((*max-prolog-source-characters* 2))
      (is-equal (quote cl-prolog-kit::a)
                (read-prolog-term stream))))
  (with-input-from-string (stream "a.")
    (let* ((*max-prolog-source-characters* 1)
           (condition
             (%assert-parser-resource-error
              (lambda () (read-prolog-term stream)) "SOURCE_CHARACTERS")))
      (is-equal 2 (prolog-parser-resource-error-position condition))))
  (let ((*max-prolog-tokens* 1))
    (is-equal (quote cl-prolog-kit::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-tokens* 0)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "a")) "TOKEN_COUNT")))
    (is-equal 0 (prolog-parser-resource-error-limit condition))
    (is-equal 1 (prolog-parser-resource-error-observed condition))
    (is-equal 0 (prolog-parser-resource-error-position condition))))

(deftest prolog-parser-enforces-lexeme-limits ()
  (let ((*max-prolog-identifier-length* 3))
    (is-equal (quote cl-prolog-kit::abc)
              (read-prolog-term "abc")))
  (let* ((*max-prolog-identifier-length* 2)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "abc")) "IDENTIFIER_LENGTH")))
    (is-equal 3 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition)))
  (let ((source (format nil "~Cabc~C" (code-char 39) (code-char 39))))
    (let ((*max-prolog-quoted-lexeme-length* 3))
      (let ((term (read-prolog-term source)))
        (is-equal "abc" (prolog-atom-text term))
        (is (eq (find-package "CL-PROLOG-KIT")
                (symbol-package term)))))
    (let* ((*max-prolog-quoted-lexeme-length* 2)
           (condition
             (%assert-parser-resource-error
              (lambda () (read-prolog-term source)) "QUOTED_LEXEME_LENGTH")))
      (is-equal 3 (prolog-parser-resource-error-observed condition))))
  (let ((*max-prolog-numeric-lexeme-length* 3))
    (is-equal 123 (read-prolog-term "123")))
  (let* ((*max-prolog-numeric-lexeme-length* 2)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "123")) "NUMERIC_LEXEME_LENGTH")))
    (is-equal 3 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition))))

(deftest prolog-parser-enforces-structural-limits ()
  (let ((*max-prolog-delimiter-depth* 1))
    (is-equal (quote cl-prolog-kit::a)
              (read-prolog-term "(a)")))
  (let ((*max-prolog-delimiter-depth* 2))
    (is-equal (quote (cl-prolog-kit::f cl-prolog-kit::a cl-prolog-kit::b))
              (read-prolog-term "f((a),(b))")))
  (let* ((*max-prolog-delimiter-depth* 1)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "((a))")) "DELIMITER_DEPTH")))
    (is-equal 2 (prolog-parser-resource-error-observed condition)))
  (let ((*max-prolog-parser-depth* 1))
    (is-equal (quote cl-prolog-kit::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-delimiter-depth* nil)
         (*max-prolog-parser-depth* 1)
         (condition
           (%assert-parser-resource-error
            (lambda () (read-prolog-term "(a)")) "PARSER_DEPTH")))
    (is-equal 2 (prolog-parser-resource-error-observed condition)))
  (dolist (source (list "(a]" ")"))
    (is (%prolog-parse-error-p
         (lambda () (read-prolog-term source)))))
  (dolist (source (list "(a]" ")"))
    (with-input-from-string (stream source)
      (is (%prolog-parse-error-p
           (lambda () (read-prolog-term stream)))))))

(deftest prolog-parser-bounds-interning-and-operator-caches ()
  (let ((cl-prolog-kit::*parser-interned-symbols*
          (make-hash-table :test (function equal)))
        (*max-prolog-interned-symbols* 1))
    (read-prolog-term "security_parser_atom_7f41a")
    (%assert-parser-resource-error
     (lambda () (read-prolog-term "security_parser_atom_7f41b"))
     "INTERNED_SYMBOLS"))
  (let ((cl-prolog-kit::*parser-interned-symbols*
          (make-hash-table :test (function equal)))
        (*max-prolog-interned-symbols* 1))
    (read-prolog-term "SecurityParserVar7f41a")
    (%assert-parser-resource-error
     (lambda () (read-prolog-term "SecurityParserVar7f41b"))
     "INTERNED_SYMBOLS"))
  (let ((name "security_unknown_operator_7f41a"))
    (is (null (nth-value
               1
               (find-symbol (string-upcase name)
                            (find-package "CL-PROLOG-KIT")))))
    (is (null
         (cl-prolog-kit::%operator-definition-for-token
          (cl-prolog-kit::%parser #() cl-prolog-kit::*standard-operator-table*)
          (cl-prolog-kit::%token :operator name 0)
          (list :xfx))))
    (is (null (nth-value
               1
               (find-symbol (string-upcase name)
                            (find-package "CL-PROLOG-KIT"))))))
  (let ((cl-prolog-kit::*operator-lexeme-cache*
          (make-hash-table :test (function eq))))
    (dotimes (index 20)
      (declare (ignorable index))
      (cl-prolog-kit::%operator-table-lexemes
       (cl-prolog-kit::%make-operator-table (list))))
    (is-equal 0
              (hash-table-count cl-prolog-kit::*operator-lexeme-cache*))
    (cl-prolog-kit::%operator-table-lexemes
     cl-prolog-kit::*standard-operator-table*)
    (is-equal 1
              (hash-table-count cl-prolog-kit::*operator-lexeme-cache*))
    (setf (gethash 'stale-entry cl-prolog-kit::*operator-lexeme-cache*) :stale)
    (is-equal 2
              (hash-table-count cl-prolog-kit::*operator-lexeme-cache*))
    (cl-prolog-kit::%operator-table-lexemes
     cl-prolog-kit::*standard-operator-table*)
    (is-equal 1
              (hash-table-count cl-prolog-kit::*operator-lexeme-cache*))))
