;;;; The atom text/symbol bijection (src/atom-name.lisp).
;;;;
;;;; Every assertion here is about one invariant: a Prolog atom IS its text.
;;;; Quoting it changes nothing (ISO 13211-1 6.4.2), case changes everything,
;;;; and the representation the engine picks -- an upcased CL-PROLOG-KIT symbol, a
;;;; verbatim CL-PROLOG-KIT.VERBATIM-ATOMS symbol, or NIL for `[]' -- must never be
;;;; observable through unification, `==/2', the standard order, the writer, or
;;;; the text-conversion builtins.

(in-package #:cl-prolog-kit.tests)

;;; Quoting is invisible: ISO 13211-1 6.4.2 makes a quoted token and the
;;; equivalent name token the same atom.

(deftest-prolog-goals quoting-does-not-change-an-atom
  "hello == 'hello'"
  "foo_bar == 'foo_bar'"
  "abc123 == 'abc123'"
  ;; An operator atom needs brackets to be an operand (ISO 6.3.4.3), but it is
  ;; still the same atom its quoted spelling denotes.
  "(+) == '+'"
  "(:-) == ':-'"
  "[] == '[]'"
  "f(a, 'b') == f('a', b)"
  ;; The predicate of a goal is an atom too, so a quoted head and an unquoted
  ;; call have to meet.
  "assertz('quoted_head'), quoted_head")

;;; Case is part of the name, so these are genuinely different atoms.

(deftest-prolog-goals atom-case-is-significant
  "'FooBar' \\== foobar"
  "'fooBar' \\== foobar"
  "'ABC' \\== abc"
  "'A' \\== a"
  "'Hello world' \\== 'hello world'"
  ;; ...and unification agrees with \==, which is the property that makes the
  ;; standard order below well defined.
  "\\+ 'ABC' = abc"
  "'ABC' = 'ABC'")

;;; The standard order of terms ranks atoms by the characters of their text
;;; (ISO 13211-1 7.2.3), not by the internal symbol name.

(deftest-prolog-goals standard-order-follows-atom-text
  ;; 'B' is code 66 and a is 97, so the upper-case atom sorts first -- the
  ;; reverse of what comparing upcased symbol names would give.
  "compare(<, 'B', a)"
  "compare(>, a, 'B')"
  "compare(=, hello, 'hello')"
  "msort([b, 'A', a], ['A', a, b])"
  ;; sort/2 removes duplicates by the standard order, so two spellings of one
  ;; atom must collapse to one element.
  "sort([b, 'hello', hello, a], [a, b, hello])")

;;; Text conversion reports the atom's text, not its representation.

(deftest-prolog-goals text-conversion-uses-atom-text
  "atom_codes(abc, [97, 98, 99])"
  "atom_chars(abc, [a, b, c])"
  "char_code(a, 97)"
  "char_code('A', 65)"
  "atom_length('FooBar', 6)"
  "atom_length([], 2)"
  "atom([])"
  "atomic([])"
  "upcase_atom(abc, 'ABC')"
  "downcase_atom('ABC', abc)"
  "atom_concat('Foo', 'Bar', 'FooBar')"
  "sub_atom('FooBar', 0, 3, _, 'Foo')"
  "read_term_from_atom('f(''Ab'')', T, []), T == f('Ab')"
  ;; Round trips through every text representation.
  "atom_codes('FooBar', C), atom_codes(A, C), A == 'FooBar'"
  "atom_chars('FooBar', C), atom_chars(A, C), A == 'FooBar'"
  "term_to_atom(f('Ab', b), A), term_to_atom(T, A), T == f('Ab', b)")

;;; The writer must emit text that reads back as the same atom.

(defparameter +round-trip-atom-texts+
  '("foo" "fooBar" "FooBar" "FOO" "A" "a" "foo bar" "Hello world" "foo_Bar"
    "abc" "ABC" "[]" "" "?x" "?X" "it's" "$VAR"
    ;; symbolic spellings, including the three the writer must keep quoted
    "+" "-" "*" "/" "\\" "=.." "@<" "\\+" ":-" "-->" "?-" "^" "@" "#$&" ".."
    "!" ";" "," "|" "{}" ".")
  "Atom texts covering every shape the writer decides quoting by.")

(deftest writeq-round-trips-every-atom-spelling ()
  (dolist (text +round-trip-atom-texts+)
    (let* ((atom (prolog-atom text))
           (rendered (prolog-term-string atom)))
      (is-equal text (prolog-atom-text atom)
                "PROLOG-ATOM-TEXT must invert PROLOG-ATOM")
      (is (eq atom (read-prolog-term rendered))
          (format nil "writeq of ~S rendered ~S, which read back as a ~
                       different atom" text rendered)))))

(defun term-contains-atom-p (term atom)
  (cond ((eq term atom) t)
        ((consp term) (or (term-contains-atom-p (car term) atom)
                          (term-contains-atom-p (cdr term) atom)))
        (t nil)))

(deftest writeq-output-reads-back-in-every-term-position ()
  "Rendering an atom at top level is not enough: the reader's treatment of an
unquoted symbolic atom depends on what follows it, so the output has to read
back as the same atom as an argument, a list element, and a list tail too."
  (dolist (text +round-trip-atom-texts+)
    (let* ((atom (prolog-atom text))
           (rendered (prolog-term-string atom)))
      (dolist (source (list rendered
                            (format nil "f(~A)" rendered)
                            (format nil "f(~A, 1)" rendered)
                            (format nil "f(1, ~A)" rendered)
                            (format nil "[~A]" rendered)
                            (format nil "[a|~A]" rendered)))
        (is (term-contains-atom-p (read-prolog-term source) atom)
            (format nil "~S rendered as ~S, which did not read back as that ~
                         atom in ~S" text rendered source))))))

(deftest writeq-quotes-only-what-cannot-read-back-bare ()
  "ISO 13211-1 6.4.2: a graphic token and the solo chars `!' and `;' are name
tokens, so quoting them would be noise.  The exceptions each have a reason —
`,' and `|' would read back as separators, and a lone `.' is the end token."
  (dolist (bare '("+" "-" "=.." "@<" "\\+" ":-" "-->" "#$&" ".." "!" ";" "{}"))
    (is-equal bare (prolog-term-string (prolog-atom bare))))
  (dolist (text '("," "|" "."))
    (is-equal (format nil "'~A'" text)
              (prolog-term-string (prolog-atom text)))))

(deftest writeq-quotes-exactly-the-atoms-that-need-it ()
  ;; A name that is already a plain atom name needs no quotes; anything else
  ;; does, or the output would read back as a different term.
  (is-equal "foo" (prolog-term-string (prolog-atom "foo")))
  (is-equal "fooBar" (prolog-term-string (prolog-atom "fooBar")))
  (is-equal "foo_bar" (prolog-term-string (prolog-atom "foo_bar")))
  (is-equal "'FooBar'" (prolog-term-string (prolog-atom "FooBar")))
  (is-equal "'ABC'" (prolog-term-string (prolog-atom "ABC")))
  (is-equal "'A'" (prolog-term-string (prolog-atom "A")))
  (is-equal "'foo bar'" (prolog-term-string (prolog-atom "foo bar")))
  ;; write/1 is unquoted, so it shows the bare text either way.
  (is-equal "FooBar"
            (with-output-to-string (stream)
              (cl-prolog-kit::%write-prolog-term-with-options
               (prolog-atom "FooBar") stream :quoted nil))))

(deftest numbervars-functor-is-the-upper-case-dollar-var ()
  "ISO 8.14.2's numbervars functor is `'$VAR'', so the distinct lower-case atom
`'$var'' must be written as an ordinary compound instead of as a variable name."
  (is-equal "A"
            (with-output-to-string (stream)
              (cl-prolog-kit::%write-prolog-term-with-options
               (list (prolog-atom "$VAR") 0) stream :numbervars t)))
  (is-equal "'$var'(0)"
            (with-output-to-string (stream)
              (cl-prolog-kit::%write-prolog-term-with-options
               (list (prolog-atom "$var") 0) stream :numbervars t))))

;;; Representation details the rest of the engine must not leak.

(deftest a-quoted-question-mark-atom-is-not-a-variable ()
  "A `?'-prefixed name is how this engine spells a logic variable internally, so
a quoted atom that starts with `?' must stay an atom in both encodings."
  (dolist (text '("?x" "?X" "?rule-program-0"))
    (let ((atom (prolog-atom text)))
      (is (not (logic-var-p atom))
          (format nil "~S must be an atom, not a variable" text))
      (is-equal text (prolog-atom-text atom)))))

(deftest uninterned-atoms-keep-their-text ()
  "The engine represents untrusted text -- a missing source's pathname, say -- as an uninterned symbol so the text is never interned.  Its text must still be the text, or the culprit in a raised error would not match the term the program passed in."
  (let ((culprit (make-symbol "/Users/Someone/Data.pl")))
    (is (not (logic-var-p culprit)))
    (is-equal "/Users/Someone/Data.pl" (prolog-atom-text culprit))
    (is (cl-prolog-kit::%same-atom-text-p
         culprit (prolog-atom "/Users/Someone/Data.pl")))))

(deftest equal-text-atoms-agree-across-unify-identity-and-order ()
  "The three ways to ask whether two atoms are the same must never disagree."
  (dolist (pair (list (list (prolog-atom "hello") 'cl-prolog-kit::hello)
                      (list (intern "LIST" '#:cl-prolog-kit.user-atoms) 'cl:list)
                      (list (make-symbol "SHARED") (make-symbol "SHARED"))))
    (destructuring-bind (left right) pair
      (is (nth-value 1 (unify left right)))
      (is (cl-prolog-kit::%term-identical-p left right))
      (is (= 0 (cl-prolog-kit::%compare-terms left right)))))
  (dolist (pair (list (list (prolog-atom "ABC") 'cl-prolog-kit::abc)
                      (list (prolog-atom "fooBar") 'cl-prolog-kit::foobar)))
    (destructuring-bind (left right) pair
      (is (not (nth-value 1 (unify left right))))
      (is (not (cl-prolog-kit::%term-identical-p left right)))
      (is (not (= 0 (cl-prolog-kit::%compare-terms left right)))))))

(cl-weave:it-property
    "an atom's text survives PROLOG-ATOM and a writeq/read round trip"
    ((text (cl-weave:gen-string
            :min-length 1 :max-length 12
            ;; Both cases, digits, and the two characters the writer has to
            ;; escape or quote around.
            :alphabet "abzABZ09_ '")))
  (cl-weave:expect-has-assertions)
  (let ((atom (prolog-atom text)))
    (is-equal text (prolog-atom-text atom))
    (is (eq atom (read-prolog-term (prolog-term-string atom))))
    ;; Same text in, same atom out -- interning is a function of the text only.
    (is (eq atom (prolog-atom (copy-seq text))))))
