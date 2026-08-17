;;;; SWI string type + string builtins + the double_quotes flag.
;;;; A Prolog string is a Common Lisp string object, written "..." in queries.

(in-package #:cl-prolog-kit.tests)

(deftest-queries string-type-classification ((make-rulebase))
  ((cl-prolog-kit::string "hi")     :succeeds)
  ((cl-prolog-kit::string abc)      :fails)
  ((cl-prolog-kit::string 42)       :fails)
  ((atomic "hi")                :succeeds)
  ((atom "hi")                  :fails)
  ((compound "hi")              :fails)
  ((cl-prolog-kit::is_list "hi")    :fails))

(deftest-queries string-standard-order ((make-rulebase))
  ((cl-prolog-kit:|==| "hi" "hi")   :succeeds)
  ((cl-prolog-kit:|==| "hi" "ho")   :fails)
  ((cl-prolog-kit::compare ?o "a" "b") :ordered (((?o . <))))
  ;; standard order: Atom < String
  ((cl-prolog-kit:|@<| abc "abc")   :succeeds)
  ((cl-prolog-kit:|@<| "abc" abc)   :fails))

(deftest-queries string-conversions ((make-rulebase))
  ((cl-prolog-kit::string_length "hello" ?n) :ordered (((?n . 5))))
  ((cl-prolog-kit::string_concat "foo" "bar" ?s) :ordered (((?s . "foobar"))))
  ((cl-prolog-kit::string_concat "fo" ?b "foobar") :ordered (((?b . "obar"))))
  ((cl-prolog-kit::string_concat ?a "bar" "foobar") :ordered (((?a . "foo"))))
  ((cl-prolog-kit::string_concat ?a ?b "ab")
   :set (((?a . "") (?b . "ab")) ((?a . "a") (?b . "b")) ((?a . "ab") (?b . ""))))
  ((cl-prolog-kit::atom_string hello ?s) :ordered (((?s . "hello"))))
  ((cl-prolog-kit::atom_string ?a "hello") :ordered (((?a . cl-prolog-kit::hello))))
  ((cl-prolog-kit::string_to_atom "hi" ?a) :ordered (((?a . cl-prolog-kit::hi))))
  ((cl-prolog-kit::number_string 42 ?s) :ordered (((?s . "42"))))
  ((cl-prolog-kit::number_string ?n "3.5") :ordered (((?n . 3.5d0))))
  ((cl-prolog-kit::number_string ?n "42") :ordered (((?n . 42))))
  ((cl-prolog-kit::string_chars "abc" ?c) :ordered (((?c cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c))))
  ((cl-prolog-kit::string_chars ?s (a b c)) :ordered (((?s . "abc"))))
  ((cl-prolog-kit::string_codes "hi" ?c) :ordered (((?c 104 105))))
  ((cl-prolog-kit::string_codes ?s (104 105)) :ordered (((?s . "hi"))))
  ((cl-prolog-kit::term_string (+ 1 2) ?s) :ordered (((?s . "1 + 2"))))
  ((cl-prolog-kit::term_string ?t "foo(a, b)") :ordered (((?t cl-prolog-kit::foo cl-prolog-kit::a cl-prolog-kit::b))))
  ((cl-prolog-kit::text_concat foo bar ?w) :ordered (((?w . cl-prolog-kit::foobar))))
  ;; Any text-like argument is coerced: float, code list, and char list.
  ((cl-prolog-kit::string_length 3.5 ?n) :ordered (((?n . 3))))
  ((cl-prolog-kit::string_length (104 105) ?n) :ordered (((?n . 2))))
  ((cl-prolog-kit::string_length (a b c) ?n) :ordered (((?n . 3)))))

(deftest-queries sub-string-builtin ((make-rulebase))
  ((cl-prolog-kit::sub_string "hello" 1 3 ?a ?sub)
   :ordered (((?a . 1) (?sub . "ell"))))
  ((cl-prolog-kit::sub_string "banana" ?b ?l ?a "an")
   :set (((?b . 1) (?l . 2) (?a . 3)) ((?b . 3) (?l . 2) (?a . 1))))
  ((cl-prolog-kit::sub_string "abc" 0 0 ?a ?sub)
   :ordered (((?a . 3) (?sub . ""))))
  ;; Before and Length both unbound: every slice is enumerated.
  ((cl-prolog-kit::sub_string "ab" ?b ?l ?a ?sub)
   :ordered (((?b . 0) (?l . 0) (?a . 2) (?sub . ""))
             ((?b . 0) (?l . 1) (?a . 1) (?sub . "a"))
             ((?b . 0) (?l . 2) (?a . 0) (?sub . "ab"))
             ((?b . 1) (?l . 0) (?a . 1) (?sub . ""))
             ((?b . 1) (?l . 1) (?a . 0) (?sub . "b"))
             ((?b . 2) (?l . 0) (?a . 0) (?sub . "")))))

(deftest number-string-integer-digit-limit-boundary ()
  ;; A 3-digit limit yields a 10-bit budget, and 999 and 1000 both have an
  ;; INTEGER-LENGTH of exactly 10, so each lands on the boundary comparison
  ;; rather than the strictly-under or strictly-over shortcut.
  (let ((*max-prolog-numeric-lexeme-length* 3))
    (assert-query (make-rulebase) (cl-prolog-kit::number_string 999 ?s)
                  :ordered (((?s . "999"))))
    (assert-query (make-rulebase) (cl-prolog-kit::number_string 1000 ?s)
                  :signals prolog-resource-error)))

(deftest-queries split-string-builtin ((make-rulebase))
  ((cl-prolog-kit::split_string "a,b,c" "," "" ?p)
   :ordered (((?p "a" "b" "c"))))
  ((cl-prolog-kit::split_string " a , b " "," " " ?p)
   :ordered (((?p "a" "b"))))
  ((cl-prolog-kit::split_string "hello" "" "" ?p)
   :ordered (((?p "hello")))))

;; A string is an atomic term: functor/=.. treat it as arity-0, copy_term and
;; ground accept it, arg rejects it, and it survives inside a compound.
(deftest-queries string-as-atomic-term ((make-rulebase))
  ((functor "hi" ?n ?a)         :ordered (((?n . "hi") (?a . 0))))
  ((functor ?t "hi" 0)          :ordered (((?t . "hi"))))
  ((cl-prolog-kit:|=..| "hi" ?l)    :ordered (((?l "hi"))))
  ((cl-prolog-kit::copy_term "hi" ?c) :ordered (((?c . "hi"))))
  ((ground "hi")                :succeeds)
  ((arg 1 "hi" ?a)              :signals)
  ((cl-prolog-kit:|==| (f "a" "b") (f "a" "b")) :succeeds)
  ((cl-prolog-kit:|\\==| "abc" abc) :succeeds)
  ;; standard order: Number < String
  ((cl-prolog-kit:|@<| 42 "abc")    :succeeds))

(deftest-queries string-conversion-errors ((make-rulebase))
  ((cl-prolog-kit::number_string ?n "notanum") :signals)
  ((cl-prolog-kit::number_string abc ?s)       :signals)
  ((cl-prolog-kit::string_chars ?s not-a-list) :signals)
  ((cl-prolog-kit::string_codes ?s not-a-list) :signals)
  ((cl-prolog-kit::string_concat "a" ?b ?whole) :signals)
  ((cl-prolog-kit::term_string ?t "foo((")    :signals))

(deftest-queries split-string-adjacent ((make-rulebase))
  ;; adjacent separators keep an empty field (no sep/pad collapse)
  ((cl-prolog-kit::split_string "a,,b" "," "" ?p) :ordered (((?p "a" "" "b"))))
  ((cl-prolog-kit::split_string "a;b,c" ",;" "" ?p) :ordered (((?p "a" "b" "c")))))

(deftest string-writer-raw-vs-quoted ()
  ;; write renders a string raw; writeq escapes " and \.
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::format "~w" ("a b")) :succeeds)
    (is (string= (get-output-stream-string output) "a b")))
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::format "~q" ("a\"b")) :succeeds)
    (is (string= (get-output-stream-string output) "\"a\\\"b\"")))
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::format "~s" ("hi")) :succeeds)
    (is (string= (get-output-stream-string output) "hi"))))

(deftest double-quotes-flag-controls-string-reading ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::read_term_from_atom "f(\"hi\")" ?t ())
                  :ordered (((?t cl-prolog-kit::f (104 105)))))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::double_quotes
                                            cl-prolog-kit.user-atoms::string)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::read_term_from_atom "f(\"hi\")" ?t ())
                  :ordered (((?t cl-prolog-kit::f "hi"))))))

(deftest double-quotes-chars-and-atom-modes ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::double_quotes
                                            cl-prolog-kit.user-atoms::chars)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::read_term_from_atom "f(\"hi\")" ?t ())
                  :ordered (((?t cl-prolog-kit::f (cl-prolog-kit::h cl-prolog-kit::i))))))
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::double_quotes
                                            cl-prolog-kit.user-atoms::atom)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::read_term_from_atom "f(\"hi\")" ?t ())
                  :ordered (((?t cl-prolog-kit::f cl-prolog-kit::hi))))))
