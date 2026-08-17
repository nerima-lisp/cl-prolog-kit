;;;; Dynamic-database builtin tests: assert/retract/abolish,
;;;; current_predicate/2, predicate_property/2, and clause/2.

(in-package #:cl-prolog-kit.tests)

(deftest dynamic-database-builtins ()
  (let ((rulebase (make-rulebase)))
    (rulebase-insert-clause! rulebase (make-clause '(ordered first)))
    (rulebase-insert-clause! rulebase (make-clause '(ordered second)))
    (assert-query rulebase (ordered ?value)
                  :ordered (((?value . first)) ((?value . second))))
    (assert-query rulebase (assertz (color apple red)) :succeeds)
    (assert-query rulebase (assertz (color apple blue)) :succeeds)
    (assert-query rulebase (assert (color banana yellow)) :succeeds)
    (assert-query rulebase (asserta (color apple green)) :succeeds)
    (assert-query rulebase (color apple ?shade)
                  :ordered (((?shade . green)) ((?shade . red)) ((?shade . blue))))
    (assert-query rulebase
                  (assertz (:- (warm ?x) (color ?x red))) :succeeds)
    (assert-query rulebase (warm apple) :succeeds)
    (assert-query rulebase (clause (color apple red) ?body)
                  :ordered (((?body . true))))
    (assert-query rulebase (clause (warm apple) (color apple red)) :succeeds)
    (assert-query rulebase
                  (assertz (:- (hot ?x) (color ?x red) (warm ?x))) :succeeds)
    (assert-query rulebase (clause (hot apple) ?body)
                  :ordered (((?body . (and (color apple red) (warm apple))))))
    (assert-query rulebase (retract (color apple ?shade))
                  :first ((?shade . green)))
    (assert-query rulebase (color apple ?shade)
                  :ordered (((?shade . red)) ((?shade . blue))))
    (assert-query rulebase (current_predicate (/ ordered 1)) :succeeds)
    (assert-query rulebase (current_predicate (/ color 2)) :succeeds)
    (assert-query rulebase (current_predicate (/ warm 1)) :succeeds)
    (assert-query rulebase (current_predicate (/ = 2)) :succeeds)
    (assert-query rulebase (predicate_property (color ?fruit ?shade) dynamic)
                  :succeeds)
    (assert-query rulebase (predicate_property (color ?fruit ?shade) user)
                  :succeeds)
    (assert-query rulebase (predicate_property (color ?fruit ?shade) defined)
                  :succeeds)
    (assert-query rulebase
                  (predicate_property (color ?fruit ?shade)
                                      (number_of_clauses 3))
                  :succeeds)
    (assert-query rulebase (predicate_property (ordered ?value) static)
                  :succeeds)
    (assert-query rulebase (predicate_property (ordered ?value) user)
                  :succeeds)
    (assert-query rulebase (predicate_property (= ?left ?right) built_in)
                  :succeeds)
    (assert-query rulebase (predicate_property (= ?left ?right) user)
                  :fails)
    (assert-query rulebase (predicate_property (missing ?value) ?property)
                  :fails)
    (is-equal
     '(((?fruit . ?fruit) (?shade . ?shade)
        (?property . cl-prolog-kit::dynamic))
       ((?fruit . ?fruit) (?shade . ?shade)
        (?property . cl-prolog-kit::user))
       ((?fruit . ?fruit) (?shade . ?shade)
        (?property . cl-prolog-kit::defined))
       ((?fruit . ?fruit) (?shade . ?shade)
        (?property cl-prolog-kit::number_of_clauses 3)))
     (query-prolog
      rulebase '(predicate_property (color ?fruit ?shade) ?property)))
    (assert-query rulebase (retractall (color apple ?shade)) :succeeds)
    (assert-query rulebase (color apple ?shade) :fails)
    (assert-query rulebase (retractall (color apple ?shade)) :succeeds)
    (assert-query rulebase (assertz (color pear green)) :succeeds)
    (assert-query rulebase (assertz (color apple red)) :succeeds)
    (assert-query rulebase (retractall (color ?fruit ?shade)) :succeeds)
    (assert-query rulebase (color ?fruit ?shade) :fails)
    (assert-query rulebase (assertz (color apple red)) :succeeds)
    (assert-query rulebase (assertz (color pear green)) :succeeds)
    (assert-query rulebase (abolish (/ color 2)) :succeeds)
    (assert-query rulebase (color apple ?shade) :signals)
    (assert-query rulebase (assertz (= left left)) :signals)
    (assert-query rulebase (clause (= left left) ?body) :signals)
    (assert-query rulebase (abolish (/ = 2)) :signals)))

(deftest retract-accepts-a-rule-pattern ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (:- (loud ?x) (color ?x red))) :succeeds)
    (assert-query rulebase (assertz (color parrot red)) :succeeds)
    (assert-query rulebase (loud parrot) :succeeds)
    (assert-query rulebase (retract (:- (loud ?x) (color ?x red))) :succeeds)
    (assert-query rulebase (loud parrot) :fails)))

(deftest lisp-shape-rule-head-matches-the-prolog-shape-rule-head ()
  "A bare atom head must work in both spellings of a rule.

The `:-'/2 term Prolog source reads normalizes its head through
%ENSURE-GOAL-FORM, so `assertz((warm :- color(a, red)))' asserts warm/0.  The
Lisp clause shape (:- HEAD . BODY-GOALS) used to test the raw head instead and
rejected the same rule."
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (color a red)) :succeeds)
    (assert-query rulebase (assertz (:- warm (color a red))) :succeeds)
    (assert-query rulebase (warm) :succeeds)
    (assert-query rulebase (clause warm ?body)
                  :ordered (((?body color a red))))))

(deftest malformed-lisp-shape-rules-raise-iso-errors ()
  "A malformed (:- HEAD . BODY) clause must raise the ISO error, not a Lisp one.

The condition class is asserted deliberately: this branch used to name an
unbound variable when building its type_error, so it terminated the query with
a Lisp UNBOUND-VARIABLE.  A bare :SIGNALS expectation accepts that just as
happily as the ISO error and would not have caught the defect."
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (:- 42 (color a red)))
                  :signals cl-prolog-kit:prolog-type-error)
    (assert-query rulebase (assertz (:- "text" (color a red)))
                  :signals cl-prolog-kit:prolog-type-error)
    ;; No head at all to blame, so the whole term is the culprit.
    (assert-query rulebase (assertz (:-))
                  :signals cl-prolog-kit:prolog-type-error)
    ;; ISO 13211-1 8.9.1.3: an uninstantiated head is an instantiation_error,
    ;; matching how assertz(X) is already handled.
    (assert-query rulebase (assertz (:- ?head (color a red)))
                  :signals cl-prolog-kit:prolog-instantiation-error)
    (assert-query rulebase (assertz (:- (ok a) 42))
                  :signals cl-prolog-kit:prolog-type-error)))

(deftest-queries predicate-property-validates-arguments ((make-rulebase))
  ("an unbound head is rejected"
   (predicate_property ?head ?property) :signals)
  ("a non-callable head is rejected"
   (predicate_property 42 ?property) :signals)
  ("a non-callable property is rejected"
   (predicate_property (missing ?value) 42) :signals))

(deftest dynamic-database-accepts-zero-arity-atoms ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz flag) :succeeds)
    (assert-query rulebase (clause flag true) :succeeds)
    (assert-query rulebase (retract flag) :succeeds)
    (assert-query rulebase flag :fails)))

(deftest current-predicate-uses-logical-update-snapshot ()
  (let ((rulebase (make-rulebase))
        (seen '()))
    (rulebase-insert-clause! rulebase (make-clause '(alpha first)))
    (rulebase-insert-clause! rulebase (make-clause '(alpha second)))
    (rulebase-insert-clause! rulebase (make-clause '(beta)))
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?indicator solution) seen)
       (rulebase-insert-clause! rulebase (make-clause '(gamma added))))
     rulebase '(current_predicate ?indicator))
    (is-equal '((/ alpha 1) (/ beta 0))
              (remove-if-not
               (lambda (indicator)
                 (member (second indicator) '(alpha beta gamma)))
               (nreverse seen)))
    (assert-query rulebase (current_predicate (/ gamma 1)) :succeeds)))

(deftest predicate-indicator-snapshots-are-detached ()
  (let ((builtin-snapshot (cl-prolog-kit::%builtin-predicate-indicators))
        (foreign-snapshot (cl-prolog-kit::%foreign-predicate-indicators)))
    (is builtin-snapshot)
    (is foreign-snapshot)
    (let ((builtin-first (first builtin-snapshot))
          (foreign-first (first foreign-snapshot)))
      (setf (first builtin-snapshot) 'mutated-builtin
            (first foreign-snapshot) 'mutated-foreign)
      (is-equal builtin-first
                (first (cl-prolog-kit::%builtin-predicate-indicators)))
      (is-equal foreign-first
                (first (cl-prolog-kit::%foreign-predicate-indicators))))))

(deftest current-predicate-validates-ground-indicators ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (current_predicate missing) :signals)
    (assert-query rulebase (current_predicate (/ missing atom)) :signals)
    (assert-query rulebase (current_predicate (/ ?name atom)) :signals)
    (assert-query rulebase (current_predicate (/ ?name 1 ?extra)) :signals)
    (assert-query rulebase (current_predicate (/ missing -1)) :signals)
    (assert-query rulebase (current_predicate (/ missing 1)) :fails)
    (assert-query rulebase (current_predicate (/ foreign-choice 1)) :succeeds)
    (assert-query rulebase (current_predicate (/ ?name 2)) :succeeds)
    (assert-query rulebase (current_predicate (/ = ?arity)) :succeeds)))

(deftest dynamic-database-enforces-static-procedure-permissions ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (dolist (goal '((asserta (fixed replacement))
                    (assertz (= left left))
                    (retract (fixed ?value))
                    (abolish (/ fixed 1))))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected a permission error for ~S" goal))
        (prolog-permission-error (condition)
          (let ((formal (second (prolog-exception-term condition))))
            (is-equal "PERMISSION_ERROR" (symbol-name (first formal)))
            (is-equal "MODIFY" (symbol-name (second formal)))
            (is-equal "STATIC_PROCEDURE" (symbol-name (third formal)))))))))

(deftest rejected-static-assertion-preserves-predicate-property ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (signals-error (query-prolog rulebase '(assertz (fixed replacement))))
    (assert-query rulebase (fixed original) :succeeds)
    (assert-query rulebase (fixed replacement) :fails)
    (assert-query rulebase (predicate_property (fixed ?value) static)
                  :succeeds)))

(deftest dynamic-database-validates-callable-arguments ()
  (let ((rulebase (make-rulebase)))
    (dolist (goal '((retract ?clause)
                    (retractall ?clause)
                    (clause ?head ?body)))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected an instantiation error for ~S" goal))
        (prolog-instantiation-error (condition)
          (is-equal "INSTANTIATION_ERROR"
                    (symbol-name (second (prolog-exception-term condition)))))))
    (dolist (goal '((retract 42)
                    (retractall 42)
                    (clause 42 ?body)))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected a callable type error for ~S" goal))
        (prolog-type-error (condition)
          (let ((formal (second (prolog-exception-term condition))))
            (is-equal "TYPE_ERROR" (symbol-name (first formal)))
            (is-equal "CALLABLE" (symbol-name (second formal)))))))))

(deftest clause-rejects-static-procedure-access ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (handler-case
        (progn
          (query-prolog rulebase '(clause (fixed ?value) ?body))
          (error "Expected a static access permission error"))
      (prolog-permission-error (condition)
        (let ((formal (second (prolog-exception-term condition))))
          (is-equal '("PERMISSION_ERROR" "ACCESS" "PRIVATE_PROCEDURE")
                    (mapcar #'symbol-name (subseq formal 0 3)))
          (is-equal '(/ fixed 1) (fourth formal)))))))

(deftest-queries abolish-validates-predicate-indicators ((make-rulebase))
  ("an unbound indicator is rejected"
   (abolish ?indicator) :signals)
  ("a bare atom is not a predicate indicator"
   (abolish fixed) :signals)
  ("a non-integer arity is rejected"
   (abolish (/ fixed one)) :signals)
  ("a negative arity is rejected"
   (abolish (/ fixed -1)) :signals))

(deftest proper-list-p-rejects-circular-lists ()
  (let ((circular (list 'value)))
    (setf (cdr circular) circular)
    (is (not (cl-prolog-kit::%proper-list-p circular)))))

(deftest abolish-removes-the-dynamic-declaration ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (temporary first)) :succeeds)
    (assert-query rulebase (abolish (/ temporary 1)) :succeeds)
    (is (null (cl-prolog-kit::%rulebase-predicate-property
               rulebase 'temporary 1)))
    (assert-query rulebase (assertz (temporary second)) :succeeds)
    (assert-query rulebase (temporary ?value) :ordered (((?value . second))))))

(deftest abolish-removes-table-declarations ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (tabled-dynamic value)) :succeeds)
    (cl-prolog-kit::%add-rulebase-table-declaration!
     rulebase 'tabled-dynamic 1 :runtime)
    (is (cl-prolog-kit::%rulebase-tabled-p rulebase 'tabled-dynamic 1))
    (assert-query rulebase (abolish (/ tabled-dynamic 1)) :succeeds)
    (is (not (cl-prolog-kit::%rulebase-tabled-p
              rulebase 'tabled-dynamic 1)))))

(deftest current-predicate-includes-empty-dynamic-procedures ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (empty-after-retract value)) :succeeds)
    (assert-query rulebase (retractall (empty-after-retract ?value)) :succeeds)
    (assert-query rulebase
                  (current_predicate (/ empty-after-retract 1)) :succeeds)))

(deftest predicate-call-keeps-logical-update-snapshot ()
  (let ((rulebase (make-rulebase))
        (seen '()))
    (assert-query rulebase (assertz (item first)) :succeeds)
    (assert-query rulebase (assertz (item second)) :succeeds)
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?value solution) seen)
       (query-prolog rulebase '(retractall (item ?discarded))))
     rulebase '(item ?value))
    (is-equal '(first second) (nreverse seen))
    (assert-query rulebase (item ?value) :fails)))

(deftest retract-backtracks-over-its-call-snapshot ()
  (let ((rulebase (make-rulebase)))
    (dolist (value '(first second third))
      (is (not (null
                (query-prolog rulebase
                              (list 'assertz (list 'item value))
                              :limit 1)))))
    (assert-query rulebase (retract (item ?value))
                  :ordered (((?value . first))
                      ((?value . second))
                      ((?value . third))))
    (assert-query rulebase (item ?value) :fails)))

;;;; ISO 13211-1 7.6.1 clause conversion: assertz/1, asserta/1, retract/1 and
;;;; retractall/1 accept the `:-'/2 term Prolog source text reads a rule as,
;;;; not only the Lisp-level (:- HEAD . BODY-GOALS) shape.

(deftest-prolog-goals dynamic-database-converts-prolog-rule-terms
  "assertz((silent :- true)), silent"
  "assertz((guarded(X) :- X > 1)), guarded(2)"
  "assertz((guarded(X) :- X > 1)), \\+ guarded(0)"
  "assertz((chain(X) :- link(X), tail(X))), assertz(link(1)), assertz(tail(1)), chain(1)"
  "asserta((early :- true)), early"
  ;; A rule asserted after a fact of the same predicate keeps both.
  "assertz(both(1)), assertz((both(X) :- X = 2)), both(1), both(2)"
  ;; clause/2 hands the body back in the shape source text spells it.
  "assertz(bare), clause(bare, Body), Body == true"
  "assertz((joined :- one, two)), clause(joined, Body), Body == (one , two)"
  ;; retract/1 matches through the same conversion, and a fact's body is `true'.
  "assertz((doomed :- true)), retract((doomed :- true)), \\+ doomed"
  "assertz(plain_fact), retract((plain_fact :- true)), \\+ plain_fact"
  "assertz((first_of(1) :- true)), assertz((first_of(2) :- true)), retract((first_of(X) :- true)), X == 1"
  "assertz((looping :- fail)), retractall((looping :- _)), \\+ looping")

(deftest asserting-a-rule-stores-it-as-a-rule ()
  "The regression this guards: the `:-'/2 term used to be stored whole as a
fact head, so the predicate it should have defined stayed undefined."
  (let ((rulebase (make-rulebase)))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "assertz((stored(X) :- X > 1))")))
    (let* ((clauses (rulebase-visible-clauses rulebase))
           (head (clause-head (first clauses))))
      (is-equal 1 (length clauses))
      (is-equal 'cl-prolog-kit::stored (first head))
      (is-equal 1 (length (rest head)))
      (is (logic-var-p (second head)))
      (is (clause-body (first clauses))
          "the asserted clause must have a body, not be a fact"))))
