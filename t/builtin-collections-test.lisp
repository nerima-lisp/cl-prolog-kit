;;;; Collection builtin tests: findall/bagof/setof, sort/msort/keysort, and
;;;; the standard-order-of-terms variable/variant grouping they share.

(in-package #:cl-prolog-kit.tests)

(deftest-queries solution-collection-builtins
    ((make-rulebase
      :clauses (list (make-clause '(edge a 2))
                     (make-clause '(edge a 1))
                     (make-clause '(edge a 2))
                     (make-clause '(edge b 3))
                     (make-clause '(edge3 a left 1))
                     (make-clause '(edge3 a right 2))
                     (make-clause '(edge3 b left 3)))))
  ((findall ?value (edge a ?value) ?bag)
                                   :ordered (((?value . ?value) (?bag 2 1 2))))
  ((findall ?value (edge missing ?value) ?bag)
                                   :ordered (((?value . ?value) (?bag))))
  ((findall ?value (edge missing ?value) ?bag ?tail)
                                   :ordered (((?value . ?value) (?bag . ?tail)
                                        (?tail . ?tail))))
  ((bagof ?value (edge ?key ?value) ?bag)
                                   :ordered (((?value . ?value) (?key . a) (?bag 2 1 2))
                                       ((?value . ?value) (?key . b) (?bag 3))))
  ((bagof ?value (^ ?key (edge ?key ?value)) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?bag 2 1 2 3))))
  ((bagof ?value (^ (pair ?key ?side) (edge3 ?key ?side ?value)) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?side . ?side) (?bag 1 2 3))))
  ((bagof ?value (^ ?key (^ ?side (edge3 ?key ?side ?value))) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?side . ?side) (?bag 1 2 3))))
  ((bagof ?value (edge missing ?value) ?bag) :fails)
  ((setof ?value (edge ?key ?value) ?bag)
                                   :ordered (((?value . ?value) (?key . a) (?bag 1 2))
                                       ((?value . ?value) (?key . b) (?bag 3))))
  ((setof ?value (edge missing ?value) ?bag) :fails))

(deftest findall-difference-list-preserves-ground-tail ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (make-clause '(edge a 2))
                          (make-clause '(edge a 1))
                          (make-clause '(edge a 2))))))
    (is-equal '(2 1 2 . tail)
              (solution-binding
               '?bag
               (query-prolog-first
                rulebase '(findall ?value (edge a ?value) ?bag tail)))
    (is-equal 'tail
              (solution-binding
               '?bag
               (query-prolog-first
                rulebase '(findall ?value (edge missing ?value) ?bag tail)))))))

(deftest-queries sorting-builtins ((make-rulebase))
  ((sort (3 1 2 1) ?sorted) :ordered (((?sorted 1 2 3))))
  ((msort (3 1 2 1) ?sorted) :ordered (((?sorted 1 1 2 3))))
  ((sort () ?sorted) :ordered (((?sorted))))
  ((sort (z 2 a 1) ?sorted) :ordered (((?sorted 1 2 a z))))
  ((sort ((a first second) (z item)) ?sorted)
   :ordered (((?sorted (z item) (a first second)))))
  ((sort (z cl-prolog-kit.user-atoms::car) ?sorted)
   :ordered (((?sorted cl-prolog-kit.user-atoms::car z))))
  ((keysort ((- 2 second) (- 1 first) (- 2 third)) ?sorted)
   :ordered (((?sorted (- 1 first) (- 2 second) (- 2 third))))))

(deftest-queries sorting-builtins-report-iso-errors ((make-rulebase))
  ((sort ?input ?sorted) :signals)
  ((sort improper ?sorted) :signals)
  ((msort ?input ?sorted) :signals)
  ((msort improper ?sorted) :signals)
  ((keysort ((not-a-pair)) ?sorted) :signals))

(deftest-queries sort4-builtin ((make-rulebase))
  ((sort 0 |@<| (3 1 2 1) ?s)  :ordered (((?s 1 2 3))))
  ((sort 0 |@=<| (3 1 2 1) ?s) :ordered (((?s 1 1 2 3))))
  ((sort 0 |@>| (3 1 2 1) ?s)  :ordered (((?s 3 2 1))))
  ((sort 0 |@>=| (3 1 2 1) ?s) :ordered (((?s 3 2 1 1))))
  ;; key 1 selects the first argument of each compound.
  ((sort 1 |@>=| ((k 3) (k 1) (k 2)) ?s)
   :ordered (((?s (k 3) (k 2) (k 1)))))
  ;; key 2 selects the second argument.
  ((sort 2 |@<| ((k a 3) (k b 1) (k c 2)) ?s)
   :ordered (((?s (k b 1) (k c 2) (k a 3)))))
  ;; key past the term's arity is an error.
  ((sort 2 |@<| ((k 1) (k 2)) ?s) :signals)
  ((sort 0 |@<| () ?s)         :ordered (((?s))))
  ((sort -1 |@<| (1 2) ?s)     :signals)
  ((sort a |@<| (1 2) ?s)      :signals)
  ((sort ?k |@<| (1 2) ?s)     :signals)
  ((sort 0 bogus (1 2) ?s)     :signals)
  ((sort 0 ?order (1 2) ?s)    :signals)
  ((sort 0 |@<| improper ?s)   :signals)
  ((sort 1 |@<| ((a . b)) ?s)  :signals))

(defun make-predsort-rulebase ()
  (prolog ((cmp ?order ?a ?b) (compare ?order ?a ?b))
          ((by-abs ?order ?a ?b) (is ?aa (abs ?a)) (is ?bb (abs ?b))
           (compare ?order ?aa ?bb))
          ((never ?order ?a ?b) (fail))
          ((bad-order eq ?a ?b))))

(deftest-queries predsort-builtin ((make-predsort-rulebase))
  ((predsort cmp (3 1 2 1) ?s)  :ordered (((?s 1 2 3))))
  ((predsort cmp () ?s)         :ordered (((?s))))
  ;; `=' results drop the later element: 1 and -1 compare equal by abs.
  ((predsort by-abs (1 -1 2) ?s) :ordered (((?s 1 2))))
  ;; several equal groups keep the first of each, stably.
  ((predsort by-abs (3 1 -1 3 2 -2) ?s) :ordered (((?s 1 2 3))))
  ;; a comparison predicate with no proof makes predsort fail (as in SWI).
  ((predsort never (1 2 3) ?s)  :fails)
  ;; a comparison binding a non-order atom raises.
  ((predsort bad-order (1 2) ?s) :signals)
  ((predsort ?g (1) ?s)         :signals)
  ((predsort cmp improper ?s)   :signals))

(deftest predsort-preserves-caller-list ()
  (let ((input '(3 1 2 1)))
    (query-prolog-first (make-predsort-rulebase)
                        (list 'predsort 'cmp input '?sorted))
    (is-equal '(3 1 2 1) input)))

;; Wrapper predicates keep the aggregate template variable inside the clause
;; body so only the result variable appears in each query.
(defun make-aggregate-rulebase ()
  (prolog ((p 3)) ((p 1)) ((p 2))
          ((f 1.5d0)) ((f 2.5d0))
          ((a-tom foo)) ((a-tom bar))
          ;; a defined predicate with no solutions, so aggregate_all sees an
          ;; empty set (an *undefined* predicate would raise existence_error).
          ((nonexistent ?x) (cl-prolog-kit::fail))
          ((agg-count ?n) (aggregate_all count (p ?x) ?n))
          ((agg-count1 ?n) (aggregate_all (count ?x) (p ?x) ?n))
          ((agg-sum ?n) (aggregate_all (sum ?x) (p ?x) ?n))
          ((agg-sum-expr ?n) (aggregate_all (sum (* ?x ?x)) (p ?x) ?n))
          ((agg-sum-float ?n) (aggregate_all (sum ?x) (f ?x) ?n))
          ((agg-max ?n) (aggregate_all (max ?x) (p ?x) ?n))
          ((agg-min ?n) (aggregate_all (min ?x) (p ?x) ?n))
          ((agg-bag ?b) (aggregate_all (bag ?x) (p ?x) ?b))
          ((agg-set ?b) (aggregate_all (set ?x) (p ?x) ?b))
          ((agg-count-empty ?n) (aggregate_all count (nonexistent ?x) ?n))
          ((agg-sum-empty ?n) (aggregate_all (sum ?x) (nonexistent ?x) ?n))
          ((agg-max-empty ?n) (aggregate_all (max ?x) (nonexistent ?x) ?n))
          ((agg-sum-atoms ?n) (aggregate_all (sum ?x) (a-tom ?x) ?n))))

(deftest-queries aggregate-all-builtin ((make-aggregate-rulebase))
  ((agg-count ?n)       :ordered (((?n . 3))))
  ((agg-count1 ?n)      :ordered (((?n . 3))))
  ((agg-sum ?n)         :ordered (((?n . 6))))
  ((agg-sum-expr ?n)    :ordered (((?n . 14))))
  ((agg-sum-float ?n)   :ordered (((?n . 4.0d0))))
  ((agg-max ?n)         :ordered (((?n . 3))))
  ((agg-min ?n)         :ordered (((?n . 1))))
  ((agg-bag ?b)         :ordered (((?b 3 1 2))))
  ((agg-set ?b)         :ordered (((?b 1 2 3))))
  ((agg-count-empty ?n) :ordered (((?n . 0))))
  ((agg-sum-empty ?n)   :ordered (((?n . 0))))
  ((agg-max-empty ?n)   :fails)
  ((agg-sum-atoms ?n)   :signals)
  ((aggregate_all ?spec (p ?x) ?n)       :signals)
  ((aggregate_all (a . b) (p ?x) ?n)     :signals)
  ((aggregate_all (bogus ?x) (p ?x) ?n)  :signals))

(deftest collection-builtins-use-standard-variable-order-and-variants ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (make-clause '(variant-key (pair ?left ?left) first))
                          (make-clause '(variant-key (pair ?right ?right) second))
                          (make-clause '(ordered-key z last))
                          (make-clause '(ordered-key a first))))))
    (let* ((solutions
             (query-prolog
              rulebase '(bagof ?value (variant-key ?key ?value) ?bag)))
           (solution (first solutions))
           (key (solution-binding '?key solution)))
      (is (= 1 (length solutions)))
      (is-equal '(first second) (solution-binding '?bag solution))
      (is (eq (second key) (third key))))
    (assert-query rulebase
                  (bagof ?value (ordered-key ?key ?value) ?bag)
                  :ordered
                  (((?value . ?value) (?key . a) (?bag first))
                   ((?value . ?value) (?key . z) (?bag last))))
    (with-single-query-solution
        (solution solutions rulebase
         (list 'sort
               (list (list 'pair '?left '?left)
                     (list 'pair '?right '?right))
               '?sorted))
      (let ((sorted (solution-binding '?sorted solution)))
        (is (= 2 (length sorted)))
        (is (not (eq (second (first sorted))
                     (second (second sorted))))))))
  (let* ((representative (list 'same))
         (equivalent (list 'same))
         (other (list 'z))
         (groups
           (cl-prolog-kit::%partition-solution-groups
            (list (cons other 'last)
                  (cons representative 'first)
                  (cons equivalent 'second))))
         (same-group (find representative groups :key #'car :test #'eq))
         (other-group (find other groups :key #'car :test #'eq)))
    (is (= 2 (length groups)))
    (is (eq representative (car same-group)))
    (is-equal '(first second) (cdr same-group))
    (is-equal '(last) (cdr other-group)))
  (let ((first-cycle (list 'cycle))
        (second-cycle (list 'cycle)))
    (setf (cdr first-cycle) first-cycle
          (cdr second-cycle) second-cycle)
    (let* ((groups
             (cl-prolog-kit::%partition-solution-groups
              (list (cons first-cycle 'first)
                    (cons second-cycle 'second))))
           (group (first groups)))
      (is (= 1 (length groups)))
      (is (eq first-cycle (car group)))
      (is-equal '(first second) (cdr group)))))
