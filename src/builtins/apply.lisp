;;;; library(apply) meta-predicates.
;;;;
;;;; maplist/2..5 and foldl/4..6 drive one or more lists in lockstep,
;;;; calling a goal per element with the CPS prover (cut-opaque, exactly like
;;;; call/1).  include/3, exclude/3, and partition/4 filter a list by testing
;;;; each element's goal for provability without leaking its bindings.

(in-package #:cl-prolog-kit)

(define-term-guard %resolve-meta-goal (goal)
  :documentation "Resolve the goal-closure argument of a meta-predicate."
  :resolve t
  :instantiation "meta-predicate goal must be instantiated")

(defun %fresh-list-variables (prefix count)
  "Return COUNT fresh logic variables named with PREFIX."
  (loop repeat count collect (fresh-logic-variable prefix)))

;;; maplist/2..5

(defun %maplist/k (closure list-terms rulebase environment depth emit operation)
  "Prove maplist(CLOSURE, LIST-TERMS...) over the CPS prover.  Mirrors the
two Prolog clauses: all lists empty, or all lists [H|T] with CLOSURE applied
to the heads before recursing on the tails."
  (labels ((descend (lists current-environment)
             ;; maplist(_, [], ..., []).
             (%term-unify-sequence
              (mapcar (lambda (list) (cons list nil)) lists)
              current-environment emit)
             ;; maplist(G, [H1|T1], ...) :- call(G, H1, ...), maplist(G, T1, ...).
             (let* ((heads (%fresh-list-variables "?MAPLIST-HEAD" (length lists)))
                    (tails (%fresh-list-variables "?MAPLIST-TAIL" (length lists)))
                    (pairs (mapcar (lambda (list head tail)
                                     (cons list (cons head tail)))
                                   lists heads tails)))
               (%term-unify-sequence
                pairs current-environment
                (lambda (cons-environment)
                  (%prove-bindings/k
                   (%extend-callable-goal closure heads cons-environment operation)
                   rulebase cons-environment depth
                   (lambda (goal-environment)
                     (descend tails goal-environment))))))))
    (descend list-terms environment)))

(define-builtin (maplist goal list1 &rest more-lists)
    (rulebase environment depth emit)
  (%maplist/k (%resolve-meta-goal goal environment (%iso-atom "MAPLIST"))
              (cons list1 more-lists)
              rulebase environment depth emit (%iso-atom "MAPLIST")))

;;; foldl/4..6

(defun %foldl/k (closure list-terms accumulator final
                 rulebase environment depth emit operation)
  "Prove foldl(CLOSURE, LIST-TERMS..., ACCUMULATOR, FINAL): thread ACCUMULATOR
through CLOSURE applied to each tuple of heads, unifying FINAL at the end."
  (labels ((descend (lists accumulator-term current-environment)
             ;; foldl(_, [], ..., V, V).
             (%term-unify-sequence
              (mapcar (lambda (list) (cons list nil)) lists)
              current-environment
              (lambda (base-environment)
                (%unify-emit final accumulator-term base-environment emit)))
             ;; foldl(G, [H|T], ..., V0, V) :-
             ;;   call(G, H, ..., V0, V1), foldl(G, T, ..., V1, V).
             (let* ((heads (%fresh-list-variables "?FOLDL-HEAD" (length lists)))
                    (tails (%fresh-list-variables "?FOLDL-TAIL" (length lists)))
                    (next (fresh-logic-variable "?FOLDL-ACC"))
                    (pairs (mapcar (lambda (list head tail)
                                     (cons list (cons head tail)))
                                   lists heads tails)))
               (%term-unify-sequence
                pairs current-environment
                (lambda (cons-environment)
                  (%prove-bindings/k
                   (%extend-callable-goal
                    closure (append heads (list accumulator-term next))
                    cons-environment operation)
                   rulebase cons-environment depth
                   (lambda (goal-environment)
                     (descend tails next goal-environment))))))))
    (descend list-terms accumulator environment)))

(defmacro %define-foldl-builtin (list-count)
  "Define foldl over LIST-COUNT lists (LIST-COUNT + 2 arguments)."
  (let ((lists (loop repeat list-count collect (gensym "LIST"))))
    `(define-builtin (foldl goal ,@lists v0 v) (rulebase environment depth emit)
       (%foldl/k (%resolve-meta-goal goal environment (%iso-atom "FOLDL"))
                 (list ,@lists) v0 v
                 rulebase environment depth emit (%iso-atom "FOLDL")))))

(%define-foldl-builtin 1)
(%define-foldl-builtin 2)
(%define-foldl-builtin 3)

;;; include/3, exclude/3, partition/4 -- provability filters.

(defun %goal-succeeds-p (closure element rulebase environment depth operation)
  "True when call(CLOSURE, ELEMENT) has a proof; bindings are discarded."
  (nth-value 1
             (%first-proof-environment
              (%extend-callable-goal closure (list element) environment operation)
              rulebase environment depth)))

(define-builtin (include goal list-term included) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "INCLUDE"))
         (closure (%resolve-meta-goal goal environment operation))
         (elements (%require-proper-list (logic-substitute list-term environment)
                                         environment operation "second argument")))
    (%unify-emit included
                 (remove-if-not
                  (lambda (element)
                    (%goal-succeeds-p closure element rulebase environment depth operation))
                  elements)
                 environment emit)))

(define-builtin (exclude goal list-term excluded) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "EXCLUDE"))
         (closure (%resolve-meta-goal goal environment operation))
         (elements (%require-proper-list (logic-substitute list-term environment)
                                         environment operation "second argument")))
    (%unify-emit excluded
                 (remove-if
                  (lambda (element)
                    (%goal-succeeds-p closure element rulebase environment depth operation))
                  elements)
                 environment emit)))

(define-builtin (partition goal list-term included excluded)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "PARTITION"))
         (closure (%resolve-meta-goal goal environment operation))
         (elements (%require-proper-list (logic-substitute list-term environment)
                                         environment operation "second argument"))
         (in '())
         (out '()))
    (dolist (element elements)
      (if (%goal-succeeds-p closure element rulebase environment depth operation)
          (push element in)
          (push element out)))
    (%term-unify-sequence
     (list (cons included (nreverse in)) (cons excluded (nreverse out)))
     environment emit)))
