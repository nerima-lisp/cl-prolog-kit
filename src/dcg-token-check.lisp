;;;; PHRASE-TOKEN-VALID-P: whether a typed token can continue a partial input.
;;;;
;;;; The grammar runs through the ordinary prover on PREFIX ++ [TOKEN|R],
;;;; with R unbound.  The check accepts when a goal is dispatched while R is
;;;; bound, when a goal that might test R would run, or when the derivation
;;;; succeeds.  It rejects only when the search is exhausted first, so a
;;;; rejection means no input starting with PREFIX ++ [TOKEN] is a phrase.
;;;;
;;;; Every goal passes %PHRASE-PROBE-DISPATCH/K, which checks R first.  User
;;;; predicates, builtins that only bind their arguments, and control
;;;; constructs that carry their inner goal's bindings into the continuation
;;;; run as usual: whatever binds R inside them is seen at the next dispatch
;;;; or at the solution.  Any other builtin or foreign predicate whose
;;;; arguments reach R is not run, and the check accepts.  That includes the
;;;; constructs that may prove a goal and then drop its bindings (\+,
;;;; forall/2, findall/3, catch/3 on an exception): they decide their result
;;;; after R is bound without dispatching anything more.  Builtins are recognized by their solver, so
;;;; a foreign predicate that shares a builtin's name is not taken for it.

(in-package #:cl-prolog-kit)

(defun %phrase-probe-solvers (templates)
  "The builtin solvers of the goals in TEMPLATES, Prolog source text."
  (remove-duplicates
   (loop for text in templates
         for goal = (%ensure-goal-form (read-prolog-term text))
         for solver = (%goal-solver (first goal) (length (rest goal)))
         when solver collect solver)))

(defparameter *phrase-probe-monotonic-solvers*
  (append
   (%phrase-probe-solvers
    '("a = b" "call(a)" "(a, b)" "(a ; b)" "true" "fail" "false"
      "member(a, b)" "append(a, b, c)" "between(a, b, c)" "length(a, b)"
      "select(a, b, c)" "phrase(a, b)" "phrase(a, b, c)"))
   ;; def-dcg-rule's matchers bind the open tail to the expected token
   ;; (dcg-runtime.lisp), and its alternatives are a disjunction.
   (remove nil (list (%goal-solver 'dcg-token-match 3)
                     (%goal-solver 'dcg-token-match-value 4)
                     (%goal-solver 'dcg-alt 4)
                     (%goal-solver 'dcg-opt 3))))
  "Solvers of builtins whose solutions only bind variables, never test
whether one is bound, so running them on the open tail loses no solution
of a concrete input.")

(defparameter *phrase-probe-control-solvers*
  (%phrase-probe-solvers
   '("once(a)" "ignore(a)" "(a -> b)" "(a -> b ; c)" "(a *-> b)" "(a *-> b ; c)"))
  "Solvers of control constructs that pass their inner goal's bindings on to
the continuation on every path, so a binding of R inside them reaches the
next dispatch.  catch/3 is not one: its recovery restores the environment
from before its goal, so a binding of R followed by an exception is lost.")

(defstruct (%phrase-probe (:constructor %make-phrase-probe (tail accept-tag)))
  "The open tail one token check watches and where it throws on acceptance."
  (tail nil :read-only t)
  (accept-tag nil :read-only t))

(defun %phrase-probe-accept ()
  (cl:throw (%phrase-probe-accept-tag *phrase-probe*) t))

(defun %phrase-probe-tail-bound-p (environment)
  "True when the tail is bound to a non-variable in ENVIRONMENT, an alist of
bindings.  Chasing the alist directly avoids building an index on every
dispatch."
  (let ((term (%phrase-probe-tail *phrase-probe*)))
    (loop repeat 64
          do (let ((binding (assoc term environment :test #'eq)))
               (cond
                 ((null binding) (return-from %phrase-probe-tail-bound-p nil))
                 ((logic-var-p (cdr binding)) (setf term (cdr binding)))
                 (t (return-from %phrase-probe-tail-bound-p t)))))
    ;; A longer alias chain, or one that loops: resolve it properly.
    (not (logic-var-p (%walk-term term environment)))))

(defun %phrase-probe-open-input-p (variable environment)
  "True when VARIABLE, an unbound input stream, is the checked token's tail."
  (eq variable (%walk-term (%phrase-probe-tail *phrase-probe*) environment)))

(defun %phrase-probe-check-tail (state)
  "Accept when the tail is bound in STATE.  Called before a goal is resolved,
so a goal that cannot be resolved does not raise once the token is past."
  (when (%phrase-probe-tail-bound-p (proof-state-bindings state))
    (%phrase-probe-accept)))

(defun %phrase-probe-dispatch/k (goal explicit-module solver state succeed)
  "Prove GOAL, resolved by %RESOLVE-DISPATCH-TARGET, under a token check."
  (cond
    ((or (null solver)
         (member solver *phrase-probe-monotonic-solvers* :test #'eq)
         (member solver *phrase-probe-control-solvers* :test #'eq))
     (%prove-resolved-goal/k goal explicit-module solver state succeed))
    ((let ((index (proof-state-environment-index state)))
       (%occurs-p-indexed
        (%walk-term-indexed (%phrase-probe-tail *phrase-probe*) index)
        goal index))
     (%phrase-probe-accept))
    (t (%prove-resolved-goal/k goal explicit-module solver state succeed))))

(defun %phrase-probe-verify (rulebase nonterminal prefix token max-depth)
  "True when the derivation of PREFIX ++ [TOKEN|R] binds or passes on R, or
succeeds, before its search is exhausted."
  (let* ((tail (fresh-logic-variable "?PROBE-REST"))
         (tag (list '%phrase-probe-accept))
         (goal (%dcg-call-with-streams nonterminal (append prefix (list* token tail)) '())))
    (eq t (cl:catch tag
            (%with-prolog-top-level-call (rulebase)
              (let ((*unification-scratch* (%make-unification-scratch))
                    (*phrase-probe* (%make-phrase-probe tail tag))
                    (cut-tag (%make-cut-tag)))
                (%with-logic-variable-order
                  (%collect-variables goal)
                  (cl:catch cut-tag
                    (%prove-goals/k
                     (%normalize-query (list goal))
                     (%make-proof-state
                      rulebase '() (%make-environment-index '()) max-depth
                      +default-prolog-module+
                      ;; A check leaves the search at its first acceptance,
                      ;; so its tables stay out of the session later queries
                      ;; share.
                      (%make-table-session (%make-term-hash-table)
                                           (make-hash-table :test #'equal)
                                           (make-hash-table :test #'equal)
                                           (%make-term-hash-table))
                      cut-tag)
                     (lambda (state)
                       (declare (cl:ignore state))
                       (cl:throw tag t)))))))
            nil))))

(defmacro %with-token-check-options ((max-depth max-inferences max-inferences-p
                                      deadline deadline-p prefix)
                                     &body body)
  "Validate PREFIX and the limit options, then run BODY under the limits."
  `(progn
     (unless (%proper-list-p ,prefix)
       (error 'type-error :datum ,prefix :expected-type 'list))
     (let ((,max-depth (%validate-max-depth ,max-depth)))
       (%with-inference-limits
           ((if ,max-inferences-p
                (%validate-max-inferences ,max-inferences)
                :default)
            (if ,deadline-p (%validate-deadline ,deadline) :default))
         ,@body))))

(defun phrase-token-valid-p (rulebase nonterminal prefix token
                             &key (max-depth *max-prolog-depth*)
                               (max-inferences *max-prolog-inferences*
                                max-inferences-p)
                               (deadline *prolog-deadline* deadline-p))
  "Return NIL only when no phrase of NONTERMINAL starts with PREFIX followed
by TOKEN.

TOKEN is a concrete token in the representation PREFIX uses.  True does not
promise that a phrase exists: the check accepts once the derivation reads
past TOKEN, so guards on later tokens are not run.  MAX-DEPTH,
MAX-INFERENCES and DEADLINE behave as the query options of the same names;
a limit signals its condition."
  (%with-token-check-options (max-depth max-inferences max-inferences-p
                              deadline deadline-p prefix)
    (%phrase-probe-verify rulebase nonterminal prefix token max-depth)))

(defun phrase-filter-candidates (rulebase nonterminal prefix candidates
                                 &key (max-depth *max-prolog-depth*)
                                   (max-inferences *max-prolog-inferences*
                                    max-inferences-p)
                                   (deadline *prolog-deadline* deadline-p))
  "Return the elements of CANDIDATES, in order, that PHRASE-TOKEN-VALID-P
does not reject after PREFIX.

MAX-INFERENCES and DEADLINE bound the checks of all candidates together;
MAX-DEPTH bounds each one."
  (unless (%proper-list-p candidates)
    (error 'type-error :datum candidates :expected-type 'list))
  (%with-token-check-options (max-depth max-inferences max-inferences-p
                              deadline deadline-p prefix)
    (remove-if-not (lambda (token)
                     (%phrase-probe-verify rulebase nonterminal prefix token max-depth))
                   candidates)))
