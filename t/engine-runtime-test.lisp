;;;; Cut, tabling, and left-recursion detection tests.  Predicate-index and
;;;; depth-limit tests live in engine-runtime-index-and-depth-test.lisp; foreign
;;;; predicates and define-builtin registration in
;;;; engine-runtime-foreign-and-registration-test.lisp; the ISO error contract in
;;;; engine-runtime-error-contract-test.lisp.

(in-package #:cl-prolog-kit.tests)

(defvar *observed-table-session* nil)

(cl-prolog-kit::define-builtin (test-nested-table-session)
    (rulebase environment depth emit)
  (let ((outer cl-prolog-kit::*current-table-session*))
    (cl-prolog-kit::%prove-bindings/k
     '(true) rulebase environment depth
     (lambda (bindings)
       (setf *observed-table-session*
             (and outer
                  (eq outer cl-prolog-kit::*current-table-session*)))
       (funcall emit bindings)))))

(deftest-queries cut-prunes-clause-alternatives
    ((prolog
      ((choice left))
      ((choice right))
      ((pick ?x) (choice ?x) !)
      ((pick fallback) (choice left))
      ((commit-here) !)
      ((commit-here) fail)
      ((commit-value first) !)
      ((commit-value second))))
  ((pick ?x) :ordered (((?x . left))))
  ((commit-value ?x) :ordered (((?x . first))))
  (((choice ?x) (commit-here)) :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) !) :ordered (((?x . left))))
  ((and (choice ?x) !) :ordered (((?x . left))))
  (((choice ?x) (call !)) :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) (once (and (choice ?y) !)))
   :ordered (((?x . left) (?y . left)) ((?x . right) (?y . left))))
  (((choice ?x) (if-then-else (and true !) true fail))
   :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) (if-then-else true ! fail)) :ordered (((?x . left))))
  (((choice ?x) (if-then-else fail true !)) :ordered (((?x . left))))
  )

(deftest malformed-clauses-are-ignored ()
  (let ((rb (make-rulebase)))
    (rulebase-insert-clause! rb (make-clause '() '((anything))))
    (rulebase-insert-clause! rb (make-clause '(ready)))
    (is-equal '(nil) (query-prolog rb '(ready)))))

(deftest-queries facts-are-tried-before-rules
    ((prolog
      ((color red))
      ((color ?x) (= ?x derived))))
  ((color ?x) :ordered (((?x . red)) ((?x . derived)))))

;; The recursive argument keeps growing, so variant tabling cannot close a
;; fixed point and the explicit depth budget must fire.  (A plain P :- P
;; loop is answered finitely by tabling and would fail instead of signal.)
(deftest-queries depth-bound-signals-incomplete-search
    ((prolog
      ((loop-forever ?n) (loop-forever (s ?n)))))
  ((loop-forever zero) :signals :max-depth 16))

(deftest variant-tabling-terminates-left-recursion ()
  (let* ((edge-count 128)
         (rulebase
           (make-rulebase
            :clauses
            (append
             (list
              (make-clause
               (quote (path ?x ?y))
               (quote ((path ?x ?z) (edge ?z ?y))))
              (make-clause
               (quote (path ?x ?y))
               (quote ((edge ?x ?y)))))
             (loop for source below edge-count
                   collect (make-clause
                            (list (quote edge) source (1+ source)))))))
         (solutions (query-prolog rulebase (quote (path 0 ?who)))))
    (is-equal
     (loop for target from 1 to edge-count
           collect (list (cons (quote ?who) target)))
     solutions)))
(deftest tabled-provability-cache-is-revision-scoped ()
  (let ((rulebase
          (prolog
            ((reachable ?x ?y) (reachable ?x ?z) (edge ?z ?y))
            ((reachable ?x ?y) (edge ?x ?y))
            ((edge a b)))))
    (is (prolog-succeeds-p rulebase (quote (reachable a b))))
    (let ((session (cl-prolog-kit::%make-rulebase-table-session rulebase)))
      (is-equal 1
                (hash-table-count
                 (cl-prolog-kit::%table-session-successful-queries session))))
    (is (prolog-succeeds-p rulebase (quote (reachable a b))))
    (let ((entry (first (nth-value 1 (cl-prolog-kit::%rulebase-predicate-entries rulebase cl-prolog-kit::+default-prolog-module+ 'edge 2))))) (rulebase-insert-clause! rulebase (make-clause '(edge a c))) (cl-prolog-kit::%rulebase-retract-entry! rulebase entry))
    (is (not (prolog-succeeds-p rulebase (quote (reachable a b)))))
    (is-equal 0
              (hash-table-count
               (cl-prolog-kit::%table-session-successful-queries
                (cl-prolog-kit::%make-rulebase-table-session rulebase))))))

(deftest-queries variant-tabling-deduplicates-answers
    ((prolog
      ((reachable ?x ?y) (reachable ?x ?z) (arc ?z ?y))
      ((reachable ?x ?y) (arc ?x ?y))
      ((arc a b))
      ((arc a b))
      ((arc b c))
      ((arc a c))))
  ((reachable a ?who)
   :ordered (((?who . b)) ((?who . c)))))

(deftest-queries variant-tabling-terminates-mutual-left-recursion
    ((prolog
      ((even-node ?x) (odd-node ?x))
      ((even-node zero))
      ((odd-node ?x) (even-node ?x))
      ((odd-node one))))
  ((even-node ?x) :ordered (((?x . one)) ((?x . zero)))))
(deftest-queries variant-tabling-preserves-nonlinear-left-recursion
  ((prolog
    ((joined ?x ?y) (joined ?x ?z) (joined ?z ?y))
    ((joined a b))
    ((joined b c))))
  ((joined a ?who) :ordered (((?who . b)) ((?who . c)))))

(deftest-queries variant-tabling-terminates-three-node-left-recursion
    ((prolog
      ((cycle-a ?x) (cycle-b ?x))
      ((cycle-a a))
      ((cycle-b ?x) (cycle-c ?x))
      ((cycle-c ?x) (cycle-a ?x))
      ((cycle-c c))))
  ((cycle-a ?x) :ordered (((?x . c)) ((?x . a)))))

(deftest tabled-predicate-preserves-and-deduplicates-cyclic-answer (:timeout 2)
    (let* ((cycle-a (cons (quote loop) nil))
           (cycle-b (cons (quote loop) nil))
           (rulebase (make-rulebase)))
      (setf (cdr cycle-a) cycle-a
            (cdr cycle-b) cycle-b)
      (rulebase-insert-clause!
       rulebase (make-clause (list (quote cyclic-answer) cycle-a)))
      (rulebase-insert-clause!
       rulebase (make-clause (list (quote cyclic-answer) cycle-b)))
      (cl-prolog-kit::%add-rulebase-table-declaration!
       rulebase (quote cyclic-answer) 1 :test)
      (let* ((solutions (query-prolog rulebase (quote (cyclic-answer ?answer))))
             (answer (solution-binding (quote ?answer) (first solutions))))
        (is-equal 1 (length solutions))
        (is (consp answer))
        (is (eq answer (cdr answer)))
        (is-equal (quote loop) (car answer)))))

  (deftest cyclic-answer-index-is-created-lazily ()
    (let ((entry (cl-prolog-kit::%make-table-entry))
          (cycle-a (cons (quote loop) nil))
          (cycle-b (cons (quote loop) nil)))
      (setf (cdr cycle-a) cycle-a
            (cdr cycle-b) cycle-b)
      (is (null (cl-prolog-kit::%table-entry-cyclic-answer-index entry)))
      (is (cl-prolog-kit::%record-table-answer! entry (quote plain) nil nil))
      (is (null (cl-prolog-kit::%table-entry-cyclic-answer-index entry)))
      (is (cl-prolog-kit::%record-table-answer! entry cycle-a t nil))
      (is (hash-table-p (cl-prolog-kit::%table-entry-cyclic-answer-index entry)))
      (is (not (cl-prolog-kit::%record-table-answer! entry cycle-b t nil)))
      (is-equal 2 (cl-prolog-kit::%table-entry-answer-count entry))))

(deftest table-declaration-and-clause-retraction-guard-repeat-updates ()
  (let ((rulebase (make-rulebase)))
    (cl-prolog-kit::%add-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (cl-prolog-kit::%add-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (is (cl-prolog-kit::%rulebase-tabled-p rulebase 'repeat-owner 1))
    (cl-prolog-kit::%remove-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :absent-owner)
    (is (cl-prolog-kit::%rulebase-tabled-p rulebase 'repeat-owner 1))
    (cl-prolog-kit::%remove-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (is (not (cl-prolog-kit::%rulebase-tabled-p rulebase 'repeat-owner 1)))
    (rulebase-insert-clause! rulebase (make-clause '(repeat-fact)))
    (let ((entry (first (cl-prolog-kit::rulebase-entries rulebase))))
      (is (cl-prolog-kit::%rulebase-retract-entry! rulebase entry))
      (is (not (cl-prolog-kit::%rulebase-retract-entry! rulebase entry))))))

(deftest left-recursion-through-leading-builtins-and-control-terminates
    (:timeout 2)
  (let ((rulebase
          (prolog
           ((builtin-direct ?x) true (builtin-direct ?x))
           ((builtin-direct direct-base))
           ((builtin-indirect-p ?x) (= ?x ?y) (builtin-indirect-q ?y))
           ((builtin-indirect-p p-base))
           ((builtin-indirect-q ?x) true (builtin-indirect-p ?x))
           ((builtin-indirect-q q-base))
           ((control-direct ?x)
            (call (and true (control-direct ?x))))
           ((control-direct control-base)))))
    (is-equal '(((?x . direct-base)))
              (query-prolog rulebase '(builtin-direct ?x)))
    (is-same-set '(((?x . p-base)) ((?x . q-base)))
                 (query-prolog rulebase '(builtin-indirect-p ?x)))
    (is-equal '(((?x . control-base)))
              (query-prolog rulebase '(control-direct ?x)))))

(deftest builtin-proof-search-inherits-table-session ()
  (let ((*observed-table-session* nil))
    (is-equal '(nil)
              (query-prolog (make-rulebase) '(test-nested-table-session)))
    (is *observed-table-session*)))

(deftest shared-table-session-isolates-rulebase-caches ()
  (let* ((recursive-rulebase
           (prolog
            ((shared-value ?value) (shared-value ?value))
            ((shared-value first))))
         (fact-rulebase
           (prolog
            ((shared-value second))
            ((padding-fact present)))))
    (cl-prolog-kit::%add-rulebase-table-declaration!
     recursive-rulebase 'shared-value 1 :test)
    (cl-prolog-kit::%add-rulebase-table-declaration!
     fact-rulebase 'shared-value 1 :test)
    (is-equal (cl-prolog-kit::rulebase-revision recursive-rulebase)
              (cl-prolog-kit::rulebase-revision fact-rulebase))
    (let* ((session
             (cl-prolog-kit::%make-rulebase-table-session recursive-rulebase))
           (recursive-state
             (cl-prolog-kit::%make-proof-state
              recursive-rulebase
              '()
              (cl-prolog-kit::%make-environment-index '())
              nil
              cl-prolog-kit::+default-prolog-module+
              session
              (cl-prolog-kit::%make-cut-tag)))
           (fact-state
             (cl-prolog-kit::%make-proof-state
              fact-rulebase
              '()
              (cl-prolog-kit::%make-environment-index '())
              nil
              cl-prolog-kit::+default-prolog-module+
              session
              (cl-prolog-kit::%make-cut-tag)))
           (recursive-answers '())
           (fact-answers '()))
      (is (not (eq (cl-prolog-kit::%proof-module-entries recursive-state)
                   (cl-prolog-kit::%proof-module-entries fact-state))))
      (is (cl-prolog-kit::%left-recursive-p '(shared-value ?value)
                                        recursive-state))
      (is (not (cl-prolog-kit::%left-recursive-p '(shared-value ?value)
                                             fact-state)))
      (let ((cl-prolog-kit::*current-table-session* session))
        (cl-prolog-kit::%prove-bindings/k
         '(shared-value ?value) recursive-rulebase '() nil
         (lambda (bindings)
           (push (logic-substitute '?value bindings) recursive-answers)
           (cl-prolog-kit::%prove-bindings/k
            '(shared-value ?value) fact-rulebase '() nil
            (lambda (nested-bindings)
              (push (logic-substitute '?value nested-bindings)
                    fact-answers))))))
      (is-equal '(first) (nreverse recursive-answers))
      (is-equal '(second) (nreverse fact-answers))
      (is-equal 2
                (hash-table-count
                 (cl-prolog-kit::%table-session-module-entries session)))
      (progn
        (is-equal 1
                  (hash-table-count
                   (cl-prolog-kit::rulebase-left-recursion-analysis
                    recursive-rulebase)))
        (is-equal 1
                  (hash-table-count
                   (cl-prolog-kit::rulebase-left-recursion-analysis
                    fact-rulebase))))
      (is-equal 2
                (hash-table-count
                 (cl-prolog-kit::%table-session-entries session))))))

(deftest interrupted-table-build-discards-partial-entry ()
  (let* ((rulebase (prolog
                     ((recursive ?x) (recursive ?x))
                     ((recursive value))))
         (session (cl-prolog-kit::%make-rulebase-table-session rulebase))
         (state (cl-prolog-kit::%make-proof-state
  rulebase
  (quote ())
  (cl-prolog-kit::%make-environment-index (quote ()))
  nil
  cl-prolog-kit::+default-prolog-module+
  session
  (cl-prolog-kit::%make-cut-tag))))
    (handler-case
        (cl-prolog-kit::%prove-clauses/k
         '(recursive ?x) state
         (lambda (answer-state)
           (declare (cl:ignore answer-state))
           (error "interrupt table construction")))
      (error () nil))
    (is-equal 0
              (hash-table-count
               (cl-prolog-kit::%table-session-entries session)))))

(deftest table-sessions-do-not-outlive-a-query-or-rulebase-revision ()
  (let ((rulebase (prolog ((value old)))))
    (is-equal '(((?x . old))) (query-prolog rulebase '(value ?x)))
    (rulebase-insert-clause! rulebase (make-clause '(value new)))
    (is-equal '(((?x . old)) ((?x . new)))
              (query-prolog rulebase '(value ?x)))))

(deftest proof-search-falls-back-when-no-constraint-hook-is-installed ()
  "*constraint-post-unify-hook* decouples fact/rule matching and unification
from the finite-domain subsystem (installed only once fd-store.lisp loads);
verify the direct-unification fallback taken by an absent hook still
produces the normal proof-search result."
  (let ((rulebase (prolog ((likes alice bob))
                          ((admires ?x ?y) (fond-of ?x ?y))
                          ((fond-of alice bob))))
        (cl-prolog-kit::*constraint-post-unify-hook* nil))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(likes alice ?y)))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(admires alice ?y)))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(= (alice . ?y) (alice . bob))))))

(deftest static-user-goal-defers-argument-substitution ()
  (let* ((rulebase (prolog ((deferred ready))))
         (goal (quote (deferred ?argument)))
         (bindings (quote ((?argument . ready))))
         (state (cl-prolog-kit::%make-proof-state
                 rulebase
                 bindings
                 (cl-prolog-kit::%make-environment-index bindings)
                 nil
                 cl-prolog-kit::+default-prolog-module+
                 (cl-prolog-kit::%make-rulebase-table-session rulebase)
                 (cl-prolog-kit::%make-cut-tag)))
         (original (symbol-function (quote cl-prolog-kit::%resolve-user-goal)))
         (observed nil)
         (succeeded nil))
    (unwind-protect
         (progn
           (setf (symbol-function (quote cl-prolog-kit::%resolve-user-goal))
                 (lambda (candidate current-state explicit-module)
                   (setf observed candidate)
                   (funcall original candidate current-state explicit-module)))
           (cl-prolog-kit::%prove-goal-dispatch/k
            goal
            state
            (lambda (answer-state)
              (declare (cl:ignore answer-state))
              (setf succeeded t)))
           (is succeeded)
           (is (eq goal observed)))
      (setf (symbol-function (quote cl-prolog-kit::%resolve-user-goal))
            original))))
(deftest rule-program-fast-path-preserves-rule-semantics ()
  (let* ((variable (fresh-logic-variable))
         (eligible
           (cl-prolog-kit::%compile-clause-template
            (make-clause (list (quote fast-same) variable variable)
                         (list (list (quote choice) variable)))))
         (cut-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (quote (fast-cut)) (quote ((!))))))
         (nested-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (quote (fast-nested (term value))) (quote ((true)))))))
    (is (cl-prolog-kit::%clause-template-rule-program eligible))
    (is (null (cl-prolog-kit::%clause-template-rule-program cut-template)))
    (is (null (cl-prolog-kit::%clause-template-rule-program nested-template))))
  ;; An improper goal list or an improper goal is not something the rule
  ;; program can encode, so compilation declines and the clause keeps the
  ;; general dispatch path instead of getting a truncated program.
  (let ((improper-body
          (cl-prolog-kit::%compile-clause-template
           (make-clause (quote (fast-improper-body))
                        (list* (quote (true)) (quote junk)))))
        (improper-goal
          (cl-prolog-kit::%compile-clause-template
           (make-clause (quote (fast-improper-goal))
                        (list (list* (quote choice) (quote junk)))))))
    (is (null (cl-prolog-kit::%clause-template-rule-program improper-body)))
    (is (null (cl-prolog-kit::%clause-template-rule-program improper-goal))))
  (let ((rulebase
          (prolog
            ((choice a))
            ((choice b))
            ((fast-pick ?x) (choice ?x))
            ((fast-same ?x ?x) (choice ?x))
            ((left a))
            ((left b))
            ((right a))
            ((right c))
            ((fast-paired ?x) (left ?x) (right ?x)))))
    (is-equal (quote (((?answer . a)) ((?answer . b))))
              (query-prolog rulebase (quote (fast-pick ?answer))))
    (is-equal (quote (nil)) (query-prolog rulebase (quote (fast-same a a))))
    (is (null (query-prolog rulebase (quote (fast-same a b)))))
    (is-equal (quote (((?answer . a))))
              (query-prolog rulebase (quote (fast-paired ?answer))))))
(deftest rule-program-lazily-materializes-shared-variables ()
  (let ((rulebase
          (prolog
            ((lazy-value a))
            ((lazy-value b))
            ((lazy-edge a linked))
            ((lazy-edge b blocked))
            ((lazy-accept linked))
            ((lazy-head-alias ?head) (lazy-value ?head))
            ((lazy-body-share ?head)
             (lazy-edge ?head ?body)
             (lazy-accept ?body))
            ((lazy-head-mismatch ?same ?same)
             (lazy-body-only ?body ?unused)))))
    (is-equal (quote (((?answer . a)) ((?answer . b))))
              (query-prolog rulebase
                            (quote (lazy-head-alias ?answer))))
    (is-equal (quote (((?answer . a))))
              (query-prolog rulebase
                            (quote (lazy-body-share ?answer))))
    (let ((original
            (symbol-function
             (quote cl-prolog-kit::%unify-rule-program-head)))
          (observed-variables nil))
      (unwind-protect
           (progn
             (setf (symbol-function
                    (quote cl-prolog-kit::%unify-rule-program-head))
                   (lambda (goal program variables environment parent-index)
                     (multiple-value-prog1
                         (funcall original goal program variables
                                  environment parent-index)
                       (setf observed-variables (copy-seq variables)))))
             (is (null
                  (query-prolog rulebase
                                (quote (lazy-head-mismatch left right)))))
             (is-equal 3 (length observed-variables))
             (is (svref observed-variables 0))
             (is (null (svref observed-variables 1)))
             (is (null (svref observed-variables 2))))
        (setf (symbol-function
               (quote cl-prolog-kit::%unify-rule-program-head))
              original)))))

(defun %make-rule-program-equivalence-rulebases ()
  (let* ((fast-variable (fresh-logic-variable))
         (fast-clause
           (make-clause
            (list (quote descriptor-candidate) fast-variable)
            (list (list (quote descriptor-choice) fast-variable)
                  (list (quote descriptor-choice) fast-variable))))
         (variable (fresh-logic-variable))
         (shared-goal (list (quote descriptor-choice) variable))
         (generic-clause
           (make-clause (list (quote descriptor-candidate) variable)
                        (list shared-goal shared-goal)))
         (fast-rulebase
           (prolog
             ((descriptor-choice a))
             ((descriptor-choice b))))
         (generic-rulebase
           (prolog
             ((descriptor-choice a))
             ((descriptor-choice b)))))
    (rulebase-insert-clause! fast-rulebase fast-clause)
    (rulebase-insert-clause! generic-rulebase generic-clause)
    (values fast-rulebase
            generic-rulebase
            (cl-prolog-kit::%compile-clause-template generic-clause))))

(defmacro with-rule-program-equivalence-rulebases ((fast-rulebase generic-rulebase generic-template) &body body)
  `(multiple-value-bind (,fast-rulebase ,generic-rulebase ,generic-template)
       (%make-rule-program-equivalence-rulebases)
     ,@body))

(deftest shared-and-cyclic-rule-graphs-use-observationally-equivalent-fallback ()
  (with-rule-program-equivalence-rulebases (fast-rulebase generic-rulebase generic-template)
    (is (null (cl-prolog-kit::%clause-template-rule-program generic-template)))
    (is-equal
     (query-prolog fast-rulebase (quote (descriptor-candidate ?answer)))
     (query-prolog generic-rulebase (quote (descriptor-candidate ?answer)))))
  (let* ((variable (fresh-logic-variable))
         (goal (list (quote descriptor-choice) variable))
         (cyclic-body (list goal)))
    (setf (cdr cyclic-body) cyclic-body)
    (is
     (null
      (cl-prolog-kit::%clause-template-rule-program
       (cl-prolog-kit::%compile-clause-template
        (make-clause (list (quote cyclic-candidate) variable)
                     cyclic-body)))))))

(deftest clause-materialization-context-is-lazy-and-preserves-graph-identity () (let* ((variable (fresh-logic-variable)) (shared (list (quote shared) variable)) (cycle (cons (quote cycle) nil)) (head (cons (quote lazy-graph) (cons variable (quote dotted-tail))))) (setf (cdr cycle) cycle) (let* ((template (cl-prolog-kit::%compile-clause-template (make-clause head (list shared shared cycle)))) (context (cl-prolog-kit::%make-clause-template-materialization-context template)) (materialized-head (cl-prolog-kit::%materialize-clause-template-head template context)) (conses (cl-prolog-kit::%clause-materialization-context-conses context))) (is (some (function null) conses)) (is (eq (quote dotted-tail) (cddr materialized-head))) (let ((materialized-body (cl-prolog-kit::%materialize-clause-template-body template context))) (is (eq (first materialized-body) (second materialized-body))) (is (eq (cadr materialized-head) (cadr (first materialized-body)))) (is (eq (third materialized-body) (cdr (third materialized-body)))) (is (every (function identity) conses))) (let* ((other-context (cl-prolog-kit::%make-clause-template-materialization-context template)) (other-head (cl-prolog-kit::%materialize-clause-template-head template other-context))) (is (not (eq (cadr materialized-head) (cadr other-head))))))))

(deftest rule-program-body-avoids-materialization-only-for-safe-direct-calls ()
  (with-rule-program-equivalence-rulebases
      (fast-rulebase generic-rulebase generic-template)
    (declare (ignore generic-rulebase generic-template))
    (let ((cl-prolog-kit::*rule-program-goal-materialization-count* 0))
      (is-equal
       (quote (((?answer . a)) ((?answer . b))))
       (query-prolog
        fast-rulebase
        (quote (descriptor-candidate ?answer))))
      (is-equal 0 cl-prolog-kit::*rule-program-goal-materialization-count*)))
  (let ((rulebase
          (prolog
            ((materialized-builtin) (true)))))
    (let ((cl-prolog-kit::*rule-program-goal-materialization-count* 0))
      (is-equal (quote (nil))
                (query-prolog rulebase (quote (materialized-builtin))))
      (is-equal 1 cl-prolog-kit::*rule-program-goal-materialization-count*))))



(deftest-queries instruction-head-fast-path-preserves-unification-semantics
    ((prolog
      ((same-through-body ?value) (pair ?value ?value))
      ((literal-through-body) (pair alpha alpha))
      ((nested-through-body ?value) (wrapped (box ?value)))
      ((pair alpha alpha))
      ((pair alpha beta))
      ((wrapped (box nested)))))
  ((same-through-body ?answer) :ordered (((?answer . alpha))))
  ((literal-through-body) :ordered (nil))
  ((nested-through-body ?answer) :ordered (((?answer . nested)))))

(deftest instruction-head-fast-path-rolls-back-partial-direct-bindings ()
  (let* ((caller-variable (fresh-logic-variable))
         (callee-variable (fresh-logic-variable))
         (parent-variable (fresh-logic-variable))
         (caller-program
           (cl-prolog-kit::%clause-template-rule-program
            (cl-prolog-kit::%compile-clause-template
             (make-clause
              (list (quote caller) caller-variable)
              (list (list (quote pair) caller-variable (quote actual)))))))
         (callee-program
           (cl-prolog-kit::%clause-template-rule-program
            (cl-prolog-kit::%compile-clause-template
             (make-clause
              (list (quote pair) callee-variable (quote expected))))))
         (instruction (svref (cl-prolog-kit::%rule-program-body caller-program) 0))
         (caller-variables
           (make-array (cl-prolog-kit::%rule-program-variable-count caller-program)
                       :initial-element nil))
         (callee-variables
           (make-array (cl-prolog-kit::%rule-program-variable-count callee-program)
                       :initial-element nil))
         (environment (list (cons parent-variable (quote stable))))
         (parent-index (cl-prolog-kit::%make-environment-index environment)))
    (multiple-value-bind (result-environment ok result-index)
        (cl-prolog-kit::%unify-rule-program-instruction-head
         instruction caller-variables callee-program callee-variables
         environment parent-index)
      (is (null ok))
      (is (eq environment result-environment))
      (is (eq parent-index result-index))
      (is (nth-value 1
            (cl-prolog-kit::%environment-index-binding parent-variable parent-index)))
      (is (null
           (nth-value 1
             (cl-prolog-kit::%environment-index-binding
              (svref caller-variables 0) parent-index))))
      (is (null
           (nth-value 1
             (cl-prolog-kit::%environment-index-binding
              (svref callee-variables 0) parent-index)))))))
(deftest instruction-head-fast-path-reuses-only-inactive-scratch ()
  (labels ((invoke (second-callee)
             (let* ((caller-left (fresh-logic-variable))
                    (caller-right (fresh-logic-variable))
                    (callee-left (fresh-logic-variable))
                    (callee-right (fresh-logic-variable))
                    (caller-program
                      (cl-prolog-kit::%clause-template-rule-program
                       (cl-prolog-kit::%compile-clause-template
                        (make-clause
                         (quote (caller))
                         (list (list (quote pair)
                                     caller-left
                                     caller-right))))))
                    (callee-program
                      (cl-prolog-kit::%clause-template-rule-program
                       (cl-prolog-kit::%compile-clause-template
                        (make-clause
                         (list (quote pair) callee-left callee-right)))))
                    (instruction
                      (svref (cl-prolog-kit::%rule-program-body caller-program) 0))
                    (caller-variables
                      (make-array
                       (cl-prolog-kit::%rule-program-variable-count caller-program)
                       :initial-element nil))
                    (callee-variables
                      (make-array
                       (cl-prolog-kit::%rule-program-variable-count callee-program)
                       :initial-element nil))
                    (caller-operands
                      (cl-prolog-kit::%rule-instruction-operands instruction))
                    (callee-operands
                      (cl-prolog-kit::%rule-program-head-operands callee-program))
                    (environment
                      (list
                       (cons (cl-prolog-kit::%rule-program-operand-value
                              (svref caller-operands 0) caller-variables)
                             (quote (box alpha)))
                       (cons (cl-prolog-kit::%rule-program-operand-value
                              (svref caller-operands 1) caller-variables)
                             (quote (box beta)))
                       (cons (cl-prolog-kit::%rule-program-operand-value
                              (svref callee-operands 0) callee-variables)
                             (quote (box alpha)))
                       (cons (cl-prolog-kit::%rule-program-operand-value
                              (svref callee-operands 1) callee-variables)
                             second-callee)))
                    (parent-index
                      (cl-prolog-kit::%make-environment-index environment)))
               (cl-prolog-kit::%unify-rule-program-instruction-head
                instruction caller-variables callee-program callee-variables
                environment parent-index))))
    (let ((scratch (cl-prolog-kit::%make-unification-scratch))
          (marker-left (list (quote marker-left)))
          (marker-right (list (quote marker-right))))
      (is (not (cl-prolog-kit::%unification-scratch-remember-pair
                scratch marker-left marker-right)))
      (let ((cl-prolog-kit::*unification-scratch* scratch))
        (is (nth-value 1 (invoke (quote (box beta))))))
      (is (not (cl-prolog-kit::%unification-scratch-active-p scratch)))
      (is (let ((index (cl-prolog-kit::%unification-scratch-first-index scratch)))
            (or (null index) (zerop (hash-table-count index)))))
      (let ((cl-prolog-kit::*unification-scratch* scratch))
        (is (null (nth-value 1 (invoke (quote (box mismatch)))))))
      (is (not (cl-prolog-kit::%unification-scratch-active-p scratch)))
      (is (let ((index (cl-prolog-kit::%unification-scratch-first-index scratch)))
            (or (null index) (zerop (hash-table-count index))))))
    (let ((scratch (cl-prolog-kit::%make-unification-scratch))
          (marker-left (list (quote active-marker-left)))
          (marker-right (list (quote active-marker-right))))
      (is (not (cl-prolog-kit::%unification-scratch-remember-pair
                scratch marker-left marker-right)))
      (setf (cl-prolog-kit::%unification-scratch-active-p scratch) t)
      (let ((cl-prolog-kit::*unification-scratch* scratch))
        (is (nth-value 1 (invoke (quote (box beta))))))
      (is (cl-prolog-kit::%unification-scratch-active-p scratch))
      (is (cl-prolog-kit::%unification-scratch-remember-pair
           scratch marker-left marker-right))
      (setf (cl-prolog-kit::%unification-scratch-active-p scratch) nil)
      (cl-prolog-kit::%reset-unification-scratch scratch))))


(deftest constraint-hook-continuation-reentry-matches-generic-fallback ()
  (with-rule-program-equivalence-rulebases (fast-rulebase generic-rulebase generic-template)
    (declare (ignore generic-template))
    (let ((cl-prolog-kit::*constraint-post-unify-hook*
            (lambda (environment continuation)
              (funcall continuation environment)
              (funcall continuation environment))))
      (let ((fast-solutions
              (query-prolog fast-rulebase
                            (quote (descriptor-candidate ?answer))))
            (generic-solutions
              (query-prolog generic-rulebase
                            (quote (descriptor-candidate ?answer)))))
        (is-equal generic-solutions fast-solutions)
        (is-equal 16 (length fast-solutions))))))

(deftest rule-program-and-fallback-share-logical-update-snapshot-semantics ()
  (labels ((observe-update (rulebase)
             (let ((seen (quote ()))
                   (inserted-p nil))
               (map-prolog-solutions
                (lambda (solution)
                  (push (solution-binding (quote ?answer) solution) seen)
                  (unless inserted-p
                    (setf inserted-p t)
                    (rulebase-insert-clause!
                     rulebase
                     (make-clause (quote (descriptor-choice c))))))
                rulebase
                (quote (descriptor-candidate ?answer)))
               (values
                (nreverse seen)
                (mapcar
                 (lambda (solution)
                   (solution-binding (quote ?answer) solution))
                 (query-prolog
                  rulebase
                  (quote (descriptor-candidate ?answer))))))))
    (with-rule-program-equivalence-rulebases (fast-rulebase generic-rulebase generic-template)
      (declare (ignore generic-template))
      (multiple-value-bind (fast-current fast-next)
          (observe-update fast-rulebase)
        (multiple-value-bind (generic-current generic-next)
            (observe-update generic-rulebase)
          (is-equal (quote (a b)) fast-current)
          (is-equal fast-current generic-current)
          (is-equal (quote (a b c)) fast-next)
          (is-equal fast-next generic-next))))))

(deftest rule-program-and-fallback-share-depth-boundaries ()
  (with-rule-program-equivalence-rulebases (fast-rulebase generic-rulebase generic-template)
    (declare (ignore generic-template))
    (let ((fast-solutions
            (query-prolog fast-rulebase
                          (quote (descriptor-candidate ?answer))
                          :max-depth 1))
          (generic-solutions
            (query-prolog generic-rulebase
                          (quote (descriptor-candidate ?answer))
                          :max-depth 1)))
      (is-equal (quote (((?answer . a)) ((?answer . b)))) fast-solutions)
      (is-equal fast-solutions generic-solutions))
    (signals-prolog-condition prolog-depth-limit-exceeded
      (query-prolog fast-rulebase
                    (quote (descriptor-candidate ?answer))
                    :max-depth 0))
    (signals-prolog-condition prolog-depth-limit-exceeded
      (query-prolog generic-rulebase
                    (quote (descriptor-candidate ?answer))
                    :max-depth 0))))

(deftest flat-fact-rule-program-eligibility ()
  (let* ((variable (fresh-logic-variable))
         (shared-term (list (quote term) (quote value)))
         (cyclic-head (list (quote flat-cycle) (quote value)))
         (ground-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (quote (flat-ground value)))))
         (variable-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (list (quote flat-variable) variable variable))))
         (nested-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (quote (flat-nested (term value))))))
         (improper-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (cons (quote flat-improper) (quote tail)))))
         (shared-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause
             (list (quote flat-shared) shared-term shared-term))))
         (variable-predicate-template
           (cl-prolog-kit::%compile-clause-template
            (make-clause (list variable (quote value))))))
    (setf (cddr cyclic-head) cyclic-head)
    (let* ((cyclic-template
             (cl-prolog-kit::%compile-clause-template
              (make-clause cyclic-head)))
           (ground-program
             (cl-prolog-kit::%clause-template-rule-program ground-template))
           (variable-program
             (cl-prolog-kit::%clause-template-rule-program variable-template)))
      (is ground-program)
      (is (typep (cl-prolog-kit::%rule-program-body ground-program)
                 (quote simple-vector)))
      (is-equal 0
                (length (cl-prolog-kit::%rule-program-body ground-program)))
      (is-equal 0
                (cl-prolog-kit::%rule-program-variable-count ground-program))
      (is variable-program)
      (is-equal 1
                (cl-prolog-kit::%rule-program-variable-count variable-program))
      (is-equal 0
                (length (cl-prolog-kit::%rule-program-body variable-program)))
      (is (null (cl-prolog-kit::%clause-template-rule-program nested-template)))
      (is (null (cl-prolog-kit::%clause-template-rule-program improper-template)))
      (is (null (cl-prolog-kit::%clause-template-rule-program shared-template)))
      (is (null
           (cl-prolog-kit::%clause-template-rule-program
            variable-predicate-template)))
      (is (null (cl-prolog-kit::%clause-template-rule-program cyclic-template))))))
(deftest flat-fact-rule-program-supports-257-distinct-variables ()
    (let* ((variables (loop repeat 257 collect (fresh-logic-variable)))
           (ground-atoms
             (loop for ordinal below 257
                   collect (intern (format nil "WIDE-GROUND-~D" ordinal))))
           (head (cons (quote wide) variables))
           (template
             (cl-prolog-kit::%compile-clause-template (make-clause head)))
           (rulebase (make-rulebase :clauses (list (make-clause head)))))
      (is (cl-prolog-kit::%clause-template-rule-program template))
      (is (prolog-succeeds-p rulebase (cons (quote wide) ground-atoms)))))

  (deftest rule-program-private-variable-registration-preserves-ordinal ()
    (cl-prolog-kit::%with-logic-variable-order
      (let ((rule-variable (cl-prolog-kit::%fresh-rule-program-variable)))
        (is-equal 0 (cl-prolog-kit::%logic-variable-ordinal rule-variable))
        (cl-prolog-kit::%register-logic-variable rule-variable)
        (is-equal 1
                  (cl-prolog-kit::%logic-variable-ordinal
                   (fresh-logic-variable))))))
  (deftest rule-program-private-variable-falls-back-after-cached-boundary ()
    (cl-prolog-kit::%with-logic-variable-order
      (loop repeat (length cl-prolog-kit::*rule-program-variable-names*)
            do (fresh-logic-variable))
      (let ((rule-variable (cl-prolog-kit::%fresh-rule-program-variable)))
        (is-equal (length cl-prolog-kit::*rule-program-variable-names*)
                  (cl-prolog-kit::%logic-variable-ordinal rule-variable)))))
  (deftest logic-variable-ordinal-context-invariants ()
    (signals-error
     (cl-prolog-kit::%register-logic-variable (gensym "?UNREGISTERED")))
    (cl-prolog-kit::%with-logic-variable-order
      (signals-error
       (cl-prolog-kit::%logic-variable-ordinal (gensym "?UNREGISTERED")))))
(deftest flat-fact-rule-program-preserves-runtime-semantics ()
  (let ((rulebase
          (prolog
            ((flat-choice first))
            ((flat-choice second))
            ((flat-choice first extra))
            ((flat-other first other))
            ((flat-any ?value))
            ((flat-same ?value ?value)))))
    (is-equal (quote (((?answer . first)) ((?answer . second))))
              (query-prolog rulebase (quote (flat-choice ?answer))))
    (is (null (query-prolog rulebase (quote (flat-choice first second)))))
    (is (null (query-prolog rulebase (quote (flat-other first second)))))
    (is-equal (quote (nil))
              (query-prolog rulebase
                            (quote (flat-choice first))
                            :max-depth 0))
    (is-equal (quote (nil))
              (query-prolog rulebase (quote (flat-same same same))))
    (is (null (query-prolog rulebase (quote (flat-same left right)))))
    (is
     (prolog-succeeds-p
      rulebase
      (quote
       (and (flat-any ?left)
            (= ?left left)
            (flat-any ?right)
            (= ?right right)))))))
(deftest generic-rule-materializes-body-only-after-head-success () (let* ((variable (fresh-logic-variable)) (shared-goal (list (quote true))) (clause (make-clause (list (quote delayed-body) (quote expected) variable) (list shared-goal shared-goal))) (template (cl-prolog-kit::%compile-clause-template clause)) (rulebase (make-rulebase)) (calls 0) (original (symbol-function (quote cl-prolog-kit::%materialize-clause-template-body)))) (is (null (cl-prolog-kit::%clause-template-rule-program template))) (rulebase-insert-clause! rulebase clause) (unwind-protect (progn (setf (symbol-function (quote cl-prolog-kit::%materialize-clause-template-body)) (lambda (template context) (incf calls) (funcall original template context))) (is (null (query-prolog rulebase (quote (delayed-body mismatch ?answer))))) (is-equal 0 calls) (let* ((results (query-prolog rulebase (quote (delayed-body expected ?answer)))) (answer (and results (cdr (assoc (quote ?answer) (first results)))))) (is results) (is (logic-var-p answer))) (is-equal 1 calls)) (setf (symbol-function (quote cl-prolog-kit::%materialize-clause-template-body)) original))))
(deftest rule-program-head-uses-only-semantically-safe-operand-fast-paths ()
  (labels ((program-for (head)
             (cl-prolog-kit::%clause-template-rule-program
              (cl-prolog-kit::%compile-clause-template (make-clause head))))
           (generic-head (goal program variables environment parent-index)
             (let ((arguments (cdr goal))
                   (operands (cl-prolog-kit::%rule-program-head-operands program))
                   (extended environment)
                   (index parent-index))
               (dotimes (operand-index (length operands)
                         (values extended t index))
                 (multiple-value-bind (next-extended ok next-index)
                     (cl-prolog-kit::%unify-indexed
                      (car arguments)
                      (cl-prolog-kit::%rule-program-operand-value
                       (svref operands operand-index) variables)
                      extended index (not (eq index parent-index)))
                   (unless ok
                     (return-from generic-head
                       (values environment nil parent-index)))
                   (setf extended next-extended
                         index next-index
                         arguments (cdr arguments))))))
           (run-parity (goal head &optional (environment (quote ())))
             (let* ((program (program-for head))
                    (variable-count
                      (cl-prolog-kit::%rule-program-variable-count program))
                    (fast-variables
                      (make-array variable-count :initial-element nil))
                    (generic-variables
                      (make-array variable-count :initial-element nil))
                    (parent-index
                      (cl-prolog-kit::%make-environment-index environment)))
               (multiple-value-bind
                     (fast-environment fast-ok fast-index)
                   (cl-prolog-kit::%unify-rule-program-head
                    goal program fast-variables environment parent-index)
                 (multiple-value-bind
                       (generic-environment generic-ok generic-index)
                     (generic-head
                      goal program generic-variables environment parent-index)
                   (is (eq (not (null fast-ok))
                           (not (null generic-ok))))
                   (values fast-environment fast-ok fast-index
                           generic-environment generic-ok generic-index
                           parent-index))))))
    (multiple-value-bind
          (fast-environment fast-ok fast-index
           generic-environment generic-ok generic-index parent-index)
        (run-parity (quote (head literal)) (quote (head literal)))
      (is fast-ok)
      (is (null fast-environment))
      (is (null generic-environment))
      (is (eq fast-index parent-index))
      (is (eq generic-index parent-index)))
    (let ((query-variable (fresh-logic-variable)))
      (multiple-value-bind
            (fast-environment fast-ok fast-index
             generic-environment generic-ok generic-index parent-index)
          (run-parity
           (list (quote head) query-variable)
           (list (quote head) (fresh-logic-variable)))
        (declare (ignore fast-environment generic-environment parent-index))
        (is fast-ok)
        (let ((fast-result
                (cl-prolog-kit::%logic-substitute-indexed
                 (list (quote head) query-variable) fast-index))
              (generic-result
                (cl-prolog-kit::%logic-substitute-indexed
                 (list (quote head) query-variable) generic-index)))
          (is (eq (car fast-result) (quote head)))
          (is (eq (car generic-result) (quote head)))
          (is (logic-var-p (cadr fast-result)))
          (is (logic-var-p (cadr generic-result)))
          (is (null
               (nth-value 1
                 (cl-prolog-kit::%environment-index-binding
                  (cadr fast-result) fast-index))))
          (is (null
               (nth-value 1
                 (cl-prolog-kit::%environment-index-binding
                  (cadr generic-result) generic-index)))))))
    (let ((query-variable (fresh-logic-variable)))
      (multiple-value-bind
            (fast-environment fast-ok fast-index
             generic-environment generic-ok generic-index parent-index)
          (run-parity
           (list (quote head) query-variable query-variable)
           (list (quote head)
                 (fresh-logic-variable)
                 (fresh-logic-variable)))
        (declare (ignore fast-environment generic-environment parent-index))
        (is fast-ok)
        (let ((fast-result
                (cl-prolog-kit::%logic-substitute-indexed
                 (list (quote head) query-variable query-variable)
                 fast-index))
              (generic-result
                (cl-prolog-kit::%logic-substitute-indexed
                 (list (quote head) query-variable query-variable)
                 generic-index)))
          (is (logic-var-p (cadr fast-result)))
          (is (logic-var-p (cadr generic-result)))
          (is (eq (cadr fast-result) (caddr fast-result)))
          (is (eq (cadr generic-result) (caddr generic-result))))))
    (let ((goal-atom (make-symbol "SAME-NAME"))
          (head-atom (make-symbol "SAME-NAME")))
      (is (not (eq goal-atom head-atom)))
      (multiple-value-bind
            (fast-environment fast-ok fast-index
             generic-environment generic-ok generic-index parent-index)
          (run-parity
           (list (quote head) goal-atom)
           (list (quote head) head-atom))
        (is fast-ok)
        (is (null fast-environment))
        (is (null generic-environment))
        (is (eq fast-index parent-index))
        (is (eq generic-index parent-index))))
    (let ((query-variable (fresh-logic-variable)))
      (multiple-value-bind
            (fast-environment fast-ok fast-index
             generic-environment generic-ok generic-index parent-index)
          (run-parity
           (list (quote head) query-variable (quote wrong))
           (quote (head literal expected)))
        (is (null fast-ok))
        (is (null generic-ok))
        (is (null fast-environment))
        (is (null generic-environment))
        (is (eq fast-index parent-index))
        (is (eq generic-index parent-index))
        (is (null
             (nth-value 1
               (cl-prolog-kit::%environment-index-binding
                query-variable parent-index))))))
    (dolist (case
              (list
               (list (quote (head value))
                     (list (quote head) (fresh-logic-variable)))
               (let ((variable (fresh-logic-variable)))
                 (list (quote (head same same))
                       (list (quote head) variable variable)))
               (list (quote (head (compound value)))
                     (list (quote head) (fresh-logic-variable)))))
      (destructuring-bind (goal head) case
        (multiple-value-bind
              (fast-environment fast-ok fast-index
               generic-environment generic-ok generic-index parent-index)
            (run-parity goal head)
          (declare (ignore parent-index))
          (is fast-ok)
          (is (equal
               (cl-prolog-kit::%logic-substitute-indexed goal fast-index)
               (cl-prolog-kit::%logic-substitute-indexed goal generic-index)))
          (is (equal
               (cl-prolog-kit::%logic-substitute-indexed
                fast-environment fast-index)
               (cl-prolog-kit::%logic-substitute-indexed
                generic-environment generic-index))))))))
