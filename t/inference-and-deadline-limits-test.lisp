;;;; Inference budget and deadline limits of top-level queries.

(in-package #:cl-prolog-kit.tests)

(defun limits-test-graph ()
  "Return a fresh rulebase whose PATH/2 rules run on the direct rule-program
path.  Fresh per query so no table or descriptor cache changes the count."
  (prolog
    ((edge a b))
    ((edge b c))
    ((edge c d))
    ((path ?x ?y) (edge ?x ?y))
    ((path ?x ?y) (edge ?x ?z) (path ?z ?y))))

;;; Measured inference counts against LIMITS-TEST-GRAPH.  `path(a, d)' makes
;;; exactly one dispatch through %PROVE-GOAL/K (the query goal); the other
;;; eleven are EDGE and recursive PATH calls dispatched by the direct
;;; rule-program path, so these counts also pin that path's accounting.
(defconstant +path-a-d-inferences+ 12)
(defconstant +path-a-d-first-inferences+ 8)
(defconstant +path-a-all-inferences+ 12)

(defmacro limits-test-condition ((condition-class) &body body)
  "Return the CONDITION-CLASS condition BODY signals, or NIL when it signals none."
  `(handler-case (progn ,@body nil)
     (,condition-class (condition) condition)))

(defun limits-test-type-error-p (function)
  "Return true when calling FUNCTION signals a TYPE-ERROR."
  (handler-case (progn (funcall function) nil)
    (type-error () t)))

(defun limits-test-query (rulebase text &rest options)
  "Run the Prolog query TEXT, read as Prolog source so its atoms name the
builtins and the predicates CONSULT-PROLOG defined."
  (apply #'query-prolog rulebase (read-prolog-term text) options))

(defun limits-test-past-deadline ()
  (1- (get-internal-real-time)))

(deftest inference-limit-boundary-is-exact ()
  (is-equal '(nil)
            (query-prolog (limits-test-graph) '(path a d)
                          :max-inferences +path-a-d-inferences+))
  (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                     (query-prolog (limits-test-graph) '(path a d)
                                   :max-inferences
                                   (1- +path-a-d-inferences+)))))
    (is condition)
    (is (typep condition 'prolog-resource-error))
    (is-equal (1- +path-a-d-inferences+)
              (prolog-inference-limit-exceeded-limit condition))))

(deftest inference-limit-counts-the-direct-rule-program-path ()
  ;; Only the query goal goes through %PROVE-GOAL/K; every EDGE and PATH call
  ;; below it is dispatched by %PROVE-RULE-PROGRAM-DIRECT/K.  A budget of one
  ;; would therefore suffice if the direct path were not counted.
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (limits-test-graph) '(path a d) :max-inferences 1)))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (limits-test-graph) '(path a ?x)
                      :max-inferences (1- +path-a-all-inferences+)))))

(deftest builtin-costs-its-dispatch-and-each-solution ()
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (make-rulebase) '(true) :max-inferences 0)))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (make-rulebase) '(true) :max-inferences 1)))
  (is-equal '(nil) (query-prolog (make-rulebase) '(true) :max-inferences 2))
  ;; One dispatch plus three solutions.
  (is-equal 3 (length (limits-test-query (make-rulebase) "between(1, 3, X)."
                                         :max-inferences 4)))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (limits-test-query (make-rulebase) "between(1, 3, X)."
                           :max-inferences 3))))

(deftest inference-limit-stops-an-infinite-loop ()
  (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                     (query-prolog (make-rulebase) '(and (repeat) (fail))
                                   :max-inferences 1000))))
    (is condition)
    (is-equal 1000 (prolog-inference-limit-exceeded-limit condition))))

(deftest deadline-in-the-past-stops-the-query-at-start ()
  (let* ((deadline (limits-test-past-deadline))
         (condition (limits-test-condition (prolog-deadline-exceeded)
                      (query-prolog (make-rulebase) '(true)
                                    :deadline deadline))))
    (is condition)
    (is (typep condition 'prolog-resource-error))
    (is-equal deadline (prolog-deadline-exceeded-deadline condition))))

(deftest short-deadline-stops-an-infinite-loop ()
  (is (limits-test-condition (prolog-deadline-exceeded)
        (query-prolog (make-rulebase) '(and (repeat) (fail))
                      :deadline (prolog-deadline-after 1/20)))))

(deftest future-deadline-lets-a-finite-query-finish ()
  (is-equal '(nil)
            (query-prolog (limits-test-graph) '(path a d)
                          :deadline (prolog-deadline-after 60))))

(deftest nested-findall-and-setof-share-the-query-budget ()
  (is-equal 3 (length (query-prolog (limits-test-graph) '(path a ?x)
                                    :max-inferences +path-a-all-inferences+)))
  ;; The collector costs its dispatch and its one solution on top of the
  ;; inner enumeration.
  (dolist (collector '(findall setof))
    (let ((query (list collector '?x '(path a ?x) '?l))
          (needed (+ 2 +path-a-all-inferences+)))
      (is-equal 1 (length (query-prolog (limits-test-graph) query
                                        :max-inferences needed)))
      (is (limits-test-condition (prolog-inference-limit-exceeded)
            (query-prolog (limits-test-graph) query
                          :max-inferences (1- needed)))))))

(deftest catch-does-not-intercept-inference-or-deadline-limits ()
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (make-rulebase)
                      '(catch (and (repeat) (fail)) ?error true)
                      :max-inferences 100)))
  (is (limits-test-condition (prolog-deadline-exceeded)
        (query-prolog (make-rulebase)
                      '(catch (and (repeat) (fail)) ?error true)
                      :deadline (prolog-deadline-after 1/20))))
  ;; Ordinary errors stay catchable under a limit.
  (is-equal 1 (length (query-prolog (make-rulebase)
                                    '(catch (throw ball) ball true)
                                    :max-inferences 100))))

(deftest setup-call-cleanup-runs-cleanup-when-a-limit-unwinds ()
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog rulebase
                        '(setup_call_cleanup true (and (repeat) (fail))
                          (assertz (cleaned inferences)))
                        :max-inferences 100)))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (query-prolog rulebase
                        '(setup_call_cleanup true (and (repeat) (fail))
                          (assertz (cleaned deadline)))
                        :deadline (prolog-deadline-after 1/20))))
    (is-equal '(((?why . inferences)) ((?why . deadline)))
              (query-prolog rulebase '(cleaned ?why)))))

(deftest limit-specials-are-the-query-defaults ()
  (let ((*max-prolog-inferences* (1- +path-a-d-inferences+)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog (limits-test-graph) '(path a d))))
    (is-equal '(nil) (query-prolog (limits-test-graph) '(path a d)
                                   :max-inferences nil)))
  (let ((*max-prolog-inferences* +path-a-d-inferences+))
    (is-equal '(nil) (query-prolog (limits-test-graph) '(path a d))))
  (let ((*prolog-deadline* (limits-test-past-deadline)))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (query-prolog (make-rulebase) '(true))))
    (is-equal '(nil) (query-prolog (make-rulebase) '(true) :deadline nil))))

(deftest invalid-limit-values-signal-type-error ()
  (dolist (value '(-1 1.5 limit))
    (is (limits-test-type-error-p
         (lambda () (query-prolog (make-rulebase) '(true)
                                  :max-inferences value))))
    (is (limits-test-type-error-p
         (lambda () (prolog-succeeds-p (make-rulebase) '(true)
                                       :max-inferences value)))))
  (dolist (value '(1.5 "soon"))
    (is (limits-test-type-error-p
         (lambda () (query-prolog-first (make-rulebase) '(true)
                                        :deadline value))))
    (is (limits-test-type-error-p
         (lambda () (prolog-succeeds-p (make-rulebase) '(true)
                                       :deadline value)))))
  (is (limits-test-type-error-p
       (lambda () (let ((*max-prolog-inferences* -1))
                    (query-prolog (make-rulebase) '(true))))))
  (is (limits-test-type-error-p
       (lambda () (let ((*prolog-deadline* 1.5))
                    (query-prolog (make-rulebase) '(true))))))
  (dolist (value '(-1 limit))
    (is (limits-test-type-error-p
         (lambda () (prolog-deadline-after value))))))

(deftest prolog-succeeds-p-and-query-prolog-first-accept-limits ()
  (is (prolog-succeeds-p (limits-test-graph) '(path a d)
                         :max-inferences +path-a-d-first-inferences+))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (prolog-succeeds-p (limits-test-graph) '(path a d)
                           :max-inferences
                           (1- +path-a-d-first-inferences+))))
  (is (limits-test-condition (prolog-deadline-exceeded)
        (prolog-succeeds-p (make-rulebase) '(true)
                           :deadline (limits-test-past-deadline))))
  (is-equal '(nil t)
            (multiple-value-list
             (query-prolog-first (limits-test-graph) '(path a d)
                                 :max-inferences
                                 +path-a-d-first-inferences+)))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog-first (limits-test-graph) '(path a d)
                            :max-inferences
                            (1- +path-a-d-first-inferences+))))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (with-prolog-query () ((limits-test-graph) '(path a d)
                               :max-inferences
                               (1- +path-a-d-first-inferences+))
          t)))
  (is (with-prolog-query () ((limits-test-graph) '(path a d)
                             :max-inferences +path-a-d-first-inferences+)
        t))
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (map-prolog-solutions #'identity rulebase '(and (repeat) (fail))
                                :max-inferences 10)))))

(define-foreign-predicate (limits-test-nested-path budget)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (if (integerp budget)
      (query-prolog rulebase '(path a d) :max-inferences budget)
      (query-prolog rulebase '(path a d)))
  (funcall emit environment))

(deftest nested-top-level-query-is-charged-to-the-enclosing-budget ()
  ;; The nested query draws on the enclosing budget whether or not it has a
  ;; limit of its own: the foreign goal's dispatch and solution plus the
  ;; nested proof.
  (let ((needed (+ 2 +path-a-d-inferences+)))
    (dolist (goal (list '(limits-test-nested-path none)
                        (list 'limits-test-nested-path +path-a-d-inferences+)))
      (is-equal '(nil) (query-prolog (limits-test-graph) goal
                                     :max-inferences needed
                                     :deadline (prolog-deadline-after 1)))
      (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                         (query-prolog (limits-test-graph) goal
                                       :max-inferences (1- needed)
                                       :deadline (prolog-deadline-after 1)))))
        (is condition)
        (is-equal (1- needed) (prolog-inference-limit-exceeded-limit condition)))))
  ;; An own limit cannot lift the enclosing one; the stop reports the
  ;; enclosing limit, the tighter bound.
  (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                     (query-prolog (limits-test-graph)
                                   (list 'limits-test-nested-path
                                         +path-a-d-inferences+)
                                   :max-inferences 2
                                   :deadline (prolog-deadline-after 1)))))
    (is condition)
    (is-equal 2 (prolog-inference-limit-exceeded-limit condition)))
  ;; A tighter own limit applies and is the one reported.
  (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                     (query-prolog (limits-test-graph)
                                   (list 'limits-test-nested-path
                                         (1- +path-a-d-inferences+))
                                   :max-inferences 1000
                                   :deadline (prolog-deadline-after 1)))))
    (is condition)
    (is-equal (1- +path-a-d-inferences+)
              (prolog-inference-limit-exceeded-limit condition))))

;;; Regression tests for limit escapes.  Every query carries a deadline as
;;; well as an inference budget, so a regression fails the test instead of
;;; hanging the suite.

(defmacro limits-test-stopped-p (&body body)
  "Return true when BODY raises either limit condition."
  `(handler-case (progn ,@body nil)
     (prolog-inference-limit-exceeded () t)
     (prolog-deadline-exceeded () t)))

(deftest builtin-solutions-are-charged-against-the-budget ()
  (dolist (query '("findall(x, repeat, L)."
                   "findall(L, length(L, _), Ls)."
                   "aggregate_all(count, repeat, C)."
                   "call_nth(repeat, 1000000000000)."
                   "findall(X, between(1, 100000000000, X), L)."
                   "r."))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (limits-test-query (consult-prolog "r :- repeat." (make-rulebase))
                             query
                             :max-inferences 200
                             :deadline (prolog-deadline-after 1)))
        (format nil "~A must exhaust its inference budget" query))))

(defun limits-test-cleanup-rules ()
  (consult-prolog "lp :- repeat, fail.
c :- setup_call_cleanup(true, lp, c).
nest(0) :- lp.
nest(N) :- N1 is N - 1, setup_call_cleanup(true, nest(N1), (assertz(ran), lp))."
                  (make-rulebase)))

(deftest recursive-cleanup-cannot-mint-new-allowances ()
  (is (limits-test-stopped-p
        (limits-test-query (limits-test-cleanup-rules) "c."
                      :max-inferences 2000
                      :deadline (prolog-deadline-after 1/5)))))

(deftest nested-cleanups-share-one-allowance ()
  (dolist (depth '(5 200))
    (let ((rulebase (limits-test-cleanup-rules)))
      (is (limits-test-stopped-p
            (limits-test-query rulebase (format nil "nest(~D)." depth)
                          :max-inferences 2000
                          :deadline (prolog-deadline-after 1/5))))
      ;; The first cleanup drains the pool, so every later one stops at its
      ;; first goal, before its assertz/1.
      (is-equal 1 (length (limits-test-query rulebase "ran."))))))

(defvar *limits-test-late-calls* 0
  "LIMITS-TEST-NOTE-CALL calls made once the effective deadline had passed.")

(define-foreign-predicate (limits-test-note-call) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (>= (get-internal-real-time) cl-prolog-kit::*inference-deadline*)
    (incf *limits-test-late-calls*))
  (funcall emit environment))

(deftest cleanup-allowance-has-a-deadline ()
  ;; The query trips its inference budget; the cleanup's few inferences are
  ;; each expensive, so only the grace period past the query's deadline
  ;; stops it, and no expensive call starts once D + G has passed.  The
  ;; cleanup runs while the query is past its limit, so its own stop is
  ;; dropped and the host receives the inference stop that started the
  ;; unwind.  Neither depends on how many calls the cleanup reached.  A
  ;; root without a deadline bounds its cleanups by the allowance alone.
  (let* ((*limits-test-late-calls* 0)
         (deadline (prolog-deadline-after 1/10))
         (condition (limits-test-condition (prolog-inference-limit-exceeded)
                      (query-prolog (make-rulebase)
                                    '(setup_call_cleanup
                                      true (and (repeat) (fail))
                                      (and (cl-prolog-kit::between 1 1000000 ?i)
                                           (cl-prolog-kit::numlist 1 100000 ?l)
                                           (limits-test-note-call)
                                           (fail)))
                                    :max-inferences 1000
                                    :deadline deadline))))
    (is condition)
    (when condition
      (is-equal 1000 (prolog-inference-limit-exceeded-limit condition)))
    (is (<= *limits-test-late-calls* 1))))

(define-foreign-predicate (limits-test-nested-loop budget)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase '(and (repeat) (fail)) :max-inferences budget)
  (funcall emit environment))

(deftest nested-query-with-own-budget-keeps-the-enclosing-deadline ()
  (let ((*prolog-deadline* nil))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (query-prolog (make-rulebase) '(limits-test-nested-loop 100000000)
                        :deadline (prolog-deadline-after 1/5))))))

(deftest bound-specials-stop-a-looping-initialization-goal ()
  ;; consult-prolog takes no limit options; its initialization goals run
  ;; under the specials.
  (let ((*max-prolog-inferences* 1000)
        (*prolog-deadline* (prolog-deadline-after 1)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (consult-prolog ":- initialization((repeat, fail))."
                          (make-rulebase))))))

;;; Candidate-generating builtins charge one inference per candidate they
;;; examine, so a search whose candidates all fail stops at the budget even
;;; though it never reaches a solution.  Each query below is one builtin
;;; call; without the per-candidate charge it would either run forever or
;;; finish its fruitless scan on a budget of two inferences.

(defparameter *limits-test-long-list* (make-list 300 :initial-element 'a))

(defparameter *limits-test-long-text* (make-string 1001 :initial-element #\a))

(defun limits-test-fruitless-search-stopped-p (rulebase query budget)
  "Return true when QUERY (Prolog text or a term) raises
PROLOG-INFERENCE-LIMIT-EXCEEDED under BUDGET and a short deadline."
  (let ((term (if (stringp query) (read-prolog-term query) query)))
    (handler-case
        (progn (query-prolog rulebase term
                             :max-inferences budget
                             :deadline (prolog-deadline-after 1/2))
               nil)
      (prolog-inference-limit-exceeded () t))))

(defmacro deftest-fruitless-search (name (&optional (rulebase '(make-rulebase)))
                                    &body queries)
  "Define test NAME asserting that each (QUERY BUDGET) stops at its budget."
  `(deftest ,name ()
     (dolist (spec (list ,@(mapcar (lambda (query) `(list ,@query)) queries)))
       (destructuring-bind (query budget) spec
         (is (limits-test-fruitless-search-stopped-p ,rulebase query budget)
             (format nil "~S must stop at its budget" query))))))

(deftest-fruitless-search length-stops-a-fruitless-search ()
  ("length(L, L)." 10))

(deftest-fruitless-search append-stops-a-fruitless-search ()
  ("append(X, [a], X)." 200))

(deftest fruitless-length-and-append-stop-at-a-deadline-alone ()
  (dolist (query '("length(L, L)." "append(X, [a], X)."))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (limits-test-query (make-rulebase) query
                             :deadline (prolog-deadline-after 1/10))))))

(deftest-fruitless-search list-scans-stop-a-fruitless-search ()
  ((list 'cl-prolog-kit::member 'z *limits-test-long-list*) 50)
  ((list 'cl-prolog-kit::memberchk 'z *limits-test-long-list*) 50)
  ((list 'cl-prolog-kit::select 'z *limits-test-long-list* '?rest) 50)
  ((list 'cl-prolog-kit::nth0 '?index *limits-test-long-list* 'z) 50)
  ((list 'cl-prolog-kit::nth1 '?index *limits-test-long-list* 'z) 50)
  ((list 'cl-prolog-kit::last *limits-test-long-list* 'z) 50))

(deftest-fruitless-search permutation-stops-a-fruitless-search ()
  ("permutation([1,2,3,4,5,6,7,8], [9,9,9,9,9,9,9,9])." 200))

(deftest-fruitless-search text-splitting-stops-a-fruitless-search ()
  ;; An odd-length text has no split into two equal halves.
  ((list 'cl-prolog-kit::atom_concat '?x '?x (intern *limits-test-long-text* :cl-prolog-kit.user-atoms))
   200)
  ((list 'cl-prolog-kit::string_concat '?x '?x *limits-test-long-text*) 200)
  ((list 'cl-prolog-kit::text_concat '?x '?x *limits-test-long-text*) 200)
  ((list 'cl-prolog-kit::sub_atom (intern *limits-test-long-text* :cl-prolog-kit.user-atoms)
         '?before '?length '?after 'zz)
   200)
  ((list 'cl-prolog-kit::sub_string *limits-test-long-text* '?before '?length '?after "zz")
   200))

(defun limits-test-many-facts ()
  (consult-prolog
   (with-output-to-string (out)
     (format out ":- dynamic(f/1).~%")
     (dotimes (index 100) (format out "f(~D).~%" index)))
   (make-rulebase)))

(deftest-fruitless-search database-scans-stop-a-fruitless-search
    ((limits-test-many-facts))
  ("retract(f(none))." 20)
  ("retractall(f(none))." 20)
  ("clause(f(none), B)." 20))

(defun limits-test-in-memory-rulebase ()
  "Return a rulebase whose standard streams are in memory: stream_property/2
on a terminal user_input can block reading it."
  (let ((*standard-input* (make-string-input-stream ""))
        (*standard-output* (make-string-output-stream))
        (*error-output* (make-broadcast-stream)))
    (make-rulebase)))

(deftest-fruitless-search reflection-stops-a-fruitless-search
    ((limits-test-in-memory-rulebase))
  ("current_predicate(no_such_predicate/_)." 50)
  ("current_op(P, T, no_such_operator)." 10)
  ("stream_property(S, alias(no_such_alias))." 5))

(deftest current-char-conversion-stops-a-fruitless-search ()
  (let ((rulebase (make-rulebase)))
    (loop for code from (char-code #\a) to (char-code #\z)
          do (limits-test-query rulebase
                                (format nil "char_conversion('~C', '~C')."
                                        (code-char code)
                                        (char-upcase (code-char code)))))
    (is (limits-test-fruitless-search-stopped-p
         rulebase "current_char_conversion('0', X)." 10))))

(deftest-fruitless-search fd-labeling-stops-a-fruitless-search ()
  ;; Bounds propagation does not see that X + Y cannot reach 1000, so
  ;; labeling tries every value of X; unbounded, this takes seconds.
  ('(and (in (?x ?y) (1 |..| 300)) (|#=| (+ ?x ?y) 1000) (labeling () (?x ?y)))
   200)
  ;; X * X = 2 fails for every value, so indomain/1 finds no solution.
  ('(and (in ?x (1 |..| 300)) (|#=| (* ?x ?x) 2) (indomain ?x)) 200))

;;; A limit stop raised by a nested query unwinds through the enclosing
;;; query's cleanups, which must still run on the shared allowance.

(define-foreign-predicate (limits-test-inner-repeat budget)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase '(and (repeat) (fail)) :max-inferences budget)
  (funcall emit environment))

(define-foreign-predicate (limits-test-inner-loop-until-deadline)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  ;; Own budget, so the query does not share the enclosing limits; it
  ;; inherits the enclosing deadline and stops there.
  (query-prolog rulebase '(and (repeat) (fail)) :max-inferences 1000000000)
  (funcall emit environment))

(deftest outer-cleanup-runs-after-a-nested-inference-stop ()
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog rulebase
                        '(setup_call_cleanup true (limits-test-inner-repeat 1)
                          (assertz (ran)))
                        :max-inferences 4
                        :deadline (prolog-deadline-after 1/2))))
    (is-equal '(nil) (query-prolog rulebase '(ran)))))

(deftest outer-cleanup-runs-after-a-nested-deadline-stop ()
  ;; The nested query's budget is charged to the outer one, so the outer
  ;; budget must outlast the deadline for the deadline to be what stops.
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (query-prolog rulebase
                        '(setup_call_cleanup true
                          (limits-test-inner-loop-until-deadline)
                          (assertz (ran)))
                        :max-inferences 100000000
                        :deadline (prolog-deadline-after 1/10))))
    (is-equal '(nil) (query-prolog rulebase '(ran)))))

(defun limits-test-tick-rules ()
  (prolog
    ((tick-loop) (cl-prolog-kit::between 1 1000000 ?_) (assertz (tick)) (fail))
    ((tick-loop))))

(defun limits-test-count (rulebase predicate)
  (length (query-prolog rulebase `(catch (,predicate) ?_ fail))))

(define-foreign-predicate (limits-test-nested-cleanups depth)
    (rulebase environment unused-depth emit)
  (declare (cl:ignore unused-depth))
  ;; DEPTH nested queries, each with its own budget and a looping cleanup;
  ;; the innermost one stops on its limit.
  (if (zerop depth)
      (query-prolog rulebase '(and (repeat) (fail)) :max-inferences 1)
      (query-prolog rulebase
                    `(setup_call_cleanup
                      true (limits-test-nested-cleanups ,(1- depth))
                      (tick-loop))
                    :max-inferences 100))
  (funcall emit environment))

;;; A tick of tick-loop costs four inferences: between/3's solution,
;;; assertz/1's dispatch and solution, and fail/0.
(defconstant +limits-test-tick-cost+ 4)

(defun limits-test-max-ticks (limit)
  "Most ticks a root query with LIMIT can make, cleanups included."
  (floor (+ limit cl-prolog-kit::*inference-limit-cleanup-allowance*)
         +limits-test-tick-cost+))

(deftest cleanup-allowance-does-not-grow-with-nesting-depth ()
  (flet ((ticks (depth)
           (let ((rulebase (limits-test-tick-rules)))
             (is (limits-test-condition (prolog-inference-limit-exceeded)
                   (query-prolog rulebase
                                 `(setup_call_cleanup
                                   true (limits-test-nested-cleanups ,depth)
                                   (tick-loop))
                                 :max-inferences 1000
                                 :deadline (prolog-deadline-after 1))))
             (limits-test-count rulebase 'tick))))
    ;; However many nested queries and cleanups unwind, all their cleanups
    ;; together stay within one allowance past the root's limit.
    (dolist (depth '(0 6 30))
      (let ((ticks (ticks depth)))
        (is (plusp ticks))
        (is (<= ticks (limits-test-max-ticks 1000))
            (format nil "depth ~D: ~D ticks" depth ticks))))))

(define-foreign-predicate (limits-test-spend count budget)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase `(and (cl-prolog-kit::between 1 ,count ?x) (fail))
                :max-inferences budget)
  (funcall emit environment))

(defun limits-test-cleanup-spending (count budget)
  "Stop a query through a nested limit, then run a cleanup that starts a
query with its own BUDGET costing about 2 * COUNT inferences before it
asserts ran/0.  Return the condition the query raised and whether ran/0
was asserted."
  (let* ((rulebase (make-rulebase))
         (condition
           (limits-test-condition (prolog-inference-limit-exceeded)
             (query-prolog rulebase
                           `(setup_call_cleanup
                             true (limits-test-inner-repeat 1)
                             (and (limits-test-spend ,count ,budget)
                                  (assertz (ran))))
                           :max-inferences 4
                           :deadline (prolog-deadline-after 1)))))
    (values condition (plusp (limits-test-count rulebase 'ran)))))

(deftest query-inside-a-cleanup-is-charged-to-the-allowance ()
  ;; The inner query's budget of 1 does not lower the outer limit of 4, so
  ;; the stop that propagates is the outer query's.  496 rounds of
  ;; between/fail fit in the 1000-inference allowance with room for
  ;; assertz/1.
  (multiple-value-bind (condition ran-p)
      (limits-test-cleanup-spending 496 1000000000)
    (is ran-p)
    (is-equal 4 (prolog-inference-limit-exceeded-limit condition)))
  ;; 499 rounds need more than the allowance: the nested query's own
  ;; budget cannot lift the cleanup's cap, and nothing is left for
  ;; assertz/1.  The cleanup runs while the query is past its limit, so its
  ;; stop is dropped and the outer query's stop propagates.
  (multiple-value-bind (condition ran-p)
      (limits-test-cleanup-spending 499 1000000000)
    (is (not ran-p))
    (is-equal 4 (prolog-inference-limit-exceeded-limit condition)))
  ;; A smaller budget of its own still applies, and its stop is dropped
  ;; the same way.
  (multiple-value-bind (condition ran-p)
      (limits-test-cleanup-spending 100 10)
    (is (not ran-p))
    (is-equal 4 (prolog-inference-limit-exceeded-limit condition))))

;;; One root query uses at most its limit L plus one cleanup allowance R,
;;; and stops by its deadline D plus one grace period G, however its queries
;;; nest and whatever catches their stops.  Ticks of assertz(tick) count the
;;; inferences a query ran, since the old pool-based limits kept no total.

(define-foreign-predicate (limits-test-spend-800) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase '(and (cl-prolog-kit::between 1 800 ?x) (assertz (tick)) (fail))
                :max-inferences 100000)
  (funcall emit environment))

(define-foreign-predicate (limits-test-wrap-800) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase '(limits-test-spend-800) :max-inferences 100000)
  (funcall emit environment))

(deftest queries-nested-in-a-cleanup-stay-within-the-allowance ()
  ;; A cleanup that starts a query with its own budget, directly or through
  ;; another nested query, still runs under L + R.
  (dolist (cleanup '((limits-test-spend-800) (limits-test-wrap-800)))
    (let ((rulebase (make-rulebase)))
      (is (limits-test-condition (prolog-inference-limit-exceeded)
            (query-prolog rulebase
                          `(setup_call_cleanup true (limits-test-inner-repeat 1)
                            ,cleanup)
                          :max-inferences 4
                          :deadline (prolog-deadline-after 1))))
      (let ((ticks (limits-test-count rulebase 'tick)))
        (is (<= ticks (limits-test-max-ticks 4))
            (format nil "~S: ~D ticks" cleanup ticks))))))

(define-foreign-predicate (limits-test-reach-inner) (rulebase environment depth emit)
  (declare (cl:ignore environment depth emit))
  (query-prolog rulebase '(repeat) :max-inferences 1))

(defun limits-test-inference-stops (function)
  "Call FUNCTION.  Return the PROLOG-INFERENCE-LIMIT-EXCEEDED it ended with,
or NIL, and how many such conditions were signalled during the call."
  (let ((count 0))
    (values (handler-case
                (handler-bind ((prolog-inference-limit-exceeded
                                 (lambda (condition)
                                   (declare (cl:ignore condition))
                                   (incf count))))
                  (funcall function)
                  nil)
              (prolog-inference-limit-exceeded (condition) condition))
            count)))

(deftest a-stop-replaced-by-a-cleanup-throw-stays-within-the-limit ()
  ;; A cleanup that throws replaces the stop with a catchable ball.  The
  ;; code after catch/3 then stops at its first charge past the cap.  Each
  ;; case counts the stops signalled, so it fails unless the cleanup threw
  ;; and catch/3 resumed: a stop after the catch is a second one.
  (let ((rulebase (make-rulebase)))
    (multiple-value-bind (condition stops)
        (limits-test-inference-stops
         (lambda ()
           (query-prolog rulebase
                         '(and (catch (setup_call_cleanup true (and (repeat) (fail))
                                       (and (assertz (thrown)) (throw ball)))
                                ball true)
                           (cl-prolog-kit::between 1 100 ?n) (assertz (tick)) (fail))
                         :max-inferences 30
                         :deadline (prolog-deadline-after 1))))
      (is condition)
      ;; The loop's stop, then the stop at the recovery's dispatch.
      (is-equal 2 stops))
    (is-equal 1 (limits-test-count rulebase 'thrown))
    (is-equal 0 (limits-test-count rulebase 'tick)))
  ;; The replaced stop was a nested query's own: the recovery and the code
  ;; after it run on to the root's limit and no further.
  (let ((rulebase (make-rulebase)))
    (multiple-value-bind (condition stops)
        (limits-test-inference-stops
         (lambda ()
           (query-prolog rulebase
                         '(and (catch (setup_call_cleanup true limits-test-reach-inner
                                       (throw ball))
                                ball (assertz (caught)))
                           (cl-prolog-kit::between 1 100 ?n) (assertz (tick)) (fail))
                         :max-inferences 30
                         :deadline (prolog-deadline-after 1))))
      (is condition)
      (when condition
        (is-equal 30 (prolog-inference-limit-exceeded-limit condition)))
      (is-equal 2 stops))
    (is-equal 1 (limits-test-count rulebase 'caught))
    (is (<= (limits-test-count rulebase 'tick) (floor 30 +limits-test-tick-cost+))))
  ;; A later cleanup may use the allowance, but not more than L + R.
  (let ((rulebase (make-rulebase)))
    (multiple-value-bind (condition stops)
        (limits-test-inference-stops
         (lambda ()
           (query-prolog rulebase
                         '(setup_call_cleanup
                           true
                           (catch (setup_call_cleanup true (and (repeat) (fail))
                                   (and (assertz (thrown)) (throw ball)))
                            ball true)
                           (and (cl-prolog-kit::between 1 2000 ?n) (assertz (tick)) (fail)))
                         :max-inferences 30
                         :deadline (prolog-deadline-after 1))))
      (is condition)
      ;; The loop's stop and the recovery's.  The later cleanup runs while
      ;; the query is past its limit, so the stop it reaches at L + R is
      ;; dropped inside it and reaches no host handler.
      (is-equal 2 stops))
    (is-equal 1 (limits-test-count rulebase 'thrown))
    (is (plusp (limits-test-count rulebase 'tick)))
    (is (<= (limits-test-count rulebase 'tick) (limits-test-max-ticks 30)))))

(define-foreign-predicate (limits-test-own-budget-ticks count) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase `(and (cl-prolog-kit::between 1 ,count ?x) (assertz (tick)) (fail))
                :max-inferences 100000000)
  (funcall emit environment))

(define-foreign-predicate (limits-test-no-budget-ticks count) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase `(and (cl-prolog-kit::between 1 ,count ?x) (assertz (tick)) (fail))
                :max-inferences nil :deadline (prolog-deadline-after 1))
  (funcall emit environment))

(deftest a-nested-query-cannot-escape-the-root-limit ()
  ;; Neither a larger budget of its own nor an explicit NIL budget lets a
  ;; nested query run past the root's limit.
  (dolist (goal '((limits-test-own-budget-ticks 5000)
                  (limits-test-no-budget-ticks 5000)))
    (let* ((rulebase (make-rulebase))
           (condition (limits-test-condition (prolog-inference-limit-exceeded)
                        (query-prolog rulebase goal
                                      :max-inferences 10
                                      :deadline (prolog-deadline-after 1)))))
      (is condition (format nil "~S must stop" goal))
      (when condition
        (is-equal 10 (prolog-inference-limit-exceeded-limit condition)))
      (is (<= (limits-test-count rulebase 'tick) (floor 10 +limits-test-tick-cost+))
          (format nil "~S: ~D ticks" goal (limits-test-count rulebase 'tick))))))

(define-foreign-predicate (limits-test-swallow-shared) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  ;; Shares the root's bounds, so the stop it swallows is the root's.
  (handler-case (query-prolog rulebase '(and (repeat) (fail)))
    (prolog-inference-limit-exceeded () nil))
  (funcall emit environment))

(defvar *limits-test-swallowed-stops* nil)

(define-foreign-predicate (limits-test-swallow-many count) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (handler-case (query-prolog rulebase '(and (repeat) (fail)))
    (prolog-inference-limit-exceeded () nil))
  (let ((stops 0))
    (dotimes (index count)
      (handler-case (query-prolog rulebase '(assertz (tick)))
        (prolog-inference-limit-exceeded () (incf stops))))
    (setf *limits-test-swallowed-stops* stops)
    (funcall emit environment)))

(deftest host-code-swallowing-a-stop-cannot-continue-past-it ()
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog rulebase
                        '(and (limits-test-swallow-shared)
                          (cl-prolog-kit::between 1 100 ?n) (assertz (tick)) (fail))
                        :max-inferences 30
                        :deadline (prolog-deadline-after 1))))
    (is-equal 0 (limits-test-count rulebase 'tick)))
  ;; Every swallowed retry stops at its first charge, and so does the code
  ;; after the foreign predicate.
  (let ((rulebase (make-rulebase))
        (*limits-test-swallowed-stops* nil))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog rulebase '(and (limits-test-swallow-many 1000)
                                   (assertz (tick)))
                        :max-inferences 30
                        :deadline (prolog-deadline-after 1))))
    (is-equal 1000 *limits-test-swallowed-stops*)
    (is-equal 0 (limits-test-count rulebase 'tick))))

(defvar *limits-test-deadlines* nil
  "Effective deadline and deadline ceiling seen by each
LIMITS-TEST-RECORD-DEADLINE call, latest first.")

(define-foreign-predicate (limits-test-record-deadline) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (push (list cl-prolog-kit::*inference-deadline*
              cl-prolog-kit::*inference-deadline-ceiling*)
        *limits-test-deadlines*)
  (funcall emit environment))

(define-foreign-predicate (limits-test-loop-with-later-deadline) (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase '(and (limits-test-record-deadline) (repeat) (fail))
                :max-inferences nil :deadline (prolog-deadline-after 3600))
  (funcall emit environment))

(defun limits-test-unwinding-cleanups (depth)
  "DEPTH cleanups around a loop, each recording its deadline and looping."
  (if (zerop depth)
      '(and (repeat) (fail))
      `(setup_call_cleanup true ,(limits-test-unwinding-cleanups (1- depth))
                           (and (limits-test-record-deadline) (repeat) (fail)))))

(defun limits-test-cleanups-inside-cleanups (depth)
  "DEPTH cleanups, each run as the cleanup of the one outside it; the
innermost records its deadline and loops."
  (if (zerop depth)
      '(and (limits-test-record-deadline) (repeat) (fail))
      `(setup_call_cleanup true true
                           ,(limits-test-cleanups-inside-cleanups (1- depth)))))

(defun limits-test-cleanup-nestings ()
  "Goals whose cleanups nest in each of the ways a deadline could stack."
  (list (limits-test-unwinding-cleanups 1)
        (limits-test-unwinding-cleanups 20)
        (limits-test-cleanups-inside-cleanups 20)
        '(setup_call_cleanup true (and (repeat) (fail))
          (limits-test-loop-with-later-deadline))))

(deftest deadline-grace-does-not-grow-with-cleanup-nesting ()
  ;; However the cleanups nest, and even inside a nested query with a later
  ;; deadline of its own, they run under D + G.  The deadline is far off and
  ;; the inference budget stops every goal, so exactly one cleanup records
  ;; its bounds whatever the scheduling: later cleanups stop at their first
  ;; charge, past the cap.
  (dolist (goal (limits-test-cleanup-nestings))
    (let* ((*limits-test-deadlines* nil)
           (deadline (prolog-deadline-after 60))
           (expected (+ deadline cl-prolog-kit::*inference-limit-cleanup-grace*)))
      (is (limits-test-condition (prolog-inference-limit-exceeded)
            (query-prolog (make-rulebase) goal
                          :max-inferences 1000
                          :deadline deadline)))
      (is-equal (list (list expected expected)) *limits-test-deadlines*
                (format nil "~S" goal)))))

(deftest nested-cleanups-stop-soon-after-the-grace-period ()
  ;; Real time: with D = 50 ms and G = 50 ms every goal stops on its
  ;; deadline well before ten times D + G.  The budget outlasts the
  ;; deadline, so an inference stop here means the deadline was missed.
  (dolist (goal (limits-test-cleanup-nestings))
    (let* ((cl-prolog-kit::*inference-limit-cleanup-grace*
             (round internal-time-units-per-second 20))
           (start (get-internal-real-time))
           (stopped-p (limits-test-condition (prolog-deadline-exceeded)
                        (query-prolog (make-rulebase) goal
                                      :max-inferences 100000000
                                      :deadline (prolog-deadline-after 1/20))))
           (elapsed (- (get-internal-real-time) start)))
      (is stopped-p (format nil "~S" goal))
      (is (< elapsed internal-time-units-per-second)
          (format nil "~S: ~D ms" goal
                  (round (* 1000 elapsed) internal-time-units-per-second))))))

(define-foreign-predicate (limits-test-recursive-cleanup-with-own-limit)
    (rulebase environment depth emit)
  (declare (cl:ignore depth))
  (query-prolog rulebase (read-prolog-term "c.") :max-inferences 10)
  (funcall emit environment))

(deftest recursive-cleanup-under-a-root-without-a-limit-stops ()
  ;; c :- setup_call_cleanup(true, lp, c) raises the cap at every cleanup.
  ;; The root has no inference limit, so only the nested query's own
  ;; ceiling, 10 + R, stops the recursion.  Without it the recursion ends
  ;; in control stack exhaustion or at the deadline, neither of which is an
  ;; inference stop.  Every cleanup runs while the nested query is past its
  ;; limit, so the stop of the one that hit the ceiling is dropped and the
  ;; nested query's own stop, at 10, propagates.
  (let ((condition (limits-test-condition (prolog-inference-limit-exceeded)
                     (query-prolog (limits-test-cleanup-rules)
                                   '(limits-test-recursive-cleanup-with-own-limit)
                                   :max-inferences nil
                                   :deadline (prolog-deadline-after 1/2)))))
    (is condition)
    (when condition
      (is-equal 10 (prolog-inference-limit-exceeded-limit condition)))))

(define-foreign-predicate (limits-test-sleep milliseconds) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (sleep (/ milliseconds 1000))
  (funcall emit environment))

(deftest leaving-a-cleanup-restores-a-check-that-still-stops ()
  ;; A cleanup that ends normally restores the check point computed before
  ;; it ran, at a smaller inference count.  The fast path must still notice
  ;; that the cleanup went past the restored cap or deadline.
  ;;
  ;; Inferences: the findall/3 in the cleanup costs more than the cap of
  ;; 100, which the cleanup may exceed by its allowance; the first charge
  ;; after it is past the cap.
  (let ((rulebase (make-rulebase)))
    (let ((condition
            (limits-test-condition (prolog-inference-limit-exceeded)
              (query-prolog rulebase
                            '(and (setup_call_cleanup
                                   true true
                                   (and (findall ?y (cl-prolog-kit::between 1 150 ?y) ?l)
                                        (assertz (cleaned))))
                              (cl-prolog-kit::between 1 100 ?n) (assertz (tick)) (fail))
                            :max-inferences 100
                            :deadline (prolog-deadline-after 1)))))
      (is condition)
      (when condition
        (is-equal 100 (prolog-inference-limit-exceeded-limit condition))))
    (is-equal 1 (limits-test-count rulebase 'cleaned))
    (is-equal 0 (limits-test-count rulebase 'tick)))
  ;; Deadline: the cleanup sleeps past D within its grace.  The code after
  ;; it stops within one poll interval.  Under load the query can pass D
  ;; before the cleanup starts and then stops with no ticks at all, so only
  ;; the tick bound is asserted, not that the cleanup ran.
  (let ((rulebase (make-rulebase)))
    (is (limits-test-condition (prolog-deadline-exceeded)
          (query-prolog rulebase
                        '(and (setup_call_cleanup
                               true true
                               (and (limits-test-sleep 100) (assertz (cleaned))))
                          (cl-prolog-kit::between 1 100000 ?n) (assertz (tick)) (fail))
                        :max-inferences 1000000
                        :deadline (prolog-deadline-after 1/20))))
    (is (<= (limits-test-count rulebase 'tick)
            (floor cl-prolog-kit::+inference-deadline-max-poll-interval+
                   +limits-test-tick-cost+)))))

(defmacro with-limits-test-poll-state ((&key deadline) &body body)
  "Run BODY with the deadline poll state of a root query bound to known
values: 100 inferences used, the next clock read and check at 300, no cap."
  `(let ((cl-prolog-kit::*inference-deadline* ,deadline)
         (cl-prolog-kit::*inference-cap* nil)
         (cl-prolog-kit::*inference-used* 100)
         (cl-prolog-kit::*inference-poll-at* 300)
         (cl-prolog-kit::*inference-check-at* 300)
         (cl-prolog-kit::*inference-poll-interval* 256)
         (cl-prolog-kit::*inference-last-poll* nil))
     ,@body))

(defun limits-test-reserve (count)
  "Pass COUNT elements through the entry point every builtin calls before a
bounded allocation."
  (cl-prolog-kit::%check-builtin-output-length
   count "LIST_LENGTH" '() (cl-prolog-kit::%iso-atom "NUMLIST") "test allocation"))

(deftest large-builtin-output-makes-the-next-inference-read-the-clock ()
  ;; Over 512 elements under a deadline, the reservation moves the clock
  ;; read and the check to the current count, and the next charge reads
  ;; the clock.  The deadline is far off, so nothing here depends on it.
  (let ((far (prolog-deadline-after 60)))
    (with-limits-test-poll-state (:deadline far)
      (limits-test-reserve 513)
      (is-equal 100 cl-prolog-kit::*inference-poll-at*)
      (is-equal 100 cl-prolog-kit::*inference-check-at*)
      (cl-prolog-kit::%count-inference)
      (is cl-prolog-kit::*inference-last-poll*)
      (is (>= cl-prolog-kit::*inference-poll-at* 101)))
    ;; 512 elements, or no deadline: the next read stays at 300.
    (dolist (case (list (list 512 far) (list 513 nil)))
      (destructuring-bind (count deadline) case
        (with-limits-test-poll-state (:deadline deadline)
          (limits-test-reserve count)
          (is-equal 300 cl-prolog-kit::*inference-poll-at* (format nil "~S" case))
          (is-equal 300 cl-prolog-kit::*inference-check-at* (format nil "~S" case))
          (cl-prolog-kit::%count-inference)
          (is-equal nil cl-prolog-kit::*inference-last-poll* (format nil "~S" case)))))))

(deftest slow-builtins-after-cheap-inferences-stop-soon-after-the-deadline ()
  ;; Real time: cheap inferences grow the poll interval to its maximum, and
  ;; each numlist/3 call that follows takes milliseconds.  Each call makes
  ;; the next inference read the clock, so the query stops about one call
  ;; after D = 50 ms; the limit is ten times D plus one call of up to 50 ms.
  ;; The clock-independent part is
  ;; LARGE-BUILTIN-OUTPUT-MAKES-THE-NEXT-INFERENCE-READ-THE-CLOCK.
  (let* ((start (get-internal-real-time))
         (stopped-p (limits-test-condition (prolog-deadline-exceeded)
                      (query-prolog (make-rulebase)
                                    '(or (and (cl-prolog-kit::between 1 2000 ?i) (fail))
                                         (and (cl-prolog-kit::between 1 30 ?j)
                                              (cl-prolog-kit::numlist 1 200000 ?l)
                                              (fail)))
                                    :max-inferences 1000000
                                    :deadline (prolog-deadline-after 1/20))))
         (elapsed (- (get-internal-real-time) start)))
    (is stopped-p)
    (is (< elapsed internal-time-units-per-second)
        (format nil "~D ms" (round (* 1000 elapsed) internal-time-units-per-second)))))
