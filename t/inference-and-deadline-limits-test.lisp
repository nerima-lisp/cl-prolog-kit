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

(deftest nested-top-level-query-inherits-or-replaces-the-budget ()
  ;; Without a limit of its own the nested query draws on the enclosing
  ;; budget: the foreign goal's dispatch and solution plus the nested proof.
  (let ((needed (+ 2 +path-a-d-inferences+)))
    (is-equal '(nil) (query-prolog (limits-test-graph)
                                   '(limits-test-nested-path none)
                                   :max-inferences needed))
    (is (limits-test-condition (prolog-inference-limit-exceeded)
          (query-prolog (limits-test-graph) '(limits-test-nested-path none)
                        :max-inferences (1- needed)))))
  ;; With its own limit the nested query neither draws on nor is bounded by
  ;; the enclosing budget.
  (is-equal '(nil)
            (query-prolog (limits-test-graph)
                          (list 'limits-test-nested-path +path-a-d-inferences+)
                          :max-inferences 2))
  (is (limits-test-condition (prolog-inference-limit-exceeded)
        (query-prolog (limits-test-graph)
                      (list 'limits-test-nested-path
                            (1- +path-a-d-inferences+))))))

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
nest(N) :- N1 is N - 1, setup_call_cleanup(true, nest(N1), (assertz(ran), lp)).
heavy :- between(1, 1000000, _), numlist(1, 100000, _), fail.
heavy."
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

(deftest cleanup-allowance-has-a-deadline ()
  ;; The query trips its inference budget; the cleanup's few inferences are
  ;; each expensive, so only the pool's grace deadline stops it in time.
  (is (limits-test-condition (prolog-deadline-exceeded)
        (limits-test-query (limits-test-cleanup-rules)
                           "setup_call_cleanup(true, lp, heavy)."
                      :max-inferences 1000))))

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
