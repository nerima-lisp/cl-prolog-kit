;;;; Query expectation helpers.
;;;;
;;;; DEFTEST-QUERIES and ASSERT-QUERY are cl-prolog-kit.tests's own query
;;;; assertions no longer here: the suite dogfoods the public
;;;; CL-PROLOG-KIT/WEAVE package (src/weave.lisp) directly, inherited via
;;;; cl-prolog-kit.tests's :USE clause.

(in-package #:cl-prolog-kit.tests)

(defun prolog-goal-holds-p (source)
  "True when the goal SOURCE, read as Prolog source text, has a proof.

Lets a case be written the way a user would type it, so an assertion about ISO
conformance does not have to spell the engine's internal term shapes."
  (prolog-succeeds-p (make-rulebase) (read-prolog-term source)))

(defmacro deftest-prolog-goals (name &body sources)
  "Define one cl-weave case per Prolog goal SOURCE, each asserting it succeeds.

Each case is independent and labelled with its own source text, so a failure
names the goal that failed rather than the whole group."
  `(cl-weave:describe-sequential ,(string name)
     ,@(mapcar
        (lambda (source)
          `(cl-weave:it ,source
             (cl-weave:expect-has-assertions)
             (is (prolog-goal-holds-p ,source))))
        sources)))

(defun prolog-goal-outcome (source)
  "Run the goal SOURCE and describe its outcome for a conformance assertion.

Returns :TRUE or :FALSE for a proof or its absence, or the raised ISO error term
rendered as Prolog text -- which is the form the standard states requirements
in, so a case can name the exact term it expects."
  (handler-case
      (if (prolog-succeeds-p (make-rulebase) (read-prolog-term source))
          :true
          :false)
    (prolog-exception (condition)
      (prolog-term-string (prolog-exception-term condition)))
    (error (condition)
      ;; A host condition escaping to here is itself a conformance failure: the
      ;; engine owes Prolog code a catchable error, not a Lisp one.
      (format nil "uncaught host condition ~A" (type-of condition)))))

(defmacro deftest-iso (name &body cases)
  "Define one cl-weave case per ISO conformance CASE.

Each case is (CLAUSE SOURCE EXPECTED), where CLAUSE cites the ISO 13211-1
subclause and EXPECTED is :TRUE, :FALSE, or a substring of the error term the
standard requires -- matched case-insensitively, since the rendered term's
spelling is not what is under test."
  `(cl-weave:describe-sequential ,(string name)
     ,@(mapcar
        (lambda (case)
          (destructuring-bind (clause source expected) case
            `(cl-weave:it ,(format nil "~A: ~A" clause source)
               (cl-weave:expect-has-assertions)
               (let ((outcome (prolog-goal-outcome ,source)))
                 ,(if (keywordp expected)
                      `(is (eq ,expected outcome)
                           ,(format nil "ISO ~A requires ~(~A~) for: ~A"
                                    clause expected source))
                      `(is (and (stringp outcome)
                                (search ,expected outcome :test #'char-equal))
                           ,(format nil "ISO ~A requires ~A for: ~A"
                                    clause expected source)))))))
        cases)))

(defmacro with-single-query-solution ((solution solutions rulebase query &rest options)
                                      &body body)
  "Execute QUERY once, assert that it yields exactly one solution, and bind it.

SOLUTIONS receives the full result list and SOLUTION receives the first solution.
Trailing OPTIONS are passed to QUERY-PROLOG."
  `(let ((,solutions (query-prolog ,rulebase ,query ,@options)))
     (is (= 1 (length ,solutions))
         "query must yield exactly one solution")
     (let ((,solution (first ,solutions)))
       ,@body)))
