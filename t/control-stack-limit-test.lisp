;;;; The control stack limit of top-level queries.

(in-package #:cl-prolog-kit.tests)

;;; Every test binds the limit to a quarter of the stack.  The goals then
;;; trip it at a depth of a few hundred levels on any platform, well under
;;; the sizes used here, and three quarters of the stack stay free, so a
;;; test cannot reach SBCL's guard page even if the limit were missed by a
;;; wide margin: under --script a guard page hit ends the process.
(defconstant +stack-test-fraction+ 1/4)

(defun stack-test-rules ()
  (consult-prolog "cnt(N, N) :- !.
cnt(I, N) :- I1 is I + 1, cnt(I1, N).
deep(0, a) :- !.
deep(N, f(T)) :- N1 is N - 1, deep(N1, T).
wrap(_, A, f(A))."
                  (make-rulebase)))

(defun stack-test-query (text &key (rulebase (stack-test-rules)))
  "Run TEXT under the test's stack fraction, a budget and a deadline.
Return the PROLOG-STACK-LIMIT-EXCEEDED it raised, or :FINISHED."
  (let ((cl-prolog-kit::*control-stack-limit-fraction* +stack-test-fraction+)
        (*standard-output* (make-broadcast-stream)))
    (handler-case
        (progn (query-prolog rulebase (read-prolog-term text)
                             :max-inferences 10000000
                             :deadline (prolog-deadline-after 10))
               :finished)
      (prolog-stack-limit-exceeded (condition) condition))))

(defun stack-test-stopped-p (text)
  (typep (stack-test-query text) 'prolog-stack-limit-exceeded))

(deftest deep-proof-search-raises-the-stack-limit ()
  ;; A tail-recursive count and a term built one level per call both
  ;; deepen the control stack at every level of proof search.
  (dolist (text '("cnt(0, 3000)." "deep(3000, T)."
                  "deep(3000, T), findall(T, true, [X]), write(X)."))
    (is (stack-test-stopped-p text) text)
    ;; The guard page was not touched, so the same goal stops the same way
    ;; again in this process.
    (is (stack-test-stopped-p text) text))
  ;; Shallow recursion is unaffected.
  (is-equal :finished (stack-test-query "cnt(0, 100).")))

(defun stack-test-deep-term (depth leaf)
  (let ((term leaf))
    (dotimes (index depth term)
      (setf term (list 'f term)))))

(defun stack-test-goal (goal &key (fraction +stack-test-fraction+))
  "Like STACK-TEST-QUERY for a goal given as a term, under FRACTION."
  (let ((cl-prolog-kit::*control-stack-limit-fraction* fraction))
    (handler-case
        (progn (query-prolog (stack-test-rules) goal
                             :max-inferences 10000000
                             :deadline (prolog-deadline-after 10))
               :finished)
      (prolog-stack-limit-exceeded (condition) condition))))

(deftest deep-terms-inside-one-builtin-raise-the-stack-limit ()
  ;; Unifying two terms 5000 levels deep and copying one walk them inside a
  ;; single builtin call, with no goal dispatch to check the stack, so only
  ;; the check in the term walk stops them.  foldl/4 grows a deep
  ;; accumulator and checks it for occurrences at every step; that costs
  ;; time quadratic in the depth reached, so it runs under a sixteenth of
  ;; the stack, where the dispatch check may be the one that stops it.  Each
  ;; runs twice: the guard page was not touched the first time.  Comparing
  ;; deep terms is not used: term comparison does not dereference through
  ;; the check and takes time quadratic in the depth.
  (dotimes (round 2)
    (is (typep (stack-test-goal (list '= (stack-test-deep-term 5000 'a)
                                      (stack-test-deep-term 5000 'a)))
               'prolog-stack-limit-exceeded))
    (is (typep (stack-test-goal (list 'cl-prolog-kit::copy_term
                                      (stack-test-deep-term 5000 '?leaf) '?copy))
               'prolog-stack-limit-exceeded))
    (is (typep (stack-test-goal (read-prolog-term
                                 "numlist(1, 5000, L), foldl(wrap, L, a, _).")
                                :fraction 1/16)
               'prolog-stack-limit-exceeded)))
  ;; A long list is walked down its tail by loops, so sorting one finishes.
  (is-equal :finished (stack-test-query "numlist(1, 20000, L), msort(L, _).")))

(deftest stack-limit-is-a-catchable-resource-error ()
  (let ((condition (stack-test-query "cnt(0, 3000).")))
    (is (typep condition 'prolog-resource-error))
    (is-equal (list (cl-prolog-kit::%iso-atom "RESOURCE_ERROR")
                    (cl-prolog-kit::%iso-atom "CONTROL_STACK"))
              (second (prolog-exception-term condition))))
  ;; catch/3 intercepts it, and the recovery runs on the unwound stack.
  (let ((solutions
          (let ((cl-prolog-kit::*control-stack-limit-fraction* +stack-test-fraction+))
            (query-prolog (stack-test-rules)
                          (read-prolog-term
                           "catch(cnt(0, 3000), error(resource_error(R), _), true).")
                          :max-inferences 10000000
                          :deadline (prolog-deadline-after 10)))))
    (is-equal 1 (length solutions))
    (is-equal (cl-prolog-kit::%iso-atom "CONTROL_STACK")
              (cdr (first (first solutions))))))

(deftest stack-limit-uses-the-querying-thread-s-own-stack ()
  ;; A query in another thread is a root of its own and measures its own
  ;; thread's stack, which lies elsewhere in memory: a limit taken from
  ;; another thread would trip at once or never.
  (destructuring-bind (deep shallow)
      (sb-thread:join-thread
       (sb-thread:make-thread
        (lambda () (list (stack-test-query "cnt(0, 3000).")
                         (stack-test-query "cnt(0, 100).")))
        :name "stack-limit-test"))
    (is (typep deep 'prolog-stack-limit-exceeded))
    (is-equal :finished shallow)))
