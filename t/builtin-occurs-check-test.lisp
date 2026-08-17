;;;; occurs_check flag: =/2 honors TRUE (default), FALSE, and ERROR modes;
;;;; unify_with_occurs_check/2 always checks.  Cyclic bindings are never
;;;; printed here (only success/failure/signal is asserted).

(in-package #:cl-prolog-kit.tests)

(deftest occurs-check-default-true-fails-on-cycle ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (= ?x (cl-prolog-kit.user-atoms::f ?x)) :fails)
    (assert-query rulebase
                  (cl-prolog-kit::unify_with_occurs_check ?x (cl-prolog-kit.user-atoms::f ?x))
                  :fails)
    (assert-query rulebase (= ?x hello) :succeeds)))

(deftest occurs-check-false-allows-cycle ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                            cl-prolog-kit.user-atoms::false)
                  :succeeds)
    ;; X = f(X) now succeeds (creating a cyclic term); assert success only.
    (assert-query rulebase (= ?x (cl-prolog-kit.user-atoms::f ?x)) :succeeds)
    (assert-query rulebase (= ?a 1) :succeeds)
    ;; unify_with_occurs_check ignores the flag and still fails.
    (assert-query rulebase
                  (cl-prolog-kit::unify_with_occurs_check ?x (cl-prolog-kit.user-atoms::f ?x))
                  :fails)))

(deftest occurs-check-error-raises-on-cycle ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                            cl-prolog-kit.user-atoms::error)
                  :succeeds)
    (assert-query rulebase (= ?x (cl-prolog-kit.user-atoms::f ?x)) :signals)
    (assert-query rulebase (= a b) :fails)
    (assert-query rulebase (= ?a 1) :succeeds)))

(deftest occurs-check-default-nested-cycle ()
  (let ((rulebase (make-rulebase)))
    ;; a deeper multi-argument cycle also fails under the default true.
    (assert-query rulebase (= ?x (cl-prolog-kit.user-atoms::s a ?x)) :fails)))

(deftest cyclic-term-writer-terminates ()
  ;; With occurs_check=false a cyclic term can be built; writing it must
  ;; terminate (emit a `...' marker) rather than recurse forever.
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                                       cl-prolog-kit.user-atoms::false)
                  :succeeds)
    (assert-query rulebase (and (= ?x (cl-prolog-kit.user-atoms::f ?x))
                                (cl-prolog-kit::write ?x))
                  :succeeds)
    (is (string= (get-output-stream-string output) "f(...)")))
  ;; multi-argument cycle: siblings before the revisit print in full
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                                       cl-prolog-kit.user-atoms::false)
                  :succeeds)
    (assert-query rulebase (and (= ?x (cl-prolog-kit.user-atoms::s a ?x))
                                (cl-prolog-kit::write ?x))
                  :succeeds)
    (is (string= (get-output-stream-string output) "s(a,...)")))
  ;; cyclic list SPINE terminates too
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                                       cl-prolog-kit.user-atoms::false)
                  :succeeds)
    (assert-query rulebase (and (= ?x (a . ?x)) (cl-prolog-kit::write ?x))
                  :succeeds)
    (is (string= (get-output-stream-string output) "[a|...]")))
  ;; writeq of a cyclic term also terminates
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                                       cl-prolog-kit.user-atoms::false)
                  :succeeds)
    (assert-query rulebase (and (= ?x (cl-prolog-kit.user-atoms::f ?x))
                                (cl-prolog-kit::writeq ?x))
                  :succeeds)
    (is (string= (get-output-stream-string output) "f(...)"))))

(deftest acyclic-shared-subterm-prints-in-full ()
  ;; A shared but acyclic subterm must print fully (path-scoped seen-set), not
  ;; be truncated to `...' — no occurs_check needed.
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (and (= ?s (cl-prolog-kit.user-atoms::f a))
                                (cl-prolog-kit::write (cl-prolog-kit.user-atoms::pair ?s ?s)))
                  :succeeds)
    (is (string= (get-output-stream-string output) "pair(f(a),f(a))"))))

(deftest occurs-check-negation-honors-flag ()
  ;; \=/2 tracks =/2: under false it FAILS on a unifiable cyclic pair; under
  ;; error it raises; under the default true it succeeds (they do not unify).
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit:|\\=| ?x (cl-prolog-kit.user-atoms::f ?x)) :succeeds))
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                            cl-prolog-kit.user-atoms::false)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit:|\\=| ?x (cl-prolog-kit.user-atoms::f ?x)) :fails))
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog-kit::set_prolog_flag cl-prolog-kit.user-atoms::occurs_check
                                            cl-prolog-kit.user-atoms::error)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit:|\\=| ?x (cl-prolog-kit.user-atoms::f ?x)) :signals)))
