;;;; Foreign-predicate dispatch and define-builtin registration/macroexpansion
;;;; tests.

(in-package #:cl-prolog-kit.tests)

(cl-prolog-kit::define-builtin (test-twice input output)
    (rulebase environment depth emit)
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (cl-prolog-kit::%unify-emit output (* 2 value) environment emit))))

(cl-prolog-kit::define-builtin ((test-collect test-collect-alias) output &rest arguments)
    (rulebase environment depth emit)
  (cl-prolog-kit::%unify-emit output (copy-list arguments) environment emit))

(deftest foreign-predicate-cps-solutions ()
  (let ((rb (make-family-rulebase))
        (*recorded-colors* '()))
    (assert-query rb (accepted :ok) :ordered (nil))
    (assert-query rb (accepted :ng) :ordered ())
    (assert-query rb (colored ?x) :ordered (((?x . tom))))
    (is-equal '(tom) *recorded-colors*)
    (assert-query rb (foreign-choice ?value)
                  :ordered (((?value . left)) ((?value . right))))))

(deftest foreign-predicate-dispatches-by-exact-indicator ()
  (let ((rb (prolog
              ((foreign-choice fallback zero))
              ((foreign-choice fallback)))))
    ;; No FOREIGN-CHOICE/0 solver or clause exists, so ISO requires an
    ;; existence error rather than a silent failure.
    (assert-query rb (foreign-choice) :signals)
    (assert-query rb (foreign-choice fallback zero) :ordered (nil))
    (assert-query rb (foreign-choice fallback) :ordered ())))

(deftest registered-foreign-predicate-is-authoritative ()
  (let ((rb (prolog ((foreign-choice clause-solution)))))
    (assert-query rb (foreign-choice ?value)
                  :ordered (((?value . left)) ((?value . right))))))

(deftest define-foreign-predicate-registers-name-and-arity ()
  (with-macroexpansion (expansion
                        '(define-foreign-predicate (foreign-example value)
                             (rulebase environment depth emit)
                           (funcall emit environment)))
    (is (%tree-contains-p expansion 'defmethod))
    (is (%tree-contains-p expansion 'cl-prolog-kit::%foreign-goal-solver))
    (is (%tree-contains-p expansion 'foreign-example))
    (is (%tree-contains-p expansion 1))))

(deftest define-foreign-predicate-rejects-a-variadic-argument-list ()
  (signals-error
    (macroexpand-1
     '(cl-prolog-kit::define-foreign-predicate
       (foreign-variadic-example value &rest more)
       (rulebase environment depth emit)
       (declare (cl:ignore rulebase environment depth more))
       (funcall emit environment)))))

(deftest iso-builtin-macro-treats-any-non-raw-compound-argument-as-resolvable ()
  (let ((expansion (macroexpand-1
                     '(cl-prolog-kit::define-iso-builtin
                       (test_iso_builtin_arg_shape (value :other)) "TEST"
                       nil))))
    (is (search "RESOLVED-VALUE" (format nil "~S" expansion)))))

(deftest-table runtime-when-guards-take-functions ()
  (:equal '(((?x . bob)))
          (query-prolog (make-family-rulebase)
                        (list 'and
                              '(parent tom ?x)
                              (list :when (lambda (x) (eq x 'bob)) '?x))))
  (:signals (query-prolog (make-family-rulebase) '(:when (equal 'bob 'bob)))))

(deftest query-limits-and-streaming ()
  (let ((rb (make-family-rulebase)))
    (assert-query rb (ancestor tom ?who) :ordered (((?who . bob))) :limit 1)
    (is-equal 2 (length (query-prolog rb (quote (ancestor tom ?who)) :limit 2)))
    (dolist (limit (quote (0 -1 1.5 "1")))
      (handler-case
          (progn
            (query-prolog rb (quote (ancestor tom ?who)) :limit limit)
            (error "Expected a TYPE-ERROR"))
        (type-error (condition)
          (is-equal limit (type-error-datum condition))
          (is-equal (quote (or null (integer 1 *)))
                    (type-error-expected-type condition))))
      (signals-prolog-condition
       type-error
       (map-prolog-solutions (lambda (solution) (declare (ignore solution)))
                             rb (quote (ancestor tom ?who)) :limit limit)))
    (signals-prolog-condition
     type-error
     (query-prolog-first rb (quote (ancestor tom ?who)) :limit 0))
    (signals-prolog-condition
     program-error
     (query-prolog rb (quote (ancestor tom ?who)) :limti 1))
    (signals-prolog-condition
     program-error
     (query-prolog rb (quote (ancestor tom ?who)) :limit))
    (is-equal (quote (((?who . bob)) t))
              (multiple-value-list
               (query-prolog-first rb (quote (ancestor tom ?who)) :limit 2)))
    (let ((seen (quote ())))
      (map-prolog-solutions (lambda (solution) (push solution seen))
                            rb (quote (ancestor tom ?who)) :limit 2)
      (is-equal (quote (((?who . bob)) ((?who . alice)))) (reverse seen)))
    (let ((raw (query-prolog rb (quote (ancestor ?x bob)) :project nil)))
      (is-equal 1 (length raw))
      (is (assoc (quote ?x) (first raw))))
    (handler-case
        (progn
          (query-prolog rb (quote (ancestor tom ?who)) :max-depth -1)
          (error "Expected an INVALID-MAX-DEPTH-ERROR"))
      (invalid-max-depth-error (condition)
        (is-equal -1 (invalid-max-depth-error-value condition))))
    (is (signals-error
         (prolog-succeeds-p rb (quote (ancestor tom ?who)) :max-depth 1.5)))))

(deftest query-projection-caches-public-variables-across-solutions ()
  (is-equal '(nil)
            (query-prolog (make-family-rulebase) '(ancestor tom eve)))
  (let ((rulebase (make-family-rulebase))
        (query
          '(and (choice ?value)
                (= ?value ?value)
                (forall (choice ?local)
                        (or (= ?local left) (= ?local right))))))
    (let ((seen '()))
      (map-prolog-solutions (lambda (solution) (push solution seen))
                            rulebase query)
      (is-equal '(((?value . left)) ((?value . right)))
                (nreverse seen)))
    (is-equal '(((?value . left)))
              (query-prolog rulebase query :limit 1))))

(deftest-table default-query-projection-paths ()
  (:equal '(((?x . tom)))
          (let ((seen '()))
            (map-prolog-solutions (lambda (solution) (push solution seen))
                                  (make-family-rulebase)
                                  '(ancestor ?x bob))
            (nreverse seen)))
  (:equal '(((?x . tom)))
          (query-prolog (make-family-rulebase) '(ancestor ?x bob))))

(deftest rule-variables-are-freshened-per-use ()
  (let ((rb (prolog
              ((same ?x ?x))
              ((both ?a ?b) (same ?a ?a) (same ?b ?b)))))
    (assert-query rb ((same ?p 1) (same ?q 2) (both ?p ?q))
                  :ordered (((?p . 1) (?q . 2))))))

(deftest define-builtin-is-extensible ()
  (is-equal '(((?y . 6)))
            (query-prolog (make-rulebase) '(test-twice 3 ?y))))



(deftest builtin-does-not-shadow-user-predicate-with-different-arity ()
  (let ((rulebase (prolog ((test-twice user-defined)))))
    (is-equal '(())
              (query-prolog rulebase '(test-twice user-defined)))))

(deftest builtin-solver-registration-replaces-conflicting-forms ()
    (let ((cl-prolog-kit::*fixed-builtin-solvers* (make-hash-table :test (function eq)))
          (cl-prolog-kit::*variadic-builtin-solvers* (make-hash-table :test (function eq)))
          (cl-prolog-kit::*builtin-predicate-indicators* (list)))
      (let ((predicate (gensym "PREDICATE"))
            (fixed (lambda (&rest ignored) (declare (ignore ignored))))
            (variadic (lambda (&rest ignored) (declare (ignore ignored))))
            (replacement (lambda (&rest ignored) (declare (ignore ignored)))))
        (cl-prolog-kit::%register-builtin-solver! predicate 1 1 fixed)
        (is (eq fixed (cl-prolog-kit::%goal-solver predicate 1)))
        (cl-prolog-kit::%register-builtin-solver! predicate 1 nil variadic)
        (is (eq variadic (cl-prolog-kit::%goal-solver predicate 1)))
        (cl-prolog-kit::%register-builtin-solver! predicate 2 2 replacement)
        (is (null (cl-prolog-kit::%goal-solver predicate 1)))
        (is (eq replacement (cl-prolog-kit::%goal-solver predicate 2))))))

  (deftest define-builtin-supports-aliases-and-rest-arguments ()
  (is-equal '(((?arguments . (a b c))))
            (query-prolog (make-rulebase)
                          '(test-collect-alias ?arguments a b c))))

(deftest define-builtin-macroexpand-registers-single-name ()
  (with-macroexpansion (expansion
                        '(cl-prolog-kit::define-builtin (twice input output)
                           (rulebase environment depth emit)
                           (cl-prolog-kit::%unify-emit output
                                                   (* 2 (logic-substitute input environment))
                                                   environment
                                                   emit)))
    (is (%tree-contains-p expansion 'cl-prolog-kit::%register-builtin-solver!))
    (is (%tree-contains-p expansion 'eval-when))
    (is (%tree-contains-p expansion 'twice))))

(deftest define-builtin-macroexpand-registers-aliases-and-rest ()
  (with-macroexpansion (expansion
                        (quote (cl-prolog-kit::define-builtin
                                   ((collect collect-alias) output &rest arguments)
                                   (rulebase environment depth emit)
                                 (declare (cl:ignore output)
                                          (cl:ignorable arguments)
                                          (optimize speed))
                                 (cl-prolog-kit::%unify-emit output
                                                         (copy-list arguments)
                                                         environment
                                                         emit))))
    (is (%tree-contains-p expansion (quote cl-prolog-kit::%register-builtin-solver!)))
    (is (%tree-contains-p expansion (quote eval-when)))
    (is (%tree-contains-p expansion (quote collect)))
    (is (%tree-contains-p expansion (quote collect-alias)))
    ;; Body forms, declarations included, are spliced verbatim; the macro
    ;; supplies its own IGNORABLE declaration for the context variables.
    (is (%tree-contains-p expansion
                          (quote (declare (cl:ignore output)
                                          (cl:ignorable arguments)
                                          (optimize speed)))))
    (is (%tree-contains-p expansion
                          (quote (declare (cl:ignorable rulebase environment
                                                        depth emit)))))))

(defvar *observed-builtin-dispatch-argument* nil)

  (cl-prolog-kit::define-builtin (test-observe-builtin argument)
      (rulebase environment depth emit)
    (setf *observed-builtin-dispatch-argument* argument)
    (funcall emit environment))

  (defvar *observed-foreign-dispatch-argument* nil)

  (define-foreign-predicate (test-observe-foreign argument)
      (rulebase environment depth emit)
    (setf *observed-foreign-dispatch-argument* argument)
    (funcall emit environment))

  (deftest builtin-dispatch-preserves-resolved-arguments ()
    (let ((*observed-builtin-dispatch-argument* nil))
      (is (query-prolog
           (make-rulebase)
           (quote ((= ?argument resolved)
                   (test-observe-builtin ?argument)))))
      (is (eq (quote resolved)
              *observed-builtin-dispatch-argument*))))

  (deftest foreign-dispatch-preserves-resolved-arguments ()
    (let ((*observed-foreign-dispatch-argument* nil))
      (is (query-prolog
           (make-rulebase)
           (quote ((= ?argument resolved)
                   (test-observe-foreign ?argument)))))
      (is (eq (quote resolved)
              *observed-foreign-dispatch-argument*))))
