;;;; Per-rulebase capability policy: file-opening builtins and directives,
;;;; foreign-predicate allowlists, and propagation through every rulebase
;;;; copy path.

(in-package #:cl-prolog-kit.tests)

(define-foreign-predicate (capability-policy-test-echo input output)
    (rulebase environment depth emit)
  (multiple-value-bind (extended matched) (unify output input environment)
    (when matched
      (funcall emit extended))))

(define-foreign-predicate (capability-policy-test-other value)
    (rulebase environment depth emit)
  (declare (ignore value))
  (funcall emit environment))

(defun capability-policy-test-rulebase (&rest policy-arguments)
  (make-rulebase
   :capability-policy (apply #'make-prolog-capability-policy policy-arguments)))

(defun capability-permission-formal (rulebase goal)
  "Return the formal permission_error term GOAL raises, or :NO-ERROR."
  (handler-case (progn (query-prolog rulebase goal) :no-error)
    (prolog-permission-error (condition)
      (second (prolog-exception-term condition)))))

(defun capability-formal-names (formal)
  (mapcar #'symbol-name (subseq formal 0 3)))

(defun capability-text-succeeds-p (rulebase format-control &rest arguments)
  (prolog-succeeds-p rulebase
                     (apply #'%read-prolog-query rulebase format-control
                            arguments)))

(defmacro with-capability-source-file ((pathname contents) &body body)
  `(uiop:with-temporary-file (:pathname ,pathname :type "pl")
     (with-open-file (stream ,pathname :direction :output
                                       :if-exists :supersede)
       (write-string ,contents stream))
     ,@body))

(deftest capability-policy-constructor-defaults-and-validation ()
  (let ((policy (make-prolog-capability-policy)))
    (is (prolog-capability-policy-p policy))
    (is (null (prolog-capability-policy-allow-file-access policy)))
    (is (null (prolog-capability-policy-allowed-foreign-predicates policy))))
  (is (eq :all (prolog-capability-policy-allowed-foreign-predicates
                (make-prolog-capability-policy
                 :allowed-foreign-predicates :all))))
  (signals-error (make-prolog-capability-policy
                  :allowed-foreign-predicates '(capability-policy-test-other)))
  (signals-error (make-prolog-capability-policy
                  :allowed-foreign-predicates '((capability-policy-test-other -1))))
  (signals-error (make-prolog-capability-policy
                  :allowed-foreign-predicates :some)))

(deftest capability-policy-defaults-to-nil-and-is-setf-able ()
  (let ((rulebase (make-rulebase))
        (policy (make-prolog-capability-policy)))
    (is (null (rulebase-capability-policy rulebase)))
    (is (query-prolog rulebase '(capability-policy-test-other 1)))
    (setf (rulebase-capability-policy rulebase) policy)
    (is (eq policy (rulebase-capability-policy rulebase)))
    (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
              (capability-formal-names
               (capability-permission-formal
                rulebase '(capability-policy-test-other 1))))
    (setf (rulebase-capability-policy rulebase) nil)
    (is (query-prolog rulebase '(capability-policy-test-other 1)))))

(deftest capability-policy-denies-open-with-a-catchable-iso-error ()
  (with-capability-source-file (path "data.")
    (let* ((rulebase (capability-policy-test-rulebase))
           (streams (cl-prolog-kit::prolog-io-context-streams
                     (cl-prolog-kit::rulebase-io-context rulebase)))
           (stream-count (hash-table-count streams)))
      (dolist (query '("open('~A', read, S)."
                       "open('~A', write, S, [])."))
        (handler-case
            (progn (capability-text-succeeds-p rulebase query (namestring path))
                   (is nil "open must be denied"))
          (prolog-permission-error (condition)
            (let ((formal (second (prolog-exception-term condition))))
              (is-equal '("PERMISSION_ERROR" "OPEN" "SOURCE_SINK")
                        (capability-formal-names formal))
              (is-equal (namestring path) (prolog-atom-text (fourth formal)))))))
      ;; The write-mode attempt must not have truncated the file.
      (is-equal "data." (uiop:read-file-string path))
      (is (capability-text-succeeds-p
           rulebase
           "catch(open('~A', read, _), ~
            error(permission_error(open, source_sink, F), _), ~
            atom(F))"
           (namestring path)))
      (is-equal stream-count (hash-table-count streams)
                "a denied open must not register a stream"))))

(deftest capability-policy-allow-file-access-permits-open ()
  (with-capability-source-file (path "data.")
    (let ((rulebase (capability-policy-test-rulebase :allow-file-access t)))
      (unwind-protect
           (is (capability-text-succeeds-p
                rulebase "open('~A', read, S), read(S, T), close(S), T == data"
                (namestring path)))
        (cl-prolog-kit::%close-all-owned-prolog-streams!
         (cl-prolog-kit::rulebase-io-context rulebase))))))

(deftest capability-policy-keeps-user-stream-builtins-available ()
  (let* ((output (make-string-output-stream))
         (rulebase (make-rulebase
                    :io-context (cl-prolog-kit::make-prolog-io-context
                                 :input (make-string-input-stream "term.")
                                 :output output
                                 :error-output (make-string-output-stream))
                    :capability-policy (make-prolog-capability-policy))))
    (is (capability-text-succeeds-p
         rulebase "read(T), T == term, write(hello), nl"))
    (is-equal (format nil "hello~%") (get-output-stream-string output))))

(deftest capability-policy-denies-source-loading-builtins ()
  (with-capability-source-file (path "loaded_fact(1).")
    (let ((rulebase (capability-policy-test-rulebase))
          (file (namestring path)))
      (dolist (query '("consult('~A')."
                       "load_files('~A')."
                       "load_files(['~A'], [if(not_loaded)])."
                       "ensure_loaded('~A')."))
        (signals-prolog-condition prolog-permission-error
          (capability-text-succeeds-p rulebase query file)
          query))
      (is (null (rulebase-visible-clauses rulebase))
          "a denied load must not change the rulebase"))))

(deftest capability-policy-catch-intercepts-a-denied-consult ()
  (with-capability-source-file (path "loaded_fact(1).")
    (is (capability-text-succeeds-p
         (capability-policy-test-rulebase)
         "catch(consult('~A'), error(permission_error(open, source_sink, _), _), true)"
         (namestring path)))))

(deftest capability-policy-allow-file-access-permits-consult ()
  (with-capability-source-file (path "capability_loaded_fact(1).")
    (let ((rulebase (capability-policy-test-rulebase :allow-file-access t)))
      (is (capability-text-succeeds-p
           rulebase "consult('~A'), capability_loaded_fact(1)"
           (namestring path))))))

(deftest capability-policy-denies-file-directives-in-loaded-source ()
  (with-capability-source-file (path "included_fact(1).")
    (dolist (directive '(":- include('~A')."
                         ":- consult('~A')."
                         ":- ensure_loaded('~A')."
                         ":- load_files('~A')."
                         ":- load_files('~A', [if(not_loaded)])."))
      (let ((rulebase (capability-policy-test-rulebase)))
        (signals-prolog-condition prolog-permission-error
          (consult-prolog (format nil directive (namestring path)) rulebase)
          directive)
        (is (null (rulebase-visible-clauses rulebase))
            "the failed load transaction must leave the rulebase unchanged")))))

(deftest capability-policy-allow-file-access-permits-include ()
  (with-capability-source-file (path "included_fact(1).")
    (let ((rulebase (capability-policy-test-rulebase :allow-file-access t)))
      (consult-prolog (format nil ":- include('~A')." (namestring path))
                      rulebase)
      (is (capability-text-succeeds-p rulebase "included_fact(1)")))))

(deftest capability-policy-denies-unlisted-foreign-predicates-on-every-route ()
  (let ((rulebase (capability-policy-test-rulebase
                   :allowed-foreign-predicates
                   '((capability-policy-test-echo 2)))))
    (dolist (goal '((capability-policy-test-other 1)
                    (call capability-policy-test-other 1)
                    (call (capability-policy-test-other 1))
                    (findall ?x (capability-policy-test-other ?x) ?xs)
                    (cl-prolog-kit::|\\+| (capability-policy-test-other 1))
                    (forall (= ?x 1) (capability-policy-test-other ?x))
                    (and (= ?g (capability-policy-test-other 1)) (call ?g))))
      (let ((formal (capability-permission-formal rulebase goal)))
        (is (consp formal) (format nil "~S must be denied" goal))
        (when (consp formal)
          (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
                    (capability-formal-names formal))
          (is-equal '(/ capability-policy-test-other 1) (fourth formal)))))
    ;; The allowlisted predicate runs on the same routes.
    (assert-query rulebase (capability-policy-test-echo 1 ?y) :ordered (((?y . 1))))
    (assert-query rulebase (call capability-policy-test-echo 2 ?y)
                  :ordered (((?y . 2))))
    (assert-query rulebase (findall ?y (capability-policy-test-echo 3 ?y) ?ys)
                  :ordered (((?y . ?y) (?ys . (3)))))
    (assert-query rulebase (cl-prolog-kit::|\\+| (capability-policy-test-echo 1 2))
                  :succeeds)))

(deftest capability-policy-denied-foreign-call-is-catchable ()
  (let* ((rulebase (capability-policy-test-rulebase))
         (solutions (query-prolog
                     rulebase
                     '(catch (capability-policy-test-other 1) ?error true))))
    (is-equal 1 (length solutions))
    (let ((formal (second (cdr (assoc '?error (first solutions))))))
      (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
                (capability-formal-names formal)))))

(deftest capability-policy-denies-foreign-predicates-reached-from-rules ()
  (let ((rulebase (capability-policy-test-rulebase)))
    (rulebase-insert-clause!
     rulebase (make-clause '(wrapper ?x) '((capability-policy-test-other ?x))))
    (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
              (capability-formal-names
               (capability-permission-formal rulebase '(wrapper 1))))))

(deftest capability-policy-all-allows-every-foreign-predicate ()
  (let ((rulebase (capability-policy-test-rulebase
                   :allowed-foreign-predicates :all)))
    (assert-query rulebase (capability-policy-test-other 1) :succeeds)
    (assert-query rulebase (capability-policy-test-echo a ?y) :ordered (((?y . a))))))

(deftest capability-policy-leaves-builtins-unaffected ()
  (let ((rulebase (capability-policy-test-rulebase)))
    (assert-query rulebase (and (= ?x 1) (is ?y (+ ?x 1))) :ordered (((?x . 1) (?y . 2))))
    (assert-query rulebase (findall ?x (member ?x (1 2)) ?xs)
                  :ordered (((?x . ?x) (?xs . (1 2)))))
    (assert-query rulebase (cl-prolog-kit::atom_length abc ?n) :ordered (((?n . 3))))))

(deftest capability-policy-nil-keeps-unrestricted-behaviour ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (capability-policy-test-other 1) :succeeds)
    (assert-query rulebase (call capability-policy-test-echo 1 ?y) :ordered (((?y . 1)))))
  (with-capability-source-file (path "nil_policy_fact(1).")
    (let ((rulebase (make-rulebase)))
      (is (capability-text-succeeds-p
           rulebase "consult('~A'), nil_policy_fact(1)" (namestring path))))))

(deftest capability-policy-survives-every-copy-path ()
  (let* ((policy (make-prolog-capability-policy))
         (rulebase (make-rulebase :capability-policy policy)))
    (is (eq policy (rulebase-capability-policy (copy-rulebase rulebase))))
    (is (eq policy (rulebase-capability-policy
                    (rulebase-extend rulebase (list (make-clause '(extra 1)))))))
    (is (eq policy (rulebase-capability-policy
                    (cl-prolog-kit::%copy-rulebase rulebase))))
    (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
              (capability-formal-names
               (capability-permission-formal
                (rulebase-extend rulebase '()) '(capability-policy-test-other 1))))
    ;; A loading transaction publishes its copy through %REPLACE-RULEBASE!.
    (consult-prolog "loaded_fact(1)." rulebase)
    (is (eq policy (rulebase-capability-policy rulebase)))
    (is (capability-text-succeeds-p rulebase "loaded_fact(1)"))
    (is-equal '("PERMISSION_ERROR" "EXECUTE" "FOREIGN_PREDICATE")
              (capability-formal-names
               (capability-permission-formal
                rulebase '(capability-policy-test-other 1))))
    (let ((target (make-rulebase)))
      (cl-prolog-kit::%replace-rulebase! target rulebase)
      (is (eq policy (rulebase-capability-policy target))))))

(deftest capability-policy-applies-to-directive-goals-in-loaded-source ()
  (let ((rulebase (capability-policy-test-rulebase)))
    (consult-prolog
     ":- dynamic(denied/0).
      :- initialization(catch(open('/nonexistent_capability_probe', read, _),
                              error(permission_error(open, source_sink, _), _),
                              assertz(denied)))."
     rulebase)
    (is (capability-text-succeeds-p rulebase "denied"))))
