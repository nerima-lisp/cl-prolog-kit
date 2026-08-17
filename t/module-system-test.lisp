(in-package #:cl-prolog-kit.tests)

(deftest module-registry-declaration-and-resolution ()
  (let ((registry (cl-prolog-kit::make-module-registry))
        (calls (quote ())))
    (cl-prolog-kit::module-registry-declare!
     registry (quote lists) (quote ((/ member 2) (/ append 3))))
    (cl-prolog-kit::module-registry-declare! registry (quote client) (quote ()))
    (cl-prolog-kit::module-registry-import!
     registry (quote client) (quote lists) (quote ((/ member 2))))
    (is (cl-prolog-kit::module-registry-exported-p
         registry (quote lists) (quote member) 2))
    (is (not (cl-prolog-kit::module-registry-exported-p
              registry (quote lists) (quote member) 3)))
    (is-equal (quote lists)
              (cl-prolog-kit::module-registry-resolve
               registry (quote client) (quote member) 2
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 nil)))
    (is-equal (quote ((client member 2))) calls)
    (setf calls (quote ()))
    (is-equal (quote client)
              (cl-prolog-kit::module-registry-resolve
               registry (quote client) (quote member) 2
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 (eq module (quote client)))))
    (is-equal (quote ((client member 2))) calls)))

(deftest module-registry-qualified-resolution ()
  (let ((registry (cl-prolog-kit::make-module-registry))
        (calls (quote ())))
    (cl-prolog-kit::module-registry-declare! registry (quote hidden) (quote ()))
    (is-equal (quote hidden)
              (cl-prolog-kit::module-registry-resolve-qualified
               registry (quote hidden) (quote private) 1
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 (equal (list module predicate arity)
                        (quote (hidden private 1))))))
    (is-equal (quote ((hidden private 1))) calls)
    (setf calls (quote ()))
    (is (not (cl-prolog-kit::module-registry-resolve-qualified
              registry (quote hidden) (quote missing) 0
              (lambda (module predicate arity)
                (push (list module predicate arity) calls)
                nil))))
    (is-equal (quote ((hidden missing 0))) calls)))

(deftest module-registry-declare-rejects-malformed-declarations ()
  (let ((registry (cl-prolog-kit::make-module-registry)))
    (signals-error
      (cl-prolog-kit::module-registry-declare! registry "not-an-atom" '()))
    (cl-prolog-kit::module-registry-declare! registry 'once '())
    (signals-error
      (cl-prolog-kit::module-registry-declare! registry 'once '()))
    (signals-error
      (cl-prolog-kit::module-registry-declare!
       registry 'duplicated '((/ same 1) (/ same 1))))
    (signals-error
      (cl-prolog-kit::module-registry-declare!
       registry 'malformed '((/ same))))
    (signals-error
      (cl-prolog-kit::module-registry-declare!
       registry 'too-long '((/ same 1 extra))))
    (signals-error
      (cl-prolog-kit::module-registry-declare!
       registry 'wrong-functor '((not-a-slash same 1))))
    (signals-error
      (cl-prolog-kit::%find-prolog-module registry "not-an-atom" "test"))))

(deftest module-registry-rejects-invalid-imports ()
    (let ((registry (cl-prolog-kit::make-module-registry)))
      (cl-prolog-kit::module-registry-declare! registry 'left '((/ same 1)))
      (cl-prolog-kit::module-registry-declare! registry 'right '((/ same 1)))
      (cl-prolog-kit::module-registry-declare! registry 'client '())
      (cl-prolog-kit::module-registry-import! registry 'client 'left)
      (signals-error
        (cl-prolog-kit::module-registry-import! registry 'client 'right))
      (signals-error
        (cl-prolog-kit::module-registry-import! registry 'client 'left '((/ hidden 1))))))

  (deftest module-registry-allows-reimport-from-same-origin ()
    (let ((registry (cl-prolog-kit::make-module-registry)))
      (cl-prolog-kit::module-registry-declare! registry 'library '((/ public 1)))
      (cl-prolog-kit::module-registry-declare! registry 'client '())
      (cl-prolog-kit::module-registry-import! registry 'client 'library)
      (cl-prolog-kit::module-registry-import! registry 'client 'library)
      (is-equal 'library
                (cl-prolog-kit::module-registry-resolve
                 registry 'client 'public 1
                 (lambda (module predicate arity)
                   (declare (ignore module predicate arity))
                   nil)))))

(deftest module-registry-rejects-import-redefinition-and-undefined-export ()
  (let ((registry (cl-prolog-kit::make-module-registry)))
    (cl-prolog-kit::module-registry-declare! registry 'library '((/ public 1)))
    (cl-prolog-kit::module-registry-declare! registry 'client '())
    (cl-prolog-kit::module-registry-import! registry 'client 'library)
    (signals-error
      (cl-prolog-kit::module-registry-ensure-definition-allowed
       registry 'client 'public 1))
    (signals-error
      (cl-prolog-kit::module-registry-validate-exports
       registry 'library
       (lambda (module predicate arity)
         (declare (ignore module predicate arity)) nil)))))

(deftest module-registry-copy-is-independent ()
  (let* ((registry (cl-prolog-kit::make-module-registry))
         (copy (cl-prolog-kit::module-registry-copy registry)))
    (cl-prolog-kit::module-registry-declare! copy 'new-module '())
    (signals-error
      (cl-prolog-kit::module-registry-resolve-qualified
       registry 'new-module 'anything 0
       (lambda (module predicate arity)
                 (declare (ignore module predicate arity)) t)))))

(deftest current-module-reflects-module-registry ()
  (let ((rulebase (make-rulebase)))
    (cl-prolog-kit::module-registry-declare!
     (cl-prolog-kit::rulebase-module-registry rulebase) 'zeta '())
    (cl-prolog-kit::module-registry-declare!
     (cl-prolog-kit::rulebase-module-registry rulebase) 'alpha '())
    (assert-query rulebase (cl-prolog-kit::current_module ?module)
      :ordered (((?module . cl-prolog-kit::user))
          ((?module . alpha))
          ((?module . zeta))))
    (assert-query rulebase (cl-prolog-kit::current_module alpha) :succeeds)
    (assert-query rulebase (cl-prolog-kit::current_module missing) :fails)
    (assert-query rulebase (cl-prolog-kit::current_module 42) :signals prolog-type-error)))

(deftest module-consult-isolates-colliding-predicates ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog ":- module(beta, [value/1]). value(beta)." rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:value(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "alpha:value(beta)."))))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "beta:value(beta).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "beta:value(alpha)."))))))

(deftest module-import-resolves-unqualified-goals ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog
     ":- use_module(alpha). selected(X) :- value(X)."
     rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "selected(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "selected(beta)."))))))

(deftest qualified-builtins-use-explicit-module ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:call(value(alpha)).")))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:assertz(stored(alpha)).")))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:stored(alpha).")))
    (signals-error
      (prolog-succeeds-p rulebase
                         (read-prolog-term "stored(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "alpha:not(value(alpha))."))))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:not(value(beta)).")))))

(deftest qualified-module-variable-resolves-through-bindings ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog
     ":- module(alpha, [value/1]). value(alpha)."
     rulebase)
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term "Module = alpha, Module:value(alpha).")))))

(deftest qualified-module-errors-are-catchable ()
  (let ((rulebase (make-rulebase)))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(Module:true, error(instantiation_error, _), true).")))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(42:true, error(type_error(atom, 42), _), true).")))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(unknown_module:true, error(existence_error(module, unknown_module), _), true).")))))

(deftest current-predicate-includes-imported-predicates ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog
     ":- module(alpha, [value/1]). value(alpha)."
     rulebase)
    (consult-prolog
     ":- module(client, []). :- use_module(alpha)."
     rulebase)
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term "client:current_predicate(value/1).")))))

(deftest module-consult-rolls-back-import-redefinition ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (signals-error
      (consult-prolog
       ":- use_module(alpha). value(local)."
       rulebase))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:value(alpha).")))
    (signals-error
      (prolog-succeeds-p rulebase
                         (read-prolog-term "value(local).")))))

(deftest-table module-system-rejects-invalid-modules ()
  (:signals (query-prolog (make-rulebase) (read-prolog-term "ghost:true."))
            "A qualified builtin must reject an unknown module")
  (:signals (consult-prolog "already_seen. :- module(late, []).")
            "A module directive must be the unique first source term")
  (:signals (consult-prolog ":- module(under_defined, [missing/1]).")
            "A module consult must reject an undefined export"))

(deftest dynamic-assertion-rejects-import-redefinition ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog ":- use_module(alpha)." rulebase)
    (signals-error
      (query-prolog rulebase (read-prolog-term "assertz(value(local)).")))
    (is (prolog-succeeds-p rulebase (read-prolog-term "value(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "value(local)."))))))

(deftest static-user-fast-path-resolves-imported-predicate ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog
     ":- module(alpha, [value/1]). value(imported)."
     rulebase)
    (consult-prolog
     ":- use_module(alpha). selected(X) :- value(X)."
     rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "selected(imported).")))
    (is (not (prolog-succeeds-p
              rulebase
              (read-prolog-term "selected(missing)."))))))

(deftest qualified-goal-rejects-stale-package-identity ()
  (let* ((package (find-package "CL-PROLOG-KIT"))
         (original-name (package-name package))
         (original-nicknames (package-nicknames package))
         (colon (find-symbol ":" package))
         (predicate
           (symbol-function
            (find-symbol "%QUALIFIED-GOAL-P" package)))
         (temporary-name "CL-PROLOG-KIT-GUARD-ORIGINAL")
         (replacement nil)
         (renamed nil))
    (is (null (find-package temporary-name)))
    (unwind-protect
         (progn
           (is (funcall predicate
                        (list colon (quote user) (quote (true)))))
           (setf renamed (rename-package package temporary-name))
           (is (eq :package-error
                   (handler-case
                       (funcall predicate
                                (list colon (quote user) (quote (true))))
                     (package-error () :package-error))))
           (setf replacement
                 (make-package original-name :use nil))
           (let ((new-colon (intern ":" replacement)))
             (is (not (eq colon new-colon)))
             (is (not (funcall predicate
                               (list colon
                                     (quote user)
                                     (quote (true))))))
             (is (funcall predicate
                          (list new-colon
                                (quote user)
                                (quote (true)))))))
      (when (and replacement (package-name replacement))
        (delete-package replacement))
      (when renamed
        (rename-package package
                        original-name
                        original-nicknames)))
    (is (eq package (find-package original-name)))
    (is (eq colon (find-symbol ":" package)))
    (is (equal original-nicknames
               (package-nicknames package)))))
