(in-package #:cl-prolog-kit)

(defun %prepare-dynamic-assertion! (rulebase entry)
  "Validate and register the predicate targeted by a dynamic assertion."
  (multiple-value-bind (predicate arity) (%entry-predicate-arity entry)
    (module-registry-ensure-definition-allowed
     (rulebase-module-registry rulebase)
     *current-prolog-module* predicate arity)
    (unless (%rulebase-predicate-property
             rulebase predicate arity *current-prolog-module*)
      (%set-rulebase-predicate-property!
       rulebase predicate arity :dynamic *current-prolog-module*))))

(define-builtin (asserta clause) (rulebase environment depth emit)
  (let* ((goal (list 'asserta clause))
         (entry (%clause-term-entry (logic-substitute clause environment)
                                    rulebase goal environment)))
    (%prepare-dynamic-assertion! rulebase entry)
    (rulebase-insert-clause! rulebase entry :position :first
                             :module *current-prolog-module*)
    (funcall emit environment)))

(define-builtin ((assert assertz) clause) (rulebase environment depth emit)
  (let* ((goal (list 'assertz clause))
         (entry (%clause-term-entry (logic-substitute clause environment)
                                    rulebase goal environment)))
    (%prepare-dynamic-assertion! rulebase entry)
    (rulebase-insert-clause! rulebase entry :position :last
                             :module *current-prolog-module*)
    (funcall emit environment)))

(define-builtin (retract clause) (rulebase environment depth emit)
  (let* ((goal (list 'retract clause))
         (pattern (logic-substitute clause environment))
         (head (%dynamic-clause-head pattern environment 'retract))
         (normalized-pattern (%dynamic-pattern-term pattern head))
         (prolog-shape-p (%prolog-rule-term-p pattern)))
    (%ensure-dynamic-predicate rulebase (first head) (length (rest head)) goal environment)
    (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
      (declare (cl:ignore snapshot))
      (dolist (entry entries)
        (when (eq (%stored-clause-module entry) *current-prolog-module*)
          (let* ((clause-entry (%stored-clause-clause entry))
                 (fresh (%freshen-dynamic-clause clause-entry))
                 (stored (%dynamic-pattern-clause-term fresh prolog-shape-p)))
            (multiple-value-bind (extended ok)
                (unify normalized-pattern stored environment)
              (when (and ok (%rulebase-retract-entry! rulebase entry))
                (funcall emit extended)))))))))

(define-builtin (retractall clause) (rulebase environment depth emit)
  (let* ((goal (list 'retractall clause))
         (pattern (logic-substitute clause environment))
         (head (%dynamic-clause-head pattern environment 'retractall))
         (normalized-pattern (%dynamic-pattern-term pattern head))
         (prolog-shape-p (%prolog-rule-term-p pattern)))
    (%ensure-dynamic-predicate rulebase (first head) (length (rest head)) goal environment)
    (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
      (declare (cl:ignore snapshot))
      (let ((matches '()))
        (dolist (entry entries)
          (when (eq (%stored-clause-module entry) *current-prolog-module*)
            (let* ((fresh (%freshen-dynamic-clause (%stored-clause-clause entry)))
                   (stored (%dynamic-pattern-clause-term fresh prolog-shape-p)))
              (multiple-value-bind (extended ok)
                  (unify normalized-pattern stored environment)
                (declare (cl:ignore extended))
                (when ok (push entry matches))))))
        (%rulebase-retract-entries! rulebase matches)))
    (funcall emit environment)))

(defun %current-module-predicate-indicators (rulebase)
  "Return (VALUES INDICATORS SEEN), the deduplicated predicate/2 indicators
visible in RULEBASE's current module: user clauses, declared predicate
properties, builtins, foreign predicates, and imports.  SEEN maps each
indicator to T for O(1) membership checks."
  (let ((seen (make-hash-table :test #'equal))
        (indicators '()))
    (labels ((remember (predicate arity)
               (let ((candidate (list '/ predicate arity)))
                 (unless (gethash candidate seen)
                   (setf (gethash candidate seen) t)
                   (push candidate indicators)))))
      (dolist (stored (%rulebase-module-entries
                       rulebase *current-prolog-module*))
        (multiple-value-bind (predicate arity)
            (%entry-predicate-arity (%stored-clause-clause stored))
          (remember predicate arity)))
      (dolist (candidate
               (%rulebase-declared-predicate-indicators
                rulebase *current-prolog-module*))
        (remember (second candidate) (third candidate)))
      (dolist (candidate (%builtin-predicate-indicators))
        (remember (second candidate) (third candidate)))
      (dolist (candidate (%foreign-predicate-indicators))
        (remember (second candidate) (third candidate)))
      (let ((namespace
              (gethash *current-prolog-module*
                       (module-registry-modules
                        (rulebase-module-registry rulebase)))))
        (when namespace
          (maphash (lambda (key origin)
                     (declare (ignore origin))
                     (remember (car key) (cdr key)))
                   (prolog-module-imports namespace)))))
    (values indicators seen)))

(define-builtin (current_predicate indicator) (rulebase environment depth emit)
  (let ((resolved (logic-substitute indicator environment)))
    (multiple-value-bind (indicators seen)
        (%current-module-predicate-indicators rulebase)
      (unless (logic-var-p resolved)
        (unless (and (%proper-list-p resolved)
                     (= (length resolved) 3)
                     (eq (first resolved) '/)
                     (symbolp (second resolved)))
          (%raise-type-error "PREDICATE_INDICATOR" resolved
                             environment 'current_predicate
                             "expected a predicate indicator"))
        (%require-bounded-integer (third resolved) environment
                                  'current_predicate "predicate arity"
                                  :allow-variable t))
      (if (or (logic-var-p resolved)
              (%term-has-variables-p resolved))
          (dolist (candidate (nreverse indicators))
            (%unify-emit indicator candidate environment emit))
          (progn
            ;; ISO 13211-1 8.8.2 enumerates the *user-defined* procedures, so a
            ;; builtin's indicator does not succeed here even though the
            ;; predicate exists -- `predicate_property/2' is what reports those.
            (when (gethash resolved seen)
              (funcall emit environment)))))))

(defun %predicate-clause-count (rulebase predicate arity module)
  (count-if (%predicate-clause-matcher predicate arity)
            (%rulebase-module-entries rulebase module)))

(defun %predicate-properties (rulebase predicate arity module)
  "Return the supported reflection properties for PREDICATE/ARITY."
  (cond
    ((%builtin-predicate-p predicate arity)
     '(built_in defined))
    (t
     (let ((declared (%rulebase-predicate-property
                      rulebase predicate arity module))
           (defined (%rulebase-defines-predicate-p
                     rulebase predicate arity module)))
       (when (or declared defined)
         (list (if (eq declared :dynamic) 'dynamic 'static)
               'user
               'defined
               (list 'number_of_clauses
                     (%predicate-clause-count rulebase predicate arity
                                              module))))))))

(define-builtin (predicate_property head property)
    (rulebase environment depth emit)
  (let* ((resolved-head (logic-substitute head environment))
         (resolved-property (logic-substitute property environment))
         (callable (%ensure-callable resolved-head environment
                                     'predicate_property))
         (predicate (first callable))
         (arity (length (rest callable))))
    (unless (or (logic-var-p resolved-property)
                (symbolp resolved-property)
                (and (%proper-list-p resolved-property)
                     (symbolp (first resolved-property))))
      (%raise-type-error "CALLABLE" resolved-property environment
                         'predicate_property
                         "predicate property must be an atom or compound term"))
    (dolist (candidate
             (%predicate-properties rulebase predicate arity
                                    *current-prolog-module*))
      (%unify-emit property candidate environment emit))))

(defun %current-module-names (rulebase)
  "Return registered module names in a deterministic reflection order."
  (let ((names '()))
    (maphash (lambda (name module)
               (declare (ignore module))
               (unless (eq name +default-prolog-module+)
                 (push name names)))
             (module-registry-modules
              (rulebase-module-registry rulebase)))
    (cons +default-prolog-module+
          (sort names #'string<
                :key (lambda (name)
                       (format nil "~A/~A"
                               (or (and (symbol-package name)
                                        (package-name (symbol-package name)))
                                   "")
                               (symbol-name name)))))))

(define-builtin (current_module module) (rulebase environment depth emit)
  (let ((resolved (logic-substitute module environment)))
    (unless (or (logic-var-p resolved) (symbolp resolved))
      (%raise-type-error "ATOM" resolved environment 'current_module
                         "module name must be an atom"))
    (if (logic-var-p resolved)
        (dolist (candidate (%current-module-names rulebase))
          (%unify-emit module candidate environment emit))
        (when (gethash resolved
                       (module-registry-modules
                        (rulebase-module-registry rulebase)))
          (funcall emit environment)))))

(define-builtin (abolish indicator) (rulebase environment depth emit)
  (let* ((goal (list 'abolish indicator))
         (resolved (logic-substitute indicator environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment 'abolish
                                  "predicate indicator must be instantiated"))
    (unless (and (%proper-list-p resolved)
                 (= (length resolved) 3)
                 (eq (first resolved) '/))
      (%raise-type-error "PREDICATE_INDICATOR" resolved environment 'abolish
                         "expected a predicate indicator"))
    ;; ISO 13211-1 8.9.4.3 reports the offending *part* of the indicator: a
    ;; non-atom name is a type_error(atom, Name), not one about the whole term.
    (unless (symbolp (second resolved))
      (%raise-type-error "ATOM" (second resolved) environment 'abolish
                         "predicate name must be an atom"))
    (unless (integerp (third resolved))
      (%raise-type-error "INTEGER" (third resolved) environment 'abolish
                         "predicate arity must be an integer"))
    (when (> (third resolved) *max-prolog-term-arity*)
      (%raise-representation-error "MAX_ARITY" environment 'abolish
                                   "arity exceeds the max_arity flag"))
    (when (minusp (third resolved))
      (%raise-domain-error "NOT_LESS_THAN_ZERO" (third resolved)
                           environment 'abolish
                           "predicate arity cannot be negative"))
    (destructuring-bind (slash predicate arity) resolved
      (declare (cl:ignore slash))
      (%ensure-dynamic-predicate rulebase predicate arity goal environment)
      (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
        (declare (cl:ignore snapshot))
        (%rulebase-retract-entries!
         rulebase
         (remove-if-not
          (lambda (stored)
            (multiple-value-bind (entry-predicate entry-arity)
                (%entry-predicate-arity (%stored-clause-clause stored))
              (and (eq (%stored-clause-module stored) *current-prolog-module*)
                   (eq predicate entry-predicate) (= arity entry-arity))))
          entries)))
      (%remove-rulebase-predicate-property!
       rulebase predicate arity *current-prolog-module*)
      (%remove-rulebase-table-declarations!
       rulebase predicate arity *current-prolog-module*))
    (funcall emit environment)))

(define-builtin (clause head body) (rulebase environment depth emit)
  (let* ((resolved-head (logic-substitute head environment))
         (callable (%ensure-callable resolved-head environment 'clause))
         (resolved-body (logic-substitute body environment)))
    ;; ISO 13211-1 8.8.1.3: a bound Body that is not callable is a type_error,
    ;; not a silent failure.
    (unless (or (logic-var-p resolved-body)
                (symbolp resolved-body)
                (%goal-form-p resolved-body))
      (%raise-type-error "CALLABLE" resolved-body environment
                         (%iso-atom "CLAUSE")
                         "clause/2 body must be a callable term"))
    (%ensure-dynamic-predicate rulebase (first callable)
                               (length (rest callable))
                               (list 'clause head body) environment
                               :operation "ACCESS"
                               :permission-type "PRIVATE_PROCEDURE")
    (dolist (entry (%rulebase-module-entries
                    rulebase *current-prolog-module*))
      (let ((fresh (%freshen-dynamic-clause (%stored-clause-clause entry))))
        (multiple-value-bind (head-environment head-ok)
            (unify callable (%entry-head fresh) environment)
          (when head-ok
            (%unify-emit body (%entry-body-term fresh) head-environment emit)))))))
