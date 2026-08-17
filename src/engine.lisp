;;;; CPS proof search core.
;;;;
;;;; Proof search is written in continuation-passing style: every prover
;;;; receives an EMIT continuation and calls it once per solution
;;;; environment.  Solutions therefore stream to the caller; nothing in the
;;;; engine accumulates result lists.
;;;;
;;;; Cut (!) is implemented with CATCH/THROW: each predicate invocation
;;;; establishes a fresh catch tag carried through the proof state, and !
;;;; throws to the tag of the clause it appears in (see prover.lisp).

(in-package #:cl-prolog-kit)

(defparameter *max-prolog-depth* nil
  "Default rule-resolution depth bound; NIL means unbounded search.")

(defparameter *max-prolog-builtin-output-length* 1048576
  "Maximum number of characters or list elements a single builtin call may
materialize in one step (format fill/repeat runs, numlist ranges, and similar).
Bounds attacker-controlled allocation; exceeding it raises a catchable
resource_error.  NIL disables the bound.")

;;; Prolog exception data

(define-condition prolog-exception (error)
  ((term :initarg :term :reader prolog-exception-term)
   (environment :initarg :environment :reader %prolog-exception-environment))
  (:report (lambda (condition stream)
             (format stream "Uncaught Prolog exception: ~S."
                     (prolog-exception-term condition))))
  (:documentation "A thrown term or ISO error term raised during Prolog execution."))

(define-condition prolog-runtime-error (prolog-exception) ()
  (:documentation "Base condition for engine-generated ISO Prolog errors."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +prolog-runtime-error-specifications+
    '((prolog-instantiation-error %raise-instantiation-error
       (environment operation message)
       (%iso-atom "INSTANTIATION_ERROR"))
      (prolog-type-error %raise-type-error
       (expected culprit environment operation message)
       (%iso-term "TYPE_ERROR" (%iso-atom expected) culprit))
      (prolog-domain-error %raise-domain-error
       (domain culprit environment operation message)
       (%iso-term "DOMAIN_ERROR" (%iso-atom domain) culprit))
      (prolog-permission-error %raise-permission-error
       (operation permission-type culprit environment context message)
       (%iso-term "PERMISSION_ERROR" (%iso-atom operation)
                  (%iso-atom permission-type) culprit))
      (prolog-existence-error %raise-existence-error
       (object-type culprit environment operation message)
       (%iso-term "EXISTENCE_ERROR" (%iso-atom object-type) culprit))
      (prolog-evaluation-error %raise-evaluation-error
       (reason environment operation message)
       (%iso-term "EVALUATION_ERROR" (%iso-atom reason)))
      ;; ISO 13211-1 7.12.2 (e): a term is well formed but outside what this
      ;; implementation can represent -- an integer that is no character code,
      ;; say.  Distinct from a domain error, which is about the value's meaning.
      (prolog-representation-error %raise-representation-error
       (flag environment operation message)
       (%iso-term "REPRESENTATION_ERROR" (%iso-atom flag)))
      ;; Resource and syntax errors are raised through hand-written entry
      ;; points that carry extra slots, so they contribute a condition only.
      (prolog-resource-error nil nil nil)
      (prolog-syntax-error nil nil nil))
    "The ISO error categories, each as (CONDITION RAISER PARAMETERS FORMAL).

PARAMETERS ends with the environment, operation and message forwarded to
%RAISE-ISO-ERROR; FORMAL builds the ISO formal term from the parameters
preceding them.  DEFINE-PROLOG-RUNTIME-ERROR-CONDITIONS and
DEFINE-PROLOG-RUNTIME-ERROR-RAISERS both read this table, so a category can
never gain a condition without its raiser or vice versa."))

(defmacro define-prolog-runtime-error-conditions ()
  "Define an empty PROLOG-RUNTIME-ERROR subtype per ISO error category."
  `(progn
     ,@(loop for (condition) in +prolog-runtime-error-specifications+
             collect `(define-condition ,condition (prolog-runtime-error) ()))))

(define-prolog-runtime-error-conditions)

(define-condition invalid-max-depth-error (error)
  ((value :initarg :value :reader invalid-max-depth-error-value))
  (:report (lambda (condition stream)
             (format stream ":MAX-DEPTH must be NIL or a non-negative integer, got ~S."
                     (invalid-max-depth-error-value condition))))
  (:documentation "Signalled when a query receives an invalid :MAX-DEPTH option."))

(define-condition prolog-halt (serious-condition)
  ((code :initarg :code :reader prolog-halt-code))
  (:report (lambda (condition stream)
             (format stream "Prolog requested halt with exit code ~D."
                     (prolog-halt-code condition))))
  (:documentation "Raised by halt/0 and halt/1; embedders decide how to exit.

Deliberately not a PROLOG-EXCEPTION: catch/3 must not intercept it."))

(define-condition prolog-depth-limit-exceeded (prolog-resource-error)
  ((goal :initarg :goal :reader prolog-depth-limit-exceeded-goal))
  (:report (lambda (condition stream)
             (format stream "Prolog rule-resolution depth limit reached while proving ~S."
                     (prolog-depth-limit-exceeded-goal condition))))
  (:documentation "Signalled when proof search would exceed an explicit rule depth bound."))

(defun %validate-max-depth (value)
  "Return VALUE when it is a valid rule depth bound, otherwise signal an error."
  (unless (typep value '(or null (integer 0 *)))
    (error 'invalid-max-depth-error :value value))
  value)

(define-contextual-error-condition invalid-goal-error (prolog-type-error)
  (goal invalid-goal-error-goal)
  (reason invalid-goal-error-reason)
  "Invalid Prolog goal ~S: ~A."
  "Signalled when a goal is structurally unusable.")

(declaim (ftype function %iso-atom %iso-term %iso-error-term))

(defparameter *max-prolog-term-arity* 1024
  "The largest arity a term may have, reported by the `max_arity' flag.

ISO 13211-1 8.17 lets an implementation answer `unbounded' here, but every
other resource this engine exposes is bounded, and an unbounded arity is the
one hole left in that: `functor(T, foo, Huge)' would allocate without limit.")

(defun %invalid-goal (goal reason &rest arguments)
  (let ((message (apply #'format nil reason arguments)))
    (error 'invalid-goal-error
           :goal goal
           :reason message
           :term (%iso-error-term (%iso-term "TYPE_ERROR" (%iso-atom "CALLABLE") goal)
                                  (%iso-atom "CALL") message)
           :environment nil)))

;;; Prolog exception construction and control flow

(defun %iso-atom (name)
  "Return the stable Prolog atom for ISO error vocabulary NAME."
  (%prolog-atom-symbol (string-downcase name)))

(defun %iso-term (functor &rest arguments)
  "Construct an ISO exception term without inheriting Common Lisp symbols."
  (cons (%iso-atom functor) arguments))

(defun %iso-error-term (formal operation message)
  "Wrap FORMAL in the ISO error/2 context used by public engine failures."
  (%iso-term "ERROR" formal (%iso-term "CONTEXT" operation message)))

(defun %raise-iso-error (condition-type formal environment operation message)
  "Raise CONDITION-TYPE carrying a catchable ISO error term."
  (error condition-type
         :term (%iso-error-term formal operation message)
         :environment environment))

(defmacro define-prolog-runtime-error-raisers ()
  "Define the %RAISE-<CATEGORY>-ERROR wrapper for every ISO error category that
declares one in +PROLOG-RUNTIME-ERROR-SPECIFICATIONS+."
  `(progn
     ,@(loop for (condition raiser parameters formal)
               in +prolog-runtime-error-specifications+
             when raiser
               collect `(defun ,raiser ,parameters
                          (%raise-iso-error ',condition ,formal
                                            ,@(last parameters 3))))))

(define-prolog-runtime-error-raisers)

(progn
  (defun %raise-resource-error
      (resource environment operation message &key condition-type goal)
    (let ((term (%iso-error-term
                 (%iso-term "RESOURCE_ERROR" (%iso-atom resource))
                 operation message)))
      (if condition-type
          (error condition-type
                 :term term
                 :environment environment
                 :goal goal)
          (error 'prolog-resource-error
                 :term term
                 :environment environment))))
  (defun %raise-parser-resource-error (condition environment operation)
    "Raise parser CONDITION as a catchable ISO resource_error/1 term."
    (%raise-resource-error
     (prolog-parser-resource-error-resource condition)
     environment
     operation
     (princ-to-string condition))))

(defun %raise-syntax-error-for (description environment operation)
  "Raise DESCRIPTION as a catchable ISO syntax_error/1 term.

DESCRIPTION becomes the culprit as an uninterned symbol, so untrusted text never
enters a package."
  (%raise-iso-error
   'prolog-syntax-error
   (%iso-term "SYNTAX_ERROR" (make-symbol description))
   environment operation description))

(defun %raise-syntax-error (condition environment operation)
  "Raise parser CONDITION as a catchable ISO syntax_error/1 term."
  (%raise-syntax-error-for (prolog-parse-error-description condition)
                           environment operation))

(defmacro define-term-guard (name (value &rest extra-parameters)
                             &key documentation resolve instantiation
                                  accept type type-message result)
  "Define NAME as a guard over one goal argument.

The generated function takes (VALUE ENVIRONMENT OPERATION . EXTRA-PARAMETERS).
It resolves VALUE against ENVIRONMENT when RESOLVE is true, raises
instantiation_error carrying INSTANTIATION while VALUE is still unbound, raises
type_error(TYPE) carrying TYPE-MESSAGE unless ACCEPT holds, and returns RESULT
\(VALUE itself by default).  INSTANTIATION, ACCEPT, TYPE-MESSAGE and RESULT are
forms evaluated with VALUE and EXTRA-PARAMETERS in scope."
  `(defun ,name (,value environment operation ,@extra-parameters)
     ,@(when documentation (list documentation))
     (let ((,value ,(if resolve `(logic-substitute ,value environment) value)))
       (when (logic-var-p ,value)
         (%raise-instantiation-error environment operation ,instantiation))
       ,@(when accept
           `((unless ,accept
               (%raise-type-error ,type ,value environment operation
                                  ,type-message))))
       ,(or result value))))

(defun %require-bounded-integer (value environment operation argument
                                 &key (minimum 0) allow-variable)
  "Validate VALUE as an integer of at least MINIMUM, naming it ARGUMENT in the
ISO error messages.  MINIMUM selects the domain reported for an out-of-range
value: not_less_than_zero for 0, not_less_than_one for 1.  An unbound VALUE is
returned unchanged when ALLOW-VARIABLE, and otherwise raises
instantiation_error.  Returns VALUE when it is valid."
  (cond
    ((logic-var-p value)
     (unless allow-variable
       (%raise-instantiation-error
        environment operation
        (format nil "~A must be instantiated" argument)))
     value)
    ((not (integerp value))
     (%raise-type-error "INTEGER" value environment operation
                        (format nil "~A must be an integer" argument)))
    ((< value minimum)
     (%raise-domain-error
      (if (plusp minimum) "NOT_LESS_THAN_ONE" "NOT_LESS_THAN_ZERO")
      value environment operation
      (format nil "~A must not be less than ~D" argument minimum)))
    (t value)))

(defun %raise-prolog-exception (term environment)
  "Raise TERM together with the binding environment active at THROW/1.
Callers must have already rejected an unbound TERM."
  (error 'prolog-exception :term term :environment environment))

;;; Builtin goal dispatch

(defvar *fixed-builtin-solvers* (make-hash-table :test #'eq))





(defun %fixed-builtin-arity-table! (predicate)
    "Return the fixed-builtin arity table for PREDICATE, creating it if needed."
    (or (gethash predicate *fixed-builtin-solvers*)
        (setf (gethash predicate *fixed-builtin-solvers*)
              (make-hash-table :test (function eql)))))
(defvar *variadic-builtin-solvers* (make-hash-table :test #'eq))

(defun %goal-solver (predicate arity)
  "Return the builtin solver registered for PREDICATE/ARITY, if any."
  (or (let ((arities (gethash predicate *fixed-builtin-solvers*)))
        (and arities (gethash arity arities)))
      (let ((entry (gethash predicate *variadic-builtin-solvers*)))
        (when (and entry (>= arity (car entry)))
          (cdr entry)))))

(defvar *builtin-predicate-indicators* '())

(defun %register-builtin-predicate! (predicate arity)
  "Register the canonical indicator exposed by CURRENT-PREDICATE/1."
  (pushnew (list '/ predicate arity) *builtin-predicate-indicators*
           :test #'equal)
  predicate)

(defun %register-builtin-solver! (predicate minimum maximum solver)
  "Register SOLVER for PREDICATE, replacing a definition loaded previously."
  ;; Reader symbols inherited from COMMON-LISP are normalized into USER-ATOMS
  ;; by the Prolog parser.  Register that canonical spelling as an alias so
  ;; parsed and Lisp-authored goals dispatch identically.
  (dolist (alias (remove-duplicates
                  (list predicate
                        (%prolog-atom-symbol (%atom-text predicate)))
                  :test (function eq)))
    (if maximum
        (progn
          (remhash alias *variadic-builtin-solvers*)
          (setf (gethash maximum (%fixed-builtin-arity-table! alias)) solver))
        (progn
          (remhash alias *fixed-builtin-solvers*)
          (setf (gethash alias *variadic-builtin-solvers*)
                (cons minimum solver)))))
  (%register-builtin-predicate! predicate minimum))

(defun %builtin-predicate-indicators ()
  "Return a detached snapshot of builtin predicate indicators."
  (reverse (copy-list *builtin-predicate-indicators*)))

(defun %argument-list-arity (argument-list)
  "Return (VALUES MINIMUM MAXIMUM) arity for ARGUMENT-LIST; MAXIMUM is NIL when variadic."
  (let ((required (position '&rest argument-list)))
    (if required
        (values required nil)
        (values (length argument-list) (length argument-list)))))

(defmacro define-builtin ((name &rest argument-list) (rulebase environment depth emit)
                          &body body)
  "Define builtin solvers for goals of shape (NAME . ARGUMENT-LIST).

NAME may also be a list of head symbols sharing one solver.  ARGUMENT-LIST
is an ordinary lambda list (only required parameters and &REST are
supported).  Fixed-arity builtins dispatch on their exact predicate indicator;
variadic builtins dispatch only at or above their required arity.
BODY must call EMIT with one extended environment per solution."
  (multiple-value-bind (minimum maximum)
      (%argument-list-arity argument-list)
    (let* ((goal (gensym "GOAL"))
           (names (if (listp name) name (list name))))
      `(progn
         ,@(mapcar
            (lambda (builtin-name)
              `(eval-when (:load-toplevel :execute)
                 (%register-builtin-solver!
                  (quote ,builtin-name) ,minimum ,maximum
                  (lambda (,goal ,rulebase ,environment ,depth ,emit)
                    (declare (ignorable ,rulebase ,environment ,depth ,emit))
                    ;; Dispatch guarantees the arity matches: fixed builtins
                    ;; are keyed by their exact indicator, variadic ones only
                    ;; receive goals at or above their required arity.
                    (destructuring-bind ,argument-list (rest ,goal)
                      ,@body)))))
            names)
         (quote ,(first names))))))

;;; Foreign predicate dispatch

(defvar *foreign-predicate-indicators* '())

(defun %foreign-predicate-indicators ()
  "Return a detached snapshot of registered foreign predicate indicators."
  (nreverse (copy-list *foreign-predicate-indicators*)))

(defgeneric %foreign-goal-solver (predicate arity)
  (:documentation
   "Return the CPS foreign solver registered for PREDICATE/ARITY."))

(defmethod %foreign-goal-solver (predicate arity)
  (declare (cl:ignore predicate arity))
  nil)

(defmacro define-foreign-predicate ((name &rest argument-list)
                                    (rulebase environment depth emit)
                                    &body body)
  "Define the authoritative foreign solver for the exact predicate NAME/ARITY.

BODY must call EMIT synchronously with one extended environment per solution.
Calling EMIT zero times fails; calling it repeatedly produces multiple solutions.
EMIT is dynamically scoped: do not retain it, call it from another thread, or
call it after BODY returns."
  (when (find-if (lambda (parameter)
                   (member parameter lambda-list-keywords))
                 argument-list)
    (error "Foreign predicates require a fixed argument list: ~S"
           argument-list))
  (let ((goal (gensym "GOAL"))
        (arity (length argument-list)))
    `(progn
       (eval-when (:load-toplevel :execute)
         (pushnew (list '/ ',name ,arity) *foreign-predicate-indicators*
                  :test #'equal))
       (defmethod %foreign-goal-solver ((predicate (eql ',name))
                                       (arity (eql ,arity)))
         (declare (cl:ignore predicate arity))
         (lambda (,goal ,rulebase ,environment ,depth ,emit)
           (declare (ignorable ,rulebase ,environment ,depth ,emit))
           (destructuring-bind ,argument-list (rest ,goal)
             ,@body))))))
