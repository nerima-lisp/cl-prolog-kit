;;;; Query normalization and goal/module resolution.
;;;;
;;;; The pure, non-continuation half of what prover.lisp used to hold: turning
;;;; a query into a list of goal forms, deriving predicate indicators, reading
;;;; revision-stable clause snapshots out of the rulebase, and resolving a goal
;;;; to the module that defines it.  The CPS proof search that consumes all of
;;;; this lives in prover.lisp, which loads immediately after.

(in-package #:cl-prolog-kit)

(defparameter +call-context-atom+ (%iso-atom "CALL"))

(defun %conjunction-p (query)
  "True when QUERY is already a list of goals rather than a single goal."
  (and (consp query)
       (or (consp (first query))
           (eq (first query) '!))))

(defun %normalize-query (query)
  "Coerce QUERY into a list of goals."
  (cond
    ((null query) '())
    ((%conjunction-p query) query)
    (t (list query))))

(defun %goal-form-p (goal)
  "True when GOAL is a proper callable goal form."
  (and (consp goal)
       (%proper-list-p goal)
       (symbolp (first goal))))

(defun %ensure-goal-form (goal)
  "Normalize bare-symbol goals to list form."
  (if (symbolp goal)
      (list goal)
      goal))

(defun %goal-predicate-indicator (goal)
  "Return the ISO predicate indicator for normalized GOAL."
  (list '/ (first goal) (length (rest goal))))

(defun %proof-module-entries (state &optional (module (proof-state-module state)))
  "Return one revision-stable module snapshot shared by the current query."
  (let* ((rulebase (proof-state-rulebase state))
         (session (proof-state-table-session state))
         (key (list rulebase (rulebase-revision rulebase) module)))
    (multiple-value-bind (entries present-p)
        (gethash key (%table-session-module-entries session))
      (if present-p
          entries
          (setf (gethash key (%table-session-module-entries session))
                (%rulebase-module-entries rulebase module))))))

(defun %proof-predicate-entries
    (goal state &optional (module (proof-state-module state)))
  "Return the immutable current descriptor entries for GOAL."
  (let* ((rulebase (proof-state-rulebase state))
         (predicate (first goal))
         (arity (length (rest goal)))
         (descriptor
           (%rulebase-predicate-descriptor
            rulebase module predicate arity)))
    (when descriptor
      (if (plusp arity)
          (%predicate-descriptor-first-argument-entries
           descriptor
           (%walk-term-indexed
            (second goal)
            (proof-state-environment-index state)))
          (%predicate-descriptor-entries descriptor)))))

(defun %rulebase-defines-goal-p (state module predicate arity)
  "True when RULEBASE contains or declares MODULE:PREDICATE/ARITY."
  (let ((rulebase (proof-state-rulebase state)))
    (or (%rulebase-predicate-property rulebase predicate arity module)
        (%rulebase-predicate-descriptor
         rulebase module predicate arity))))

(defun %qualified-goal-p (goal)
  (and (%goal-form-p goal)
       (= (length goal) 3)
       (eq (first goal)
           (let* ((cached (load-time-value (%prolog-symbol ":") t))
                  (home (symbol-package cached)))
             (if (and home
                      (eq (find-package (quote #:cl-prolog-kit)) home))
                 cached
                 (%prolog-symbol ":"))))))

(defun %resolve-qualified-module (module state)
  "Resolve MODULE through the current bindings and validate it as a module atom."
  (let* ((environment (proof-state-bindings state))
         (resolved
           (%logic-substitute-indexed
            module (proof-state-environment-index state)))
         (context +call-context-atom+))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment context
                                  "module qualifier must be instantiated"))
    (unless (symbolp resolved)
      (%raise-type-error "ATOM" resolved environment context
                         "module qualifier must be an atom"))
    (unless (gethash resolved
                     (module-registry-modules
                      (rulebase-module-registry (proof-state-rulebase state))))
      (%raise-existence-error "MODULE" resolved environment context
                              "unknown module"))
    resolved))

(defun %resolve-user-goal (goal state &optional explicit-module)
  (let* ((rulebase (proof-state-rulebase state))
         (registry (rulebase-module-registry rulebase))
         (caller (or explicit-module (proof-state-module state)))
         (predicate (first goal))
         (arity (length (rest goal)))
         (local-descriptor
           (and (not explicit-module)
                (%rulebase-predicate-descriptor
                 rulebase caller predicate arity))))
    (if local-descriptor
        (values goal caller)
        (let ((local-p
                (lambda (module name count)
                  (%rulebase-defines-goal-p state module name count))))
          (values goal
                  (if explicit-module
                      (module-registry-resolve-qualified
                       registry caller predicate arity local-p)
                      (module-registry-resolve
                       registry caller predicate arity local-p)))))))
