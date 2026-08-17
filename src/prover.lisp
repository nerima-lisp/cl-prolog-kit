;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.  The state threaded through the
;;;; continuation is defined in proof-state.lisp.

(in-package #:cl-prolog-kit)

(declaim (ftype function %proper-list-p %prove-goal/k %prove-clauses/k %prove-rule/k))

(defmacro %with-propagated-bindings
    ((propagated propagated-index extended extended-index) &body body)
  (let ((continue (gensym "CONTINUE")))
    `(if *constraint-post-unify-hook*
         (flet ((,continue (,propagated)
                  (let ((,propagated-index
                          (%environment-index-after-bindings
                           ,propagated ,extended ,extended-index)))
                    ,@body)))
           (declare (dynamic-extent #',continue))
           (funcall *constraint-post-unify-hook*
                    ,extended
                    (function ,continue)))
         (let* ((,propagated ,extended)
                (,propagated-index
                  (%environment-index-after-bindings
                   ,propagated ,extended ,extended-index)))
           ,@body))))

(defun %continue-matching-fact (goal entry state succeed)
  "Unify GOAL against stored fact ENTRY and continue with the extended state."
  (let ((template (%stored-clause-template entry)))
    (let ((program (%clause-template-rule-program template))
          (parent-bindings (proof-state-bindings state))
          (parent-index (proof-state-environment-index state)))
      (multiple-value-bind (extended ok extended-index)
          (if (and program
                   (zerop (length (%rule-program-body program))))
              (let* ((variable-count
                       (%rule-program-variable-count program))
                     (variables
                       (if (zerop variable-count)
                           #()
                           (make-array variable-count))))
                (declare (dynamic-extent variables))
                (dotimes (index variable-count)
                  (setf (svref variables index)
                        (%fresh-rule-program-variable)))
                (%unify-rule-program-head
                 goal program variables parent-bindings parent-index))
              (let ((fresh-head
                      (clause-head
                       (%materialize-stored-clause-for-proof entry template))))
                (%unify-indexed
                 goal fresh-head parent-bindings parent-index nil)))
        (when ok
          (%with-propagated-bindings
              (propagated propagated-index extended extended-index)
            (funcall succeed
                     (%state-with
                      state
                      :bindings propagated
                      :environment-index propagated-index))))))))

(defun %prove-goals/k (goals state succeed)
  "Prove conjunction GOALS, calling SUCCEED with each solution state.

Each goal's solutions continue into the rest of the conjunction rebased
onto this conjunction's own cut barrier, so a callee's barrier never leaks
into the caller's remaining goals."
  (if (endp goals)
      (funcall succeed state)
      (let ((cut-tag (proof-state-cut-tag state)))
        (%prove-goal/k (first goals) state
                       (lambda (next-state)
                         (%prove-goals/k (rest goals)
                                         (%state-with next-state :cut-tag cut-tag)
                                         succeed))))))

(defun %resolve-dispatched-goal (goal state environment context)
  "Validate GOAL is callable after substitution and qualification.
Return (VALUES NORMALIZED-GOAL EXPLICIT-MODULE)."
  (let ((resolved-goal
          (%logic-substitute-indexed
           goal (proof-state-environment-index state))))
    (when (logic-var-p resolved-goal)
      (%raise-instantiation-error environment context
                                  "callable term must be instantiated"))
    (unless (or (symbolp resolved-goal)
                (%goal-form-p resolved-goal))
      (%invalid-goal resolved-goal
                     "a goal must be a symbol or a proper list headed by a symbol"))
    (let* ((qualified-p (%qualified-goal-p resolved-goal))
           (callable-goal
             (if qualified-p (third resolved-goal) resolved-goal)))
      (when (logic-var-p callable-goal)
        (%raise-instantiation-error environment context
                                    "callable term must be instantiated"))
      (unless (or (symbolp callable-goal)
                  (%goal-form-p callable-goal))
        (%invalid-goal resolved-goal
                       "a goal must be a symbol or a proper list headed by a symbol"))
      (values (%ensure-goal-form callable-goal)
              (and qualified-p
                   (%resolve-qualified-module (second resolved-goal) state))))))

(defun %resolve-dispatch-target (goal state environment context)
  (if (and (%goal-form-p goal)
           (symbolp (first goal))
           (not (logic-var-p (first goal)))
           (not (%qualified-goal-p goal)))
      (let* ((predicate (first goal))
             (arity (length (rest goal)))
             (builtin-solver (%goal-solver predicate arity))
             (foreign-solver (%foreign-goal-solver predicate arity))
             (static-solver (or builtin-solver foreign-solver)))
        (if static-solver
            (multiple-value-bind (resolved-goal resolved-module)
                (%resolve-dispatched-goal goal state environment context)
              (values resolved-goal resolved-module static-solver))
            (values goal nil nil)))
      (multiple-value-bind (resolved-goal resolved-module)
          (%resolve-dispatched-goal goal state environment context)
        (let* ((predicate (first resolved-goal))
               (arity (length (rest resolved-goal)))
               (builtin-solver (%goal-solver predicate arity))
               (foreign-solver (%foreign-goal-solver predicate arity)))
          (values resolved-goal resolved-module
                  (or builtin-solver foreign-solver))))))

(progn
  (defun %prove-cut-goal/k (state succeed)
    "Succeed once, then prune this predicate invocation's alternatives."
    (funcall succeed state)
    (cl:throw (proof-state-cut-tag state) t))

  (defun %prove-static-goal/k
      (solver normalized-goal explicit-module state succeed)
    "Invoke a registered solver and continue with each returned binding set."
    (when explicit-module
      (%find-prolog-module
       (rulebase-module-registry (proof-state-rulebase state))
       explicit-module "invoke qualified goal"))
    (let* ((solver-state
            (if explicit-module
                (%state-with state :module explicit-module)
                state))
           (*current-prolog-module* (proof-state-module solver-state))
           (*caller-cut-tag* (proof-state-cut-tag solver-state)))
      (funcall solver
               normalized-goal
               (proof-state-rulebase solver-state)
               (proof-state-bindings solver-state)
               (proof-state-remaining-depth solver-state)
               (lambda (bindings)
                 (funcall succeed
                          (%state-with solver-state :bindings bindings))))))

  (defun %prove-user-goal/k
      (normalized-goal explicit-module state environment context succeed)
    "Prove a user predicate or apply ISO undefined-procedure semantics."
    (multiple-value-bind (resolved-user-goal defining-module)
        (%resolve-user-goal normalized-goal state explicit-module)
      (if defining-module
          (%prove-clauses/k resolved-user-goal
                            (if (eq defining-module (proof-state-module state)) state (%state-with state :module defining-module))
                            succeed)
          ;; ISO 13211-1 7.7.7 and 7.11.2.4: what an undefined procedure
          ;; does is the unknown' flag's to decide.  error' (the default)
          ;; raises; fail' and warning' let the call simply fail, which
          ;; is what makes a partially written program runnable.
          (if (logic-var-p (first normalized-goal))
              (%raise-instantiation-error
               environment context
               "a goal reached at run time must be instantiated")
              (let ((mode (%prolog-flag-value (proof-state-rulebase state)
                                              (%find-prolog-flag "UNKNOWN"))))
                (when (string= mode "ERROR")
                  (%raise-existence-error
                   "PROCEDURE" (%goal-predicate-indicator normalized-goal)
                   environment context
                   "the invoked predicate is not defined"))
                nil)))))

  (defun %prove-goal-dispatch/k (goal state succeed)
    "Prove GOAL from STATE after any active depth-limit accounting."
    (let* ((*current-table-session* (proof-state-table-session state))
           (environment (proof-state-bindings state))
           (context +call-context-atom+))
      (multiple-value-bind (normalized-goal explicit-module solver)
          (%resolve-dispatch-target goal state environment context)
        (cond
          ((and (eq (first normalized-goal) (quote !))
                (null (rest normalized-goal)))
           (%prove-cut-goal/k state succeed))
          (solver
           (%prove-static-goal/k
            solver normalized-goal explicit-module state succeed))
          (t
           (%prove-user-goal/k
            normalized-goal explicit-module state environment context succeed)))))))

(defun %prove-goal/k (goal state succeed)
  "Prove GOAL, counting every dispatched call for local depth limits."
  (if (null *call-depth-limit-token*)
      (%prove-goal-dispatch/k goal state succeed)
      (progn
        (when (zerop *call-depth-limit-remaining*)
          (cl:throw *call-depth-limit-token* *call-depth-limit-token*))
        (let ((*call-depth-limit-remaining*
                (1- *call-depth-limit-remaining*))
              (*call-depth-limit-used*
                (1+ *call-depth-limit-used*)))
          (%prove-goal-dispatch/k goal state succeed)))))

(defun %prove-with-cut-tag/k (query rulebase bindings remaining-depth cut-tag
                              succeed &optional (module *current-prolog-module*))
  "Prove QUERY under an existing cut barrier CUT-TAG."
  (let ((*unification-scratch*
          (or *unification-scratch* (%make-unification-scratch))))
    (%prove-goals/k
     (%normalize-query query)
     (%make-proof-state
      rulebase
      bindings
      (%make-environment-index bindings)
      remaining-depth
      module
      (or *current-table-session*
          (%make-rulebase-table-session rulebase))
      cut-tag)
     (lambda (state)
       (funcall succeed (proof-state-bindings state))))))

(defun %prove-bindings/k (query rulebase bindings remaining-depth succeed
                          &optional (module *current-prolog-module*))
  "Prove QUERY and call SUCCEED with each resulting binding environment.

QUERY runs behind its own cut barrier: a cut inside it never prunes the
caller's alternatives, matching ISO CALL/1 opacity."
  (let ((cut-tag (%make-cut-tag)))
    (cl:catch cut-tag
      (%prove-with-cut-tag/k query rulebase bindings remaining-depth cut-tag
                             succeed module))))

(defun %prove-transparent/k (query rulebase bindings remaining-depth succeed)
  "Prove QUERY sharing the caller's cut barrier.

Cut-transparent control constructs must call this at solver entry, before
any nested proof rebinds *CALLER-CUT-TAG*."
  (%prove-with-cut-tag/k query rulebase bindings remaining-depth
                         *caller-cut-tag* succeed))

(defun %prove-raw-clauses/k (goal state succeed)
  "Prove GOAL within one predicate invocation and consume its cut.

The fresh CATCH tag is this invocation cut barrier: a cut in any clause
body throws here, abandoning the remaining clause alternatives."
  (let ((cut-tag (%make-cut-tag)))
    (cl:catch cut-tag
      (dolist (entry (%proof-predicate-entries goal state))
        (let ((clause (%stored-clause-clause entry)))
          (if (null (clause-body clause))
              (%continue-matching-fact goal entry state succeed)
              (%prove-rule/k goal entry state cut-tag succeed)))))))

(progn
  (defvar *rule-program-goal-materialization-count* nil)

  (declaim (inline %rule-program-operand-value))
  (defun %rule-program-operand-value (operand variables)
    (ecase (%clause-template-reference-kind operand)
      (:literal (%clause-template-reference-value operand))
      (:variable
       (let* ((index (%clause-template-reference-value operand))
              (variable (svref variables index)))
         (or variable
             (setf (svref variables index)
                   (%fresh-rule-program-variable)))))))

  (defun %unify-rule-program-head (goal program variables environment parent-index)
    "Unify GOAL arguments with PROGRAM operands without materializing a rule head."
    (let ((arguments (cdr goal))
          (operands (%rule-program-head-operands program))
          (extended environment)
          (index parent-index)
          (index-owned-p nil))
      (labels ((extend-directly (variable term)
                 (unless index-owned-p
                   (setf index (%copy-environment-index index)
                         index-owned-p t))
                 (let ((binding (cons variable term))) (%push-environment-index-binding binding index) (push binding extended)))
               (atomic-equal-p (left right)
                 (or (eq left right)
                     (and (symbolp left)
                          (symbolp right)
                          (%same-atom-text-p left right))
                     (equal left right)))
               (unify-operand (argument operand)
                 (let ((value (%rule-program-operand-value operand variables)))
                   (cond
                     ((consp argument) (values nil nil))
                     ((logic-var-p argument)
                      (multiple-value-bind (binding present-p)
                          (%environment-index-binding argument index)
                        (declare (ignore binding))
                        (if present-p
                            (values nil nil)
                            (if (and (logic-var-p value)
                                     (nth-value 1
                                       (%environment-index-binding value index)))
                                (values nil nil)
                                (progn
                                  (unless (eq argument value)
                                    (extend-directly argument value))
                                  (values t t))))))
                     ((logic-var-p value)
                      (multiple-value-bind (binding present-p)
                          (%environment-index-binding value index)
                        (declare (ignore binding))
                        (if present-p
                            (values nil nil)
                            (progn
                              (extend-directly value argument)
                              (values t t)))))
                     ((consp value) (values nil nil))
                     (t (values t (atomic-equal-p argument value)))))))
        (dotimes (operand-index (length operands) (values extended t index))
          (let ((argument (car arguments))
                (operand (svref operands operand-index)))
            (multiple-value-bind (handled-p ok)
                (unify-operand argument operand)
              (cond
                ((and handled-p (not ok))
                 (return-from %unify-rule-program-head
                   (values nil nil parent-index)))
                ((not handled-p)
                 (multiple-value-bind (next-extended generic-ok next-index)
                     (%unify-indexed argument
                                     (%rule-program-operand-value operand variables)
                                     extended index index-owned-p)
                   (unless generic-ok
                     (return-from %unify-rule-program-head
                       (values nil nil parent-index)))
                   (setf extended next-extended
                         index next-index
                         index-owned-p (not (eq index parent-index)))))))
            (setf arguments (cdr arguments)))))))

  (defun %unify-rule-program-instruction-head
      (instruction caller-variables program callee-variables
                   environment parent-index)
    "Unify encoded caller operands with an encoded callee head."
    (let ((caller-operands (%rule-instruction-operands instruction))
          (callee-operands (%rule-program-head-operands program))
          (extended environment)
          (index parent-index)
          (index-owned-p nil)
          (scratch (and *unification-scratch* (not (%unification-scratch-active-p *unification-scratch*)) *unification-scratch*)))
      (labels ((extend-directly (variable term)
                                (unless index-owned-p
                                  (setf index (%copy-environment-index index)
                                        index-owned-p t))
                                (let ((binding (cons variable term))) (%push-environment-index-binding binding index) (push binding extended)))
               (atomic-equal-p (left right)
                               (or (eq left right)
                                   (and (symbolp left)
                                        (symbolp right)
                                        (%same-atom-text-p left right))
                                   (equal left right)))
               (unify-operands (caller-operand callee-operand)
                               (let ((caller-value
                                      (%rule-program-operand-value
                                       caller-operand caller-variables))
                                     (callee-value
                                      (%rule-program-operand-value
                                       callee-operand callee-variables)))
                                 (cond
                                  ((consp caller-value) (values nil nil))
                                  ((logic-var-p caller-value)
                                   (multiple-value-bind (binding present-p)
                                       (%environment-index-binding caller-value index)
                                     (declare (ignore binding))
                                     (if present-p
                                         (values nil nil)
                                         (if (and (logic-var-p callee-value)
                                                  (nth-value 1
                                                             (%environment-index-binding
                                                              callee-value index)))
                                             (values nil nil)
                                             (progn
                                               (unless (eq caller-value callee-value)
                                                 (extend-directly caller-value callee-value))
                                               (values t t))))))
                                  ((logic-var-p callee-value)
                                   (multiple-value-bind (binding present-p)
                                       (%environment-index-binding callee-value index)
                                     (declare (ignore binding))
                                     (if present-p
                                         (values nil nil)
                                         (progn
                                           (extend-directly callee-value caller-value)
                                           (values t t)))))
                                  ((consp callee-value) (values nil nil))
                                  (t (values t
                                             (atomic-equal-p
                                              caller-value callee-value)))))))
        (dotimes (operand-index (length caller-operands)
                                (values extended t index))
          (let ((caller-operand (svref caller-operands operand-index))
                (callee-operand (svref callee-operands operand-index)))
            (multiple-value-bind (handled-p ok)
                (unify-operands caller-operand callee-operand)
              (cond
               ((and handled-p (not ok))
                (return-from %unify-rule-program-instruction-head
                             (values environment nil parent-index)))
               ((not handled-p)
                (unless scratch
                  (setf scratch (%make-unification-scratch)))
                (let ((*unification-scratch* scratch))
                  (multiple-value-bind (next-extended generic-ok next-index)
                      (%unify-indexed
                       (%rule-program-operand-value
                        caller-operand caller-variables)
                       (%rule-program-operand-value
                        callee-operand callee-variables)
                       extended index index-owned-p)
                    (unless generic-ok
                      (return-from %unify-rule-program-instruction-head
                                   (values environment nil parent-index)))
                    (setf extended next-extended
                          index next-index
                          index-owned-p (not (eq index parent-index)))))))))))))

  (defun %materialize-rule-program-goal (instruction variables)
    "Materialize one instruction with shared fresh variables."
    (when *rule-program-goal-materialization-count*
      (incf *rule-program-goal-materialization-count*))
    (let* ((operands (%rule-instruction-operands instruction))
           (arguments nil))
      (loop for index downfrom (1- (length operands)) to 0
            do (push (%rule-program-operand-value
                      (svref operands index) variables)
                     arguments))
      (cons (%rule-instruction-predicate instruction) arguments)))

  (defun %rule-program-instruction-left-recursive-p
      (predicate arity state)
    "Return the cached revision-scoped recursion status without making a goal."
    (let* ((rulebase (proof-state-rulebase state))
           (revision (rulebase-revision rulebase))
           (module (proof-state-module state))
           (cache (rulebase-left-recursion-analysis rulebase)))
      (multiple-value-bind (index present-p)
          (%left-recursion-scope-index cache revision module)
        (unless present-p
          (setf index
                (multiple-value-bind (adjacency reverse-adjacency nodes)
                    (%first-user-goal-adjacency state)
                  (%make-left-recursion-index-from-recursive-nodes
                   (%strongly-connected-recursive-nodes
                    (%dfs-finish-order nodes adjacency)
                    adjacency
                    reverse-adjacency))))
          (%cache-left-recursion-scope-index! cache revision module index))
        (%left-recursion-index-recursive-p index predicate arity))))

  (defun %rule-program-direct-entries (instruction variables state)
    "Return a safe immutable candidate snapshot and whether direct dispatch applies."
    (let* ((rulebase (proof-state-rulebase state))
           (module (proof-state-module state))
           (predicate (%rule-instruction-predicate instruction))
           (operands (%rule-instruction-operands instruction))
           (arity (length operands))
           (descriptor
             (%rulebase-predicate-descriptor
              rulebase module predicate arity)))
      (if (or (null descriptor)
              (%goal-solver predicate arity)
              (%foreign-goal-solver predicate arity)
              (%rulebase-predicate-property rulebase predicate arity module)
              (%rulebase-tabled-p rulebase predicate arity module)
              (and (not *depth-limited-search-p*)
                   (not (and *constraints-active-p-hook*
                             (funcall *constraints-active-p-hook*)))
                   (%rule-program-instruction-left-recursive-p
                    predicate arity state)))
          (values nil nil)
          (let ((entries
                  (if (plusp arity)
                      (%predicate-descriptor-first-argument-entries
                       descriptor
                       (%walk-term-indexed
                        (%rule-program-operand-value
                         (svref operands 0) variables)
                        (proof-state-environment-index state)))
                      (%predicate-descriptor-entries descriptor))))
            (if (every
                 (lambda (entry)
                   (%clause-template-rule-program
                    (%stored-clause-template entry)))
                 entries)
                (values entries t)
                (values nil nil))))))

  (defun %state-descending-into-rule-program-instruction
      (state bindings environment-index instruction variables cut-tag)
    "Descend into an encoded rule, materializing only a depth error payload."
    (let ((remaining (proof-state-remaining-depth state)))
      (when (eql remaining 0)
        (%raise-resource-error
         "DEPTH_LIMIT"
         (proof-state-bindings state)
         (%iso-atom "CALL")
         "explicit rule-resolution depth limit exceeded"
         :condition-type (quote prolog-depth-limit-exceeded)
         :goal (%materialize-rule-program-goal instruction variables)))
      (%state-with state
                   :bindings bindings
                   :environment-index environment-index
                   :remaining-depth (and remaining (1- remaining))
                   :cut-tag cut-tag)))

  (defun %prove-rule-program-direct-entry/k
    (instruction caller-variables entry state cut-tag succeed)
  "Try one snapshotted encoded fact or rule entry."
  (let* ((clause (%stored-clause-clause entry))
         (program
           (%clause-template-rule-program
            (%stored-clause-template entry)))
         (variable-count (%rule-program-variable-count program))
         (variables
           (if (zerop variable-count)
               #()
               (make-array variable-count :initial-element nil)))
         (goal-cache
           (make-array (length (%rule-program-body program))
                       :initial-element nil))
         (parent-bindings (proof-state-bindings state))
         (parent-index (proof-state-environment-index state)))
    (declare (dynamic-extent variables goal-cache))
    (multiple-value-bind (extended ok extended-index)
        (%unify-rule-program-instruction-head
         instruction caller-variables program variables
         parent-bindings parent-index)
      (when ok
        (%with-propagated-bindings
            (propagated propagated-index extended extended-index)
          (if (null (clause-body clause))
              (funcall succeed
                       (%state-with
                        state
                        :bindings propagated
                        :environment-index propagated-index))
              (%prove-rule-program-body/k
               program variables goal-cache 0
               (%state-descending-into-rule-program-instruction
                state propagated propagated-index
                instruction caller-variables cut-tag)
               succeed)))))))

  (defun %prove-rule-program-direct/k
      (instruction variables entries state succeed)
    "Prove one encoded local user call over a single descriptor snapshot."
    (let ((cut-tag (%make-cut-tag)))
      (cl:catch cut-tag
        (dolist (entry entries)
          (%prove-rule-program-direct-entry/k
           instruction variables entry state cut-tag succeed)))))

  (defun %prove-rule-program-instruction/k
    (instruction variables goal-cache pc state succeed)
  "Dispatch one instruction directly when the complete local snapshot is safe."
  (multiple-value-bind (entries direct-p)
      (%rule-program-direct-entries instruction variables state)
    (if direct-p
        (if (null *call-depth-limit-token*)
            (%prove-rule-program-direct/k
             instruction variables entries state succeed)
            (progn
              (when (zerop *call-depth-limit-remaining*)
                (cl:throw *call-depth-limit-token*
                          *call-depth-limit-token*))
              (let ((*call-depth-limit-remaining*
                      (1- *call-depth-limit-remaining*))
                    (*call-depth-limit-used*
                      (1+ *call-depth-limit-used*)))
                (%prove-rule-program-direct/k
                 instruction variables entries state succeed))))
        (let ((goal
                (or (svref goal-cache pc)
                    (setf (svref goal-cache pc)
                          (%materialize-rule-program-goal
                           instruction variables)))))
          (%prove-goal/k goal state succeed)))))

  (defun %prove-rule-program-body/k
    (program variables goal-cache pc state succeed)
  "Execute PROGRAM body, directly dispatching safe encoded local calls."
  (let ((body (%rule-program-body program)))
    (if (= pc (length body))
        (funcall succeed state)
        (let ((cut-tag (proof-state-cut-tag state)))
          (flet ((%continue-rule-program-body (next-state)
                   (%prove-rule-program-body/k
                    program variables goal-cache (1+ pc)
                    (%state-with next-state :cut-tag cut-tag)
                    succeed)))
            (declare (dynamic-extent (function %continue-rule-program-body)))
            (%prove-rule-program-instruction/k
             (svref body pc) variables goal-cache pc state
             (function %continue-rule-program-body)))))))

  (defun %prove-rule-program/k (goal program state cut-tag succeed)
  "Resolve GOAL using the restricted immutable rule instruction path."
  (let* ((variables
           (make-array (%rule-program-variable-count program)
                       :initial-element nil))
         (parent-bindings (proof-state-bindings state))
         (parent-index (proof-state-environment-index state)))
    (declare (dynamic-extent variables))
    (multiple-value-bind (extended ok extended-index)
        (%unify-rule-program-head
         goal program variables parent-bindings parent-index)
      (when ok
        (%with-propagated-bindings
            (propagated propagated-index extended extended-index)
          (let* ((rule-state
                   (%state-descending-into-rule
                    state propagated propagated-index goal cut-tag))
                 (goal-cache
                   (make-array
                    (length (%rule-program-body program))
                    :initial-element nil)))
            (declare (dynamic-extent goal-cache))
            (%prove-rule-program-body/k
             program variables goal-cache 0 rule-state succeed)))))))

  (defun %prove-generic-rule/k (goal entry state cut-tag succeed) "Resolve GOAL through the general graph-template rule path." (let* ((template (%stored-clause-template entry)) (stored-rule (%stored-clause-clause entry)) (context (unless (zerop (%clause-template-variable-count template)) (%make-clause-template-materialization-context template))) (head (if context (%materialize-clause-template-head template context) (clause-head stored-rule))) (parent-bindings (proof-state-bindings state)) (parent-index (proof-state-environment-index state))) (multiple-value-bind (extended ok extended-index) (%unify-indexed goal head parent-bindings parent-index nil) (when ok (%with-propagated-bindings (propagated propagated-index extended extended-index) (%prove-goals/k (if context (%materialize-clause-template-body template context) (clause-body stored-rule)) (%state-descending-into-rule state propagated propagated-index goal cut-tag) succeed))))))

  (defun %prove-rule/k (goal entry state cut-tag succeed)
    "Resolve GOAL against one stored rule; a cut in the body prunes the clause list."
    (let* ((template (%stored-clause-template entry))
           (program (%clause-template-rule-program template)))
      (if program
          (%prove-rule-program/k goal program state cut-tag succeed)
          (%prove-generic-rule/k goal entry state cut-tag succeed)))))

(defmacro %with-first-solution (block-name (continuation-var) &body body)
  "Return a one-shot success continuation for a proof search wrapped in
BLOCK-NAME: run BODY once with CONTINUATION-VAR bound to the reached
proof state, then unwind out of BLOCK-NAME with BODY's final value.

All but the last form of BODY are spliced directly into the lambda body,
so a leading (declare ...) stays legal; the last form supplies the value
passed to (return-from BLOCK-NAME ...)."
  `(lambda (,continuation-var)
     ,@(butlast body)
     (return-from ,block-name ,(car (last body)))))

(defun %provable-p (query rulebase environment depth
                    &optional (module +default-prolog-module+))
  "Return true when QUERY has at least one proof."
  ;; PROLOG-SUCCEEDS-P calls this directly rather than going through
  ;; %MAP-PROLOG-SOLUTIONS* (src/query.lisp), so it independently
  ;; participates in the same top-level-call accounting that makes dead
  ;; rulebase entries eligible for compaction -- see
  ;; %WITH-PROLOG-TOP-LEVEL-CALL and *PROLOG-ACTIVE-TOP-LEVEL-CALLS* in
  ;; data.lisp.
  (%with-prolog-top-level-call (rulebase)
    (let* ((*unification-scratch*
             (or *unification-scratch* (%make-unification-scratch)))
           (*tabled-search-active-p* nil)
           (normalized-query (%normalize-query query))
           (session (%make-rulebase-table-session rulebase))
           (cacheable-p (and (null environment)
                             (null depth)
                             (= (length normalized-query) 1)))
           (cache-key (and cacheable-p
                           (multiple-value-bind (canonical cyclic-p)
                               (%canonicalize-variant (first normalized-query))
                             (list module
                                   (if cyclic-p
                                       (%variant-graph-key canonical)
                                       canonical)))))
           (successful-queries (%table-session-successful-queries session)))
      (when (and cache-key (gethash cache-key successful-queries))
        (return-from %provable-p t))
      (%with-logic-variable-order
        (block provable
          (let ((cut-tag (%make-cut-tag)))
            (cl:catch cut-tag
              (%prove-goals/k
               normalized-query
               (%make-proof-state
                rulebase
                environment
                (%make-environment-index environment)
                depth
                module
                session
                cut-tag)
               (lambda (state)
                 (declare (cl:ignore state))
                 (when (and cache-key *tabled-search-active-p*)
                   (setf (gethash cache-key successful-queries) t))
                 (return-from provable t))))
            nil))))))
