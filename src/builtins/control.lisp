(in-package #:cl-prolog-kit)

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin ((fail false)) (rulebase environment depth emit))

(defun %resolve-callable-goal (goal environment operation)
  "Resolve GOAL in ENVIRONMENT and validate it as an ISO callable term."
  (%ensure-callable (logic-substitute goal environment)
                    environment operation))

(define-builtin ((not |\\+|) goal) (rulebase environment depth emit)
  (unless (%provable-p (%resolve-callable-goal goal environment
                                               (%iso-atom "NOT"))
                       rulebase environment depth *current-prolog-module*)
    (funcall emit environment)))

(defun %extend-callable-goal (closure arguments environment
                              &optional (operation (%iso-atom "CALL")))
  "Append ARGUMENTS to CLOSURE, returning an engine-level goal form."
  (cond
    ((logic-var-p closure)
     (%raise-instantiation-error environment operation
                                 "CALL/N requires an instantiated callable term"))
    ((symbolp closure)
     (cons closure arguments))
    ((and (consp closure) (symbolp (first closure)))
     (append closure arguments))
    (t
     (%raise-type-error "CALLABLE" closure environment operation
                        "CALL/N requires a callable atom or compound term"))))

(defun %check-callable-body (body whole environment operation)
  "Check every goal in BODY before any of it runs, per ISO 13211-1 7.8.3.

`call/1' converts its argument to a *body* first, so `call((write(3), 1))'
raises type_error(callable, (write(3), 1)) without writing anything -- the
whole body is the culprit, not the `1' inside it.  Walks the control constructs
that make up a body and stops at anything else, which is an ordinary goal."
  (labels ((check (goal)
             (cond
               ;; A variable in a body position is legitimate: conversion turns
               ;; it into `call(V)', so it is an instantiation error only if the
               ;; search actually reaches it -- `call((fail, X))' just fails.
               ((logic-var-p goal))
               ((%term-atom-p goal))
               ((not (%goal-form-p goal))
                (%raise-type-error "CALLABLE" whole environment operation
                                   "every goal in a body must be callable"))
               (t
                (let ((text (%atom-text (first goal)))
                      (arguments (rest goal)))
                  ;; Both spellings of each construct: the parser folds `,'/2
                  ;; and `;'/2 into the internal `and'/`or', while functional
                  ;; notation reaches here under the ISO names.
                  (when (member text '("," ";" "->" "*->" "and" "or"
                                       "if-then-else" "soft-if-then-else")
                                :test #'string=)
                    (mapc #'check arguments)))))))
    (check body)))

(define-builtin (call closure &rest arguments) (rulebase environment depth emit)
  (let ((resolved-closure (logic-substitute closure environment))
        (resolved-arguments
          (mapcar (lambda (argument)
                    (logic-substitute argument environment))
                  arguments)))
    (let ((goal (%extend-callable-goal resolved-closure resolved-arguments
                                       environment)))
      (%check-callable-body goal goal environment (%iso-atom "CALL"))
      (%prove-bindings/k goal rulebase environment depth emit))))

(define-builtin (once goal) (rulebase environment depth emit)
  (block first-proof
    (%prove-bindings/k
     (%resolve-callable-goal goal environment (%iso-atom "ONCE"))
     rulebase environment depth
     (%with-first-solution first-proof (extended)
       (funcall emit extended)
       nil))))

(define-builtin (call_nth goal n) (rulebase environment depth emit)
  (let* ((resolved-goal
           (%extend-callable-goal (logic-substitute goal environment)
                                  '() environment))
         (resolved-n (logic-substitute n environment)))
    (%require-bounded-integer resolved-n environment (%iso-atom "CALL_NTH")
                              "call_nth/2 solution number"
                              :minimum 1 :allow-variable t)
    (let ((count 0))
      (block requested-proof
        (%prove-bindings/k
         resolved-goal rulebase environment depth
         (lambda (extended)
           (incf count)
           (if (logic-var-p resolved-n)
               (%unify-emit n count extended emit)
               (when (= count resolved-n)
                 (funcall emit extended)
                 (return-from requested-proof nil)))))))))

(define-iso-builtin (call_with_depth_limit goal limit (result :raw)) "CALL_WITH_DEPTH_LIMIT"
  (let ((resolved-goal
          (%extend-callable-goal resolved-goal '() environment operation)))
    (%require-bounded-integer resolved-limit environment operation
                              "call_with_depth_limit/3 depth limit")
    (if (zerop resolved-limit)
        (%unify-emit result (%iso-atom "DEPTH_LIMIT_EXCEEDED")
                     environment emit)
        (let ((token (list '%call-depth-limit))
              (outer-token *call-depth-limit-token*)
              (outer-remaining *call-depth-limit-remaining*)
              (outer-used *call-depth-limit-used*)
              (outer-depth-limited-p *depth-limited-search-p*))
          (macrolet ((with-restored-limit (&body body)
                       ;; Solutions escape to the caller, so undo this goal's
                       ;; own binding of the four depth-limit variables first.
                       `(let ((*call-depth-limit-token* outer-token)
                              (*call-depth-limit-remaining* outer-remaining)
                              (*call-depth-limit-used* outer-used)
                              (*depth-limited-search-p* outer-depth-limited-p))
                          ,@body)))
            (let ((*call-depth-limit-token* token)
                  (*call-depth-limit-remaining* resolved-limit)
                  (*call-depth-limit-used* 0)
                  (*depth-limited-search-p* t))
              (when (eq token
                        (cl:catch token
                          (%prove-bindings/k
                           resolved-goal rulebase environment depth
                           (lambda (extended)
                             (let ((used *call-depth-limit-used*))
                               (with-restored-limit
                                 (%unify-emit result used extended emit)))))
                          nil))
                (with-restored-limit
                  (%unify-emit result (%iso-atom "DEPTH_LIMIT_EXCEEDED")
                               environment emit)))))))))

(defun %first-proof-environment (goal rulebase environment depth)
  "Return the first proof environment for GOAL and whether one exists."
  (let ((matched-p nil)
        (result nil))
    (block first-proof
      (%prove-bindings/k
       (logic-substitute goal environment)
       rulebase environment depth
       (%with-first-solution first-proof (extended)
         (setf matched-p t
               result extended)
         nil)))
    (values result matched-p)))

(defparameter +cleanup-deterministic-builtin-names+
  '("TRUE" "=" "UNIFY_WITH_OCCURS_CHECK" "==" "\\==")
  "Builtin functor names known to yield at most one proof, so
SETUP_CALL_CLEANUP's cleanup goal can skip its choice-point bookkeeping.")

(defun %cleanup-goal-deterministic-p (goal)
  "Return true when GOAL is a builtin that cannot yield multiple proofs."
  (let ((name (and (consp goal)
                   (symbolp (first goal))
                   (symbol-name (first goal)))))
    (or (and (symbolp goal)
             (string= (symbol-name goal) "TRUE"))
        (member name +cleanup-deterministic-builtin-names+ :test #'string=))))

(defun %call-cleanup/k (setup goal cleanup rulebase environment depth emit
                        operation)
  "Commit to SETUP, stream GOAL's proofs, then run CLEANUP exactly once.

CLEANUP failure is ignored, while a condition raised by CLEANUP propagates."
  (multiple-value-bind (setup-environment setup-succeeded-p)
      (%first-proof-environment
       (%resolve-callable-goal setup environment operation)
       rulebase environment depth)
    (when setup-succeeded-p
      (let ((cleanup-environment setup-environment)
            (cleanup-ran-p nil)
            (deterministic-goal-p (%cleanup-goal-deterministic-p goal)))
        (labels ((run-cleanup ()
                   (unless cleanup-ran-p
                     (setf cleanup-ran-p t)
                     (%first-proof-environment
                      (%resolve-callable-goal cleanup cleanup-environment
                                              operation)
                      rulebase cleanup-environment depth))))
          (unwind-protect
               (%prove-bindings/k
                (%resolve-callable-goal goal setup-environment operation)
                rulebase setup-environment depth
                (lambda (goal-environment)
                  (setf cleanup-environment goal-environment)
                  (when deterministic-goal-p
                    (run-cleanup))
                  (funcall emit goal-environment)))
            (run-cleanup)))))))

(define-builtin (setup_call_cleanup setup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k setup goal cleanup
                   rulebase environment depth emit
                   (%iso-atom "SETUP_CALL_CLEANUP")))

(define-builtin (call_cleanup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k 'true goal cleanup
                   rulebase environment depth emit
                   (%iso-atom "CALL_CLEANUP")))

(define-builtin (forall condition action) (rulebase environment depth emit)
  (let ((succeeded-p t))
    (block failed-action
      (%prove-bindings/k
       (%resolve-callable-goal condition environment (%iso-atom "FORALL"))
       rulebase environment depth
       (lambda (condition-environment)
         (unless (nth-value 1
                            (%first-proof-environment
                             (%resolve-callable-goal
                              action condition-environment
                              (%iso-atom "FORALL"))
                             rulebase condition-environment depth))
           (setf succeeded-p nil)
           (return-from failed-action)))))
    (when succeeded-p
      (funcall emit environment))))

(define-builtin (cl-prolog-kit.user-atoms::ignore goal)
    (rulebase environment depth emit)
  (multiple-value-bind (goal-environment succeeded-p)
      (%first-proof-environment
       (%resolve-callable-goal goal environment (%iso-atom "IGNORE"))
       rulebase environment depth)
    (funcall emit (if succeeded-p goal-environment environment))))

(defun %solve-if-then-else
    (condition then else rulebase environment depth caller-cut-tag emit operation)
  "Prove ISO 7.8.8's if-then-else.

The condition is opaque to cut (once semantics); the taken branch shares the
caller's cut barrier, as ISO requires."
  (multiple-value-bind (condition-environment matched-p)
      (%first-proof-environment
       (%resolve-callable-goal condition environment operation)
       rulebase environment depth)
    (let ((branch-environment
            (if matched-p condition-environment environment)))
      (%prove-with-cut-tag/k
       (%resolve-callable-goal (if matched-p then else)
                               branch-environment operation)
       rulebase branch-environment depth caller-cut-tag emit))))

(defun %solve-soft-if-then-else
    (condition then else rulebase environment depth caller-cut-tag emit operation)
  "Prove ISO 7.8.8's soft-cut if-then-else.

Unlike if-then-else the condition keeps all its solutions; the taken branch
still shares the caller's cut barrier."
  (let ((matched-p nil))
    (%prove-bindings/k
     (%resolve-callable-goal condition environment operation)
     rulebase environment depth
     (lambda (condition-environment)
       (setf matched-p t)
       (%prove-with-cut-tag/k
        (%resolve-callable-goal then condition-environment operation)
        rulebase condition-environment depth caller-cut-tag emit)))
    (unless matched-p
      (%prove-with-cut-tag/k
       (%resolve-callable-goal else environment operation)
       rulebase environment depth caller-cut-tag emit))))

;;; ISO 7.8.7's if-then, `(If -> Then)', is a control construct in its own right
;;; and not only the left half of if-then-else: it fails when If fails.  The
;;; parser produces the two-argument shape whenever no `;' follows, so without
;;; the `->'/2 and `*->'/2 entries below a bare `( Cond -> Then )' raised
;;; existence_error(procedure, (->)/2).  `fail' as the else branch is exactly
;;; ISO's reading and inherits each construct's cut and solution-count behavior.
(macrolet ((define-if-then-else-builtins (&body specifications)
             `(progn
                ,@(loop for (name arity solver operation) in specifications
                        collect
                        `(define-builtin
                             (,name condition then
                                    ,@(when (= arity 3) '(else)))
                             (rulebase environment depth emit)
                           (,solver condition then ,(if (= arity 3) 'else ''fail)
                                    rulebase environment depth *caller-cut-tag*
                                    emit (%iso-atom ,operation)))))))
  (define-if-then-else-builtins
    (if-then-else 3 %solve-if-then-else "IF_THEN_ELSE")
    (-> 2 %solve-if-then-else "IF_THEN")
    (soft-if-then-else 3 %solve-soft-if-then-else "SOFT_IF_THEN_ELSE")
    (*-> 2 %solve-soft-if-then-else "SOFT_IF_THEN")))

(define-builtin (throw ball) (rulebase environment depth emit)
  (let ((resolved-ball (logic-substitute ball environment)))
    (when (logic-var-p resolved-ball)
      (%raise-instantiation-error environment (%iso-atom "THROW")
                                  "THROW/1 requires a non-variable ball"))
    (%raise-prolog-exception resolved-ball environment)))

(define-builtin (catch goal catcher recover) (rulebase environment depth emit)
  (let ((continuation-condition nil))
    (handler-case
        (%prove-bindings/k
         (%resolve-callable-goal goal environment (%iso-atom "CATCH"))
         rulebase environment depth
         (lambda (goal-environment)
           (handler-case
               (funcall emit goal-environment)
             (prolog-exception (condition)
               (setf continuation-condition condition)
               (error condition)))))
      (prolog-exception (condition)
        (when (eq condition continuation-condition)
          (error condition))
        (multiple-value-bind (recovery-environment matched-p)
            (unify (prolog-exception-term condition)
                   (logic-substitute catcher environment)
                   environment)
          (if matched-p
              (%prove-bindings/k
               (%resolve-callable-goal recover recovery-environment
                                       (%iso-atom "CATCH"))
               rulebase recovery-environment depth emit)
              (error condition)))))))

(define-builtin (repeat) (rulebase environment depth emit)
  (loop (funcall emit environment)))

(define-builtin (halt) (rulebase environment depth emit)
  (error 'prolog-halt :code 0))

(define-builtin (halt code) (rulebase environment depth emit)
  (let ((resolved (logic-substitute code environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment (%iso-atom "HALT")
                                  "halt/1 requires an instantiated exit code"))
    (unless (integerp resolved)
      (%raise-type-error "INTEGER" resolved environment (%iso-atom "HALT")
                         "halt/1 requires an integer exit code"))
    (error 'prolog-halt :code resolved)))

(define-builtin ((and |,|) &rest goals) (rulebase environment depth emit)
  ;; GOALS is always a list of goals; normalize each element so a leading
  ;; bare atom is not mistaken for a compound goal's functor.
  (%prove-transparent/k (mapcar #'%ensure-goal-form goals)
                        rulebase environment depth emit))

(defun %if-then-arrow-goal (term)
  "Return (VALUES CONDITION THEN SOLVER) when TERM is an `->'/2 or `*->'/2 goal.

ISO 7.8.8 defines if-then-else as `;'/2 whose left argument is an arrow, so the
disjunction has to look inside its first alternative rather than treat the arrow
as an ordinary goal.  The parser folds the infix spelling into one term, but the
functional notation `;(->(C,T), E)' the standard also admits arrives here."
  (when (and (%proper-list-p term) (= 3 (length term)) (symbolp (first term)))
    (let ((text (%atom-text (first term))))
      (cond
        ((string= text "->") (values (second term) (third term)
                                     #'%solve-if-then-else))
        ((string= text "*->") (values (second term) (third term)
                                      #'%solve-soft-if-then-else))))))

(define-builtin ((or |;|) &rest alternatives) (rulebase environment depth emit)
  (let ((caller-cut-tag *caller-cut-tag*))
    (multiple-value-bind (condition then solver)
        (and (= 2 (length alternatives)) (%if-then-arrow-goal (first alternatives)))
      (if solver
          (funcall solver condition then (second alternatives)
                   rulebase environment depth caller-cut-tag emit
                   (%iso-atom "IF_THEN_ELSE"))
          (dolist (alternative alternatives)
            (%prove-with-cut-tag/k alternative rulebase environment depth
                                   caller-cut-tag emit))))))

(define-builtin (^ existential goal) (rulebase environment depth emit)
  ;; ISO 8.10.2: `^'/2 marks a variable existential inside bagof/setof; called
  ;; as an ordinary goal it is simply its right argument.
  (declare (ignore existential))
  (%prove-with-cut-tag/k (%resolve-callable-goal goal environment
                                                 (%iso-atom "^"))
                         rulebase environment depth *caller-cut-tag* emit))
