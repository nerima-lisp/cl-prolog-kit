;;;; Tabling (memoized resolution): left-recursion detection and the
;;;; declared-tabled-predicate answer cache built on top of core CPS
;;;; proof search.
(in-package #:cl-prolog-kit)

(defun %replay-table-answers/k (goal state entry succeed)
  "Unify each currently eligible answer for ENTRY with GOAL and invoke SUCCEED."
  (let ((parent-bindings (proof-state-bindings state))
        (parent-index (proof-state-environment-index state))
        (answers
        (if (%table-entry-delta-active-p entry) (%table-entry-delta-answers entry)
          (%table-entry-answers entry)))
        (answer-count
        (if (%table-entry-delta-active-p entry) (%table-entry-delta-count entry)
          (%table-entry-answer-count entry))))
    (loop repeat answer-count
          for table-answer in answers
          for answer = (%table-answer-term table-answer)
          do (multiple-value-bind (extended ok extended-index) (%unify-indexed
          goal
          (if (%table-answer-contains-variables-p table-answer) (%instantiate-variant answer (%table-answer-cyclic-p table-answer))
            answer)
          parent-bindings
          parent-index
          nil)
        (when ok
          (funcall
            succeed
            (%state-with state :bindings extended :environment-index extended-index)))))))

(defun %predicate-key (goal)
  (when (%goal-form-p goal)
    (cons (first goal) (length (rest goal)))))

(defun %record-table-answer! (entry answer cyclic-p contains-variables-p)
  "Append ANSWER to ENTRY, returning true only when it was not already tabled."
  (let ((index (if cyclic-p
                   (or (%table-entry-cyclic-answer-index entry)
                       (setf (%table-entry-cyclic-answer-index entry)
                             (make-hash-table :test (function equal))))
                   (%table-entry-answer-index entry)))
        (answer-key (if cyclic-p (%variant-graph-key answer) answer)))
    (unless (nth-value 1 (gethash answer-key index))
      (let ((cell (list (%make-table-answer answer contains-variables-p cyclic-p)))
            (tail (%table-entry-answers-tail entry)))
        (if tail
            (setf (cdr tail) cell)
            (setf (%table-entry-answers entry) cell))
        (setf (%table-entry-answers-tail entry) cell
              (gethash answer-key index) t)
        (incf (%table-entry-answer-count entry))
        t))))

(defun %table-key (state canonical-goal cyclic-goal-p)
  "Return the session table key identifying CANONICAL-GOAL variant."
  (let ((rulebase (proof-state-rulebase state)))
    (list*
      rulebase
      (rulebase-revision rulebase)
      (proof-state-module state)
      (if cyclic-goal-p (list :cyclic (%variant-graph-key canonical-goal))
        (list canonical-goal)))))

(defun %linear-direct-left-recursive-p (goal state)
  "True when relevant clauses contain at most one direct, leading recursive call."
  (let ((goal-key (%predicate-key goal)))
    (labels ((opaque-control-p (body-goal)
               (and
            (%goal-form-p body-goal)
            (let ((name (symbol-name (first body-goal))))
              (or
                (string= name "CALL")
                (assoc name +tabling-transparent-control-strategies+ :test (function string=)))))))
      (every
        (lambda (stored-clause)
          (let ((recursive-count 0)
                (recursive-position nil)
                (opaque-p nil)
                (body (clause-body (%stored-clause-clause stored-clause))))
            (loop for body-goal in body
                  for position from 0
                  do (when (opaque-control-p body-goal)
                (setf opaque-p t)) (when (%left-recursive-p body-goal state)
                (incf recursive-count)
                (setf recursive-position position)))
            (and
              (not opaque-p)
              (or
                (zerop recursive-count)
                (and
                  (= recursive-count 1)
                  (zerop recursive-position)
                  (equal goal-key (%predicate-key (first body))))))))
        (%proof-predicate-entries goal state)))))

(defun %prove-tabled/k (goal state key entries succeed)
  "Build the answer table for GOAL at KEY by iterating to a fixpoint. A non-local exit before completion discards the partial table."
  (let ((*tabled-search-active-p* t))
    (let ((entry (%make-table-entry))
          (completed-p nil))
      (setf (gethash key entries) entry)
      (unwind-protect (progn
          (loop with changed-p
                with delta-p = (and (%left-recursive-p goal state) (%linear-direct-left-recursive-p goal state))
                with boundary-count = 0
                with boundary-tail = nil
                do (let ((current-count (%table-entry-answer-count entry))
                  (current-tail (%table-entry-answers-tail entry)))
              (setf changed-p nil
                    (%table-entry-delta-active-p entry) delta-p)
              (when delta-p
                (setf (%table-entry-delta-answers entry) (if boundary-tail (cdr boundary-tail)
                    (%table-entry-answers entry))
                      (%table-entry-delta-count entry) (- current-count boundary-count)
                      boundary-count current-count
                      boundary-tail current-tail))) (%prove-raw-clauses/k
              goal
              state
              (lambda (answer-state)
                (multiple-value-bind (answer cyclic-answer-p contains-variables-p) (%canonicalize-variant goal (proof-state-environment-index answer-state))
                  (when (%record-table-answer! entry answer cyclic-answer-p contains-variables-p)
                    (setf changed-p t)
                    (funcall succeed answer-state)))))
                while changed-p)
          (setf completed-p t))
        (setf (%table-entry-delta-active-p entry) nil
              (%table-entry-delta-answers entry) (quote ())
              (%table-entry-delta-count entry) 0)
        (unless completed-p
          (remhash key entries))))))

(defun %prove-clauses/k (goal state succeed)
  "Prove GOAL, tabling declared predicates and detected left recursion."
  (if (or
      *depth-limited-search-p*
      (and *constraints-active-p-hook* (funcall *constraints-active-p-hook*))
      (not
        (or
          (%rulebase-tabled-p
            (proof-state-rulebase state)
            (first goal)
            (length (rest goal))
            (proof-state-module state))
          (%left-recursive-p goal state)))) (%prove-raw-clauses/k goal state succeed)
    (let ((entries (%table-session-entries (proof-state-table-session state))))
      (multiple-value-bind (canonical-goal cyclic-goal-p) (%canonicalize-variant goal (proof-state-environment-index state))
        (let* ((key (%table-key state canonical-goal cyclic-goal-p))
               (entry (gethash key entries)))
          (if entry (%replay-table-answers/k goal state entry succeed)
            (%prove-tabled/k goal state key entries succeed)))))))
