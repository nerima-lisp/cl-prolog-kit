;;;; Static left-recursion analysis over a rulebase's first-user-goal call graph.
;;;;
;;;; Kosaraju's algorithm run over the graph whose nodes are predicate
;;;; indicators and whose edges are "this clause's body can reach that
;;;; predicate as its first user goal", plus the per-revision/per-module
;;;; membership index that caches the answer.  Split out of tabling.lisp: this
;;;; half is a pure compile-time-style property of the clause store, while the
;;;; answer cache and the tabled CPS resolution that consume it are runtime.
;;;; It loads before prover.lisp, which also queries the index directly.

(in-package #:cl-prolog-kit)

(defparameter +tabling-transparent-control-strategies+ '(("NOT" . :unary)
    ("\\+" . :unary)
    ("ONCE" . :unary)
    ("IGNORE" . :unary)
    ("CALL_NTH" . :unary)
    ("CALL_WITH_DEPTH_LIMIT" . :unary)
    ("AND" . :sequence)
    ("SETUP_CALL_CLEANUP" . :sequence)
    ("CALL_CLEANUP" . :sequence)
    ("FORALL" . :sequence)
    ("OR" . :alternatives)
    ("IF-THEN-ELSE" . :if-then-else)
    ("SOFT-IF-THEN-ELSE" . :if-then-else)
    ("CATCH" . :catch))
  "How %FIRST-USER-PREDICATE-KEYS's left-recursion analysis sees through a
control construct to the first-user-goal(s) beyond it, keyed by ISO functor
name: :UNARY looks only at the second argument; :SEQUENCE and :ALTERNATIVES
look at every remaining argument, conjunctively or disjunctively
transparent; :IF-THEN-ELSE and :CATCH thread a distinguished condition goal
through their branches. CALL is handled separately -- it resolves its
closure argument before recursing, rather than picking a fixed argument
shape.")

(defun %first-user-predicate-keys (clause)
  "Return possible first user-predicate indicators reached by CLAUSE."
  (labels ((static-call-goal (closure arguments)
             (cond
          ((and (symbolp closure) (not (logic-var-p closure))) (cons closure arguments))
          ((and (consp closure) (%goal-form-p closure)) (append closure arguments))))
           (analyze-alternatives (goals)
             (let ((keys '())
              (transparent-p nil))
          (dolist (goal goals)
            (multiple-value-bind (goal-keys goal-transparent-p) (analyze-goal goal)
              (setf keys (nconc keys goal-keys)
                    transparent-p (or transparent-p goal-transparent-p))))
          (values (remove-duplicates keys :test #'equal) transparent-p)))
           (analyze-sequence (goals)
             (if (null goals) (values nil t)
          (multiple-value-bind (keys transparent-p) (analyze-goal (first goals))
            (if transparent-p (multiple-value-bind (later-keys later-transparent-p) (analyze-sequence (rest goals))
                (values
                  (remove-duplicates (nconc keys later-keys) :test #'equal)
                  later-transparent-p))
              (values keys nil)))))
           (analyze-conditional (condition branches)
             (multiple-value-bind (keys transparent-p) (analyze-goal condition)
          (if transparent-p (multiple-value-bind (branch-keys branch-transparent-p) (analyze-alternatives branches)
              (values
                (remove-duplicates (nconc keys branch-keys) :test #'equal)
                branch-transparent-p))
            (values keys nil))))
           (analyze-goal (raw-goal)
             (let* ((goal (%ensure-goal-form raw-goal))
               (key (%predicate-key goal)))
          (cond
            ((null key) (values nil t))
            ((and
                (not (%goal-solver (car key) (cdr key)))
                (not (%foreign-goal-solver (car key) (cdr key))))
              (values (list key) nil))
            ((%foreign-goal-solver (car key) (cdr key)) (values nil t))
            (t
              (let ((name (string-upcase (symbol-name (first goal)))))
                (if (string= name "CALL") (let ((called (static-call-goal (second goal) (cddr goal))))
                    (if called (analyze-goal called)
                      (values nil t)))
                  (case (cdr (assoc name +tabling-transparent-control-strategies+ :test #'string=))
                    (:unary (analyze-goal (second goal)))
                    (:sequence (analyze-sequence (rest goal)))
                    (:alternatives (analyze-alternatives (rest goal)))
                    (:if-then-else (analyze-conditional (second goal) (cddr goal)))
                    (:catch (analyze-conditional (second goal) (list (fourth goal))))
                    (otherwise (values nil t))))))))))
    (nth-value 0 (analyze-sequence (clause-body clause)))))

(defun %first-user-goal-adjacency (state)
  "Return (VALUES ADJACENCY REVERSE-ADJACENCY NODES) for the call graph
STATE's rulebase forms among first-user-goal predicate keys."
  (let ((adjacency (make-hash-table :test #'equal))
        (reverse-adjacency (make-hash-table :test #'equal))
        (nodes '()))
    (labels ((ensure-node (key)
               (multiple-value-bind (neighbors node-present-p) (gethash key adjacency)
            (declare (ignore neighbors))
            (unless node-present-p
              (setf (gethash key adjacency) '()
                    (gethash key reverse-adjacency) '())
              (push key nodes)))))
      (dolist (entry (%proof-module-entries state))
        (let* ((clause (%stored-clause-clause entry))
               (head-key (%predicate-key (clause-head clause)))
               (successors (%first-user-predicate-keys clause)))
          (when head-key
            (ensure-node head-key)
            (dolist (successor successors)
              (ensure-node successor)
              (push successor (gethash head-key adjacency))
              (push head-key (gethash successor reverse-adjacency)))))))
    (values adjacency reverse-adjacency nodes)))

(defun %dfs-finish-order (nodes adjacency)
  "Return NODES in iterative depth-first postorder over ADJACENCY
(Kosaraju's algorithm, first pass)."
  (let ((visited (make-hash-table :test #'equal))
        (finish-order '()))
    (dolist (node nodes)
      (unless (gethash node visited)
        (let ((stack (list (cons node nil))))
          (loop while stack
                for frame = (pop stack)
                for current = (car frame)
                for expanded-p = (cdr frame)
                do (if expanded-p (push current finish-order)
              (unless (gethash current visited)
                (setf (gethash current visited) t)
                (push (cons current t) stack)
                (dolist (next (gethash current adjacency))
                  (unless (gethash next visited)
                    (push (cons next nil) stack)))))))))
    finish-order))

(defun %strongly-connected-recursive-nodes (finish-order adjacency reverse-adjacency)
  "Return a hash-table marking every node in FINISH-ORDER that belongs to a
nontrivial strongly-connected component or has a self-loop, over ADJACENCY
and its transpose REVERSE-ADJACENCY (Kosaraju's algorithm, second pass)."
  (let ((assigned (make-hash-table :test #'equal))
        (recursive (make-hash-table :test #'equal)))
    (dolist (node finish-order)
      (unless (gethash node assigned)
        (let ((component '())
              (stack (list node)))
          (loop while stack
                for current = (pop stack)
                do (unless (gethash current assigned)
              (setf (gethash current assigned) t)
              (push current component)
              (dolist (previous (gethash current reverse-adjacency))
                (unless (gethash previous assigned)
                  (push previous stack)))))
          (when (or
              (rest component)
              (member (first component) (gethash (first component) adjacency) :test #'equal))
            (dolist (member component)
              (setf (gethash member recursive) t))))))
    recursive))

(defstruct (%left-recursion-index
    (:copier nil)
    (:constructor %make-left-recursion-index (predicate-arities))) "Allocation-free membership index for one left-recursion analysis scope."
  (predicate-arities (make-hash-table :test #'eq) :type hash-table :read-only t))

(defun %make-left-recursion-index-from-recursive-nodes (recursive-nodes)
  "Build a predicate/arity membership index from RECURSIVE-NODES."
  (let ((predicate-arities (make-hash-table :test #'eq)))
    (maphash
      (lambda (key recursive-p)
        (when recursive-p
          (let ((arities
                (or
                  (gethash (car key) predicate-arities)
                  (setf (gethash (car key) predicate-arities) (make-hash-table :test #'eql)))))
            (setf (gethash (cdr key) arities) t))))
      recursive-nodes)
    (%make-left-recursion-index predicate-arities)))

(defun %left-recursion-index-recursive-p (index predicate arity)
  "Return true when PREDICATE/ARITY belongs to INDEX."
  (let ((arities (gethash predicate (%left-recursion-index-predicate-arities index))))
    (and arities (gethash arity arities))))

(defun %left-recursion-scope-index (cache revision module)
  "Return CACHE entry and presence flag for REVISION and MODULE."
  (let ((modules (gethash revision cache)))
    (if modules (gethash module modules)
      (values nil nil))))

(defun %cache-left-recursion-scope-index! (cache revision module index)
  "Store INDEX in CACHE under REVISION and MODULE."
  (let ((modules
        (or
          (gethash revision cache)
          (setf (gethash revision cache) (make-hash-table :test (function eq))))))
    (setf (gethash module modules) index)))

(defun %left-recursive-p (goal state)
  "Return true when GOAL belongs to a first-user-goal call cycle."
  (when (%goal-form-p goal)
    (let* ((rulebase (proof-state-rulebase state))
           (revision (rulebase-revision rulebase))
           (module (proof-state-module state))
           (cache (rulebase-left-recursion-analysis rulebase)))
      (multiple-value-bind (index present-p) (%left-recursion-scope-index cache revision module)
        (unless present-p
          (setf index (multiple-value-bind (adjacency reverse-adjacency nodes) (%first-user-goal-adjacency state)
              (%make-left-recursion-index-from-recursive-nodes
                (%strongly-connected-recursive-nodes
                  (%dfs-finish-order nodes adjacency)
                  adjacency
                  reverse-adjacency))))
          (%cache-left-recursion-scope-index! cache revision module index))
        (%left-recursion-index-recursive-p index (first goal) (length (rest goal)))))))
