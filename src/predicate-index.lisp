;;;; Predicate lookup layer: the per-predicate index buckets keyed by
;;;; (module predicate arity) and the immutable current-state descriptors
;;;; that provide first-argument indexing.  Operates on stored clauses from
;;;; clause.lisp; the rulebase that owns these tables lives in data.lisp.
(in-package #:cl-prolog-kit)

(defstruct (%predicate-descriptor
    (:copier nil)
    (:constructor
      %make-predicate-descriptor
      (entries symbol-first-argument-index atom-first-argument-index variable-entries))) "Immutable current-state lookup data for one predicate."
  (entries (quote ()) :type list :read-only t)
  (symbol-first-argument-index
    (make-hash-table :test (function equal))
    :type hash-table :read-only t)
  (atom-first-argument-index
    (make-hash-table :test (function eql))
    :type hash-table :read-only t)
  (variable-entries (quote ()) :type list :read-only t))

(defun %stored-clause-predicate-key (entry)
  "Return ENTRY's (module predicate arity) key, or NIL for a malformed head."
  (let ((head (clause-head (%stored-clause-clause entry))))
    (when (and (consp head) (symbolp (first head)))
      (list (%stored-clause-module entry) (first head) (length (rest head))))))

(defun %append-to-predicate-index-tail! (key entry index tails)
  "Append ENTRY to KEY's bucket in INDEX using TAILS in O(1)."
  (let ((cell (list entry))
        (tail (gethash key tails)))
    (if tail (setf (cdr tail) cell)
      (setf (gethash key index) cell))
    (setf (gethash key tails) cell)))

(defun %insert-index-entry! (key entry position index tails)
  "Insert ENTRY into KEY's bucket at POSITION while maintaining its tail."
  (ecase position
    (:first
      (let* ((entries (gethash key index))
             (cell (cons entry entries)))
        (setf (gethash key index) cell)
        (unless entries
          (setf (gethash key tails) cell))))
    (:last (%append-to-predicate-index-tail! key entry index tails))))

(defun %stored-clause-first-argument (entry)
  (let ((head (clause-head (%stored-clause-clause entry))))
    (and (consp (rest head)) (second head))))

(defun %make-rulebase-predicate-index (entries)
  "Build the predicate index for ENTRIES."
  (let ((predicate-index (make-hash-table :test #'equal))
        (predicate-tails (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (%stored-clause-predicate-key entry)))
        (when key
          (%append-to-predicate-index-tail! key entry predicate-index predicate-tails))))
    (values predicate-index predicate-tails)))

(defun %set-rulebase-predicate-descriptor! (descriptors module predicate arity descriptor)
  "Install DESCRIPTOR in the nested MODULE/PREDICATE/ARITY index."
  (let ((predicates (gethash module descriptors)))
    (cond
      (descriptor
        (unless predicates
          (setf predicates (make-hash-table :test #'eq)
                (gethash module descriptors) predicates))
        (let ((arities (gethash predicate predicates)))
          (unless arities
            (setf arities (make-hash-table :test #'eql)
                  (gethash predicate predicates) arities))
          (setf (gethash arity arities) descriptor)))
      (predicates
        (let ((arities (gethash predicate predicates)))
          (when arities
            (remhash arity arities)
            (when (zerop (hash-table-count arities))
              (remhash predicate predicates))
            (when (zerop (hash-table-count predicates))
              (remhash module descriptors)))))))
  descriptor)

(defun %build-predicate-descriptor (entries)
  "Build detached current-state lookup lists for ENTRIES."
  (let ((owned-entries (copy-list entries))
        (symbol-candidates (make-hash-table :test (function equal)))
        (atom-candidates (make-hash-table :test (function eql)))
        (variable-entries (quote ()))
        (position 0))
    (dolist (entry owned-entries)
      (let ((candidate (cons position entry))
            (first-argument (%stored-clause-first-argument entry)))
        (incf position)
        (cond
          ((logic-var-p first-argument) (push candidate variable-entries))
          ((symbolp first-argument)
            (push candidate (gethash (%atom-text first-argument) symbol-candidates)))
          ((or (numberp first-argument) (characterp first-argument))
            (push candidate (gethash first-argument atom-candidates))))))
    (setf variable-entries (nreverse variable-entries))
    (labels ((merge-candidates (exact-candidates)
               (loop with exact = (nreverse exact-candidates)
                with variable = variable-entries
                while (or exact variable)
                if (or (null variable) (and exact (< (caar exact) (caar variable))))
                  collect (cdr (pop exact))
                else
                  collect (cdr (pop variable))))
             (finish-index (index)
               (loop
                 for key being the hash-keys of index
                   using (hash-value exact-candidates)
                 do (setf (gethash key index)
                          (merge-candidates exact-candidates)))
               index))
      (%make-predicate-descriptor
        owned-entries
        (finish-index symbol-candidates)
        (finish-index atom-candidates)
        (mapcar (function cdr) variable-entries)))))

(defun %make-rulebase-predicate-descriptors (predicate-index revision)
  "Build the nested current-state descriptor index at REVISION."
  (let ((descriptors (make-hash-table :test #'eq)))
    (loop
      for key being the hash-keys of predicate-index
        using (hash-value entries)
      do
         (destructuring-bind (module predicate arity) key
           (let ((visible-entries
                   (%visible-stored-clauses entries revision)))
             (when visible-entries
               (%set-rulebase-predicate-descriptor!
                 descriptors module predicate arity
                 (%build-predicate-descriptor visible-entries))))))
    descriptors))

(defun %predicate-descriptor-first-argument-entries (descriptor first-argument)
  "Return precomputed candidates for FIRST-ARGUMENT without mutating indexes."
  (cond
    ((logic-var-p first-argument) (%predicate-descriptor-entries descriptor))
    ((symbolp first-argument)
      (or
        (gethash
          (%atom-text first-argument)
          (%predicate-descriptor-symbol-first-argument-index descriptor))
        (%predicate-descriptor-variable-entries descriptor)))
    ((or (numberp first-argument) (characterp first-argument))
      (or
        (gethash
          first-argument
          (%predicate-descriptor-atom-first-argument-index descriptor))
        (%predicate-descriptor-variable-entries descriptor)))
    (t (%predicate-descriptor-entries descriptor))))
