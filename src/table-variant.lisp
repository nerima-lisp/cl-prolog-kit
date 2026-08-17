;;;; Tabling data: the %table-entry/%table-session structs recording
;;;; memoized answers, and variant canonicalization (%canonicalize-variant,
;;;; %variant-graph-key, %instantiate-variant) used to key and replay them.
(in-package #:cl-prolog-kit)

(defstruct (%table-entry (:copier nil) (:constructor %make-table-entry ())) "Variant-call answers accumulated during one tabled proof."
  (answers (quote ()) :type list)
  (answers-tail (quote ()) :type list)
  (answer-count 0 :type (integer 0 *))
  (answer-index
    (make-hash-table :test (function equal))
    :type
    hash-table
    :read-only
    t)
  (cyclic-answer-index nil :type (or null hash-table))
  (delta-answers (quote ()) :type list)
  (delta-count 0 :type (integer 0 *))
  (delta-active-p nil :type boolean))

(defstruct (%table-answer (:copier nil) (:constructor %make-table-answer (term contains-variables-p cyclic-p))) "An immutable canonical answer and the replay metadata needed to instantiate it." (term nil :read-only t) (contains-variables-p nil :type boolean :read-only t) (cyclic-p nil :type boolean :read-only t))

(defstruct (%table-session
    (:copier nil)
    (:constructor
      %make-table-session
      (entries module-entries predicate-entries successful-queries))) "Tables shared by every proof nested within one rulebase revision."
  (entries (make-hash-table :test (function equal)) :type hash-table :read-only t)
  (module-entries
    (make-hash-table :test (function equal))
    :type
    hash-table
    :read-only
    t)
  (predicate-entries
    (make-hash-table :test (function equal))
    :type
    hash-table
    :read-only
    t)
  (successful-queries
    (make-hash-table :test (function equal))
    :type
    hash-table
    :read-only
    t))

(defparameter +variant-variable-marker+ (gensym "VARIANT-VARIABLE-")
  "Unforgeable marker used in canonical table keys and answers.")

(defun %make-rulebase-table-session (rulebase)
  "Return the current revision's shared table session for RULEBASE."
  (let* ((revision (rulebase-revision rulebase))
         (cache (rulebase-table-session-cache rulebase)))
    (multiple-value-bind (session present-p) (gethash revision cache)
      (if present-p session
        (setf (gethash revision cache) (%make-table-session
            (make-hash-table :test (function equal))
            (make-hash-table :test (function equal))
            (make-hash-table :test (function equal))
            (make-hash-table :test (function equal))))))))

(defun %canonicalize-variant (term &optional environment-index)
  "Rename TERM variables by first occurrence after resolving ENVIRONMENT-INDEX.
The second value reports whether the resolved graph contains a cons cycle; the
third reports whether its canonical form contains logical variable markers."
  (when (null environment-index)
    (cond
      ((atom term)
       (return-from %canonicalize-variant (values term nil nil)))
      ((and (not (%term-has-variables-p term))
            (%term-acyclic-p term))
       (return-from %canonicalize-variant (values term nil nil)))))
  (let ((variables nil)
        (inline-node-0 nil)
        (inline-copy-0 nil)
        (inline-node-1 nil)
        (inline-copy-1 nil)
        (inline-node-2 nil)
        (inline-copy-2 nil)
        (inline-node-3 nil)
        (inline-copy-3 nil)
        (inline-copy-count 0)
        (copy-table nil)
        (next-index 0)
        (cyclic-p nil)
        (contains-variables-p nil))
    (labels ((resolve (node)
               (if (and environment-index (logic-var-p node))
                   (%walk-term-indexed node environment-index)
                   node))
             (find-copy (node)
               (if copy-table
                   (gethash node copy-table)
                   (cond
                     ((and (> inline-copy-count 0) (eq node inline-node-0))
                      (values inline-copy-0 t))
                     ((and (> inline-copy-count 1) (eq node inline-node-1))
                      (values inline-copy-1 t))
                     ((and (> inline-copy-count 2) (eq node inline-node-2))
                      (values inline-copy-2 t))
                     ((and (> inline-copy-count 3) (eq node inline-node-3))
                      (values inline-copy-3 t)))))
             (remember-copy (node copy)
               (case inline-copy-count
                 (0
                  (setf inline-node-0 node
                        inline-copy-0 copy)
                  (incf inline-copy-count))
                 (1
                  (setf inline-node-1 node
                        inline-copy-1 copy)
                  (incf inline-copy-count))
                 (2
                  (setf inline-node-2 node
                        inline-copy-2 copy)
                  (incf inline-copy-count))
                 (3
                  (setf inline-node-3 node
                        inline-copy-3 copy)
                  (incf inline-copy-count))
                 (otherwise
                  (unless copy-table
                    (setf copy-table (make-hash-table :test (function eq)))
                    (setf (gethash inline-node-0 copy-table) inline-copy-0
                          (gethash inline-node-1 copy-table) inline-copy-1
                          (gethash inline-node-2 copy-table) inline-copy-2
                          (gethash inline-node-3 copy-table) inline-copy-3))
                  (setf (gethash node copy-table) copy))))
             (canonicalize (term &optional active)
               (let ((node (resolve term)))
                 (cond
                   ((logic-var-p node)
                    (setf contains-variables-p t)
                    (let ((table
                            (or variables
                                (setf variables
                                      (make-hash-table :test (function eq))))))
                      (or (gethash node table)
                          (setf (gethash node table)
                                (list +variant-variable-marker+
                                      (prog1 next-index
                                        (incf next-index)))))))
                   ((consp node)
                    (multiple-value-bind (copy present-p) (find-copy node)
                      (if present-p
                          (progn
                            (when (member node active :test (function eq))
                              (setf cyclic-p t))
                            copy)
                          (let ((copy (cons nil nil))
                                (active (cons node active)))
                            (remember-copy node copy)
                            (setf (car copy) (canonicalize (car node) active)
                                  (cdr copy) (canonicalize (cdr node) active))
                            copy))))
                   (t node)))))
      (values (canonicalize term)
              cyclic-p
              contains-variables-p))))

(defun %variant-graph-key (term)
  "Return an EQUAL-safe encoding of TERM's cons graph."
  (let ((identities (make-hash-table :test #'eq))
        (next-index 0))
    (labels ((encode (node)
               (if (consp node) (multiple-value-bind (index present-p) (gethash node identities)
              (if present-p (list :reference index)
                (let ((index
                      (prog1
                        next-index
                        (incf next-index))))
                  (setf (gethash node identities) index)
                  (list :cons index (encode (car node)) (encode (cdr node))))))
            (list :atom node))))
      (encode term))))

(defun %instantiate-variant (term &optional (cyclic-p t))
  "Replace canonical variable markers in TERM with fresh logic variables.
CYCLIC-P selects graph-safe copying; acyclic terms use a lower-allocation
copy-on-write traversal. Ground subtrees are shared because unification never
mutates terms."
  (let ((first-marker nil)
        (first-variable nil)
        (variables nil)
        (copies (and cyclic-p (make-hash-table :test (function eq)))))
    (labels ((marker-variable (node)
               (cond
                 (variables
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (fresh-logic-variable "?TABLE"))))
                 ((null first-marker)
                  (setf first-marker node
                        first-variable (fresh-logic-variable "?TABLE")))
                 ((equal node first-marker)
                  first-variable)
                 (t
                  (setf variables
                        (make-hash-table :test (function equal)))
                  (setf (gethash first-marker variables) first-variable
                        (gethash node variables)
                        (fresh-logic-variable "?TABLE")))))
             (instantiate-tree (node)
               (cond
                 ((and (consp node)
                       (eq (first node) +variant-variable-marker+)
                       (consp (rest node))
                       (null (cddr node)))
                  (marker-variable node))
                 ((consp node)
                  (let ((new-car (instantiate-tree (car node)))
                        (new-cdr (instantiate-tree (cdr node))))
                    (if (and (eq new-car (car node))
                             (eq new-cdr (cdr node)))
                        node
                        (cons new-car new-cdr))))
                 (t node)))
             (instantiate-graph (node)
               (cond
                 ((and (consp node)
                       (eq (first node) +variant-variable-marker+)
                       (consp (rest node))
                       (null (cddr node)))
                  (marker-variable node))
                 ((consp node)
                  (multiple-value-bind (copy present-p)
                      (gethash node copies)
                    (if present-p
                        copy
                        (let ((copy (cons nil nil)))
                          (setf (gethash node copies) copy)
                          (let ((new-car (instantiate-graph (car node)))
                                (new-cdr (instantiate-graph (cdr node))))
                            (setf (car copy) new-car
                                  (cdr copy) new-cdr)
                            (if (and (eq new-car (car node))
                                     (eq new-cdr (cdr node)))
                                (progn
                                  (setf (gethash node copies) node)
                                  node)
                                copy))))))
                 (t node))))
      (if cyclic-p
          (instantiate-graph term)
          (instantiate-tree term)))))
