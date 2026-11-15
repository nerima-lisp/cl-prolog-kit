;;;; Tabling data: the %table-entry/%table-session structs recording
;;;; memoized answers, and variant canonicalization (%canonicalize-variant,
;;;; %variant-graph-key, %instantiate-variant) used to key and replay them.
(in-package #:cl-prolog-kit)

(defconstant +term-hash-modulus+ (expt 2 56))

(defconstant +term-hash-tree-walk-limit+ 65536
  "Nodes %TERM-STRUCTURE-HASH walks as a tree before it hashes each
distinct cons once instead.")

(defun %term-structure-hash (term)
  "Return an EQUAL-consistent hash of the whole cons structure of TERM.

SXHASH looks at only the first few levels of a cons tree, so canonical
goals and answers that differ only deep down (nat(s(s(...)))) all share one
bucket and every lookup compares against all of them.  The hash is a
polynomial over the preorder sequence of the tree's nodes, so it depends on
the tree alone.  It is computed by walking that tree, which allocates only a
stack of pending cdrs.  A term walked past +TERM-HASH-TREE-WALK-LIMIT+ nodes
may share subterms: f(T, T) nested n times stands for a tree of 2^n nodes,
none of whose steps are charged to the inference budget.  Such a term is
hashed again by %TERM-STRUCTURE-HASH/MEMO, which yields the same value from
each distinct cons once.  TERM must be acyclic; cyclic tables are keyed by
%VARIANT-GRAPH-KEY encodings."
  (let ((hash 0)
        (budget +term-hash-tree-walk-limit+)
        (pending '())
        (node term))
    (declare (type (integer 0 (#.(expt 2 56))) hash)
             (type fixnum budget))
    (loop
      (when (minusp (decf budget))
        (return (%term-structure-hash/memo term)))
      (cond
        ((consp node)
         (setf hash (mod (+ (* hash 31) 7) +term-hash-modulus+))
         (push (cdr node) pending)
         (setf node (car node)))
        (t
         (setf hash (mod (+ (* hash 31) (mod (sxhash node) +term-hash-modulus+))
                         +term-hash-modulus+))
         (if pending
             (setf node (pop pending))
             (return hash)))))))

(defun %term-structure-hash/memo (term)
  "%TERM-STRUCTURE-HASH of TERM, from each distinct cons once.

A node's part of the polynomial is a pair (VALUE . WEIGHT), WEIGHT being 31
to the number of nodes in its tree; the pairs of a cons, its car and its
cdr concatenate as the preorder sequences do."
  (flet ((leaf (node)
           (cons (mod (sxhash node) +term-hash-modulus+) 31))
         (concatenate-parts (left right)
           (cons (mod (+ (* (car left) (cdr right)) (car right)) +term-hash-modulus+)
                 (mod (* (cdr left) (cdr right)) +term-hash-modulus+))))
    (if (not (consp term))
        (car (leaf term))
        (let ((memo (make-hash-table :test (function eq)))
              (pending (list term)))
          (flet ((known (node)
                   (if (consp node) (gethash node memo) (leaf node))))
            (loop while pending
                  do (let* ((node (first pending))
                            (car-part (known (car node)))
                            (cdr-part (known (cdr node))))
                       (cond
                         ((gethash node memo) (pop pending))
                         ((null car-part) (push (car node) pending))
                         ((null cdr-part) (push (cdr node) pending))
                         (t (setf (gethash node memo)
                                  (concatenate-parts
                                   (concatenate-parts (cons 7 31) car-part)
                                   cdr-part))
                            (pop pending)))))
            (car (gethash term memo)))))))

(defun %make-term-hash-table ()
  "Return an EQUAL hash table hashed by %TERM-STRUCTURE-HASH."
  (make-hash-table :test (function equal)
                   :hash-function (function %term-structure-hash)))

(defstruct (%table-entry (:copier nil) (:constructor %make-table-entry ())) "Variant-call answers accumulated during one tabled proof."
  (answers (quote ()) :type list)
  (answers-tail (quote ()) :type list)
  (answer-count 0 :type (integer 0 *))
  (answer-index
    (%make-term-hash-table)
    :type
    hash-table
    :read-only
    t)
  (cyclic-answer-index nil :type (or null hash-table))
  (delta-answers (quote ()) :type list)
  (delta-count 0 :type (integer 0 *))
  (delta-active-p nil :type boolean)
  ;; The PHRASE-NEXT-TERMINALS probe phase the entry was created in, if any.
  (probe-phase nil))

(defstruct (%table-answer (:copier nil) (:constructor %make-table-answer (term contains-variables-p cyclic-p))) "An immutable canonical answer and the replay metadata needed to instantiate it." (term nil :read-only t) (contains-variables-p nil :type boolean :read-only t) (cyclic-p nil :type boolean :read-only t))

(defstruct (%table-session
    (:copier nil)
    (:constructor
      %make-table-session
      (entries module-entries predicate-entries successful-queries))) "Tables shared by every proof nested within one rulebase revision."
  (entries (%make-term-hash-table) :type hash-table :read-only t)
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
            (%make-term-hash-table)
            (make-hash-table :test (function equal))
            (make-hash-table :test (function equal))
            (%make-term-hash-table)))))))

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
                          (let ((head (cons nil nil)))
                            (remember-copy node head)
                            ;; Recurse on cars only; walk the cdr spine in
                            ;; place.  ACTIVE grows along the spine exactly as
                            ;; the recursive cdr calls extended it.
                            (loop with cell = head
                                  with source = node
                                  with path = (cons node active)
                                  do (setf (car cell) (canonicalize (car source) path))
                                     (let ((next (resolve (cdr source))))
                                       (if (not (consp next))
                                           (progn
                                             (setf (cdr cell) (canonicalize next path))
                                             (return))
                                           (multiple-value-bind (shared seen-p)
                                               (find-copy next)
                                             (if seen-p
                                                 (progn
                                                   (when (member next path
                                                                 :test (function eq))
                                                     (setf cyclic-p t))
                                                   (setf (cdr cell) shared)
                                                   (return))
                                                 (let ((fresh (cons nil nil)))
                                                   (remember-copy next fresh)
                                                   (setf (cdr cell) fresh
                                                         cell fresh
                                                         source next
                                                         path (cons next path))))))))
                            head))))
                   (t node)))))
      (values (canonicalize term)
              cyclic-p
              contains-variables-p))))

(defun %variant-graph-key (term)
  "Return an EQUAL-safe encoding of TERM's cons graph."
  (let ((identities (make-hash-table :test #'eq))
        (next-index 0))
    (labels ((register (node)
               (setf (gethash node identities)
                     (prog1 next-index (incf next-index))))
             (encode (node)
               (if (consp node)
                   (multiple-value-bind (index present-p) (gethash node identities)
                     (if present-p
                         (list :reference index)
                         ;; The encoding nests each cdr as the fourth element.
                         ;; Build that chain in place, recursing on cars only.
                         (let ((root (list :cons (register node) nil nil)))
                           (loop with encoded = root
                                 with source = node
                                 do (setf (third encoded) (encode (car source)))
                                    (let ((next (cdr source)))
                                      (if (or (not (consp next))
                                              (nth-value 1 (gethash next identities)))
                                          (progn
                                            (setf (fourth encoded) (encode next))
                                            (return))
                                          (let ((fresh (list :cons (register next) nil nil)))
                                            (setf (fourth encoded) fresh
                                                  encoded fresh
                                                  source next)))))
                           root)))
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
    (labels ((marker-p (node)
               (and (consp node)
                    (eq (first node) +variant-variable-marker+)
                    (consp (rest node))
                    (null (cddr node))))
             (marker-variable (node)
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
                  ;; Recurse on cars only.  Walk the spine forward, then rebuild
                  ;; it backward so an unchanged suffix stays shared, as the
                  ;; recursive form did.
                  (let ((spine '())
                        (new-cars '())
                        (tail nil))
                    (loop with source = node
                          do (push source spine)
                             (push (instantiate-tree (car source)) new-cars)
                             (let ((next (cdr source)))
                               (if (and (consp next) (not (marker-p next)))
                                   (setf source next)
                                   (progn (setf tail (instantiate-tree next))
                                          (return)))))
                    (loop with result = tail
                          for source in spine
                          for new-car in new-cars
                          do (setf result
                                   (if (and (eq new-car (car source))
                                            (eq result (cdr source)))
                                       source
                                       (cons new-car result)))
                          finally (return result))))
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
                        ;; Recurse on cars only.  Walk the spine forward,
                        ;; registering each copy before its car is visited, then
                        ;; finish the cells backward, deepest first, as the
                        ;; recursive form returned.
                        (let ((spine '())
                              (tail nil))
                          (loop with source = node
                                do (let ((copy (cons nil nil)))
                                     (setf (gethash source copies) copy)
                                     (push (list source copy
                                                 (instantiate-graph (car source)))
                                           spine))
                                   (let ((next (cdr source)))
                                     (if (and (consp next)
                                              (not (marker-p next))
                                              (not (nth-value 1 (gethash next copies))))
                                         (setf source next)
                                         (progn (setf tail (instantiate-graph next))
                                                (return)))))
                          (loop with result = tail
                                for (source copy new-car) in spine
                                do (setf (car copy) new-car
                                         (cdr copy) result
                                         result
                                         (if (and (eq new-car (car source))
                                                  (eq result (cdr source)))
                                             (setf (gethash source copies) source)
                                             copy))
                                finally (return result))))))
                 (t node))))
      (if cyclic-p
          (instantiate-graph term)
          (instantiate-tree term)))))
