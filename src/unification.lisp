;;;; Unification and substitution.
;;;;
;;;; Environments are association lists mapping logic variables to terms.
;;;; They are persistent: UNIFY never mutates an environment, it extends it,
;;;; so backtracking is simply "keep using the older environment".
;;;; Variable identity lives in logic-variable.lisp and the environment
;;;; index it unifies against lives in environment-index.lisp.

(in-package #:cl-prolog-kit)

(defconstant +occurs-linear-threshold+ 8)
(defconstant +unification-pair-primary-threshold+ 8)
(defconstant +unification-pair-linear-threshold+ 16)

  (defstruct (%unification-scratch (:constructor %make-unification-scratch ())) (pair-seen nil :type (or null simple-vector)) (pair-seen-secondary nil :type (or null simple-vector)) (pair-count 0 :type fixnum) (pair-hash-mode-p nil :type boolean) (first-index nil :type (or null hash-table)) (collision-index nil :type (or null hash-table)) (occurs-seen nil :type (or null simple-vector)) (occurs-count 0 :type fixnum) (occurs-index nil :type (or null hash-table)) (active-p nil :type boolean))
  (declaim
    (inline
      %clear-occurs-scratch
      %occurs-scratch-remember-p))


(defun %clear-occurs-scratch (scratch)
  (let ((seen (%unification-scratch-occurs-seen scratch))
        (count (%unification-scratch-occurs-count scratch))
        (index (%unification-scratch-occurs-index scratch)))
    (when seen
      (dotimes (position count)
        (setf (svref seen position) nil)))
    (when index
      (clrhash index))
    (setf (%unification-scratch-occurs-count scratch) 0))
  nil)
(defun %occurs-scratch-remember-p (scratch node)
  (let ((index (%unification-scratch-occurs-index scratch)))
    (if index
        (if (gethash node index)
            t
            (progn
              (setf (gethash node index) t)
              nil))
        (let* ((seen
                 (or (%unification-scratch-occurs-seen scratch)
                     (setf (%unification-scratch-occurs-seen scratch)
                           (make-array +occurs-linear-threshold+
                                       :initial-element nil))))
               (count (%unification-scratch-occurs-count scratch)))
          (cond
            ((loop for position below count
                   thereis (eq node (svref seen position)))
             t)
            ((< count +occurs-linear-threshold+)
             (setf (svref seen count) node
                   (%unification-scratch-occurs-count scratch) (1+ count))
             nil)
            (t
             (let ((new-index (make-hash-table :test (function eq))))
               (dotimes (position count)
                 (setf (gethash (svref seen position) new-index) t))
               (setf (gethash node new-index) t
                     (%unification-scratch-occurs-index scratch) new-index)
               nil)))))))
(defun %occurs-p-indexed (var term index &optional (scratch (%make-unification-scratch)) term-resolved-p)
  (%clear-occurs-scratch scratch)
  (unwind-protect
      (labels ((occurs-p (node &optional resolved-p)
                 (let ((resolved (if resolved-p
                                     node
                                     (%walk-term-indexed node index))))
                   (cond
                     ((eq var resolved) t)
                     ((not (consp resolved)) nil)
                     ((%occurs-scratch-remember-p scratch resolved) nil)
                     (t (or (occurs-p (car resolved))
                            (occurs-p (cdr resolved))))))))
        (occurs-p term term-resolved-p))
    (%clear-occurs-scratch scratch)))
(defun %occurs-p (var term env)
  (%occurs-p-indexed var term (%make-environment-index env)))

  (defvar *unification-scratch* nil)
  (declaim (inline %reset-unification-scratch))
  (defun %unification-scratch-remember-pair-hashed (scratch left right) (let ((first-index (or (%unification-scratch-first-index scratch) (setf (%unification-scratch-first-index scratch) (make-hash-table :test (function eq)))))) (declare (type hash-table first-index)) (multiple-value-bind (first-right present-p) (gethash left first-index) (cond ((not present-p) (setf (gethash left first-index) right) nil) ((eq right first-right) t) (t (let ((collision-index (%unification-scratch-collision-index scratch))) (unless collision-index (setf collision-index (make-hash-table :test (function eq)) (%unification-scratch-collision-index scratch) collision-index)) (let ((extras (gethash left collision-index))) (cond ((hash-table-p extras) (if (gethash right extras) t (progn (setf (gethash right extras) t) nil))) ((member right extras :test (function eq)) t) ((>= (length extras) 4) (let ((right-index (make-hash-table :test (function eq)))) (setf (gethash first-right right-index) t) (dolist (extra extras) (setf (gethash extra right-index) t)) (setf (gethash right right-index) t (gethash left collision-index) right-index) nil)) (t (setf (gethash left collision-index) (cons right extras)) nil)))))))))
  (defun %unification-scratch-promote-pairs (scratch primary secondary count left right) (setf (%unification-scratch-pair-hash-mode-p scratch) t) (dotimes (position count) (let* ((secondary-p (>= position +unification-pair-primary-threshold+)) (segment (if secondary-p secondary primary)) (segment-position (if secondary-p (- position +unification-pair-primary-threshold+) position)) (offset (* 2 segment-position))) (%unification-scratch-remember-pair-hashed scratch (svref segment offset) (svref segment (1+ offset))))) (%unification-scratch-remember-pair-hashed scratch left right))
  (defun %unification-scratch-remember-pair (scratch left right) "Return true for a remembered directed EQ pair; otherwise remember it." (declare (type %unification-scratch scratch) (optimize (speed 3) (safety 1))) (if (%unification-scratch-pair-hash-mode-p scratch) (%unification-scratch-remember-pair-hashed scratch left right) (let* ((primary (or (%unification-scratch-pair-seen scratch) (setf (%unification-scratch-pair-seen scratch) (make-array (* 2 +unification-pair-primary-threshold+) :initial-element nil)))) (secondary (%unification-scratch-pair-seen-secondary scratch)) (count (%unification-scratch-pair-count scratch)) (primary-count (min count +unification-pair-primary-threshold+)) (secondary-count (- count primary-count))) (cond ((or (loop for position below primary-count for offset = (* 2 position) thereis (and (eq left (svref primary offset)) (eq right (svref primary (1+ offset))))) (and secondary (loop for position below secondary-count for offset = (* 2 position) thereis (and (eq left (svref secondary offset)) (eq right (svref secondary (1+ offset))))))) t) ((< count +unification-pair-linear-threshold+) (if (< count +unification-pair-primary-threshold+) (let ((offset (* 2 count))) (setf (svref primary offset) left (svref primary (1+ offset)) right)) (let* ((secondary (or secondary (setf (%unification-scratch-pair-seen-secondary scratch) (make-array (* 2 +unification-pair-primary-threshold+) :initial-element nil)))) (offset (* 2 (- count +unification-pair-primary-threshold+)))) (setf (svref secondary offset) left (svref secondary (1+ offset)) right))) (setf (%unification-scratch-pair-count scratch) (1+ count)) nil) (t (%unification-scratch-promote-pairs scratch primary secondary count left right))))))
  (progn
  (defun %reset-unification-scratch (scratch)
    "Release references retained by SCRATCH and make it reusable."
    (declare (type %unification-scratch scratch)
             (optimize (speed 3) (safety 1)))
    (let* ((primary (%unification-scratch-pair-seen scratch))
           (secondary (%unification-scratch-pair-seen-secondary scratch))
           (count (%unification-scratch-pair-count scratch))
           (primary-count (min count +unification-pair-primary-threshold+))
           (secondary-count (max 0 (- count +unification-pair-primary-threshold+)))
           (first-index (%unification-scratch-first-index scratch))
           (collision-index (%unification-scratch-collision-index scratch)))
      (when primary
        (dotimes (position (* 2 primary-count))
          (setf (svref primary position) nil)))
      (when secondary
        (dotimes (position (* 2 secondary-count))
          (setf (svref secondary position) nil)))
      (when first-index (clrhash first-index))
      (when collision-index (clrhash collision-index)))
    (%clear-occurs-scratch scratch)
    (setf (%unification-scratch-pair-count scratch) 0
          (%unification-scratch-pair-hash-mode-p scratch) nil
          (%unification-scratch-active-p scratch) nil)
    nil)

  (defun %call-with-unification-scratch (scratch thunk)
    (let ((*unification-scratch* scratch))
      (setf (%unification-scratch-active-p scratch) t)
      (unwind-protect
          (funcall thunk)
        (%reset-unification-scratch scratch)))))

(defun %unify-indexed (left right env base-index
                       &optional index-owned-p (occurs-check t))
  "Unify using BASE-INDEX, returning environment, success flag, and new index.
INDEX-OWNED-P permits in-place extension of a caller-owned transient index.
When OCCURS-CHECK is NIL the occurs check is skipped, so a variable may bind to
a term containing it (producing a rational/cyclic term)."
  (let* ((candidate *unification-scratch*)
         (scratch
           (if (and candidate
                    (not (%unification-scratch-active-p candidate)))
               candidate
               (%make-unification-scratch))))
    (%call-with-unification-scratch
      scratch
      (lambda ()
        (let ((index base-index)
              (copied-p index-owned-p))
          (labels ((ensure-writable-index ()
                     (unless copied-p
                       (setf index (%copy-environment-index index)
                             copied-p t)))
                   (extend-environment (variable term environment)
                     (ensure-writable-index)
                     (let ((binding (cons variable term)))
                       (%push-environment-index-binding binding index)
                       (cons binding environment)))
                   (unify-terms (left right environment)
                     (setf left (%walk-term-indexed left index)
                           right (%walk-term-indexed right index))
                     (cond
                       ((eq left right) (values environment t))
                       ((logic-var-p left)
                        (if (and occurs-check
                                 (%occurs-p-indexed left right index scratch t))
                            (values nil nil)
                            (values
                              (extend-environment left right environment)
                              t)))
                       ((logic-var-p right)
                        (unify-terms right left environment))
                       ((and (consp left) (consp right))
                        (if (%unification-scratch-remember-pair
                              scratch left right)
                            (values environment t)
                            (multiple-value-bind (extended ok)
                                (unify-terms
                                  (car left)
                                  (car right)
                                  environment)
                              (if ok
                                  (unify-terms
                                    (cdr left)
                                    (cdr right)
                                    extended)
                                  (values nil nil)))))
                       ((and
                          (symbolp left)
                          (symbolp right)
                          (%same-atom-text-p left right))
                        (values environment t))
                       ((equal left right) (values environment t))
                       (t (values nil nil)))))
            (multiple-value-bind (extended ok)
                (unify-terms left right env)
              (if ok
                  (values extended t index)
                  (values nil nil base-index)))))))))

(defun unify (left right &optional (env (quote ())) (occurs-check t)) "Unify LEFT and RIGHT against ENV.\n\nReturns (VALUES EXTENDED-ENV T) on success and (VALUES NIL NIL) on failure.\nOCCURS-CHECK defaults to T; pass NIL to allow cyclic bindings (see the\n`occurs_check\x27 Prolog flag).  Kept positional (not &key) so the hot\nclause-resolution path pays no keyword-dispatch cost." (if (eq left right) (values env t) (let ((*unification-scratch* (%make-unification-scratch))) (multiple-value-bind (extended ok index) (%unify-indexed left right env (%make-environment-index env 1) t occurs-check) (declare (ignore index)) (values extended ok)))))

(defun %logic-substitute-indexed (template index)
  "Apply INDEX to TEMPLATE while preserving dotted and cyclic structure."
  (let ((root (%walk-term-indexed template index)))
    (if (not (consp root))
        root
        (let ((copies (%make-freshening-map)))
          (labels ((copy-resolved-term (resolved)
                     (if (consp resolved)
                         (multiple-value-bind (copy present-p)
                             (%freshening-map-lookup resolved copies)
                           (if present-p
                               copy
                               (let ((copy (cons nil nil)))
                                 (%freshening-map-insert resolved copy copies)
                                 (setf (car copy)
                                       (substitute-term (car resolved))
                                       (cdr copy)
                                       (substitute-term (cdr resolved)))
                                 copy)))
                         resolved))
                   (substitute-term (term)
                     (copy-resolved-term
                       (%walk-term-indexed term index))))
            (copy-resolved-term root))))))

(defun logic-substitute (template env)
  "Recursively apply ENV to TEMPLATE, preserving dotted structure."
  (%logic-substitute-indexed template (%make-environment-index env)))

(defun %collect-variables (term)
  "Return the logic variables of TERM in first-appearance order."
  (let ((seen (make-hash-table :test #'eq))
        (seen-conses (make-hash-table :test #'eq))
        (variables '()))
    (labels ((walk (node)
               (cond
            ((logic-var-p node)
              (when *logic-variable-ordinals*
                (%register-logic-variable node))
              (unless (gethash node seen)
                (setf (gethash node seen) t)
                (push node variables)))
            ((consp node)
              (unless (gethash node seen-conses)
                (setf (gethash node seen-conses) t)
                (walk (car node))
                (walk (cdr node)))))))
      (walk term))
    (nreverse variables)))

(defconstant +freshening-map-threshold+ 12)
  (defstruct (%freshening-map (:constructor %make-freshening-map ()))
    (entries (make-array (* 2 +freshening-map-threshold+) :initial-element nil))
    (count 0 :type fixnum)
    (table nil :type (or null hash-table)))
  (defun %freshening-map-lookup (key mapping)
    (if (hash-table-p mapping)
        (gethash key mapping)
        (let ((table (%freshening-map-table mapping)))
          (if table
              (gethash key table)
              (let ((entries (%freshening-map-entries mapping))
                    (count (%freshening-map-count mapping)))
                (loop for index below count
                      for offset = (* index 2)
                      when (eq key (svref entries offset))
                        do (return (values (svref entries (1+ offset)) t))
                      finally (return (values nil nil))))))))
  (defun %freshening-map-insert (key value mapping)
    (cond
      ((hash-table-p mapping)
       (setf (gethash key mapping) value))
      ((%freshening-map-table mapping)
       (setf (gethash key (%freshening-map-table mapping)) value))
      (t
       (let ((count (%freshening-map-count mapping))
             (entries (%freshening-map-entries mapping)))
         (if (< count +freshening-map-threshold+)
             (progn
               (setf (svref entries (* count 2)) key
                     (svref entries (1+ (* count 2))) value)
               (incf (%freshening-map-count mapping))
               value)
             (let ((table (make-hash-table
                            :test (function eq)
                            :size (* 2 +freshening-map-threshold+))))
               (dotimes (index count)
                 (let ((offset (* index 2)))
                   (setf (gethash (svref entries offset) table)
                         (svref entries (1+ offset)))))
               (setf (%freshening-map-table mapping) table
                     (%freshening-map-entries mapping) nil
                     (gethash key table) value)
               value))))))
  (defun %freshen-term
      (term table &optional (copies (make-hash-table :test (function eq))))
    "Copy TERM, replacing each logic variable via TABLE with a fresh one.
COPIES preserves cons identity and cycles across calls that share it."
    (labels ((freshen (node)
               (cond
                 ((logic-var-p node)
                  (multiple-value-bind (fresh present-p)
                      (%freshening-map-lookup node table)
                    (if present-p
                        fresh
                        (%freshening-map-insert
                          node (fresh-logic-variable "?FRESH") table))))
                 ((consp node)
                  (multiple-value-bind (copy present-p)
                      (%freshening-map-lookup node copies)
                    (if present-p
                        copy
                        (let ((copy (cons nil nil)))
                          (%freshening-map-insert node copy copies)
                          (setf (car copy) (freshen (car node))
                                (cdr copy) (freshen (cdr node)))
                          copy))))
                 (t node))))
      (freshen term)))

(defun %term-has-variables-p (term)
  "True when TERM contains at least one logic variable."
  (cond
    ((logic-var-p term) t)
    ((not (consp term)) nil)
    (t
     (let ((seen (make-hash-table :test #'eq)))
       (labels ((has-variables-p (node)
                  (cond
                    ((logic-var-p node) t)
                    ((not (consp node)) nil)
                    ((gethash node seen) nil)
                    (t
                     (setf (gethash node seen) t)
                     (or (has-variables-p (car node))
                         (has-variables-p (cdr node)))))))
         (has-variables-p term))))))

(defun %freshen-clause (clause)
  "Return CLAUSE with all logic variables consistently renamed to fresh ones."
  (let ((mapping (%make-freshening-map)))
    (make-clause
      (%freshen-term (clause-head clause) mapping mapping)
      (mapcar
        (lambda (goal)
          (%freshen-term goal mapping mapping))
        (clause-body clause)))))
