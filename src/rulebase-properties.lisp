;;;; Per-predicate declarations stored on a rulebase: `table' declarations and
;;;; the general predicate-property table (dynamic, discontiguous, and the
;;;; rest).  Both are keyed by module/predicate/arity and are independent of
;;;; the clause storage and logical-update machinery in data.lisp, which loads
;;;; immediately before.

(in-package #:cl-prolog-kit)

(defun %rulebase-tabled-p (rulebase predicate arity
                           &optional (module +default-prolog-module+))
  (not (null (gethash (list module predicate arity)
                      (rulebase-table-declarations rulebase)))))

(defun %add-rulebase-table-declaration! (rulebase predicate arity owner
                                          &optional (module +default-prolog-module+))
  "Add OWNER's table declaration, advancing the revision on first ownership."
  (let* ((key (list module predicate arity))
         (owners (gethash key (rulebase-table-declarations rulebase))))
    (unless (member owner owners :test #'equal)
      (unless owners
        (%next-rulebase-revision! rulebase))
      (push owner (gethash key (rulebase-table-declarations rulebase))))
    rulebase))

(defun %remove-rulebase-table-declaration! (rulebase predicate arity owner
                                             &optional (module +default-prolog-module+))
  "Remove OWNER's declaration, advancing the revision when no owner remains."
  (let* ((key (list module predicate arity))
         (owners (gethash key (rulebase-table-declarations rulebase)))
         (remaining (remove owner owners :test #'equal)))
    (unless (= (length owners) (length remaining))
      (if remaining
          (setf (gethash key (rulebase-table-declarations rulebase)) remaining)
          (progn
            (remhash key (rulebase-table-declarations rulebase))
            (%next-rulebase-revision! rulebase))))
    rulebase))

(defun %remove-rulebase-table-declarations! (rulebase predicate arity
                                              &optional (module +default-prolog-module+))
  "Remove every table declaration for PREDICATE/ARITY in MODULE."
  (let ((key (list module predicate arity)))
    (when (remhash key (rulebase-table-declarations rulebase))
      (%next-rulebase-revision! rulebase))
    rulebase))

(defun %predicate-property-key (predicate arity module)
  (list module predicate arity))

(defun %rulebase-predicate-property (rulebase predicate arity
                                     &optional (module +default-prolog-module+))
  (gethash (%predicate-property-key predicate arity module)
           (rulebase-predicate-properties rulebase)))

(defun %set-rulebase-predicate-property! (rulebase predicate arity property
                                          &optional (module +default-prolog-module+))
  (setf (gethash (%predicate-property-key predicate arity module)
                 (rulebase-predicate-properties rulebase))
        property))

(defun %remove-rulebase-predicate-property! (rulebase predicate arity
                                              &optional (module +default-prolog-module+))
  "Remove PREDICATE/ARITY's declaration and return whether one existed."
  (remhash (%predicate-property-key predicate arity module)
           (rulebase-predicate-properties rulebase)))

(defun %rulebase-declared-predicate-indicators
    (rulebase &optional (module +default-prolog-module+))
  "Return declared predicate indicators in parser AST form (/ NAME ARITY)."
  (let ((indicators '()))
    (maphash (lambda (key property)
               (declare (cl:ignore property))
               (when (eq (first key) module)
                 (push (list '/ (second key) (third key)) indicators)))
             (rulebase-predicate-properties rulebase))
    indicators))
