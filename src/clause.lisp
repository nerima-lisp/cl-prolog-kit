;;;; What a clause is: the CLAUSE struct, the immutable instantiation
;;;; templates compiled from it, and the stored-clause lifetime record.
;;;; The rulebase container that holds stored clauses lives in data.lisp;
;;;; the predicate lookup layer lives in predicate-index.lisp.

(in-package #:cl-prolog-kit)

(defstruct (clause (:copier nil) (:constructor make-clause (head &optional (body (quote ())))))
  "A Horn clause. An empty BODY denotes a fact."
  (head (quote ()) :type list :read-only t)
  (body (quote ()) :type list :read-only t))
(defstruct (%clause-template-reference
            (:copier nil)
            (:constructor %make-clause-template-reference (kind value)))
  (kind :literal :type (member :literal :variable :cons) :read-only t)
  (value nil :read-only t))
(defstruct (%rule-instruction
            (:copier nil)
            (:constructor %make-rule-instruction (predicate operands)))
  (predicate nil :type symbol :read-only t)
  (operands #() :type simple-vector :read-only t))
(defstruct (%rule-program
            (:copier nil)
            (:constructor %make-rule-program
                (variable-count head-operands body)))
  (variable-count 0 :type (integer 0 *) :read-only t)
  (head-operands #() :type simple-vector :read-only t)
  (body #() :type simple-vector :read-only t))
(defstruct (%clause-template
            (:copier nil)
            (:constructor %make-clause-template
                (variable-count cons-cars cons-cdrs head body
                 &optional rule-program)))
  (variable-count 0 :type (integer 0 *) :read-only t)
  (cons-cars #() :type simple-vector :read-only t)
  (cons-cdrs #() :type simple-vector :read-only t)
  (head nil :type %clause-template-reference :read-only t)
  (body nil :type %clause-template-reference :read-only t)
  (rule-program nil :type (or null %rule-program) :read-only t))
(defun %rule-program-literal-p (term)
  (or (symbolp term) (numberp term) (characterp term)))
(defun %compile-rule-program (clause)
  "Compile the restricted immutable rule instruction subset, or return NIL."
  (let ((variables (make-hash-table :test (function eq)))
        (seen-conses (make-hash-table :test (function eq)))
        (next-variable 0))
    (labels ((encode-operand (term)
               (cond
                 ((logic-var-p term)
                  (multiple-value-bind (reference present-p)
                      (gethash term variables)
                    (if present-p
                        reference
                        (let ((reference
                                (%make-clause-template-reference
                                 :variable next-variable)))
                          (setf (gethash term variables) reference)
                          (incf next-variable)
                          reference))))
                 ((%rule-program-literal-p term)
                  (%make-clause-template-reference :literal term))
                 (t (return-from %compile-rule-program nil))))
             (compile-call (goal)
               (unless (consp goal)
                 (return-from %compile-rule-program nil))
               (when (gethash goal seen-conses)
                 (return-from %compile-rule-program nil))
               (setf (gethash goal seen-conses) t)
               (let ((predicate (car goal))
                     (cursor (cdr goal))
                     (operands (make-array 4 :adjustable t :fill-pointer 0)))
                 (unless (and (symbolp predicate)
                              (not (logic-var-p predicate))
                              (not (string= (symbol-name predicate) "!")))
                   (return-from %compile-rule-program nil))
                 (loop
                   (cond
                     ((null cursor)
                      (return
                        (%make-rule-instruction
                         predicate
                         (coerce operands (quote simple-vector)))))
                     ((not (consp cursor))
                      (return-from %compile-rule-program nil))
                     ((gethash cursor seen-conses)
                      (return-from %compile-rule-program nil))
                     (t
                      (setf (gethash cursor seen-conses) t)
                      (vector-push-extend (encode-operand (car cursor)) operands)
                      (setf cursor (cdr cursor)))))))
             (compile-body (body)
               (cond
                 ((null body)
                  (return-from compile-body #())))
               (let ((instructions (make-array 4 :adjustable t :fill-pointer 0))
                     (cursor body))
                 (loop
                   (cond
                     ((null cursor)
                      (return (coerce instructions (quote simple-vector))))
                     ((not (consp cursor))
                      (return-from %compile-rule-program nil))
                     ((gethash cursor seen-conses)
                      (return-from %compile-rule-program nil))
                     (t
                      (setf (gethash cursor seen-conses) t)
                      (vector-push-extend (compile-call (car cursor)) instructions)
                      (setf cursor (cdr cursor))))))))
      (let* ((head-instruction (compile-call (clause-head clause)))
             (body (compile-body (clause-body clause))))
        (%make-rule-program next-variable
                            (%rule-instruction-operands head-instruction)
                            body)))))
(defun %compile-clause-template (clause)
  "Compile CLAUSE into an immutable graph template with stable IDs."
  (let ((variables (make-hash-table :test (function eq)))
        (conses (make-hash-table :test (function eq)))
        (cons-cars (make-array 8 :adjustable t :fill-pointer 0))
        (cons-cdrs (make-array 8 :adjustable t :fill-pointer 0))
        (next-variable 0)
        (rule-program (%compile-rule-program clause)))
    (labels ((encode (term)
               (cond
                 ((logic-var-p term)
                  (multiple-value-bind (reference present-p) (gethash term variables)
                    (if present-p
                        reference
                        (let ((reference
                                (%make-clause-template-reference
                                 :variable next-variable)))
                          (setf (gethash term variables) reference)
                          (incf next-variable)
                          reference))))
                 ((consp term)
                  (multiple-value-bind (reference present-p) (gethash term conses)
                    (if present-p
                        reference
                        (let* ((identifier (fill-pointer cons-cars))
                               (reference (%make-clause-template-reference :cons identifier)))
                          (setf (gethash term conses) reference)
                          (vector-push-extend nil cons-cars)
                          (vector-push-extend nil cons-cdrs)
                          (setf (aref cons-cars identifier) (encode (car term))
                                (aref cons-cdrs identifier) (encode (cdr term)))
                          reference))))
                 (t (%make-clause-template-reference :literal term)))))
      (let ((head (encode (clause-head clause)))
            (body (encode (clause-body clause))))
        (%make-clause-template next-variable
                               (coerce cons-cars (quote simple-vector))
                               (coerce cons-cdrs (quote simple-vector))
                               head body rule-program)))))
(defstruct (%clause-materialization-context (:constructor %make-clause-materialization-context (template variables conses))) template variables conses)

(defun %make-clause-template-materialization-context (template) (let* ((variable-count (%clause-template-variable-count template)) (variables (make-array variable-count)) (conses (make-array (length (%clause-template-cons-cars template)) :initial-element nil))) (dotimes (index variable-count) (setf (svref variables index) (fresh-logic-variable))) (%make-clause-materialization-context template variables conses)))

(defun %materialize-clause-template-reference (reference context) (ecase (%clause-template-reference-kind reference) (:literal (%clause-template-reference-value reference)) (:variable (svref (%clause-materialization-context-variables context) (%clause-template-reference-value reference))) (:cons (let* ((index (%clause-template-reference-value reference)) (conses (%clause-materialization-context-conses context)) (existing (svref conses index))) (or existing (let* ((template (%clause-materialization-context-template context)) (cell (cons nil nil))) (setf (svref conses index) cell (car cell) (%materialize-clause-template-reference (svref (%clause-template-cons-cars template) index) context) (cdr cell) (%materialize-clause-template-reference (svref (%clause-template-cons-cdrs template) index) context)) cell))))))

(defun %materialize-clause-template-head (template context) (%materialize-clause-template-reference (%clause-template-head template) context))

(defun %materialize-clause-template-body (template context) (%materialize-clause-template-reference (%clause-template-body template) context))
(defun %materialize-clause-template (template) "Instantiate TEMPLATE with fresh variables and lazily allocated cons shells." (let ((context (%make-clause-template-materialization-context template))) (make-clause (%materialize-clause-template-head template context) (%materialize-clause-template-body template context))))

(defun %copy-clause (clause)
  "Copy CLAUSE's cons graph while preserving atoms, sharing, and cycles."
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((copy-term (term)
               (if (consp term)
                   (multiple-value-bind (copy present-p)
                       (gethash term copies)
                     (if present-p
                         copy
                         (let ((copy (cons nil nil)))
                           (setf (gethash term copies) copy
                                 (car copy) (copy-term (car term))
                                 (cdr copy) (copy-term (cdr term)))
                           copy)))
                   term)))
      (make-clause (copy-term (clause-head clause))
                   (copy-term (clause-body clause))))))

(defstruct (%stored-clause
            (:copier nil)
            (:constructor %make-stored-clause
                (clause template module born-revision &optional source)))
  "Internal lifetime metadata for one clause in a rulebase."
  (clause (make-clause '()) :type clause :read-only t)
  (template nil :type %clause-template :read-only t)
  (module +default-prolog-module+ :type symbol :read-only t)
  (born-revision 0 :type (integer 0 *) :read-only t)
  (source nil :type (or null pathname) :read-only t)
  (died-revision nil :type (or null (integer 0 *))))

(defun %make-owned-stored-clause
    (clause module born-revision &optional source)
  "Snapshot CLAUSE and compile its immutable instantiation template once."
  (let ((template (%compile-clause-template clause)))
    (%make-stored-clause (%copy-clause clause) template
                         module born-revision source)))

(defun %materialize-stored-clause-for-proof (entry template)
  "Instantiate variable clauses, but share the owned snapshot of a ground clause."
  (if (zerop (%clause-template-variable-count template))
      (%stored-clause-clause entry)
      (%materialize-clause-template template)))

(defun %stored-clause-visible-p (entry revision)
  (and (<= (%stored-clause-born-revision entry) revision)
       (let ((died (%stored-clause-died-revision entry)))
         (or (null died) (< revision died)))))

(defun %visible-stored-clauses (entries revision)
  (loop for entry in entries
        when (%stored-clause-visible-p entry revision)
          collect entry))
