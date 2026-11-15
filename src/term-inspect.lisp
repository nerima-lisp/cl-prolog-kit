;;;; Term type-inspection builtins: var/nonvar/atom/atomic/number/integer/
;;;; float/compound/callable/ground/acyclic_term/cyclic_term, and the
;;;; %term-resolve/%term-atom-p/%term-compound-p/%term-acyclic-p primitives
;;;; they share.

(in-package #:cl-prolog-kit)

(defun %term-resolve (term environment)
  "Resolve TERM's variables through ENVIRONMENT, preserving structure sharing.

Delegates to LOGIC-SUBSTITUTE, which builds ENVIRONMENT's lookup index once
and reuses it for every node -- unlike a naive per-node %WALK-TERM call,
which would rebuild that index on every recursive step."
  (logic-substitute term environment))

(defun %term-atom-p (term)
  (and (symbolp term) (not (logic-var-p term))))

(defun %term-compound-p (term)
  (and (consp term)
       (%proper-list-p term)
       (%term-atom-p (first term))))

(defun %term-acyclic-p (term &optional (environment '()))
  "Return true when TERM contains no cycle through cons cells."
  (let ((active (make-hash-table :test #'eq))
        (complete (make-hash-table :test #'eq)))
    ;; WALK recurses on cars only and follows the cdr spine in place; every
    ;; spine cell stays active until the spine ends, as it did while the
    ;; recursive cdr call ran.  A cycle anywhere makes the whole answer false,
    ;; so it returns at once instead of unwinding the tables.
    (labels ((walk (node)
               (let ((spine '()))
                 (loop
                   (let ((resolved (%walk-term node environment)))
                     (cond
                       ((atom resolved) (return))
                       ((gethash resolved complete) (return))
                       ((gethash resolved active)
                        (return-from %term-acyclic-p nil))
                       (t
                        (setf (gethash resolved active) t)
                        (push resolved spine)
                        (walk (car resolved))
                        (setf node (cdr resolved))))))
                 (dolist (cell spine t)
                   (remhash cell active)
                   (setf (gethash cell complete) t)))))
      (walk term))))

(defmacro define-term-predicate-builtin (&body definitions)
  "Define ISO term type-check builtins that test a single resolved term.
Each of DEFINITIONS is (NAME PREDICATES &key NEGATE); PREDICATES is a single
predicate symbol, or a list of predicate symbols combined with OR. The
builtin succeeds, without bindings, when the (disjunction of the) predicate
holds of the resolved term, or fails to hold when NEGATE is true."
  `(progn
     ,@(loop for (name predicates &key negate) in definitions
             collect
             (let ((predicate-list (if (listp predicates) predicates (list predicates))))
               `(define-builtin (,name term) (rulebase environment depth emit)
                  (let ((resolved-term (%term-resolve term environment)))
                    (,(if negate 'unless 'when)
                     (or ,@(mapcar (lambda (predicate) `(,predicate resolved-term))
                                   predicate-list))
                     (funcall emit environment))))))))

(define-term-predicate-builtin
  (var logic-var-p)
  (nonvar logic-var-p :negate t)
  (atom %term-atom-p)
  (number %prolog-number-p)
  (integer integerp)
  (float floatp)
  (compound %term-compound-p)
  (ground %term-has-variables-p :negate t)
  (string stringp)
  (atomic (%term-atom-p %prolog-number-p stringp))
  (callable (%term-atom-p %term-compound-p)))

(define-builtin (acyclic_term term) (rulebase environment depth emit)
  (when (%term-acyclic-p term environment)
    (funcall emit environment)))

(define-builtin (cyclic_term term) (rulebase environment depth emit)
  (unless (%term-acyclic-p term environment)
    (funcall emit environment)))
