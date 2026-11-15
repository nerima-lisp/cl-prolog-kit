(in-package #:cl-prolog-kit)

(defstruct (fd-store (:constructor %make-fd-store (domains constraints)))
  (domains nil :type list :read-only t)
  (constraints nil :type list :read-only t))

(defstruct (fd-constraint (:constructor %make-fd-constraint (operator arguments)))
  operator
  (arguments nil :type list :read-only t))

(defparameter *fd-store* (%make-fd-store nil nil))

(defun %fd-strictly-increasing-p (values)
  (loop for (left right) on values
        while right
        always (< left right)))

(defun %fd-domain (values)
  "Return VALUES as a sorted duplicate-free domain.
Propagation results are already in that form, so they are returned unchanged
instead of being copied, deduplicated and sorted again."
  (if (%fd-strictly-increasing-p values)
      values
      (sort (remove-duplicates (copy-list values)) #'<)))

(declaim (ftype function %check-builtin-output-length))

(defun %fd-interval (lower upper environment context)
  "Return the explicit value list LOWER..UPPER.
Domains are stored as explicit lists, so the interval size is bounded by
*MAX-PROLOG-BUILTIN-OUTPUT-LENGTH* before any value is materialized."
  (when (<= lower upper)
    (%check-builtin-output-length
     (1+ (- upper lower)) "FD_DOMAIN_SIZE" environment context
     "finite domain interval exceeds the configured output limit")
    (loop for value from lower to upper collect value)))

(defun %fd-range-symbol-p (term)
  (and (symbolp term) (string= (symbol-name term) "..")))

(defun %fd-domain-spec (term environment &optional (context (%iso-atom "IN")))
  (let ((resolved (logic-substitute term environment)))
    (cond
      ((and (%proper-list-p resolved)
            (= (length resolved) 3)
            (%fd-range-symbol-p (second resolved))
            (integerp (first resolved))
            (integerp (third resolved)))
       (%fd-interval (first resolved) (third resolved) environment context))
      ((and (%proper-list-p resolved)
            (= (length resolved) 3)
            (%fd-range-symbol-p (first resolved))
            (integerp (second resolved))
            (integerp (third resolved)))
       (%fd-interval (second resolved) (third resolved) environment context))
      ((and (%proper-list-p resolved) (every #'integerp resolved))
       (%fd-domain resolved))
      (t
       (%raise-type-error "FINITE_DOMAIN" resolved environment
                          context "integer list or (Lower .. Upper) required")))))

(defun %fd-domain-of (store variable)
  (cdr (assoc variable (fd-store-domains store) :test #'eq)))

(defun %fd-put-domain (store variable domain)
  (%make-fd-store (acons variable domain
                         (remove variable (fd-store-domains store)
                                 :key #'car :test #'eq))
                  (fd-store-constraints store)))

(defun %fd-intersection (current allowed)
  "Return CURRENT's values that also occur in ALLOWED, in linear time.
CL:INTERSECTION is quadratic, which a domain near the output limit turns
into a host-blocking loop."
  (if (and (%fd-strictly-increasing-p current)
           (%fd-strictly-increasing-p allowed))
      ;; Both sorted: merge instead of hashing.
      (loop with rest = allowed
            for value in current
            do (loop while (and rest (< (first rest) value))
                     do (pop rest))
            when (and rest (= (first rest) value))
              collect value)
      (let ((members (make-hash-table :test #'eql :size (length allowed))))
        (dolist (value allowed)
          (setf (gethash value members) t))
        (remove-if-not (lambda (value) (gethash value members)) current))))

(defun %fd-restrict-domain (store variable allowed)
  (let* ((current (%fd-domain-of store variable))
         (domain (if current (%fd-intersection current allowed) allowed)))
    (values (%fd-put-domain store variable (%fd-domain domain))
            (not (null domain)))))

(defun %fd-add-constraint (store operator arguments)
  (%make-fd-store (fd-store-domains store)
                  (cons (%make-fd-constraint operator arguments)
                        (fd-store-constraints store))))

(defun %fd-expression-value (expression environment)
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((integerp term) (values term t))
                 ((logic-var-p term) (values nil nil))
                 ((and (%proper-list-p term) (= (length term) 3))
                  (multiple-value-bind (left leftp) (evaluate (second term))
                    (multiple-value-bind (right rightp) (evaluate (third term))
                      (if (and leftp rightp)
                          (values (case (%arithmetic-operator-key
                                         (first term)
                                         *binary-arithmetic-functions*)
                                    (:+ (+ left right))
                                    (:- (- left right))
                                    (:* (* left right))
                                    (otherwise
                                     (%raise-type-error "FD_EXPRESSION" term environment
                                                        (%iso-atom "CLPFD")
                                                        "only +, - and * are supported")))
                                  t)
                          (values nil nil)))))
                 (t (%raise-type-error "FD_EXPRESSION" term environment
                                       (%iso-atom "CLPFD")
                                       "integer, variable, or binary expression required")))))
      (evaluate resolved))))

(defun %fd-relation-true-p (operator left right)
  (ecase operator
    (|#=| (= left right))
    (|#\\=| (/= left right))
    (|#<| (< left right))
    (|#=<| (<= left right))
    (|#>| (> left right))
    (|#>=| (>= left right))))

(defun %fd-filter-variable (store variable operator constant variable-left-p)
  (let ((domain (%fd-domain-of store variable)))
    (if domain
        (%fd-restrict-domain
         store variable
         (remove-if-not (lambda (value)
                          (if variable-left-p
                              (%fd-relation-true-p operator value constant)
                              (%fd-relation-true-p operator constant value)))
                        domain))
        (values store t))))

(defun %fd-values-supported-by (operator own-domain other-domain own-position)
  "Return the subset of OWN-DOMAIN whose OWN-POSITION (:LEFT or :RIGHT) role
in OPERATOR's relation is satisfied by some value in OTHER-DOMAIN.

Each test runs in constant time against a summary of OTHER-DOMAIN (its bounds,
a membership table, or a second distinct value), so the cost is linear in the
two domain sizes.  Pairwise testing was quadratic, which domains near
*MAX-PROLOG-BUILTIN-OUTPUT-LENGTH* turn into a builtin call no limit can
interrupt.  OWN-DOMAIN's order is preserved."
  (when (null other-domain)
    (return-from %fd-values-supported-by '()))
  ;; With OWN on the right, OPERATOR's sides swap: other < own is own > other.
  (let ((relation (if (eq own-position :left)
                      operator
                      (ecase operator
                        (|#<| '|#>|) (|#=<| '|#>=|) (|#>| '|#<|) (|#>=| '|#=<|)
                        ((|#=| |#\\=|) operator)))))
    (case relation
      ((|#<| |#=<|)
       (let ((upper (reduce #'max other-domain)))
         (remove-if-not (lambda (value) (%fd-relation-true-p relation value upper))
                        own-domain)))
      ((|#>| |#>=|)
       (let ((lower (reduce #'min other-domain)))
         (remove-if-not (lambda (value) (%fd-relation-true-p relation value lower))
                        own-domain)))
      (|#=|
       (let ((members (make-hash-table :test #'eql)))
         (dolist (value other-domain)
           (setf (gethash value members) t))
         (remove-if-not (lambda (value) (gethash value members)) own-domain)))
      (|#\\=|
       ;; Some other value differs from VALUE unless OTHER-DOMAIN holds only
       ;; VALUE itself.
       (let* ((first (first other-domain))
              (second-distinct-p (find first other-domain :test-not #'eql)))
         (if second-distinct-p
             (copy-list own-domain)
             (remove first own-domain :test #'eql)))))))

(defun %fd-filter-variable-pair (store left right operator)
  (let ((left-domain (%fd-domain-of store left))
        (right-domain (%fd-domain-of store right)))
    (if (and left-domain right-domain)
        (let ((supported-left
                (%fd-values-supported-by operator left-domain right-domain :left)))
          (multiple-value-bind (left-store left-success-p)
              (%fd-restrict-domain store left supported-left)
            (if (not left-success-p)
                (values left-store nil)
                (let ((restricted-left (%fd-domain-of left-store left)))
                  (%fd-restrict-domain
                   left-store right
                   (%fd-values-supported-by
                    operator right-domain restricted-left :right))))))
        (values store t))))

(defun %fd-propagate-relation (store constraint environment)
  (destructuring-bind (left right) (fd-constraint-arguments constraint)
    (multiple-value-bind (left-value left-ground-p)
        (%fd-expression-value left environment)
      (multiple-value-bind (right-value right-ground-p)
          (%fd-expression-value right environment)
        (let ((operator (fd-constraint-operator constraint))
              (resolved-left (logic-substitute left environment))
              (resolved-right (logic-substitute right environment)))
          (cond
            ((and left-ground-p right-ground-p)
             (values store (%fd-relation-true-p operator left-value right-value)))
            ((and (logic-var-p resolved-left)
                  (eq resolved-left resolved-right))
             (values store
                     (member operator '(|#=| |#=<| |#>=|) :test #'eq)))
            ((and (logic-var-p resolved-left) right-ground-p)
             (%fd-filter-variable store resolved-left operator right-value t))
            ((and left-ground-p (logic-var-p resolved-right))
             (%fd-filter-variable store resolved-right operator left-value nil))
            ((and (logic-var-p resolved-left) (logic-var-p resolved-right))
             (%fd-filter-variable-pair store resolved-left resolved-right operator))
            (t (values store t))))))))

(defun %fd-distinct-assignment-p (variables store used)
  "True when VARIABLES have a distinct assignment compatible with STORE."
  (let ((matching (make-hash-table :test #'eql))
        (reserved (make-hash-table :test #'eql)))
    (dolist (value used)
      (setf (gethash value reserved) t))
    (labels ((augment (variable seen)
               (some (lambda (value)
                       (unless (or (gethash value reserved)
                                   (gethash value seen))
                         (setf (gethash value seen) t)
                         (multiple-value-bind (owner assignedp)
                             (gethash value matching)
                           (when (or (not assignedp)
                                     (augment owner seen))
                             (setf (gethash value matching) variable)
                             t))))
                     (%fd-domain-of store variable))))
      (every (lambda (variable)
               (augment variable (make-hash-table :test #'eql)))
             variables))))

(defun %fd-propagate-all-different (store constraint environment)
  (let* ((terms (mapcar (lambda (term) (logic-substitute term environment))
                        (fd-constraint-arguments constraint)))
         (ground (remove-if-not #'integerp terms))
         (variables (remove-if-not #'logic-var-p terms)))
    (if (or (/= (length ground) (length (remove-duplicates ground)))
            (/= (length variables) (length (remove-duplicates variables :test #'eq))))
        (values store nil)
        (loop with current = store
              for term in terms
              when (and (logic-var-p term) (%fd-domain-of current term))
                do (multiple-value-bind (next successp)
                       (%fd-restrict-domain current term
                                            (set-difference (%fd-domain-of current term) ground))
                     (unless successp (return (values next nil)))
                     (setf current next))
              finally
                 (return
                   (values current
                           (%fd-distinct-assignment-p
                            (remove-if-not
                             (lambda (variable)
                               (%fd-domain-of current variable))
                             variables)
                            current ground)))))))

(defun %fd-propagate (store environment)
  (dolist (entry (fd-store-domains store))
    (let ((resolved (logic-substitute (car entry) environment)))
      (when (or (and (integerp resolved)
                     (not (member resolved (cdr entry))))
                (and (not (logic-var-p resolved))
                     (not (integerp resolved))))
        (return-from %fd-propagate (values store nil)))))
  (loop with current = store
        repeat (1+ (length (fd-store-constraints store)))
        do (let ((before (fd-store-domains current)))
             (dolist (constraint (fd-store-constraints current))
               (multiple-value-bind (next successp)
                   (if (eq (fd-constraint-operator constraint) 'all_different)
                       (%fd-propagate-all-different current constraint environment)
                       (%fd-propagate-relation current constraint environment))
                 (unless successp (return-from %fd-propagate (values next nil)))
                 (setf current next)))
             (when (equal before (fd-store-domains current))
               (return (values current t))))
        finally (return (values current t))))

(defun %fd-store-active-p (&optional (store *fd-store*))
  "Return true when STORE contains finite-domain state."
  (or (fd-store-domains store)
      (fd-store-constraints store)))

(defun %fd-after-unify (environment emit)
  "Propagate the active FD store after unification and emit consistent states."
  (if (%fd-store-active-p)
      (multiple-value-bind (store successp)
          (%fd-propagate *fd-store* environment)
        (when successp
          (let ((*fd-store* store))
            (funcall emit environment))))
      (funcall emit environment)))

(setf *constraint-post-unify-hook* #'%fd-after-unify
      *constraints-active-p-hook* #'%fd-store-active-p)
