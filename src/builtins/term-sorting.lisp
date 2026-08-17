;;;; Standard-order term sorting and the builtins built on it: sort/2, msort/2,
;;;; keysort/2, sort/4 and predsort/3.
;;;;
;;;; Split out of collection.lisp, which keeps the solution-collection family
;;;; (findall/bagof/setof) that consumes %STANDARD-TERM-SORT-UNIQUE from here.
;;;; assoc.lisp also depends on %PROLOG-TERM<.

(in-package #:cl-prolog-kit)

(defun %prolog-term< (left right)
  "Compare terms using the standard Prolog order."
  (minusp (%compare-terms left right)))

(defun %standard-term-sort (values)
  "Return a stable standard-order copy of VALUES."
  (stable-sort (copy-list values) #'%prolog-term<))

(defun %standard-term-sort-unique (values)
  "Sort VALUES and remove adjacent terms whose standard comparison is zero."
  (let ((ordered (%standard-term-sort values))
        (result '()))
    (dolist (value ordered (nreverse result))
      (unless (and result (zerop (%compare-terms (car result) value)))
        (push value result)))))

(defun %key-value-term-p (term)
  (and (%proper-list-p term)
       (= (length term) 3)
       (symbolp (first term))
       (string= (symbol-name (first term)) "-")))

(defun %keysort-key (pair environment)
  (unless (%key-value-term-p pair)
    (%raise-type-error "PAIR" pair environment (%iso-atom "KEYSORT")
                       "KEYSORT/2 expects Key-Value pairs"))
  (second pair))

(defmacro define-standard-order-sort-builtin (name operation-name sort-fn)
  "Define NAME as a builtin resolving INPUT to a proper list and unifying
SORTED with (SORT-FN list), reporting ISO errors under OPERATION-NAME."
  `(define-builtin (,name input sorted) (rulebase environment depth emit)
     (let* ((values (%require-proper-list
                     (logic-substitute input environment)
                     environment (%iso-atom ,operation-name) "the input list"))
            (ordered (,sort-fn values)))
       (%unify-emit sorted ordered environment emit))))

(define-standard-order-sort-builtin sort "SORT" %standard-term-sort-unique)

(define-standard-order-sort-builtin msort "MSORT" %standard-term-sort)

(define-builtin (keysort input sorted) (rulebase environment depth emit)
  (let ((pairs (%require-proper-list (logic-substitute input environment)
                                       environment (%iso-atom "KEYSORT")
                                       "the input list")))
    (dolist (pair pairs)
      (%keysort-key pair environment))
    (%unify-emit sorted
                 (stable-sort (copy-list pairs) #'%prolog-term<
                              :key (lambda (pair)
                                     (%keysort-key pair environment)))
                 environment emit)))

;;; sort/4 -- key-and-order sort.  Key 0 selects the whole term; a positive
;;; key selects that argument of each compound.  Order is one of @<, @=<, @>,
;;; @>= ; @< and @> remove elements comparing equal on the key.

(defun %sort4-key (element key environment operation)
  "Return the sort key of ELEMENT for sort/4 KEY (0 = whole term)."
  (if (zerop key)
      element
      (progn
        (unless (and (%proper-list-p element) (> (length element) key))
          (%raise-type-error "COMPOUND" element environment operation
                             "sort/4 key argument exceeds the term's arity"))
        (nth key element))))

(defun %sort4-order (order environment operation)
  "Return (VALUES ASCENDING-P DEDUP-P) for a sort/4 order atom."
  (unless (%term-atom-p order)
    (if (logic-var-p order)
        (%raise-instantiation-error environment operation
                                    "sort/4 order must be instantiated")
        (%raise-type-error "ATOM" order environment operation
                           "sort/4 order must be an atom")))
  (let ((name (symbol-name order)))
    (cond
      ((string-equal name "@<")  (values t t))
      ((string-equal name "@=<") (values t nil))
      ((string-equal name "@>")  (values nil t))
      ((string-equal name "@>=") (values nil nil))
      (t (%raise-domain-error "ORDER" order environment operation
                              "sort/4 order must be @<, @=<, @> or @>=")))))

(define-builtin (sort key order input sorted) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "SORT"))
         (key-value (logic-substitute key environment))
         (order-value (logic-substitute order environment))
         (values (%require-proper-list (logic-substitute input environment)
                                              environment operation
                                              "the input list")))
    (%require-bounded-integer key-value environment operation "sort/4 key")
    (multiple-value-bind (ascending-p dedup-p)
        (%sort4-order order-value environment operation)
      ;; Extract every key up front so an out-of-range key is reported even for
      ;; short lists and each key is computed once.
      (let* ((tagged (mapcar (lambda (element)
                               (cons (%sort4-key element key-value
                                                 environment operation)
                                     element))
                             values))
             (ordered (stable-sort tagged
                                   (lambda (a b)
                                     (let ((c (%compare-terms (car a) (car b))))
                                       (if ascending-p (minusp c) (plusp c))))))
             (result (if dedup-p
                         (let ((kept '()))
                           (dolist (cell ordered (nreverse kept))
                             (unless (and kept
                                          (zerop (%compare-terms (car (car kept))
                                                                 (car cell))))
                               (push cell kept))))
                         ordered)))
        (%unify-emit sorted (mapcar #'cdr result) environment emit)))))

;;; predsort/3 -- sort with a user comparison predicate call(Pred, Order, A, B),
;;; dropping elements whose comparison yields `='.

(defun %predsort-order (closure a b rulebase environment depth operation fail-tag)
  "Call CLOSURE(Order, A, B) and return -1/0/1 from the resulting order atom.
Throws FAIL-TAG when the comparison predicate has no proof, so predsort/3
fails (as in SWI) rather than raising."
  (let ((order-var (fresh-logic-variable "?PREDSORT-ORDER")))
    (multiple-value-bind (result-environment succeeded-p)
        (%first-proof-environment
         (%extend-callable-goal closure (list order-var a b) environment operation)
         rulebase environment depth)
      (unless succeeded-p
        (cl:throw fail-tag :fail))
      (let ((order (logic-substitute order-var result-environment)))
        (unless (%term-atom-p order)
          (%raise-type-error "ATOM" order environment operation
                             "predsort/3 comparison must bind an order atom"))
        (let ((name (symbol-name order)))
          (cond ((string-equal name "<") -1)
                ((string-equal name "=") 0)
                ((string-equal name ">") 1)
                (t (%raise-domain-error "ORDER" order environment operation
                                        "predsort/3 order must be <, = or >"))))))))

(defun %predsort-merge-runs (left right comparison)
  "Merge sorted runs LEFT and RIGHT, dropping duplicate right-hand elements."
  (let ((accumulator '()))
    (loop
      (cond
        ((null left) (return (nreconc accumulator right)))
        ((null right) (return (nreconc accumulator left)))
        (t (let ((order (funcall comparison (car left) (car right))))
             (cond
               ((minusp order) (push (pop left) accumulator))
               ((zerop order)
                (push (pop left) accumulator)
                (pop right))
               ((plusp order) (push (pop right) accumulator)))))))))

(defun %predsort-sort-run (items comparison)
  "Destructively split and merge-sort a private proper list spine."
  (if (or (null items) (null (cdr items)))
      items
      (let* ((half (floor (length items) 2))
             (left-tail (nthcdr (1- half) items))
             (right (cdr left-tail)))
        (setf (cdr left-tail) nil)
        (%predsort-merge-runs (%predsort-sort-run items comparison)
                               (%predsort-sort-run right comparison)
                               comparison))))

(define-builtin (predsort goal input sorted) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "PREDSORT"))
         (closure (logic-substitute goal environment))
         (values (%require-proper-list (logic-substitute input environment)
                                       environment operation
                                       "the input list"))
         (fail-tag (list 'predsort-fail)))
    (when (logic-var-p closure)
      (%raise-instantiation-error environment operation
                                  "predsort/3 comparison predicate must be instantiated"))
    ;; Copy the substituted list before destructively splitting its private spine.
    (let ((result
            (cl:catch fail-tag
              (%predsort-sort-run
               (copy-list values)
               (lambda (a b)
                 (%predsort-order closure a b rulebase environment depth
                                  operation fail-tag))))))
      (unless (eq result :fail)
        (%unify-emit sorted result environment emit)))))
