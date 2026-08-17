;;;; library(lists) predicates beyond the ISO core.
;;;;
;;;; These are the deterministic, non-meta list utilities SWI-Prolog exposes
;;;; through library(lists).  Meta-predicates that call a goal per element
;;;; (maplist, foldl, include, exclude, partition) live in apply.lisp.

(in-package #:cl-prolog-kit)

(defun %check-builtin-output-length (count resource environment operation message)
  "Bound COUNT against *max-prolog-builtin-output-length*, raising a catchable
resource_error when it is exceeded.  Returns COUNT.  Shared by the list and
format builtins to cap attacker-controlled allocation."
  (when (and *max-prolog-builtin-output-length*
             (> count *max-prolog-builtin-output-length*))
    (%raise-resource-error resource environment operation message))
  count)

(define-term-guard %require-proper-list (value argument)
  :documentation "Return VALUE when it is a proper list, otherwise raise the ISO error."
  :instantiation (format nil "~A must be instantiated" argument)
  :accept (%proper-list-p value)
  :type "LIST"
  :type-message (format nil "~A must be a proper list" argument))

(define-term-guard %require-number (value)
  :documentation "Return VALUE when it is a Prolog number, otherwise raise the ISO error."
  :instantiation "list elements must be instantiated numbers"
  :accept (or (integerp value) (floatp value))
  :type "NUMBER"
  :type-message "list elements must be numbers")

(defun %resolved-number-list (list-term environment operation argument)
  "Resolve LIST-TERM to a proper list of Prolog numbers."
  (let ((value (logic-substitute list-term environment)))
    (mapcar (lambda (element)
              (%require-number element environment operation))
            (%require-proper-list value environment operation argument))))

;;; sum_list/2, sumlist/2, max_list/2, min_list/2

(define-builtin ((sum_list sumlist) list-term sum) (rulebase environment depth emit)
  (%unify-emit sum
               (reduce #'+ (%resolved-number-list list-term environment
                                                  (%iso-atom "SUM_LIST") "first argument")
                       :initial-value 0)
               environment emit))

(defmacro %define-list-extremum-builtin (name reducer operation)
  "Define a fold-over-numbers builtin that fails on the empty list."
  `(define-builtin (,name list-term extremum) (rulebase environment depth emit)
     (let ((numbers (%resolved-number-list list-term environment
                                           (%iso-atom ,operation) "first argument")))
       (when numbers
         (%unify-emit extremum (reduce ,reducer numbers) environment emit)))))

(%define-list-extremum-builtin max_list #'max "MAX_LIST")
(%define-list-extremum-builtin min_list #'min "MIN_LIST")

;;; numlist/3

(define-iso-builtin (numlist low high (list-term :raw)) "NUMLIST"
  (dolist (bound (list resolved-low resolved-high))
    (cond
      ((logic-var-p bound)
       (%raise-instantiation-error environment operation
                                   "numlist/3 bounds must be instantiated"))
      ((not (integerp bound))
       (%raise-type-error "INTEGER" bound environment operation
                          "numlist/3 bounds must be integers"))))
  (when (<= resolved-low resolved-high)
    (%check-builtin-output-length
     (1+ (- resolved-high resolved-low)) "LIST_LENGTH" environment operation
     "numlist/3 range exceeds the configured length limit")
    (%unify-emit list-term
                 (loop for n from resolved-low to resolved-high collect n)
                 environment emit)))

;;; list_to_set/2 -- deduplicate under standard order of terms (==),
;;; preserving first-occurrence order.  O(n log n): tag each element with its
;;; index, sort by standard order (index breaks ties), drop adjacent
;;; duplicates keeping the earliest index, then restore original order.

(define-builtin (list_to_set list-term set) (rulebase environment depth emit)
  (let* ((value (logic-substitute list-term environment))
         (elements (%require-proper-list value environment
                                         (%iso-atom "LIST_TO_SET") "first argument"))
         (tagged (loop for element in elements
                       for index from 0
                       collect (cons index element)))
         (sorted (stable-sort (copy-list tagged)
                              (lambda (a b)
                                (let ((order (%compare-terms (cdr a) (cdr b))))
                                  (if (zerop order)
                                      (< (car a) (car b))
                                      (minusp order))))))
         (unique '())
         (previous nil))
    (dolist (cell sorted)
      (when (or (null previous)
                (not (zerop (%compare-terms (cdr previous) (cdr cell)))))
        (push cell unique))
      (setf previous cell))
    (%unify-emit set
                 (mapcar #'cdr (sort unique #'< :key #'car))
                 environment emit)))

;;; subtract/3, intersection/3, union/3 -- membership tested by unifiability.
;;; Unlike SWI's memberchk, a successful unification's bindings are discarded
;;; rather than propagated, so these never bind variables in the caller's
;;; terms; on ground lists the behavior is identical to SWI.  This divergence
;;; is documented in docs/src/reference/semantics.md.

(defun %unifiable-member-p (item list environment)
  "True when ITEM unifies with some element of LIST, discarding bindings."
  (some (lambda (element) (nth-value 1 (unify item element environment))) list))

(macrolet ((define-set-operation-builtins (&body specifications)
             ;; Each specification is (NAME RESULT OPERATION COMBINATION);
             ;; COMBINATION runs with A and B bound to the resolved lists and
             ;; IN-B-P testing membership in B.
             `(progn
                ,@(loop for (name result operation combination) in specifications
                        collect
                        `(define-builtin (,name set-a set-b ,result)
                             (rulebase environment depth emit)
                           (let ((operation (%iso-atom ,operation))
                                 (a (logic-substitute set-a environment))
                                 (b (logic-substitute set-b environment)))
                             (%require-proper-list a environment operation
                                                   "first argument")
                             (%require-proper-list b environment operation
                                                   "second argument")
                             (flet ((in-b-p (element)
                                      (%unifiable-member-p element b environment)))
                               (%unify-emit ,result ,combination
                                            environment emit))))))))
  (define-set-operation-builtins
    (subtract difference "SUBTRACT" (remove-if #'in-b-p a))
    (intersection common "INTERSECTION" (remove-if-not #'in-b-p a))
    ;; union(A, B, C): the elements of A not in B, followed by all of B.
    (union combined "UNION" (append (remove-if #'in-b-p a) b))))

;;; permutation/2 -- nondeterministic; enumerates permutations of the
;;; instantiated list argument.

(define-iso-builtin (permutation list-term permuted) "PERMUTATION"
  (multiple-value-bind (elements result-variable)
      (cond
        ((%proper-list-p resolved-list-term) (values resolved-list-term permuted))
        ((%proper-list-p resolved-permuted) (values resolved-permuted list-term))
        (t (%raise-instantiation-error
            environment operation
            "permutation/2 requires one proper-list argument")))
    (labels ((permute (remaining accumulator current-environment)
               (if (null remaining)
                   (%unify-emit result-variable (reverse accumulator)
                                current-environment emit)
                   ;; Select each position in a single pass: SKIPPED holds
                   ;; the already-passed elements (reversed), so the rest is
                   ;; built with one REVAPPEND instead of NTH + two SUBSEQs.
                   ;; Handles duplicate and numeric elements without EQ.
                   (let ((skipped '()))
                     (loop for cell on remaining
                           do (permute (revappend skipped (cdr cell))
                                       (cons (car cell) accumulator)
                                       current-environment)
                              (push (car cell) skipped))))))
      (permute elements '() environment))))
