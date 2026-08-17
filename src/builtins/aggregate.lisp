;;;; aggregate_all/3: count/sum/max/min/bag/set reductions over a goal.
;;;;
;;;; Split out of collection.lisp, which keeps the findall/bagof/setof
;;;; solution-collection primitives this reduces over and loads immediately
;;;; before.

(in-package #:cl-prolog-kit)

;;; aggregate_all/3 -- count/sum/max/min/bag/set reductions over a goal.

(defun %aggregate-numbers (expr-template goal rulebase environment depth)
  "Collect the solutions of EXPR-TEMPLATE over GOAL and evaluate each as an
arithmetic expression, matching SWI's sum/max/min templates (a bare number
evaluates to itself)."
  (mapcar (lambda (value)
            (%evaluate-arithmetic-expression value environment))
          (%collect-template-solutions expr-template goal
                                       rulebase environment depth)))

(defmacro define-aggregate-spec-table (name &body definitions)
  `(defparameter ,name
     (list ,@(loop for (tag . body) in definitions
                   collect `(cons ,tag
                                  (lambda (argument collect numbers)
                                    (declare (cl:ignorable argument collect numbers))
                                    ,@body))))
     "Data table mapping each aggregate_all/3 tag to a reducer returning
\(VALUES RESULT FOUNDP); a false FOUNDP fails the goal instead of unifying.
COLLECT gathers ARGUMENT's solutions, NUMBERS additionally evaluates them."))

(define-aggregate-spec-table +aggregate-specs+
  ("count" (values (length (funcall collect argument)) t))
  ("sum" (values (reduce #'+ (funcall numbers argument) :initial-value 0) t))
  ;; max/min over no solutions fails rather than raising.
  ("max" (let ((found (funcall numbers argument)))
           (values (and found (reduce #'max found)) (and found t))))
  ("min" (let ((found (funcall numbers argument)))
           (values (and found (reduce #'min found)) (and found t))))
  ("bag" (values (funcall collect argument) t))
  ("set" (values (%standard-term-sort-unique (funcall collect argument)) t)))

(define-builtin (aggregate_all spec goal result) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "AGGREGATE_ALL"))
         (resolved-spec (logic-substitute spec environment)))
    (flet ((collect (expression)
             (%collect-template-solutions expression goal rulebase
                                          environment depth))
           (numbers (expression)
             (%aggregate-numbers expression goal rulebase environment depth)))
      (multiple-value-bind (tag argument)
          (cond
            ;; count/0 as the bare atom `count'; any other bare atom is a
            ;; domain error rather than a zero-argument reduction.
            ((and (%term-atom-p resolved-spec)
                  (string-equal (symbol-name resolved-spec) "count"))
             (values "count" t))
            ((and (%proper-list-p resolved-spec) (= (length resolved-spec) 2)
                  (%term-atom-p (first resolved-spec)))
             (values (symbol-name (first resolved-spec)) (second resolved-spec)))
            ((logic-var-p resolved-spec)
             (%raise-instantiation-error
              environment operation
              "aggregate_all/3 template must be instantiated")))
        (let ((reducer (cdr (assoc tag +aggregate-specs+ :test #'string-equal))))
          (unless reducer
            (%raise-domain-error "AGGREGATE_SPEC" resolved-spec environment
                                 operation
                                 "unsupported aggregate_all/3 template"))
          (multiple-value-bind (value foundp)
              (funcall reducer argument #'collect #'numbers)
            (when foundp
              (%unify-emit result value environment emit))))))))
