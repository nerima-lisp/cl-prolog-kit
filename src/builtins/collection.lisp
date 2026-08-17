(in-package #:cl-prolog-kit)

(defun %existential-marker-p (goal)
  "True when GOAL is a `^'/2 term, whatever spelling reached the engine."
  (and (%proper-list-p goal)
       (= (length goal) 3)
       (symbolp (first goal))
       (string= "^" (%atom-text (first goal)))))

(defun %collect-existential-variables (goal variables)
  "Add every variable `^'-quantified anywhere in GOAL to VARIABLES.

ISO 13211-1 8.10.2 scopes `^' over the whole goal, not just its front: in
`(Y^(X=1 ; Y=1) ; X=3)' the Y is existential even though the goal itself is a
disjunction.  Only the control constructs a goal is built from are descended
into -- a `^' inside an ordinary argument is data, not a quantifier."
  (cond
    ((%existential-marker-p goal)
     (dolist (variable (%collect-variables (second goal)))
       (pushnew variable variables :test #'eq))
     (%collect-existential-variables (third goal) variables))
    ((and (%proper-list-p goal) (symbolp (first goal)))
     (if (member (%atom-text (first goal))
                 '("," ";" "->" "*->" "and" "or"
                   "if-then-else" "soft-if-then-else")
                 :test #'string=)
         (dolist (argument (rest goal) variables)
           (setf variables (%collect-existential-variables argument variables)))
         variables))
    (t variables)))

(defun %strip-existential-quantifiers (goal)
  "Return GOAL without its leading `^'/2 wrappers, and every variable the goal
existentially quantifies anywhere within it."
  (let ((variables (%collect-existential-variables goal '())))
    (loop while (%existential-marker-p goal)
          do (setf goal (third goal)))
    (values goal (nreverse variables))))

(defun %collect-template-solutions (template goal rulebase environment depth)
  "Collect copied TEMPLATE instances for every proof of GOAL."
  (let ((solutions '()))
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment depth
     (lambda (extended)
       (push (%freshen-term (logic-substitute template extended)
                            (make-hash-table :test #'eq))
             solutions)))
    (nreverse solutions)))

(defun %bagof-free-variables (template goal existential-variables)
  "Return the free variables by which BAGOF and SETOF group solutions."
  (let ((template-variables (%collect-variables template)))
    (remove-if (lambda (variable)
                 (or (member variable template-variables :test #'eq)
                     (member variable existential-variables :test #'eq)))
               (%collect-variables goal))))

(defun %collect-grouped-solutions (template goal free-variables
                                    rulebase environment depth)
  "Collect (KEY . TEMPLATE) pairs while preserving proof order."
  (let ((solutions '()))
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment depth
     (lambda (extended)
       (let ((table (make-hash-table :test #'eq)))
         (push (cons (%freshen-term
                      (mapcar (lambda (variable)
                                (logic-substitute variable extended))
                              free-variables)
                      table)
                     (%freshen-term (logic-substitute template extended) table))
               solutions))))
    (nreverse solutions)))

(defun %group-adjacent-by (items same-group-p)
  "Split ITEMS into runs of adjacent elements, starting a new run whenever
SAME-GROUP-P rejects the pair (run head, current item).  Runs and the items
within them keep their original order."
  (let ((groups '())
        (current '())
        (head nil))
    (dolist (item items)
      (if (and current (funcall same-group-p head item))
          (push item current)
          (progn
            (when current (push (nreverse current) groups))
            (setf head item
                  current (list item)))))
    (when current (push (nreverse current) groups))
    (nreverse groups)))

(defun %partition-solution-groups (solutions)
  "Partition SOLUTIONS by variant-equivalent keys in standard term order."
  (let* ((entries
           (stable-sort
            (mapcar (lambda (solution)
                      (list (%canonicalize-variant (car solution))
                            (car solution)
                            (cdr solution)))
                    solutions)
            (lambda (left right)
              (minusp (%compare-terms (first left) (first right))))))
         (groups (%group-adjacent-by
                  entries
                  (lambda (head entry)
                    (zerop (%compare-terms (first head) (first entry)))))))
    (stable-sort (mapcar (lambda (group)
                           (cons (second (first group))
                                 (mapcar #'third group)))
                         groups)
                 #'%prolog-term< :key #'car)))

(defun %emit-solution-group (free-variables key values bag environment emit)
  "Unify one grouped KEY and VALUES with the caller and emit on success."
  (let ((extended environment)
        (ok t))
    (loop for variable in free-variables
          for value in key
          while ok
          do (multiple-value-setq (extended ok)
               (unify variable value extended)))
    (when ok
      (%unify-emit bag values extended emit))))

(defun %emit-bagof-solutions (template quantified-goal bag setp
                              rulebase environment depth emit)
  (multiple-value-bind (goal existential-variables)
      (%strip-existential-quantifiers quantified-goal)
    ;; ISO 13211-1 8.10.2.3: the goal is converted to a body first, so
    ;; `setof(X, X^(true ; 4), L)' blames the whole `(true ; 4)' and runs none
    ;; of it -- the same conversion `call/1' does, and the same culprit rule.
    (%check-callable-body goal goal environment (%iso-atom "BAGOF"))
    (let* ((free-variables
             (%bagof-free-variables template goal existential-variables))
           (solutions
             (%collect-grouped-solutions template goal free-variables
                                         rulebase environment depth)))
      (dolist (group (%partition-solution-groups solutions))
        (let ((values (cdr group)))
          (when setp
            (setf values (%standard-term-sort-unique values)))
          (%emit-solution-group free-variables (car group) values bag
                                environment emit))))))

(define-builtin (findall template goal bag) (rulebase environment depth emit)
  (%unify-emit bag
               (%collect-template-solutions template goal rulebase environment depth)
               environment emit))

(define-builtin (findall template goal bag tail)
    (rulebase environment depth emit)
  (let ((solutions
          (%collect-template-solutions template goal rulebase environment depth)))
    (%unify-emit bag
                 (append solutions tail)
                 environment emit)))

(define-builtin (bagof template goal bag) (rulebase environment depth emit)
  (%emit-bagof-solutions template goal bag nil rulebase environment depth emit))

(define-builtin (setof template goal bag) (rulebase environment depth emit)
  (%emit-bagof-solutions template goal bag t rulebase environment depth emit))

(define-builtin (:when test &rest variables) (rulebase environment depth emit)
  ;; TEST receives the solved value of each of VARIABLES.  The DSL compiles
  ;; (:when EXPR) guards into such functions; hand-written queries must pass
  ;; a function object too.
  (unless (functionp test)
    (%invalid-goal (list* :when test variables)
                   ":WHEN needs a guard function, got ~S (use the PROLOG ~
                    macro to compile expression guards)" test))
  (when (apply test
               (mapcar (lambda (variable)
                         (logic-substitute variable environment))
                       variables))
    (funcall emit environment)))
