(in-package #:cl-prolog-kit)

(defun %arithmetic-function-table (arity)
  (case arity
    (1 *unary-arithmetic-functions*)
    (2 *binary-arithmetic-functions*)))

(defun %require-arithmetic-function (operator arity expression environment)
  (let* ((table (%arithmetic-function-table arity))
         (entry (and table
                     (assoc (%arithmetic-operator-key operator table) table))))
    (unless entry
      (%raise-type-error
       "EVALUABLE" (%iso-term "/" operator arity) environment
       (%iso-atom "ARITHMETIC")
       (format nil "~S is not an evaluable arithmetic function" expression)))
    (values table entry)))

(defun %call-arithmetic-function (entry arguments expression)
  (handler-case
      (let ((result (apply (cdr entry) (append arguments (list expression)))))
        (unless (%prolog-number-p result)
          (%arithmetic-error expression
                             "arithmetic result is not an integer or float: ~S"
                             result))
        result)
    ;; ISO 13211-1 9.1.4.2 names the float exceptions, so they must reach Prolog
    ;; code as catchable evaluation_error/1 terms rather than as a host
    ;; condition -- `2.0 ** 1e300' overflows a double, and a program is entitled
    ;; to catch that.
    (cl:floating-point-overflow ()
      (%raise-evaluation-error "FLOAT_OVERFLOW" nil (%iso-atom "ARITHMETIC")
                               "arithmetic result overflows a float"))
    (cl:floating-point-underflow ()
      (%raise-evaluation-error "UNDERFLOW" nil (%iso-atom "ARITHMETIC")
                               "arithmetic result underflows a float"))
    (cl:division-by-zero ()
      (%raise-evaluation-error "ZERO_DIVISOR" nil (%iso-atom "ARITHMETIC")
                               "division by zero"))
    (cl:arithmetic-error (condition)
      (%arithmetic-error expression "host arithmetic failure: ~A" condition))))

(defun %evaluate-binary-arithmetic (entry left right expression)
  (%call-arithmetic-function entry (list left right) expression))

(defun %evaluate-unary-arithmetic (entry argument expression)
  (%call-arithmetic-function entry (list argument) expression))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
             (let ((constant-entry
                     (assoc (%arithmetic-operator-key
                             term *arithmetic-constants*)
                            *arithmetic-constants*)))
               (cond
                 ((numberp term)
                  (%require-prolog-number term environment)
                  term)
                 (constant-entry
                  (cdr constant-entry))
                 ((logic-var-p term)
                  (%raise-instantiation-error
                   environment (%iso-atom "ARITHMETIC")
                   "arithmetic expression is not ground"))
                 ((not (consp term))
                  ;; ISO 13211-1 7.12.2 (b): the culprit of an evaluable type
                  ;; error is the indicator `Name/Arity', so a bare atom is
                  ;; reported as `foo/0' rather than as `foo'.
                  (%raise-type-error
                   "EVALUABLE"
                   (if (%term-atom-p term) (%iso-term "/" term 0) term)
                   environment (%iso-atom "ARITHMETIC")
                   "number or arithmetic expression required"))
                 ((not (%proper-list-p term))
                  (%raise-type-error
                   "EVALUABLE" term environment
                   (%iso-atom "ARITHMETIC")
                   "proper arithmetic expression required"))
                 (t
                  (let ((operator (first term))
                        (arguments (rest term)))
                    (multiple-value-bind (table entry)
                        (%require-arithmetic-function
                         operator (length arguments) term environment)
                      (declare (cl:ignore table))
                      (case (length arguments)
                        (1 (%evaluate-unary-arithmetic
                            entry (evaluate (first arguments)) term))
                        (2 (%evaluate-binary-arithmetic
                            entry
                            (evaluate (first arguments))
                            (evaluate (second arguments))
                            term))))))))))
      (evaluate resolved))))

(define-builtin (is result expression) (rulebase environment depth emit)
  (%unify-emit result
               (%evaluate-arithmetic-expression expression environment)
               environment emit))

(defmacro define-arithmetic-comparison (&body definitions)
  "Define ISO arithmetic comparison builtins.  Each of DEFINITIONS is
(NAME CL-OPERATOR); the builtin succeeds once, without bindings, when
CL-OPERATOR holds between the evaluated LEFT and RIGHT expressions."
  `(progn
     ,@(loop for (name operator) in definitions
             collect `(define-builtin (,name left right) (rulebase environment depth emit)
                        (when (,operator (%evaluate-arithmetic-expression left environment)
                                         (%evaluate-arithmetic-expression right environment))
                          (funcall emit environment))))))

(define-arithmetic-comparison
  (|=:=| =)
  (|=\\=| /=)
  (< <)
  (=< <=)
  (> >)
  (>= >=))

(define-iso-builtin (between low high value) "BETWEEN"
  (when (or (logic-var-p resolved-low) (logic-var-p resolved-high))
    (%raise-instantiation-error environment operation
                                "lower and upper bounds must be instantiated"))
  (dolist (argument (list resolved-low resolved-high resolved-value))
    (unless (or (logic-var-p argument) (integerp argument))
      (%raise-type-error "INTEGER" argument environment operation
                         "arguments must be integers")))
  (if (logic-var-p resolved-value)
      (loop for candidate from resolved-low to resolved-high
            do (%unify-emit value candidate environment emit))
      (when (<= resolved-low resolved-value resolved-high)
        (funcall emit environment))))

(define-iso-builtin (succ predecessor successor) "SUCC"
  (when (and (logic-var-p resolved-predecessor)
             (logic-var-p resolved-successor))
    (%raise-instantiation-error environment operation
                                "one argument must be instantiated"))
  (dolist (argument (list resolved-predecessor resolved-successor))
    (%require-bounded-integer argument environment operation "arguments"
                              :allow-variable t))
  (cond
    ((not (logic-var-p resolved-predecessor))
     (%unify-emit successor (1+ resolved-predecessor) environment emit))
    ((plusp resolved-successor)
     (%unify-emit predecessor (1- resolved-successor) environment emit))))

(define-iso-builtin (plus a b c) "PLUS"
  ;; The integer relation A + B =:= C, usable in any single-unknown mode.
  (flet ((check (argument)
           (unless (or (logic-var-p argument) (integerp argument))
             (%raise-type-error "INTEGER" argument environment operation
                                "plus/3 arguments must be integers"))))
    (check resolved-a) (check resolved-b) (check resolved-c)
    (cond
      ((and (integerp resolved-a) (integerp resolved-b))
       (%unify-emit c (+ resolved-a resolved-b) environment emit))
      ((and (integerp resolved-a) (integerp resolved-c))
       (%unify-emit b (- resolved-c resolved-a) environment emit))
      ((and (integerp resolved-b) (integerp resolved-c))
       (%unify-emit a (- resolved-c resolved-b) environment emit))
      (t (%raise-instantiation-error
          environment operation
          "plus/3 requires at least two instantiated arguments")))))
