;;;; The arithmetic function tables: the evaluation error condition, operand
;;;; type guards, magnitude limits, and the unary/binary/constant dispatch
;;;; tables keyed by ISO functor name.
;;;;
;;;; Split out of arithmetic.lisp, which keeps the expression evaluator and the
;;;; surface builtins (is/2, the comparisons, between/3, succ/2, plus/3) that
;;;; look up these tables and loads immediately after.

(in-package #:cl-prolog-kit)

(define-contextual-error-condition arithmetic-evaluation-error (prolog-evaluation-error)
  (expression arithmetic-error-expression)
  (reason arithmetic-error-reason)
  "Cannot evaluate Prolog arithmetic expression ~S: ~A."
  "Signalled when a Prolog arithmetic expression cannot be evaluated.")

(defun %arithmetic-error (expression reason &rest arguments)
  (let ((message (apply #'format nil reason arguments)))
    (error 'arithmetic-evaluation-error
           :expression expression
           :reason message
           :term (%iso-error-term (%iso-term "EVALUATION_ERROR" (%iso-atom "UNDEFINED"))
                                  (%iso-atom "ARITHMETIC") message)
           :environment nil)))

(defun %prolog-number-p (value)
  "Return true for numeric types representable by ISO Prolog arithmetic."
  (or (integerp value) (floatp value)))

(macrolet ((define-arithmetic-operand-guards (&body specifications)
             ;; Each specification is (NAME ACCEPTED-P ISO-TYPE MESSAGE), where
             ;; ACCEPTED-P is a form over VALUE.
             `(progn
                ,@(loop for (name accepted-p type message) in specifications
                        collect
                        `(defun ,name (value &optional environment)
                           (unless ,accepted-p
                             (%raise-type-error ,type value environment
                                                (%iso-atom "ARITHMETIC")
                                                ,message)))))))
  (define-arithmetic-operand-guards
    (%require-integer (integerp value) "INTEGER"
                      "integer operand required")
    (%require-prolog-number (%prolog-number-p value) "EVALUABLE"
                            "integer or float operand required")
    (%require-real (and (%prolog-number-p value) (realp value)) "NUMBER"
                   "real operand required")))

(progn
  (defparameter *max-prolog-arithmetic-exponent-magnitude*
    *max-prolog-numeric-lexeme-length*
    "Maximum absolute exponent accepted by Prolog arithmetic.")
  (defparameter *max-prolog-arithmetic-result-bits*
    (ceiling (* *max-prolog-numeric-lexeme-length* 3322) 1000)
    "Maximum estimated bit length of an integer arithmetic result.")
  (defun %check-nonzero-divisor (expression divisor)
    (when (zerop divisor)
      (%raise-evaluation-error "ZERO_DIVISOR" nil (%iso-atom "ARITHMETIC")
                               (format nil "division by zero in ~S" expression))))
  (defun %bounded-expt (base exponent expression)
    (declare (ignore expression))
    (when (and (realp exponent)
               (> (abs exponent)
                  *max-prolog-arithmetic-exponent-magnitude*))
      (%raise-resource-error
       "EXPONENT_MAGNITUDE" nil (%iso-atom "ARITHMETIC")
       "arithmetic exponent exceeds the configured magnitude limit"))
    (when (and (integerp base)
               (integerp exponent)
               (not (zerop exponent))
               (> (abs base) 1))
      (let* ((base-bits (integer-length (abs base)))
             (power (abs exponent))
             (growth-bits (max 1 (1- base-bits))))
        (when (or (> base-bits *max-prolog-arithmetic-result-bits*)
                  (> power
                     (floor *max-prolog-arithmetic-result-bits*
                            growth-bits)))
          (%raise-resource-error
           "INTEGER_SIZE" nil (%iso-atom "ARITHMETIC")
           "arithmetic result exceeds the configured size limit"))))
    (expt base exponent))
  (defun %integer-preserving-expt (base exponent expression)
    "Evaluate ISO 13211-1 9.3.10's `^', which keeps an integer result integral.

Raising an integer other than 1, 0 or -1 to a negative integer power has no
integer value, which 9.3.10.3 makes a type_error(float, Base) rather than an
approximation."
    (when (and (integerp base)
               (integerp exponent)
               (minusp exponent)
               (not (member base '(-1 0 1))))
      (%raise-type-error "FLOAT" base nil (%iso-atom "ARITHMETIC")
                         "^ with a negative integer power needs a float base"))
    (%bounded-expt base exponent expression))
  (defun %bounded-ash (integer count)
    "Arithmetic shift bounded like %BOUNDED-EXPT: a left shift whose result
would exceed the configured bit size raises resource_error rather than
allocating an unbounded bignum."
    (when (and (plusp count)
               (> (+ (integer-length (abs integer)) count)
                  *max-prolog-arithmetic-result-bits*))
      (%raise-resource-error
       "INTEGER_SIZE" nil (%iso-atom "ARITHMETIC")
       "arithmetic result exceeds the configured size limit"))
    (ash integer count)))

(defun %arithmetic-operator-key (operator table)
  (when (symbolp operator)
    (let ((entry
            (find (symbol-name operator) table
                  :key (lambda (candidate)
                         (symbol-name (car candidate)))
                  :test #'string-equal)))
      (and entry (car entry)))))

(defmacro define-arithmetic-table (name arity &body definitions)
  `(defparameter ,name
     (list ,@(loop for (operator parameters . body) in definitions
                   collect `(cons ,operator
                                  (lambda ,parameters
                                    (declare (cl:ignorable expression))
                                    ,@body))))
     ,(format nil "Data table for supported ~D-argument arithmetic functions." arity)))

(define-arithmetic-table *unary-arithmetic-functions* 1
  (:+ (value expression) value)
  (:- (value expression) (- value))
  (:abs (value expression) (abs value))
  (:sign (value expression) (signum value))
  (:signum (value expression) (signum value))
  (:truncate (value expression) (truncate value))
  (:round (value expression) (round value))
  (:ceiling (value expression) (ceiling value))
  (:floor (value expression) (floor value))
  (:float (value expression) (float value 1.0d0))
  (:float_integer_part (value expression)
    (%require-real value)
    (float (truncate value) value))
  (:float_fractional_part (value expression)
    (%require-real value)
    (- value (truncate value)))
  (:sqrt (value expression)
    (%require-real value)
    (when (minusp value)
      (%arithmetic-error expression "square root is undefined for ~S" value))
    (sqrt value))
  (:exp (value expression) (exp value))
  (:log (value expression)
    (%require-real value)
    (unless (plusp value)
      (%arithmetic-error expression "logarithm is undefined for ~S" value))
    ;; Call LOG through its function object: SBCL's compile-time interval
    ;; derivation for LOG evaluates float bounds, and on hosts with broken
    ;; FP-trap delivery that would otherwise make evaluation hang
    ;; COMPILE-FILE.  The dynamic call skips the derivation entirely.
    (funcall (symbol-function 'cl:log) value))
  (:sin (value expression) (sin value))
  (:cos (value expression) (cos value))
  (:tan (value expression) (tan value))
  (:asin (value expression) (asin value))
  (:acos (value expression) (acos value))
  (:atan (value expression) (atan value))
  (:|\\| (value expression)
    (%require-integer value)
    (lognot value))
  (:msb (value expression)
    (%require-integer value)
    (unless (plusp value)
      (%arithmetic-error expression "msb is undefined for ~S" value))
    (1- (integer-length value)))
  (:lsb (value expression)
    (%require-integer value)
    (unless (plusp value)
      (%arithmetic-error expression "lsb is undefined for ~S" value))
    (1- (integer-length (logand value (- value)))))
  (:popcount (value expression)
    (%require-integer value)
    (unless (>= value 0)
      (%arithmetic-error expression "popcount is undefined for ~S" value))
    (logcount value)))

(define-arithmetic-table *binary-arithmetic-functions* 2
  (:+ (left right expression) (+ left right))
  (:- (left right expression) (- left right))
  (:* (left right expression) (* left right))
  (:/ (left right expression)
    (%require-real left) (%require-real right)
    (%check-nonzero-divisor expression right)
    ;; ISO 13211-1 9.1.3: dividing two integers exactly yields the integer
    ;; quotient; only an inexact one becomes a float.
    (if (and (integerp left) (integerp right) (zerop (mod left right)))
        (truncate left right)
        (/ (float left 1.0d0) (float right 1.0d0))))
  (:// (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (truncate left right))
  (:div (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (floor left right))
  (:rem (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (rem left right))
  (:mod (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (mod left right))
  (:min (left right expression) (min left right))
  (:max (left right expression) (max left right))
  ;; ISO 13211-1 distinguishes the two powers: 9.3.1's `**' is float power, so
  ;; `2 ** 3' is 8.0, while 9.3.10's `^' preserves integers, so `2 ^ 3' is 8.
  (:** (left right expression)
    (%require-real left) (%require-real right)
    (float (%bounded-expt left right expression) 1.0d0))
  (:^ (left right expression) (%integer-preserving-expt left right expression))
  (:|/\\| (left right expression)
    (%require-integer left) (%require-integer right)
    (logand left right))
  (:|\\/| (left right expression)
    (%require-integer left) (%require-integer right)
    (logior left right))
  (:xor (left right expression)
    (%require-integer left) (%require-integer right)
    (logxor left right))
  (:<< (left right expression)
    (%require-integer left) (%require-integer right)
    (%bounded-ash left right))
  (:>> (left right expression)
    (%require-integer left) (%require-integer right)
    (%bounded-ash left (- right)))
  (:atan (left right expression)
    (%require-real left) (%require-real right)
    (atan (float left 1.0d0) (float right 1.0d0)))
  (:atan2 (left right expression)
    (%require-real left) (%require-real right)
    (atan (float left 1.0d0) (float right 1.0d0)))
  (:gcd (left right expression)
    (%require-integer left) (%require-integer right)
    (gcd left right)))

(defparameter *arithmetic-constants*
  (list (cons :pi pi)
        (cons :e (exp 1.0d0)))
  "Data table for zero-argument arithmetic constants.")
