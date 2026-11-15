;;;; Single builtins whose allocation is sized by an argument: finite domain
;;;; intervals, format digit counts, and length/2 on a partial list must be
;;;; bounded by *max-prolog-builtin-output-length*.  Each test lowers the
;;;; limit so a regression allocates only a few thousand cells, never the
;;;; whole heap.

(in-package #:cl-prolog-kit.tests)

(defmacro with-small-output-limit (&body body)
  `(let ((cl-prolog-kit:*max-prolog-builtin-output-length* 100))
     ,@body))

(defun bounded-rulebase ()
  (let ((*standard-input* (make-string-input-stream ""))
        (*standard-output* (make-string-output-stream))
        (*error-output* (make-broadcast-stream)))
    (make-rulebase)))

(defun bounded-query-resource (text)
  "Run TEXT; return the resource_error's resource name, or :NO-ERROR."
  (let ((rulebase (bounded-rulebase)))
    (handler-case
        (progn (prolog-succeeds-p rulebase (%read-prolog-query rulebase "~A" text))
               :no-error)
      (prolog-resource-error (condition)
        (symbol-name (second (second (prolog-exception-term condition))))))))

(defun bounded-query-succeeds-p (text)
  (let ((rulebase (bounded-rulebase)))
    (prolog-succeeds-p rulebase (%read-prolog-query rulebase "~A" text))))

(defun bounded-format-output (text)
  "Run the format goal TEXT and return what it wrote to user_output."
  (let* ((output (make-string-output-stream))
         (rulebase (let ((*standard-output* output)
                         (*standard-input* (make-string-input-stream "")))
                     (make-rulebase))))
    (prolog-succeeds-p rulebase (%read-prolog-query rulebase "~A" text))
    (get-output-stream-string output)))

(deftest fd-interval-is-bounded-by-the-output-limit ()
  (with-small-output-limit
    (is-equal "FD_DOMAIN_SIZE" (bounded-query-resource "X in 1..10000"))
    (is (bounded-query-succeeds-p
         "catch(X in 1..10000, error(resource_error(_), _), true)"))
    (is (bounded-query-succeeds-p "X in 1..100, findall(X, indomain(X), Xs), length(Xs, 100)"))))

(deftest fd-domain-intersection-keeps-common-values ()
  (is (bounded-query-succeeds-p
       "X in 1..10, X in 5..20, findall(X, indomain(X), Xs), Xs == [5,6,7,8,9,10]")))

(deftest format-decimal-digit-count-is-bounded ()
  (with-small-output-limit
    (is-equal "DIGIT_COUNT" (bounded-query-resource "format(\"~10000d\", [1])"))
    (is-equal "3.14" (bounded-format-output "format(\"~2d\", [314])"))))

(deftest format-float-digit-counts-are-bounded ()
  (with-small-output-limit
    (dolist (text '("format(\"~10000e\", [1.0])"
                    "format(\"~10000f\", [1.0])"
                    "format(\"~10000g\", [1.0])"))
      (is-equal "DIGIT_COUNT" (bounded-query-resource text) text))
    (is-equal "1.00" (bounded-format-output "format(\"~2f\", [1.0])"))))

(deftest format-star-digit-count-is-bounded ()
  (with-small-output-limit
    (is-equal "DIGIT_COUNT" (bounded-query-resource "format(\"~*d\", [10000, 1])"))
    (is-equal "DIGIT_COUNT" (bounded-query-resource "format(\"~*e\", [10000, 1.0])"))))

(deftest format-negative-star-digit-count-is-a-domain-error ()
  (dolist (text '("format(\"~*d\", [-1, 1])" "format(\"~*e\", [-1, 1.0])"))
    (is (bounded-query-succeeds-p
         (format nil "catch(~A, error(domain_error(not_less_than_zero, -1), _), true)"
                 text))
        text)))

(deftest length-of-a-partial-list-is-bounded ()
  (with-small-output-limit
    (is-equal "LIST_LENGTH" (bounded-query-resource "length(L, 10000)"))
    (is-equal "LIST_LENGTH" (bounded-query-resource "length([a,b|L], 10000)"))
    (is (bounded-query-succeeds-p "length([a,b|L], 3), L = [_]"))
    ;; A proper list allocates nothing, so its length is not bounded.
    (is (bounded-query-succeeds-p
         "numlist(1, 100, L), append(L, L, LL), length(LL, 200)"))))

(deftest format-generated-output-is-bounded-per-call ()
  (with-small-output-limit
    (dolist (text '("format(\"~60c~60c\", [0'x, 0'y])"
                    "format(\"~60d~60d\", [1, 2])"
                    "format(\"~60e~60f\", [1.0, 2.0])"
                    "format(\"~60|~120|\")"
                    "format(\"~60n~60n\")"))
      (is-equal "FORMAT_OUTPUT_LENGTH" (bounded-query-resource text) text))
    (is (bounded-query-succeeds-p "format(\"~40c~40c\", [0'x, 0'y])"))))
