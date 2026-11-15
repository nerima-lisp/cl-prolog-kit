;;;; Float literals outside the double-float range must surface as Prolog
;;;; syntax errors from every reader entry point, never as a Lisp reader error.

(in-package #:cl-prolog-kit.tests)

(defun float-query-succeeds-p (text)
  (let ((rulebase (make-rulebase)))
    (prolog-succeeds-p rulebase (%read-prolog-query rulebase "~A" text))))

(deftest term-to-atom-reports-an-overflowing-float-as-a-syntax-error ()
  (is (float-query-succeeds-p
       "catch(term_to_atom(_, '1.0e400'), error(syntax_error(_), _), true)")))

(deftest read-term-from-atom-reports-an-overflowing-float-as-a-syntax-error ()
  (is (float-query-succeeds-p
       "catch(read_term_from_atom('1e400', _, []), error(syntax_error(_), _), true)")))

(deftest term-string-reports-an-overflowing-float-as-a-syntax-error ()
  (is (float-query-succeeds-p
       "catch(term_string(_, \"1.0e400\"), error(syntax_error(_), _), true)")))

(deftest read-reports-an-overflowing-float-as-a-syntax-error ()
  (let* ((*standard-input* (make-string-input-stream "1e400."))
         (rulebase (make-rulebase)))
    (is (prolog-succeeds-p
         rulebase
         (%read-prolog-query
          rulebase "catch(read(_), error(syntax_error(_), _), true)")))))

(deftest consult-reports-an-overflowing-float-as-a-parse-error ()
  (handler-case
      (progn (consult-prolog "x(1.0e400)." (make-rulebase))
             (is nil "consulting an overflowing float must fail"))
    (cl-prolog-kit::prolog-parse-error ()
      (is t))
    (error (condition)
      (is nil (format nil "expected a parse error, got ~S" (type-of condition))))))

(deftest in-range-float-literals-still-parse ()
  (is (float-query-succeeds-p "X = 1.5e300, X > 1.0e299"))
  (is (float-query-succeeds-p "term_to_atom(T, '2.5e-3'), T =:= 0.0025")))
