;;;; ISO operator builtin contract.

(in-package #:cl-prolog-kit.tests)

(defun operator-error-type (goal)
  (query-error-summary (make-rulebase) goal))

(deftest operator-declaration-and-removal ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (cl-prolog-kit::op 450 cl-prolog-kit::yfx custom-operator) :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::current_op 450 cl-prolog-kit::yfx custom-operator)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::op 0 cl-prolog-kit::yfx custom-operator) :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::current_op 450 cl-prolog-kit::yfx custom-operator)
                  :fails)))

(deftest operator-list-update-is-transactional ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (cl-prolog-kit::op 450 cl-prolog-kit::yfx (valid-name 7))
                  :signals prolog-type-error)
    (assert-query rulebase
                  (cl-prolog-kit::current_op 450 cl-prolog-kit::yfx valid-name)
                  :fails)))

(deftest current-op-enumerates-with-cps-backtracking ()
    (let* ((rulebase (make-rulebase))
           (expected (length (cl-prolog-kit::%operator-table-current
                              (cl-prolog-kit::rulebase-operator-table rulebase))))
           (solutions (query-prolog
                       rulebase
                       '(cl-prolog-kit::current_op ?priority ?specifier ?name))))
      (is-equal expected (length solutions))))

  (deftest operator-specifier-lookup-does-not-intern-untrusted-names ()
    (let ((before (package-owned-symbol-count :keyword))
          (operation (cl-prolog-kit::%iso-atom "OP")))
      (loop for name in '("FX" "FY" "XF" "YF" "XFX" "XFY" "YFX")
            for expected in cl-prolog-kit::+operator-specifiers+
            do (is (eq expected
                       (cl-prolog-kit::%operator-specifier
                        (make-symbol name) nil operation))))
      (loop for index below 128
            for name = (format nil
                               "CL-PROLOG-KIT-INVALID-SPECIFIER-~D" index)
            do (is (null (find-symbol name :keyword)))
               (signals-prolog-condition prolog-domain-error
                 (cl-prolog-kit::%operator-specifier
                  (make-symbol name) nil operation))
               (is (null (find-symbol name :keyword))))
      (is-equal before (package-owned-symbol-count :keyword))))

(deftest-table operator-builtins-report-iso-errors ()
  (:equal 'prolog-instantiation-error
          (operator-error-type '(cl-prolog-kit::op ?priority cl-prolog-kit::yfx name)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog-kit::op 1.5 cl-prolog-kit::yfx name)))
  (:equal 'prolog-domain-error
          (operator-error-type '(cl-prolog-kit::op 1201 cl-prolog-kit::yfx name)))
  (:equal 'prolog-domain-error
          (operator-error-type '(cl-prolog-kit::op 500 cl-prolog-kit::invalid name)))
  (:equal 'prolog-instantiation-error
          (operator-error-type '(cl-prolog-kit::op 500 cl-prolog-kit::yfx ?name)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog-kit::current_op bad ?specifier ?name)))
  (:equal 'prolog-permission-error
          (operator-error-type '(cl-prolog-kit::op 500 cl-prolog-kit::xfy cl-prolog-kit::|,|)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog-kit::op 500 42 name)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog-kit::op 500 cl-prolog-kit::yfx 42)))
  (:equal 'prolog-instantiation-error
          (operator-error-type
           '(cl-prolog-kit::op 450 cl-prolog-kit::yfx (valid-name ?x)))))

(deftest-queries char-conversion-builtins ((make-rulebase))
  ((cl-prolog-kit::current_char_conversion ?from ?to) :fails)
  ((cl-prolog-kit::char_conversion cl-prolog-kit::a cl-prolog-kit::b) :succeeds)
  ((cl-prolog-kit::char_conversion cl-prolog-kit::x cl-prolog-kit::y) :succeeds)
  ((cl-prolog-kit::current_char_conversion cl-prolog-kit::b ?to) :fails)
  ;; Mapping a character to itself removes its conversion.
  ((cl-prolog-kit::char_conversion cl-prolog-kit::a cl-prolog-kit::a) :succeeds)
  ((cl-prolog-kit::current_char_conversion cl-prolog-kit::a ?to) :fails)
  ((cl-prolog-kit::char_conversion ?from cl-prolog-kit::b) :signals)
  ((cl-prolog-kit::char_conversion ab cl-prolog-kit::b) :signals)
  ((cl-prolog-kit::char_conversion cl-prolog-kit::a 7) :signals))

(deftest char-conversion-enumeration-reflects-one-rulebase ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (cl-prolog-kit::char_conversion cl-prolog-kit::a cl-prolog-kit::b)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::char_conversion cl-prolog-kit::x cl-prolog-kit::y)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::current_char_conversion ?from ?to)
                  :ordered (((?from . cl-prolog-kit::a) (?to . cl-prolog-kit::b))
                      ((?from . cl-prolog-kit::x) (?to . cl-prolog-kit::y))))
    (assert-query rulebase (cl-prolog-kit::current_char_conversion cl-prolog-kit::a ?to)
                  :ordered (((?to . cl-prolog-kit::b))))))
