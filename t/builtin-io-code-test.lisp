;;;; Query-level contract for ISO character-code I/O.

(in-package #:cl-prolog-kit.tests)

(deftest io-code-builtins-use-current-streams ()
  (with-io-rulebase (rulebase input output) "A"
    (assert-query rulebase (cl-prolog-kit::peek_code ?code) :ordered (((?code . 65))))
    (assert-query rulebase (cl-prolog-kit::get_code ?code) :ordered (((?code . 65))))
    (assert-query rulebase (cl-prolog-kit::peek_code ?eof) :ordered (((?eof . -1))))
    (assert-query rulebase (cl-prolog-kit::get_code ?eof) :ordered (((?eof . -1))))
    (assert-query rulebase (cl-prolog-kit::put_code 66) :succeeds)
    (is-equal "B" (get-output-stream-string output))))

(deftest io-code-builtins-use-explicit-streams ()
  (with-io-rulebase (rulebase input output) "C"
    (assert-query rulebase
                  (cl-prolog-kit::peek_code cl-prolog-kit::user_input ?code)
                  :ordered (((?code . 67))))
    (assert-query rulebase
                  (cl-prolog-kit::get_code cl-prolog-kit::user_input ?code)
                  :ordered (((?code . 67))))
    (assert-query rulebase
                  (cl-prolog-kit::put_code cl-prolog-kit::user_output 68)
                  :succeeds)
    (is-equal "D" (get-output-stream-string output))))

(deftest io-code-end-of-stream-state-progresses-from-at-to-past ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::end_of_stream ?state))
     :ordered (((?state . cl-prolog-kit::at))))
    (assert-query rulebase (cl-prolog-kit::get_code ?code)
                  :ordered (((?code . -1))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::end_of_stream ?state))
     :ordered (((?state . cl-prolog-kit::past))))
    (assert-query rulebase (cl-prolog-kit::get_code ?code)
                  :ordered (((?code . -1))))))

(deftest-table io-code-builtins-report-iso-errors ()
  (:signals (query-prolog (make-rulebase) '(cl-prolog-kit::put_code ?code)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog-kit::put_code atom)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog-kit::put_code -1))))
