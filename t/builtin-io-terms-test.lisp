;;;; Term I/O semantics: read_term/write_term/write_canonical/read/write/
;;;; writeq variants, char_conversion, and end-of-file reporting for term
;;;; reads.  Defines the shared with-io-rulebase fixture used across this
;;;; file and its siblings; stream lifecycle (open/close, stream_property,
;;;; set_stream_position, current-input/output selection) lives in
;;;; builtin-io-streams-lifecycle-test.lisp, and malformed-input/error-rejection
;;;; coverage lives in builtin-io-open-errors-test.lisp.

(in-package #:cl-prolog-kit.tests)

(defmacro with-io-rulebase ((rulebase input output) input-text &body body)
  `(let* ((,input (make-string-input-stream ,input-text))
          (,output (make-string-output-stream))
          (context (cl-prolog-kit::make-prolog-io-context
                    :input ,input :output ,output
                    :error-output (make-string-output-stream)))
          (,rulebase (make-rulebase :io-context context)))
     (unwind-protect
          (progn ,@body)
       (cl-prolog-kit::%close-all-owned-prolog-streams! context))))

(deftest io-builtins-report-eof-and-reject-unsupported-writer-options ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::end_of_file))))
    (assert-query rulebase
                  (cl-prolog-kit::write_term cl-prolog-kit::user_output hello
                                         ((cl-prolog-kit::quoted maybe)))
                  :signals)))

(deftest-io-queries io-read-term-handles-options ()
  ("reports variables and variable_names for the current stream"
   "pair(X, X, Y)."
   (cl-prolog-kit::read_term
    ?term ((cl-prolog-kit::variables ?variables)
           (cl-prolog-kit::variable_names ?names)))
   :ordered (((?term . (cl-prolog-kit::pair cl-prolog-kit::?x cl-prolog-kit::?x cl-prolog-kit::?y))
              (?variables . (cl-prolog-kit::?x cl-prolog-kit::?y))
              (?names . ((cl-prolog-kit::= #.(cl-prolog-kit:prolog-atom "X") cl-prolog-kit::?x)
                         (cl-prolog-kit::= #.(cl-prolog-kit:prolog-atom "Y") cl-prolog-kit::?y))))))
  ("rejects an unsupported option"
   "ok." (cl-prolog-kit::read_term ?term ((cl-prolog-kit::bogus value))) :signals)
  ("rejects a non-list options argument"
   "ok." (cl-prolog-kit::read_term ?term cl-prolog-kit::not_a_list) :signals)
  ("rejects a malformed option shape"
   "ok." (cl-prolog-kit::read_term ?term (cl-prolog-kit::malformed_shape)) :signals))
(deftest io-read-term-preserves-quoted-question-atoms ()
  (with-io-rulebase (rulebase input output) "'?x'."
    (with-single-query-solution
        (solution solutions rulebase
         (list 'cl-prolog-kit::read_term '?term
               (list (list 'cl-prolog-kit::variables '?variables))))
      (let ((term (logic-substitute '?term solution))
            (variables (logic-substitute '?variables solution)))
        (is (eq (find-package '#:cl-prolog-kit.user-atoms)
                (symbol-package term)))
        (is (not (logic-var-p term)))
        (is (null variables))))))

(deftest io-read-term-reports-named-singletons-only ()
  (with-io-rulebase (rulebase input output) "tuple(X, Y, X, _, Z)."
    (with-single-query-solution
        (solution solutions rulebase
         (list 'cl-prolog-kit::read_term '?term
               (list (list 'cl-prolog-kit::singletons '?singletons))))
      (let ((term (logic-substitute '?term solution))
            (singletons (logic-substitute '?singletons solution)))
        (destructuring-bind (functor x y repeated-x anonymous z) term
          (is (eq 'cl-prolog-kit::tuple functor))
          (is (eq x repeated-x))
          (is (logic-var-p anonymous))
          (is-equal
           (list (list 'cl-prolog-kit::= '#.(cl-prolog-kit:prolog-atom "Y") y)
                 (list 'cl-prolog-kit::= '#.(cl-prolog-kit:prolog-atom "Z") z))
           singletons))))))

(deftest io-read-term-validates-syntax-error-policy ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (assert-query rulebase
                  (cl-prolog-kit::read_term
                   ?term ((cl-prolog-kit::syntax_errors cl-prolog-kit::fail)))
                  :fails))
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog-kit::read_term
                   ?term ((cl-prolog-kit::syntax_errors unsupported)))
                  :signals)))

(deftest io-read-term-syntax-errors-are-catchable-iso-errors ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (is
     (prolog-succeeds-p
      rulebase
      (%read-prolog-query
       rulebase
       "catch(read_term(_, []), error(syntax_error(_), context(read_term, _)), true).")))))

(deftest io-write-term-current-stream-honors-ignore-ops ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   (cl-prolog-kit::+ 1 2)
                   ((cl-prolog-kit::quoted cl-prolog-kit::true)
                    (cl-prolog-kit::ignore_ops cl-prolog-kit::true)
                    (cl-prolog-kit::numbervars cl-prolog-kit::false)))
                  :succeeds)
    (is-equal "'+'(1,2)" (get-output-stream-string output))))

(deftest io-write-term-current-stream-honors-quoted ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   #.(cl-prolog-kit:prolog-atom "Mary Jane")
                   ((cl-prolog-kit::quoted cl-prolog-kit::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   #.(cl-prolog-kit:prolog-atom "Mary Jane")
                   ((cl-prolog-kit::quoted cl-prolog-kit::false)))
                  :succeeds)
    (is-equal "'Mary Jane'Mary Jane" (get-output-stream-string output))))

(deftest io-write-term-explicit-stream-honors-numbervars ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   cl-prolog-kit::user_output
                   (#.(cl-prolog-kit:prolog-atom "$VAR") 25)
                   ((cl-prolog-kit::numbervars cl-prolog-kit::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   cl-prolog-kit::user_output
                   (#.(cl-prolog-kit:prolog-atom "$VAR") 26)
                   ((cl-prolog-kit::numbervars cl-prolog-kit::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   cl-prolog-kit::user_output
                   (#.(cl-prolog-kit:prolog-atom "$VAR") 0)
                   ((cl-prolog-kit::numbervars cl-prolog-kit::false)
                    (cl-prolog-kit::quoted cl-prolog-kit::true)))
                  :succeeds)
    (is-equal "ZA1'$VAR'(0)" (get-output-stream-string output))))

(deftest io-write-term-combines-ignore-ops-and-quoting ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog-kit::write_term
                   (cl-prolog-kit::+ (#.(cl-prolog-kit:prolog-atom "$VAR") 0) #.(cl-prolog-kit:prolog-atom "Mary Jane"))
                   ((cl-prolog-kit::quoted cl-prolog-kit::false)
                    (cl-prolog-kit::ignore_ops cl-prolog-kit::true)
                    (cl-prolog-kit::numbervars cl-prolog-kit::true)))
                  :succeeds)
    (is-equal "+(A,Mary Jane)" (get-output-stream-string output))))

(deftest io-write-term-rejects-invalid-boolean-options ()
  (dolist (option '(cl-prolog-kit::quoted cl-prolog-kit::ignore_ops
                    cl-prolog-kit::numbervars))
    (with-io-rulebase (rulebase input output) ""
      (signals-error
       (query-prolog rulebase
                     `(cl-prolog-kit::write_term hello ((,option maybe))))))))

(deftest-io-variants io-read-write-facades-share-term-semantics
    ((rulebase input output) "pair(X, X).")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog-kit::read ?term)
                 :ordered (((?term . (cl-prolog-kit::pair
                                cl-prolog-kit::?x cl-prolog-kit::?x)))))
   (assert-query rulebase
                 (cl-prolog-kit::write (#.(cl-prolog-kit:prolog-atom "$VAR") 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog-kit::writeq #.(cl-prolog-kit:prolog-atom "Mary Jane"))
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output)))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog-kit::read cl-prolog-kit::user_input ?term)
                 :ordered (((?term . (cl-prolog-kit::pair
                                cl-prolog-kit::?x cl-prolog-kit::?x)))))
   (assert-query rulebase
                 (cl-prolog-kit::write cl-prolog-kit::user_output
                                   (#.(cl-prolog-kit:prolog-atom "$VAR") 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog-kit::writeq cl-prolog-kit::user_output
                                    #.(cl-prolog-kit:prolog-atom "Mary Jane"))
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output))))

(deftest io-char-conversion-applies-to-unquoted-read-text ()
  (with-io-rulebase (rulebase input output) "aaa. 'aaa'. aaa."
    (assert-query rulebase
                  (cl-prolog-kit::char_conversion cl-prolog-kit::a cl-prolog-kit::b)
                  :succeeds)
    ;; The conversion table is inert until the flag is switched on.
    (assert-query rulebase (cl-prolog-kit::read_term ?term ())
                  :ordered (((?term . cl-prolog-kit::aaa))))
    (assert-query rulebase
                  (cl-prolog-kit::set_prolog_flag cl-prolog-kit::char_conversion on)
                  :succeeds)
    ;; Quoted atoms are exempt from conversion.
    (assert-query rulebase (cl-prolog-kit::read_term ?term ())
                  :ordered (((?term . cl-prolog-kit::aaa))))
    (assert-query rulebase (cl-prolog-kit::read_term ?term ())
                  :ordered (((?term . cl-prolog-kit::bbb))))))

(deftest io-write-canonical-round-trips-quoted-structure ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog-kit::write_canonical
                   (cl-prolog-kit::foo (cl-prolog-kit::bar 1 2) #.(cl-prolog-kit:prolog-atom "a b")))
                  :succeeds)
    (let ((text (get-output-stream-string output)))
      (is (search "'a b'" text)
          "write_canonical must quote atoms that need quoting")
      (is-equal '(cl-prolog-kit::foo (cl-prolog-kit::bar 1 2) #.(cl-prolog-kit:prolog-atom "a b"))
                (read-prolog-term (concatenate 'string text " ."))))))

(deftest io-read-resource-errors-remain-catchable-for-all-syntax-policies ()
  (dolist
      (query-source
       (list
        "catch(read(_), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, []), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, [syntax_errors(fail)]), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, [syntax_errors(quiet)]), error(resource_error(identifier_length), _), true)."))
    (with-io-rulebase (rulebase input output) "toolong."
      (let ((query (%read-prolog-query rulebase query-source)))
        (let ((*max-prolog-identifier-length* 1))
          (is (prolog-succeeds-p rulebase query)))))))

(deftest io-dual-builtin-macro-rejects-malformed-clauses ()
  (signals-error
    (macroexpand-1
     '(cl-prolog-kit::%define-io-dual-builtin
       (bogus_io_test_builtin () () "BOGUS")
       (rulebase environment depth emit)
       :wrong-key form
       :explicit form))))

(deftest io-read-term-and-peek-char-report-end-of-file ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog-kit::read_term ?term ())
                  :ordered (((?term . cl-prolog-kit::end_of_file))))
    (assert-query rulebase (cl-prolog-kit::peek_char ?character)
                  :ordered (((?character . cl-prolog-kit::end_of_file))))))

(deftest io-explicit-stream-variants-cover-read-write-and-control-goals ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog-kit::rulebase-io-context rulebase))
          (extra-input (make-string-input-stream "term_a."))
          (sink (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context extra-input :read :alias 'cl-prolog-kit::explicit_input)
      (cl-prolog-kit::%register-prolog-stream!
       context sink :write :alias 'cl-prolog-kit::explicit_output)
      (assert-query rulebase
                    (cl-prolog-kit::read_term
                     cl-prolog-kit::explicit_input ?term ())
                    :ordered (((?term . cl-prolog-kit::term_a))))
      (assert-query rulebase
                    (cl-prolog-kit::write_canonical
                     cl-prolog-kit::explicit_output cl-prolog-kit::term_a)
                    :succeeds)
      (assert-query rulebase (cl-prolog-kit::nl cl-prolog-kit::explicit_output)
                    :succeeds)
      (assert-query rulebase (cl-prolog-kit::flush_output cl-prolog-kit::explicit_output)
                    :succeeds))))
