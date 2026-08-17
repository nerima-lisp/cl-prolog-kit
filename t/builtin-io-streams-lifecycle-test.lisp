;;;; Stream lifecycle: open/close, stream_property, set_stream_position,
;;;; char/byte builtins over binary and text streams, current-input/
;;;; current-output selection, and rulebase-copy isolation.  Uses the
;;;; with-io-rulebase fixture defined in builtin-io-terms-test.lisp; term
;;;; reading/writing semantics live there, and malformed-input/error-
;;;; rejection coverage lives in builtin-io-open-errors-test.lisp.

(in-package #:cl-prolog-kit.tests)

(deftest io-builtins-use-rulebase-standard-streams ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog-kit::current_input ?stream) :succeeds)
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::a))))
    (assert-query rulebase (cl-prolog-kit::at_end_of_stream) :succeeds)
    (assert-query rulebase (cl-prolog-kit::put_char z) :succeeds)
    (assert-query rulebase (cl-prolog-kit::nl) :succeeds)
    (assert-query rulebase (cl-prolog-kit::flush_output) :succeeds)
    (is-equal (format nil "z~%") (get-output-stream-string output))))

(deftest io-builtins-support-explicit-streams ()
  (with-io-rulebase (rulebase input output) "x"
    (assert-query rulebase
                  (cl-prolog-kit::get_char cl-prolog-kit::user_input ?character)
                  :ordered (((?character . cl-prolog-kit::x))))
    (assert-query rulebase
                  (cl-prolog-kit::put_char cl-prolog-kit::user_output q)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::at_end_of_stream cl-prolog-kit::user_input)
                  :succeeds)))

(deftest io-end-of-stream-state-progresses-from-at-to-past ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::end_of_stream ?state))
     :ordered (((?state . cl-prolog-kit::at))))
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::end_of_stream ?state))
     :ordered (((?state . cl-prolog-kit::past))))
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::end_of_file))))))

(deftest rulebase-copies-isolate-io-context ()
  (with-io-rulebase (rulebase input output) ""
    (let ((copy (cl-prolog-kit::%copy-rulebase rulebase)))
      (is (not (eq (cl-prolog-kit::rulebase-io-context rulebase)
                   (cl-prolog-kit::rulebase-io-context copy)))))))

(deftest io-close-one-argument-closes-and-forgets-owned-stream ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog-kit::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :write :alias 'cl-prolog-kit::temporary)
      (assert-query rulebase (cl-prolog-kit::close cl-prolog-kit::temporary) :succeeds)
      (is (not (open-stream-p stream)))
      (assert-query rulebase
                    (cl-prolog-kit::stream_property cl-prolog-kit::temporary ?property)
                    :signals))))

(deftest io-close-with-force-option-validates-and-succeeds ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog-kit::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :write :alias 'cl-prolog-kit::temporary)
      (assert-query rulebase
                    (cl-prolog-kit::close cl-prolog-kit::temporary
                                      ((cl-prolog-kit::force cl-prolog-kit::true)))
                    :succeeds)
      (is (not (open-stream-p stream))))))

(deftest io-close-with-force-option-requires-instantiation ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog-kit::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :write :alias 'cl-prolog-kit::temporary)
      (assert-query rulebase
                    (cl-prolog-kit::close cl-prolog-kit::temporary
                                      ((cl-prolog-kit::force ?f)))
                    :signals))))

(deftest io-close-with-force-option-rejects-non-boolean-value ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog-kit::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :write :alias 'cl-prolog-kit::temporary)
      (assert-query rulebase
                    (cl-prolog-kit::close cl-prolog-kit::temporary
                                      ((cl-prolog-kit::force cl-prolog-kit::maybe)))
                    :signals))))

(deftest io-stream-property-supports-enumeration-and-partial-properties ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::mode ?mode))
     :ordered (((?mode . cl-prolog-kit.user-atoms::read))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      ?stream (cl-prolog-kit::alias cl-prolog-kit::user_output))
     :ordered (((?stream . cl-prolog-kit::user_output))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit.user-atoms::type ?type))
     :ordered (((?type . cl-prolog-kit::text))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::reposition ?value))
     :ordered (((?value . cl-prolog-kit:true))))))

(deftest io-set-stream-position-repositions-input-stream ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query rulebase (cl-prolog-kit::get_char ?first)
                  :ordered (((?first . cl-prolog-kit::a))))
    (assert-query rulebase
                  (cl-prolog-kit::set_stream_position cl-prolog-kit::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::get_char ?again)
                  :ordered (((?again . cl-prolog-kit::a))))))

(deftest io-set-stream-position-clears-past-end-state ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::a))))
    (assert-query rulebase (cl-prolog-kit::get_char ?character)
                  :ordered (((?character . cl-prolog-kit::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog-kit::stream_property
      cl-prolog-kit::user_input (cl-prolog-kit::end_of_stream cl-prolog-kit::past))
     :succeeds)
    (assert-query rulebase
                  (cl-prolog-kit::set_stream_position cl-prolog-kit::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog-kit::get_char ?again)
                  :ordered (((?again . cl-prolog-kit::a))))))

(deftest-io-queries io-stream-position-validates-property-and-position ()
  ("stream_property rejects an unknown property"
   "" (cl-prolog-kit::stream_property cl-prolog-kit::user_input (cl-prolog-kit::unknown value))
   :signals)
  ("set_stream_position rejects a non-integer position"
   "" (cl-prolog-kit::set_stream_position cl-prolog-kit::user_input atom)
   :signals)
  ("set_stream_position rejects a negative position"
   "" (cl-prolog-kit::set_stream_position cl-prolog-kit::user_input -1)
   :signals))

(deftest-io-variants io-character-lookahead-supports-current-and-explicit-streams
    ((rulebase input output) "ab")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog-kit::peek_char ?value)
                 :ordered (((?value . cl-prolog-kit::a))))
   (assert-query rulebase
                 (cl-prolog-kit::get_char ?value)
                 :ordered (((?value . cl-prolog-kit::a)))))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog-kit::peek_char cl-prolog-kit::user_input ?value)
                 :ordered (((?value . cl-prolog-kit::a))))
   (assert-query rulebase
                 (cl-prolog-kit::get_char cl-prolog-kit::user_input ?value)
                 :ordered (((?value . cl-prolog-kit::a))))))

(defmacro with-binary-stream-rulebase
    ((rulebase context input-path output-path) input-bytes &body body)
  `(let* ((,input-path
            (merge-pathnames
             (format nil "cl-prolog-kit-input-~36R.bin" (random (expt 36 8)))
             #p"/tmp/"))
          (,output-path
            (merge-pathnames
             (format nil "cl-prolog-kit-output-~36R.bin" (random (expt 36 8)))
             #p"/tmp/")))
     (unwind-protect
          (progn
            (with-open-file (stream ,input-path :direction :output
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
              (write-sequence ,input-bytes stream))
            (let* ((,context (cl-prolog-kit::make-prolog-io-context
                              :input (make-string-input-stream "")
                              :output (make-string-output-stream)
                              :error-output (make-string-output-stream)))
                   (,rulebase (make-rulebase :io-context ,context)))
              (unwind-protect
                   (progn ,@body)
                (cl-prolog-kit::%close-all-owned-prolog-streams! ,context))))
       (ignore-errors (delete-file ,input-path))
       (ignore-errors (delete-file ,output-path)))))

(deftest io-byte-builtins-support-binary-streams-and-eof ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65 255)
    (let ((input (open input-path :direction :input
                       :element-type '(unsigned-byte 8)))
          (output (open output-path :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))))
      (cl-prolog-kit::%register-prolog-stream!
       context input :read :type :binary :alias 'cl-prolog-kit::binary_input)
      (cl-prolog-kit::%register-prolog-stream!
       context output :write :type :binary :alias 'cl-prolog-kit::binary_output)
      (assert-query rulebase
                    (cl-prolog-kit::peek_byte cl-prolog-kit::binary_input ?byte)
                    :ordered (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog-kit::get_byte cl-prolog-kit::binary_input ?byte)
                    :ordered (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog-kit::get_byte cl-prolog-kit::binary_input ?byte)
                    :ordered (((?byte . 255))))
      (assert-query rulebase
                    (cl-prolog-kit::at_end_of_stream cl-prolog-kit::binary_input)
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog-kit::get_byte cl-prolog-kit::binary_input ?byte)
                    :ordered (((?byte . -1))))
      (assert-query rulebase
                    (cl-prolog-kit::put_byte cl-prolog-kit::binary_output 128)
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog-kit::put_byte cl-prolog-kit::binary_output 256)
                    :signals)
      (cl-prolog-kit::%close-all-owned-prolog-streams! context)
      (with-open-file (stream output-path :direction :input
                             :element-type '(unsigned-byte 8))
        (is-equal '(128) (loop for byte = (read-byte stream nil nil)
                               while byte collect byte))))))

(deftest io-character-and-byte-builtins-reject-wrong-stream-types ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65)
    (let ((input (open input-path :direction :input
                       :element-type '(unsigned-byte 8))))
      (cl-prolog-kit::%register-prolog-stream!
       context input :read :type :binary :alias 'cl-prolog-kit::binary_input)
      (assert-query rulebase
                    (cl-prolog-kit::get_char cl-prolog-kit::binary_input ?character)
                    :signals)
      (assert-query rulebase
                    (cl-prolog-kit::get_byte cl-prolog-kit::user_input ?byte)
                    :signals))))

(deftest io-open-accepts-three-and-four-argument-forms ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "hello." stream))
    (with-io-rulebase (rulebase input output) ""
      (let ((source (prolog-atom (namestring path))))
        (dolist (query (list (list 'cl-prolog-kit::open source
                                   'cl-prolog-kit.user-atoms::read '?stream)
                             (list 'cl-prolog-kit::open source
                                   'cl-prolog-kit.user-atoms::read '?stream '())))
          (with-single-query-solution (solution solutions rulebase query)
            (is (not (null (solution-binding '?stream solution)))
                "open must bind a stream")
            (assert-query rulebase (cl-prolog-kit::stream_property
                                    ?stream (cl-prolog-kit::mode ?mode))
                          :succeeds)))))))

(deftest io-open-close-and-current-output-follow-stream-selection ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', write, _, [alias(sel)]), ~
                set_output(sel), put_char(q), close(sel), ~
                current_output(Output), Output == user_output."
               (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-open-close-and-current-input-follow-stream-selection ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "q" stream))
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', read, _, [alias(sel)]), ~
                set_input(sel), get_char(Char), close(sel), ~
                current_input(Input), Input == user_input, ~
                Char == 'q'."
               (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-open-supports-binary-streams-and-append-mode ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', write, S1, [type(binary)]), close(S1), ~
                open('~A', append, S2), close(S2)."
               (namestring path) (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-byte-builtins-current-stream-forms-work-and-validate-type ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65 255)
    (let ((input (open input-path :direction :input
                       :element-type '(unsigned-byte 8)))
          (output (open output-path :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))))
      (cl-prolog-kit::%register-prolog-stream!
       context input :read :type :binary :alias 'cl-prolog-kit::binary_input)
      (cl-prolog-kit::%register-prolog-stream!
       context output :write :type :binary :alias 'cl-prolog-kit::binary_output)
      (assert-query rulebase (cl-prolog-kit::set_input cl-prolog-kit::binary_input)
                    :succeeds)
      (assert-query rulebase (cl-prolog-kit::get_byte ?byte) :ordered (((?byte . 65))))
      (assert-query rulebase (cl-prolog-kit::peek_byte ?byte) :ordered (((?byte . 255))))
      (assert-query rulebase (cl-prolog-kit::set_output cl-prolog-kit::binary_output)
                    :succeeds)
      (assert-query rulebase (cl-prolog-kit::put_byte cl-prolog-kit::not_an_integer)
                    :signals))))

(deftest io-put-char-rejects-a-binary-output-stream ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65)
    (let ((output (open output-path :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))))
      (cl-prolog-kit::%register-prolog-stream!
       context output :write :type :binary :alias 'cl-prolog-kit::binary_output)
      (assert-query rulebase
                    (cl-prolog-kit::put_char cl-prolog-kit::binary_output z)
                    :signals))))

(deftest io-at-end-of-stream-fails-when-not-at-end ()
  (with-io-rulebase (rulebase input output) "ab"
    (assert-query rulebase (cl-prolog-kit::at_end_of_stream) :fails)
    (assert-query rulebase
                  (cl-prolog-kit::at_end_of_stream cl-prolog-kit::user_input)
                  :fails)))

(deftest-io-queries io-stream-property-rejects-malformed-property-shapes ()
  ("a non-list property argument is rejected"
   "" (cl-prolog-kit::stream_property
       cl-prolog-kit::user_input cl-prolog-kit::not_a_property_list)
   :signals)
  ("a property with an extra argument is rejected"
   "" (cl-prolog-kit::stream_property
       cl-prolog-kit::user_input (cl-prolog-kit::mode extra argument))
   :signals)
  ("a property with a non-atom name is rejected"
   "" (cl-prolog-kit::stream_property cl-prolog-kit::user_input (123 cl-prolog-kit::value))
   :signals))

(deftest io-stream-property-enumeration-skips-closed-streams ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog-kit::rulebase-io-context rulebase))
          (stream (make-string-output-stream)))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :write :alias 'cl-prolog-kit::doomed_stream)
      ;; Closing the underlying stream out of band leaves a stale entry in the
      ;; table; enumeration must filter it while still serving live streams.
      (close stream)
      (assert-query rulebase
                    (cl-prolog-kit::stream_property ?stream (cl-prolog-kit::mode ?mode))
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog-kit::stream_property
                     ?stream (cl-prolog-kit::alias cl-prolog-kit::doomed_stream))
                    :fails))))
