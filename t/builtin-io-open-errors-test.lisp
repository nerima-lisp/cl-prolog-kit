;;;; Malformed-input and error-rejection coverage: open rejecting non-atom
;;;; sources/aliases, missing files/parent directories, unsupported type
;;;; options, rollback on duplicate alias, boolean-option/non-symbol/non-
;;;; boolean rejections, stream-designator-does-not-unify, and unseekable-
;;;; stream reposition reporting.  Uses the with-io-rulebase fixture defined
;;;; in builtin-io-terms-test.lisp; stream lifecycle lives in
;;;; builtin-io-streams-lifecycle-test.lisp.

(in-package #:cl-prolog-kit.tests)

(deftest io-boolean-option-rejects-an-unbound-value ()
  (handler-case
      (progn
        (cl-prolog-kit::%io-boolean '?value nil (cl-prolog-kit::%iso-atom "TEST"))
        (error "Expected an unbound boolean option to be rejected"))
    (prolog-instantiation-error (condition)
      (declare (ignore condition))
      (is t "An unbound boolean option must raise an instantiation error"))))

(deftest-io-queries io-builtins-reject-malformed-option-values ()
  ("read_term rejects an unbound syntax_errors value"
   "ok." (cl-prolog-kit::read_term ?term ((cl-prolog-kit::syntax_errors ?policy)))
   :signals)
  ("read_term rejects a non-symbol syntax_errors value"
   "ok." (cl-prolog-kit::read_term ?term ((cl-prolog-kit::syntax_errors 123)))
   :signals)
  ("the boolean option rejects a non-symbol value"
   "" (cl-prolog-kit::write_term hello ((cl-prolog-kit::quoted 5)))
   :signals)
  ("put_char rejects a multi-character atom"
   "" (cl-prolog-kit::put_char cl-prolog-kit::ab)
   :signals)
  ("put_char rejects a non-symbol character"
   "" (cl-prolog-kit::put_char 123)
   :signals)
  ("open rejects a non-symbol type option"
   "" (cl-prolog-kit::open cl-prolog-kit::whatever cl-prolog-kit.user-atoms::read ?stream
                       ((cl-prolog-kit::type 123)))
   :signals))

(deftest-io-queries io-open-rejects-malformed-arguments ()
  ("open rejects a non-atom source"
   "" (cl-prolog-kit::open 123 cl-prolog-kit.user-atoms::read ?stream)
   :signals prolog-type-error)
  ("open reports an existence error for a missing unquoted source"
   "" (cl-prolog-kit::open cl-prolog-kit::nonexistent_io_open_test_source_xyz
                       cl-prolog-kit.user-atoms::read ?stream)
   :signals prolog-existence-error)
  ("open rejects an unsupported type option"
   "" (cl-prolog-kit::open cl-prolog-kit::whatever cl-prolog-kit.user-atoms::read ?stream
                       ((cl-prolog-kit::type cl-prolog-kit::bogus)))
   :signals prolog-domain-error)
  ("open rejects a non-atom alias"
   "" (cl-prolog-kit::open cl-prolog-kit::whatever cl-prolog-kit.user-atoms::read ?stream
                       ((cl-prolog-kit::alias 123)))
   :signals prolog-type-error))

(deftest io-open-reports-existence-error-for-a-missing-parent-directory ()
  (with-io-rulebase (rulebase input output) ""
    (let ((query
            (%read-prolog-query
             rulebase
             "open('/tmp/definitely_nonexistent_dir_xyz_123/file.pl', write, _).")))
      (signals-prolog-condition prolog-existence-error
        (prolog-succeeds-p rulebase query)))))

(deftest io-open-rolls-back-a-newly-opened-stream-on-duplicate-alias ()
  (uiop:with-temporary-file (:pathname path-a :type "txt")
    (uiop:with-temporary-file (:pathname path-b :type "txt")
      (with-io-rulebase (rulebase input output) ""
        (let ((query
                (%read-prolog-query
                 rulebase
                 "open('~A', write, S1, [alias(dup_alias_test)]), ~
                  open('~A', write, S2, [alias(dup_alias_test)])."
                 (namestring path-a) (namestring path-b))))
          (signals-error (prolog-succeeds-p rulebase query)))))))

(deftest io-open-fails-when-the-stream-designator-does-not-unify ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "q" stream))
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query rulebase "open('~A', read, wrong_designator)."
                                   (namestring path))))
        (is (not (prolog-succeeds-p rulebase query)))))))

(defclass unseekable-binary-input-stream (sb-gray:fundamental-binary-input-stream)
  ((bytes :initarg :bytes :accessor unseekable-binary-input-stream-bytes))
  (:documentation "A binary stream with no FILE-POSITION support, for exercising
peek_byte/1's fallback when a stream cannot save and restore its position."))

(defmethod sb-gray:stream-read-byte ((stream unseekable-binary-input-stream))
  (if (unseekable-binary-input-stream-bytes stream)
      (pop (unseekable-binary-input-stream-bytes stream))
      :eof))

(deftest io-peek-byte-rejects-a-stream-without-file-position-support ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog-kit::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-binary-input-stream
                                 :bytes (list 65))))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :read :type :binary :alias 'cl-prolog-kit::unseekable_input)
      (assert-query rulebase
                    (cl-prolog-kit::peek_byte cl-prolog-kit::unseekable_input ?byte)
                    :signals))))

(deftest io-set-stream-position-rejects-a-stream-without-file-position-support ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog-kit::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-binary-input-stream
                                 :bytes (list 65))))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :read :type :binary :alias 'cl-prolog-kit::unseekable_input2)
      (assert-query rulebase
                    (cl-prolog-kit::set_stream_position
                     cl-prolog-kit::unseekable_input2 0)
                    :signals))))

(defclass unseekable-character-input-stream
    (sb-gray:fundamental-character-input-stream)
  ((characters :initarg :characters
               :accessor unseekable-character-input-stream-characters))
  (:documentation "A character stream with no FILE-POSITION support, for
exercising stream_property/2 on a stream that cannot report a position."))

(defmethod sb-gray:stream-read-char ((stream unseekable-character-input-stream))
  (if (unseekable-character-input-stream-characters stream)
      (pop (unseekable-character-input-stream-characters stream))
      :eof))

(defmethod sb-gray:stream-peek-char ((stream unseekable-character-input-stream))
  (if (unseekable-character-input-stream-characters stream)
      (car (unseekable-character-input-stream-characters stream))
      :eof))

(deftest io-stream-property-reports-false-reposition-for-unseekable-stream ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog-kit::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-character-input-stream
                                 :characters (list #\a))))
      (cl-prolog-kit::%register-prolog-stream!
       context stream :read :type :text :alias 'cl-prolog-kit::unseekable_prop)
      (assert-query rulebase
                    (cl-prolog-kit::stream_property
                     cl-prolog-kit::unseekable_prop (cl-prolog-kit::reposition ?value))
                    :ordered (((?value . cl-prolog-kit::false))))
      (assert-query rulebase
                    (cl-prolog-kit::stream_property
                     cl-prolog-kit::unseekable_prop
                     (cl-prolog-kit.user-atoms::position ?p))
                    :fails))))
