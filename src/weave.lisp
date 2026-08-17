;;;; cl-weave helpers for testing cl-prolog-kit queries.
;;;;
;;;; The package itself is declared in src/package-weave.lisp.

(in-package #:cl-prolog-kit/weave)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-query-assertion (kind arguments)
    (case kind
      ((:ordered :set :first)
       (unless arguments
         (error "~S query assertions require an expected value." kind))
       (values (first arguments) (rest arguments)))
      ((:succeeds :fails)
       (values nil arguments))
      (:signals
       (if (and arguments (not (keywordp (first arguments))))
           (values (first arguments) (rest arguments))
           (values nil arguments)))
      (otherwise
       (error "Unknown query assertion kind ~S." kind))))

  (defun %query-run-form (rulebase query kind options)
    (case kind
      (:first
       `(cl-prolog-kit:query-prolog-first ,rulebase ',query ,@options))
      (:succeeds
       `(cl-prolog-kit:prolog-succeeds-p ,rulebase ',query ,@options))
      (otherwise
       `(cl-prolog-kit:query-prolog ,rulebase ',query ,@options))))

  (defun %query-assertion-form (rulebase query kind arguments)
    (multiple-value-bind (expected options)
        (%split-query-assertion kind arguments)
      (let ((run-form (%query-run-form rulebase query kind options)))
        (case kind
          (:ordered
           `(cl-weave:expect ,run-form :to-equal ',expected))
          (:set
           `(cl-weave:expect ,run-form :to-solve ',expected))
          (:first
           `(cl-weave:expect ,run-form :to-equal ',expected))
          (:succeeds
           `(cl-weave:expect ,run-form :to-be-truthy))
          (:fails
           `(cl-weave:expect ,run-form :to-be-null))
          (:signals
           (if expected
               `(cl-weave:expect (lambda () ,run-form) :to-throw ',expected)
               `(cl-weave:expect (lambda () ,run-form) :to-throw)))))))

  (defun %parse-query-spec (spec)
    (unless (consp spec)
      (error "Query specification must be a list, got ~S." spec))
    (let* ((labelledp (stringp (first spec)))
           (label (and labelledp (first spec)))
           (body (if labelledp (rest spec) spec)))
      (unless (and (consp body) (consp (rest body)))
        (error "Query specification requires a query and assertion kind: ~S." spec))
      (values (or label (prin1-to-string (first body)))
              (first body)
              (second body)
              (cddr body)))))

(defun %solution-multiset-remove (solutions removals)
  "Return SOLUTIONS with one occurrence of each solution in REMOVALS deleted."
  (let ((remaining (copy-list solutions)))
    (dolist (removal removals remaining)
      (let ((position (position removal remaining :test #'equal)))
        (when position
          (setf remaining (append (subseq remaining 0 position)
                                  (subseq remaining (1+ position)))))))))

(defun %solution-multiset-diff (actual expected)
  "Return the EXPECTED solutions missing from ACTUAL and the unexpected extras."
  (values (%solution-multiset-remove expected actual)
          (%solution-multiset-remove actual expected)))

(cl-weave:defmatcher :to-solve (actual expected)
  "Passes when ACTUAL holds exactly the EXPECTED solutions in any order."
  (unless (and expected (null (rest expected)))
    (error ":to-solve requires exactly one expected solution list, got ~S."
           expected))
  (let ((wanted (first expected)))
    (multiple-value-bind (missing unexpected)
        (%solution-multiset-diff actual wanted)
      (values (and (null missing) (null unexpected))
              (list :solutions actual :missing missing :unexpected unexpected)
              (list :solutions wanted :order :any)))))

(defmacro assert-query (rulebase query kind &rest arguments)
  "Assert one literal QUERY against RULEBASE using a query assertion KIND."
  (%query-assertion-form rulebase query kind arguments))

(defmacro deftest-queries (name (rulebase-form) &body specs)
  "Define independent cl-weave cases for literal query SPECS.

Each case evaluates RULEBASE-FORM afresh. A spec may start with a string label;
otherwise the printed query is used as its label."
  `(cl-weave:describe-sequential ,(string name)
     ,@(mapcar
        (lambda (spec)
          (multiple-value-bind (label query kind arguments)
              (%parse-query-spec spec)
            `(cl-weave:it ,label
               (cl-weave:expect-has-assertions)
               (let ((rulebase ,rulebase-form))
                 ,(%query-assertion-form 'rulebase query kind arguments)))))
        specs)))
