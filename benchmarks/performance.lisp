(require :asdf)
  (asdf:load-asd (truename "cl-prolog-kit.asd"))
  (asdf:load-system :cl-prolog-kit)
  (in-package #:cl-user)

  (defparameter *benchmark-result* nil)

  (defun percentile (values fraction)
  (let* ((sorted (sort (copy-list values) #'<))
         (rank (ceiling (* fraction (length sorted))))
         (index (max 0 (1- rank))))
    (nth index sorted)))

  (defun run-benchmark (name thunk operations samples warmup)
  (dotimes (iteration warmup)
    (declare (ignore iteration))
    (setf *benchmark-result* (funcall thunk)))
  (let ((times '())
        (bytes '()))
    (dotimes (sample samples)
      (declare (ignore sample))
      (sb-ext:gc :full t)
      (let ((start-time (get-internal-real-time))
            (start-bytes (sb-ext:get-bytes-consed)))
        (setf *benchmark-result* (funcall thunk))
        (let ((elapsed (- (get-internal-real-time) start-time))
              (allocated (- (sb-ext:get-bytes-consed) start-bytes)))
          (push (round (/ (* elapsed 1000000000)
                          internal-time-units-per-second
                          operations))
                times)
          (push (round (/ allocated operations)) bytes))))
    (format t
            "{~S:~S,~S:~S,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D,~S:~D}~%"
            "name" name
            "measurement_mode" "warm-steady-state"
            "operations_per_sample" operations
            "sample_operations" operations
            "samples" samples
            "warmup_iterations" warmup
            "warmup_operations" (* warmup operations)
            "min_ns_per_op" (apply #'min times)
            "p50_ns_per_op" (percentile times 0.50)
            "p90_ns_per_op" (percentile times 0.90)
            "max_ns_per_op" (apply #'max times)
            "min_bytes_per_op" (apply #'min bytes)
            "p50_bytes_per_op" (percentile bytes 0.50)
            "p90_bytes_per_op" (percentile bytes 0.90)
            "max_bytes_per_op" (apply #'max bytes))))

  (defun make-alias-chain (length)
    (let ((variables (loop repeat length
                           collect (cl-prolog-kit:fresh-logic-variable)))
          (environment '()))
      (loop for left on variables
            while (cdr left)
            do (push (cons (car left) (cadr left)) environment))
      (values (car variables) (car (last variables)) environment)))

  (defun benchmark-alias-chain ()
    (multiple-value-bind (start terminal environment)
        (make-alias-chain 50001)
      (multiple-value-bind (resolved-environment ok)
          (cl-prolog-kit:unify start :resolved environment)
        (assert ok)
        (assert (eq :resolved
                    (cl-prolog-kit:logic-substitute start resolved-environment)))
        (run-benchmark
         "alias-chain-50001/unify"
         (lambda ()
           (multiple-value-bind (result success)
               (cl-prolog-kit:unify start :resolved environment)
             (unless success (error "Alias-chain unification failed."))
             result))
         1 15 5)
        (run-benchmark
         "alias-chain-50001/substitute"
         (lambda ()
           (cl-prolog-kit:logic-substitute start resolved-environment))
         1 15 5))))

(defun benchmark-indexed-substitution ()
  (let* ((left (cl-prolog-kit:fresh-logic-variable "?LEFT"))
         (right (cl-prolog-kit:fresh-logic-variable "?RIGHT"))
         (bound-compound (list :node :bound))
         (goal (list :goal left (list :nested right left)))
         (environment (list (cons left bound-compound)
                            (cons right :resolved)))
         (index (cl-prolog-kit::%make-environment-index environment))
         (expected (list :goal (list :node :bound)
                         (list :nested :resolved (list :node :bound))))
         (result (cl-prolog-kit::%logic-substitute-indexed goal index))
         (cases
           (coerce
            (loop repeat 64
                  collect
                  (let ((sample-left (cl-prolog-kit:fresh-logic-variable))
                        (sample-right (cl-prolog-kit:fresh-logic-variable)))
                    (cons
                     (list :goal sample-left (list :nested sample-right sample-left))
                     (cl-prolog-kit::%make-environment-index
                      (list (cons sample-left (list :node :bound))
                            (cons sample-right :resolved))))))
            (quote vector)))
         (cursor 0))
    (assert (equal expected result))
    (assert (not (eq goal result)))
    (assert (eq (second result) (third (third result))))
    (assert (not (eq bound-compound (second result))))
    (run-benchmark
     "indexed-substitution/small-bound-compound-goal"
     (lambda ()
       (let ((timed-result nil))
         (dotimes (iteration 100000 timed-result)
           (declare (ignore iteration))
           (let ((case (aref cases cursor)))
             (setf cursor (logand (1+ cursor) 63))
             (setf timed-result
                   (cl-prolog-kit::%logic-substitute-indexed (car case) (cdr case)))))))
     100000 15 5)))

  (defun make-predicate-index-rulebase (count)
    (cl-prolog-kit:make-rulebase
     :clauses
     (nconc (loop repeat count
                  collect (cl-prolog-kit:make-clause
                           (list (gensym "COLD-PREDICATE-") :value)))
            (list (cl-prolog-kit:make-clause '(:hot :ok))))))

  (progn
  (defun benchmark-predicate-index ()
    (let ((rulebase (make-predicate-index-rulebase 20000)))
      (assert (cl-prolog-kit:prolog-succeeds-p rulebase (quote (:hot :ok))))
      (run-benchmark
       "predicate-index-20000/succeeds"
       (lambda ()
         (dotimes (iteration 1000 t)
           (declare (ignore iteration))
           (unless (cl-prolog-kit:prolog-succeeds-p rulebase (quote (:hot :ok)))
             (error "Indexed predicate lookup failed."))))
       1000 15 5)))

  (defun make-first-argument-index-rulebase (count)
    (cl-prolog-kit:make-rulebase
     :clauses
     (loop for value below count
           collect (cl-prolog-kit:make-clause (list :lookup value)))))

  (defun benchmark-first-argument-index ()
    (let* ((count 20000)
           (target (1- count))
           (rulebase (make-first-argument-index-rulebase count))
           (query (list :lookup target)))
      (assert (cl-prolog-kit:prolog-succeeds-p rulebase query))
      (run-benchmark
       "first-argument-index-20000/bound-tail-succeeds"
       (lambda ()
         (dotimes (iteration 1000 t)
           (declare (ignore iteration))
           (unless (cl-prolog-kit:prolog-succeeds-p rulebase query)
             (error "Bound first-argument lookup failed."))))
       1000 15 5)))

  (defun first-argument-solution-checksum (solutions variable)
    (loop for solution in solutions
          sum (cdr (assoc variable solution))))

  (defun run-mixed-first-argument-workload (rulebase count)
    (let* ((middle (floor count 2))
           (tail (1- count))
           (variable (cl-prolog-kit:fresh-logic-variable "?VALUE"))
           (solutions nil)
           (checksum nil))
      (unless (cl-prolog-kit:prolog-succeeds-p rulebase (list :lookup 0))
        (error "Head first-argument lookup failed."))
      (unless (cl-prolog-kit:prolog-succeeds-p rulebase (list :lookup middle))
        (error "Middle first-argument lookup failed."))
      (unless (cl-prolog-kit:prolog-succeeds-p rulebase (list :lookup tail))
        (error "Tail first-argument lookup failed."))
      (when (cl-prolog-kit:prolog-succeeds-p rulebase (list :lookup count))
        (error "Missing first-argument lookup unexpectedly succeeded."))
      (setf solutions (cl-prolog-kit:query-prolog rulebase (list :lookup variable))
            checksum (first-argument-solution-checksum solutions variable))
      (unless (and (= count (length solutions))
                   (= checksum (/ (* count (1- count)) 2)))
        (error "Unbound first-argument lookup returned incorrect solutions."))
      checksum))

  (defun benchmark-mixed-first-argument-index ()
    (let* ((count 20000)
           (rulebase (make-first-argument-index-rulebase count)))
      (run-mixed-first-argument-workload rulebase count)
      (run-benchmark
       "first-argument-index-20000/mixed-head-middle-tail-miss-unbound"
       (lambda ()
         (run-mixed-first-argument-workload rulebase count))
       5 7 2))))

  (defun make-recursive-path-rulebase (length)
    (let ((x (cl-prolog-kit:fresh-logic-variable))
          (y (cl-prolog-kit:fresh-logic-variable))
          (z (cl-prolog-kit:fresh-logic-variable)))
      (cl-prolog-kit:make-rulebase
       :clauses
       (nconc (loop for from below length
                    collect (cl-prolog-kit:make-clause
                             (list :edge from (1+ from))))
              (list (cl-prolog-kit:make-clause
                     (list :path x y)
                     (list (list :edge x y)))
                    (cl-prolog-kit:make-clause
                     (list :path x y)
                     (list (list :edge x z)
                           (list :path z y))))))))

  (progn
  (defun benchmark-recursive-path ()
    (let* ((rulebase (make-recursive-path-rulebase 20))
           (destination (cl-prolog-kit:fresh-logic-variable))
           (query (list :path 0 destination)))
      (assert (= 20 (length (cl-prolog-kit:query-prolog rulebase query))))
      (run-benchmark
       "recursive-path-20/query-all"
       (lambda () (cl-prolog-kit:query-prolog rulebase query))
       1 11 3)))

  (defun make-branching-path-rulebase (node-count)
  (let ((source (cl-prolog-kit:fresh-logic-variable "?SOURCE"))
        (destination (cl-prolog-kit:fresh-logic-variable "?DESTINATION"))
        (middle (cl-prolog-kit:fresh-logic-variable "?MIDDLE")))
    (cl-prolog-kit:make-rulebase
     :clauses
     (nconc
      (loop for child from 1 below node-count
            for parent = (floor (1- child) 2)
            collect (cl-prolog-kit:make-clause (list :edge parent child)))
      (list
       (cl-prolog-kit:make-clause
        (list :path source destination)
        (list (list :edge source destination)))
       (cl-prolog-kit:make-clause
        (list :path source destination)
        (list (list :edge source middle)
              (list :path middle destination))))))))

  (defun branching-path-all-solutions (rulebase node-count)
    (let* ((destination (cl-prolog-kit:fresh-logic-variable "?DESTINATION"))
           (solutions (cl-prolog-kit:query-prolog
                       rulebase
                       (list :path 0 destination)))
           (checksum (first-argument-solution-checksum solutions destination)))
      (unless (and (= (1- node-count) (length solutions))
                   (= checksum (/ (* node-count (1- node-count)) 2)))
        (error "Branching transitive closure returned incorrect solutions."))
      checksum))

  (defun benchmark-branching-path-ground ()
    (let* ((node-count 31)
           (target (1- node-count))
           (rulebase (make-branching-path-rulebase node-count))
           (query (list :path 0 target)))
      (assert (cl-prolog-kit:prolog-succeeds-p rulebase query))
      (run-benchmark
       "branching-path-31/ground-reachability"
       (lambda ()
         (dotimes (iteration 100 t)
           (declare (ignore iteration))
           (unless (cl-prolog-kit:prolog-succeeds-p rulebase query)
             (error "Ground branching reachability failed."))))
       100 11 3)))

  (defun benchmark-branching-path-all-solutions ()
    (let* ((node-count 31)
           (rulebase (make-branching-path-rulebase node-count)))
      (branching-path-all-solutions rulebase node-count)
      (run-benchmark
       "branching-path-31/query-all"
       (lambda ()
         (branching-path-all-solutions rulebase node-count))
       1 11 3))))

  (progn
  (benchmark-alias-chain)
  (benchmark-indexed-substitution))
  (progn
  (benchmark-predicate-index)
  (benchmark-first-argument-index)
  (benchmark-mixed-first-argument-index))
  (progn
  (benchmark-recursive-path)
  (benchmark-branching-path-ground)
  (benchmark-branching-path-all-solutions))

(progn
  (defun make-tabled-answer-replay-rulebase (predicate answer)
    (let ((rulebase
            (cl-prolog-kit:make-rulebase
             :clauses (list (cl-prolog-kit:make-clause (list predicate answer))))))
      (cl-prolog-kit::%add-rulebase-table-declaration! rulebase predicate 1 :benchmark)
      rulebase))

  (defun make-tabled-answer-replay-query (predicate values)
    (let ((variables
            (loop repeat (length values)
                  collect (cl-prolog-kit:fresh-logic-variable))))
      (append
       (loop for variable in variables
             collect (list predicate variable))
       (loop for variable in variables
             for value in values
             collect (list (quote =) variable value)))))

  (defun benchmark-table-answer-replay ()
    (dolist (case (list (list "ground" (list (quote alpha) (quote alpha) (quote alpha)))
                        (list "nonground" (list (quote alpha) (quote beta) (quote gamma)))))
      (let* ((name (first case))
             (values (second case))
             (predicate (if (string= name "ground")
                            (quote tabled-replay-ground)
                            (quote tabled-replay-nonground)))
             (answer (if (string= name "ground")
                         (quote alpha)
                         (cl-prolog-kit:fresh-logic-variable)))
             (rulebase (make-tabled-answer-replay-rulebase predicate answer))
             (query (make-tabled-answer-replay-query predicate values)))
        (unless (cl-prolog-kit:prolog-succeeds-p rulebase query)
          (error "Table-answer replay setup failed for ~A." name))
        (run-benchmark
         (format nil "table-answer-replay-3-consumers/~A" name)
         (lambda ()
           (dotimes (iteration 100 t)
             (declare (ignore iteration))
             (unless (cl-prolog-kit:prolog-succeeds-p rulebase query)
               (error "Table-answer replay failed for ~A." name))))
         100 11 3))))

  (defun make-left-recursive-path-rulebase (length)
    (let ((x (cl-prolog-kit:fresh-logic-variable))
          (y (cl-prolog-kit:fresh-logic-variable))
          (z (cl-prolog-kit:fresh-logic-variable)))
      (cl-prolog-kit:make-rulebase
       :clauses
       (nconc
        (list
         (cl-prolog-kit:make-clause
          (list (quote left-path) x y)
          (list (list (quote left-path) x z)
                (list (quote left-edge) z y)))
         (cl-prolog-kit:make-clause
          (list (quote left-path) x y)
          (list (list (quote left-edge) x y))))
        (loop for source below length
              collect (cl-prolog-kit:make-clause
                       (list (quote left-edge) source (1+ source))))))))

  (defun left-recursive-path-succeeds-p (rulebase length)
    (cl-prolog-kit:prolog-succeeds-p rulebase (list (quote left-path) 0 length)))

  (defun benchmark-left-recursion-cache ()
    (let ((length 64))
      (run-benchmark
       "left-recursion-cache-64/cold-rulebase"
       (lambda ()
         (dotimes (iteration 20 t)
           (declare (ignore iteration))
           (let ((rulebase (make-left-recursive-path-rulebase length)))
             (unless (left-recursive-path-succeeds-p rulebase length)
               (error "Cold left-recursion query failed.")))))
       20 7 2)
      (let ((rulebase (make-left-recursive-path-rulebase length)))
        (unless (left-recursive-path-succeeds-p rulebase length)
          (error "Warm left-recursion setup failed."))
        (run-benchmark
         "left-recursion-cache-64/warm-shared-scope"
         (lambda ()
           (dotimes (iteration 100 t)
             (declare (ignore iteration))
             (unless (left-recursive-path-succeeds-p rulebase length)
               (error "Warm left-recursion query failed."))))
         100 11 3))
      (let ((rulebase (make-left-recursive-path-rulebase length))
            (revision 0))
        (unless (left-recursive-path-succeeds-p rulebase length)
          (error "Revision-change left-recursion setup failed."))
        (run-benchmark
         "left-recursion-cache-64/revision-change"
         (lambda ()
           (dotimes (iteration 20 t)
             (declare (ignore iteration))
             (cl-prolog-kit:rulebase-insert-clause!
              rulebase
              (cl-prolog-kit:make-clause (list (quote cache-noise) revision)))
             (incf revision)
             (unless (left-recursive-path-succeeds-p rulebase length)
               (error "Revision-change left-recursion query failed."))))
         20 7 2))))

  (benchmark-table-answer-replay)
  (benchmark-left-recursion-cache))

;;; Knowledge-base workloads (names prefixed kb/).  They must run unchanged
;;; against v1.5.0 and later sources, so optional APIs are looked up by name
;;; and probed at run time instead of being referenced directly.
(progn
  (defun kb-variable (name)
    (cl-prolog-kit:fresh-logic-variable name))

  (defun kb-optional-function (name)
    (let ((symbol (find-symbol name '#:cl-prolog-kit)))
      (and symbol (fboundp symbol) (symbol-function symbol))))

  (defun kb-repeat-check (iterations thunk message)
    (dotimes (iteration iterations t)
      (declare (ignore iteration))
      (unless (funcall thunk)
        (error message))))

  (defun make-kb-small-rulebase ()
    (cl-prolog-kit:make-rulebase
     :clauses (loop for index below 20
                    collect (cl-prolog-kit:make-clause
                             (list 'kb-item index
                                   (intern (format nil "VALUE-~D" index)
                                           '#:keyword))))))

  (defun benchmark-kb-small-queries (prefix options)
    "Time tiny queries; OPTIONS are passed to every query entry point."
    (let* ((rulebase (make-kb-small-rulebase))
           (ground (list 'kb-item 7 :value-7))
           (variable (kb-variable "?VALUE"))
           (open-goal (list 'kb-item 7 variable))
           (iterations 1000))
      (flet ((succeeds ()
               (apply #'cl-prolog-kit:prolog-succeeds-p rulebase ground options))
             (first-binding ()
               (multiple-value-bind (solution found)
                   (apply #'cl-prolog-kit:query-prolog-first
                          rulebase open-goal options)
                 (and found
                      (eq :value-7
                          (cl-prolog-kit:solution-binding variable solution)))))
             (all-solutions ()
               (let ((solutions (apply #'cl-prolog-kit:query-prolog
                                       rulebase open-goal options)))
                 (and (= 1 (length solutions))
                      (eq :value-7
                          (cl-prolog-kit:solution-binding
                           variable (first solutions)))))))
        (assert (succeeds))
        (assert (first-binding))
        (assert (all-solutions))
        (assert (not (apply #'cl-prolog-kit:prolog-succeeds-p
                            rulebase (list 'kb-item 7 :value-8) options)))
        (run-benchmark
         (format nil "~A/succeeds-ground" prefix)
         (lambda () (kb-repeat-check iterations #'succeeds
                                     "Small ground query failed."))
         iterations 15 5)
        (run-benchmark
         (format nil "~A/query-first-one-variable" prefix)
         (lambda () (kb-repeat-check iterations #'first-binding
                                     "Small query-prolog-first failed."))
         iterations 15 5)
        (run-benchmark
         (format nil "~A/query-all-one-solution" prefix)
         (lambda () (kb-repeat-check iterations #'all-solutions
                                     "Small query-prolog failed."))
         iterations 15 5))))

  (defun max-inferences-option-supported-p ()
    "True when this source accepts :MAX-INFERENCES on every query entry point.
v1.5.0 signals PROGRAM-ERROR for an unknown query option or keyword."
    (let ((rulebase (make-kb-small-rulebase))
          (goal (list 'kb-item 0 :value-0)))
      (handler-case
          (and (cl-prolog-kit:prolog-succeeds-p rulebase goal)
               (apply #'cl-prolog-kit:prolog-succeeds-p
                      rulebase goal '(:max-inferences 1000000))
               (nth-value 1 (apply #'cl-prolog-kit:query-prolog-first
                                   rulebase goal '(:max-inferences 1000000)))
               (apply #'cl-prolog-kit:query-prolog
                      rulebase goal '(:max-inferences 1000000))
               t)
        (program-error () nil))))

  (defun make-kb-bulk-clauses (count)
    (loop for index below count
          collect (cl-prolog-kit:make-clause (list 'kb-bulk index))))

  (defun kb-bulk-rulebase-valid-p (rulebase count)
    (let* ((variable (kb-variable "?INDEX"))
           (solutions (cl-prolog-kit:query-prolog
                       rulebase (list 'kb-bulk variable))))
      (and (= count (length solutions))
           (= (first-argument-solution-checksum solutions variable)
              (/ (* count (1- count)) 2))
           (eql (1- count)
                (cl-prolog-kit:solution-binding variable (car (last solutions)))))))

  (defun benchmark-kb-bulk-insert ()
    (let* ((count 10000)
           (clauses (make-kb-bulk-clauses count))
           (bulk-insert (kb-optional-function "RULEBASE-INSERT-CLAUSES!"))
           (variants
             (append
              (list
               ;; Per-clause insertion refreshes the predicate descriptor each
               ;; time, so v1.5.0 spends seconds per sample here: keep the
               ;; sample count low.
               (list "insert-clause-loop" 3 1
                     (lambda ()
                       (let ((rulebase (cl-prolog-kit:make-rulebase)))
                         (dolist (clause clauses rulebase)
                           (cl-prolog-kit:rulebase-insert-clause!
                            rulebase clause :position :last)))))
               (list "make-rulebase-clauses" 7 2
                     (lambda () (cl-prolog-kit:make-rulebase :clauses clauses))))
              (when bulk-insert
                (list
                 (list "insert-clauses" 7 2
                       (lambda ()
                         (funcall bulk-insert (cl-prolog-kit:make-rulebase)
                                  clauses :position :last))))))))
      (loop for (label samples warmup thunk) in variants
            do (unless (kb-bulk-rulebase-valid-p (funcall thunk) count)
                 (error "Bulk insert variant ~A built a wrong rulebase." label))
               (run-benchmark (format nil "kb/bulk-insert-~D/~A" count label)
                              thunk count samples warmup))))

  (defun benchmark-kb-assertz-loop ()
    (let* ((count 2000)
           (index (kb-variable "?INDEX"))
           (query (list (list 'cl-prolog-kit::between 1 count index)
                        (list 'cl-prolog-kit:assertz (list 'kb-dynamic index))
                        (list 'cl-prolog-kit:fail)))
           (check (kb-variable "?CHECK")))
      (flet ((run ()
               (let ((rulebase (cl-prolog-kit:make-rulebase)))
                 (when (cl-prolog-kit:prolog-succeeds-p rulebase query)
                   (error "Failure-driven assertz loop unexpectedly succeeded."))
                 rulebase)))
        (let ((solutions (cl-prolog-kit:query-prolog
                          (run) (list 'kb-dynamic check))))
          (assert (= count (length solutions)))
          (assert (= (first-argument-solution-checksum solutions check)
                     (/ (* count (1+ count)) 2))))
        (run-benchmark (format nil "kb/bulk-insert-~D/assertz-query-loop" count)
                       #'run count 7 2))))

  (defun make-kb-lookup-rulebase (count key-function)
    (cl-prolog-kit:make-rulebase
     :clauses (loop for index below count
                    collect (cl-prolog-kit:make-clause
                             (list 'kb-lookup (funcall key-function index) index)))))

  (defun benchmark-kb-first-arg-lookup (kind key-function)
    "Bound first-argument lookups at head, middle and tail, plus a miss."
    (let* ((count 5000)
           (rulebase (make-kb-lookup-rulebase count key-function))
           (iterations 100))
      (loop for (position index expected) in
                (list (list "head" 0 t)
                      (list "middle" (floor count 2) t)
                      (list "tail" (1- count) t)
                      (list "miss" count nil))
            do (let* ((key (funcall key-function index))
                      (variable (kb-variable "?INDEX"))
                      (goal (list 'kb-lookup key index)))
                 (multiple-value-bind (solution found)
                     (cl-prolog-kit:query-prolog-first
                      rulebase (list 'kb-lookup key variable))
                   (unless (if expected
                               (and found
                                    (eql index (cl-prolog-kit:solution-binding
                                                variable solution)))
                               (not found))
                     (error "~A lookup at ~A returned ~S." kind position solution)))
                 (run-benchmark
                  (format nil "kb/~A-first-arg-lookup-~D/~A" kind count position)
                  (lambda ()
                    (dotimes (iteration iterations t)
                      (declare (ignore iteration))
                      (unless (eq expected
                                  (cl-prolog-kit:prolog-succeeds-p rulebase goal))
                        (error "~A lookup at ~A changed result." kind position))))
                  iterations 11 3)))))

  (defun kb-string-key (index)
    (format nil "cmd~D" index))

  (defun kb-atom-key (index)
    (intern (format nil "CMD~D" index) '#:keyword))

  (defun kb-compound-key (index)
    ;; 50 distinct functors with 100 facts each: a functor/arity index narrows
    ;; a lookup to one group, and the remaining arguments pick the fact.
    (list (intern (format nil "OP~D" (mod index 50)) '#:keyword)
          (format nil "x~D" index)
          index))

  (defun make-kb-setof-rulebase (count groups)
    (cl-prolog-kit:make-rulebase
     :clauses (loop for index below count
                    collect (cl-prolog-kit:make-clause
                             (list 'kb-member (mod index groups)
                                   ;; Reverse order so setof has to sort.
                                   (- count index))))))

  (defun benchmark-kb-setof ()
    (let* ((count 2000)
           (groups 20)
           (rulebase (make-kb-setof-rulebase count groups))
           (group (kb-variable "?GROUP"))
           (member (kb-variable "?MEMBER"))
           (members (kb-variable "?MEMBERS"))
           (all-member (kb-variable "?ALL-MEMBER"))
           (all-group (kb-variable "?ALL-GROUP"))
           (all-members (kb-variable "?ALL-MEMBERS"))
           (grouped-query
             (list (list 'cl-prolog-kit:setof member
                         (list 'kb-member group member) members)))
           (all-query
             (list (list 'cl-prolog-kit:setof all-member
                         (list 'cl-prolog-kit::^ all-group (list 'kb-member all-group all-member))
                         all-members))))
      (flet ((grouped ()
               (let ((solutions (cl-prolog-kit:query-prolog rulebase grouped-query)))
                 (unless (and (= groups (length solutions))
                              (every (lambda (solution)
                                       (let ((bag (cl-prolog-kit:solution-binding
                                                   members solution)))
                                         (and (= (/ count groups) (length bag))
                                              (apply #'< bag))))
                                     solutions))
                   (error "Grouped setof returned incorrect solutions."))
                 solutions))
             (all ()
               (let* ((solutions (cl-prolog-kit:query-prolog rulebase all-query))
                      (bag (cl-prolog-kit:solution-binding
                            all-members (first solutions))))
                 (unless (and (= 1 (length solutions))
                              (= count (length bag))
                              (eql 1 (first bag))
                              (apply #'< bag))
                   (error "Setof over all facts returned incorrect solutions."))
                 solutions)))
        (grouped)
        (all)
        (run-benchmark (format nil "kb/setof-~D/grouped-~D" count groups)
                       #'grouped 1 11 3)
        (run-benchmark (format nil "kb/setof-~D/all" count)
                       #'all 1 11 3))))

  (benchmark-kb-small-queries "kb/small-query-fixed-cost" '())
  (benchmark-kb-bulk-insert)
  (benchmark-kb-assertz-loop)
  (benchmark-kb-first-arg-lookup "string" #'kb-string-key)
  (benchmark-kb-first-arg-lookup "atom" #'kb-atom-key)
  (benchmark-kb-first-arg-lookup "compound" #'kb-compound-key)
  (benchmark-kb-setof)
  (when (max-inferences-option-supported-p)
    (benchmark-kb-small-queries "kb/max-inferences-overhead"
                                '(:max-inferences 1000000))))
