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
