(require :asdf)

(asdf:load-asd (truename "cl-prolog-kit.asd"))

(asdf:load-system :cl-prolog-kit)

(in-package #:cl-user)

(defparameter *external-node-count* 31)

(defun make-external-rulebase ()
  (let ((source (cl-prolog-kit:fresh-logic-variable "?SOURCE"))
        (destination (cl-prolog-kit:fresh-logic-variable "?DESTINATION"))
        (middle (cl-prolog-kit:fresh-logic-variable "?MIDDLE")))
    (cl-prolog-kit:make-rulebase
      :clauses
      (nconc
        (loop for child from 1 below *external-node-count*
              for parent = (floor (1- child) 2)
              collect (cl-prolog-kit:make-clause (list :edge parent child)))
        (list
          (cl-prolog-kit:make-clause
            (list :path source destination)
            (list (list :edge source destination)))
          (cl-prolog-kit:make-clause
            (list :path source destination)
            (list (list :edge source middle) (list :path middle destination))))))))

(defun external-solution-stats (solutions variable)
  (loop with count = 0
        with checksum = 0
        with fingerprint = 0
        for solution in solutions
        for value = (cdr (assoc variable solution))
        do (incf count)
           (incf checksum value)
           (setf fingerprint
                 (mod (+ (* fingerprint 131) value) 2147483647))
        finally (return (values count checksum fingerprint))))

(defun run-external-workload (rulebase iterations)
  (loop repeat iterations
        sum (let* ((destination
                     (cl-prolog-kit:fresh-logic-variable "?DESTINATION"))
                   (solutions
                     (cl-prolog-kit:query-prolog
                      rulebase
                      (list (list :path 0 destination)))))
              (multiple-value-bind (count checksum fingerprint)
                  (external-solution-stats solutions destination)
                (unless (and (= count 30)
                             (= checksum 465)
                             (= fingerprint 1589920743))
                  (error "Branching closure mismatch: count=~D checksum=~D fingerprint=~D"
                         count checksum fingerprint))
                checksum))))

(let ((rulebase (make-external-rulebase)))
  (run-external-workload rulebase 100)
  (format t "READY~%")
  (finish-output)
  (let* ((line (read-line *standard-input* nil nil))
         (iterations (and line
                          (parse-integer line :junk-allowed t))))
    (unless (and iterations (plusp iterations))
      (error "Expected a positive iteration count, got ~S" line))
    (let ((aggregate (run-external-workload rulebase iterations)))
      (unless (= aggregate (* iterations 465))
        (error "Aggregate mismatch: ~D" aggregate))
      (format t "RESULT 30 465 1589920743 ~D~%" aggregate)
      (finish-output))))
