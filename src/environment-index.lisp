;;;; Environment indexing: an identity-keyed view over an association-list
;;;; environment, with a bounded overlay so extending an environment stays
;;;; cheap, plus the variable dereferencing (walk) built on top of it.

(in-package #:cl-prolog-kit)

(defconstant +environment-index-overlay-threshold+ 8)
(progn
(defstruct (%environment-index-chunk
    (:constructor %make-environment-index-chunk (table bindings newest-rank size)))
  (table (make-hash-table :test (function eq)) :type hash-table)
  (bindings nil :type list)
  (newest-rank 0 :type integer)
  (size 0 :type (integer 0 *)))
(defstruct (%environment-index
    (:constructor %make-environment-index-object
      (table chunks overlay overlay-length next-binding-rank bindings base-rank-offset)))
  (table (make-hash-table :test (function eq)) :type hash-table)
  (chunks nil :type list)
  (overlay nil :type list)
  (overlay-length 0 :type (integer 0 *))
  (next-binding-rank -1 :type integer)
  (bindings nil :type list)
  (base-rank-offset 0 :type integer)))
(defun %environment-index-binding (variable index)
  "Return the newest source binding for VARIABLE from INDEX."
  (dolist (binding (%environment-index-overlay index))
    (when (eq variable (car binding))
      (return-from %environment-index-binding (values binding t))))
  (dolist (chunk (%environment-index-chunks index))
    (multiple-value-bind (binding present-p)
        (gethash variable (%environment-index-chunk-table chunk))
      (when present-p
        (return-from %environment-index-binding (values binding t)))))
  (gethash variable (%environment-index-table index)))

(defun %environment-index-rank (variable index)
  "Return VARIABLE rank; ranks preserve source binding order."
  (loop for binding in (%environment-index-overlay index) for rank from (1+ (%environment-index-next-binding-rank index)) when (eq variable (car binding)) do (return-from %environment-index-rank rank))
  (dolist (chunk (%environment-index-chunks index))
    (loop for binding in (%environment-index-chunk-bindings chunk)
          for rank from (%environment-index-chunk-newest-rank chunk)
          when (eq variable (car binding))
            do (return-from %environment-index-rank rank)))
  (loop for binding in (%environment-index-bindings index)
        for rank from (%environment-index-base-rank-offset index)
        when (eq variable (car binding)) return rank))

(defun %make-environment-index (environment &optional (additional-capacity 0))
  "Index ENVIRONMENT by variable identity while preserving first-binding wins."
  (check-type additional-capacity (integer 0 *))
  (let ((table (make-hash-table :test (function eq)
                                :size (+ (length environment) additional-capacity))))
    (dolist (binding environment)
      (multiple-value-bind (present-binding present-p) (gethash (car binding) table)
        (declare (ignore present-binding))
        (unless present-p (setf (gethash (car binding) table) binding))))
    (%make-environment-index-object table nil nil 0 -1 environment 0)))
(defun %copy-environment-index (index)
  "Return a writable index object sharing the immutable contents of INDEX."
  (%make-environment-index-object
    (%environment-index-table index)
    (%environment-index-chunks index)
    (%environment-index-overlay index)
    (%environment-index-overlay-length index)
    (%environment-index-next-binding-rank index)
    (%environment-index-bindings index)
    (%environment-index-base-rank-offset index)))
(progn
(defun %make-environment-index-chunk-from-bindings (bindings newest-rank)
  (let ((table (make-hash-table :test (function eq) :size (length bindings))))
    (dolist (binding bindings)
      (multiple-value-bind (present-binding present-p)
          (gethash (car binding) table)
        (declare (ignore present-binding))
        (unless present-p
          (setf (gethash (car binding) table) binding))))
    (%make-environment-index-chunk table bindings newest-rank (length bindings))))
(defun %merge-environment-index-chunks (newer older)
  (%make-environment-index-chunk-from-bindings
    (append (%environment-index-chunk-bindings newer)
            (%environment-index-chunk-bindings older))
    (%environment-index-chunk-newest-rank newer)))
(defun %compact-environment-index (index)
  "Freeze the overlay and merge equal-sized immutable chunks like a binary counter."
  (if (zerop (%environment-index-overlay-length index))
      index
      (let ((carry (%make-environment-index-chunk-from-bindings
                     (%environment-index-overlay index)
                     (1+ (%environment-index-next-binding-rank index))))
            (chunks (%environment-index-chunks index)))
        (loop while (and chunks
                         (= (%environment-index-chunk-size carry)
                            (%environment-index-chunk-size (car chunks))))
              do (setf carry (%merge-environment-index-chunks carry (car chunks))
                       chunks (cdr chunks)))
        (%make-environment-index-object
          (%environment-index-table index)
          (cons carry chunks)
          nil
          0
          (%environment-index-next-binding-rank index)
          (%environment-index-bindings index)
          (%environment-index-base-rank-offset index)))))
)
(progn
(defmacro %push-environment-index-binding (binding index-place)
  `(progn
     (push ,binding (%environment-index-overlay ,index-place))
     (incf (%environment-index-overlay-length ,index-place))
     (decf (%environment-index-next-binding-rank ,index-place))
     (when (= (%environment-index-overlay-length ,index-place)
              +environment-index-overlay-threshold+)
       (setf ,index-place (%compact-environment-index ,index-place)))))
(defun %extend-environment-index (index bindings)
  "Return INDEX extended by BINDINGS ordered oldest to newest."
  (let ((extended (%copy-environment-index index)))
    (dolist (binding bindings extended)
      (%push-environment-index-binding binding extended))))
)
(defun %environment-index-after-bindings
    (bindings parent-bindings parent-index)
  "Reuse PARENT-INDEX for an unchanged environment or extend a prepended prefix."
  (if (eq bindings parent-bindings)
      parent-index
      (let ((reversed-prefix (quote ()))
            (tail bindings))
        (loop until (eq tail parent-bindings)
              do (unless (consp tail)
                   (return-from
                     %environment-index-after-bindings
                     (%make-environment-index bindings)))
                 (push (car tail) reversed-prefix)
                 (setf tail (cdr tail)))
        (%extend-environment-index parent-index reversed-prefix))))
(defun %alias-cycle-representative (start index)
  "Choose the earliest effective binding in the alias cycle containing START."
  (let* ((binding (%environment-index-binding start index))
         (representative start)
         (best-rank (%environment-index-rank start index))
         (term (cdr binding)))
    (loop until (eq term start)
          for term-binding = (%environment-index-binding term index)
          for rank = (%environment-index-rank term index)
          when (< rank best-rank)
            do (setf representative term
                     best-rank rank)
          do (setf term (cdr term-binding))
          finally (return representative))))
(defun %walk-term-indexed (term index)
  (when (not (logic-var-p term))
    (return-from %walk-term-indexed term))
  (let ((checkpoint term)
        (cursor term)
        (power 1)
        (distance 0))
    (loop
      (multiple-value-bind (binding present-p)
          (%environment-index-binding cursor index)
        (unless present-p
          (return cursor))
        (setf cursor (cdr binding)))
      (unless (logic-var-p cursor)
        (return cursor))
      (incf distance)
      (when (eq checkpoint cursor)
        (return (%alias-cycle-representative cursor index)))
      (when (= distance power)
        (setf checkpoint cursor
              power (* 2 power)
              distance 0)))))
(defun %walk-term (term env)
  "Chase TERM through ENV until it is unbound or not a variable."
  (%walk-term-indexed term (%make-environment-index env)))
