;;;; Predicate lookup layer: the per-predicate index buckets keyed by
;;;; (module predicate arity) and the immutable current-state descriptors
;;;; that provide first-argument indexing.  Operates on stored clauses from
;;;; clause.lisp; the rulebase that owns these tables lives in data.lisp.
(in-package #:cl-prolog-kit)

(defstruct (%predicate-descriptor
    (:copier nil)
    (:constructor
      %make-predicate-descriptor
      (entries
       symbol-first-argument-index
       symbol-eq-first-argument-index
       atom-first-argument-index
       string-first-argument-index
       compound-first-argument-index
       compound-eq-first-argument-index
       compound-max-arity
       variable-entries
       compound-fallback-entries)))
  "Immutable current-state lookup data for one predicate.

Symbol buckets are keyed by atom text, because two distinct symbols can denote
one atom; the EQ table maps every symbol seen in a clause head to the same
bucket its text selects, so a goal naming that exact symbol skips building the
text.  Compound buckets are keyed by functor text and then arity, and the
compound EQ table likewise maps each functor symbol to its text's arity alist."
  (entries (quote ()) :type list :read-only t)
  (symbol-first-argument-index
    (make-hash-table :test (function equal))
    :type hash-table :read-only t)
  (symbol-eq-first-argument-index
    (make-hash-table :test (function eq))
    :type hash-table :read-only t)
  (atom-first-argument-index
    (make-hash-table :test (function eql))
    :type hash-table :read-only t)
  (string-first-argument-index
    (make-hash-table :test (function equal))
    :type hash-table :read-only t)
  (compound-first-argument-index
    (make-hash-table :test (function equal))
    :type hash-table :read-only t)
  (compound-eq-first-argument-index
    (make-hash-table :test (function eq))
    :type hash-table :read-only t)
  (compound-max-arity -1 :type fixnum :read-only t)
  (variable-entries (quote ()) :type list :read-only t)
  (compound-fallback-entries (quote ()) :type list :read-only t))

(defun %stored-clause-predicate-key (entry)
  "Return ENTRY's (module predicate arity) key, or NIL for a head with no functor.
Signals an error when the argument list is dotted or circular.  LENGTH does not
return on a circular list, so LIST-LENGTH detects the cycle, and the error names
only the functor because printing the head itself would not terminate either."
  (let ((head (clause-head (%stored-clause-clause entry))))
    (when (and (consp head) (symbolp (first head)))
      (let ((arity (list-length (rest head))))
        (unless arity
          (error "The clause head for ~S has a circular argument list."
                 (first head)))
        (list (%stored-clause-module entry) (first head) arity)))))

(defun %append-to-predicate-index-tail! (key entry index tails)
  "Append ENTRY to KEY's bucket in INDEX using TAILS in O(1)."
  (let ((cell (list entry))
        (tail (gethash key tails)))
    (if tail (setf (cdr tail) cell)
      (setf (gethash key index) cell))
    (setf (gethash key tails) cell)))

(defun %insert-index-entry! (key entry position index tails)
  "Insert ENTRY into KEY's bucket at POSITION while maintaining its tail."
  (ecase position
    (:first
      (let* ((entries (gethash key index))
             (cell (cons entry entries)))
        (setf (gethash key index) cell)
        (unless entries
          (setf (gethash key tails) cell))))
    (:last (%append-to-predicate-index-tail! key entry index tails))))

(defun %stored-clause-first-argument (entry)
  (let ((head (clause-head (%stored-clause-clause entry))))
    (and (consp (rest head)) (second head))))

(defun %make-rulebase-predicate-index (entries)
  "Build the predicate index for ENTRIES."
  (let ((predicate-index (make-hash-table :test #'equal))
        (predicate-tails (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (%stored-clause-predicate-key entry)))
        (when key
          (%append-to-predicate-index-tail! key entry predicate-index predicate-tails))))
    (values predicate-index predicate-tails)))

(defun %set-rulebase-predicate-descriptor! (descriptors module predicate arity descriptor)
  "Install DESCRIPTOR in the nested MODULE/PREDICATE/ARITY index."
  (let ((predicates (gethash module descriptors)))
    (cond
      (descriptor
        (unless predicates
          (setf predicates (make-hash-table :test #'eq)
                (gethash module descriptors) predicates))
        (let ((arities (gethash predicate predicates)))
          (unless arities
            (setf arities (make-hash-table :test #'eql)
                  (gethash predicate predicates) arities))
          (setf (gethash arity arities) descriptor)))
      (predicates
        (let ((arities (gethash predicate predicates)))
          (when arities
            (remhash arity arities)
            (when (zerop (hash-table-count arities))
              (remhash predicate predicates))
            (when (zerop (hash-table-count predicates))
              (remhash module descriptors)))))))
  descriptor)

(defun %finite-proper-list-length (list)
  "Return LIST's length when it is a finite proper list, otherwise NIL."
  (loop for fast = list then (cddr fast)
        for slow = list then (cdr slow)
        for count of-type fixnum from 0 by 2
        do (cond ((null fast) (return count))
                 ((atom fast) (return nil))
                 ((null (cdr fast)) (return (1+ count)))
                 ((atom (cdr fast)) (return nil)))
        when (and (plusp count) (eq fast slow))
          do (return nil)))

(defun %indexable-compound-arity (term)
  "Return TERM's arity when TERM is a proper compound with an atom functor.
Lists are conses too, so `[a, b]' is indexed like `a(b)', which is how
unification treats it.  Any other cons returns NIL."
  (let ((functor (car term)))
    (when (and (symbolp functor) (not (logic-var-p functor)))
      (let ((length (%finite-proper-list-length term)))
        (and length (1- length))))))

(defun %merge-position-candidates (left right)
  "Merge two (POSITION . ENTRY) lists that are each in ascending POSITION order."
  (loop while (or left right)
        collect (if (or (null right) (and left (< (caar left) (caar right))))
                    (pop left)
                    (pop right))))

(defun %build-predicate-descriptor (entries)
  "Build detached current-state lookup lists for ENTRIES."
  (let ((owned-entries (copy-list entries))
        (symbol-candidates (make-hash-table :test (function equal)))
        (symbol-texts (make-hash-table :test (function eq)))
        (atom-candidates (make-hash-table :test (function eql)))
        (string-candidates (make-hash-table :test (function equal)))
        (compound-candidates (make-hash-table :test (function equal)))
        (functor-texts (make-hash-table :test (function eq)))
        (max-arity -1)
        (variable-entries (quote ()))
        (unkeyed-compound-entries (quote ()))
        (position 0))
    (declare (type fixnum max-arity position))
    (dolist (entry owned-entries)
      (let ((candidate (cons position entry))
            (first-argument (%stored-clause-first-argument entry)))
        (incf position)
        (cond
          ((logic-var-p first-argument) (push candidate variable-entries))
          ((symbolp first-argument)
           (let ((text (or (gethash first-argument symbol-texts)
                           (setf (gethash first-argument symbol-texts)
                                 (%atom-text first-argument)))))
             (push candidate (gethash text symbol-candidates))))
          ((or (numberp first-argument) (characterp first-argument))
           (push candidate (gethash first-argument atom-candidates)))
          ((stringp first-argument)
           (push candidate (gethash first-argument string-candidates)))
          ((consp first-argument)
           (let ((arity (%indexable-compound-arity first-argument)))
             (if arity
                 (let* ((functor (car first-argument))
                        (text (or (gethash functor functor-texts)
                                  (setf (gethash functor functor-texts)
                                        (%atom-text functor)))))
                   (setf max-arity (max max-arity arity))
                   (push candidate
                         (gethash (cons text arity) compound-candidates)))
                 (push candidate unkeyed-compound-entries)))))))
    (setf variable-entries (nreverse variable-entries))
    (let ((compound-fallback
            (%merge-position-candidates
             variable-entries (nreverse unkeyed-compound-entries)))
          (compound-index (make-hash-table :test (function equal)))
          (symbol-eq-index (make-hash-table :test (function eq)))
          (compound-eq-index (make-hash-table :test (function eq))))
      (labels ((finish-candidates (exact-candidates fallback)
                 (mapcar (function cdr)
                         (%merge-position-candidates
                          (nreverse exact-candidates) fallback)))
               (finish-index (index fallback)
                 (loop
                   for key being the hash-keys of index
                     using (hash-value exact-candidates)
                   do (setf (gethash key index)
                            (finish-candidates exact-candidates fallback)))
                 index))
        (finish-index symbol-candidates variable-entries)
        (finish-index atom-candidates variable-entries)
        (finish-index string-candidates variable-entries)
        (loop
          for key being the hash-keys of compound-candidates
            using (hash-value exact-candidates)
          do (push (cons (cdr key)
                         (finish-candidates exact-candidates compound-fallback))
                   (gethash (car key) compound-index)))
        (loop
          for symbol being the hash-keys of symbol-texts using (hash-value text)
          do (setf (gethash symbol symbol-eq-index)
                   (gethash text symbol-candidates)))
        (loop
          for functor being the hash-keys of functor-texts using (hash-value text)
          do (setf (gethash functor compound-eq-index)
                   (gethash text compound-index)))
        (%make-predicate-descriptor
          owned-entries
          symbol-candidates
          symbol-eq-index
          atom-candidates
          string-candidates
          compound-index
          compound-eq-index
          max-arity
          (mapcar (function cdr) variable-entries)
          (mapcar (function cdr) compound-fallback))))))

(defun %make-rulebase-predicate-descriptors (predicate-index revision)
  "Build the nested current-state descriptor index at REVISION."
  (let ((descriptors (make-hash-table :test #'eq)))
    (loop
      for key being the hash-keys of predicate-index
        using (hash-value entries)
      do
         (destructuring-bind (module predicate arity) key
           (let ((visible-entries
                   (%visible-stored-clauses entries revision)))
             (when visible-entries
               (%set-rulebase-predicate-descriptor!
                 descriptors module predicate arity
                 (%build-predicate-descriptor visible-entries))))))
    descriptors))

(defun %predicate-descriptor-compound-entries (descriptor term)
  "Return precomputed candidates for the cons goal argument TERM.
A functor that is a variable, or a spine ending in a variable, may still unify
with any keyed compound, so those goals get every entry."
  (let ((functor (car term)))
    (cond
      ((logic-var-p functor) (%predicate-descriptor-entries descriptor))
      ((not (symbolp functor))
       (%predicate-descriptor-compound-fallback-entries descriptor))
      (t
       (let ((arities
               (or (gethash functor
                            (%predicate-descriptor-compound-eq-first-argument-index
                             descriptor))
                   (let ((index (%predicate-descriptor-compound-first-argument-index
                                 descriptor)))
                     (and (plusp (hash-table-count index))
                          (gethash (%atom-text functor) index)))))
             (fallback (%predicate-descriptor-compound-fallback-entries descriptor))
             (max-arity (%predicate-descriptor-compound-max-arity descriptor)))
         (if (null arities)
             fallback
             ;; Walk at most MAX-ARITY argument cells: a longer or cyclic spine
             ;; cannot unify with any keyed clause argument.
             (loop for tail = (cdr term) then (cdr tail)
                   for arity of-type fixnum from 0
                   do (cond
                        ((null tail)
                         (return (or (cdr (assoc arity arities)) fallback)))
                        ((logic-var-p tail)
                         (return (%predicate-descriptor-entries descriptor)))
                        ((atom tail) (return fallback))
                        ((>= arity max-arity) (return fallback))))))))))

(defun %predicate-descriptor-first-argument-entries (descriptor first-argument)
  "Return precomputed candidates for FIRST-ARGUMENT without mutating indexes."
  (cond
    ((logic-var-p first-argument) (%predicate-descriptor-entries descriptor))
    ((symbolp first-argument)
     (or
       (gethash
         first-argument
         (%predicate-descriptor-symbol-eq-first-argument-index descriptor))
       (let ((index (%predicate-descriptor-symbol-first-argument-index descriptor)))
         (and (plusp (hash-table-count index))
              (gethash (%atom-text first-argument) index)))
       (%predicate-descriptor-variable-entries descriptor)))
    ((or (numberp first-argument) (characterp first-argument))
     (or
       (gethash
         first-argument
         (%predicate-descriptor-atom-first-argument-index descriptor))
       (%predicate-descriptor-variable-entries descriptor)))
    ((stringp first-argument)
     (or
       (gethash
         first-argument
         (%predicate-descriptor-string-first-argument-index descriptor))
       (%predicate-descriptor-variable-entries descriptor)))
    ((consp first-argument)
     (%predicate-descriptor-compound-entries descriptor first-argument))
    (t (%predicate-descriptor-entries descriptor))))
