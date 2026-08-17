;;;; What a logic variable is: recognition, creation, and the
;;;; creation-order bookkeeping that gives every variable a stable ordinal
;;;; within a query.  Independent of unification and environment indexing.

(in-package #:cl-prolog-kit)

(defvar *logic-variable-ordinals* nil)

(defvar *next-logic-variable-ordinal* 0)

(defvar *rule-program-variable-names* nil)
(defvar *rule-program-variable-name-ordinals* nil)
(defvar *rule-program-private-variables* nil)

(defmacro %with-logic-variable-order (&body body)
  "Run BODY inside a variable-creation-order context.

An enclosing context is reused so nested queries (e.g. a builtin proving a
sub-query) keep the ordinals of variables created by their caller."
  `(if *logic-variable-ordinals*
       (progn ,@body)
       (let ((*logic-variable-ordinals* (make-hash-table :test #'eq))
             (*next-logic-variable-ordinal* 0)
             (*rule-program-private-variables*
               (make-array (length *rule-program-variable-names*)
                           :initial-element nil)))
         ,@body)))

(defun %current-private-rule-variable-ordinal (variable)
  "Return VARIABLE's current-context private ordinal and true, or NIL and false."
  (if (and *rule-program-private-variables*
           (null (symbol-package variable)))
      (multiple-value-bind (ordinal present-p)
          (gethash (symbol-name variable)
                   *rule-program-variable-name-ordinals*)
        (if (and present-p
                 (eq variable
                     (svref *rule-program-private-variables* ordinal)))
            (values ordinal t)
            (values nil nil)))
      (values nil nil)))

(defun %register-logic-variable (variable)
  "Assign VARIABLE its stable creation ordinal and return VARIABLE."
  (unless *logic-variable-ordinals*
    (error "Logic variables require an active ordering context."))
  (multiple-value-bind (private-ordinal private-p)
      (%current-private-rule-variable-ordinal variable)
    (declare (ignore private-ordinal))
    (unless private-p
      (multiple-value-bind (ordinal present-p)
          (gethash variable *logic-variable-ordinals*)
        (declare (ignore ordinal))
        (unless present-p
          (setf (gethash variable *logic-variable-ordinals*)
                (prog1
                    *next-logic-variable-ordinal*
                  (incf *next-logic-variable-ordinal*)))))))
  variable)

(defun %logic-variable-ordinal (variable)
  "Return VARIABLE's registered ordinal."
  (multiple-value-bind (ordinal present-p)
      (gethash variable *logic-variable-ordinals*)
    (if present-p
        ordinal
        (multiple-value-bind (private-ordinal private-p)
            (%current-private-rule-variable-ordinal variable)
          (if private-p
              private-ordinal
              (error "Unregistered logic variable ~S." variable))))))

(declaim (inline logic-var-p))
(defun logic-var-p (term)
  "Return true when TERM is a logic variable rather than a dedicated Prolog atom.

A `?'-prefixed name interned in either atom package -- USER-ATOMS for `'?x',
VERBATIM-ATOMS for `'?X' -- is an atom the source quoted deliberately, not a
variable."
  (and
    (symbolp term)
    (let ((package (symbol-package term))
          (name (symbol-name term)))
      (and
        (or
          (null package)
          (and
            (not (keywordp term))
            (not (eq package *user-atom-package*))
            (not (eq package *verbatim-atom-package*))))
        (plusp (length name))
        (char= (char name 0) #\?)))))


(defun fresh-logic-variable (&optional (prefix "?VAR"))
  "Return a fresh, never-before-seen logic variable."
  (let ((variable (gensym prefix)))
    (if *logic-variable-ordinals*
        (%register-logic-variable variable)
        variable)))

(defparameter *rule-program-variable-names*
  (let ((names (make-array 256)))
    (dotimes (index (length names) names)
      (setf (svref names index)
            (format nil "?RULE-PROGRAM-~D" index)))))

(defparameter *rule-program-variable-name-ordinals*
  (let ((ordinals
          (make-hash-table
            :test #'equal
            :size (length *rule-program-variable-names*))))
    (dotimes (index (length *rule-program-variable-names*) ordinals)
      (setf (gethash (svref *rule-program-variable-names* index) ordinals)
            index))))

(declaim (inline %fresh-rule-program-variable))
(defun %fresh-rule-program-variable ()
  "Return a registered fresh variable with a cached printable name."
  (let ((ordinal *next-logic-variable-ordinal*))
    (if (and *logic-variable-ordinals*
             (< ordinal (length *rule-program-variable-names*)))
        (let ((variable
                (make-symbol
                  (svref *rule-program-variable-names* ordinal))))
          (setf (svref *rule-program-private-variables* ordinal) variable)
          (incf *next-logic-variable-ordinal*)
          variable)
        (fresh-logic-variable "?RULE-PROGRAM-"))))
