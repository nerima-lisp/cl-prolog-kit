;;;; Control stack limit: the stack pointer address a root query may not
;;;; pass, and the check that raises PROLOG-STACK-LIMIT-EXCEEDED there.
;;;; Loaded before the term code so that any of it can check the limit.

(in-package #:cl-prolog-kit)

;;; SBCL signals running out of control stack by hitting a guard page.
;;; Under --lose-on-corruption, which --script implies, that is fatal, and
;;; even without it a second hit before the stack unwinds past the guard is.
;;; So the root query records a limit short of the end of its thread's
;;; control stack, and every goal dispatch compares the stack pointer with
;;; it.  Proof search deepens the stack only through goal dispatch, so the
;;; check sees every level of it.  The bounds are the thread's own: SBCL
;;; binds *CONTROL-STACK-START* and *CONTROL-STACK-END* per thread, and
;;; CONTROL-STACK-USAGE in SBCL's gc.lisp reads them the same way.

(defconstant +control-stack-grows-downward-p+
  (and (member :stack-grows-downward-not-upward sb-impl:+internal-features+) t)
  "True where the control stack grows toward lower addresses (x86-64), false
where it grows upward (arm64).")

(defparameter *control-stack-limit-fraction* 4/5
  "Fraction of the thread's control stack that proof search may use before
a goal dispatch raises PROLOG-STACK-LIMIT-EXCEEDED.  The rest is left for
the builtin call running at the last dispatch, for signalling and handling
the condition, and for cleanup goals run while it unwinds.")

(declaim (type fixnum *control-stack-limit*))
(defvar *control-stack-limit*
  (if +control-stack-grows-downward-p+ 0 most-positive-fixnum)
  "Stack pointer address past which a goal dispatch raises
PROLOG-STACK-LIMIT-EXCEEDED.  Bound by the root query; the global value
never trips.")

(defun %control-stack-limit ()
  "Return the stack pointer address at *CONTROL-STACK-LIMIT-FRACTION* of the
current thread's control stack."
  (let* ((start (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-start*)))
         (end (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-end*)))
         (usable (floor (* (- end start) *control-stack-limit-fraction*))))
    (if +control-stack-grows-downward-p+
        (- end usable)
        (+ start usable))))

(defun %raise-stack-limit-exceeded ()
  (%raise-resource-error "CONTROL_STACK" nil (%iso-atom "CALL")
                         "control stack limit exceeded"
                         :condition-type 'prolog-stack-limit-exceeded))

(defmacro %check-control-stack ()
  "Raise PROLOG-STACK-LIMIT-EXCEEDED when the stack pointer is past
*CONTROL-STACK-LIMIT*."
  `(when (,(if +control-stack-grows-downward-p+ '< '>)
          (sb-sys:sap-int (sb-kernel:current-sp))
          *control-stack-limit*)
     (%raise-stack-limit-exceeded)))
