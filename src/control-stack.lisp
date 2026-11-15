;;;; Control stack limit: the stack pointer address a root query may not
;;;; pass, and the check that raises PROLOG-STACK-LIMIT-EXCEEDED there.
;;;; Loaded before the term code so that any of it can check the limit.

(in-package #:cl-prolog-kit)

;;; SBCL signals running out of control stack by hitting a guard page.
;;; Under --lose-on-corruption, which --script implies, that is fatal, and
;;; even without it a second hit before the stack unwinds past the guard is.
;;; So the root query records a limit short of the end of its thread's
;;; control stack, and two places compare the stack pointer with it: every
;;; goal dispatch, through which proof search deepens the stack, and every
;;; dereference in %WALK-TERM-INDEXED, through which unification, the occurs
;;; check and term copying walk a term node by node within one builtin
;;; call.  Walks that dereference only variables, such as variant
;;; canonicalization over a ground term, and the term writer, are not
;;; checked.  The bounds are the thread's own: SBCL binds
;;; *CONTROL-STACK-START* and *CONTROL-STACK-END* per thread, and
;;; CONTROL-STACK-USAGE in SBCL's gc.lisp reads them the same way.

(defconstant +control-stack-grows-downward-p+
  (and (member :stack-grows-downward-not-upward sb-impl:+internal-features+) t)
  "True where the control stack grows toward lower addresses (x86-64), false
where it grows upward (arm64).")

(defparameter *control-stack-limit-fraction* 4/5
  "Fraction of the thread's control stack that proof search may use before
a goal dispatch raises PROLOG-STACK-LIMIT-EXCEEDED.  The rest is left for
the builtin call running at the last dispatch, for signalling and handling
the condition, and for cleanup goals run while it unwinds; see
*CONTROL-STACK-CLEANUP-MARGIN*.")

(defparameter *control-stack-cleanup-margin* (* 224 1024)
  "Bytes of control stack past the limit that a cleanup goal may use.
SBCL runs an unwind-protect cleanup on top of the stack at the point the
non-local exit started, so after a stack stop every cleanup starts past the
limit.  A rule level of one assertz/1 and one is/2 takes about 1150 bytes
on arm64 SBCL 2.6, so this is about 200 such levels.  The root adds it once
to its limit to get *CONTROL-STACK-CEILING*.")

(defconstant +control-stack-ceiling-headroom+ (* 32 1024)
  "Bytes the ceiling stays short of the guard page, for signalling and
handling the condition raised at the ceiling.")

(declaim (type fixnum *control-stack-limit*))
(defvar *control-stack-limit*
  (if +control-stack-grows-downward-p+ 0 most-positive-fixnum)
  "Stack pointer address past which a goal dispatch raises
PROLOG-STACK-LIMIT-EXCEEDED.  Bound by the root query; the global value
never trips.")

(declaim (type fixnum *control-stack-ceiling*))
(defvar *control-stack-ceiling* *control-stack-limit*
  "Stack pointer address that *CONTROL-STACK-LIMIT* takes inside a cleanup
goal.  Bound by the root query once, so nested cleanups share it.")

(defun %control-stack-limit ()
  "Return the stack pointer address at *CONTROL-STACK-LIMIT-FRACTION* of the
current thread's control stack."
  (let* ((start (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-start*)))
         (end (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-end*)))
         (usable (floor (* (- end start) *control-stack-limit-fraction*))))
    (if +control-stack-grows-downward-p+
        (- end usable)
        (+ start usable))))

(defun %control-stack-ceiling (limit)
  "Return LIMIT moved *CONTROL-STACK-CLEANUP-MARGIN* bytes toward the end
of the current thread's control stack, but no closer than
+CONTROL-STACK-CEILING-HEADROOM+ to its guard page, and never short of
LIMIT.  SBCL's runtime (validate.h) puts the guard page and the hard guard
page below it at the end of the stack, each one backend page long."
  (let* ((start (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-start*)))
         (end (sb-sys:sap-int (sb-int:descriptor-sap sb-vm:*control-stack-end*)))
         (reserved (+ (* 2 sb-c:+backend-page-bytes+)
                      +control-stack-ceiling-headroom+)))
    (if +control-stack-grows-downward-p+
        (min limit (max (- limit *control-stack-cleanup-margin*)
                        (+ start reserved)))
        (max limit (min (+ limit *control-stack-cleanup-margin*)
                        (- end reserved))))))

(defun %raise-stack-limit-exceeded ()
  (%raise-resource-error "CONTROL_STACK" nil (%iso-atom "CALL")
                         "control stack limit exceeded"
                         :condition-type 'prolog-stack-limit-exceeded))

(defmacro %control-stack-past-limit-p ()
  "Return true when the stack pointer is past *CONTROL-STACK-LIMIT*."
  `(,(if +control-stack-grows-downward-p+ '< '>)
    (sb-sys:sap-int (sb-kernel:current-sp))
    *control-stack-limit*))

(defmacro %check-control-stack ()
  "Raise PROLOG-STACK-LIMIT-EXCEEDED when the stack pointer is past
*CONTROL-STACK-LIMIT*."
  `(when (%control-stack-past-limit-p)
     (%raise-stack-limit-exceeded)))
