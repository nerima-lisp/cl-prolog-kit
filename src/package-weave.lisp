;;;; src/package-weave.lisp
;;;;
;;;; Not folded into src/package.lisp, which CODING_STANDARD.md otherwise makes
;;;; the single home for defpackage forms. CL-PROLOG-KIT/WEAVE belongs to the
;;;; cl-prolog-kit/weave system, which exists only so the test suite can depend on
;;;; cl-weave without the engine doing so -- "dependency-free" is a claim the
;;;; core system's .asd makes and this repository's README repeats. Defining
;;;; the package in src/package.lisp would hand every plain (asdf:load-system
;;;; "cl-prolog-kit") a package whose two exported macros have no definitions.
;;;; package-<subsystem>.lisp is the name the standard's own checker
;;;; recognises for exactly this case.

(defpackage #:cl-prolog-kit/weave
  (:use #:cl)
  (:documentation
   "cl-weave assertions for cl-prolog-kit queries: ASSERT-QUERY for a single
expectation and DEFTEST-QUERIES for a table of them. Defined by the
cl-prolog-kit/weave system, not by cl-prolog-kit itself.")
  (:export
   #:assert-query
   #:deftest-queries))
