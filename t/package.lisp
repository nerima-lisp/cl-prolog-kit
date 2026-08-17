;;;; t/package.lisp -- the package every file in the suite is read into.
;;;;
;;;; Named package.lisp, not support.lisp: CODING_STANDARD.md exempts
;;;; package.lisp and suite.lisp from the <source>-test.lisp rule because they
;;;; are manifests, and that is all this file is. The shared fixtures and
;;;; assertions it used to sit beside live in t/support/.

(defpackage #:cl-prolog-kit.tests
  (:use #:cl #:cl-prolog-kit #:cl-prolog-kit/weave)
  (:shadowing-import-from #:cl-prolog-kit #:assert #:catch #:throw)
  (:export #:deftest
           #:deftest-table
           #:deftest-io-variants
           #:deftest-io-queries
           #:deftest-queries
           #:assert-query
           #:with-single-query-solution
           #:is
           #:is-equal
           #:is-same-set
           #:signals-error
           #:signals-prolog-condition
           #:make-family-rulebase))

(in-package #:cl-prolog-kit.tests)
