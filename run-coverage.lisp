;;;; The Lisp-level entry point for the cl-prolog-kit coverage report.
;;;;
;;;;     sbcl --script run-coverage.lisp [report-directory]
;;;;
;;;; Mirrors run-tests.lisp's role as the one org-wide answer to "how do I
;;;; run this without Nix?" (see PACKAGE_STANDARD.md); flake.nix's
;;;; checks.coverage invokes exactly this script so the local command and the
;;;; CI gate cannot drift apart.
;;;;
;;;; The project-owned :cl-prolog-kit, :cl-prolog-kit/weave, and
;;;; :cl-prolog-kit/callgraph systems are compiled with SB-COVER's instrumentation
;;;; on. cl-weave is loaded before coverage is enabled, while both test systems
;;;; load after it is disabled, so neither external harness code nor tests enter
;;;; the report.
;;;;
;;;; cl-weave must be reachable through CL_SOURCE_REGISTRY, exactly as
;;;; run-tests.lisp requires.
(require :asdf)

(require :sb-cover)

(let ((root
      (make-pathname :name nil :type nil :version nil :defaults *load-truename*)))
  (asdf:load-asd (merge-pathnames "cl-prolog-kit.asd" root)))

(progn
  (asdf:load-system "cl-weave")
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:load-system "cl-prolog-kit" :force t)
  (asdf:load-system "cl-prolog-kit/weave" :force t)
  (asdf:load-system "cl-prolog-kit/callgraph" :force t))

(declaim (optimize (sb-cover:store-coverage-data 0)))

;; Both systems register tests globally in cl-weave, so load them before the
;; single aggregate run and reject a red suite before writing the report.
(progn
  (asdf:load-system "cl-prolog-kit/test")
  (asdf:load-system "cl-prolog-kit/callgraph/test")
  (unless (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)
    (error "cl-prolog-kit cl-weave test suites failed.")))

(let* ((argument (second sb-ext:*posix-argv*))
       (report-directory
      (if argument (uiop:ensure-directory-pathname (uiop:parse-native-namestring argument))
        (merge-pathnames "coverage/" (truename ".")))))
  (sb-cover:report report-directory)
  (format t "~&Coverage report written to ~A~%" report-directory))
