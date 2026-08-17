;;;; System names are written as STRINGS, not #:symbols or :keywords: a string
;;;; does not depend on the reader's package state at load time, and a single
;;;; spelling keeps `grep` reliable across the org.

;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version -- the
;;;; file is read in whatever package happens to be current. Saying it makes
;;;; the file self-contained.
(in-package #:asdf-user)

(asdf:defsystem "cl-prolog-kit"
  :description "A small, dependency-free Common Lisp Prolog engine."
  :long-description "A macro-first Common Lisp Prolog engine with CPS proof search, an extensible builtin registry, and a compact rule DSL."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  ;; Single source of truth for the version. flake.nix parses this exact form
  ;; (first match wins) and release.yml refuses to publish a tag that
  ;; disagrees with it, so a release edits this one line.
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :long-name "cl-prolog-kit"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "atom-name")
               (:file "operator-table")
               (:file "module-system")
               (:file "source-registry")
               (:file "logic-variable")
               (:file "clause")
               (:file "predicate-index")
               (:file "data")
               (:file "rulebase-properties")
               (:file "rulebase-compaction")
               (:file "table-variant")
               (:file "environment-index")
               (:file "unification")
               (:file "lexer")
               (:file "lexer-operator-lexemes")
               (:file "lexer-tokenizer")
               (:file "grammar")
               (:file "term-writer")
               (:file "engine")
               (:file "io-context")
               (:file "proof-state")
               (:file "goal-resolution")
               (:file "left-recursion-analysis")
               (:file "prover")
               (:file "tabling")
               (:module "builtins"
                :serial t
                :components ((:file "term-sorting")
                             (:file "core")
                             (:file "unify")
                             (:file "control")
                             (:file "collection")
                             (:file "aggregate")
                             (:file "dynamic")
                             (:file "arithmetic-functions")
                             (:file "arithmetic")
                             (:file "list")
                             (:file "text-conversion")
                             (:file "atom-ops")
                             (:file "atom-number-conversion")
                             (:file "operator")
                             (:file "io")
                             (:file "io-streams")
                             (:file "io-code")))
               (:file "fd-store")
               (:file "builtins/fd")
               (:file "term-inspect")
               (:file "term-compare")
               (:file "term-construct")
               (:file "builtins/list-extra")
               (:file "builtins/apply")
               (:file "builtins/format")
               (:file "builtins/char-type")
               (:file "builtins/term-io")
               (:file "builtins/string")
               (:file "builtins/assoc")
               (:file "builtins/pairs")
               (:file "dcg-runtime")
               (:file "query")
               (:file "source-io")
               (:file "source-directives")
               (:file "source-rollback")
               (:file "source-loader")
               (:file "dsl-compiler")
               (:file "dsl")
               (:file "dcg"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-prolog-kit/test"))))

(asdf:defsystem "cl-prolog-kit/weave"
  :description "cl-weave helpers for testing cl-prolog-kit queries."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :depends-on (#:cl-prolog-kit #:cl-weave)
  :pathname "src"
  ;; :serial t so package-weave.lisp is loaded before the file that reads
  ;; symbols into the package it declares.
  :serial t
  :components ((:file "package-weave")
               (:file "weave")))

;;; The test system is `cl-prolog-kit/test` — singular, slash-separated — with
;;; :pathname "t". It is NOT `cl-prolog-kit-test` and NOT `cl-prolog-kit/tests`.
(asdf:defsystem "cl-prolog-kit/test"
  :description "Test system for cl-prolog-kit."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :depends-on (#:cl-prolog-kit/weave)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:module "support-files"
                :pathname "support"
                :serial t
                :components ((:file "core")
                             (:file "query")
                             (:file "fixtures")))
               (:file "unification-test")
               (:file "iso-conformance-test")
               (:file "iso-inria-test")
               (:file "atom-canonicalization-test")
               (:file "operator-table-test")
               (:file "parser-test")
               (:file "term-writer-test")
               (:file "io-context-test")
               (:file "source-loader-test")
               (:file "source-loader-transactions-test")
               (:file "source-loader-limits-test")
               (:file "engine-surface-test")
               (:file "engine-queries-test")
               (:file "builtin-collections-test")
               (:file "builtin-list-test")
               (:file "builtin-list-extra-test")
               (:file "builtin-dynamic-database-test")
               (:file "builtin-arithmetic-and-flags-test")
               (:file "engine-runtime-test")
               (:file "engine-runtime-index-and-depth-test")
               (:file "engine-runtime-foreign-and-registration-test")
               (:file "engine-runtime-error-contract-test")
               (:file "builtin-term-test")
               (:file "builtin-atom-test")
               (:file "builtin-operator-test")
               (:file "builtin-io-terms-test")
               (:file "builtin-io-streams-lifecycle-test")
               (:file "builtin-io-open-errors-test")
               (:file "builtin-io-code-test")
               (:file "builtin-format-test")
               (:file "builtin-char-type-test")
               (:file "builtin-term-io-test")
               (:file "builtin-string-test")
               (:file "builtin-occurs-check-test")
               (:file "builtin-assoc-test")
               (:file "builtin-pairs-test")
               (:file "builtin-fd-test")
               (:file "module-system-test")
               (:file "dcg-test")
               (:file "weave-public-test")
               (:file "weave-quality-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)
               (error "cl-prolog-kit cl-weave test suite failed."))))

(asdf:defsystem "cl-prolog-kit/examples"
  :description "Runnable examples for cl-prolog-kit."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :depends-on (#:cl-prolog-kit)
  :serial t
  :pathname "examples"
  :components ((:file "quick-start")
               (:file "family-tree")
               (:file "relational-lists")))

;;; Generic, program-representation-independent call-graph analysis:
;;; reachability, dead-code/mutual-recursion detection, FD-constraint graph
;;; coloring, and a DCG grammar for a small textual edge notation. Split out
;;; of nerima-lisp/cl-cc's packages/prolog-tools, which paired this with a
;;; thin adapter walking cl-cc's own AST nodes; cl-cc now depends on this
;;; system and keeps only that adapter.
(asdf:defsystem "cl-prolog-kit/callgraph"
  :description "Generic Prolog-backed call-graph analysis (reachability, dead-code, FD-constraint coloring, edge-notation DCG)."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :depends-on (#:cl-prolog-kit)
  :serial t
  :pathname "callgraph"
  :components ((:file "package")
               (:file "call-graph")
               (:file "edge-dcg")
               (:file "graph-coloring"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-prolog-kit/callgraph/test"))))

;;; A separate secondary system, not folded into `cl-prolog-kit/test`: this
;;; keeps callgraph's cl-weave dependency scoped to exactly the package it
;;; tests, mirrors `cl-prolog-kit/callgraph` living in its own directory, and
;;; lets it be run in isolation (`(asdf:test-system "cl-prolog-kit/callgraph")`)
;;; the same way nerima-lisp/cl-cc's own packages/prolog-tools test system
;;; was run independently of that repo's umbrella test aggregate.
(asdf:defsystem "cl-prolog-kit/callgraph/test"
  :description "cl-weave test suite for cl-prolog-kit/callgraph."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.5.0"
  :homepage "https://github.com/nerima-lisp/cl-prolog-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-prolog-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-prolog-kit.git")
  :depends-on (#:cl-prolog-kit/callgraph #:cl-weave)
  :serial t
  :pathname "callgraph/test"
  :components ((:file "package")
               (:file "call-graph-test")
               (:file "edge-dcg-test")
               (:file "graph-coloring-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)
               (error "cl-prolog-kit/callgraph cl-weave test suite failed."))))
