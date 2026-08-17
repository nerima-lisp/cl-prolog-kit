;;;; callgraph/package.lisp - CL-PROLOG-KIT/CALLGRAPH package
;;;;
;;;; Generic, program-representation-independent call-graph analysis over
;;;; cl-prolog-kit: reachability, dead-code detection, mutual-recursion
;;;; detection, FD-constraint graph coloring, and a DCG-based grammar for a
;;;; small textual edge notation ("main -> helper").
;;;;
;;;; Split out of nerima-lisp/cl-cc's packages/prolog-tools, which paired
;;;; this generic logic with a thin adapter walking cl-cc's own AST nodes.
;;;; Nothing in this package or its sibling files (call-graph.lisp,
;;;; edge-dcg.lisp, graph-coloring.lisp) refers to any host AST; a caller
;;;; feeds BUILD-CALL-GRAPH-FROM-EDGES a plain list of names and
;;;; (caller . callee) conses.
;;;;
;;;; Only :CL is used, not :CL-PROLOG-KIT: every cl-prolog-kit engine symbol
;;;; (ASSERTZ, RETRACT, FINDALL, INS, LABELING, ...) is referenced through an
;;;; explicit CL-PROLOG-KIT: prefix throughout this package's source, matching
;;;; the convention the original cl-cc code already used and avoiding the
;;;; ASSERT/CATCH/THROW shadowing conflict a `:use #:cl-prolog-kit` would
;;;; otherwise create alongside `:use #:cl`.
(defpackage #:cl-prolog-kit/callgraph
  (:use #:cl)
  (:export
    ;; call-graph.lisp
    #:call-graph
    #:call-graph-rulebase
    #:call-graph-defined
    #:call-graph-entry-points
    #:build-call-graph-from-edges
    #:reachable-p
    #:reachable-from
    #:direct-callees
    #:find-dead-code
    #:find-mutually-recursive-pairs
    ;; edge-dcg.lisp
    #:tokenize-edge-spec
    #:edge-spec-well-formed-p
    #:parse-edge-spec
    ;; graph-coloring.lisp
    #:color-call-graph
    #:valid-coloring-p))
