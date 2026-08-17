;;;; callgraph/test/call-graph-test.lisp
;;;;
;;;; Ported from nerima-lisp/cl-cc's packages/prolog-tools/tests/
;;;; call-graph-tests.lisp: same scenarios, but fixtures are built directly
;;;; as (names, edges) pairs and passed to BUILD-CALL-GRAPH-FROM-EDGES
;;;; rather than constructed as cl-cc AST nodes and passed through the
;;;; AST-specific BUILD-CALL-GRAPH adapter (which now lives in cl-cc's
;;;; thinned-down packages/prolog-tools and is not exercised here). The
;;;; snapshot-based dead-code scenario from the original file is NOT ported:
;;;; it exists only to pin cl-cc's own package-qualified printing behavior
;;;; and stays in cl-cc as an integration test of the adapter.

(in-package #:cl-prolog-kit/callgraph/test)

(defun mk-graph (names edges &key entry-points)
  (build-call-graph-from-edges names edges :entry-points entry-points))

(describe "build-call-graph-from-edges"
  (it "treats a direct call as reachable"
    (let ((cg (mk-graph '(main helper) '((main . helper)) :entry-points '(main))))
      (expect (reachable-p cg 'main 'helper) :to-be-truthy)))

  (it "follows transitive calls"
    (let ((cg (mk-graph '(main helper leaf)
                         '((main . helper) (helper . leaf))
                         :entry-points '(main))))
      (expect (reachable-p cg 'main 'leaf) :to-be-truthy)
      (expect (reachable-p cg 'leaf 'main) :to-be-falsy)))

  (it "reports direct callees via a single findall query"
    (let ((cg (mk-graph '(main a b) '((main . a) (main . b)) :entry-points '(main))))
      (expect (sort (copy-list (direct-callees cg 'main)) #'string< :key #'symbol-name)
              :to-equal '(a b))))

  (it "finds functions unreachable from any entry point"
    (let ((cg (mk-graph '(main helper leaf orphan)
                         '((main . helper) (helper . leaf) (orphan . leaf))
                         :entry-points '(main))))
      (expect (find-dead-code cg) :to-equal '(orphan))))

  (it "finds mutually recursive function pairs"
    (let ((cg (mk-graph '(a b) '((a . b) (b . a)) :entry-points '(a))))
      (expect (find-mutually-recursive-pairs cg) :to-equal '((a . b)))))

  (it-property "reachable-from is closed under composition on random call graphs"
      ((edges (gen-list (gen-tuple (gen-member '(a b c d e)) (gen-member '(a b c d e)))
                         :min-length 0 :max-length 10)))
    (let* ((names '(a b c d e))
           (cg (mk-graph names
                          (mapcar (lambda (edge) (cons (first edge) (second edge))) edges)
                          :entry-points '(a))))
      (dolist (edge edges)
        (expect (reachable-p cg (first edge) (second edge)) :to-be-truthy))
      (dolist (x names)
        (let ((reachable-from-x (reachable-from cg x)))
          (dolist (y reachable-from-x)
            (dolist (z (reachable-from cg y))
              (expect (member z reachable-from-x) :to-be-truthy))))))))
