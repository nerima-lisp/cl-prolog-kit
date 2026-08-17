;;;; callgraph/test/graph-coloring-test.lisp
;;;;
;;;; Ported from nerima-lisp/cl-cc's packages/prolog-tools/tests/
;;;; graph-coloring-tests.lisp. The original file's fixtures were built
;;;; through cl-cc's AST-specific MK-DEFUN/BUILD-CALL-GRAPH helpers (defined
;;;; in that repo's call-graph-tests.lisp and shared across the test
;;;; system's :serial load order) even though graph-coloring.lisp itself is
;;;; fully generic — so this port replaces those fixtures with direct
;;;; (names, edges) calls to BUILD-CALL-GRAPH-FROM-EDGES; the assertions are
;;;; unchanged.

(in-package #:cl-prolog-kit/callgraph/test)

(defun mk-graph (names edges &key entry-points)
  (build-call-graph-from-edges names edges :entry-points entry-points))

(describe "color-call-graph"
  (it "colors a triangle of mutual calls with 3 colors"
    (let* ((cg (mk-graph '(a b c) '((a . b) (a . c) (b . c)) :entry-points '(a)))
           (coloring (color-call-graph cg 3)))
      (expect coloring :to-be-truthy)
      (expect (valid-coloring-p cg coloring) :to-be-truthy)))

  (it "cannot color a triangle with only 2 colors"
    (let ((cg (mk-graph '(a b c) '((a . b) (a . c) (b . c)) :entry-points '(a))))
      (expect (color-call-graph cg 2) :to-be-null)))

  (it "colors a bipartite call graph with 2 colors"
    (let* ((cg (mk-graph '(a b c d)
                          '((a . c) (a . d) (b . c) (b . d))
                          :entry-points '(a b)))
           (coloring (color-call-graph cg 2)))
      (expect coloring :to-be-truthy)
      (expect (valid-coloring-p cg coloring) :to-be-truthy)))

  (it "kills mutants of the pairwise-disequality invariant"
    (let ((results (run-mutations
                    '(if (eql color-a color-b) nil t)
                    (lambda (form mutation)
                      (declare (ignore mutation))
                      (and (equal (eval `(let ((color-a 1) (color-b 2)) ,form)) t)
                           (equal (eval `(let ((color-a 1) (color-b 1)) ,form)) nil))))))
      (assert-mutation-score results 1.0))))
