;;;; term_to_atom/2 and read_term_from_atom/3: convert between a term and its
;;;; textual (re-readable) representation, routing parser failures through the
;;;; engine's catchable ISO syntax/resource errors.

(in-package #:cl-prolog-kit)

(defun %parse-term-from-text (text rulebase environment operation)
  "Parse one Prolog term from TEXT under RULEBASE's operator table and
double_quotes flag, converting parser failures into catchable ISO errors."
  (let ((*double-quotes* (%double-quotes-mode rulebase)))
    (handler-case (read-prolog-term text (rulebase-operator-table rulebase))
      (prolog-parser-resource-error (condition)
        (%raise-parser-resource-error condition environment operation))
      (prolog-parse-error (condition)
        (%raise-syntax-error condition environment operation)))))

(defun %term-text-source (value environment operation)
  "Return the textual source of VALUE (an atom, number, or CL string)."
  (%text-of value environment operation
            :accept :atomic
            :instantiation "the source text must be instantiated"
            :type-message "expected an atom, number, or string"))

(define-iso-builtin (term_to_atom term (atom :raw)) "TERM_TO_ATOM"
  (if (logic-var-p resolved-term)
      ;; Parse mode: ATOM must be instantiated text.
      (let* ((text (%term-text-source (logic-substitute atom environment)
                                      environment operation))
             (parsed (%parse-term-from-text
                      text rulebase environment operation)))
        (%unify-emit term parsed environment emit))
      ;; Write mode: ATOM = the quoted, re-readable text of TERM.
      (%unify-emit atom (%text-atom (prolog-term-string resolved-term))
                   environment emit)))

(define-iso-builtin (read_term_from_atom atom (term :raw) options) "READ_TERM_FROM_ATOM"
  (let ((text (%term-text-source resolved-atom environment operation)))
    ;; Shares the read-option validator with read_term/2,3, so an unsupported
    ;; option raises the ISO 8.14.1.3 domain_error(read_option, O) here too
    ;; rather than being accepted and ignored.
    (%io-read-options resolved-options environment operation)
    (%unify-emit term
                 (%parse-term-from-text text rulebase environment operation)
                 environment emit)))
