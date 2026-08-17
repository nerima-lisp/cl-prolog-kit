;;;; Character classification and case conversion: char_type/2, code_type/2,
;;;; upcase_atom/2, downcase_atom/2.  A SWI-compatible subset of the type
;;;; specifiers, sharing one classifier between the char and code variants.

(in-package #:cl-prolog-kit)

(defun %char-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)
          :test #'char=))

(defun %char-graphic-p (character)
  (and (graphic-char-p character) (not (char= character #\Space))))

(defun %classifier-out (character code-mode-p)
  "Wrap CHARACTER as the out-value kind expected by the caller: a character
code when CODE-MODE-P, otherwise a one-character atom."
  (if code-mode-p (char-code character) (%text-atom (string character))))

(defun %char-type-holds-p (character type code-mode-p environment operation emit
                           base-environment)
  "Test CHARACTER against the TYPE specifier.  For parametric specifiers the
out-argument is unified through EMIT; simple specifiers emit BASE-ENVIRONMENT
on success.  Returns without emitting on failure."
  (labels ((ok () (funcall emit base-environment))
           (out (term value) (%unify-emit term value base-environment emit))
           (simple (test) (when test (ok))))
    (cond
      ((%term-atom-p type)
       (let ((name (symbol-name type)))
         (cond
           ((string-equal name "alpha") (simple (alpha-char-p character)))
           ((string-equal name "alnum") (simple (alphanumericp character)))
           ((string-equal name "digit") (simple (digit-char-p character)))
           ((or (string-equal name "space") (string-equal name "white"))
            (simple (%char-whitespace-p character)))
           ((string-equal name "upper") (simple (upper-case-p character)))
           ((string-equal name "lower") (simple (lower-case-p character)))
           ((string-equal name "punct")
            (simple (and (%char-graphic-p character)
                         (not (alphanumericp character)))))
           ((string-equal name "graph") (simple (%char-graphic-p character)))
           ((string-equal name "csym")
            (simple (or (alphanumericp character) (char= character #\_))))
           ((string-equal name "csymf")
            (simple (or (alpha-char-p character) (char= character #\_))))
           ((string-equal name "ascii") (simple (< (char-code character) 128)))
           ;; SWI `graphic' means a Prolog symbol character, distinct from `graph'.
           ((string-equal name "graphic")
            (simple (%prolog-graphic-character-p character)))
           ((or (string-equal name "end_of_line") (string-equal name "newline"))
            (simple (member character '(#\Newline #\Return) :test #'char=)))
           ((string-equal name "period")
            (simple (member character '(#\. #\! #\?) :test #'char=)))
           (t (%raise-domain-error "CHAR_TYPE" type environment operation
                                   "unsupported char_type/2 specifier")))))
      ((and (%proper-list-p type) (= (length type) 2) (%term-atom-p (first type)))
       (let ((tag (symbol-name (first type)))
             (arg (second type)))
         (cond
           ((string-equal tag "digit")
            (let ((weight (digit-char-p character)))
              (when weight (out arg weight))))
           ((string-equal tag "to_lower")
            (out arg (%classifier-out (char-downcase character) code-mode-p)))
           ((string-equal tag "to_upper")
            (out arg (%classifier-out (char-upcase character) code-mode-p)))
           ((string-equal tag "upper")
            (when (upper-case-p character)
              (out arg (%classifier-out (char-downcase character) code-mode-p))))
           ((string-equal tag "lower")
            (when (lower-case-p character)
              (out arg (%classifier-out (char-upcase character) code-mode-p))))
           ((string-equal tag "code")
            (out arg (char-code character)))
           (t (%raise-domain-error "CHAR_TYPE" type environment operation
                                   "unsupported char_type/2 specifier")))))
      ((logic-var-p type)
       (%raise-instantiation-error environment operation
                                   "char_type/2 type must be instantiated"))
      (t (%raise-type-error "CHAR_TYPE" type environment operation
                            "char_type/2 type must be an atom or compound")))))

(define-iso-builtin (char_type char (type :raw)) "CHAR_TYPE"
  (unless (%character-atom-p resolved-char)
    (if (logic-var-p resolved-char)
        (%raise-instantiation-error environment operation
                                    "char_type/2 char must be instantiated")
        (%raise-type-error "CHARACTER" resolved-char environment operation
                           "char_type/2 expects a one-character atom")))
  (%char-type-holds-p (char (%atom-text resolved-char) 0)
                      (logic-substitute type environment)
                      nil environment operation emit environment))

(define-iso-builtin (code_type code (type :raw)) "CODE_TYPE"
  (when (logic-var-p resolved-code)
    (%raise-instantiation-error environment operation
                                "code_type/2 code must be instantiated"))
  (%char-type-holds-p (%code-character resolved-code environment operation)
                      (logic-substitute type environment)
                      t environment operation emit environment))

;;; upcase_atom/2, downcase_atom/2

(defun %case-fold-atom-goal (source target caser environment emit operation)
  (let ((text (%text-of (logic-substitute source environment)
                        environment operation
                        :accept :atomic
                        :instantiation "the source must be instantiated"
                        :type-message "the source must be atomic")))
    (%unify-emit target (%text-atom (funcall caser text)) environment emit)))

(define-builtin (upcase_atom source upper) (rulebase environment depth emit)
  (%case-fold-atom-goal source upper #'string-upcase environment emit
                        (%iso-atom "UPCASE_ATOM")))

(define-builtin (downcase_atom source lower) (rulebase environment depth emit)
  (%case-fold-atom-goal source lower #'string-downcase environment emit
                        (%iso-atom "DOWNCASE_ATOM")))
