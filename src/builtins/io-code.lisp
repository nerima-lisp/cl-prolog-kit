;;;; ISO character-code stream predicates.

(in-package #:cl-prolog-kit)

(defun %io-read-code (entry environment operation &key peek)
  (let ((character (%io-read-character entry environment operation :peek peek)))
    (if character (char-code character) -1)))

(defun %io-code-character (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         "character code must be an integer"))
    (or (and (<= 0 value)
             (< value char-code-limit)
             (code-char value))
        (%raise-domain-error
         "CHARACTER_CODE" value environment operation
         "integer does not designate a supported character"))))

(defun %io-write-code (entry term environment operation)
  (%io-write-text-unit entry term environment operation #'%io-code-character))

(%define-io-dual-builtin (get_code (code) :input "GET_CODE")
    (rulebase environment depth emit)
  (%unify-emit code (%io-read-code entry environment operation)
               environment emit))

(%define-io-dual-builtin (peek_code (code) :input "PEEK_CODE")
    (rulebase environment depth emit)
  (%unify-emit code (%io-read-code entry environment operation :peek t)
               environment emit))

(%define-io-dual-builtin (put_code (code) :output "PUT_CODE")
    (rulebase environment depth emit)
  (%io-write-code entry code environment operation)
  (funcall emit environment))
