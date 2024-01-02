(require "steel/result")

(provide apply-options)
(require (prefix-in helix. "helix/commands.scm"))

;;@doc
;; Applies the options in the `options-assoc-list` to the
;; overall editor. Requires that the options list is given as a list of
;; pairs of symbols.
;;
;; Raises an error if the operation failed
(define (apply-options options-assoc-list)
  ; (->c hx.context? (listof (listof symbol?)) void?)
  (try-apply-options-impl options-assoc-list))

;;@doc
;; Tries to apply the options in the `options-assoc-list` to the
;; overall editor. Requires that the options list is given as a list of
;; pairs of symbols.
;;
;; Returns a result specifying whether the application succeeded
(define (try-apply-options options-assoc-list)
  (try-apply-options-impl options-assoc-list))

(define (foo)
  (helix.vsplit-new))

(define (try-apply-options-impl options-assoc-list)
  (apply helix.set-options (~>> options-assoc-list (flatten) (map symbol->string))))
