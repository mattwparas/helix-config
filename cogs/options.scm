(require "steel/contracts/contract.scm"
         (for-syntax "steel/contracts/contract.scm")
         "steel/contracts/types.scm")

(require "steel/result")

(provide apply-options)

;;@doc
;; Applies the options in the `options-assoc-list` to the
;; overall editor. Requires that the options list is given as a list of
;; pairs of symbols.
;;
;; Raises an error if the operation failed
(define/c (apply-options cx options-assoc-list)
  (->c hx.context? (listof (listof symbol?)) void?)
  (unwrap-ok (try-apply-options-impl cx options-assoc-list)))

;;@doc
;; Tries to apply the options in the `options-assoc-list` to the
;; overall editor. Requires that the options list is given as a list of
;; pairs of symbols.
;;
;; Returns a result specifying whether the application succeeded
(define/c (try-apply-options cx options-assoc-list)
  (->c hx.context? (listof (listof symbol?)) void?)
  (try-apply-options-impl cx options-assoc-list))

(define (try-apply-options-impl cx options-assoc-list)
  (helix.set-options cx
                     (~>> options-assoc-list (flatten) (map symbol->string))
                     helix.PromptEvent::Validate))
