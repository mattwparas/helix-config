;; (require-builtin helix/core/typable as helix.)
;; (require-builtin helix/core/static as helix.static.)
(require-builtin helix/core/keybindings as helix.keybindings.)

(provide (for-syntax require-helix)
         make-minor-mode!)

;; Provide the macro to help with setting up new files
(define-syntax require-helix
  (syntax-rules ()
    [(require-helix)
     (begin
       (require-builtin helix/core/typable as helix.)
       (require-builtin helix/core/static as helix.static.)
       (require-builtin helix/core/keybindings as helix.keybindings.))]))

;;@doc
;; Registers a minor mode with the registered modifer and key map
;;
;; Examples:
;; ```scheme
;; (make-minor-mode! "normal" "+"
;;    (hash "P" ":lam"))
;; ```
(define (make-minor-mode! mode modifier bindings)
  (~> (hash "normal" (hash mode (hash modifier bindings)))
      (value->jsexpr-string)
      (error "DEPRECATE ME")))
; (helix.keybindings.set-keybindings!)))
