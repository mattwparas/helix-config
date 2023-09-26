(require-builtin helix/core/keymaps as helix.keymaps.)

(require "steel/contracts/contract.scm"
         (for-syntax "steel/contracts/contract.scm")
         "steel/contracts/types.scm")

(provide *keybinding-map-set!*
         *keybinding-map-insert*
         *get-buffer-or-extension-keybindings*
         *reverse-buffer-map-insert*
         merge-keybindings
         helix-current-keymap
         set-global-buffer-or-extension-keymap
         add-global-keybinding)

(define helix-current-keymap helix.keymaps.helix-current-keymap)

;;@doc
;; Set the keybinding map to the specified map
(define (*keybinding-map-set!* map)
  (set-box! helix.keymaps.*buffer-or-extension-keybindings* map))

;;@doc
;; Insert a key value pair representing a binding to the keybinding map
(define (*keybinding-map-insert* key value)
  (set-box! helix.keymaps.*buffer-or-extension-keybindings*
            (hash-insert (unbox helix.keymaps.*buffer-or-extension-keybindings*) key value)))

;;@doc
;; Returns the buffer or extension keybinding map
(define (*get-buffer-or-extension-keybindings*)
  (unbox helix.keymaps.*buffer-or-extension-keybindings*))

;;@doc
;; Insert a value into the reverse buffer map
(define (*reverse-buffer-map-insert* key value)
  (set-box! helix.keymaps.*reverse-buffer-map*
            (hash-insert (unbox helix.keymaps.*reverse-buffer-map*) key value)))

;; Marshall values in and out of keybindings, referencing the associated values
;; within steel
(define/c (merge-keybindings keymap steel-key-map)
  (->c helix.keymaps.keymap? hash? void?)
  (helix.keymaps.helix-merge-keybindings
   keymap
   (~> steel-key-map (value->jsexpr-string) (helix.keymaps.helix-string->keymap))))

;;@doc
;; Check that the types on this map check out, otherwise we don't need to consistently do these checks
(define/c (set-global-buffer-or-extension-keymap map)
  (->c (hashof string? helix.keymaps.keymap?) void?)
  (*keybinding-map-set!* map))

;;@doc
;; Add keybinding to the global default
(define (add-global-keybinding map)
  (helix.keymaps.helix-merge-keybindings
   helix.keymaps.*global-keybinding-map*
   (~> map (value->jsexpr-string) (helix.keymaps.helix-string->keymap))))
