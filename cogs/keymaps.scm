(require-builtin helix/core/keymaps as helix.keymaps.)

(require "helix/configuration.scm")

(provide *keybinding-map-set!*
         *keybinding-map-insert*
         *get-buffer-or-extension-keybindings*
         *reverse-buffer-map-insert*
         merge-keybindings
         set-global-buffer-or-extension-keymap
         add-global-keybinding
         deep-copy-global-keybindings)

;;@doc
;; Set the keybinding map to the specified map
(define (*keybinding-map-set!* map)
  (set-strong-box! helix.keymaps.*buffer-or-extension-keybindings* map))

;;@doc
;; Insert a key value pair representing a binding to the keybinding map
(define (*keybinding-map-insert* key value)
  (set-strong-box!
   helix.keymaps.*buffer-or-extension-keybindings*
   (hash-insert (unbox-strong helix.keymaps.*buffer-or-extension-keybindings*) key value)))

;;@doc
;; Returns the buffer or extension keybinding map
(define (*get-buffer-or-extension-keybindings*)
  (unbox-strong helix.keymaps.*buffer-or-extension-keybindings*))

;;@doc
;; Insert a value into the reverse buffer map
(define (*reverse-buffer-map-insert* key value)
  (set-strong-box! helix.keymaps.*reverse-buffer-map*
                   (hash-insert (unbox-strong helix.keymaps.*reverse-buffer-map*) key value)))

;; Marshall values in and out of keybindings, referencing the associated values
;; within steel
(define (merge-keybindings keymap steel-key-map)
  (helix.keymaps.helix-merge-keybindings
   keymap
   (~> steel-key-map (value->jsexpr-string) (helix.keymaps.helix-string->keymap))))

;;@doc
;; Check that the types on this map check out, otherwise we don't need to consistently do these checks
(define (set-global-buffer-or-extension-keymap map)
  (*keybinding-map-set!* map))

;;@doc
;; Add keybinding to the global default
(define (add-global-keybinding map)

  ;; Copy the global ones
  (define global-bindings (get-keybindings))
  (helix.keymaps.helix-merge-keybindings
   global-bindings
   (~> map (value->jsexpr-string) (helix.keymaps.helix-string->keymap)))

  (keybindings global-bindings)

  ;; Merge keybindings
  ; (helix.keymaps.helix-merge-keybindings
  ;  helix.keymaps.*global-keybinding-map*
  ;  (~> map (value->jsexpr-string) (helix.keymaps.helix-string->keymap)))
  )

;;@doc
;; Deep copy the global keymap
(define (deep-copy-global-keybindings)

  ;; Copy the global keybindings directly
  ;; off of the configuration object
  (get-keybindings))
