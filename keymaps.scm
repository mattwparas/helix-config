(require-builtin helix/core/keymaps as helix.keymaps.)

(provide *keybinding-map-set!*
         *keybinding-map-insert*
         *get-buffer-or-extension-keybindings*
         *reverse-buffer-map-insert*)

;;@doc
;; Set the keybinding map to the specified map
(define (*keybinding-map-set!* map)
  (set-box! helix.keymaps.*buffer-or-extension-keybindings* map))

;;@doc
;; Insert a key value pair representing a binding to the keybinding map
(define (*keybinding-map-insert* key value)
  (set-box! helix.keymaps.*buffer-or-extension-keybindings*
            (unbox helix.keymaps.*buffer-or-extension-keybindings*)
            key
            value))

;;@doc
;; Returns the buffer or extension keybinding map
(define (*get-buffer-or-extension-keybindings*)
  (unbox helix.keymaps.*buffer-or-extension-keybindings*))

;;@doc
;; Insert a value into the reverse buffer map
(define (*reverse-buffer-map-insert* key value)
  (set-box! helix.keymaps.*reverse-buffer-map* (unbox helix.keymaps.*reverse-buffer-map*) key value))
