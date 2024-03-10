(require-builtin steel/random as rand::)

(require "cogs/keymaps.scm")
(require (only-in "cogs/file-tree.scm" FILE-TREE-KEYBINDINGS FILE-TREE))
(require (only-in "cogs/recentf.scm" recentf-open-files get-recent-files recentf-snapshot))

(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/configuration.scm")

;; (require (only-in "test.scm" show-welcome-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rng (rand::thread-rng!))

;; Picking one from the possible themes is _fine_
(define possible-themes '("tokyonight_storm" "catppuccin_macchiato" "solarized_dark"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range rng 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme (select-random options)))

(randomly-pick-theme possible-themes)

;;;;;;;;;;;;;;;;;;;;;;;; Default modes ;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the recentf snapshot, will watch every 2 minutes for active files,
;; and flush those down to disk
(recentf-snapshot)

;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;

;; To remove a binding, set it to 'no_op
;; For example, this will make it impossible to enter insert mode:
;; (hash "normal" (hash "i" 'no_op))

;; Set the global keybinding for now
(add-global-keybinding
 (hash
  ; "insert"
  ; (hash "C-r" (hash "f" ":dummy"))
  "normal"
  (hash
   "C-r"
   (hash "f" ":recentf-open-files")
   ; "c"
   ; ":dummy") ;; "space" (hash "/" ":search-in-directory") ;; Uncomment if you'd like to make this keybinding
   )))

(define scm-keybindings (hash "insert" (hash "ret" ':scheme-indent "C-l" ':insert-lambda)))

;; Grab whatever the existing keybinding map is
(define standard-keybindings (deep-copy-global-keybindings))

(define file-tree-base (deep-copy-global-keybindings))

(merge-keybindings standard-keybindings scm-keybindings)
(merge-keybindings file-tree-base FILE-TREE-KEYBINDINGS)

;; <scratch> + <doc id> is probably the best way to handle this?
(set-global-buffer-or-extension-keymap (hash "scm" standard-keybindings FILE-TREE file-tree-base))

;;;;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(file-picker (fp-hidden #f))
(cursorline #t)
(soft-wrap (sw-enable #t))

(randomly-pick-theme possible-themes)

;; (show-welcome-message)

;; Probably should be a symbol?
; (register-hook! 'post-insert-char 'prompt-on-char-press)
