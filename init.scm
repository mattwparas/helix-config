(require-builtin steel/random as rand::)
(require-builtin helix/core/static as helix.static.)
(require-builtin helix/core/typable as helix.)

(require "cogs/keymaps.scm")
(require (only-in "cogs/options.scm" apply-options))

(require (only-in "cogs/file-tree.scm" FILE-TREE-KEYBINDINGS FILE-TREE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rng (rand::thread-rng!))

;; Picking one from the possible themes is _fine_
(define possible-themes '("tokyonight_storm" "catppuccin_macchiato" "kanagawa"))

; (define possible-themes '("kanagawa"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range rng 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme *helix.cx* (list (select-random options)) helix.PromptEvent::Validate))

(randomly-pick-theme possible-themes)

;;;;;;;;;;;;;;;;;;;;;;;; Default modes ;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the recentf snapshot, will watch every 2 minutes for active files,
;; and flush those down to disk
(recentf-snapshot *helix.cx*)

;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;

;; To remove a binding, set it to 'no_op
;; For example, this will make it impossible to enter insert mode:
;; (hash "normal" (hash "i" 'no_op))

(define scm-keybindings (hash "insert" (hash "ret" ':scheme-indent "C-l" ':insert-lambda)))

;; Grab whatever the existing keybinding map is
(define standard-keybindings (helix-current-keymap))

(define file-tree-base (helix-current-keymap))

(merge-keybindings standard-keybindings scm-keybindings)
(merge-keybindings file-tree-base FILE-TREE-KEYBINDINGS)

;; <scratch> + <doc id> is probably the best way to handle this?
(set-global-buffer-or-extension-keymap (hash "scm" standard-keybindings FILE-TREE file-tree-base))

;; Set the global keybinding for now
(add-global-keybinding
 (hash
  "normal"
  (hash
   "C-r"
   (hash
    "f"
    ":recentf-open-files") ;; "space" (hash "/" ":search-in-directory") ;; Uncomment if you'd like to make this keybinding
   )))

; (make-minor-mode! "C-r" (hash "f" ":recentf-open-files"))
; (make-minor-mode! "space" (hash "/" ":search-in-directory"))

;;;;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *config-map* '((file-picker.hidden false) (cursorline true) (soft-wrap.enable true)))

(apply-options *helix.cx* *config-map*)
