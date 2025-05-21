(require-builtin steel/random as rand::)

(require "cogs/keymaps.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "splash.scm")
(require "focus.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Picking one from the possible themes is _fine_
(define possible-themes '("focus_nova"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range 0 (length lst))]) (list-ref lst index)))

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
  "normal"
  (hash "C-r" (hash "f" ":recentf-open-files") "space" (hash "l" ":load-buffer" "o" ":eval-sexpr"))))

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

;; Macro for language config?
(update-language-config! "scheme"
                         (hash 'name
                               "scheme"
                               'formatter
                               (hash 'command "raco" 'args '("fmt" "-i"))
                               'auto-format
                               #t
                               'language-servers
                               '("steel-language-server")))

(set-lsp-config! "steel-language-server" (hash 'command "steel-language-server" 'args '()))
(set-lsp-config! "rust-analyzer" (hash 'config (hash 'experimental (hash 'testExplorer #t))))

(define-syntax language
  (syntax-rules (#%crunch #%name #%conf)
    [(_ #%crunch #%name name #%conf conf (key value))
     (update-language-config! name (hash-insert conf (quote key) value))]
    [(_ #%crunch #%name name #%conf conf (key value) remaining ...)
     ;  ;; Crunch the remaining stuff
     (language #%crunch #%name name #%conf (hash-insert conf (quote key) value) remaining ...)]
    [(_ name (key value)) (language #%crunch #%name name #%conf (hash) (key value))]
    [(_ name (key value) ...) (language #%crunch #%name name #%conf (hash) (key value) ...)]))

;; New language definition
(language "scheme"
          (name "scheme")
          (formatter (hash 'command "raco" 'args '("fmt" "-i")))
          (auto-format #true)
          (language-servers '("steel-language-server")))

(when (equal? (command-line) '("hx"))
  (show-splash))

; (open-term)

;; (show-welcome-message)

;; Probably should be a symbol?
; (register-hook! 'post-insert-char 'prompt-on-char-press)
