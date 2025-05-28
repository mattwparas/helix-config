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
(keymap (global)
        (normal (C-r (f ":recentf-open-files")) (space (l ":load-buffer") (o ":eval-sexpr"))))

(define scm-keybindings (hash "insert" (hash "ret" ':scheme-indent "C-l" ':insert-lambda)))

;; Grab whatever the existing keybinding map is
(define standard-keybindings (deep-copy-global-keybindings))

(define file-tree-base (deep-copy-global-keybindings))

(merge-keybindings standard-keybindings scm-keybindings)
(merge-keybindings file-tree-base FILE-TREE-KEYBINDINGS)

;; <scratch> + <doc id> is probably the best way to handle this?
(set-global-buffer-or-extension-keymap (hash "scm" standard-keybindings FILE-TREE file-tree-base))

;; Create minor mode
; (define-syntax keybindings
;   (syntax-rules ()
;     [(_ modifier (key function))
;      (add-global-keybinding (hash modifier (minor-mode-cruncher (key function))))]

;     [(_ modifier (key (function ...)))
;      (add-global-keybinding (hash modifier (minor-mode-cruncher (key (function ...)))))]

;     [(_ modifier (key function) remaining ...)
;      (add-global-keybinding (hash modifier (minor-mode-cruncher (key function) remaining ...)))]

;     [(_ modifier (key (function ...)) remaining ...)
;      (add-global-keybinding (hash modifier (minor-mode-cruncher (key function) ... remaining ...)))]))

; (define-syntax minor-mode-cruncher
;   (syntax-rules ()
;     [(minor-mode-cruncher (key (function ...)))
;      (hash key
;            ; (map (lambda (x) (string-append ":" (symbol->string x))) (quote (function ...)))
;            (minor-mode-cruncher (function ...)))]

;     [(minor-mode-cruncher (key function))
;      (hash key (string-append ":" (symbol->string (quote function))))]

;     ;; TODO: Fix expansion here
;     [(minor-mode-cruncher (key (function ...)) remaining ...)
;      (hash-insert (minor-mode-cruncher remaining ...) key (minor-mode-cruncher (function ...)))]

;     [(minor-mode-cruncher (key function) remaining ...)
;      (hash-insert (minor-mode-cruncher remaining ...)
;                   key
;                   (string-append ":" (symbol->string (quote function))))]))

;; New keybindings API
; (keybindings "normal" ("C-r" ("f" recentf-open-files)) ("space" ("l" load-buffer) ("o" eval-sexpr)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(file-picker (fp-hidden #f))
(cursorline #t)
(soft-wrap (sw-enable #t))

(randomly-pick-theme possible-themes)

;; New LSP definitions
(define-lsp "steel-language-server" (command "steel-language-server") (args '()))
(define-lsp "rust-analyzer" (config (experimental (hash 'testExplorer #t))))

;; New language definition
(language "scheme"
          (formatter (command "raco") (args '("fmt" "-i")))
          (auto-format #true)
          (language-servers '("steel-language-server")))

(when (equal? (command-line) '("hx"))
  (show-splash))

; (open-term)

;; (show-welcome-message)

;; Probably should be a symbol?
; (register-hook! 'post-insert-char 'prompt-on-char-press)
