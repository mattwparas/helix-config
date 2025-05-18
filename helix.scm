(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/editor.scm")

(require "helix/misc.scm")
(require "cogs/keymaps.scm")

(require "cogs/package.scm")

; (require "cogs/themes/spacemacs.scm")

;; Register helix packages - these will be dynamically
;; required in the global environment when this file is required.
;;
;; Note: A package that fails to load currently will silently fail
;; when using this mechanism. We log it, but otherwise Helix will continue
;; on business as usual. Keep that in mind if blindly copying this
;; configuration.
(load-package "term.scm")
(load-package "cogs/file-tree.scm")
(load-package "cogs/recentf.scm")
(load-package "cogs/git-status-picker.scm")
(load-package "cogs/scheme-indent.scm")
(load-package "cogs/helix-ext.scm")

;; Additional themes
(load-package "cogs/themes/spacemacs.scm")

;; Make this accessible to the global context
(provide list-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define load-buffer helix.static.load-buffer!)

(provide insert-lambda
         insert-string-at-selection
         highlight-to-matching-paren
         delete-sexpr
         make-minor-mode!
         git-status
         open-helix-scm
         open-init-scm
         new-function
         current-focus
         git-add
         load-buffer
         expanded-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (git-add)
  (expanded-shell "git" "add" "%"))

(provide fmt-lambda)

(define (fmt-lambda)

  (define current-selection (helix.static.current-selection-object))

  (helix.static.select_all)
  (helix.static.regex-selection "lambda\n")
  (helix.static.replace-selection-with "λ\n")

  (helix.static.select_all)
  (helix.static.regex-selection "lambda ")
  (helix.static.replace-selection-with "λ ")

  (helix.static.merge_selections)

  (helix.static.move_visual_line_down)
  (helix.static.move_visual_line_up))

(require-builtin steel/random as rand::)

;; Picking one from the possible themes is _fine_
; (define possible-themes '("tokyonight_storm" "catppuccin_macchiato" "kanagawa"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme (list (select-random options))))

(provide change-theme-on-mode-change-hook)

(define (change-theme-on-mode-change-hook _event)
  (randomly-pick-theme (cx->themes)))

(provide move-window-left)
(define (move-window-left)
  (helix.static.move-window-far-left))

; (define (test-component)
;   (push-component!
;    (new-component! "steel-dynamic-component" (list) (lambda (area frame context) void) (hash))))

(define (helix-prompt! prompt-str thunk)
  (push-component! (prompt prompt-str thunk)))

;; TODO: Move this to its own component API - components are pretty compelling to have, but
;; require just a tad bit more integration than standard commands
(provide helix-picker!)
(define (helix-picker! . pick-list)
  (push-component! (picker pick-list)))

;; I think options might still come through as void?
(define (unwrap-or obj alt)
  (if (void? obj) alt obj))

;;@doc
;; Specialized shell - also be able to override the existing definition, if possible.
(define (expanded-shell . args)
  ;; Replace the % with the current file
  (define expanded
    (map (lambda (x)
           (if (equal? x "%")
               (current-path)
               x))
         args))
  (apply helix.run-shell-command expanded))

;;@doc
;; Get the path of the currently focused file
(define (current-focus)
  (insert-string-at-selection (to-string (current-path))))

;; Only get the doc if it exists - also use real options instead of false here cause it kinda sucks
; (define (editor-get-doc-if-exists editor doc-id)
;   (if (editor-doc-exists? editor doc-id) (editor->get-document editor doc-id) #f))

; (define (editor-get-doc-if-exists doc-id)
;   (if (editor-doc-exists? doc-id) (editor->get-document doc-id) #f))

(define (current-path)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (editor-document->path focus-doc-id)))
; [document (editor-get-doc-if-exists focus-doc-id)])

; (if document (Document-path document) #f)))

;; Last focused - will allow us to swap between the last view we were at
(define *last-focus* 'uninitialized)

;; Mark the last focused document, so that we can return to it
(define (mark-last-focused!)
  (let* ([focus (editor-focus)])
    (set! *last-focus* focus)
    focus))

(define (currently-focused)
  (editor-focus))

;; Grab whatever we're currently focused on
(define (get-current-focus)
  (~> (editor-focus)))

;; Get the current document id
(define (get-current-doc-id)
  (let* ([focus (editor-focus)]) (editor->doc-id focus)))

;;@doc
;; Insert a lambda
(define (insert-lambda)
  (helix.static.insert_char #\λ)
  (helix.static.insert_mode))

;;@doc
;; Insert the string at the selection and go back into insert mode
(define (insert-string-at-selection str)
  (helix.static.insert_string str)
  (helix.static.insert_mode))

;;@doc
;; Registers a minor mode with the registered modifer and key map
;;
;; Examples:
;; ```scheme
;; (make-minor-mode! "+"
;;    (hash "P" ":lam"))
;; ```
(define (make-minor-mode! modifier bindings)
  (~> (hash "normal" (hash modifier bindings)) (value->jsexpr-string) (error "DEPRECATE ME")))
; (helix.keybindings.set-keybindings!)))

(define-syntax minor-mode!
  (syntax-rules (=>)
    [(minor-mode! modifier (key => function))
     (make-minor-mode! modifier (minor-mode-cruncher (key => function)))]

    [(minor-mode! modifier (key => (function ...)))
     (make-minor-mode! modifier (minor-mode-cruncher (key => (function ...))))]

    [(minor-mode! modifier (key => function) remaining ...)
     (make-minor-mode! modifier (minor-mode-cruncher (key => function) remaining ...))]

    [(minor-mode! modifier (key => (function ...)) remaining ...)
     (make-minor-mode! modifier (minor-mode-cruncher (key => function) ... remaining ...))]))

(define-syntax minor-mode-cruncher
  (syntax-rules (=>)
    [(minor-mode-cruncher (key => (function ...)))
     (hash key (map (lambda (x) (string-append ":" (symbol->string x))) (quote (function ...))))]

    [(minor-mode-cruncher (key => function))
     (hash key (string-append ":" (symbol->string (quote function))))]

    [(minor-mode-cruncher (key => (function ...)) remaining ...)
     (hash-insert (minor-mode-cruncher remaining ...)
                  key
                  (map (lambda (x) (string-append ":" (symbol->string x))) (quote (function ...))))]

    [(minor-mode-cruncher (key => function) remaining ...)
     (hash-insert (minor-mode-cruncher remaining ...)
                  key
                  (string-append ":" (symbol->string (quote function))))]))

;;@doc
;; Highlight to the matching paren
(define (highlight-to-matching-paren)
  (helix.static.select_mode)
  (helix.static.match_brackets))

;;@doc
;; Delete the s-expression matching this bracket
;; If the current selection is not on a bracket, this is a no-op
(define (delete-sexpr)
  (define current-selection (helix.static.current_selection))
  (when (or (equal? "(" current-selection) (equal? ")" current-selection))
    (highlight-to-matching-paren)
    (helix.static.delete_selection)))

(provide eval-sexpr)

;;@doc
;; Evaluate the s-expression underneath the cursor
(define (eval-sexpr)
  (define current-selection-object (helix.static.current-selection-object))
  (define current-selection (helix.static.current_selection))
  (define last-mode (editor-mode))
  (helix.static.match_brackets)
  (helix.static.select_mode)
  (helix.static.match_brackets)
  (eval-string (helix.static.current-highlighted-text!))
  (editor-set-mode! last-mode)
  (helix.static.set-current-selection-object! current-selection-object))

; (minor-mode! "+" ("l" => lam)
;                  ("q" => (set-theme-dracula lam)))

;; (minor-mode! "P"
;; ("l" => lam)
;;           ("p" => highlight-to-matching-paren)
;;           ("d" => delete-sexpr)
;;           ("r" => run-expr)
; ("t" => run-prompt)
;; ("t" => test-component)
;;          )

; (make-minor-mode! "+" (hash "l" ":lam"))

(define (git-status)
  (helix.run-shell-command "git" "status"))

; (minor-mode! "G" ("s" => git-status))
; (minor-mode! "C-r" ("f" => recentf-open-files))

;;@doc
;; Open the helix.scm file
(define (open-helix-scm)
  (helix.open (helix.static.get-helix-scm-path)))

;;@doc
;; Opens the init.scm file
(define (open-init-scm)
  (helix.open (helix.static.get-init-scm-path)))

;;@doc run git status
(define (new-function)
  (git-status))

;;@doc
;; Collect memory usage of engine runtime?
(define (print-engine-stats)
  (error "TODO"))

(provide create-commented-code-block)
(define (create-commented-code-block)
  (helix.static.insert_string "/// ```scheme\n/// \n/// ```")
  (helix.static.move_line_up)
  (helix.static.insert_mode))
