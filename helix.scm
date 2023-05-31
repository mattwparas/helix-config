; (require-builtin helix/core/typable as helix.)
; (require-builtin helix/core/static as helix.static.)
; (require-builtin helix/core/keybindings as helix.keybindings.)

(require "prelude.scm"
         (for-syntax "prelude.scm"))

(require-helix)
; (require "steel/result" as result/)
; (require "steel/option")

;; (require-builtin external-command-module as ext.)

(provide set-theme-dracula
         set-theme-custom
         theme-then-vsplit
         custom-undo
         lam
         delete-word-forward
         insert-string-at-selection
         highlight-to-matching-paren
         delete-sexpr
         run-expr
         run-highlight
         make-minor-mode!
         git-status
         reload-helix-scm
         open-helix-scm
         open-init-scm
         new-function
         dummy
         ;; wrapped-go-change-theme
         ;test-component
         )

;; Run without quoting?
; (define (steel cx . exprs)
;   (helix.static.run-in-engine! )

;; Hmmmmm should just use my class system instead? that might help with the dispatching
;(define (Component::render-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-render obj)
;    Component::render))

;(define (Component::handle-event-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-handle-event obj)
;    Component::handle-event))

;(define (Component::should-update-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-should-update obj)
;    Component::should-update))

;(define (Component::cursor-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-cursor obj)
;    Component::cursor))

;(define (Component::required-size-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-required-size obj)
;    Component::required-size))

;(define (make-dynamic! component)
;  (new-component! "steel-dynamic-component"
;                  component
;                  (Component::render-func component)
;                  (hash
;                    "handle_event" (Component::handle-event-func component)
;                    "should_update" (Component::should-update-func component)
;                    "cursor" (Component::cursor-func component)
;                    "required_size" (Component::required-size-func component))))

; (define (wrapped-popup))

; (define (test-component cx)
;   (push-component! cx (new-component! "steel-dynamic-component"
;                                    (list)
;                                    (lambda (area frame context) void)
;                                    (hash))))

;; I think options might still come through as void?
(define (unwrap-or obj alt)
  (if (void? obj) alt obj))

;; TODO: Figure out a more ergonomic way of expression some of the position manipulation.
;; As it is, we have to do quite a bit to grab the right stuff
;(define (test-component cx)
;  (let ((component (make-dynamic! (Component::Text "hello world!"))))
;    (push-component!
;      cx
;        (Popup::new component
;                    (~> cx
;                        (cx->cursor)
;                        (car)
;                        (unwrap-or (Position::default)))))))

;;@doc
;; change the theme, but from a dylib!
; (define (wrapped-go-change-theme cx entered-theme)
; (ext.go-change-theme cx (list entered-theme) helix.PromptEvent::Validate))

;;@doc
;; Call this dummy function!
(define (dummy cx)
  void)

;;@doc
;; Sets the theme to be the dracula theme
(define (set-theme-dracula cx)
  (helix.theme cx (list "dracula") helix.PromptEvent::Validate))

;;@doc
;; Sets the theme to be the theme passed in
(define (set-theme-custom cx entered-theme)
  (helix.theme cx (list entered-theme) helix.PromptEvent::Validate))

;;@doc
;; Switch theme to the entered theme, then split the current file into
;; a vsplit
(define (theme-then-vsplit cx entered-theme)
  (set-theme-custom cx entered-theme)
  (helix.vsplit cx '() helix.PromptEvent::Validate))

;;@doc
;; Perform an undo
(define (custom-undo cx)
  (helix.static.undo cx))

;;@doc
;; Insert a lambda
(define (lam cx)
  (helix.static.insert_char cx #\λ)
  (helix.static.insert_mode cx))

;;@doc
;; Insert the string at the selection and go back into insert mode
(define (insert-string-at-selection cx str)
  (helix.static.insert_string cx str)
  (helix.static.insert_mode cx))

;;@doc
;; Delete the word forward
(define (delete-word-forward cx)
  (helix.static.delete_word_forward cx))

;;@doc
;; Registers a minor mode with the registered modifer and key map
;;
;; Examples:
;; ```scheme
;; (make-minor-mode! "+"
;;    (hash "P" ":lam"))
;; ```
(define (make-minor-mode! modifier bindings)
  (~> (hash "normal" (hash modifier bindings))
      (value->jsexpr-string)
      (helix.keybindings.set-keybindings!)))

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
(define (highlight-to-matching-paren cx)
  (helix.static.select_mode cx)
  (helix.static.match_brackets cx))

(define (run-expr cx)
  (define current-selection (helix.static.current_selection cx))
  (when (or (equal? "(" current-selection) (equal? ")" current-selection))
    (highlight-to-matching-paren cx)
    (helix.static.run-in-engine! cx (helix.static.current-highlighted-text! cx))
    (helix.static.normal_mode cx)))

(define (run-highlight cx)
  (helix.static.run-in-engine! cx (helix.static.current-highlighted-text! cx)))

;;@doc
;; Delete the s-expression matching this bracket
;; If the current selection is not on a bracket, this is a no-op
(define (delete-sexpr cx)
  (define current-selection (helix.static.current_selection cx))
  (when (or (equal? "(" current-selection) (equal? ")" current-selection))
    (highlight-to-matching-paren cx)
    (helix.static.delete_selection cx)))

; (minor-mode! "+" ("l" => lam)
;                  ("q" => (set-theme-dracula lam)))

(minor-mode! "P"
             ("l" => lam)
             ("p" => highlight-to-matching-paren)
             ("d" => delete-sexpr)
             ("r" => run-expr)
             ;; ("t" => test-component)
             )

(make-minor-mode! "+" (hash "l" ":lam"))

(define (git-status cx)
  (helix.run-shell-command cx '("git" "status") helix.PromptEvent::Validate))

(minor-mode! "G" ("s" => git-status))

;;@doc
;; Reload the helix.scm file
(define (reload-helix-scm cx)
  (helix.static.run-in-engine! cx
                               (string-append "(require \"" (helix.static.get-helix-scm-path) "\")")))

;;@doc
;; Open the helix.scm file
(define (open-helix-scm cx)
  (helix.open cx (list (helix.static.get-helix-scm-path)) helix.PromptEvent::Validate))

;;@doc
;; Opens the init.scm file
(define (open-init-scm cx)
  (helix.open cx (list (helix.static.get-init-scm-path)) helix.PromptEvent::Validate))

;;@doc run git status
(define (new-function cx)
  (git-status cx))

;;@doc
;; Collect memory usage of engine runtime?
(define (print-engine-stats)
  (error "TODO"))

;; TODO: Capture the output of standard out and pipe it to the
;; window here
;; (help open-helix-scm)

;; (error "uh oh!")
