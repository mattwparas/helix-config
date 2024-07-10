;; (require-builtin helix/core/typable as helix.)
;; (require-builtin helix/core/static as helix.static.)
;; (require-builtin helix/core/editor)

(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/editor.scm")

(require "helix/misc.scm")
(require "cogs/keymaps.scm")
(require (only-in "cogs/scheme-indent.scm" scheme-indent))

(require "steel/sorting/merge-sort.scm")

(require "term.scm")

(provide open-term
         new-term
         kill-active-terminal
         switch-term
         term-resize
         xplr
         open-debug-window
         close-debug-window)

;; This gets run first...
; (require "/home/matt/Documents/helix-fork/helix/helix-term/src/commands/engine/controller.scm"
; (for-syntax
; "/home/matt/Documents/helix-fork/helix/helix-term/src/commands/engine/controller.scm"))

;;;;;;;;;;;;;;;; TESTING SANDBOXING ;;;;;;;;;;;;;;;

;; Load the plugin in a fresh environment
; (register-plugin! "/home/matt/.config/helix/cogs/sandbox.scm"
; "sandboxed-plugin"
; (list "my-custom-vsplit"))

; (provide my-custom-vsplit)
; (define/plugin-func my-custom-vsplit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define load-buffer helix.static.load-buffer!)

(provide insert-lambda
         insert-string-at-selection
         highlight-to-matching-paren
         delete-sexpr
         ; run-expr
         ; run-highlight
         make-minor-mode!
         git-status
         ; reload-helix-scm
         open-helix-scm
         open-init-scm
         new-function
         shell
         current-focus
         git-add
         load-buffer)

;;;;;;;;;;;;;;; File Tree ;;;;;;;;;;;;;;;;;

;; TODO: Add a way to provide-all-out from a module to make this easier
(require "cogs/file-tree.scm")

(provide fold-directory
         unfold-all-one-level
         open-file-from-picker
         create-file-tree
         create-file
         create-directory
         fold-all
         FILE-TREE
         FILE-TREE-KEYBINDINGS)

;;;;;;;;;;;;;; Git status picker ;;;;;;;;;;;;;;;

(require "cogs/git-status-picker.scm")

(provide create-gs-picker
         add-modified-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide scheme-indent)

(define (git-add)
  (shell "git" "add" "%"))

(require "cogs/recentf.scm")
(provide refresh-files
         flush-recent-files
         recentf-snapshot
         recentf-open-files)

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

(define rng (rand::thread-rng!))

;; Picking one from the possible themes is _fine_
; (define possible-themes '("tokyonight_storm" "catppuccin_macchiato" "kanagawa"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range rng 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme (list (select-random options))))

(provide change-theme-on-mode-change-hook)

(define (change-theme-on-mode-change-hook _event)
  (randomly-pick-theme (cx->themes)))

(provide move-window-left)
(define (move-window-left)
  (helix.static.move-window-far-left))

; (provide dummy)
; (define (dummy cx)
;   (helix.static.show-completion-prompt-with cx '("foo" "bar" "baz")))

; (provide prompt-on-char-press)

; (define (prompt-on-char-press cx char)
;   (when (equal? char #\f)
;     (dummy cx)))

; (helix.static.set-current-selection-object! cx current-selection))

; (provide search-in-directory)
; (define (search-in-directory cx)
;   (enqueue-thread-local-callback cx
;                                  (lambda (cx)
;                                    (helix.static.search-in-directory cx "~/.config/helix/cogs"))))

; (provide find-note)
; (define (find-note cx)
;   (helix.change-current-directory (list "~/.config/helix/cogs") helix.PromptEvent::Validate)
;   (helix.open cx (list "~/.config/helix/cogs") helix.PromptEvent::Validate))

; (provide search-in-directory)
; (define (search-in-directory cx dir)
;   (let ([cwd (helix.static.get-helix-cwd cx)])

;     (helix.change-current-directory cx (list dir) helix.PromptEvent::Validate)
;     ;; After changing the directory, attempt the search
;     (enqueue-thread-local-callback
;      cx
;      (lambda (cx)
;        (helix.static.global_search cx)
;        (enqueue-thread-local-callback
;         cx
;         (lambda (cx) (helix.change-current-directory cx (list cwd) helix.PromptEvent::Validate)))))))

; (define foo 'uninitialized)

; (provide logging)
; (define (logging cx)

; (create-file-tree *context*))

; (define (read-lines path)

;   (define port (open-input-file path))

;   (define (get-next-word! port)
;     (define line (read-line-from-port port))
;     (if (symbol? line) #f (trim line)))

;   (define (read-to-list lst)
;     (define next-word (get-next-word! port))
;     (if next-word (read-to-list (cons next-word lst)) lst))

;   (read-to-list '()))

; (provide get-git-ignore)
; (define (get-git-ignore cx)
;   (helix.insert-output cx (cons "echo" (read-lines ".gitignore")) helix.PromptEvent::Validate))

;; Experimenting with using a buffer as a repl? Continuously write results to it...
;; is there a way to do that? Or use a widget? I suppose a buffer could be treated
;; as a buffer if we feel so inclined...
; (define (foobar cx)
; (helix.vsplit-new cx '() helix.PromptEvent::Validate)
; (insert-string-at-selection cx "Hello world!")
; (helix.buffer-close! cx '() helix.PromptEvent::Validate))

;; Run without quoting?
; define (steel cx . exprs)
;   (helix.static.run-in-engine!))

;; Hmmmmm should just use my class system instead? that might help with the dispatching
; (define (Component::render-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-render obj)
;    Component::render))

; (define (Component::handle-event-func obj)
;  (if (SteelDynamicComponent? obj)
;    (SteelDynamicComponent-handle-event obj)
;    Component::handle-event))

; (define (Component::should-update-func obj)
;   (if (SteelDynamicComponent? obj)
;       (SteelDynamicComponent-should-update obj)
;       Component::should-update))

; (define (Component::cursor-func obj)
;   (if (SteelDynamicComponent? obj) (SteelDynamicComponent-cursor obj) Component::cursor))

; (define (Component::required-size-func obj)
;   (if (SteelDynamicComponent? obj)
;       (SteelDynamicComponent-required-size obj)
;       Component::required-size))

; (define (make-dynamic! component)
;   (new-component! "steel-dynamic-component"
;                   component
;                   (Component::render-func component)
;                   (hash "handle_event"
;                         (Component::handle-event-func component)
;                         "should_update"
;                         (Component::should-update-func component)
;                         "cursor"
;                         (Component::cursor-func component)
;                         "required_size"
;                         (Component::required-size-func component))))

(define (test-component)
  (push-component!
   (new-component! "steel-dynamic-component" (list) (lambda (area frame context) void) (hash))))

; (provide run-prompt)
; (define (run-prompt cx)
;   (push-component! cx (Prompt::new "Please enter your name:" (lambda (cx result) (error! result)))))

;; TODO: Recursive prompts... still broken for the above reason. Probably need some sort of
;; consuming callback queue

;;@doc
;; Testing out a prompt here
; (define (run-prompt cx)
;   (helix-prompt!
;    cx
;    "Please enter your name:"
;    (lambda (cx result)
;      (helix-prompt! cx
;                     (string-append "You just entered: " result ". Is that right?: (y/n)")
;                     (lambda (cx result) (create-file-tree cx))))))

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
;; Specialized shell - also be able to override the existing definition, if possible.
(define (shell . args)
  ;; Replace the % with the current file
  (define expanded (map (lambda (x) (if (equal? x "%") (current-path) x)) args))
  (apply helix.run-shell-command expanded))

;;@doc
;; Get the path of the currently focused file
(define (current-focus)
  (insert-string-at-selection (to-string (current-path))))

;; Only get the doc if it exists - also use real options instead of false here cause it kinda sucks
; (define (editor-get-doc-if-exists editor doc-id)
;   (if (editor-doc-exists? editor doc-id) (editor->get-document editor doc-id) #f))

(define (editor-get-doc-if-exists doc-id)
  (if (editor-doc-exists? doc-id) (editor->get-document doc-id) #f))

(define (current-path)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)]
         [document (editor-get-doc-if-exists focus-doc-id)])

    (if document (Document-path document) #f)))

;; Last focused - will allow us to swap between the last view we were at
(define *last-focus* 'uninitialized)

;; Mark the last focused document, so that we can return to it
(define (mark-last-focused!)
  (let* ([focus (editor-focus)])
    (set! *last-focus* focus)
    focus))

(define (currently-focused)
  (editor-focus))

;; (hash? string? )
; (define *temporary-buffer-map* (hash))
; (set! *reverse-buffer-map* (hash))

;; Grab whatever we're currently focused on
(define (get-current-focus)
  (~> (editor-focus)))

;; Get the current document id
(define (get-current-doc-id)
  (let* ([focus (editor-focus)]) (editor->doc-id focus)))

;; Create a named repl, that we can reference statefully
; (struct SteelRepl (doc-id))

; (define *engine* (Engine::new))

; (define (append-to-buffer cx)
;   (helix.static.goto_file_end cx)
;   (helix.static.insert_newline cx)
;   (helix.static.insert_string cx "> git status")
;   (helix.static.insert_newline cx)
;   (helix.insert-output cx '("git" "status") helix.PromptEvent::Validate))

; ;;@doc
; ;; Run the current test in a shell?
; (define (run-rust-test-under-cursor cx)
;   (error! "Unimplemented!"))

; ;;@doc
; ;; Call this dummy function!
; (define (dummy cx)
;   void)

; ;;@doc
; ;; Sets the theme to be the dracula theme
; (define (set-theme-dracula cx)
;   (helix.theme cx (list "dracula") helix.PromptEvent::Validate))

; ;;@doc
; ;; Sets the theme to be the theme passed in
; (define (set-theme-custom cx entered-theme)
;   (helix.theme cx (list entered-theme) helix.PromptEvent::Validate))

; ;;@doc
; ;; Switch theme to the entered theme, then split the current file into
; ;; a vsplit
; (define (theme-then-vsplit cx entered-theme)
;   (set-theme-custom cx entered-theme)
;   (helix.vsplit cx '() helix.PromptEvent::Validate))

; ;;@doc
; ;; Perform an undo
; (define (custom-undo cx)
;   (helix.static.undo cx))

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

; ;;@doc
; ;; Reload the helix.scm file
; (define (reload-helix-scm)
; (helix.static.run-in-engine! (string-append "(require \"" (helix.static.get-helix-scm-path) "\")")))

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

;;;; Embedded Terminal ;;;;;

(skip-compile
 ; (require-builtin steel/pty-process)
 (define *pty-process* 'uninitialized)
 ;; Start at every 20 ms. At some point, we are going to be idled, and we don't want to constantly
 ;; be running in a loop to refresh. At this point we can just delay
 (define *DEFAULT-REFRESH-DELAY* 20)
 (define *terminal-refresh-delay* *DEFAULT-REFRESH-DELAY*)
 (define fail-check 0)
 (define (reset-fail-check!)
   (set! fail-check 0))
 (define (mark-failed!)
   (set! fail-check (+ 1 fail-check)))
 ;; TODO: Cap this to some large enough value
 ;; This could definitely cause issues for long running stuff...
 (define (increase-terminal-refresh-delay!)
   (set! *terminal-refresh-delay* (* *terminal-refresh-delay* *terminal-refresh-delay*)))
 (define (reset-terminal-refresh-delay!)
   (set! *terminal-refresh-delay* *DEFAULT-REFRESH-DELAY*))
 (provide initialize-pty-process)
 (define (initialize-pty-process)
   (set! *pty-process* (create-native-pty-system!)))
 (provide kill-terminal)
 (define (kill-terminal)
   (kill-pty-process! *pty-process*))
 (define *go-signal* #t)
 (provide interrupt-terminal)
 (define (interrupt-terminal)
   (set! *go-signal* #f)
   (enqueue-thread-local-callback (lambda () (set! *go-signal* #true))))
 (provide terminal-loop)
 (define (terminal-loop)
   (when *go-signal*
     (begin

       (let ([line-future (async-try-read-line *pty-process*)])
         (helix-await-callback line-future
                               (lambda (line)
                                 (async-write-from-terminal-loop cx line)

                                 (terminal-loop cx))))))))

;; Goes until there isn't any output to read, writing each line
(skip-compile (define (read-until-no-more-lines)
                (error! "TODO"))
              ; (let ([output (pty-process-try-read-line *pty-process*)])
              ;   (when output
              ;     (helix.static.insert_string cx output)
              ;     (read-until-no-more-lines cx))))
              (define fail-check 0)
              (define (write-line-to-terminal line)
                (temporarily-switch-focus (lambda ()
                                            ;; Open up the repl, write the output
                                            (open-labelled-buffer "steel-repl")
                                            (helix.static.insert_string line))))
              (define (write-char-to-terminal char)
                (cond
                  [(equal? char #\newline) (helix.static.insert_string "\n")]
                  [(equal? char #\return)
                   void
                   ; (temporarily-switch-focus cx
                   ;                           (lambda (cx)
                   ;                             ;; Open up the repl, write the output
                   ;                             (open-labelled-buffer cx "steel-repl")

                   ;                             (helix.static.insert_newline cx)
                   ;                             (helix.static.delete_selection cx)))
                   ]
                  [else (helix.static.insert_char char)])))

;; TODO:
;; Create a highlighter stream of spans to apply syntax highlighting
;; to a document. It _probably_ is not performant in the slightest bit, but it could help.
;; See syntax.rs and highlight event + Styles. Might be possible to map ansi code -> style,
;; and then access enough of the document API to make it possible.

(skip-compile
 (define *ansi-parser* (make-ansi-tokenizer))
 ;; This is a bit silly, but we'll have to model cursor movement.
 (define *cursor-position* 1)
 (define (helix-clear-line)

   (helix.static.extend_to_line_bounds)
   (helix.static.delete_selection))
 (define escape-code-map
   ;; EraseToEndOfLine - helix.static.kill_to_line_end
   (list (lambda (cx)
           (when (equal? 1 *cursor-position*)
             (helix-clear-line)))
         ;; EraseToStartOfLine
         helix.static.kill_to_line_start
         ;; EraseLine
         (lambda ()
           (helix.static.extend_to_line_bounds)
           (helix.static.delete_selection))
         ;; Cursor Position escape sequence
         (lambda () (helix.static.insert_string "CURSOR POSITION ESCAPE SEQUENCE"))))
 (define (async-write-from-terminal-loop line)

   (unless (hash-try-get *temporary-buffer-map* "steel-repl")
     (open-repl))

   (temporarily-switch-focus
    (lambda ()
      ;; Open up the repl, write the output
      (open-labelled-buffer "steel-repl")

      (transduce (tokenize-line *ansi-parser* line)
                 (into-for-each (lambda (line)
                                  (cond
                                    ; (write-line-to-terminal cx (to-string "ESCAPE CODE:" line))
                                    [(int? line) ((list-ref escape-code-map line) cx)]
                                    [(char? line) (write-char-to-terminal cx line)]
                                    ; (write-line-to-terminal cx line)
                                    [line (helix.static.insert_string cx line)]
                                    [else void])))))))
 (define (write-from-terminal-loop)

   (unless (hash-try-get *temporary-buffer-map* "steel-repl")
     (open-repl))

   (temporarily-switch-focus (lambda ()
                               ;; Open up the repl, write the output
                               (let ([output (async-try-read-line *pty-process*)])
                                 (if output
                                     (begin
                                       (open-labelled-buffer "steel-repl")
                                       (helix.static.insert_string output)
                                       (read-until-no-more-lines))
                                     (mark-failed!))))))
 ;; Every time we send a command, we can just unpark the delay
 (provide send-ls)
 (define (send-ls)
   (reset-terminal-refresh-delay!)
   (reset-fail-check!)
   (pty-process-send-command *pty-process* "ls -l\r"))
 ; (require "steel/transducers/transducers.scm")
 (provide send-command)
 (define (send-command . args)

   (define carriage-return-ammended-string
     (list-transduce (tadd-between " ") rcons (append args '("\r"))))

   (pty-process-send-command *pty-process* (apply string-append carriage-return-ammended-string))))

(define-syntax skip-compile
  (syntax-rules ()
    [(skip-compile) (begin)]
    [(skip-compile expr) (begin)]
    [(skip-compile expr exprs ...)
     (begin
       (skip-compile exprs ...))]))
