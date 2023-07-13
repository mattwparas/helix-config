; (require-builtin helix/core/typable as helix.)
; (require-builtin helix/core/static as helix.static.)
; (require-builtin helix/core/keybindings as helix.keybindings.)

(require "prelude.scm"
         (for-syntax "prelude.scm"))

(require-helix)

(require "steel/sorting/merge-sort.scm")

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
         shell
         current-focus
         ; foobar
         git-add
         new-labelled-buffer
         open-labelled-buffer
         git-status-in-label
         run-in-repl
         create-file-tree
         open-file-from-picker
         ; fold-directory
         ;; wrapped-go-change-theme
         ;test-component
         )

(define (git-add cx)
  (shell cx "git" "add" "%"))

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

(define (test-component cx)
  (push-component!
   cx
   (new-component! "steel-dynamic-component" (list) (lambda (area frame context) void) (hash))))

(provide run-prompt)
; (define (run-prompt cx)
;   (push-component! cx (Prompt::new "Please enter your name:" (lambda (cx result) (error! result)))))

;; TODO: Recursive prompts... still broken for the above reason. Probably need some sort of
;; consuming callback queue

;;@doc
;; Testing out a prompt here
(define (run-prompt cx)
  (helix-prompt!
   cx
   "Please enter your name:"
   (lambda (cx result)
     (helix-prompt! cx
                    (string-append "You just entered: " result ". Is that right?: (y/n)")
                    (lambda (cx result) (create-file-tree cx))))))

(define (helix-prompt! cx prompt-str thunk)
  (push-component! cx (Prompt::new prompt-str thunk)))

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

;; Specialized shell - also be able to override the existing definition, if possible.
(define (shell cx . args)
  ;; Replace the % with the current file
  (define expanded (map (lambda (x) (if (equal? x "%") (current-path cx) x)) args))
  (helix.run-shell-command cx expanded helix.PromptEvent::Validate))

;;@doc
;; Get the path of the currently focused file
(define (current-focus cx)
  (insert-string-at-selection cx (to-string (current-path cx))))

;; Only get the doc if it exists - also use real options instead of false here cause it kinda sucks
(define (editor-get-doc-if-exists editor doc-id)
  (if (editor-doc-exists? editor doc-id) (editor->get-document editor doc-id) #f))

(define (current-path cx)
  (let* ([editor (cx-editor! cx)]
         [focus (editor-focus editor)]
         [focus-doc-id (editor->doc-id editor focus)]
         [document (editor-get-doc-if-exists editor focus-doc-id)])

    (if document (Document-path document) #f)))

;; Last focused - will allow us to swap between the last view we were at
(define *last-focus* 'uninitialized)

;; Mark the last focused document, so that we can return to it
(define (mark-last-focused! cx)
  (let* ([editor (cx-editor! cx)] [focus (editor-focus editor)])
    (set! *last-focus* focus)
    focus))

(define (currently-focused cx)
  (~> cx (cx-editor!) (editor-focus)))

;; (hash? string? )
(define *temporary-buffer-map* (hash))
(set! *reverse-buffer-map* (hash))

;; Grab whatever we're currently focused on
(define (get-current-focus cx)
  (~> cx (cx-editor!) (editor-focus)))

;; Get the current document id
(define (get-current-doc-id cx)
  (let* ([editor (cx-editor! cx)] [focus (editor-focus editor)]) (editor->doc-id editor focus)))

;;@doc
;; Creates a new labelled buffer that can be access by the key `label`.
;; Optionally sets the language type if provided
(define (make-new-labelled-buffer! cx
                                   #:label label
                                   #:language-type (language-type void)
                                   #:side (side 'right))

  ;; Save our last state to return to it afterwards
  (define last-focused (currently-focused cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))

  ;; Open up the new labelled buffer in a vertical split, set the language accordingly
  ;; if it has been passed in
  (helix.vsplit-new cx '() helix.PromptEvent::Validate)

  ; (when (eq? side 'left)
  ;   (helix.static.swap_view_left cx))

  (when language-type
    (helix.set-language cx (list language-type) helix.PromptEvent::Validate))

  ;; Add the document id to our internal mapping.
  (set! *temporary-buffer-map* (hash-insert *temporary-buffer-map* label (get-current-doc-id cx)))
  (set! *reverse-buffer-map*
        (hash-insert *reverse-buffer-map* (doc-id->usize (get-current-doc-id cx)) label))

  ;; Go back to where we were before
  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

;; Create a named repl, that we can reference statefully
(struct SteelRepl (doc-id))

(define *engine* (Engine::new))

(define (open-repl cx)
  (make-new-labelled-buffer! cx #:label "steel-repl" #:language-type "scheme"))

(define (run-in-repl cx)

  (define highlighted-expr (helix.static.current-highlighted-text! cx))

  (unless (hash-try-get *temporary-buffer-map* "steel-repl")
    (open-repl cx))

  (temporarily-switch-focus
   cx
   (lambda (cx)
     ;; Open up the repl, write the output
     (open-labelled-buffer cx "steel-repl")

     (helix.static.goto_file_end cx)
     (helix.static.insert_newline cx)
     (helix.static.insert_string cx (string-append "> " highlighted-expr))

     ;; This isn't _exactly_ what we want, but it could work just fine if we handle
     ;; displaying errors and such
     (helix.static.insert_string cx (to-string (run! *engine* highlighted-expr)))

     ; (helix.static.insert_newline cx)
     )))

;; Initialize all roots to be flat so that we don't blow things up, recursion only goes in to things
;; that are expanded
(define (create-file-tree cx)
  (unless (hash-contains? *temporary-buffer-map* "file-tree")
    (make-new-labelled-buffer! cx #:label "file-tree" #:side 'left))

  (temporarily-switch-focus cx
                            (lambda (cx)
                              (open-labelled-buffer cx "file-tree")
                              (helix.static.select_all cx)
                              (helix.static.delete_selection cx)

                              ;; Update the current file tree value
                              (set! *file-tree*
                                    (tree (helix-find-workspace)
                                          (lambda (str)
                                            (helix.static.insert_string cx str)
                                            (helix.static.open_below cx)
                                            (helix.static.goto_line_start cx)))))))

;; Switch the focus for the duration of the thunk, and return to where we were previously
(define (temporarily-switch-focus cx thunk)
  (define last-focused (mark-last-focused! cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))
  (thunk cx)
  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

;; Enqueue
(define (new-labelled-buffer cx label)
  (define last-focused (mark-last-focused! cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))
  (helix.vsplit-new cx '() helix.PromptEvent::Validate)
  (helix.set-language cx '("bash") helix.PromptEvent::Validate)

  (append-to-buffer cx)

  ;; Add the focus to our map
  (set! *temporary-buffer-map* (hash-insert *temporary-buffer-map* label (get-current-doc-id cx)))

  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

(define (open-or-switch-focus cx document-id)
  (define maybe-view-id? (editor-doc-in-view? (cx-editor! cx) document-id))
  (if maybe-view-id?
      (editor-set-focus! (cx-editor! cx) maybe-view-id?)
      (editor-switch! (cx-editor! cx) document-id)))

(define (git-status-in-label cx label)
  (define last-focused (mark-last-focused! cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))

  (open-labelled-buffer cx label)

  (append-to-buffer cx)

  ;; TODO: Don't just blindly set the focus, we should check if the doc is in view
  ;; then switch to that doc otherwise. Check out `Editor::switch` to see
  ;; if that will work for our use case here
  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

(define (open-labelled-buffer cx label)
  (open-or-switch-focus cx (hash-ref *temporary-buffer-map* label)))

(define (append-to-buffer cx)
  (helix.static.goto_file_end cx)
  (helix.static.insert_newline cx)
  (helix.static.insert_string cx "> git status")
  (helix.static.insert_newline cx)
  (helix.insert-output cx '("git" "status") helix.PromptEvent::Validate))

;;@doc
;; Run the current test in a shell?
(define (run-rust-test-under-cursor cx)
  (error! "Unimplemented!"))

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

;;@doc
;; Run the s expression
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
             ("t" => run-prompt)
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

(define *file-tree* '())
(define *directories* (hash))
(define *ignore-set* (hashset "target" ".git"))

(define (flatten x)
  (cond
    [(null? x) '()]
    [(not (list? x)) (list x)]
    [else (append (flatten (car x)) (flatten (cdr x)))]))

(define (format-dir path)
  (if (hash-contains? *directories* path)
      (if (hash-try-get *directories* path) ">  " "v  ")
      ">  " ;; First time we're visiting, mark as closed
      ))

(define *extension-map* (hash "rs" " " "scm" "󰘧 "))

(define (path->symbol path)
  (let ([extension (path->extension path)])
    (if extension
        (begin
          (define lookup (hash-try-get *extension-map* (path->extension path)))
          (if lookup lookup " "))

        " ")))

;; Simple tree implementation
;; Walks the file structure and prints without much fancy formatting
;; Returns a list of the visited files for convenience
(define (tree p writer-thunk)
  (define (tree-rec path padding)
    (define name (file-name path))

    (if (hashset-contains? *ignore-set* name)
        '()
        (begin
          (writer-thunk
           (string-append padding (if (is-dir? path) (format-dir path) (path->symbol path)) name))
          (cond
            [(is-file? path) path]
            [(is-dir? path)
             ;; If we're not supposed to see this path (i.e. its been folded),
             ;; then we're going to ignore it
             ;; Also - if it doesn't exist in the set, default it to folded
             (if (not (hash-contains? *directories* path))
                 (begin
                   (set! *directories* (hash-insert *directories* path #t))
                   (list path))
                 (if (hash-try-get *directories* path)
                     (list path)

                     (cons path
                           (map (fn (x) (tree-rec x (string-append padding "    ")))
                                (merge-sort (read-dir path))))))]
            [else void]))))
  (flatten (tree-rec p "")))

;;@doc
;; Open the currently selected line
(define (open-file-from-picker cx)
  (when (currently-in-labelled-buffer? cx "file-tree")
    (define file-to-open (list-ref *file-tree* (helix.static.get-current-line-number cx)))
    (helix.open cx (list file-to-open) helix.PromptEvent::Validate)))

(define (update-file-tree cx)

  (define current-selection (helix.static.current-selection-object cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))

  (helix.static.select_all cx)
  (helix.static.delete_selection cx)

  ;; Update the current file tree value
  (set! *file-tree*
        (tree (helix-find-workspace)
              (lambda (str)
                (helix.static.insert_string cx str)
                (helix.static.open_below cx)
                (helix.static.goto_line_start cx))))

  ;; Set it BACK to where we were previously!
  (helix.static.set-current-selection-object! cx current-selection)

  (editor-set-mode! (cx-editor! cx) last-mode))

(define (currently-in-labelled-buffer? cx label)
  (define requested-label (hash-try-get *temporary-buffer-map* label))
  (if requested-label
      (equal? (doc-id->usize requested-label) (doc-id->usize (get-current-doc-id cx)))
      #false))

(define (fold! directory)
  (set! *directories* (hash-insert *directories* directory #t)))

(define (unfold! directory)
  (set! *directories* (hash-insert *directories* directory #f)))

(provide fold-directory)

;; Fold the directory that we're currently hovering over
;; TODO: Assert that we're in a valid file picker buffer
(define (fold-directory cx)
  (when (currently-in-labelled-buffer? cx "file-tree")
    (define directory-to-fold (list-ref *file-tree* (helix.static.get-current-line-number cx)))
    (when (is-dir? directory-to-fold)
      (begin
        ;; If its already folded, unfold it
        (if (hash-try-get *directories* directory-to-fold)
            (unfold! directory-to-fold)
            (fold! directory-to-fold))

        (update-file-tree cx)))))

;; Create a file under wherever we are
(provide create-file)
(define (create-file cx)
  (when (currently-in-labelled-buffer? cx "file-tree")
    (define currently-selected (list-ref *file-tree* (helix.static.get-current-line-number cx)))
    (define prompt
      (if (is-dir? currently-selected)
          (string-append "New file: " currently-selected "/")
          (string-append "New file: "
                         (trim-end-matches currently-selected (file-name currently-selected)))))

    (helix-prompt!
     cx
     prompt
     (lambda (cx result)
       (define file-name (string-append (trim-start-matches prompt "New file: ") result))
       (temporarily-switch-focus cx
                                 (lambda (cx)
                                   (helix.vsplit-new cx '() helix.PromptEvent::Validate)
                                   (helix.open cx (list file-name) helix.PromptEvent::Validate)
                                   (helix.write cx (list file-name) helix.PromptEvent::Validate)
                                   (helix.quit cx '() helix.PromptEvent::Validate)))

       ;; TODO:
       ;; This is happening before the write is finished, so its not working. We will have to manually insert
       ;; the new file into the right spot in the tree, which would require rewriting this to have a proper sorted
       ;; tree representation in memory, which we don't yet have. For now, we can just do this I guess
       (enqueue-thread-local-callback cx refresh-file-tree)))))

(provide refresh-file-tree)
(define (refresh-file-tree cx)
  (temporarily-switch-focus cx
                            (lambda (cx)
                              (open-labelled-buffer cx "file-tree")
                              (update-file-tree cx))))

(provide create-directory)
(define (create-directory cx)
  (when (currently-in-labelled-buffer? cx "file-tree")
    (define currently-selected (list-ref *file-tree* (helix.static.get-current-line-number cx)))
    (define prompt
      (if (is-dir? currently-selected)
          (string-append "New directory: " currently-selected "/")
          (string-append "New directory: "
                         (trim-end-matches currently-selected (file-name currently-selected)))))

    (helix-prompt!
     cx
     prompt
     (lambda (cx result)
       (define directory-name (string-append (trim-start-matches prompt "New directory: ") result))
       (hx.create-directory directory-name)
       (enqueue-thread-local-callback cx refresh-file-tree)))))

(provide fold-all)
(define (fold-all cx)
  (when (currently-in-labelled-buffer? cx "file-tree")

    (set! *directories*
          (transduce *directories* (mapping (lambda (x) (list (list-ref x 0) #t))) (into-hashmap)))

    (helix.static.goto_file_start cx)

    (refresh-file-tree cx)))

(provide unfold-all-one-level)
;;@doc
;; Unfold all of the currently open directories one level.
(define (unfold-all-one-level cx)
  (when (currently-in-labelled-buffer? cx "file-tree")

    (set! *directories*
          (transduce *directories* (mapping (lambda (x) (list (list-ref x 0) #f))) (into-hashmap)))

    (refresh-file-tree cx)))

(provide create-commented-code-block)
(define (create-commented-code-block cx)
  (helix.static.insert_string cx "/// ```scheme\n/// \n/// ```")
  (helix.static.move_line_up cx)
  (helix.static.insert_mode cx))

;; Something about this infinite loops...
; (provide unfold-all)
; (define (unfold-all cx)

;   (define (loop cx)
;     (define current-directories *directories*)
;     (unfold-all-one-level cx)
;     (if (equal? current-directories *directories*) 'finished (loop cx)))

;   (loop cx))

;; Initialize custom keybindings... For certain buffers.
; (set! *buffer-or-extension-keybindings* (hash))

; (define scm-keybindings (hash "normal" (hash "P" (hash "n" 'run-prompt))))

; (define standard-keybindings (helix-default-keymap))

; (helix-merge-keybindings standard-keybindings
;                          (~> scm-keybindings (value->jsexpr-string) (helix-string->keymap)))

; ;;
; (set! *buffer-or-extension-keybindings* (hash "scm" standard-keybindings))
