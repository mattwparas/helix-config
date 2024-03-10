(require "helix/configuration.scm")
(require "helix/misc.scm")

(require-builtin helix/components)

(provide show-welcome-message)

(#%require-dylib "libsteel_pty"
                 (only-in create-native-pty-system!
                          kill-pty-process!
                          pty-process-send-command
                          async-try-read-line
                          virtual-terminal
                          vte/advance-bytes
                          vte/lines
                          vte/line->string
                          vte/cursor
                          vte/line->cells
                          vte/cell->fg
                          vte/cell->bg
                          term/color-attribute
                          vte/cell-width
                          vte/cell-string
                          vte/reset-iterator!
                          vte/advance-iterator!
                          vte/iter-x
                          vte/iter-y
                          vte/iter-cell-fg
                          vte/iter-cell-bg
                          vte/iter-cell-str
                          vte/cursor-x
                          vte/cursor-y
                          vte/resize
                          pty-resize!
                          raw-virtual-terminal))

(require "steel/result")
(require "helix/misc.scm")

(require-builtin steel/time)

(provide start-terminal
         kill-terminal
         terminal-loop
         send-ls
         term)

(define default-style (~> (style) (style-bg Color/Black) (style-fg Color/White)))

;; Save Color around rather than allocate a new one each time
(define (attribute->color attr base-color)
  (cond
    [(list? attr)
     (set-color-rgb! base-color (list-ref attr 0) (list-ref attr 1) (list-ref attr 2))

     base-color]
    [(int? attr)
     (set-color-indexed! base-color attr)

     ; (log::info! (to-string "Found indexed color " attr))

     base-color
     ; (Color/Indexed attr)
     ]
    [else #f]))

(define (cell-fg-bg->style base-style base-color-fg base-color-bg fg bg)

  (set-style-bg! base-style
                 (or (attribute->color (term/color-attribute bg) base-color-bg) Color/Black))
  (set-style-fg! base-style
                 (or (attribute->color (term/color-attribute fg) base-color-fg) Color/White)))

;; Spawn the background thread with the pty process
;; communicate over the process?

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define *pty-process* 'uninitialized)
(define *vte* 'uninitialized)

; (struct DebugWindow (should-show?) #:mutable)

; ; ;; New debug window, top left
; (define *debug-window* (DebugWindow #f))
; ;; Send debug output to this window

; (define *debug-vte* 'uninitialized)

; (define-values (debug-writer debug-async-reader) (make-async-reader-writer))

; ;; Set the output port to be this terminal sink
; (current-output-port debug-writer)

; (define (debug-loop)
;   (helix-await-callback (async-read-line debug-async-reader)
;                         (lambda (line)
;                           (when line
;                             ;; Handle output?
;                             (vte/advance-bytes *debug-vte* line)

;                             ;; Kick off the debug loop again
;                             (debug-loop)))))

; (define (debug-window)
;   (set! *debug-vte* (raw-virtual-terminal))
;   (vte/resize *debug-vte* 45 80))

; ; (define (debug-event-handler state event)
; ;   (define char (key-event-char event))
; ;   (cond
; ;     ;; Close with esc
; ;     [(key-event-escape? event) event-result/close]

; ;     [else event-result/ignore]))

; (define (debug-render state rect frame)
;   (when (DebugWindow-should-show? state)

;     ;; Max width
;     ;; TODO: This should be dynamic based on the viewport size
;     (define max-width 80)
;     (define left-shift (round (/ max-width 2)))

;     ;; Put this at 3/4 to the right side of the screen
;     (define x (- (round (* 3/4 (area-width rect))) left-shift))

;     ;; Halfway down
;     (define y (round (* 1/4 (area-height rect))))

;     (define block-area (area (max (- x 10) 0) (max (- y 5) 0) (+ max-width 5) (+ 45 5)))

;     (define x-offset (+ 2 (area-x block-area)))
;     (define y-offset (+ 2 (area-y block-area)))

;     ;; Clear out the target for the terminal
;     (buffer/clear frame block-area)
;     (block/render frame block-area (block))

;     ;; Start at 0
;     (vte/reset-iterator! *debug-vte*)

;     ;; Advancing the iterator
;     (while (vte/advance-iterator! *debug-vte*)
;            (define cell-x (vte/iter-x *debug-vte*))
;            (define cell-y (vte/iter-y *debug-vte*))
;            (define str (vte/iter-cell-str *debug-vte*))
;            ;; If there is something to render, lets do it
;            (when str
;              (frame-set-string! frame (+ x-offset cell-x) (+ y-offset cell-y) str default-style)))))

; (define (show-debug)
;   (set-DebugWindow-should-show?! *debug-window* #t)
;   (push-component! (new-component! "DebugTerminal" *debug-window* debug-render (hash))))

; (define (hide-debug)
;   (set-DebugWindow-should-show?! *debug-window* #f))

; (define *ansi-tokenizer* (make-ansi-tokenizer))
(define (start-terminal)
  (set! *pty-process* (create-native-pty-system! "/usr/bin/zsh"))
  (set! *vte* (virtual-terminal *pty-process*))

  (vte/resize *vte* 45 80)
  (pty-resize! *pty-process* 45 80))

(define (kill-terminal)
  (kill-pty-process! *pty-process*)
  (set! *vte* 'uninitialized)
  (set! *pty-process* 'uninitialized))

;; Appends the carriage return to the given command
(define (send-command command)
  (pty-process-send-command *pty-process* (string-append command "\r")))

(define (send-string str)
  (pty-process-send-command *pty-process* str))

(define (send-ls)
  (send-command "ls -l"))

(define (try-drop lst n)
  (if (> n (length lst)) lst (drop lst n)))

(struct WelcomeMessage (cursor character-buffer lines) #:mutable)

;; Embedded terminal widget
(struct Terminal (cursor viewport-width viewport-height focused?) #:mutable)
(define *terminal*
  (Terminal (position 0 0)
            80 ;; Number of cols to render
            45 ;; Number of rows to render
            #f ;; Default, not focused
            ))

; (define (pop-n vec count)
;   (unless (zero? count)
;     (mutable-vector-pop! vec)
;     (pop-n vec (- count 1))))

; (define (split-at lst pos)
;   (values (take lst pos) (drop lst pos)))

; (define (insert-at lst pos x)
;   (define-values (before after) (split-at lst pos))
;   (append before (cons x after)))

; (define default-style (~> (style) (style-bg Color/Black) (style-fg Color/White)))

(define style-cursor (style))
(define color-cursor-fg (Color/rgb 0 0 0))
(define color-cursor-bg (Color/rgb 0 0 0))

(define (terminal-render state rect frame)
  (define t (instant/now))

  ;; Max width
  ;; TODO: This should be dynamic based on the viewport size
  (define max-width 80)
  (define left-shift (round (/ max-width 2)))

  ;; Put this at 3/4 to the right side of the screen
  (define x (- (round (* 3/4 (area-width rect))) left-shift))

  ;; Halfway down
  (define y (round (* 1/4 (area-height rect))))

  (define block-area
    (area (max (- x 10) 0) (max (- y 5) 0) (+ max-width 5) (+ (Terminal-viewport-height state) 5)))

  (define x-offset (+ 2 (area-x block-area)))
  (define y-offset (+ 2 (area-y block-area)))

  ;; Clear out the target for the terminal
  (buffer/clear frame block-area)
  (block/render frame block-area (block))

  ;; Start at 0
  (vte/reset-iterator! *vte*)

  ;; Advancing the iterator
  (while (vte/advance-iterator! *vte*)
         (define cell-x (vte/iter-x *vte*))
         (define cell-y (vte/iter-y *vte*))
         (define fg-style (vte/iter-cell-fg *vte*))
         (define bg-style (vte/iter-cell-bg *vte*))
         (define str (vte/iter-cell-str *vte*))
         ;; If there is something to render, lets do it
         (when str
           ; (log::info! (to-string cell-x cell-y))
           (cell-fg-bg->style style-cursor color-cursor-fg color-cursor-bg fg-style bg-style)
           ; (log::info! (to-string style-cursor))
           (frame-set-string! frame (+ x-offset cell-x) (+ y-offset cell-y) str style-cursor)))

  ; (log::info! (to-string "Cursor pos: " (vte/cursor-x *vte*) (vte/cursor-y *vte*)))

  (set-Terminal-cursor! state
                        (position (+ y-offset (vte/cursor-y *vte*))
                                  (+ x-offset (vte/cursor-x *vte*) 1)))

  (log::info! (string-append "Total Rendering time: " (duration->string (instant/elapsed t)))))

;; Don't handle any events
(define (terminal-event-handler state event)
  (define char (key-event-char event))
  (if (Terminal-focused? state)

      (cond
        ;; Backspace
        [(key-event-backspace? event)

         ;; Sending backspace - Note: This will fail to parse
         ;; until steel is upgraded.
         (send-string "\x7f")

         event-result/consume]

        ;; Close with ctrl-esc
        [(key-event-escape? event)
         (if (equal? (key-event-modifier event) key-modifier-ctrl)
             event-result/close
             (begin
               (send-string "\x1b")
               event-result/consume))]
        [(key-event-enter? event)
         (send-string "\r")
         event-result/consume]

        [(key-event-tab? event)
         (send-string "\x09")
         event-result/consume]

        ;; TODO: Handle modifiers here
        [(key-event-up? event) (send-string "\x1b[A")]
        [(key-event-down? event) (send-string "\x1b[B")]
        [(key-event-right? event) (send-string "\x1b[C")]
        [(key-event-left? event) (send-string "\x1b[D")]

        [char
         (send-string (string char))
         event-result/consume]

        [else event-result/ignore])

      event-result/ignore))

(define (term)
  ;; Don't start the terminal if its already going
  (when (symbol? *pty-process*)
    (start-terminal)
    (terminal-loop)))

(define (show-term)
  (set-Terminal-focused?! *terminal* #t)
  (push-component! (new-component! "Terminal"
                                   *terminal*
                                   terminal-render
                                   (hash "handle_event"
                                         terminal-event-handler
                                         "cursor"
                                         (lambda (state area) (Terminal-cursor state))))))

;; Just spawn on another thread, communicate that way
(define (terminal-loop)
  (when (not (symbol? *pty-process*))

    (helix-await-callback (async-try-read-line *pty-process*)
                          (lambda (line)
                            (when line
                              ;; Handle output?
                              (vte/advance-bytes *vte* line)

                              ;; Kick off the terminal loop again
                              (terminal-loop))))))

(define welcome-message
  (list "    __  __     ___                        _____ __            __"
        "   / / / /__  / (_)  __   ____  ____     / ___// /____  ___  / /"
        "  / /_/ / _ \\/ / / |/_/  / __ \\/ __ \\    \\__ \\/ __/ _ \\/ _ \\/ / "
        " / __  /  __/ / />  <   / /_/ / / / /   ___/ / /_/  __/  __/ /  "
        "/_/ /_/\\___/_/_/_/|_|   \\____/_/ /_/   /____/\\__/\\___/\\___/_/   "
        "                                                                "
        "              Helix integrated with Steel v0.6.0                "))

(define (loop thunk x lst)
  (if (empty? lst)
      void
      (begin
        (thunk x (car lst))
        (loop thunk (+ x 1) (cdr lst)))))

(define (overlay component)
  (overlaid component)
  component)

;; Default style
(define welcome-message-style (~> (style) (style-bg Color/Black) (style-fg Color/White)))

(define even-line (~> (style) (style-bg Color/Black) (style-fg Color/Red)))
(define odd-line (~> (style) (style-bg Color/Black) (style-fg Color/Blue)))

;; We just want the last n lines of the stuff to render
(define (welcome-message-render state rect frame)

  (define max-width (foldl max 0 (map string-length welcome-message)))
  (define left-shift (round (/ max-width 2)))

  ;; Panics if it goes out of bounds.
  ;; We have to add some bounds checks on the block area stuff I think
  ;; TL;DR - use this functionality to move windows around
  ;; Pin to various locations - something like pinning the window to the right side
  ;; or having it pop up while being able to move the editor around would be awesome.

  ;; Put this at 3/4 to the right side of the screen
  (define x (- (round (* 3/4 (area-width rect))) left-shift))

  ;; Halfway down
  (define y (round (* 1/4 (area-height rect))))

  (define block-area (area (max (- x 10) 0) (max (- y 5) 0) (+ max-width 20) 20))

  (define character-buffer (WelcomeMessage-character-buffer state))
  (define lines (WelcomeMessage-lines state))
  ;; How many lines we want to show up in the output
  (define viewport-height 25)
  (define lines-to-render
    (reverse (map (lambda (x) (list->string (reverse x)))
                  (take (cons character-buffer lines) (+ viewport-height 1)))))

  (buffer/clear frame block-area)
  (block/render frame block-area (block))

  ; (widget/list/render
  ;  frame
  ;  block-area
  ;  (widget/list
  ;   '("foo" "bar" "baz" "bananas" "applesauce" "blagh" "slkfjowiejfsd" "isjdfoiwjeflksdjf")))

  (loop (lambda (row message)
          (frame-set-string! frame x row message (if (even? row) even-line odd-line)))
        y
        (append welcome-message lines-to-render))

  (set-WelcomeMessage-cursor!
   state
   ;; Just need to keep this around
   (position (+ y (length welcome-message) (min viewport-height (length lines)))
             (+ x (length character-buffer)))))

(define (welcome-message-event-handler state event)
  (define char (key-event-char event))
  (define character-buffer (WelcomeMessage-character-buffer state))
  (define lines (WelcomeMessage-lines state))

  (cond
    [(key-event-backspace? event)

     ;; If the buffer is empty, attempt to
     ;; roll back to the previous line.
     (if (empty? character-buffer)

         ;; If we have a line to roll back to, do it
         (when (not (empty? lines))
           (set-WelcomeMessage-character-buffer! state (car lines))
           (set-WelcomeMessage-lines! state (cdr lines)))

         (set-WelcomeMessage-character-buffer! state (cdr character-buffer)))

     event-result/consume]
    [(key-event-escape? event) event-result/close]

    ;; Move the window up and to the left
    [(and (equal? (key-event-modifier event) key-modifier-ctrl) (equal? char #\j))

     ; ;; Just move around from starting at the
     ; ;; center to somewhere else?
     ; (set! x-anchor (- x-anchor 5))
     ; (set! y-anchor (- y-anchor 5))

     event-result/consume]

    [(key-event-enter? event)

     (set-WelcomeMessage-lines! state (cons character-buffer lines))
     (set-WelcomeMessage-character-buffer! state '())

     event-result/consume]

    [char
     (set-WelcomeMessage-character-buffer! state (cons char character-buffer))

     event-result/consume]

    [else event-result/consume]))

;;@doc
;; Render a nice helpful welcome message
(define (show-welcome-message)
  ;; Render - how will that work?
  (push-component! (new-component! "DynamicComponent"
                                   (WelcomeMessage (position 0 0) '() '())
                                   ; global-component
                                   ;; Render
                                   welcome-message-render
                                   ; (hash)
                                   (hash "handle_event"
                                         welcome-message-event-handler
                                         ;; Handle event
                                         "should_update"
                                         ;; Should update
                                         (lambda (_) #t)
                                         "cursor"
                                         ;; Cursor
                                         (lambda (state area) (WelcomeMessage-cursor state))
                                         ;       "required_size"
                                         ;       ;; Required size
                                         ;       (lambda (state viewport) void))
                                         ; )
                                         ))))
