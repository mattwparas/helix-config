(require "helix/configuration.scm")
(require "helix/misc.scm")

(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))

(require "helix/editor.scm")

;; Pull in all of the functions from the dylib.
;; See steel-pty for the definitions
(#%require-dylib "libsteel_pty"
                 (only-in create-native-pty-system!
                          kill-pty-process!
                          pty-process-send-command
                          pty-process-send-command-char
                          async-try-read-line
                          virtual-terminal
                          vte/advance-bytes
                          vte/advance-bytes-with-carriage-return
                          vte/lines
                          vte/line->string
                          vte/cursor
                          vte/line->cells
                          vte/cell->fg
                          vte/cell->bg
                          term/color-attribute
                          term/color-attribute-set!
                          vte/cell-width
                          vte/cell-string
                          vte/reset-iterator!
                          vte/advance-iterator!
                          vte/iter-x
                          vte/iter-y
                          vte/iter-cell-fg
                          vte/iter-cell-bg
                          vte/iter-cell-fg-set-attr!
                          vte/iter-cell-bg-set-attr!
                          vte/iter-cell-bg-fg-set-attr!
                          vte/empty-cell
                          vte/iter-cell-str
                          vte/iter-cell-str-set-str!
                          vte/cursor-x
                          vte/cursor-y
                          vte/resize
                          pty-resize!
                          raw-virtual-terminal
                          vte/scroll-up
                          vte/scroll-down))

(require "steel/result")
(require "helix/misc.scm")

(require-builtin steel/time)
(require-builtin steel/ffi)

(provide open-term
         new-term
         kill-active-terminal
         switch-term
         term-resize
         (contract/out set-default-terminal-cols! (->/c int? void?))
         (contract/out set-default-terminal-rows! (->/c int? void?))
         (contract/out set-default-shell! (->/c string? void?))
         xplr
         open-debug-window
         close-debug-window
         hide-terminal)

(define *default-terminal-rows* 45)

;; Use this for the width, rows is going to be the default
(define *default-terminal-cols* 85)

(define (set-default-terminal-rows! rows)
  (set! *default-terminal-rows* rows)
  void)

(define (set-default-terminal-cols! cols)
  (set! *default-terminal-cols* cols)
  void)

(define *default-shell* "/bin/zsh")

(define (set-default-shell! path-to-shell)
  (set! *default-shell* path-to-shell)
  void)

; (define default-style (~> (style) (style-bg Color/Black) (style-fg Color/White)))
; (define default-style (style))

(define bg-attr (ffi-vector #f #f #f #f))
(define fg-attr (ffi-vector #f #f #f #f))

;; Save Color around rather than allocate a new one each time
(define (attribute->color attr bg/fg base-color fg?)
  (cond
    [(int? attr)
     ; (log::info! "Getting here")
     ;; TODO: Figure out why this seems kinda busted?
     (set-color-indexed! base-color attr)
     base-color]

    ;; Updating succeeded, use the shared memory space
    [attr
     (log::info! "Found color")
     (set-color-rgb! base-color
                     (ffi-vector-ref bg/fg 0)
                     (ffi-vector-ref bg/fg 1)
                     (ffi-vector-ref bg/fg 2))
     base-color]

    ; base-color
    [else (if fg? (style->fg (theme->fg *helix.cx*)) base-color)]))

(define (cell-fg-bg->style base-style base-color-fg base-color-bg fg bg)
  (set-style-bg!
   base-style
   (or (attribute->color (term/color-attribute-set! bg bg-attr) bg-attr base-color-bg #f)
       Color/Black))

  (set-style-fg!
   base-style
   (or (attribute->color (term/color-attribute-set! fg fg-attr) fg-attr base-color-fg #t)
       Color/White)))

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

;; Embedded terminal widget. This contains all of the UI fields
;; necessary to render, as well as the actual virtual terminal emulator
;; that we interact with. The implementation separates the vte and the
;; pty into separate entities - the vte stores all of the state of the
;; terminal, and the pty is the actual process that we're communicating
;; with. We run an async loop to pull output from the pty and feed
;; that into the vte. When rendering the terminal, we iterate over the
;; cells of the vte, and translate that into the representation that
;; Helix understands. In general, this struct is used mutably, however
;; in order to more efficiently interact with certain fields, some are
;; manually boxed while others are left as immutable.
(struct Terminal
        (name cursor
              viewport-width
              viewport-height
              focused?
              active
              *pty-process*
              *vte*
              style-cursor
              color-cursor-fg
              color-cursor-bg
              kill-switch
              str-cell
              cell-fg
              cell-bg
              area
              dragged?
              ;; Functions for the actual
              ;; component API
              renderer
              event-handler
              cursor-handler
              x-term
              y-term))

;; Construct the terminal - for some use cases
;; we don't actually need to create a pty. The debug
;; window for capturing output from steel is an
;; example of this - but in theory anything that we'd
;; like to "print" to that wants to be reflected
;; as a terminal window could be handled that way.
(define (make-terminal name shell rows cols on-start-func callback-function)
  (define *pty-process* (create-native-pty-system! shell))
  (define *vte* (virtual-terminal *pty-process*))

  (vte/resize *vte* rows cols)
  (pty-resize! *pty-process* rows cols)

  (let ([terminal (Terminal name
                            (position 0 0)
                            (box cols) ;; Width
                            (box rows) ;; Height
                            (box #f) ;; Focused - Are we capturing user input
                            (box #f) ;; Active - Is the terminal on the screen
                            *pty-process*
                            *vte*
                            (style)
                            (Color/rgb 0 0 0)
                            (Color/rgb 0 0 0)
                            ;; More or less a one shot channel. This just says to kill the update
                            ;; loop that is running in the background.
                            (box #f)
                            (mutable-string)
                            (vte/empty-cell)
                            (vte/empty-cell)
                            ;; Don't have an area yet!
                            (box #f)
                            ;; Are we currently dragging the terminal?
                            (box #f)
                            terminal-render
                            terminal-event-handler
                            terminal-cursor-handler
                            (box #f) ;; x-term
                            (box #f) ;; y-term
                            )])

    ;; Call the on start function if relevant. In general, this
    ;; is going to be `default-on-start-function`, but for
    ;; other use cases (like launching something like xplr)
    ;; we might want to go straight in to xplr.
    (when on-start-func
      (on-start-func terminal))

    (terminal-loop terminal callback-function)

    terminal))

(define (default-on-start-function terminal)
  (pty-process-send-command (Terminal-*pty-process* terminal)
                            (string-append "cd " (helix-find-workspace) "\r clear\r")))

(define (terminal-loop term callback-function)
  ;; Kick off the terminal loop, so that we can run this
  ;; in the background to (hopefully) gracefully kill
  ;; running terminals
  (define (terminal-loop-inner)
    (define *pty-process* (Terminal-*pty-process* term))
    (define *vte* (Terminal-*vte* term))
    (define *kill-switch* (Terminal-kill-switch term))
    (if (unbox *kill-switch*)
        (pop-last-component! (Terminal-name term))
        (helix-await-callback (async-try-read-line *pty-process*)
                              (lambda (line)
                                (when line
                                  (callback-function *vte* line)
                                  ;; Kick off the terminal loop again
                                  (terminal-loop-inner))))))

  (terminal-loop-inner))

(define (stop-terminal terminal)
  ;; Kill the underlying process
  (kill-pty-process! (Terminal-*pty-process* terminal))
  (set-box! (Terminal-kill-switch terminal) #t)
  (set-box! (Terminal-focused? terminal) #f)
  (set-box! (Terminal-active terminal) #f)
  ;; If its rendering, remove it from the stack
  (pop-last-component! (Terminal-name terminal)))

;; TODO: Figure out why this fails to expand properly
; (define (create-component-dict term)
;   (~>
;    (hash)
;    (lambda (t)
;      (if (Terminal-event-handler term)
;          (hash-insert t "handle_event" (Terminal-event-handler term))
;          t))
;    (lambda (t)
;      (if (Terminal-cursor-handler term) (hash-insert t "cursor" (Terminal-cursor-handler term)) t))))

(define (create-component-dict term)
  (define add-handle
    (lambda (t)
      (if (Terminal-event-handler term)
          (hash-insert t "handle_event" (Terminal-event-handler term))
          t)))

  (define add-cursor
    (lambda (t)
      (if (Terminal-cursor-handler term) (hash-insert t "cursor" (Terminal-cursor-handler term)) t)))
  (~> (hash) add-handle add-cursor))

(define (show-term term)
  ;; Update the box to now show this
  (set-box! (Terminal-focused? term) #t)

  (set-editor-clip-right! *default-terminal-cols*)

  ;; Mark the terminal as active, only if it isn't active already.
  ;; We don't want to push this component again if it already is
  ;; present on the screen.
  (unless (unbox (Terminal-active term))
    (set-box! (Terminal-active term) #t)
    (push-component! (new-component! (Terminal-name term)
                                     term
                                     ;; Rather than hard code the functions for the component
                                     ;; api, we pass through the functions embedded on the state
                                     ;; it is slightly more efficient to do it this way than
                                     ;; to access the functions on the state object itself,
                                     ;; and decouples the object from the interface required.
                                     (Terminal-renderer term)
                                     (create-component-dict term)))))

(define *min-term-width* 4)
(define *min-term-height* 2)

(define global-max-height #f)
(define global-max-width #f)

(define stashed-area #f)
(define terminal-area #f)

;; Window size calculation
(struct FractionAsWidth (fraction))
(struct FractionAsHeight (fraction))

(define *terminal-fraction* (/ 1 3))

;; Do as a percentage of the terminal area, rather
;; than a fixed size
(define (alternative-calculate-area state rect)
  (if (and terminal-area (equal? stashed-area rect))
      terminal-area
      (begin
        (set! stashed-area rect)

        ;; Just drop the width by 4, always use a quarter of the screen
        (set! *default-terminal-cols* (round (* *terminal-fraction* (area-width rect))))

        (set-editor-clip-right! *default-terminal-cols*)
        (term-resize-impl (- (area-height rect) 3) (- *default-terminal-cols* 5)) ;; Shave one off
        (set! terminal-area
              (area (+ (area-x rect) (- (area-width rect) *default-terminal-cols*))
                    (area-y rect)
                    *default-terminal-cols*
                    (- (area-height rect) 1)))
        terminal-area)))

; (define (alternative-calculate-block-area state rect)
;   ;; Perhaps... use this?
;   (area (unbox (Terminal-x-term state))
;         (unbox (Terminal-y-term state))
;         (unbox (Terminal-viewport-width state))
;         (unbox (Terminal-viewport-height state))))

;; We don't need to run this on _every_ frame. Just when
;; something has actually changed.
(define (calculate-block-area state rect)

  ;; Max width
  ;; TODO: This should be dynamic based on the viewport size
  (define max-width (unbox (Terminal-viewport-width state)))

  ;; Center the terminal, somehow
  (define left-shift (round (/ max-width 2)))

  (define x-term (unbox (Terminal-x-term state)))
  (define y-term (unbox (Terminal-y-term state)))

  (define x (if x-term (- x-term left-shift) (- (round (* 3/4 (area-width rect))) left-shift)))

  ;; Halfway down
  (define y (or y-term (round (* 0/4 (area-height rect)))))

  (define calculated-area
    (area x
          y
          (+ max-width *min-term-width*)
          (+ (unbox (Terminal-viewport-height state)) *min-term-height*)))

  ;; Check if we need to resize the boundaries
  (define resize-height? (> (+ y (area-height calculated-area)) (area-height rect)))
  (define resize-width? (> (+ x (area-width calculated-area)) (area-width rect)))

  ; (set! x-term (+ x left-shift))
  ; (set! y-term y)

  (set-box! (Terminal-x-term state) (+ x left-shift))
  (set-box! (Terminal-y-term state) y)

  (set! global-max-height (area-height rect))
  (set! global-max-width (area-width rect))

  ;; TODO: Any time we call term-resize, we basically just need to make sure
  ;; that the values are positive. Otherwise we're going to have a really bad time.
  (cond
    [(and resize-height? resize-width?)

     (define shrink-by-height (- (+ y (area-height calculated-area)) (area-height rect)))
     (define shrink-by-width (- (+ x (area-width calculated-area)) (area-width rect)))

     (displayln shrink-by-height shrink-by-width)

     (term-resize-from-term state
                            (- (unbox (Terminal-viewport-height state)) shrink-by-height)
                            (- (unbox (Terminal-viewport-width state)) shrink-by-width))

     ;; Grab the new area via this calculation
     (calculate-block-area state rect)]

    [resize-height?
     (define shrink-by-height (- (+ y (area-height calculated-area)) (area-height rect)))

     (displayln shrink-by-height)

     (term-resize-from-term state
                            (- (unbox (Terminal-viewport-height state)) shrink-by-height)
                            (unbox (Terminal-viewport-width state)))

     ;; Grab the new area via this calculation
     (calculate-block-area state rect)]

    [resize-width?

     (define shrink-by-width (- (+ x (area-width calculated-area)) (area-width rect)))
     (displayln shrink-by-width)

     (term-resize-from-term state
                            (unbox (Terminal-viewport-height state))
                            (- (unbox (Terminal-viewport-width state)) shrink-by-width))

     ;; Grab the new area via this calculation
     (calculate-block-area state rect)]

    [else calculated-area]))

(define terminal-cursor-handler
  (lambda (state _) (if (unbox (Terminal-focused? state)) (Terminal-cursor state) #f)))

;; Renders the terminal. The renderer is implemented primarily as a cursor
;; over the cells of the terminal, translated from the underlying
;; representation in the wezterm library back into something that helix can
;; understand. It isn't the most efficient code, since there has to be some
;; translation between the associated representations, however care has been
;; taken in order to reduce the overall amount of copying that occurs, as well
;; as to reduce the total amount of allocations. Allocations are reused
;; where relevant explicitly - the foreground and background cell styles
;; are reused, as well as the string allocation for the individual cell
;; that we are currently rendering.
(define (terminal-render state rect frame)

  (define now (instant/now))

  ;; If this is still alive, keep it around
  (unless (unbox (Terminal-kill-switch state))

    (define block-area (alternative-calculate-area state rect))

    (define x-offset (+ 1 (area-x block-area)))
    (define y-offset (+ 1 (area-y block-area)))

    (define style-cursor (Terminal-style-cursor state))
    ; (define color-cursor-fg (Terminal-color-cursor-fg state))
    ; (define color-cursor-bg (Terminal-color-cursor-bg state))

    (define color-cursor-fg (style->fg (theme->fg *helix.cx*)))
    (define color-cursor-bg (style->bg (theme->bg *helix.cx*)))

    (define *vte* (Terminal-*vte* state))
    (define cursor (Terminal-cursor state))
    (define cell-str (Terminal-str-cell state))

    (define cell-fg (Terminal-cell-fg state))
    (define cell-bg (Terminal-cell-bg state))

    ;; Keep a record of the state of the area for the event handler.
    (set-box! (Terminal-area state) block-area)

    ;; Clear out the target for the terminal
    (buffer/clear frame block-area)
    ;; Render a block
    ; (block/render frame block-area (make-block (style) (style) "all" "plain"))
    (block/render frame
                  block-area
                  (make-block (theme->bg *helix.cx*) (theme->bg *helix.cx*) "all" "plain"))

    ;; TODO: Don't render while its being dragged around. We should probably
    ;; rendering something like "<Rendering paused while window is being dragged>"
    (unless (unbox (Terminal-dragged? state))

      ;; Start at 0
      (vte/reset-iterator! *vte*)

      ;; Advancing the iterator
      (while (vte/advance-iterator! *vte*)
             (define str (vte/iter-cell-str-set-str! *vte* cell-str))
             (when str
               (vte/iter-cell-bg-fg-set-attr! *vte* cell-bg cell-fg)
               (cell-fg-bg->style style-cursor color-cursor-fg color-cursor-bg cell-fg cell-bg)
               (frame-set-string! frame
                                  (+ x-offset (vte/iter-x *vte*))
                                  (+ y-offset (vte/iter-y *vte*))
                                  cell-str
                                  style-cursor))))

    ;; (log::info! (to-string "Rendering time: " (duration->string (instant/elapsed now))))

    ;; Update the cursor accordingly
    (set-position-row! cursor (+ y-offset (vte/cursor-y *vte*)))
    (set-position-col! cursor (+ x-offset (vte/cursor-x *vte*) 1))))

;; Measure the diff between these two
(define on-click-start (mutable-vector 0 0))
(define on-click-end (mutable-vector 0 0))

(define (handle-mouse-event state event *vte*)

  (cond
    [(mouse-event-within-area? event (unbox (Terminal-area state)))

     (case (event-mouse-kind event)
       ;; Mouse event down - any mouse button
       [(0 1 2)

        ;; Update the position to compare against
        (vector-set! on-click-start 0 (event-mouse-col event))
        (vector-set! on-click-start 1 (event-mouse-row event))

        event-result/consume]

       [(3 4 5)

        (set-box! (Terminal-dragged? state) #f)

        event-result/consume]

       [(6 7 8)
        ;; TODO: Implement mouse smoothing, or at least don't necessarily re-render on _every_ single drag?
        ;; Maybe move multiple pixels at a time?
        (define delta-x (- (event-mouse-col event) (mut-vector-ref on-click-start 0)))
        (define delta-y (- (event-mouse-row event) (mut-vector-ref on-click-start 1)))
        (define left-min (round (/ (area-width (unbox (Terminal-area state))) 2)))
        (define x-term (unbox (Terminal-x-term state)))
        (define y-term (unbox (Terminal-y-term state)))

        ;; Only drag when the delta is large enough to warrant it
        ;; TODO: This should be a ratio of the overall screen space
        (if (or (> (abs delta-x) 3) (> (abs delta-y) 3))

            (begin

              (vector-set! on-click-start 0 (event-mouse-col event))
              (vector-set! on-click-start 1 (event-mouse-row event))

              (set-box! (Terminal-dragged? state) #t)

              (when x-term
                (when (< (+ x-term delta-x left-min) global-max-width)
                  (set-box! (Terminal-x-term state) (max (+ x-term delta-x) (round left-min)))))
              (when y-term
                (when (< (+ y-term delta-y (area-height (unbox (Terminal-area state))))
                         global-max-height)
                  (set-box! (Terminal-y-term state) (max (+ y-term delta-y) 0))))

              event-result/consume)

            event-result/consume-without-rerender)]
       ; [(3) (error "todo")]
       ; [(4) (error "todo")]
       ; [(5) (error "todo")]
       ; [(6) (error "todo")]
       ; [(7) (error "todo")]
       ; [(8) (error "todo")]
       ; [(9) (error "todo")]
       ;; Scroll down
       [(10)

        (vte/scroll-down *vte*)

        event-result/consume]
       ;; Scroll up
       ; (pty-process-send-command *pty-process* "\u001e")
       ; event-result/consume
       [(11)

        (vte/scroll-up *vte*)

        event-result/consume]
       ; [(12) (error "todo")]
       ; [(13) (error "todo")]
       ; (error "todo")
       [else event-result/ignore])]

    [else

     (set-box! (Terminal-focused? state) #f)

     event-result/ignore]))

;; Event handler for the terminal.
;; This primarily focuses on forwarding the key events
;; and the mouse events down to the underlying terminal
;; instance. Care is taken to avoid extra allocations
;; in order to make this as smooth as possible.
(define (terminal-event-handler state event)
  (define char (key-event-char event))
  (define *pty-process* (Terminal-*pty-process* state))
  (define *vte* (Terminal-*vte* state))
  (define now (instant/now))

  (cond
    ;; If the terminal is focused, we are going to
    ;; possibly capture input
    [(unbox (Terminal-focused? state))

     (cond
       ;; Backspace
       [(key-event-backspace? event)

        (pty-process-send-command *pty-process* "\x7f;")
        event-result/consume]

       ;; Close with ctrl-esc
       [(key-event-escape? event)
        (if (equal? (key-event-modifier event) key-modifier-ctrl)
            (begin

              (set-box! (Terminal-active state) #f)
              (set-editor-clip-right! 0)

              event-result/close)
            (begin
              (pty-process-send-command *pty-process* "\x1b;")
              event-result/consume))]

       [(equal? char #\c)

        (if (equal? (key-event-modifier event) key-modifier-ctrl)
            (begin
              (pty-process-send-command *pty-process* "\x03;")
              event-result/consume)

            (begin

              (pty-process-send-command-char *pty-process* char)
              event-result/consume))]

       [(key-event-enter? event)
        (pty-process-send-command *pty-process* "\r")
        event-result/consume]

       [(key-event-tab? event)

        (if (equal? (key-event-modifier event) key-modifier-shift)
            (begin
              (set-box! (Terminal-focused? state) #f)
              event-result/consume)

            (begin
              (pty-process-send-command *pty-process* "\x09;")
              event-result/consume))]

       ;; TODO: Handle modifiers here
       [(key-event-up? event)
        (pty-process-send-command *pty-process* "\x1b;[A")
        event-result/consume]
       [(key-event-down? event)
        (pty-process-send-command *pty-process* "\x1b;[B")
        event-result/consume]
       [(key-event-right? event)
        (pty-process-send-command *pty-process* "\x1b;[C")
        event-result/consume]
       [(key-event-left? event)
        (pty-process-send-command *pty-process* "\x1b;[D")
        event-result/consume]

       [char
        (pty-process-send-command-char *pty-process* char)
        event-result/consume]

       [(mouse-event? event) (handle-mouse-event state event *vte*)]
       [else event-result/ignore])]

    [(mouse-event? event)
     (cond
       [(mouse-event-within-area? event (unbox (Terminal-area state)))
        (case (event-mouse-kind event)
          ;; Mouse event down - any mouse button
          [(0 1 2)
           (set-box! (Terminal-focused? state) #t)
           event-result/consume]
          [else event-result/ignore])]
       [else event-result/ignore])]
    ;; Close the terminal popup if it is open
    [(unbox (Terminal-kill-switch state)) event-result/close]

    [else event-result/ignore]))

;; Make one global registry, that when creating a new terminal,
;; we just add to the list.
(struct TerminalRegistry (terminals cursor) #:mutable)

(define *terminal-registry* (TerminalRegistry '() #f))

;; For debug output
(define debug-window #f)

(define (debug-window-event-handler state event)
  (define char (key-event-char event))
  (define *vte* (Terminal-*vte* state))
  (define now (instant/now))

  (cond
    ;; If the terminal is focused, we are going to
    ;; possibly capture input
    [(unbox (Terminal-focused? state))
     (cond
       ;; TODO: Combine this with terminal-event-handler
       [(mouse-event? event) (handle-mouse-event state event *vte*)]
       [else event-result/ignore])]

    [(mouse-event? event)
     (cond
       [(mouse-event-within-area? event (unbox (Terminal-area state)))
        (case (event-mouse-kind event)
          ;; Mouse event down - any mouse button
          [(0 1 2)
           (set-box! (Terminal-focused? state) #t)
           event-result/consume]
          [else event-result/ignore])]
       [else event-result/ignore])]
    ;; Close the terminal popup if it is open
    [(unbox (Terminal-kill-switch state)) event-result/close]

    [else event-result/ignore]))

(define (make-debug-window rows cols on-start-func)
  (define *vte* (raw-virtual-terminal))

  (vte/resize *vte* rows cols)

  (let ([terminal (Terminal "steel-debug-window"
                            (position 0 0)
                            (box cols) ;; Width
                            (box rows) ;; Height
                            (box #f) ;; Focused - Are we capturing user input
                            (box #f) ;; Active - Is the terminal on the screen
                            #f
                            *vte*
                            (style)
                            (Color/rgb 0 0 0)
                            (Color/rgb 0 0 0)
                            ;; More or less a one shot channel. This just says to kill the update
                            ;; loop that is running in the background.
                            (box #f)
                            (mutable-string)
                            (vte/empty-cell)
                            (vte/empty-cell)
                            ;; Don't have an area yet!
                            (box #f)
                            ;; Are we currently dragging the terminal?
                            (box #f)
                            terminal-render
                            debug-window-event-handler
                            #f
                            (box #f)
                            (box #f))])

    ;; Call the on start function if relevant. In general, this
    ;; is going to be `default-on-start-function`, but for
    ;; other use cases (like launching something like xplr)
    ;; we might want to go straight in to xplr.
    (when on-start-func
      (on-start-func terminal))

    (debug-window-loop terminal)

    terminal))

;; Setup capturing generic displayln stuff
(define-values (replaced-writer replaced-reader) (make-async-reader-writer))

;; Overwrite the default writer
(current-output-port replaced-writer)

(define (debug-window-loop term)
  (define (debug-window-loop-inner)
    (define *vte* (Terminal-*vte* term))
    (define *kill-switch* (Terminal-kill-switch term))
    (if (unbox *kill-switch*)
        (pop-last-component! (Terminal-name term))

        ;; Change how we do callbacks here - we're reading from
        ;; the builtin async writer, rather waiting on a pty
        ;; process response.
        (helix-await-callback (async-read-line replaced-reader)
                              (lambda (line)
                                (when line
                                  ;; Handle output?
                                  (vte/advance-bytes-with-carriage-return *vte* line)

                                  ;; Kick off the terminal loop again
                                  (debug-window-loop-inner))))))

  (debug-window-loop-inner))

(define (open-debug-window)
  (cond
    [debug-window (show-term debug-window)]
    [else
     (define new-debug-window (make-debug-window *default-terminal-rows* *default-terminal-cols* #f))
     (set! debug-window new-debug-window)

     ;; TODO: Fix this!
     (set-TerminalRegistry-terminals! *terminal-registry* (list new-debug-window))
     (set-TerminalRegistry-cursor! *terminal-registry* 0)

     (show-term new-debug-window)]))

(define (close-debug-window)
  (when debug-window
    ;; Kill the underlying process
    (set-box! (Terminal-kill-switch debug-window) #t)
    (set-box! (Terminal-focused? debug-window) #f)
    (set-box! (Terminal-active debug-window) #f)
    (pop-last-component! (Terminal-name debug-window))
    (set! debug-window #f)))

;;@doc
;; Hides the terminal
(define (hide-terminal)
  (define cursor (TerminalRegistry-cursor *terminal-registry*))
  (define term (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))

  (when cursor
    (set-box! (Terminal-focused? term) #f)
    (set-box! (Terminal-active term) #f)
    (set-editor-clip-right! 0)
    (pop-last-component! (Terminal-name term))))

;;@doc
;; Opens a new terminal
(define (open-term)
  (define cursor (TerminalRegistry-cursor *terminal-registry*))

  ;; When the cursor exists, we defer to opening an existing one
  (cond
    [cursor (show-term (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))]
    [else
     ;; 45 rows, 80 cols
     (define new-term
       (make-terminal (string-append "Terminal-0")
                      *default-shell*
                      *default-terminal-rows*
                      *default-terminal-cols*
                      default-on-start-function
                      vte/advance-bytes))

     (set-TerminalRegistry-terminals! *terminal-registry* (list new-term))
     (set-TerminalRegistry-cursor! *terminal-registry* 0)

     (show-term new-term)]))

;;@doc
;; Create a new terminal instance
(define (new-term)
  ;; 45 rows, 80 cols
  (define new-term
    (make-terminal
     (string-append "Terminal-"
                    (int->string (length (TerminalRegistry-terminals *terminal-registry*))))
     *default-shell*
     *default-terminal-rows*
     *default-terminal-cols*
     default-on-start-function
     vte/advance-bytes))

  (define cursor (TerminalRegistry-cursor *terminal-registry*))

  ;; Hide the old one
  (when cursor
    (define existing-terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))
    (set-box! (Terminal-active existing-terminal) #f)
    (enqueue-thread-local-callback (lambda () void)))

  ;; Append the new terminal to the
  (set-TerminalRegistry-terminals! *terminal-registry*
                                   (cons new-term (TerminalRegistry-terminals *terminal-registry*)))
  (set-TerminalRegistry-cursor! *terminal-registry* 0)

  (show-term new-term))

;;@doc
;; Swaps to the next active terminal, if there is one.
(define (switch-term)

  (define cursor (TerminalRegistry-cursor *terminal-registry*))

  (when cursor

    (define existing-terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))
    ;; Hide the other terminal
    (set-box! (Terminal-active existing-terminal) #f)

    (if (= (length (TerminalRegistry-terminals *terminal-registry*)) (+ 1 cursor))

        (set-TerminalRegistry-cursor! *terminal-registry* 0)
        (set-TerminalRegistry-cursor! *terminal-registry* (+ 1 cursor)))

    (show-term (list-ref (TerminalRegistry-terminals *terminal-registry*) (+ 1 cursor)))))

(define (term-resize-from-term terminal rows cols)
  (define *vte* (Terminal-*vte* terminal))
  (define *pty-process* (Terminal-*pty-process* terminal))

  (vte/resize *vte* rows cols)

  (when *pty-process*
    (pty-resize! *pty-process* rows cols))

  (set-box! (Terminal-viewport-width terminal) cols)
  ; (set-box! (Terminal-viewport-height terminal) rows)
  )

(define (term-resize-impl rows cols)
  (define cursor (TerminalRegistry-cursor *terminal-registry*))
  (define terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))
  (define *vte* (Terminal-*vte* terminal))
  (define *pty-process* (Terminal-*pty-process* terminal))

  (vte/resize *vte* rows cols)

  (when *pty-process*
    (pty-resize! *pty-process* rows cols))

  (set-box! (Terminal-viewport-width terminal) cols)
  (set-box! (Terminal-viewport-height terminal) rows))

;;@doc
;; Resizes the terminal window to have the given rows and cols
;; `:term-resize <rows> <cols>`
(define (term-resize srows scols)
  (term-resize-impl (string->int srows) (string->int scols)))

(define (remove-nth lst n)
  (let loop ([i 0]
             [lst lst])
    (cond
      [(= i n) (rest lst)]
      [else (cons (first lst) (loop (add1 i) (rest lst)))])))

;;@doc
;; Kill the currently active terminal, if there is one.
(define (kill-active-terminal)
  (define cursor (TerminalRegistry-cursor *terminal-registry*))
  ;; Stop the terminal before we remove it
  (stop-terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))

  ;; Drop the struct from the active terminal list.
  (set-TerminalRegistry-terminals! *terminal-registry*
                                   (remove-nth (TerminalRegistry-terminals *terminal-registry*)
                                               cursor))

  ;; Move the cursor to the first one, if it exists, otherwise false
  (if (empty? (TerminalRegistry-terminals *terminal-registry*))
      (begin
        (set-TerminalRegistry-cursor! *terminal-registry* #f)
        (set-editor-clip-right! 0))
      (set-TerminalRegistry-cursor! *terminal-registry* 0))

  (enqueue-thread-local-callback (lambda () void)))

;;;;; Xplr file picker ;;;;;;
;;; There is a lot of code duplication between this and the core terminal stuff
;;; That being said, for now we'll leave it as is.

(define *EXITED* #f)
(define *NEXT-LOOP* #f)

(define (xplr-event-handler state event)
  (define char (key-event-char event))
  (define *pty-process* (Terminal-*pty-process* state))
  (define *vte* (Terminal-*vte* state))
  (define now (instant/now))

  ; (log::info! "Intercepting key press")

  (cond
    ;; If the terminal is focused, we are going to
    ;; possibly capture input
    [(unbox (Terminal-focused? state))

     (cond
       ;; Backspace
       [(key-event-backspace? event)

        (pty-process-send-command *pty-process* "\x7f;")
        event-result/consume-without-rerender]

       ;; Close with ctrl-esc
       [(key-event-escape? event)
        (if (equal? (key-event-modifier event) key-modifier-ctrl)
            (begin

              (set-box! (Terminal-active state) #f)

              ;; Reset the clipping back to 0 while its not active
              (set-editor-clip-right! 0)

              event-result/close)
            (begin
              (pty-process-send-command *pty-process* "\x1b;")
              event-result/consume-without-rerender))]
       [(key-event-enter? event)
        (pty-process-send-command *pty-process* "\r")

        ;; Okay - now that we've hit enter, we want to grab the lines
        ;; from the output. However at this point the vte hasn't been
        ;; updated with the latest changes. So we need to mark that
        ;; we should grab it on the next go when pulling values out.

        (set! *EXITED* #t)

        event-result/consume-without-rerender]

       [(key-event-tab? event)

        (if (equal? (key-event-modifier event) key-modifier-shift)
            (begin
              (set-box! (Terminal-focused? state) #f)
              event-result/consume-without-rerender)

            (begin
              (pty-process-send-command *pty-process* "\x09;")
              event-result/consume-without-rerender))]

       ;; TODO: Handle modifiers here
       [(key-event-up? event)
        (pty-process-send-command *pty-process* "\x1b;[A")
        event-result/consume-without-rerender]
       [(key-event-down? event)
        (pty-process-send-command *pty-process* "\x1b;[B")
        event-result/consume-without-rerender]
       [(key-event-right? event)
        (pty-process-send-command *pty-process* "\x1b;[C")
        event-result/consume-without-rerender]
       [(key-event-left? event)
        (pty-process-send-command *pty-process* "\x1b;[D")
        event-result/consume-without-rerender]

       [char
        (pty-process-send-command-char *pty-process* char)
        event-result/consume-without-rerender]

       [(mouse-event? event) (handle-mouse-event state event *vte*)]
       [else event-result/ignore])]

    [(mouse-event? event)
     (cond
       [(mouse-event-within-area? event (unbox (Terminal-area state)))
        (case (event-mouse-kind event)
          ;; Mouse event down - any mouse button
          [(0 1 2)
           (set-box! (Terminal-focused? state) #t)
           event-result/consume]
          [else event-result/ignore])]
       [else event-result/ignore])]
    ;; Close the terminal popup if it is open
    [(unbox (Terminal-kill-switch state)) event-result/close]

    [else event-result/ignore]))

(define *xplr* #f)

(define (make-xplr shell rows cols)
  (define *pty-process* (create-native-pty-system! shell))
  (define *vte* (virtual-terminal *pty-process*))

  (vte/resize *vte* rows cols)
  (pty-resize! *pty-process* rows cols)

  (pty-process-send-command *pty-process* (string-append "cd " (helix-find-workspace) " && xplr\r"))

  (let ([terminal (Terminal "xplr"
                            (position 0 0)
                            (box cols) ;; Width
                            (box rows) ;; Height
                            (box #f) ;; Focused - Are we capturing user input
                            (box #f) ;; Active - Is the terminal on the screen
                            *pty-process*
                            *vte*
                            (style)
                            (Color/rgb 0 0 0)
                            (Color/rgb 0 0 0)
                            ;; More or less a one shot channel. This just says to kill the update
                            ;; loop that is running in the background.
                            (box #f)
                            (mutable-string)
                            (vte/empty-cell)
                            (vte/empty-cell)
                            ;; Don't have an area yet!
                            (box #f)
                            ;; Are we currently dragging the terminal?
                            (box #f)
                            terminal-render
                            xplr-event-handler
                            #f
                            (box #f) ;; x-term
                            (box #f) ;; y-term
                            )])

    (xplr-loop terminal)

    terminal))

(define (xplr-loop term)
  ;; Kick off the terminal loop, so that we can run this
  ;; in the background to (hopefully) gracefully kill
  ;; running terminals
  (define (xplr-loop-inner)
    (define *pty-process* (Terminal-*pty-process* term))
    (define *vte* (Terminal-*vte* term))
    (define *kill-switch* (Terminal-kill-switch term))
    (unless (unbox *kill-switch*)
      (helix-await-callback (async-try-read-line *pty-process*)
                            (lambda (line)
                              (when line
                                ;; Handle output?
                                (vte/advance-bytes *vte* line)

                                (if *NEXT-LOOP*
                                    (begin

                                      (enqueue-thread-local-callback-with-delay
                                       100 ;; ms
                                       (lambda ()
                                         (helix.open (vte/line->string (list-ref (vte/lines *vte*)
                                                                                 2)))
                                         (set! *EXITED* #f)
                                         (set! *NEXT-LOOP* #f)
                                         (stop-terminal term)
                                         (pop-last-component! "xplr")))

                                      ; (displayln (vte/line->string (list-ref (vte/lines *vte*) 2)))
                                      ; (helix.open (vte/line->string (list-ref (vte/lines *vte*) 2)))
                                      ; (set! *EXITED* #f)
                                      ; (set! *NEXT-LOOP* #f)
                                      ; (stop-terminal term)
                                      ; (pop-last-component! "xplr")
                                      )

                                    (begin
                                      ;; After the kill switch is enabled,
                                      ;; the next update loop should render the latest form
                                      ;; of the terminal.
                                      (when *EXITED*
                                        (set! *NEXT-LOOP* #t))

                                      ;; Kick off the terminal loop again
                                      (xplr-loop-inner))))))))

  (xplr-loop-inner))

(define (xplr)
  ;; Set the xplr width and height
  ;; Also configure the location of the file tree
  (define new-term (make-xplr *default-shell* *default-terminal-rows* *default-terminal-cols*))
  (set! *xplr* new-term)
  (show-term new-term))

(define (close-xplr)
  (when *xplr*
    (stop-terminal *xplr*)
    (set! *xplr* #f)))
