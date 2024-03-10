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
     base-color]
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

;; Embedded terminal widget
(struct Terminal
        (cursor viewport-width
                viewport-height
                focused?
                active
                *pty-process*
                *vte*
                style-cursor
                color-cursor-fg
                color-cursor-bg
                kill-switch))

(define (make-terminal shell rows cols)
  (define *pty-process* (create-native-pty-system! shell))
  (define *vte* (virtual-terminal *pty-process*))

  (vte/resize *vte* rows cols)
  (pty-resize! *pty-process* rows cols)

  (let ([terminal (Terminal (position 0 0)
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
                            (box #f))])

    (terminal-loop terminal)

    terminal))

(define (terminal-loop term)
  ;; Kick off the terminal loop, so that we can run this
  ;; in the background to (hopefully) gracefully kill
  ;; running terminals
  (define (terminal-loop-inner)
    (define *pty-process* (Terminal-*pty-process* term))
    (define *vte* (Terminal-*vte* term))
    (define *kill-switch* (Terminal-kill-switch term))
    (unless (unbox *kill-switch*)
      (helix-await-callback (async-try-read-line *pty-process*)
                            (lambda (line)
                              (when line
                                ;; Handle output?
                                (vte/advance-bytes *vte* line)

                                ;; Kick off the terminal loop again
                                (terminal-loop-inner))))))

  (terminal-loop-inner))

(define (stop-terminal terminal)
  ;; Kill the underlying process
  (kill-pty-process! (Terminal-*pty-process* terminal))
  (set-box! (Terminal-kill-switch terminal) #t)
  (set-box! (Terminal-focused? terminal) #f)
  (set-box! (Terminal-active terminal) #f))

(define (show-term term)
  ;; Update the box to now show this
  (set-box! (Terminal-focused? term) #t)
  ;; Mark the terminal as active, only if it isn't active already.
  ;; We don't want to push this component again if it already is
  ;; present on the screen.
  (unless (unbox (Terminal-active term))
    (set-box! (Terminal-active term) #t)
    (push-component! (new-component! "Terminal"
                                     term
                                     terminal-render
                                     (hash "handle_event"
                                           terminal-event-handler
                                           "cursor"
                                           (lambda (state _)

                                             (if (unbox (Terminal-focused? state))

                                                 (Terminal-cursor state)

                                                 #f)))))))

; (define (pin-right overlay-width rect)
;   (define left-shift (- (area-width rect) overlay-width))

;   )

(define (calculate-block-area state rect)

  ;; Max width
  ;; TODO: This should be dynamic based on the viewport size
  (define max-width (unbox (Terminal-viewport-width state)))
  (define left-shift (round (/ max-width 2)))
  ; (define left-shift (- (area-width rect) max-width))
  ; (define x (- (area-width rect) max-width 4))

  ;; Put this at 3/4 to the right side of the screen
  (define x (- (round (* 3/4 (area-width rect))) left-shift))

  ;; Halfway down
  (define y (round (* 1/4 (area-height rect))))

  (define calculated-area (area x y (+ max-width 4) (+ (unbox (Terminal-viewport-height state)) 2)))

  ;; If the rows will extend beyond the bottom of the screen,
  ;; shrink the terminal to fit.
  (if (> (+ y (area-height calculated-area)) (area-height rect))

      ;; Resize the block area to fit, recalculate

      (begin
        (define shrink-by-height (- (+ y (area-height calculated-area)) (area-height rect)))

        (log::info! (to-string (+ y (area-height calculated-area)) " " (area-height rect)))
        ;; TODO: Add a term resize that accepts ints, not strings - it only takes strings
        ;; since that is what is getting passed in via the commands
        (term-resize (int->string (- (unbox (Terminal-viewport-height state)) shrink-by-height))
                     (int->string (unbox (Terminal-viewport-width state))))

        ;; Grab the new area via this calculation
        (calculate-block-area state rect))

      calculated-area

      ;; Resize?
      ; (error "Can't fit!")
      )

  ;; TODO: Have this be configurable. The location from which the terminal
  ;; should start.
  ; (area x y (+ max-width 4) (+ (#%unbox (Terminal-viewport-height state)) 2))
  )

(define (terminal-render state rect frame)
  ;; If this is still alive, keep it around
  (unless (unbox (Terminal-kill-switch state))

    (define block-area (calculate-block-area state rect))

    (define x-offset (+ 1 (area-x block-area)))
    (define y-offset (+ 1 (area-y block-area)))

    (define style-cursor (Terminal-style-cursor state))
    (define color-cursor-fg (Terminal-color-cursor-fg state))
    (define color-cursor-bg (Terminal-color-cursor-bg state))
    (define *vte* (Terminal-*vte* state))
    (define cursor (Terminal-cursor state))

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
             (cell-fg-bg->style style-cursor color-cursor-fg color-cursor-bg fg-style bg-style)
             (frame-set-string! frame (+ x-offset cell-x) (+ y-offset cell-y) str style-cursor)))

    ;; Update the cursor accordingly
    (set-position-row! cursor (+ y-offset (vte/cursor-y *vte*)))
    (set-position-col! cursor (+ x-offset (vte/cursor-x *vte*) 1))))

;; Don't handle any events
(define (terminal-event-handler state event)
  (define char (key-event-char event))
  (define *pty-process* (Terminal-*pty-process* state))

  (cond
    ;; If the terminal is focused, we are going to
    ;; possibly capture input
    [(unbox (Terminal-focused? state))

     (cond
       ;; Backspace
       [(key-event-backspace? event)

        (pty-process-send-command *pty-process* "\x7f")
        event-result/consume]

       ;; Close with ctrl-esc
       [(key-event-escape? event)
        (if (equal? (key-event-modifier event) key-modifier-ctrl)
            (begin

              (set-box! (Terminal-active state) #f)

              event-result/close)
            (begin
              (pty-process-send-command *pty-process* "\x1b")
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
              (pty-process-send-command *pty-process* "\x09")
              event-result/consume))]

       ;; TODO: Handle modifiers here
       [(key-event-up? event)
        (pty-process-send-command *pty-process* "\x1b[A")
        event-result/consume]
       [(key-event-down? event)
        (pty-process-send-command *pty-process* "\x1b[B")
        event-result/consume]
       [(key-event-right? event)
        (pty-process-send-command *pty-process* "\x1b[C")
        event-result/consume]
       [(key-event-left? event)
        (pty-process-send-command *pty-process* "\x1b[D")
        event-result/consume]

       [char
        (pty-process-send-command *pty-process* (string char))
        event-result/consume]

       [else event-result/ignore])]

    ;; Close the terminal popup if it is open
    [(unbox (Terminal-kill-switch state)) event-result/close]

    [else event-result/ignore]))

;; Make one global registry, that when creating a new terminal,
;; we just add to the list.
(struct TerminalRegistry (terminals cursor) #:mutable)

(define *terminal-registry* (TerminalRegistry '() #f))

;; Opens a new terminal
(define (open-term)
  (define cursor (TerminalRegistry-cursor *terminal-registry*))

  ;; When the cursor exists, we defer to opening an existing one
  (cond
    [cursor (show-term (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))]
    [else
     ;; 45 rows, 80 cols
     (define new-term (make-terminal "/usr/bin/zsh" 45 80))

     (set-TerminalRegistry-terminals! *terminal-registry* (list new-term))
     (set-TerminalRegistry-cursor! *terminal-registry* 0)

     (show-term new-term)]))

;; Cycle terminals?
(define (new-term)
  ;; 45 rows, 80 cols
  (define new-term (make-terminal "/usr/bin/zsh" 45 80))

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

(define (switch-term)

  (define cursor (TerminalRegistry-cursor *terminal-registry*))

  (when cursor

    (define existing-terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))
    ;; Hide the other terminal
    (set-box! (Terminal-active existing-terminal) #f)

    (if (= (length (TerminalRegistry-terminals *terminal-registry*)) (+ 1 cursor))

        (set-TerminalRegistry-cursor! *terminal-registry* 0)
        (set-TerminalRegistry-cursor! (+ 1 cursor)))

    (show-term (list-ref (TerminalRegistry-terminals *terminal-registry*) (+ 1 cursor)))))

(define (term-resize srows scols)

  (define cursor (TerminalRegistry-cursor *terminal-registry*))
  (define rows (string->int srows))
  (define cols (string->int scols))
  (define terminal (list-ref (TerminalRegistry-terminals *terminal-registry*) cursor))
  (define *vte* (Terminal-*vte* terminal))
  (define *pty-process* (Terminal-*pty-process* terminal))

  (vte/resize *vte* rows cols)
  (pty-resize! *pty-process* rows cols)

  (set-box! (Terminal-viewport-width terminal) cols)
  (set-box! (Terminal-viewport-height terminal) rows))

(define (remove-nth lst n)
  (let loop ([i 0] [lst lst])
    (cond
      [(= i n) (rest lst)]
      [else (cons (first lst) (loop (add1 i) (rest lst)))])))

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
      (set-TerminalRegistry-cursor! *terminal-registry* #f)
      (set-TerminalRegistry-cursor! *terminal-registry* 0))

  (enqueue-thread-local-callback (lambda () void)))
