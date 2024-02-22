(require "helix/configuration.scm")
(require "helix/misc.scm")

(require-builtin helix/components)

(provide show-welcome-message)

(struct Component (rendered) #:mutable)

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

(define welcome-message-style (~> (style) (style-bg Color/Black) (style-fg Color/White)))

;; Mutable character buffer, for storing the inputs
(define character-buffer '())
;; Prior lines, if we newline while typing
(define lines '())

;; TODO: Max width, auto wrap in the buffer

;; Perhaps use a mutable buffer instead?
(define cursor (position 0 0))

;;@doc
;; Render a nice helpful welcome message
(define (show-welcome-message)
  ;; Render - how will that work?
  (push-component!
   ; (overlay
   (new-component!
    "DynamicComponent"
    (Component #t)
    ;; Render
    (lambda (state rect frame)

      (define max-width (foldl max 0 (map string-length welcome-message)))
      (define left-shift (round (/ max-width 2)))

      (define x (- (round (/ (area-width rect) 2)) left-shift))
      (define y (round (/ (area-height rect) 2)))

      (define block-area (area (- x 10) (- y 5) (+ max-width 20) 20))

      (buffer/clear frame block-area)
      (block/render frame block-area (block))

      (loop (lambda (row message) (frame-set-string! frame x row message welcome-message-style))
            y
            (append welcome-message
                    (reverse (map (lambda (x) (list->string (reverse x)))
                                  (cons character-buffer lines)))))

      ; (frame-set-string! frame
      ;                    x
      ;                    (+ y (length welcome-message))
      ;                    (list->string (reverse character-buffer))
      ;                    welcome-message-style)

      (set! cursor-shape
            ;; Just need to keep this around
            (position (+ y (length welcome-message) (length lines)) (+ x (length character-buffer))))

      ;; Mark this as rendered
      (set-Component-rendered! state #t)

      ;; Set the
      )
    ; (hash)
    (hash "handle_event"
          ;; Handle event
          (lambda (state event)

            (define char (key-event-char event))

            ;; Since at this point we take almost every event,
            ;; we should just go ahead and reset
            (set-Component-rendered! state #f)

            (cond
              [(key-event-backspace? event)

               ;; If the buffer is empty, attempt to
               ;; roll back to the previous line.
               (if (empty? character-buffer)

                   ;; If we have a line to roll back to, do it
                   (when (not (empty? lines))
                     (set! character-buffer (car lines))
                     (set! lines (cdr lines)))

                   (set! character-buffer (cdr character-buffer)))

               event-result/consume]
              [(key-event-escape? event) event-result/close]

              [(key-event-enter? event)

               ;; Push the lines down
               (set! lines (cons character-buffer lines))
               (set! character-buffer '())

               event-result/consume]

              [char

               ;; TODO: Handle newline?
               ; (if (equal? char #\newline)

               (set! character-buffer (cons char character-buffer))
               event-result/consume]

              [else event-result/consume]))
          "should_update"
          ;; Should update
          (lambda (state)
            ; (Component-rendered state)
            #t)
          "cursor"
          ;; Cursor
          (lambda (state area) cursor-shape)
          ;       "required_size"
          ;       ;; Required size
          ;       (lambda (state viewport) void))
          ; )
          ))))

(show-welcome-message)
