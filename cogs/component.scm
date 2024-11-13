(require "helix/configuration.scm")
(require "helix/misc.scm")

(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))

(require "helix/editor.scm")
(require (prefix-in helix.static. "helix/static.scm"))

(provide cursor-selection)

(define (for-each-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (when (null? lst)
          (return! void))
        (for-each-index func (cdr lst) (+ index 1)))))

;; The callback to call with the value found at that list
;; The list of items to be rendered, and the callback to call
;; with the value found at that list
(struct Picker
        (items callback
               ;; the position in the list of items
               cursor
               ;; The max length to display
               max-length
               ;; Where the window will start in the list
               window-start
               ;; How to format the item to show in the list
               value-formatter
               ;; If present, prefix the selected item with this
               highlight-prefix
               ;; The style to highlight the lines that are not selected
               default-style
               ;; The style to highlight the selected lines
               highlight-style))

;; Moves the cursor down
(define (move-cursor-down picker)
  ;; Current element that we're on
  (define current (Picker-cursor picker))
  (define window-start (Picker-window-start picker))

  (set-box! current (modulo (+ 1 (unbox current)) (length (Picker-items picker))))

  (when (> (unbox current) (+ (unbox window-start) (- (Picker-max-length picker) 1)))
    (set-box! window-start (+ (unbox window-start) 1)))

  (when (< (unbox current) (unbox window-start))
    (set-box! window-start (unbox current))))

(define (saturating-sub x y)
  (max (- x y) 0))

;; Moves the cursor up
(define (move-cursor-up picker)
  ;; Current element that we're on.
  (define current (Picker-cursor picker))
  (define window-start (Picker-window-start picker))

  (set-box! current (modulo (- (unbox current) 1) (length (Picker-items picker))))

  ;; Adjust viewport
  (when (< (unbox current) (unbox window-start))
    (set-box! window-start (- (unbox window-start) 1)))

  ;; Wrap around
  (when (> (unbox current) (+ (unbox window-start) (- (Picker-max-length picker) 1)))
    (set-box! window-start (saturating-sub (unbox current) (- (Picker-max-length picker) 1)))))

(define (picker-event-handler state event)
  (define char (key-event-char event))

  (cond
    [(key-event-down? event)
     (move-cursor-down state)
     event-result/consume]

    [(key-event-up? event)
     (move-cursor-up state)
     event-result/consume]

    [(key-event-tab? event)
     (if (equal? (key-event-modifier event) key-modifier-shift)
         (move-cursor-up state)
         (move-cursor-down state))

     event-result/consume]

    ;; Map the current cursor -> item in the list
    ;; Call the function on that, close
    [(key-event-enter? event)
     (define callback (Picker-callback state))
     ;; Make the callback
     (callback (list-ref (Picker-items state) (unbox (Picker-cursor state))))
     event-result/close]

    [else
     ;; TODO: Include a completion handler here!
     (enqueue-thread-local-callback (lambda () (pop-last-component! "steel-picker")))
     event-result/ignore]))

(define (picker-render state rect frame)
  ;; Snag the cursor position, mapped to an index within the list
  (define cursor-position (unbox (Picker-cursor state)))

  ;; Calculate the block area in terms of the parent

  ; (define half-parent-width (round (/ (area-width rect) 2)))
  ; (define half-parent-height (round (/ (area-height rect) 2)))

  (define half-parent-height (Picker-max-length state))
  ; (define half-parent-height (length (Picker-items state))

  ;; TODO: Lift highlighted prefix + selection highlighted out of this
  (define half-parent-width
    (+ 3
       (apply max
              (map (lambda (value) (string-length ((Picker-value-formatter state) value)))
                   (Picker-items state)))))

  ;; Not sure at what point this would not work
  (define current-cursor-position (car (cx->cursor)))
  (define starting-x-offset (position-col current-cursor-position))
  (define starting-y-offset (+ 1 (position-row current-cursor-position)))

  (define block-area
    (area starting-x-offset
          starting-y-offset
          half-parent-width
          ;; TODO: Clamp the window height here, otherwise the window scrolls off the bottom
          (if (> (+ half-parent-height starting-y-offset) (area-height rect))
              (- (area-height rect) starting-y-offset)
              half-parent-height)))

  (define x (+ 1 (area-x block-area)))
  (define y (area-y block-area))

  ;; Our range should only adjust to match the view port
  (define start (unbox (Picker-window-start state)))

  (define currently-highlighted (- cursor-position (unbox (Picker-window-start state))))

  (define wide-string (make-string (- half-parent-width 1) #\space))

  ;; Clear out the target for the terminal
  ;; Ensure that this is within the bounds
  (buffer/clear frame block-area)

  (for-each-index
   (lambda (index row)
     ;; Don't render if its below the bottom of the parent frame
     ;; TODO: This seems to cause some issues - look at the buffer/clear with the block-area?
     (when (< (+ index y) (+ (area-y rect) (area-height rect)))

       (if (equal? index currently-highlighted)
           (begin
             (frame-set-string! frame x (+ index y) wide-string (Picker-highlight-style state))
             ;; TODO: Clean this up!
             (frame-set-string! frame
                                x
                                (+ index y)
                                ;; TODO: This isn't my favorite stuff here
                                (if (string? (Picker-highlight-prefix state))
                                    (string-append (Picker-highlight-prefix state)
                                                   ((Picker-value-formatter state) row))
                                    ((Picker-value-formatter state) row))
                                (Picker-highlight-style state)))
           (frame-set-string! frame
                              x
                              (+ index y)
                              ((Picker-value-formatter state) row)
                              (Picker-default-style state)))))
   (slice (Picker-items state) start (Picker-max-length state))
   0))

(define (default-formatter value)
  (with-output-to-string (lambda () (display value))))

;;@doc
;; Drop down at the given cursor. This does not push the component, but just creates it.
;; You are still responsible to call `push-component!` on this.
(define (cursor-selection items
                          on-selection
                          #:max-window-size [max-window-size 8]
                          #:value-formatter [formatter default-formatter]
                          #:highlight-prefix [highlight-prefix void]
                          #:default-style [default-style (style)]
                          #:highlight-style [highlight-style (style-bg (style) Color/Gray)])

  (new-component! "steel-picker"
                  (Picker items
                          on-selection
                          (box 0)
                          max-window-size
                          (box 0)
                          formatter
                          highlight-prefix
                          default-style
                          highlight-style)
                  picker-render
                  (hash "handle_event" picker-event-handler)))

(define (np)
  (push-component! (cursor-selection (range 0 100) (lambda (_) void))))
