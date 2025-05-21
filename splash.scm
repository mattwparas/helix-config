(require "helix/components.scm")
(require "helix/misc.scm")

(provide show-splash)

(define (for-each-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (when (null? lst)
          (return! void))
        (for-each-index func (cdr lst) (+ index 1)))))

(define splash
  "
 .
 ###x.        .|
 d#####x,   ,v||
  '+#####v||||||
     ,v|||||+'.      _     _           _
  ,v|||||^'>####    | |   | |   ___   | | (_) __  __
 |||||^'  .v####    | |___| |  /   \\  | |  _  \\ \\/ /
 ||||=..v#####P'    |  ___  | /  ^  | | | | |  \\  /
 ''v'>#####P'       | |   | | |  ---  | | | |  /  \\
 ,######/P||x.      |_|   |_|  \\___/  |_| |_| /_/\\_\\
 ####P' \"x|||||,
 |/'       'x|||    (A (post-modern (modal (text editor)))).
  '           '|")

(define splash-split (split-many splash "\n"))

(define max-width (apply max (map string-length splash-split)))
(define splash-depth (length splash-split))

(struct Splash ())

(define (splash-render state rect frame)
  ;; Snag the cursor position, mapped to an index within the list

  ;; Calculate the block area in terms of the parent
  ; (define half-parent-width (round (/ (area-width rect) 2)))

  (define half-parent-width (+ 10 max-width))

  (define half-parent-height (round (/ (area-height rect) 4)))

  ; (define starting-x-offset (round (/ (area-width rect) 4)))

  (define starting-x-offset (exact (- (round (/ (area-width rect) 2)) (round (/ max-width 2)) 5)))
  ; (define starting-x-offset (round (*  (/ (area-width rect) 2) max-width)))

  (define starting-y-offset (round (/ (area-height rect) 4)))

  ;; Draw a preview area on the right
  (define block-area
    (area starting-x-offset
          (- starting-y-offset 1)
          half-parent-width
          ;; TODO: Clamp the window height here, otherwise the window scrolls off the bottom
          (+ 10
             (if (> (+ half-parent-height starting-y-offset) (area-height rect))
                 (- (area-height rect) starting-y-offset)
                 half-parent-height))))

  ;; Shift the text about half way through
  ; (define x (+ (area-x block-area) (round (/ max-width 4))))
  (define x (- (round (/ (area-width rect) 2)) (round (/ max-width 2))))
  (define y (area-y block-area))

  ; ;; Our range should only adjust to match the view port
  ; (define start (unbox (Picker-window-start state)))

  ; (define currently-highlighted (- cursor-position (unbox (Picker-window-start state))))

  ; (define wide-string (make-string (round (/ (- half-parent-width 2) 2)) #\space))

  ; (set-box! (Picker-max-length state) (area-height block-area))

  ; (define view-slice
  ;   (slice (unbox (Picker-items-view state)) start (- (unbox (Picker-max-length state)) 1)))

  ; (define selection (try-list-ref view-slice currently-highlighted))

  (define found-style
    (~> (style)
        (style-bg (style->bg (theme->bg *helix.cx*)))
        (style-fg (style->fg (theme->fg *helix.cx*)))))

  (define text-style (theme-scope "ui.text"))
  (define bg-style (theme-scope "ui.background"))

  ; (define experiment (~> (style) (style-fg Color/Blue)))

  (define string-text (theme-scope "string"))
  (define keyword (theme-scope "keyword"))

  ; (displayln text-style)
  ; (error "test")

  ;; Clear out the target for the terminal
  ;; Ensure that this is within the bounds
  (buffer/clear frame block-area)

  (block/render frame
                (area (- (area-x block-area) 1)
                      (- (area-y block-area) 1)
                      (+ 2 (area-width block-area))
                      (+ 2 (area-height block-area)))
                ;; TODO: Figure out how to get the styles right
                #;(make-block (theme->bg *helix.cx*) (theme->bg *helix.cx*) "all" "plain")
                (make-block bg-style bg-style "all" "plain"))

  (for-each-index (lambda (index line) (frame-set-string! frame x (+ y index) line string-text))
                  splash-split
                  0)

  ;; Render the various things. Probably just, <space> - F to pick files?
  (frame-set-string! frame x (+ y splash-depth 3) "<space>f to open the file picker" keyword)
  (frame-set-string! frame x (+ y splash-depth 4) "<space>? to see all the commands" keyword)
  (frame-set-string! frame x (+ y splash-depth 5) ":theme <name> to change themes" keyword)
  (frame-set-string! frame x (+ y splash-depth 6) ":evalp to evaluate a steel expression" keyword))

(define (splash-event-handler _ event)
  (if (key-event? event) event-result/close event-result/ignore))

(define (show-splash)
  (push-component! (new-component! "splash-screen"
                                   (Splash)
                                   splash-render
                                   (hash "handle_event" splash-event-handler))))
