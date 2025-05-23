(require "helix/editor.scm")
(require "helix/components.scm")
(require "helix/misc.scm")

(provide focus
         unfocus)

(define window-size #f)
(define focused? #f)

(struct FocusMode ())

(define (render-focus-mode _ rect _)
  (define width (exact (round (/ (area-width rect) 6))))
  (set-editor-clip-right! width)
  (set-editor-clip-left! width))

(define (event-handler _ _)
  event-result/ignore)

(define (focus)
  (unless focused?
    (set! focused? #t)
    (push-component!
     (new-component! "focus-mode" (FocusMode) render-focus-mode (hash "handle_event" event-handler)))
    (enqueue-thread-local-callback (lambda () void))))

(define (unfocus)
  (when focused?
    (set! focused? #f)
    (pop-last-component-by-name! "focus-mode")
    (enqueue-thread-local-callback (lambda ()
                                     (set-editor-clip-right! 0)
                                     (set-editor-clip-left! 0)))))
