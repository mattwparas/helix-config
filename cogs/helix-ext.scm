(require "helix/editor.scm")
(require "helix/misc.scm")
(require-builtin helix/core/text as text.)
(require "steel/sync")

(provide eval-buffer
         evalp
         running-on-main-thread?
         hx.with-context
         hx.block-on-task)

(define (get-document-as-slice)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (text.rope->string (editor->text focus-doc-id))))

;;@doc
;; Eval the current buffer, morally equivalent to load-buffer!
(define (eval-buffer)
  (eval-string (get-document-as-slice)))

;;@doc
;; Eval prompt
(define (evalp)
  (push-component! (prompt "" (lambda (expr) (set-status! (eval-string expr))))))

;;@doc
;; Check what the main thread id is, compare to the main thread
(define (running-on-main-thread?)
  (= (current-thread-id) *helix.id*))

;; If running on the main thread already, just do nothing.
;; Check the ID of the engine, and if we're already on the
;; main thread, just continue as is - i.e. just block.
(define (hx.with-context thunk)
  (if (running-on-main-thread?)
      (thunk)
      (begin
        (define task (task #f))
        ;; Send on the main thread
        (acquire-context-lock thunk task)
        task)))

(define (hx.block-on-task thunk)
  (if (running-on-main-thread?) (thunk) (block-on-task (hx.with-context thunk))))
