(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "key-emulation.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/editor.scm")

(require-builtin steel/time)
(require-builtin helix/core/text)

;; u
(define (vim-undo)
  (helix.static.undo)
  (helix.static.collapse_selection))

;; a
(define (vim-append-mode)
  ;; Move to insert mode
  (helix.static.insert_mode)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_right))))

;; l
(define (move-char-right-same-line)
  (define count (editor-count))
  (do-n-times count move-char-right-same-line-impl))

(define (move-char-right-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_right))))

;; h
(define (move-char-left-same-line)
  (define count (editor-count))
  (do-n-times count move-char-left-same-line-impl))

(define (move-char-left-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_left))))

;; k
(define (move-line-up)
  (helix.static.move_line_up)
  (move-line-up-impl))

(define (move-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (move-char-left-same-line-impl))))

;; j
(define (move-line-down)
  (helix.static.move_line_down)
  (move-line-down-impl))

(define (move-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (move-char-left-same-line-impl))))

;; f(char)
(define (vim-find-next-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-next-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\f char)))))))

(define (vim-find-next-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (move-right-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; F(char)
(define (vim-find-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\F char)))))))

(define (vim-find-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move left n times
      [(equal? char next-char) (move-left-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; t(char)
(define (vim-find-till-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-till-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\t char)))))))

(define (vim-find-till-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (move-right-n i)
       (helix.static.move_char_left)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (move-right-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; T(char)
(define (vim-till-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-till-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\T char)))))))

(define (vim-till-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (move-left-n i)
       (helix.static.move_char_right)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (move-left-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; NOTE: this is midly hacky, but the goal is to run the macro
;; stored in the , register, so whatever it takes...
;; ,
(define (vim-repeat-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (helix.echo find-macro)
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (vim-find-next-char-impl char count)]
    [(equal? action #\F) (vim-find-prev-char-impl char count)]
    [(equal? action #\t) (vim-find-till-char-impl char count)]
    [(equal? action #\T) (vim-till-prev-char-impl char count)]))

;; ;
(define (vim-reverse-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (vim-find-prev-char-impl char count)]
    [(equal? action #\F) (vim-find-next-char-impl char count)]
    [(equal? action #\t) (vim-till-prev-char-impl char count)]
    [(equal? action #\T) (vim-find-till-char-impl char count)]))

;; G or (line-number)G
(define (vim-goto-line-or-last)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))

  (helix.static.goto_line)

  (define end-pos (cursor-position))

  ;; If we didn't move, no count was provided - go to last line
  (when (= start-pos end-pos)
    (helix.static.goto_last_line)))

;; e
(define (vim-next-word-end)
  (helix.static.move_next_word_end)
  (helix.static.collapse_selection))

;; E
(define (vim-next-long-word-end)
  (helix.static.move_next_long_word_end)
  (helix.static.collapse_selection))

;; b
(define (vim-prev-word-start)
  (helix.static.move_prev_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (vim-prev-word-start)))

;; B
(define (vim-prev-long-word-start)
  (helix.static.move_prev_long_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (vim-prev-long-word-start)))

;; w
(define (vim-next-word-start)
  (define count (editor-count))
  (do-n-times count vim-next-word-start-impl))

(define (vim-next-word-start-impl)
  (get-next-word-start move-right-n))

;; W
(define (vim-next-long-word-start)
  (define count (editor-count))
  (do-n-times count vim-next-long-word-start-impl))

(define (vim-next-long-word-start-impl)
  (get-next-long-word-start move-right-n))

;; {
(define (vim-goto-prev-paragraph)
  (helix.static.goto_prev_paragraph)
  (helix.static.collapse_selection))

;; }
(define (vim-goto-next-paragraph)
  (helix.static.goto_next_paragraph)
  (helix.static.collapse_selection))

;; V
(define (visual-line-mode)
  (set-visual-line-mode! #t)
  (helix.static.select_mode)
  (helix.static.extend_to_line_bounds))

;; esc from normal
(define (vim-exit-insert-mode)
  (helix.static.collapse_selection)
  (helix.static.normal_mode)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (when (equal? #\newline char)
      (move-char-left-same-line-impl)
      ;; BUG: for some reason, editor-count stacks, so I have to set it to 0 before continuing?
      ;; (e.g exiting insert mode from end of line -> 7k results in movement from 17k if below line isnt added)
      ;; TODO: look into Helix code to see why this happens
      (set-editor-count! 0))))

(provide vim-undo
         vim-append-mode
         move-char-right-same-line
         move-char-left-same-line
         move-line-up-impl
         move-line-up
         move-line-down-impl
         move-line-down
         vim-find-next-char
         vim-find-prev-char
         vim-find-till-char
         vim-till-prev-char
         vim-repeat-last-find
         vim-reverse-last-find
         vim-goto-line-or-last
         vim-next-word-start
         vim-next-word-end
         vim-prev-word-start
         vim-prev-long-word-start
         vim-next-long-word-start
         vim-next-long-word-end
         vim-goto-next-paragraph
         vim-goto-prev-paragraph
         visual-line-mode
         vim-exit-insert-mode)
