(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "key-emulation.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/editor.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

;; l
(define (extend-char-right-same-line)
  (define count (editor-count))
  (do-n-times count extend-char-right-same-line-impl))

(define (extend-char-right-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_right))))

;; h
(define (extend-char-left-same-line)
  (define count (editor-count))
  (do-n-times count extend-char-left-same-line-impl))

(define (extend-char-left-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_left))))

;; k
(define (extend-line-up)
  (helix.static.extend_line_up)
  (extend-line-up-impl)
  (when (is-visual-line-mode?)
    (helix.static.extend_to_line_bounds)))

(define (extend-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (extend-char-left-same-line-impl))))

;; j
(define (extend-line-down)
  (helix.static.extend_line_down)
  (extend-line-down-impl)
  (when (is-visual-line-mode?)
    (helix.static.extend_to_line_bounds)))

(define (extend-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (extend-char-left-same-line-impl))))

;; w
(define (vim-extend-next-word-start)
  (define count (editor-count))
  (do-n-times count vim-extend-next-word-start-impl))

(define (vim-extend-next-word-start-impl)
  (get-next-word-start extend-right-n))

;; W
(define (vim-extend-next-long-word-start)
  (define count (editor-count))
  (do-n-times count vim-extend-next-long-word-start-impl))

(define (vim-extend-next-long-word-start-impl)
  (get-next-long-word-start extend-right-n))

;; { (select)
(define (vim-extend-to-prev-paragraph)
  (helix.static.extend_to_line_bounds)
  (helix.static.goto_prev_paragraph))

;; } (select)
(define (vim-extend-to-next-paragraph)
  (helix.static.extend_to_line_bounds)
  (helix.static.goto_next_paragraph))

(define (select-around-impl key)
  (helix.static.select_textobject_around)
  (trigger-on-key-callback key))

(define (select-inner-impl key)
  (helix.static.select_textobject_inner)
  (trigger-on-key-callback key))

;; vaw
(define (select-around-word)
  (select-around-impl w-key))

;; viw
(define (select-inner-word)
  (select-inner-impl w-key))

;; vap
(define (select-around-paragraph)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))

  ;; Find paragraph boundaries
  (define para-start-line (find-paragraph-start rope cur-line))
  (define para-end-line (find-paragraph-end rope cur-line))

  ;; Find end of trailing blank lines
  (define blank-end-line (find-blank-lines-end rope (+ para-end-line 1)))

  ;; Use the blank line end if there are trailing blanks, otherwise use paragraph end
  (define actual-end-line (if (> blank-end-line para-end-line) blank-end-line para-end-line))

  ;; Convert line numbers to character positions
  (define start-char (rope-line->char rope para-start-line))
  (define end-line-start (rope-line->char rope actual-end-line))

  ;; Get the end of the last line
  (define end-line-rope (rope->line rope actual-end-line))
  (define end-line-len (rope-len-chars end-line-rope))
  (define end-char (+ end-line-start end-line-len))

  ;; Move to start and extend to end
  (move-to-position start-char)
  (extend-to-position (- end-char 1)))

;; vip
(define (select-inner-paragraph)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))

  ;; Find paragraph boundaries
  (define para-start-line (find-paragraph-start rope cur-line))
  (define para-end-line (find-paragraph-end rope cur-line))

  ;; Convert line numbers to character positions
  (define start-char (rope-line->char rope para-start-line))
  (define end-line-start (rope-line->char rope para-end-line))

  ;; Get the end of the last line in the paragraph
  (define end-line-rope (rope->line rope para-end-line))
  (define end-line-len (rope-len-chars end-line-rope))
  (define end-char (+ end-line-start end-line-len))

  ;; Move to start and extend to end
  (move-to-position start-char)
  (extend-to-position (- end-char 1)))

;; vaf
(define (select-around-function)
  (select-around-impl f-key))

;; vif
(define (select-inner-function)
  (select-inner-impl f-key))

;; vac
(define (select-around-comment)
  (select-around-impl c-key))

;; vic
(define (select-inner-comment)
  (select-inner-impl c-key))

;; vae
(define (select-around-data-structure)
  (select-around-impl e-key))

;; vie
(define (select-inner-data-structure)
  (select-inner-impl e-key))

;; vax
(define (select-around-html-tag)
  (select-around-impl x-key))

;; vix
(define (select-inner-html-tag)
  (select-inner-impl x-key))

;; vit
(define (select-around-type-definition)
  (select-around-impl t-key))

;; vit
(define (select-inner-type-definition)
  (select-inner-impl t-key))

;; vaT
(define (select-around-test)
  (select-around-impl T-key))

;; viT
(define (select-inner-test)
  (select-inner-impl T-key))

;; vi{
;; vi[
;; vi(
;; vi"
;; vi'
;; vi<
(define (select-inner-bracket open-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))

  (let ([pair (find-bracket-pair rope cur-pos open-ch)])
    (when pair
      (let ([open-pos (car pair)]
            [close-pos (cdr pair)])
        ;; Determine which is opening and which is closing
        (if (< open-pos close-pos)
            (begin
              ;; Move to position after opening bracket
              (move-to-position (+ open-pos 1))
              ;; Extend to position before closing bracket
              (extend-to-position (- close-pos 1)))
            (begin
              ;; Reversed - match_brackets put us on closing bracket
              (move-to-position (+ close-pos 1))
              (extend-to-position (- open-pos 1))))))))

;; Select around bracket - enhanced with forward search
;; va{
;; va[
;; va(
;; va"
;; va'
;; va<
(define (select-around-bracket open-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))

  (let ([pair (find-bracket-pair rope cur-pos open-ch)])
    (when pair
      (let ([open-pos (car pair)]
            [close-pos (cdr pair)])
        ;; Determine which is opening and which is closing
        (if (< open-pos close-pos)
            (begin
              (move-to-position open-pos)
              (extend-to-position close-pos))
            (begin
              (move-to-position close-pos)
              (extend-to-position open-pos)))))))

(define (select-inner-quote quote-ch)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))

  (helix.static.select_textobject_inner)
  (trigger-on-key-callback (string->key-event (string quote-ch)))

  ;; If didn't work, search forward
  (when (= start-pos (cursor-position))
    (let loop ([i 1])
      (define pos (+ start-pos i))
      (when (< pos (rope-len-chars rope))
        (let ([ch (rope-char-ref rope pos)])
          (if (char=? ch quote-ch)
              (begin
                (move-right-n (+ i 1)) ; Move past the quote
                (helix.static.select_textobject_inner)
                (trigger-on-key-callback (string->key-event (string quote-ch))))
              (loop (+ i 1))))))))

(define (select-around-quote quote-ch)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))

  (helix.static.select_textobject_around)
  (trigger-on-key-callback (string->key-event (string quote-ch)))

  ;; If didn't work, search forward
  (when (= start-pos (cursor-position))
    (let loop ([i 1])
      (define pos (+ start-pos i))
      (when (< pos (rope-len-chars rope))
        (let ([ch (rope-char-ref rope pos)])
          (if (char=? ch quote-ch)
              (begin
                (move-right-n (+ i 1)) ; Move past the quote
                (helix.static.select_textobject_around)
                (trigger-on-key-callback (string->key-event (string quote-ch))))
              (loop (+ i 1))))))))

;; Public API functions
(define (select-inner-curly)
  (select-inner-bracket #\{))

(define (select-around-curly)
  (select-around-bracket #\{))

(define (select-inner-paren)
  (select-inner-bracket #\())

(define (select-around-paren)
  (select-around-bracket #\())

(define (select-inner-square)
  (select-inner-bracket #\[))

(define (select-around-square)
  (select-around-bracket #\[))

(define (select-inner-double-quote)
  (select-inner-quote #\"))

(define (select-around-double-quote)
  (select-around-quote #\"))

(define (select-inner-single-quote)
  (select-inner-quote #\'))

(define (select-around-single-quote)
  (select-around-quote #\'))

(define (select-inner-arrow)
  (select-inner-bracket #\<))

(define (select-around-arrow)
  (select-around-bracket #\<))

;; f(char)
(define (select-find-next-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-next-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\f char)))))))

(define (select-find-next-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (extend-right-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; F(char)
(define (select-find-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\F char)))))))

(define (select-find-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (extend-left-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; t(char)
(define (select-find-till-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-till-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\t char)))))))

(define (select-find-till-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (extend-right-n i)
       (helix.static.extend_char_left)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (extend-right-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; T(char)
(define (select-till-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-till-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\T char)))))))

(define (select-till-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (extend-left-n i)
       (helix.static.extend_char_right)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (extend-left-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; ,
(define (select-repeat-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (select-find-next-char-impl char count)]
    [(equal? action #\F) (select-find-prev-char-impl char count)]
    [(equal? action #\t) (select-find-till-char-impl char count)]
    [(equal? action #\T) (select-till-prev-char-impl char count)]))

;; ;
(define (select-reverse-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (select-find-prev-char-impl char count)]
    [(equal? action #\F) (select-find-next-char-impl char count)]
    [(equal? action #\t) (select-till-prev-char-impl char count)]
    [(equal? action #\T) (select-find-till-char-impl char count)]))

(define (exit-visual-line-mode)
  (when is-visual-line-mode?
    (set-visual-line-mode! #f))
  (helix.static.collapse_selection)
  (helix.static.normal_mode)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (define prev-char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (if (equal? #\newline char)
        ;; i don't want to import normal motions
        (unless (equal? #\newline prev-char)
          (helix.static.move_char_left)))))

(provide extend-char-right-same-line
         extend-char-left-same-line
         extend-line-up
         extend-line-down
         vim-extend-next-word-start
         vim-extend-next-long-word-start
         vim-extend-to-next-paragraph
         vim-extend-to-prev-paragraph
         select-around-word
         select-inner-word
         select-around-paragraph
         select-inner-paragraph
         select-around-function
         select-inner-function
         select-around-comment
         select-inner-comment
         select-around-data-structure
         select-inner-data-structure
         select-around-html-tag
         select-inner-html-tag
         select-around-type-definition
         select-inner-type-definition
         select-around-test
         select-inner-test
         select-inner-curly
         select-around-curly
         select-inner-paren
         select-around-paren
         select-inner-square
         select-around-square
         select-inner-double-quote
         select-around-double-quote
         select-inner-single-quote
         select-around-single-quote
         select-inner-arrow
         select-around-arrow
         select-find-next-char
         select-find-prev-char
         select-find-till-char
         select-till-prev-char
         select-repeat-last-find
         select-reverse-last-find
         exit-visual-line-mode)
