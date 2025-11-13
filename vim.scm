(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/configuration.scm")
(require "helix/keymaps.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/ext.scm")
(require "helix/components.scm")

(require-builtin helix/core/text)

; "h" | "left" => extend_same_line_char_left,
; "j" | "down" => extend_anchored_visual_line_down,
; "k" | "up" => extend_anchored_visual_line_up,
; "l" | "right" => extend_same_line_char_right,

; "a" => select_textobject_around,
; "i" => select_textobject_inner,

; //"w" => extend_next_word_start,
; //"b" => extend_prev_word_start,
; //"e" => extend_next_word_end,
; //"W" => extend_next_long_word_start,
; //"B" => extend_prev_long_word_start,
; //"E" => extend_next_long_word_end,

; "A-e" => extend_parent_node_end,
; "A-b" => extend_parent_node_start,

; //"n" => extend_search_next,
; //"N" => extend_search_prev,

; //"t" => extend_till_char,
; //"f" => extend_next_char,
; //"T" => extend_till_prev_char,
; //"F" => extend_prev_char,

; "home" => extend_to_line_start,
; "end" => extend_to_line_end,
; "esc" => exit_select_mode,

; "g" => { "Goto"
;     "k" => extend_anchored_line_up,
;     "j" => extend_anchored_line_down,
;     "w" => extend_to_word,
; },

(keymap (global)
        (normal (l ":move-char-right-same-line")
                (h ":move-char-left-same-line")
                (k ":move-line-up")
                (j ":move-line-down")
                (f ":evil-find-next-char")
                (F ":evil-find-prev-char")
                (t ":evil-find-till-char")
                (T ":evil-till-prev-char")
                (a ":evil-append-mode")
                (w ":evil-next-word-start")
                (A-d "no_op")
                (A-c "no_op")
                ;; Selecting the whole file
                (% "no_op")
                (x "no_op")
                (X "no_op")
                (A-x "no_op")
                ; "d" => evil_delete,
                (d (d ":evil-delete-line") (w ":evil-delete-word"))
                ; "c" => evil_change,
                (c (c ":evil-change-line"))
                ; "x" => evil_delete_immediate,
                ; "y" => evil_yank,
                ; "b" => evil_prev_word_start,
                ; "e" => evil_next_word_end,
                ; "w" => evil_next_word_start,
                ; "B" => evil_prev_long_word_start,
                ; "E" => evil_next_long_word_end,
                ; "W" => evil_next_long_word_start,
                (b "no_op")
                (e "no_op")
                (W "no_op")
                (B "no_op")
                (E "no_op"))
        (select (a "select_textobject_around") (i "select_textobject_inner")))

(define (evil-append-mode)
  ;; Move to insert mode
  (helix.static.insert_mode)
  (helix.static.collapse_selection)
  (helix.static.move_char_right))

(define (get-document-as-slice)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (editor->text focus-doc-id)))

(define (move-char-right-same-line)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_right))))

(define (move-char-left-same-line)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_left))))

(define (move-line-up)
  (helix.static.move_line_up)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (when (equal? #\newline char)
      (define char-to-left (rope-char-ref (get-document-as-slice) (- pos 1)))
      (when char-to-left
        (unless (equal? #\newline char-to-left)
          (helix.static.move_char_left))))))

(define (move-line-down)
  (helix.static.move_line_down)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (when (equal? #\newline char)
      (define char-to-left (rope-char-ref (get-document-as-slice) (- pos 1)))
      (when char-to-left
        (unless (equal? #\newline char-to-left)
          (helix.static.move_char_left))))))

(define (do-n-times n func)
  (if (= n 0)
      void
      (begin
        (func)
        (do-n-times (- n 1) func))))

(define (evil-find-next-char)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (do-n-times i helix.static.move_char_right)]
      [else (loop (+ i 1) next-char)]))

  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (loop 1 char)))))

(define (evil-find-prev-char)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (do-n-times i helix.static.move_char_left)]
      [else (loop (+ i 1) next-char)]))

  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (loop 1 char)))))

(define (evil-find-till-char)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (do-n-times i helix.static.move_char_right)
       (helix.static.move_char_left)]
      [else (loop (+ i 1) next-char)]))

  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (loop 0 char)))))

(define (evil-till-prev-char)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (do-n-times i helix.static.move_char_left)
       (helix.static.move_char_right)]
      [else (loop (+ i 1) next-char)]))

  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (loop 0 char)))))

;; TODO:
;; line wise vs character wise visual mode

(define (evil-delete-line)
  (helix.static.extend_to_line_bounds)
  (helix.static.delete_selection))

(define (evil-change-line)
  (evil-delete-line)
  (helix.static.move_line_up)
  (helix.static.open_below)
  (helix.static.goto_line_start))

(define (evil-delete-word)
  (define pos (cursor-position))
  (helix.static.extend_next_word_start)
  (define new-pos (cursor-position))
  (when (> (- new-pos pos) 1)
    (helix.static.extend_char_right))
  (helix.static.delete_selection))

;; TODO: Move the cursor to the next word.
;; If we've moved only one, don't move back one
;; Otherwise, move back one character.
(define (evil-next-word-start)
  (define pos (cursor-position))
  (helix.static.extend_next_word_start)
  (helix.static.collapse_selection)

  (define new-pos (cursor-position))

  (when (> (- new-pos pos) 1)
    (helix.static.move_char_right)))
