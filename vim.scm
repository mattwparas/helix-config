(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/configuration.scm")
(require "helix/keymaps.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/ext.scm")
(require "helix/components.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

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
                (e ":evil-next-word-end")
                (A-d "no_op")
                (A-c "no_op")
                ;; Selecting the whole file
                (% "match_brackets")
                (X "no_op")
                (A-x "no_op")
                (p "paste_after")
                (P "paste_before")
                ;; TODO: More delete things
                (d (d ":evil-delete-line") (w ":evil-delete-word"))
                ;; TODO: More change things
                (c (c ":evil-change-line"))
                (x "delete_selection_noyank")
                ;; TODO: More yank things
                (y (y ":evil-yank-link") (a (w ":yank-around-word") (i ":yank-inner-word")))
                (b ":evil-prev-word-start")
                (B ":evil-prev-long-word-start")
                (E ":evil-next-long-word-end")
                (W ":evil-next-long-word-start")
                ; (0 "goto_line_start")
                ($ "goto_line_end")
                (^ "goto_first_nonwhitespace")
                (del "delete_selection"))
        ;; Select bindings
        ;; TODO: Rename this to VIS
        (select (a "select_textobject_around")
                (i "select_textobject_inner")
                (h ":extend-char-left-same-line")
                (l ":extend-char-right-same-line")
                (j ":extend-line-down")
                (k ":extend-line-up"))
        (insert (C-d "unindent") (C-t "indent")))

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

(define (extend-char-right-same-line)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_right))))

(define (move-char-left-same-line)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_left))))

(define (extend-char-left-same-line)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_left))))

(define (extend-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (define char-to-left (rope-char-ref doc (- pos 1)))
      (when char-to-left
        (unless (char=? #\newline char-to-left)
          (helix.static.extend_char_left))))))

(define (extend-line-up)
  (helix.static.extend_line_up)
  (extend-line-up-impl))

(define (move-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (define char-to-left (rope-char-ref doc (- pos 1)))
      (when char-to-left
        (unless (char=? #\newline char-to-left)
          (helix.static.move_char_left))))))

(define (move-line-up)
  (helix.static.move_line_up)
  (move-line-up-impl))

(define (extend-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (define char-to-left (rope-char-ref doc (- pos 1)))
      (when char-to-left
        (unless (char=? #\newline char-to-left)
          (helix.static.extend_char_left))))))

(define (extend-line-down)
  (helix.static.extend_line_down)
  (extend-line-down-impl))

(define (move-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (define char-to-left (rope-char-ref doc (- pos 1)))
      (when char-to-left
        (unless (char=? #\newline char-to-left)
          (helix.static.move_char_left))))))

(define (move-line-down)
  (helix.static.move_line_down)
  (move-line-down-impl))

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

(define (evil-next-word-end)
  (helix.static.move_next_word_end)
  (helix.static.collapse_selection))

(define (evil-prev-word-start)
  (helix.static.move_prev_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (evil-prev-word-start)))

(define (evil-prev-long-word-start)
  (helix.static.move_prev_long_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (evil-prev-long-word-start)))

(define (evil-next-long-word-start)
  (define pos (cursor-position))
  (helix.static.extend_next_long_word_start)
  (helix.static.collapse_selection)
  (define doc (get-document-as-slice))
  (define new-pos (cursor-position))
  (when (> (- new-pos pos) 1)
    (helix.static.move_char_right))

  (define cur-char (rope-char-ref doc new-pos))
  (when (and cur-char (char-whitespace? cur-char))
    (evil-next-long-word-start)))

(define (evil-next-long-word-end)
  (helix.static.move_next_long_word_end)
  (helix.static.collapse_selection))

(define (yank-current-line)
  (helix.static.extend_to_line_bounds)
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.normal_mode)
  (helix.static.collapse_selection))

(define w-key (string->key-event "w"))
(define (select-around-word)
  (helix.static.select_textobject_around)
  (trigger-on-key-callback w-key))

(define (select-inner-word)
  (helix.static.select_textobject_inner)
  (trigger-on-key-callback w-key))

;; Emulate a keypress?
(define (yank-around-word)
  (select-around-word)
  (helix.static.move_prev_word_start)
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.collapse_selection))

(define (yank-inner-word)
  (select-inner-word)
  (helix.static.move_prev_word_start)
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.collapse_selection))
