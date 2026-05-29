(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "visual-motions.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

;; TODO: implement for yank commands using custom implementations

(define (yank-impl func)
  (when (func)
    (helix.static.yank_main_selection_to_clipboard)
    (helix.static.flip_selections)
    (helix.static.collapse_selection)))

;; y (select)
(define (vim-yank-selection)
  (yank-impl helix.static.no_op)
  (exit-visual-line-mode))

;; yaw
(define (yank-around-word)
  (yank-impl select-around-word))

;; yiw
(define (yank-inner-word)
  (yank-impl select-inner-word))

;; yw
(define (yank-word)
  (vim-extend-next-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.flip_selections)
  (helix.static.collapse_selection))

;; yW
(define (yank-long-word)
  (vim-extend-next-long-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.flip_selections)
  (helix.static.collapse_selection))

;; yb
(define (yank-prev-word)
  (yank-impl helix.static.extend_prev_word_start))

;; yB
(define (yank-prev-long-word)
  (yank-impl helix.static.extend_prev_long_word_start))

;; y$
(define (yank-line-end)
  (yank-impl helix.static.extend_to_line_end))

;; y^
(define (yank-line-start)
  (yank-impl helix.static.extend_to_line_start))

;; y0
(define (yank-line-start-non-whitespace)
  (yank-impl helix.static.extend_to_first_nonwhitespace))

;; yy
(define (vim-yank-line)
  (define start-pos (cursor-position))
  (define count (editor-count))
  (when (> count 1)
    (set-editor-count! (- count 1))
    (helix.static.extend_line_down)
  )
  (helix.static.extend_to_line_bounds)
  (helix.static.yank_main_selection_to_clipboard)

  ;; Flash the selection briefly (if highlight_selections exists)
  ;; This provides visual feedback
  ;; (when (defined? 'helix.static.highlight_selections)
  ;;   (helix.static.highlight_selections))

  (helix.static.normal_mode)
  (helix.static.collapse_selection)

  (define current-pos (cursor-position))
  (define distance (- start-pos current-pos))
  (cond
    [(> distance 0) (move-right-n distance)]
    [(< distance 0) (move-left-n (- distance))]))

;; yap/yip
(define (yank-around-paragraph)        (yank-impl select-around-paragraph))
(define (yank-inner-paragraph)         (yank-impl select-inner-paragraph))

;; yaf/yif
(define (yank-around-function)         (yank-impl select-around-function))
(define (yank-inner-function)          (yank-impl select-inner-function))

;; yac/yic
(define (yank-around-comment)          (yank-impl select-around-comment))
(define (yank-inner-comment)           (yank-impl select-inner-comment))

;; yae/yie
(define (yank-around-data-structure)   (yank-impl select-around-data-structure))
(define (yank-inner-data-structure)    (yank-impl select-inner-data-structure))

;; yax/yix
(define (yank-around-html-tag)         (yank-impl select-around-html-tag))
(define (yank-inner-html-tag)          (yank-impl select-inner-html-tag))

;; yat/yit
(define (yank-around-type-definition)  (yank-impl select-around-type-definition))
(define (yank-inner-type-definition)   (yank-impl select-inner-type-definition))

;; yaT/yiT
(define (yank-around-test)             (yank-impl select-around-test))
(define (yank-inner-test)              (yank-impl select-inner-test))

;; ya{/yi{
(define (yank-around-curly)            (yank-impl select-around-curly))
(define (yank-inner-curly)             (yank-impl select-inner-curly))

;; ya[/yi[
(define (yank-around-square)           (yank-impl select-around-square))
(define (yank-inner-square)            (yank-impl select-inner-square))

;; ya(/yi(
(define (yank-around-paren)            (yank-impl select-around-paren))
(define (yank-inner-paren)             (yank-impl select-inner-paren))

;; ya"/yi"
(define (yank-around-double-quote)     (yank-impl select-around-double-quote))
(define (yank-inner-double-quote)      (yank-impl select-inner-double-quote))

;; ya'/yi'
(define (yank-around-single-quote)     (yank-impl select-around-single-quote))
(define (yank-inner-single-quote)      (yank-impl select-inner-single-quote))

;; ya</yi<
(define (yank-around-arrow)            (yank-impl select-around-arrow))
(define (yank-inner-arrow)             (yank-impl select-inner-arrow))

;; yaW/yiW
(define (yank-around-long-word)        (yank-impl select-around-long-word))
(define (yank-inner-long-word)         (yank-impl select-inner-long-word))

(provide vim-yank-selection
         yank-around-word
         yank-inner-word
         yank-around-paragraph
         yank-inner-paragraph
         yank-around-function
         yank-inner-function
         yank-around-comment
         yank-inner-comment
         yank-around-data-structure
         yank-inner-data-structure
         yank-around-html-tag
         yank-inner-html-tag
         yank-around-type-definition
         yank-inner-type-definition
         yank-around-test
         yank-inner-test
         yank-around-curly
         yank-inner-curly
         yank-around-square
         yank-inner-square
         yank-around-paren
         yank-inner-paren
         yank-around-double-quote
         yank-inner-double-quote
         yank-around-single-quote
         yank-inner-single-quote
         yank-around-arrow
         yank-inner-arrow
         yank-around-long-word
         yank-inner-long-word
         yank-word
         yank-long-word
         yank-prev-word
         yank-prev-long-word
         yank-line-end
         yank-line-start
         yank-line-start-non-whitespace
         vim-yank-line)
