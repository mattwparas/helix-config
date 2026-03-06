(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/editor.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

(require "visual-motions.scm")

(define (change-impl func)
  (func)
  (helix.clipboard-yank)
  (helix.static.change_selection))

;; c (select)
(define (vim-change-selection)
  (helix.clipboard-yank)
  (helix.static.change_selection))

;; S
(define (vim-change-line)
  (change-impl helix.static.extend_to_line_bounds))

;; cw
(define (vim-change-word)
  (change-impl helix.static.extend_next_word_end))

;; cW
(define (vim-change-long-word)
  (change-impl helix.static.extend_next_long_word_end))

;; cb
(define (vim-change-prev-word)
  (change-impl helix.static.extend_prev_word_start))

;; cB
(define (vim-change-prev-long-word)
  (change-impl helix.static.extend_prev_long_word_start))

;; c$
(define (vim-change-line-end)
  (change-impl helix.static.extend_to_line_end))

;; c^
(define (vim-change-line-start)
  (change-impl helix.static.extend_to_line_start))

;; ce
(define (vim-change-word-end)
  (change-impl helix.static.extend_next_word_end))

;; cE
(define (vim-change-long-word-end)
  (change-impl helix.static.extend_next_long_word_end))

;; caw
(define (vim-change-around-word)
  (change-impl select-around-word))

;; ciw
(define (vim-change-inner-word)
  (change-impl select-inner-word))

;; TODO: finish implementing this
;; caW
;; (define (vim-change-around-long-word)
;;   (select-around-word)
;;   (helix.static.change_selection))

;; ciW
;; (define (vim-change-inner-long-word)
;;   (select-inner-word)
;;   (helix.static.change_selection))

;; cap
(define (vim-change-around-paragraph)
  (change-impl select-around-paragraph))

;; cip
(define (vim-change-inner-paragraph)
  (change-impl select-inner-paragraph))

;; caf
(define (vim-change-around-function)
  (change-impl select-around-function))

;; cif
(define (vim-change-inner-function)
  (change-impl select-inner-function))

;; cac
(define (vim-change-around-comment)
  (change-impl select-around-comment))

;; cic
(define (vim-change-inner-comment)
  (change-impl select-inner-comment))

;; cae
(define (vim-change-around-data-structure)
  (change-impl select-around-data-structure))

;; cie
(define (vim-change-inner-data-structure)
  (change-impl select-inner-data-structure))

;; cax
(define (vim-change-around-html-tag)
  (change-impl select-around-html-tag))

;; cix
(define (vim-change-inner-html-tag)
  (change-impl select-inner-html-tag))

;; cat
(define (vim-change-around-type-definition)
  (change-impl select-around-type-definition))

;; cit
(define (vim-change-inner-type-definition)
  (change-impl select-inner-type-definition))

;; caT
(define (vim-change-around-test)
  (change-impl select-around-test))

;; ciT
(define (vim-change-inner-test)
  (change-impl select-inner-test))

;; ca{
(define (vim-change-around-curly)
  (change-impl select-around-curly))

;; ci{
(define (vim-change-inner-curly)
  (change-impl select-inner-curly))

;; ca[
(define (vim-change-around-square)
  (change-impl select-around-square))

;; ci[
(define (vim-change-inner-square)
  (change-impl select-inner-square))

;; ci(
(define (vim-change-inner-paren)
  (change-impl select-inner-paren))

;; ca(
(define (vim-change-around-paren)
  (change-impl select-around-paren))

;; ca"
(define (vim-change-around-double-quote)
  (change-impl select-around-double-quote))

;; ci"
(define (vim-change-inner-double-quote)
  (change-impl select-inner-double-quote))

;; ca'
(define (vim-change-around-single-quote)
  (change-impl select-around-single-quote))

;; ci'
(define (vim-change-inner-single-quote)
  (change-impl select-inner-single-quote))

;; ca<
(define (vim-change-around-arrow)
  (change-impl select-around-arrow))

;; ci<
(define (vim-change-inner-arrow)
  (change-impl select-inner-arrow))

(provide vim-change-selection
         vim-change-line
         vim-change-word
         vim-change-long-word
         vim-change-prev-word
         vim-change-prev-long-word
         vim-change-word-end
         vim-change-long-word-end
         vim-change-line-end
         vim-change-line-start
         vim-change-around-word
         vim-change-inner-word
         vim-change-around-paragraph
         vim-change-inner-paragraph
         vim-change-around-function
         vim-change-inner-function
         vim-change-around-comment
         vim-change-inner-comment
         vim-change-around-data-structure
         vim-change-inner-data-structure
         vim-change-around-html-tag
         vim-change-inner-html-tag
         vim-change-around-type-definition
         vim-change-inner-type-definition
         vim-change-around-test
         vim-change-inner-test
         vim-change-around-curly
         vim-change-inner-curly
         vim-change-around-square
         vim-change-inner-square
         vim-change-inner-paren
         vim-change-around-paren
         vim-change-around-double-quote
         vim-change-inner-double-quote
         vim-change-around-single-quote
         vim-change-inner-single-quote
         vim-change-around-arrow
         vim-change-inner-arrow
         vim-change-inner-square
         vim-change-inner-paren
         vim-change-around-paren
         vim-change-around-double-quote
         vim-change-inner-double-quote
         vim-change-around-single-quote
         vim-change-inner-single-quote
         vim-change-around-arrow
         vim-change-inner-arrow)
