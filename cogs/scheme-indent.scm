(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/editor.scm")
(require "helix/misc.scm")

(provide lisp-words
         add-lisp-word!
         scheme-indent)

(define LISP-WORDS
  (hashset "define-syntax"
           "let*"
           "lambda"
           "λ"
           "case"
           "=>"
           "quote-splicing"
           "unquote-splicing"
           "set!"
           "let"
           "letrec"
           "letrec-syntax"
           "let-values"
           "let*-values"
           "do"
           "else"
           "cond"
           "unquote"
           "begin"
           "let-syntax"
           "and"
           "quasiquote"
           "letrec"
           "delay"
           "or"
           "identifier-syntax"
           "assert"
           "library"
           "export"
           "import"
           "rename"
           "only"
           "except"
           "prefix"
           "provide"
           "require"
           "define"
           "cond"
           "if"
           "syntax-rules"
           "when"
           "unless"
           "defmacro"))

(define (lisp-words)
  LISP-WORDS)

(define (add-lisp-word! word)
  (set! LISP-WORDS (hashset-insert LISP-WORDS word)))

; (define-syntax skip-compile
;   (syntax-rules ()
;     [(skip-compile) (begin)]
;     [(skip-compile expr) (begin)]
;     [(skip-compile expr exprs ...)
;      (begin
;        (skip-compile exprs ...))]))

;; INCLUDE ON THE NEXT COMPILATION
; (skip-compile
(require-builtin helix/core/text as text.)
(struct CharIterator (slice offset index) #:mutable)
;; Mutable advance the iterator
(define (CharIterator-next! char-iterator)
  (define return-value
    (text.rope-char-ref (CharIterator-slice char-iterator) (CharIterator-offset char-iterator)))
  (if (char? return-value)
      (begin
        (set-CharIterator-offset! char-iterator (+ (CharIterator-offset char-iterator) 1))
        (set-CharIterator-index! char-iterator (+ (CharIterator-index char-iterator) 1))
        ; (log::info! (to-string return-value))
        return-value)
      return-value))

(define (CharIterator-take-while-whitespace char-iterator count)
  (define char (CharIterator-next! char-iterator))
  (if (and (char? char) (char-whitespace? char))
      (CharIterator-take-while-whitespace char-iterator (+ 1 count))
      count))

;; TODO: Need to double check that the parens don't exist within a character declaration
;; or within a string
(define (close-paren? char)
  (or (equal? char #\)) (equal? char #\]) (equal? char #\})))

(define (open-paren? char)
  (or (equal? char #\() (equal? char #\[) (equal? char #\{)))

(define (string-repeat str count)
  (apply string-append (map (lambda (_) " ") (range 0 count))))
;; Crunch the slice in reverse, keeping track of the depth, and the index (offset) that we're at. Since we're doing it
;; in reverse, the _index-complement is just to save us some funny business of math
(define (walk-backwards line depth _index-complement index)
  (define char (text.rope-char-ref line _index-complement))

  ; (log::info!
  ;  (to-string "Calling walk-backwards with char" (text.slice->string line) char _index-complement))

  (cond
    [(close-paren? char) (walk-backwards line (+ depth 1) (- _index-complement 1) (+ index 1))]
    [(open-paren? char)

     (if (equal? depth 0)

         (let ([offset (+ 1 _index-complement)]
               [char-iter-from-paren (CharIterator line _index-complement 0)]
               ;; TODO: Check if this var is used
               [end 'uninitialized])

           ; (log::info! (to-string "Offset is" offset))
           ; (log::info! (to-string "index is" index))

           ;; Walk until we've found whitespace, and then crunch the whitespace until the start of the next symbol
           ;; if there is _no_ symbol after that, we should just default to the default behavior
           (let loop ([index (CharIterator-index char-iter-from-paren)]
                      [char (CharIterator-next! char-iter-from-paren)])
             (cond
               ;; We have whitespace, we've finished crunching the first word.
               [(and (char? char) (char-whitespace? char))
                (begin
                  ;; This is the end of our range
                  (let ([last index])
                    ; (log::info! (to-string "Inner crunching index: " index))
                    ; (log::info! (text.slice->string (text.slice->slice line offset (+ offset index))))

                    (cond

                      ;; If we have multiple parens in a row, match to the start:
                      ;; for instance, (cond [(equal? x 10) RET])
                      ;;                     ^ We want to line up to this
                      ;;
                      ;; To do so, just create an indent that is the width of the offset.
                      ; (log::info! "Found an open paren")
                      [(open-paren? (text.rope-char-ref line offset)) (string-repeat " " offset)]
                      [(hashset-contains?
                        LISP-WORDS
                        ;; TODO: Figure out these strange off by one errors...
                        (~> line (text.rope->slice offset (+ offset index -1)) (text.rope->string)))

                       ; (log::info! "Found a Lisp-Word!")

                       (string-repeat " " (+ 1 offset))]
                      [else
                       =>

                       (let ([last (CharIterator-take-while-whitespace char-iter-from-paren index)])
                         ; (log::info! (to-string "Length: " (text.slice-len-chars line)))
                         ; (log::info! (to-string "Last + offset" (+ last offset)))

                         ;; If we have something like (list RET)
                         ;; We want the result to look like:
                         ;; (list
                         ;;   )
                         ;;
                         ;; So we special case the lack of an additional word after
                         ;; the first symbol
                         (if (and (equal? (text.rope-len-chars line) (+ last offset 1))
                                  (char-whitespace? (text.rope-char-ref line (+ last offset))))
                             (string-repeat " " (+ offset 1))
                             (string-repeat " " (+ last offset))))])))]

               ;; We found a character, but it wasn't whitespace. Advance the loop.
               [(char? char)

                (loop (CharIterator-index char-iter-from-paren)
                      (CharIterator-next! char-iter-from-paren))]
               ;; We ran out of characters in the iterator. Fallback and add 1 to the offset
               [else
                =>
                (string-repeat " " (+ offset 1))])))

         (walk-backwards line (- depth 1) (- _index-complement 1) (+ index 1)))]

    [(char? char) (walk-backwards line depth (- _index-complement 1) (+ index 1))]

    ;; If we haven't found a match, return the depth
    [else
     =>
     depth]))

(define (indent-loop text-up-to-cursor cursor depth)

  ;; Current line that we're iterating over
  (define line (text.rope->line text-up-to-cursor cursor))

  ; (log::info! (to-string "Calling indent loop at line: " (text.slice->string line)))

  (if (text.rope-starts-with? (text.rope-trim-start line) ";")
      (if (equal? cursor 0)
          ;; We're at the top
          (begin

            ; (log::info! "Cursor at the top, returning empty indent")

            "")
          ;; Go forward a line
          (indent-loop text-up-to-cursor (- cursor 1) depth))

      ;; TODO: slice-len-chars might need to be (- (text.slice-len-chars line) 1)
      (let ([result (walk-backwards line depth (- (text.rope-len-chars line) 1) 0)])

        (cond
          ;; We found a string, meaning we have our indent. Return that directly
          [(string? result) result]
          ;; We're at the top, just return an empty string
          ; (log::info! "Cursor at the top, returning empty indent")
          [(equal? cursor 0) ""]

          [(int? result) (indent-loop text-up-to-cursor (- cursor 1) result)]
          [else
           =>
           (error! "Unknown result type for the branch")]))))

; (provide scheme-indent)
(define (scheme-indent-impl text line-before line-before-end-pos)
  (define byte-pos (text.rope-char->byte text line-before-end-pos))
  (define text-up-to-cursor (text.rope->byte-slice text 0 byte-pos))
  (indent-loop text-up-to-cursor line-before 0))

; )

(define (editor-get-doc-if-exists doc-id)
  (if (editor-doc-exists? doc-id) (editor->get-document doc-id) #f))

(define (get-document-as-slice)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)]
         [document (editor-get-doc-if-exists focus-doc-id)])
    (text.document->slice document)))

;;@doc
;; Override the scheme indents with a custom indentation system
(define (scheme-indent)
  ;; This is our rope
  (define rope (get-document-as-slice))
  (define current-line (helix.static.get-current-line-number))
  (define pos (hx.cx->pos))

  ;; This will call the function in the right spot!
  (hx.custom-insert-newline (scheme-indent-impl rope current-line pos)))
