(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin helix/core/text)

(define (get-document-as-slice)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (editor->text focus-doc-id)))

(define (rope-char-at rope pos)
  (if (and (>= pos 0) (< pos (rope-len-chars rope)))
      (rope-char-ref rope pos)
      #f))

(define (is-whitespace? ch)
  (and ch (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\newline) (char=? ch #\return))))

(define (is-alphabetic? ch)
  (and ch (or (and (char>=? ch #\a) (char<=? ch #\z)) (and (char>=? ch #\A) (char<=? ch #\Z)))))

(define (is-numeric? ch)
  (and ch (char>=? ch #\0) (char<=? ch #\9)))

(define (is-word-char? ch)
  (and ch (or (is-alphabetic? ch) (is-numeric? ch) (char=? ch #\_))))

(define (is-punctuation? ch)
  (and ch (not (is-whitespace? ch)) (not (is-word-char? ch))))

(define (is-bracket? ch)
  (and ch
       (or (char=? ch #\{)
           (char=? ch #\})
           (char=? ch #\()
           (char=? ch #\))
           (char=? ch #\[)
           (char=? ch #\])
           (char=? ch #\")
           (char=? ch #\")
           (char=? ch #\')
           (char=? ch #\')
           (char=? ch #\<)
           (char=? ch #\>))))

(define (get-selection-range)
  ;; Try to get selection info from editor
  ;; This may need adjustment based on actual Steel API
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    ;; If there's an API like editor->selection-range, use it
    ;; For now, we'll use a workaround
    #f))

;; Helper to check if we have a real selection (more than 1 char)
(define (has-real-selection? start-pos)
  (define end-pos (cursor-position))
  ;; If cursor moved, we have a selection
  (not (= start-pos end-pos)))

(define (move-to-position target-pos)
  (define current-pos (cursor-position))
  (define distance (- target-pos current-pos))
  (cond
    [(> distance 0) (move-right-n distance)]
    [(< distance 0) (move-left-n (- distance))]))

(define (extend-to-position target-pos)
  (define current-pos (cursor-position))
  (define distance (- target-pos current-pos))
  (cond
    [(> distance 0)
     (let loop ([n distance])
       (when (> n 0)
         (helix.static.extend_char_right)
         (loop (- n 1))))]
    [(< distance 0)
     (let loop ([n (- distance)])
       (when (> n 0)
         (helix.static.extend_char_left)
         (loop (- n 1))))]))

(define (skip-whitespace-forward rope)
  (let ([ch (rope-char-at rope (cursor-position))])
    (when (is-whitespace? ch)
      (helix.static.move_char_right)
      (skip-whitespace-forward rope))))

;;@doc
;; moves left n times and sets editor count to 1
(define (move-left-n n)
  (set-editor-count! 1)
  (when (> n 0)
    (helix.static.move_char_left)
    (move-left-n (- n 1))))

;;@doc
;; moves right n times and sets editor count to 1
(define (move-right-n n)
  (set-editor-count! 1)
  (when (> n 0)
    (helix.static.move_char_right)
    (move-right-n (- n 1))))

;;@doc
;; extends left n times and sets editor count to 1
(define (extend-left-n n)
  (set-editor-count! 1)
  (when (> n 0)
    (helix.static.extend_char_left)
    (extend-left-n (- n 1))))

;;@doc
;; extends right n times and sets editor count to 1
(define (extend-right-n n)
  (set-editor-count! 1)
  (when (> n 0)
    (helix.static.extend_char_right)
    (extend-right-n (- n 1))))

(define (do-n-times n func)
  (if (= n 0)
      void
      (begin
        (func)
        (do-n-times (- n 1) func))))

(define (move-to-char target-ch)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))
  (define len (rope-len-chars rope))

  (let loop ([i 1])
    (define pos (+ start-pos i))
    (cond
      [(>= pos len) #f]
      [else
       (let ([ch (rope-char-ref rope pos)])
         (cond
           [(char=? ch target-ch)
            ;; Found it - move cursor there
            (move-right-n i)
            #t]
           [else (loop (+ i 1))]))])))

(define *visual-line-mode-box* (box #f))

(define (set-visual-line-mode! val)
  (set-box! *visual-line-mode-box* val))

(define (is-visual-line-mode?)
  (unbox *visual-line-mode-box*))

(define (string-blank? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(is-whitespace? (string-ref str i)) (loop (+ i 1))]
      [else #f])))

(define (line-blank? rope line-idx)
  (define line (rope->line rope line-idx))
  (define line-str (rope->string line))
  (string-blank? line-str))

(define (find-paragraph-start rope cur-line)
  (let loop ([line-idx cur-line])
    (cond
      [(<= line-idx 0) 0]
      [(line-blank? rope line-idx) (loop (- line-idx 1))]
      [(line-blank? rope (- line-idx 1)) line-idx]
      [else (loop (- line-idx 1))])))

(define (find-paragraph-end rope cur-line)
  (define total-lines (rope-len-lines rope))
  (let loop ([line-idx cur-line])
    (cond
      ;; At end of file
      [(>= line-idx (- total-lines 1)) (- total-lines 1)]
      ;; Current line is blank, move down
      [(line-blank? rope line-idx) (loop (+ line-idx 1))]
      ;; Next line is blank, we're at end of paragraph
      [(line-blank? rope (+ line-idx 1)) line-idx]
      ;; Next line is not blank, keep going down
      [else (loop (+ line-idx 1))])))

(define (find-blank-lines-end rope start-line)
  (define total-lines (rope-len-lines rope))
  (let loop ([line-idx start-line])
    (cond
      ;; At end of file
      [(>= line-idx (- total-lines 1)) (- total-lines 1)]
      ;; Current line is blank, keep going
      [(line-blank? rope line-idx) (loop (+ line-idx 1))]
      ;; Found non-blank line, return previous line
      [else (- line-idx 1)])))

(define bracket-pairs '((#\{ . #\}) (#\( . #\)) (#\[ . #\]) (#\< . #\>)))

;; Bracket characters we support
(define bracket-chars '(#\{ #\( #\[ #\<))

;; Check if character is a bracket we care about
(define (is-target-bracket? ch target-ch)
  (char=? ch target-ch))

;; Find enclosing bracket pair using Helix's match_brackets
;; Returns (open-pos . close-pos) or #f
(define (find-enclosing-pair-with-match rope cur-pos target-open-ch)
  (define len (rope-len-chars rope))
  (define cur-char (rope-char-ref rope cur-pos))

  ;; First check if we're already on the target bracket
  (if (is-target-bracket? cur-char target-open-ch)
      (begin
        ;; We're on the bracket - use match_brackets
        (define start-pos (cursor-position))
        (helix.static.match_brackets)
        (define match-pos (cursor-position))

        ;; Move back to original position
        (move-to-position cur-pos)

        ;; Return pair if we found a match
        (if (not (= start-pos match-pos))
            (if (< start-pos match-pos)
                (cons start-pos match-pos)
                (cons match-pos start-pos))
            #f))
      ;; Not on the bracket, search backward
      (let loop ([pos (- cur-pos 1)])
        (cond
          [(< pos 0) #f]
          [else
           (let ([ch (rope-char-ref rope pos)])
             (if (is-target-bracket? ch target-open-ch)
                 ;; Found the target bracket - check if we're inside its pair
                 (begin
                   ;; Move to this bracket
                   (move-to-position pos)
                   (define bracket-pos (cursor-position))

                   ;; Use Helix's match_brackets to find the pair
                   (helix.static.match_brackets)
                   (define match-pos (cursor-position))

                   ;; Move back to original position
                   (move-to-position cur-pos)

                   ;; Check if match is after our cursor (meaning we're inside)
                   (if (and (not (= bracket-pos match-pos)) ; Matched something
                            (> match-pos cur-pos)) ; Match is after cursor
                       (cons bracket-pos match-pos) ; Found it!
                       (loop (- pos 1)))) ; Keep searching
                 ;; Not the target bracket, keep searching
                 (loop (- pos 1))))]))))

(define (find-next-bracket rope cur-pos target-ch)
  (define len (rope-len-chars rope))

  (let loop ([pos cur-pos])
    (cond
      [(>= pos len) #f]
      [else
       (let ([ch (rope-char-ref rope pos)])
         (if (char=? ch target-ch)
             pos
             (loop (+ pos 1))))])))

;; Find bracket pair with forward search fallback
(define (find-bracket-pair rope cur-pos target-open-ch)
  ;; First try to find enclosing pair
  (let ([enclosing (find-enclosing-pair-with-match rope cur-pos target-open-ch)])
    (if enclosing
        enclosing
        ;; Not inside pair - search forward and check
        (let ([next-pos (find-next-bracket rope cur-pos target-open-ch)])
          (if next-pos
              (begin
                ;; Move to the bracket
                (move-to-position next-pos)
                (define bracket-pos (cursor-position))

                ;; Use match_brackets to find pair
                (helix.static.match_brackets)
                (define match-pos (cursor-position))

                ;; Move back to original
                (move-to-position cur-pos)

                ;; Return pair if we found a match
                (if (not (= bracket-pos match-pos))
                    (cons bracket-pos match-pos)
                    #f))
              #f)))))

;; Get matching closing bracket
(define (get-closing-bracket open-ch)
  (let ([pair (assoc open-ch bracket-pairs)])
    (if pair
        (cdr pair)
        #f)))

;; Get matching opening bracket
(define (get-opening-bracket close-ch)
  (let loop ([pairs bracket-pairs])
    (cond
      [(null? pairs) #f]
      [(char=? (cdar pairs) close-ch) (caar pairs)]
      [else (loop (cdr pairs))])))

;; Check if character is an opening bracket
(define (is-opening-bracket? ch)
  (and ch (assoc ch bracket-pairs)))

;; Check if character is a closing bracket
(define (is-closing-bracket? ch)
  (and ch (get-opening-bracket ch)))

;; Find matching closing bracket position, returns #f if not found
;; Handles nested brackets correctly
(define (find-matching-close rope start-pos open-ch)
  (define close-ch (get-closing-bracket open-ch))
  (define len (rope-len-chars rope))

  (displayln (string-append "find-matching-close from pos " (number->string start-pos)))

  (let loop ([pos (+ start-pos 1)]
             [depth 1]
             [iter 0])
    (cond
      [(>= pos len)
       (displayln "Reached end of file, no match found")
       #f]
      [(> iter 1000)
       (displayln "Too many iterations, stopping")
       #f]
      [else
       (let ([ch (rope-char-ref rope pos)])
         (when (and (< iter 20) (or (char=? ch open-ch) (char=? ch close-ch)))
           (displayln (string-append "  pos "
                                     (number->string pos)
                                     ": "
                                     (string ch)
                                     " depth="
                                     (number->string depth))))
         (cond
           [(char=? ch open-ch) (loop (+ pos 1) (+ depth 1) (+ iter 1))]
           [(char=? ch close-ch)
            (if (= depth 1)
                (begin
                  (displayln (string-append "Found matching close at " (number->string pos)))
                  pos)
                (loop (+ pos 1) (- depth 1) (+ iter 1)))]
           [else (loop (+ pos 1) depth (+ iter 1))]))])))

;; Find matching opening bracket position, returns #f if not found
;; Handles nested brackets correctly
(define (find-matching-open rope start-pos close-ch)
  (define open-ch (get-opening-bracket close-ch))

  (let loop ([pos (- start-pos 1)]
             [depth 1])
    (cond
      [(< pos 0) #f]
      [else
       (let ([ch (rope-char-ref rope pos)])
         (cond
           [(char=? ch close-ch) (loop (- pos 1) (+ depth 1))]
           [(char=? ch open-ch)
            (if (= depth 1)
                pos
                (loop (- pos 1) (- depth 1)))]
           [else (loop (- pos 1) depth)]))])))

;; Check if cursor is inside a bracket pair
;; Returns (open-pos . close-pos) or #f
(define (find-enclosing-pair rope cur-pos open-ch)
  ;; Search backwards for opening bracket
  (let loop-back ([pos (- cur-pos 1)])
    (cond
      [(< pos 0) #f]
      [else
       (let ([ch (rope-char-ref rope pos)])
         (cond
           ;; Found potential opening bracket
           [(char=? ch open-ch)
            (let ([match-pos (find-matching-close rope pos open-ch)])
              (if (and match-pos (>= match-pos cur-pos))
                  ;; Found enclosing pair
                  (cons pos match-pos)
                  ;; Keep searching backwards
                  (loop-back (- pos 1))))]
           [else (loop-back (- pos 1))]))])))

(define (get-next-word-start func)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))
  (define len (rope-len-chars rope))

  (define cur-char (rope-char-at rope start-pos))
  (define on-whitespace (is-whitespace? cur-char))
  (define on-word (is-word-char? cur-char))
  (define on-punct (is-punctuation? cur-char))

  (define (skip-whitespace pos)
    (let loop ([p pos])
      (let ([ch (rope-char-at rope p)])
        (cond
          [(>= p len) len]
          [(is-whitespace? ch) (loop (+ p 1))]
          [else p]))))

  (define (find-next-word-start pos)
    (cond
      [(>= pos len) len]

      ;; On whitespace: skip to first non-whitespace
      [on-whitespace (skip-whitespace pos)]

      ;; On word char: skip word chars, then skip whitespace
      [on-word
       (let* ([after-word (let loop ([p pos])
                            (let ([ch (rope-char-at rope p)])
                              (cond
                                [(>= p len) len]
                                [(is-word-char? ch) (loop (+ p 1))]
                                [else p])))]
              [after-space (skip-whitespace after-word)])
         after-space)]

      ;; On punctuation: skip punctuation, then skip whitespace
      [on-punct
       (let* ([after-punct (let loop ([p pos])
                             (let ([ch (rope-char-at rope p)])
                               (cond
                                 [(>= p len) len]
                                 [(is-punctuation? ch) (loop (+ p 1))]
                                 [else p])))]
              [after-space (skip-whitespace after-punct)])
         after-space)]

      [else pos]))

  (define target-pos (find-next-word-start start-pos))
  (when (> target-pos start-pos)
    (func (- target-pos start-pos))))

;; TODO: try to move helper functions out of above and below functions
; if possible

(define (get-next-long-word-start func)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))
  (define len (rope-len-chars rope))

  (define cur-char (rope-char-at rope start-pos))
  (define on-whitespace (is-whitespace? cur-char))

  (define (skip-whitespace pos)
    (let loop ([p pos])
      (let ([ch (rope-char-at rope p)])
        (cond
          [(>= p len) len]
          ;; Stop if we hit a newline (empty line boundary)
          [(and (is-whitespace? ch) (not (char=? ch #\newline))) (loop (+ p 1))]
          [(char=? ch #\newline)
           ;; Move past the newline, but stop if next line is empty or has content
           (let ([next-pos (+ p 1)])
             (if (>= next-pos len)
                 len
                 (let ([next-ch (rope-char-at rope next-pos)])
                   (if (char=? next-ch #\newline)
                       ;; Empty line - stop here
                       next-pos
                       ;; Continue skipping whitespace on this line
                       (if (and next-ch (is-whitespace? next-ch) (not (char=? next-ch #\newline)))
                           (loop next-pos)
                           next-pos)))))]
          [else p]))))

  (define (skip-non-whitespace pos)
    (let loop ([p pos])
      (let ([ch (rope-char-at rope p)])
        (cond
          [(>= p len) len]
          [(is-whitespace? ch) p]
          [else (loop (+ p 1))]))))

  (define (find-next-word-start pos)
    (cond
      [(>= pos len) len]

      ;; On whitespace: skip to first non-whitespace
      [on-whitespace (skip-whitespace pos)]

      ;; On non-whitespace: skip to end, then skip whitespace
      [else
       (let* ([after-word (skip-non-whitespace pos)]
              [after-space (skip-whitespace after-word)])
         after-space)]))

  (define target-pos (find-next-word-start start-pos))
  (when (> target-pos start-pos)
    (func (- target-pos start-pos))))

(provide get-document-as-slice
         rope-char-at
         is-whitespace?
         is-alphabetic?
         is-numeric?
         is-word-char?
         is-punctuation?
         skip-whitespace-forward
         move-left-n
         move-right-n
         extend-left-n
         extend-right-n
         do-n-times
         is-bracket?
         get-selection-range
         has-real-selection?
         move-to-position
         move-to-char
         set-visual-line-mode!
         is-visual-line-mode?
         extend-to-position
         string-blank?
         line-blank?
         find-paragraph-start
         find-paragraph-end
         find-blank-lines-end
         bracket-pairs
         get-closing-bracket
         get-opening-bracket
         is-opening-bracket?
         is-closing-bracket?
         find-matching-close
         find-matching-open
         find-enclosing-pair
         find-next-bracket
         find-bracket-pair
         get-next-word-start
         get-next-long-word-start)
