(require "../prelude.scm"
         (for-syntax "../prelude.scm"))
(require-helix)

;; Book keeping for keymaps
(require (only-in "keymaps.scm" *reverse-buffer-map-insert*))

(provide make-new-labelled-buffer!
         temporarily-switch-focus
         open-or-switch-focus
         currently-in-labelled-buffer?
         open-labelled-buffer
         maybe-fetch-doc-id
         fetch-doc-id)

;; Temporary buffer map, key -> doc id
(define *temporary-buffer-map* (hash))
;; Last focused - will allow us to swap between the last view we were at
(define *last-focus* 'uninitialized)

;; Mark the last focused document, so that we can return to it
(define (mark-last-focused! cx)
  (let* ([editor (cx-editor! cx)] [focus (editor-focus editor)])
    (set! *last-focus* focus)
    focus))

;; TODO: These appear to be the same function
(define (currently-focused cx)
  (~> cx (cx-editor!) (editor-focus)))

;; Grab whatever we're currently focused on
(define (get-current-focus cx)
  (~> cx (cx-editor!) (editor-focus)))

;; Get the current document id
(define (get-current-doc-id cx)
  (let* ([editor (cx-editor! cx)] [focus (editor-focus editor)]) (editor->doc-id editor focus)))

;;@doc
;; Attempts to find the doc id associated with the given key, returns #false if
;; the key doesn't exist
(define (maybe-fetch-doc-id key)
  (hash-try-get *temporary-buffer-map* key))

;;@doc
;; Attempts to find the doc id associated with the given key, errors if
;; the key does not exist
(define (fetch-doc-id key)
  (hash-get *temporary-buffer-map* key))

;;@doc
;; Creates a new labelled buffer that can be access by the key `label`.
;; Optionally sets the language type if provided
(define (make-new-labelled-buffer! cx
                                   #:label label
                                   #:language-type (language-type void)
                                   #:side (side 'right))

  ;; Save our last state to return to it afterwards
  (define last-focused (currently-focused cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))

  ;; Open up the new labelled buffer in a vertical split, set the language accordingly
  ;; if it has been passed in
  (helix.vsplit-new cx '() helix.PromptEvent::Validate)

  ;; Label this buffer - it will now show up instead of `[scratch]`
  (set-scratch-buffer-name! cx (string-append "[" label "]"))

  ; (when (eq? side 'left)
  ;   (helix.static.swap_view_left cx))

  (when language-type
    (helix.set-language cx (list language-type) helix.PromptEvent::Validate))

  ;; Add the document id to our internal mapping.
  (set! *temporary-buffer-map* (hash-insert *temporary-buffer-map* label (get-current-doc-id cx)))

  (*reverse-buffer-map-insert* (doc-id->usize (get-current-doc-id cx)) label)

  ;; Go back to where we were before
  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

;; Switch the focus for the duration of the thunk, and return to where we were previously
(define (temporarily-switch-focus cx thunk)
  (define last-focused (mark-last-focused! cx))
  (define last-mode (~> cx (cx-editor!) (editor-mode)))
  (thunk cx)
  (editor-set-focus! (cx-editor! cx) last-focused)
  (editor-set-mode! (cx-editor! cx) last-mode))

(define (open-or-switch-focus cx document-id)
  (define maybe-view-id? (editor-doc-in-view? (cx-editor! cx) document-id))
  (if maybe-view-id?
      (editor-set-focus! (cx-editor! cx) maybe-view-id?)
      (editor-switch! (cx-editor! cx) document-id)))

(define (open-labelled-buffer cx label)
  (open-or-switch-focus cx (hash-ref *temporary-buffer-map* label)))

(define (currently-in-labelled-buffer? cx label)
  (define requested-label (hash-try-get *temporary-buffer-map* label))
  (equal? (doc-id->usize requested-label) (doc-id->usize (get-current-doc-id cx))))
