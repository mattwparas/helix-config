(require "../prelude.scm"
         (for-syntax "../prelude.scm"))

(require-helix)

;; labelled buffers ->
(require (only-in "labelled-buffers.scm"
                  make-new-labelled-buffer!
                  temporarily-switch-focus
                  open-or-switch-focus
                  currently-in-labelled-buffer?
                  open-labelled-buffer
                  maybe-fetch-doc-id
                  fetch-doc-id))

(define GS-PICKER "github.com/mattwparas/helix-config/git-status-picker")

(provide create-gs-picker
         add-modified-file)

;;@doc
;; Opens the git status picker in a fresh (or reused) buffer
(define (create-gs-picker cx)

  ;; The doc id, or #false if it is not in the map
  (define doc-id (maybe-fetch-doc-id GS-PICKER))

  (unless doc-id
    (make-new-labelled-buffer! cx #:label GS-PICKER #:side 'left))

  (unless (~> cx (cx-editor!) (editor-doc-exists? (fetch-doc-id GS-PICKER)))
    (make-new-labelled-buffer! cx #:label GS-PICKER #:side 'left))

  ;; Run git status in another window, check what it does
  (temporarily-switch-focus
   cx
   (lambda (cx)
     (open-labelled-buffer cx GS-PICKER)
     (helix.static.select_all cx)
     (helix.static.delete_selection cx)

     (helix.insert-output cx '("git" "status") helix.PromptEvent::Validate))))

(define (refresh-gs-picker cx)
  (temporarily-switch-focus cx
                            (lambda (cx)
                              (open-labelled-buffer cx GS-PICKER)
                              (update-gs-picker cx))))

(define (update-gs-picker cx)

  (helix.static.select_all cx)
  (helix.static.delete_selection cx)

  (helix.insert-output cx '("git" "status") helix.PromptEvent::Validate))

;;@doc
;; If the file is "modified" - add that file to git
(define (add-modified-file cx)
  (when (currently-in-labelled-buffer? cx GS-PICKER)
    ;; Select the current line
    (helix.static.extend_line_below cx)

    ;; Grab the text from the current selected line
    (let ([line (trim-start (helix.static.current-highlighted-text! cx))])
      (when (starts-with? line "modified:   ")
        (let ([trimmed (trim-end (trim-start-matches line "modified:   "))])

          ;; Run git add for the given file that is modified
          (helix.run-shell-command cx (list "git" "add" trimmed) helix.PromptEvent::Validate)

          ;; TODO: Attempt to enqueue this _after_ the git add command as finished. Right now we're just operating on a naive check
          ;; that the git status has finished
          (enqueue-thread-local-callback-with-delay cx 10 refresh-gs-picker))))))
