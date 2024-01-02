; (require-builtin helix/core/typable as helix.)
; (require-builtin helix/core/static as helix.static.)
; (require-builtin helix/core/editor)

(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/editor.scm")

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
(define (create-gs-picker)

  ;; The doc id, or #false if it is not in the map
  (define doc-id (maybe-fetch-doc-id GS-PICKER))

  (unless doc-id
    (make-new-labelled-buffer! #:label GS-PICKER #:side 'left))

  (unless (editor-doc-exists? (fetch-doc-id GS-PICKER))
    (make-new-labelled-buffer! #:label GS-PICKER #:side 'left))

  ;; Run git status in another window, check what it does
  (temporarily-switch-focus (lambda ()
                              (open-labelled-buffer GS-PICKER)
                              (helix.static.select_all)
                              (helix.static.delete_selection)

                              (helix.insert-output "git" "status"))))

(define (refresh-gs-picker)
  (temporarily-switch-focus (lambda ()
                              (open-labelled-buffer GS-PICKER)
                              (update-gs-picker))))

(define (update-gs-picker)

  (helix.static.select_all)
  (helix.static.delete_selection)

  (helix.insert-output '("git" "status")))

;;@doc
;; If the file is "modified" - add that file to git
(define (add-modified-file)
  (when (currently-in-labelled-buffer? GS-PICKER)
    ;; Select the current line
    (helix.static.extend_line_below)

    ;; Grab the text from the current selected line
    (let ([line (trim-start (helix.static.current-highlighted-text!))])
      (when (starts-with? line "modified:   ")
        (let ([trimmed (trim-end (trim-start-matches line "modified:   "))])

          ;; Run git add for the given file that is modified
          (helix.run-shell-command "git" "add" trimmed)

          ;; TODO: Attempt to enqueue this _after_ the git add command as finished. Right now we're just operating on a naive check
          ;; that the git status has finished
          (enqueue-thread-local-callback-with-delay 10 refresh-gs-picker))))))
