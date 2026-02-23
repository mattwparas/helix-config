(require "picker.scm")
(require "helix/misc.scm")

(require "helix/components.scm")

(provide load-package
         current-packages
         list-packages)

(define *loaded-package-registry* (hash))

(define *package-index* (hash))

(define (mark-package-loaded! path status)
  (set! *loaded-package-registry* (hash-insert *loaded-package-registry* path status)))

;;@doc
;; Fallibly require a package from the given path.
;; If it fails, the repl should continue on, business as usual
(define (load-package path)
  (with-handler (lambda (err)
                  (log::info! (to-string "Error requiring module: " path))
                  (log::info! (to-string err))
                  (mark-package-loaded! path #f))
                (eval `(require ,path))
                (mark-package-loaded! path #t)))

(define (current-packages)
  *loaded-package-registry*)

(define (for-each-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (when (null? lst)
          (return! void))
        (for-each-index func (cdr lst) (+ index 1)))))

(define (preview-function picker selection rect frame)
  (define symbols (module->exports selection))
  (frame-set-string! frame
                     (+ 1 (area-x rect))
                     (+ 1 (area-y rect))
                     (to-string "Loaded successfully:" (hash-get *loaded-package-registry* selection))
                     (theme-scope "ui.text"))

  ;; Go through each
  (for-each-index (lambda (index line)
                    (frame-set-string! frame
                                       (+ 1 (area-x rect))
                                       (+ 1 (area-y rect) index)
                                       (to-string line)
                                       (theme-scope "ui.text")))
                  symbols
                  1))

;;@doc
;; List the packages that have been loaded using this package system.
;; The preview shows the status of the packages, alongside
;; a list
(define (list-packages)
  (push-component! (picker-selection (hash-keys->list *loaded-package-registry*)
                                     (lambda (_) void)
                                     #:preview-function preview-function
                                     #:highlight-prefix "> ")))
