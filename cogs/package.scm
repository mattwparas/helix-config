(require "cogs/picker.scm")
(require "helix/misc.scm")
(require-builtin helix/components)

(provide load-package
         current-packages
         path->package
         module->exported
         list-packages)

(define *loaded-package-registry* (hash))

(define *package-index* (hash))

(define (mark-package-loaded! path status)
  (set! *loaded-package-registry* (hash-insert *loaded-package-registry* path status)))

;; Implementation detail that should be cleaned up. We shouldn't really make
;; this accessible at the top level like this - it should more or less be a
;; reserved detail. These should not be top level values, and if they are
;; top level values, they should have a better name.
;;
;; This is simply an implementation defined behavior.
(define (path->package path)
  (eval (string->symbol (string-append "__module-mangler" (canonicalize-path path) "__%#__"))))

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

;;@doc
;; Grab the exported symbols from the module located at the given path
(define (module->exported path)
  (~> path path->package hash-keys->list))

;;@doc
;; List the packages that have been loaded using this package system.
;; The preview shows the status of the packages, alongside
;; a list
(define (list-packages)
  (push-component! (picker-selection
                    (hash-keys->list *loaded-package-registry*)
                    (lambda (_) void)
                    #:preview-function
                    (lambda (picker selection rect frame)
                      ;; TODO: Draw something more interesting here, other than the usual stuff
                      (frame-set-string! frame
                                         (+ 1 (area-x rect))
                                         (+ 1 (area-y rect))
                                         (to-string "Loaded successfully:"
                                                    (hash-get *loaded-package-registry* selection))
                                         (style)))
                    #:highlight-prefix "> ")))
