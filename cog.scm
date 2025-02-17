(define package-name 'mattwparas-helix-package)
(define version "0.1.0")

;; Point to all of the packages that exist
(define dependencies
  '((#:name steel-editor-config #:git "https://github.com/mattwparas/steel-editor-config")))

(define dylibs '((#:name "steel-editor-config")))
