(define package-name 'mattwparas-helix-package)
(define version "0.1.0")

;; Point to all of the packages that exist
(define dependencies
  '((#:name steel-pty #:git-url "https://github.com/mattwparas/steel-pty.git")
    (#:name helix-file-watcher #:git-url "https://github.com/mattwparas/helix-file-watcher.git")))

(define dylibs '())
