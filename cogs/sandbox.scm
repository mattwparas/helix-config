(require (prefix-in helix. "helix/commands.scm"))
(require "helix/misc.scm")

(require "/home/matt/Documents/helix-fork/helix/helix-term/src/commands/engine/controller.scm")

(provide my-custom-vsplit)

(define (my-custom-vsplit)
  (helix.vsplit-new)

  ;; Bind the callback to the specific runtime that we're in
  (enqueue-thread-local-callback (make-runtime-specific-function (lambda () (helix.vsplit-new)))))
