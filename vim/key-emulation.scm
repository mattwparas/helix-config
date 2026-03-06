(require (prefix-in helix. "helix/commands.scm"))
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/ext.scm")
(require "helix/components.scm")
(require-builtin helix/core/text)

(define a-key (string->key-event "a"))
(define e-key (string->key-event "e"))
(define t-key (string->key-event "t"))
(define T-key (string->key-event "T"))
(define w-key (string->key-event "w"))
(define p-key (string->key-event "p"))
(define f-key (string->key-event "f"))
(define c-key (string->key-event "c"))
(define x-key (string->key-event "x"))
(define at-key (string->key-event "@"))
(define comma-key (string->key-event ","))

(provide 
  a-key
  e-key
  t-key
  T-key
  w-key
  p-key
  f-key
  c-key
  x-key
  at-key
  comma-key
)
