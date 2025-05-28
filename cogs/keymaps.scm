(require-builtin helix/core/keymaps as helix.keymaps.)

(require "helix/configuration.scm")

(provide *reverse-buffer-map-insert*
         merge-keybindings
         set-global-buffer-or-extension-keymap
         add-global-keybinding
         deep-copy-global-keybindings
         keymap)

(define (get-doc name)
  ;; Do our best - if the identifier doesn't exist (for example, if we're checking)
  ;; something like 'no_op, we should just continue
  (with-handler (lambda (_) #f)
                (eval `(#%function-ptr-table-get #%function-ptr-table ,(string->symbol name)))))

(define (get-typed-command-doc name)
  (get-doc (trim-start-matches name ":")))

(define (walk-leaves keybindings)
  (if (hash? keybindings) (map walk-leaves (hash-values->list keybindings)) keybindings))

(define (keybindings->leaves keybindings)
  (flatten (walk-leaves keybindings)))

(define (keybindings->docs keybindings)
  (define leaves
    (map (lambda (key) (if (symbol? key) (symbol->string key) key))
         (keybindings->leaves keybindings)))

  ;; Filter out anything without values - so we only want strings
  (define doc-map
    (map (lambda (command) (cons (trim-start-matches command ":") (get-typed-command-doc command)))
         leaves))

  ;; Filter out anything without values - so we only want strings on the
  ;; right hand side
  (transduce doc-map (filtering (lambda (p) (string? (cdr p)))) (into-hashmap)))

;;@doc
;; Insert a value into the reverse buffer map
(define (*reverse-buffer-map-insert* key value)
  (helix.keymaps.#%add-reverse-mapping key value))

;; Marshall values in and out of keybindings, referencing the associated values
;; within steel
(define (merge-keybindings keymap steel-key-map)
  (helix.keymaps.helix-merge-keybindings
   keymap
   (~> steel-key-map (value->jsexpr-string) (helix.keymaps.helix-string->keymap)))

  (helix.keymaps.keymap-update-documentation! keymap (keybindings->docs steel-key-map)))

;;@doc
;; Check that the types on this map check out, otherwise we don't need to consistently do these checks
(define (set-global-buffer-or-extension-keymap map)
  (transduce map
             (into-for-each (lambda (p)
                              (helix.keymaps.#%add-extension-or-labeled-keymap (list-ref p 0)
                                                                               (list-ref p 1))))))

;;@doc
;; Add keybinding to the global default
(define (add-global-keybinding map)

  ;; Copy the global ones
  (define global-bindings (get-keybindings))
  (helix.keymaps.helix-merge-keybindings
   global-bindings
   (~> map (value->jsexpr-string) (helix.keymaps.helix-string->keymap)))

  (helix.keymaps.keymap-update-documentation! global-bindings (keybindings->docs map))

  (keybindings global-bindings))

;;@doc
;; Deep copy the global keymap
(define (deep-copy-global-keybindings)

  ;; Copy the global keybindings directly
  ;; off of the configuration object
  (get-keybindings))

(define-syntax #%keybindings
  (syntax-rules ()
    [(_ conf (key (value ...) rest ...))
     (hash-insert conf
                  (dbg! (if (string? (quote key)) (quote key) (symbol->string (quote key))))
                  (#%keybindings (hash) (value ...) rest ...))]

    [(_ conf (key (value ...)))
     (hash (dbg! (if (string? (quote key) (symbol->string (quote key)))))
           (#%keybindings (hash) (value ...)))]

    [(_ conf (key value))

     (hash-insert conf
                  (dbg! (if (string? (quote key)) (quote key) (symbol->string (quote key))))
                  (if (string? value) value (~>> (quote value) symbol->string (string-append ":"))))]

    [(_ conf (key (value ...)) rest ...)

     (#%keybindings
      (hash-insert conf
                   (dbg! (if (string? (quote key)) (quote key) (symbol->string (quote key))))
                   (#%keybindings (hash) (value ...)))
      rest ...)]

    [(_ conf (key value) rest ...)

     (#%keybindings
      (hash-insert conf
                   (dbg! (if (string? (quote key)) (quote key) (symbol->string (quote key))))
                   (if (string? value) value (~>> (quote value) symbol->string (string-append ":"))))
      rest ...)]))

(define-syntax keymap
  (syntax-rules (global insert normal select)
    [(_ (global) args ...) (add-global-keybinding (keymap args ...))]

    [(_) (hash)]

    [(_ (insert args ...) rest ...)
     (hash-union (#%keybindings (hash) ("insert" args ...)) (keymap rest ...))]

    [(_ (normal args ...) rest ...)
     (hash-union (#%keybindings (hash) ("normal" args ...)) (keymap rest ...))]

    [(_ (select args ...) rest ...)
     (hash-union (#%keybindings (hash) ("select" args ...)) (keymap rest ...))]))
