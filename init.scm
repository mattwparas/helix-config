(require-builtin steel/random as rand::)
(require-builtin helix/core/static as helix.static.)
(require-builtin helix/core/typable as helix.)

;; TODO: Fix the path resolution here
(require "cogs/keymaps.scm")

(require (only-in "cogs/options.scm" apply-options))

; (require-builtin helix/core/keymaps)

; (require-builtin steel/web/blocking/requests as requests.)
(require "steel/result")
(require-builtin steel/time)

(require-builtin steel/process as process/)
(require "steel/lists/lists.scm")

; (helix.config-reload *helix.cx* '() helix.PromptEvent::Validate)

;; Nice! This doesn't segfault anymore
;; (require-builtin custom-commands)

;; (External-talk-to-context *helix.cx*)

;; (displayln (FooBarBaz::new))

;; (require-builtin external-command-module as ext.)
;; (ext.go-change-theme *helix.cx* (list "default") helix.PromptEvent::Validate)

; (require "steel/result")

;; (require-builtin dylib/toml as toml::)
;; (displayln (toml::add-100 #\c))

(define (level1)
  (level2))

(define (level2)
  (error! "I want the stack trace to show up"))

(define (big-error cx)
  (level1))

; (define (get-weather)
;   (~> (requests.get "http://wttr.in/?format=3")
;       (requests.call)
;       (unwrap-ok)
;       (requests.response->text)
;       (unwrap-ok)))

; (define (format-weather weather-line)
; (~>> weather-line (trim-end)))

;; Main loop for getting the weather
; (define (main)
;   (while #t
;          (begin
;            (set-status-line! (format-weather (get-weather)))
;            (time/sleep-ms (* 60 1000)))))

(define rng (rand::thread-rng!))

;; Spawn a thread for the weather!
; (spawn-thread! main)

(set-theme-dracula *helix.cx*)

;; Picking one from the possible themes is _fine_
(define possible-themes '("ayu_mirage" "tokyonight_storm" "catppuccin_macchiato"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range rng 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme *helix.cx* (list (select-random options)) helix.PromptEvent::Validate))

(randomly-pick-theme possible-themes)

;;@doc
;; Ensure the LSP registered is installed on the machine, and if not, install it
(define (ensure-installed program install-thunk)
  (when (void? (process/which program))
    (install-thunk)))

;;@doc
;; Install marksman using the snap tool chain prior to launching
(define (install-marksman)
  (displayln "Installing marksman...")
  (if (equal? (current-os!) "linux")
      (helix.static.block-on-shell-command *helix.cx*
                                           (list "sudo snap install marksman")
                                           helix.PromptEvent::Validate)
      (error "Unknown installation mechanism for marksman on non unix platforms!")))

;;@doc
;; Install taplo using cargo
(define (install-taplo)
  (displayln "Installing taplo...")
  (helix.static.block-on-shell-command *helix.cx*
                                       (list "cargo install taplo-cli --locked")
                                       helix.PromptEvent::Validate))

;;@doc
;; Install marksman via a subprocess installation - standardize this with some sort of
;; program installation builder struct.
(define (install-marksman-subprocess)
  (~> (process/command "sudo" '("snap" "install" "marksman")) (process/spawn-process)))

;;@doc
;; Install taplo
(define (install-taplo-subprocess)
  (~> (process/command "cargo" '("install" "taplo-cli" "--locked"))))

;;@doc
;; Install raco fmt
; (define (install-raco-fmt)
;   (displayln "Installing raco fmt...")
;   (helix.static.block-on-shell-command)

;   )

(define (ensure-all-installed program-thunk-pairs)
  (for-each (λ (pairs) (ensure-installed (list-ref pairs 0) (list-ref pairs 1))) program-thunk-pairs))

(define (do-nothing)
  void)

;; Ensure that all of the following are installed:
(ensure-all-installed
 (list (list "marksman" install-marksman) (list "taplo" install-taplo) (list "black" do-nothing)))

; ;; Install marksman
; (ensure-installed *helix.cx* "marksman" install-marksman)

; ;; Install taplo (toml)
; (ensure-installed *helix.cx* "taplo" install-taplo)

; ;; Ensure that the following programs are installed
; (ensure-installed *helix.cx* "black" (λ () void))

;;;;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (apply-options options-assoc-list)
;   (helix.set-options *helix.cx*
;                      (~>> options-assoc-list (flatten) (map symbol->string))
;                      helix.PromptEvent::Validate))

(define *config-map* '((file-picker.hidden false) (cursorline true) (soft-wrap.enable true)))

(apply-options *helix.cx* *config-map*)

;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;

;; To remove a binding, set it to 'no_op
;; For example, this will make it impossible to enter insert mode:
;; (hash "normal" (hash "i" 'no_op))

(define scm-keybindings (hash "normal" (hash "P" (hash "n" ':run-prompt) "tab" ':fold-directory)))

(define file-tree-keybindings
  (hash "normal"
        (hash "i"
              'no_op
              "v"
              'no_op
              "|"
              'no_op
              "!"
              'no_op
              "A-!"
              'no_op
              "$"
              'no_op
              "C-a"
              'no_op
              "C-x"
              'no_op
              "a"
              'no_op
              "I"
              'no_op
              "o"
              'no_op
              "O"
              'no_op
              "d"
              'no_op
              "A-d"
              'no_op
              "tab"
              ':fold-directory
              "E"
              ':unfold-all-one-level
              "o"
              ':open-file-from-picker
              "n"
              (hash "f" ':create-file "d" ':create-directory)
              "F"
              ':fold-all)))

;; Grab whatever the existing keybinding map is
(define standard-keybindings (helix-current-keymap))
(define file-tree-base (helix-current-keymap))

(merge-keybindings standard-keybindings scm-keybindings)
(merge-keybindings file-tree-base file-tree-keybindings)

;; <scratch> + <doc id> is probably the best way to handle this?
(set-global-buffer-or-extension-keymap (hash "scm" standard-keybindings "file-tree" file-tree-base))
