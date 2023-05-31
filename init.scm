(require-builtin steel/random as rand::)
(require-builtin helix/core/static as helix.static.)
(require-builtin helix/core/typable as helix.)

(require-builtin steel/web/blocking/requests as requests.)
(require "steel/result")
(require-builtin steel/time)

(require-builtin steel/process as process/)
(require "steel/lists/lists.scm")

;; (require-builtin external-command-module as ext.)
;; (ext.go-change-theme *helix.cx* (list "default") helix.PromptEvent::Validate)

; (require "steel/result")

;; (require-builtin dylib/toml as toml::)
;; (displayln (toml::add-100 #\c))

(define (get-weather)
  (~> (requests.get "http://wttr.in/?format=3")
      (requests.call)
      (unwrap-ok)
      (requests.response->text)
      (unwrap-ok)))

(define (format-weather weather-line)
  (~>> weather-line (trim-end)))

;; Main loop for getting the weather
(define (main)
  (while #t
         (begin
           (set-status-line! (format-weather (get-weather)))
           (time/sleep-ms (* 60 1000)))))

(define rng (rand::thread-rng!))

;; Spawn a thread for the weather!
(spawn-thread! main)

(set-theme-dracula *helix.cx*)

;; Picking one from the possible themes is _fine_
(define possible-themes '("ayu_mirage" "tokyonight_storm"))

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

;; Allow hidden files to show up
(helix.set-option *helix.cx* '("file-picker.hidden" "false"))
