(require-builtin steel/random as rand::)
(require-builtin helix/core/static as helix.static.)
(require-builtin helix/core/typable as helix.)

(require "cogs/keymaps.scm")
(require (only-in "cogs/options.scm" apply-options))

(require (only-in "cogs/file-tree.scm" FILE-TREE-KEYBINDINGS FILE-TREE))

(require (only-in "cogs/recentf.scm" recentf-open-files get-recent-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rng (rand::thread-rng!))

;; Picking one from the possible themes is _fine_
(define possible-themes '("tokyonight_storm" "catppuccin_macchiato" "kanagawa"))

(define (select-random lst)
  (let ([index (rand::rng->gen-range rng 0 (length lst))]) (list-ref lst index)))

(define (randomly-pick-theme options)
  ;; Randomly select the theme from the possible themes list
  (helix.theme *helix.cx* (list (select-random options)) helix.PromptEvent::Validate))

(randomly-pick-theme possible-themes)

;;;;;;;;;;;;;;;;;;;;;;;; Default modes ;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the recentf snapshot, will watch every 2 minutes for active files,
;; and flush those down to disk
(recentf-snapshot *helix.cx*)

;;;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (require-builtin helix/core/themes as hx.themes.)
;  (require-builtin helix/core/languages as hx.languages.)

;  (define (theme.key #:fg (fg void)
;                     #:bg (bg void)
;                     #:underline (underline void)
;                     #:modifiers (modifiers void))

;    (transduce (hash "fg" fg "bg" bg "underline" underline "modifiers" modifiers)
;               (filtering (lambda (pair) (not (void? (cadr pair)))))
;               (into-hashmap)))

;  (define bg_acme_bar (theme.key #:bg "acme_bar_bag"))

;  (define acme_bar_bg "#aeeeee")
;  (define acme_bar_inactive

;                "#eaffff"
;    )

;  (define acme-theme
;    (hash "comment"
;          "green"
;          "string"
;          "red"
;          "ui.text"
;          "black"
;          "ui.virtual"
;          "indent"
;          "diagnostic.error"
;          (hash "bg" "red" "fg" "white" "modifiers" (list "bold"))
;          "diagnostic.hint"
;          (hash "fg" "gray" "modifiers" (list "bold"))
;          "diagnostic.warning"
;          (hash "bg" "orange" "fg" "black" "modifiers" (list "bold"))
;          "diff.delta"
;          (hash "fg" "acme_bar_bg")
;          "diff.minus"
;          (hash "fg" "red")
;          "diff.plus"
;          (hash "fg" "green")
;          "palette"
;          (hash "acme_bar_bg"
;                "#aeeeee"
;                "acme_bar_inactive"
;                "#eaffff"
;                "acme_bg"
;                "#ffffea"
;                "black"
;                "#000000"
;                "cursor"
;                "#444444"
;                "gray"
;                "#777777"
;                "green"
;                "#065905"
;                "indent"
;                "#aaaaaa"
;                "orange"
;                "#f0ad4e"
;                "red"
;                "#a0342f"
;                "selected"
;                "#eeee9e"
;                "white"
;                "#ffffff")
;          "ui.background"
;          (hash "bg" "acme_bg")
;          ; (hash "bg" "black")
;          "ui.bufferline"
;          (hash "bg" "acme_bar_bg" "fg" "black")
;          "ui.bufferline.active"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.cursor"
;          (hash "bg" "cursor" "fg" "white")
;          "ui.cursor.match"
;          bg_acme_bar
;          "ui.cursorline"
;          bg_acme_bar
;          "ui.debug"
;          (hash "fg" "orange")
;          "ui.help"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.highlight.frameline"
;          (hash "bg" "#da8581")
;          "ui.linenr"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.linenr.selected"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.menu"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.menu.selected"
;          (hash "bg" "selected")
;          "ui.popup"
;          (hash "bg" "acme_bg" "fg" "black")
;          "ui.selection"
;          (hash "bg" "selected")
;          "ui.statusline"
;          (hash "bg" "acme_bar_bg" "fg" "black")
;          "ui.statisline.inactive"
;          (hash "bg" "acme_bar_inactive" "fg" "black")
;          "ui.virtual.ruler"
;          bg_acme_bar
;          "ui.window"
;          (hash "bg" "acme_bg")))

;  (let ([result (hx.themes.register-theme! "custom-acme" (value->jsexpr-string acme-theme))])
;    (when (Err? result)
;      (error result)))

; ;;;;;;;;;;;;;;;;;;;;;;;;; Language Configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (define scheme-configuration
;    (hash "language"
;          (list (hash "auto-format"
;                      #t
;                      "name"
;                      "scheme"
;                      "grammar"
;                      "scheme"
;                      "formatter"
;                      (hash "args" '("fmt" "-i") "command" "raco")
;                      "file-types" '("scm")))))

;  (let ([result (hx.languages.register-language-configuration!
;                 (value->jsexpr-string scheme-configuration))])

;    (when (Err? result)
;      (error result)))

;  (hx.languages.flush-configuration *helix.cx*)

;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;

;; To remove a binding, set it to 'no_op
;; For example, this will make it impossible to enter insert mode:
;; (hash "normal" (hash "i" 'no_op))

;; Set the global keybinding for now
(add-global-keybinding
 (hash
  ; "insert"
  ; (hash "C-r" (hash "f" ":dummy"))
  "normal"
  (hash
   "C-r"
   (hash "f" ":recentf-open-files")
   ; "c"
   ; ":dummy") ;; "space" (hash "/" ":search-in-directory") ;; Uncomment if you'd like to make this keybinding
   )))

(define scm-keybindings (hash "insert" (hash "ret" ':scheme-indent "C-l" ':insert-lambda)))

;; Grab whatever the existing keybinding map is
(define standard-keybindings (deep-copy-global-keybindings))

(define file-tree-base (deep-copy-global-keybindings))

(merge-keybindings standard-keybindings scm-keybindings)
(merge-keybindings file-tree-base FILE-TREE-KEYBINDINGS)

;; <scratch> + <doc id> is probably the best way to handle this?
(set-global-buffer-or-extension-keymap (hash "scm" standard-keybindings FILE-TREE file-tree-base))

;;;;;;;;;;;;;;;;;;;;;;;;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *config-map* '((file-picker.hidden false) (cursorline true) (soft-wrap.enable true)))

(apply-options *helix.cx* *config-map*)

(randomly-pick-theme possible-themes)

;; Probably should be a symbol?

; (register-hook! 'post-insert-char 'prompt-on-char-press)
