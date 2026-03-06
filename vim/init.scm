(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/configuration.scm")
(require "helix/keymaps.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/ext.scm")
(require "helix/components.scm")

(require "change-motions.scm")
(require "delete-motions.scm")
(require "normal-motions.scm")
(require "visual-motions.scm")
(require "yank-motions.scm")
(require "utils.scm")

(define vim-keybindings
  (keymap (normal (l ":move-char-right-same-line")
                  (h ":move-char-left-same-line")
                  (k ":move-line-up")
                  (j ":move-line-down")
                  (right ":move-char-right-same-line")
                  (left ":move-char-left-same-line")
                  (up ":move-line-up")
                  (down ":move-line-down")
                  (f ":vim-find-next-char")
                  (F ":vim-find-prev-char")
                  (t ":vim-find-till-char")
                  (T ":vim-till-prev-char")
                  (a ":vim-append-mode")
                  (u ":vim-undo")
                  (w ":vim-next-word-start")
                  (e ":vim-next-word-end")
                  (G ":vim-goto-line-or-last")
                  (V ":visual-line-mode")
                  (A-d "no_op")
                  (A-c "no_op")
                  (% "match_brackets")
                  (X "no_op")
                  (A-x "no_op")
                  (p ":clipboard-paste-after")
                  (P ":clipboard-paste-before")
                  ;; TODO: More delete things
                  (d (d ":vim-delete-line")
                     (w ":vim-delete-word")
                     (W ":vim-delete-long-word")
                     (b ":vim-delete-prev-word")
                     (B ":vim-delete-prev-long-word")
                     (e ":vim-delete-word-end")
                     (E ":vim-delete-long-word-end")
                     ($ ":vim-delete-line-end")
                     (^ ":vim-delete-line-start")
                     (a (w ":vim-delete-around-word")
                        (p ":vim-delete-around-paragraph")
                        (f ":vim-delete-around-function")
                        (c ":vim-delete-around-comment")
                        (e ":vim-delete-around-data-structure")
                        (x ":vim-delete-around-html-tag")
                        (t ":vim-delete-around-type-definition")
                        (T ":vim-delete-around-test")
                        ("{" ":vim-delete-around-curly")
                        ("[" ":vim-delete-around-square")
                        ("(" ":vim-delete-around-paren")
                        ("<" ":vim-delete-around-arrow")
                        ("\"" ":vim-delete-around-double-quote")
                        ("'" ":vim-delete-around-single-quote"))
                     (i (w ":vim-delete-inner-word")
                        (p ":vim-delete-inner-paragraph")
                        (f ":vim-delete-inner-function")
                        (c ":vim-delete-inner-comment")
                        (e ":vim-delete-inner-data-structure")
                        (x ":vim-delete-inner-html-tag")
                        (t ":vim-delete-inner-type-definition")
                        (T ":vim-delete-inner-test")
                        ("{" ":vim-delete-inner-curly")
                        ("[" ":vim-delete-inner-square")
                        ("(" ":vim-delete-inner-paren")
                        ("<" ":vim-delete-inner-arrow")
                        ("\"" ":vim-delete-inner-double-quote")
                        ("'" ":vim-delete-inner-single-quote")))
                  (c (c ":vim-change-line")
                     (w ":vim-change-word")
                     (W ":vim-change-long-word")
                     (b ":vim-change-prev-word")
                     (B ":vim-change-prev-long-word")
                     (e ":vim-change-word-end")
                     (E ":vim-change-long-word-end")
                     ($ ":vim-change-line-end")
                     (^ ":vim-change-line-start")
                     (a (w ":vim-change-around-word")
                        (p ":vim-change-around-paragraph")
                        (f ":vim-change-around-function")
                        (c ":vim-change-around-comment")
                        (e ":vim-change-around-data-structure")
                        (x ":vim-change-around-html-tag")
                        (t ":vim-change-around-type-definition")
                        (T ":vim-change-around-test")
                        ("{" ":vim-change-around-curly")
                        ("[" ":vim-change-around-square")
                        ("(" ":vim-change-around-paren")
                        ("<" ":vim-change-around-arrow")
                        ("\"" ":vim-change-around-double-quote")
                        ("'" ":vim-change-around-single-quote"))
                     (i (w ":vim-change-inner-word")
                        (p ":vim-change-inner-paragraph")
                        (f ":vim-change-inner-function")
                        (c ":vim-change-inner-comment")
                        (e ":vim-change-inner-data-structure")
                        (x ":vim-change-inner-html-tag")
                        (t ":vim-change-inner-type-definition")
                        (T ":vim-change-inner-test")
                        ("{" ":vim-change-inner-curly")
                        ("[" ":vim-change-inner-square")
                        ("(" ":vim-change-inner-paren")
                        ("<" ":vim-change-inner-arrow")
                        ("\"" ":vim-change-inner-double-quote")
                        ("'" ":vim-change-inner-single-quote")))
                  (S ":vim-change-line")
                  (x ":vim-delete-selection")
                  ;; TODO: More yank things
                  (y (y ":vim-yank-line")
                     ;; TODO: around/inner long word
                     ;; TODO: paragraph, function, comment, test, html tag, etc.
                     (a (w ":yank-around-word")
                        (p ":yank-around-paragraph")
                        (f ":yank-around-function")
                        (c ":yank-around-comment")
                        (e ":yank-around-data-structure")
                        (x ":yank-around-html-tag")
                        (t ":yank-around-type-definition")
                        (T ":yank-around-test"))
                     (i (w ":yank-inner-word")
                        (p ":yank-inner-paragraph")
                        (f ":yank-inner-function")
                        (c ":yank-inner-comment")
                        (e ":yank-inner-data-structure")
                        (x ":yank-inner-html-tag")
                        (t ":yank-inner-type-definition")
                        (T ":yank-inner-test"))
                     (w ":yank-word")
                     (W ":yank-long-word")
                     (e ":yank-word")
                     (E ":yank-long-word")
                     (b ":yank-prev-word")
                     (B ":yank-prev-long-word")
                     ($ ":yank-line-end")
                     ("0" ":yank-line-start")
                     (^ ":yank-line-start-non-whitespace"))
                  (b ":vim-prev-word-start")
                  (B ":vim-prev-long-word-start")
                  (E ":vim-next-long-word-end")
                  (W ":vim-next-long-word-start")
                  ("0" "goto_line_start")
                  ($ "goto_line_end")
                  (^ "goto_first_nonwhitespace")
                  (del "delete_selection")
                  (C-r "redo")
                  (> (> "indent"))
                  (< (< "unindent"))
                  (D ":vim-delete-line-end")
                  (C ":vim-change-line-end")
                  ("{" ":vim-goto-prev-paragraph")
                  ("}" ":vim-goto-next-paragraph")
                  ;; NOTE: this implementation uses the , register
                  ;; so be careful with saving other things there
                  ("," ":vim-repeat-last-find")
                  (";" ":vim-reverse-last-find"))
          ;; TODO: make full "reflow mode"
          ;; ("=" ":reflow"))
          ;; Select bindings
          ;; TODO: Rename this to VIS (nacl TODO: figure out if I care)
          (select (a (w ":select-around-word")
                     (p ":select-around-paragraph")
                     (f ":select-around-function")
                     (c ":select-around-comment")
                     (e ":select-around-data-structure")
                     (x ":select-around-html-tag")
                     (t ":select-around-type-definition")
                     (T ":select-around-test")
                     ("{" ":select-around-curly")
                     ("[" ":select-around-square")
                     ("(" ":select-around-paren")
                     ("<" ":select-around-arrow")
                     ("\"" ":select-around-double-quote")
                     ("'" ":select-around-single-quote"))
                  (i (w ":select-inner-word")
                     (p ":select-inner-paragraph")
                     (f ":select-inner-function")
                     (c ":select-inner-comment")
                     (e ":select-inner-data-structure")
                     (x ":select-inner-html-tag")
                     (t ":select-inner-type-definition")
                     (T ":select-inner-test")
                     ("{" ":select-inner-curly")
                     ("[" ":select-inner-square")
                     ("(" ":select-inner-paren")
                     ("<" ":select-inner-arrow")
                     ("\"" ":select-inner-double-quote")
                     ("'" ":select-inner-single-quote"))
                  (h ":extend-char-left-same-line")
                  (l ":extend-char-right-same-line")
                  (j ":extend-line-down")
                  (k ":extend-line-up")
                  (w ":vim-extend-next-word-start")
                  (W ":vim-extend-next-long-word-start")
                  (p ":clipboard-paste-after")
                  (P ":clipboard-paste-before")
                  (y ":vim-yank-selection")
                  (d ":vim-delete-selection")
                  (x ":vim-delete-selection")
                  (c ":vim-change-selection")
                  ("{" ":vim-extend-to-prev-paragraph")
                  ("}" ":vim-extend-to-next-paragraph")
                  ("0" "extend_to_line_start")
                  ($ "extend_to_line_end")
                  (^ "extend_to_first_nonwhitespace")
                  (G "extend_to_file_end")
                  (% "match_brackets")
                  (f ":select-find-next-char")
                  (F ":select-find-prev-char")
                  (t ":select-find-till-char")
                  (T ":select-till-prev-char")
                  ("," ":select-repeat-last-find")
                  (";" ":select-reverse-last-find")
                  ("=" ":reflow")
                  (> (> "indent"))
                  (< (< "unindent"))
                  (left ":extend-char-left-same-line")
                  (right ":extend-char-right-same-line")
                  (down ":extend-line-down")
                  (up ":extend-line-up")
                  (esc ":exit-visual-line-mode"))
          (insert (C-d "unindent") (C-t "indent") (esc ":vim-exit-insert-mode"))))

(define (set-vim-keybindings!)
  (add-global-keybinding vim-keybindings))

(provide set-vim-keybindings!)
;; change motions
(provide vim-change-selection
         vim-change-line
         vim-change-word
         vim-change-long-word
         vim-change-prev-word
         vim-change-prev-long-word
         vim-change-word-end
         vim-change-long-word-end
         vim-change-line-end
         vim-change-line-start
         vim-change-around-word
         vim-change-inner-word
         vim-change-around-paragraph
         vim-change-inner-paragraph
         vim-change-around-function
         vim-change-inner-function
         vim-change-around-comment
         vim-change-inner-comment
         vim-change-around-data-structure
         vim-change-inner-data-structure
         vim-change-around-html-tag
         vim-change-inner-html-tag
         vim-change-around-type-definition
         vim-change-inner-type-definition
         vim-change-around-test
         vim-change-inner-test
         vim-change-around-curly
         vim-change-inner-curly
         vim-change-around-square
         vim-change-inner-square
         vim-change-inner-paren
         vim-change-around-paren
         vim-change-around-double-quote
         vim-change-inner-double-quote
         vim-change-around-single-quote
         vim-change-inner-single-quote
         vim-change-around-arrow
         vim-change-inner-arrow
         vim-change-inner-square
         vim-change-inner-paren
         vim-change-around-paren
         vim-change-around-double-quote
         vim-change-inner-double-quote
         vim-change-around-single-quote
         vim-change-inner-single-quote
         vim-change-around-arrow
         vim-change-inner-arrow

         ;; delete motions
         vim-delete-selection
         vim-delete-line
         vim-delete-word
         vim-delete-word-end
         vim-delete-long-word-end
         vim-delete-long-word
         vim-delete-line-end
         vim-delete-line-start
         vim-delete-around-word
         vim-delete-inner-word
         vim-delete-around-paragraph
         vim-delete-inner-paragraph
         vim-delete-prev-word
         vim-delete-prev-long-word
         vim-delete-around-function
         vim-delete-inner-function
         vim-delete-around-comment
         vim-delete-inner-comment
         vim-delete-around-data-structure
         vim-delete-inner-data-structure
         vim-delete-around-html-tag
         vim-delete-inner-html-tag
         vim-delete-around-type-definition
         vim-delete-inner-type-definition
         vim-delete-around-test
         vim-delete-inner-test
         vim-delete-around-curly
         vim-delete-inner-curly
         vim-delete-around-square
         vim-delete-inner-square
         vim-delete-inner-paren
         vim-delete-around-paren
         vim-delete-around-double-quote
         vim-delete-inner-double-quote
         vim-delete-around-single-quote
         vim-delete-inner-single-quote
         vim-delete-around-arrow
         vim-delete-inner-arrow

         ;; normal motions
         vim-undo
         vim-append-mode
         move-char-right-same-line
         move-char-left-same-line
         move-line-up-impl
         move-line-up
         move-line-down-impl
         move-line-down
         vim-find-next-char
         vim-find-prev-char
         vim-find-till-char
         vim-till-prev-char
         vim-repeat-last-find
         vim-reverse-last-find
         vim-goto-line-or-last
         vim-next-word-start
         vim-next-word-end
         vim-prev-word-start
         vim-prev-long-word-start
         vim-next-long-word-start
         vim-next-long-word-end
         vim-goto-next-paragraph
         vim-goto-prev-paragraph
         visual-line-mode
         vim-exit-insert-mode

         ;; visual motions
         extend-char-right-same-line
         extend-char-left-same-line
         extend-line-up
         extend-line-down
         vim-extend-next-word-start
         vim-extend-next-long-word-start
         vim-extend-to-next-paragraph
         vim-extend-to-prev-paragraph
         select-around-word
         select-inner-word
         select-around-paragraph
         select-inner-paragraph
         select-around-function
         select-inner-function
         select-around-comment
         select-inner-comment
         select-around-data-structure
         select-inner-data-structure
         select-around-html-tag
         select-inner-html-tag
         select-around-type-definition
         select-inner-type-definition
         select-around-test
         select-inner-test
         select-inner-curly
         select-around-curly
         select-inner-paren
         select-around-paren
         select-inner-square
         select-around-square
         select-inner-double-quote
         select-around-double-quote
         select-inner-single-quote
         select-around-single-quote
         select-inner-arrow
         select-around-arrow
         select-find-next-char
         select-find-prev-char
         select-find-till-char
         select-till-prev-char
         select-repeat-last-find
         select-reverse-last-find
         exit-visual-line-mode

         ;; yank motions
         vim-yank-selection
         yank-around-word
         yank-inner-word
         yank-word
         yank-long-word
         yank-prev-word
         yank-prev-long-word
         yank-line-end
         yank-line-start
         yank-line-start-non-whitespace
         vim-yank-line)
