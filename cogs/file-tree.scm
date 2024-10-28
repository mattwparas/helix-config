;; Dropping the builtin, in lieu of something that uses the global context?
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/misc.scm")
(require "helix/editor.scm")
; (require "steel/sorting/merge-sort.scm")

;;; -----------------------------------------------------------------
;;; Merge two lists of numbers which are already in increasing order

(define merge-lists
  (lambda (l1 l2 comparator)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (if (comparator (car l1) (car l2))
                (cons (car l1) (merge-lists (cdr l1) l2 comparator))
                (cons (car l2) (merge-lists (cdr l2) l1 comparator)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

(define even-numbers
  (lambda (l)
    (if (null? l) '() (if (null? (cdr l)) '() (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define odd-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l)) (list (car l)) (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

(define (merge-sort l #:comparator [comparator <])
  (if (null? l)
      l
      (if (null? (cdr l))
          l
          (merge-lists (merge-sort (odd-numbers l) #:comparator comparator)
                       (merge-sort (even-numbers l) #:comparator comparator)
                       comparator))))

(provide fold-directory
         unfold-all-one-level
         open-file-from-picker
         create-file
         create-directory
         fold-all
         FILE-TREE
         FILE-TREE-KEYBINDINGS
         create-file-tree
         file-tree-set-side!)

;; labelled buffers ->
(require (only-in "labelled-buffers.scm"
                  make-new-labelled-buffer!
                  temporarily-switch-focus
                  open-or-switch-focus
                  currently-in-labelled-buffer?
                  open-labelled-buffer
                  maybe-fetch-doc-id
                  fetch-doc-id))

;; TODO: This should be moved to a shared module somewhere, once the component API is cleaned up
(define (helix-prompt! prompt-str thunk)
  (push-component! (prompt prompt-str thunk)))

;; TODO: Prefix function names to keep them separate
;; File Tree keybindings
(define FILE-TREE-KEYBINDINGS
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
              "F"
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

;; This needs to be globally unique
(define FILE-TREE "github.com/mattwparas/helix-config/file-tree")

(define file-tree-open-to-side 'left)

;; TODO: This should probably be a contract
(define (file-tree-set-side! side)
  (unless (or (equal? side 'left) (equal? side 'right))
    (error! "file-tree-set-side! requires either the 'left or 'right"))
  (set! file-tree-open-to-side side))

(define *file-tree* '())
(define *directories* (hash))
(define *ignore-set* (hashset "target" ".git"))

(define (fold! directory)
  (set! *directories* (hash-insert *directories* directory #t)))

(define (unfold! directory)
  (set! *directories* (hash-insert *directories* directory #f)))

(define (flatten x)
  (cond
    [(null? x) '()]
    [(not (list? x)) (list x)]
    [else (append (flatten (car x)) (flatten (cdr x)))]))

(define (format-dir path)
  (if (hash-contains? *directories* path)
      (if (hash-try-get *directories* path) ">  " "v  ")
      ">  " ;; First time we're visiting, mark as closed
      ))

(define *extension-map* (hash "rs" " " "scm" "󰘧 "))

(define (path->symbol path)
  (let ([extension (path->extension path)])
    (if (not (void? extension))
        (begin
          (define lookup (hash-try-get *extension-map* (path->extension path)))
          (if lookup lookup " "))

        " ")))

;; Simple tree implementation
;; Walks the file structure and prints without much fancy formatting
;; Returns a list of the visited files for convenience
(define (tree p writer-thunk)
  (define (tree-rec path padding)
    (define name (file-name path))

    (if (hashset-contains? *ignore-set* name)
        '()
        (begin
          (writer-thunk
           (string-append padding (if (is-dir? path) (format-dir path) (path->symbol path)) name))
          (cond
            [(is-file? path) path]
            [(is-dir? path)
             ;; If we're not supposed to see this path (i.e. its been folded),
             ;; then we're going to ignore it
             ;; Also - if it doesn't exist in the set, default it to folded
             (if (not (hash-contains? *directories* path))
                 (begin
                   (set! *directories* (hash-insert *directories* path #t))
                   (list path))
                 (if (hash-try-get *directories* path)
                     (list path)

                     (cons path
                           (map (fn (x) (tree-rec x (string-append padding "    ")))
                                (merge-sort (read-dir path) #:comparator string<?)))))]
            [else void]))))
  (flatten (tree-rec p "")))

;;@doc
;; Open the currently selected line
(define (open-file-from-picker)
  (when (currently-in-labelled-buffer? FILE-TREE)
    (define file-to-open (list-ref *file-tree* (helix.static.get-current-line-number)))
    (helix.open file-to-open)))

;; Initialize all roots to be flat so that we don't blow things up, recursion only goes in to things
;; that are expanded
(define (create-file-tree)

  ;; The doc id, or #false if it is not in the map
  (define doc-id (maybe-fetch-doc-id FILE-TREE))

  (unless doc-id
    (make-new-labelled-buffer! #:label FILE-TREE #:side file-tree-open-to-side))

  (unless (editor-doc-exists? (fetch-doc-id FILE-TREE))
    (make-new-labelled-buffer! #:label FILE-TREE #:side file-tree-open-to-side))

  (temporarily-switch-focus
   (lambda ()
     (open-labelled-buffer FILE-TREE)

     ;; Open depending on the setting
     (cond
       [(equal? file-tree-open-to-side 'left) (helix.static.move-window-far-left)]
       [(equal? file-tree-open-to-side 'right) (helix.static.move-window-far-right)]
       [else void])

     ;;
     (helix.static.move-window-far-left)

     (helix.static.select_all)
     (helix.static.delete_selection)

     ;; Update the current file tree value
     (set! *file-tree*
           (tree (helix-find-workspace)
                 (lambda (str)
                   (helix.static.insert_string str)
                   (helix.static.open_below)
                   (helix.static.goto_line_start)))))))

;;@doc
;; Fold the directory that we're currently hovering over
(define (fold-directory)
  (when (currently-in-labelled-buffer? FILE-TREE)
    (define directory-to-fold (list-ref *file-tree* (helix.static.get-current-line-number)))
    (when (is-dir? directory-to-fold)
      (begin
        ;; If its already folded, unfold it
        (if (hash-try-get *directories* directory-to-fold)
            (unfold! directory-to-fold)
            (fold! directory-to-fold))

        (update-file-tree)))))

;;@doc
;; Create a file under wherever we are
(define (create-file)
  (when (currently-in-labelled-buffer? FILE-TREE)
    (define currently-selected (list-ref *file-tree* (helix.static.get-current-line-number)))
    (define prompt
      (if (is-dir? currently-selected)
          (string-append "New file: " currently-selected "/")
          (string-append "New file: "
                         (trim-end-matches currently-selected (file-name currently-selected)))))

    (helix-prompt!
     prompt
     (lambda (result)
       (define file-name (string-append (trim-start-matches prompt "New file: ") result))
       (temporarily-switch-focus (lambda ()
                                   (helix.vsplit-new)
                                   (helix.open file-name)
                                   (helix.write file-name)
                                   (helix.quit)))

       ;; TODO:
       ;; This is happening before the write is finished, so its not working. We will have to manually insert
       ;; the new file into the right spot in the tree, which would require rewriting this to have a proper sorted
       ;; tree representation in memory, which we don't yet have. For now, we can just do this I guess
       (enqueue-thread-local-callback refresh-file-tree)))))

(define (update-file-tree)

  (define current-selection (helix.static.current-selection-object))
  ; (define line-number (helix.static.get-current-line-number))
  (define last-mode (editor-mode))

  (helix.static.select_all)
  (helix.static.delete_selection)

  ;; Update the current file tree value
  (set! *file-tree*
        (tree (helix-find-workspace)
              (lambda (str)
                (helix.static.insert_string str)
                (helix.static.open_below)
                (helix.static.goto_line_start))))

  ;; Set it BACK to where we were previously!
  ;; TODO: Currently the following bug exists:
  ;; Open helix, open file tree, run SPC-b to open the file tree
  ;; buffer (there should now be two of them). Press TAB, then F.
  ;; Helix will crash. One way to fix it is to not update the selection,
  ;; however that makes the file tree experience way worse. A better
  ;; way for now is to just disallow that command in the file tree
  ;; buffer since I haven't yet figured out how to get it working.
  (helix.static.set-current-selection-object! current-selection)

  (editor-set-mode! last-mode))

(define (refresh-file-tree)
  (temporarily-switch-focus (lambda ()
                              (open-labelled-buffer FILE-TREE)
                              (update-file-tree))))

;;@doc
;; Create a new directory
(define (create-directory)
  (when (currently-in-labelled-buffer? FILE-TREE)
    (define currently-selected (list-ref *file-tree* (helix.static.get-current-line-number)))
    (define prompt
      (if (is-dir? currently-selected)
          (string-append "New directory: " currently-selected "/")
          (string-append "New directory: "
                         (trim-end-matches currently-selected (file-name currently-selected)))))

    (helix-prompt! prompt
                   (lambda (result)
                     (define directory-name
                       (string-append (trim-start-matches prompt "New directory: ") result))
                     (hx.create-directory directory-name)
                     (enqueue-thread-local-callback refresh-file-tree)))))

;;@doc
;; Fold all of the directories
(define (fold-all)
  (when (currently-in-labelled-buffer? FILE-TREE)

    (set! *directories*
          (transduce *directories* (mapping (lambda (x) (list (list-ref x 0) #t))) (into-hashmap)))

    (helix.static.goto_file_start)

    (refresh-file-tree)))

;;@doc
;; Unfold all of the currently open directories one level.
(define (unfold-all-one-level)
  (when (currently-in-labelled-buffer? FILE-TREE)

    (set! *directories*
          (transduce *directories* (mapping (lambda (x) (list (list-ref x 0) #f))) (into-hashmap)))

    (refresh-file-tree)))
