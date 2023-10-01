(require "../prelude.scm"
         (for-syntax "../prelude.scm"))

(require-helix)

(provide refresh-files
         flush-recent-files
         recentf-open-files
         recentf-snapshot
         get-recent-files)

(define MAX-FILE-COUNT 25)

(define RECENTF-FILE ".helix/recent-files.txt")

(define ENABLED #f)

;; Only get the doc if it exists - also use real options instead of false here cause it kinda sucks
(define (editor-get-doc-if-exists editor doc-id)
  (if (editor-doc-exists? editor doc-id) (editor->get-document editor doc-id) #f))

(define (read-recent-files)
  (unless (path-exists? ".helix")
    (create-directory! ".helix"))

  (cond
    ;; We're just storing these as strings with the quotes still there, so that we
    ;; can call `read` on them accordingly
    [(path-exists? RECENTF-FILE) (~> (open-input-file RECENTF-FILE) (read-port-to-string) (read!))]
    [else '()]))

(define *recent-files* (read-recent-files))

(define (get-recent-files)
  *recent-files*)

(define (remove-duplicates lst)
  ;; Iterate over, grabbing each value, check if its in the hash, otherwise skip it
  (define (remove-duplicates-via-hash lst accum set)
    (cond
      [(null? lst) accum]
      [else
       (let ([elem (car lst)])
         (if (hashset-contains? set elem)
             (remove-duplicates-via-hash (cdr lst) accum set)
             (remove-duplicates-via-hash (cdr lst) (cons elem accum) (hashset-insert set elem))))]))

  (reverse (remove-duplicates-via-hash lst '() (hashset))))

(define (refresh-files cx)
  (let* ([editor (cx-editor! cx)]
         [document-ids (editor-all-documents editor)]
         [currently-opened-files (filter string?
                                         (map (lambda (doc-id)
                                                (let ([document (editor-get-doc-if-exists editor
                                                                                          doc-id)])
                                                  (Document-path document)))
                                              document-ids))])

    ;; Merge the files with the existing list
    (let* ([full-list (append currently-opened-files *recent-files*)]
           [deduped (remove-duplicates full-list)])

      (set! *recent-files* (take deduped MAX-FILE-COUNT)))))

(define (flush-recent-files)
  ;; Open the output file, and then we'll write all the recent files down
  (let ([output-file (open-output-file RECENTF-FILE)])
    (map (lambda (line)
           (when (string? line)
             (write-line! output-file line)))
         *recent-files*)))

(define (helix-picker! cx pick-list)
  (push-component! cx (Picker::new pick-list)))

(define (recentf-open-files cx)
  (helix-picker! cx *recent-files*))

;; Runs every 2 minutes, and snapshots the visited files
(define (recentf-snapshot cx)

  (refresh-files cx)
  (flush-recent-files)

  (enqueue-thread-local-callback-with-delay cx (* 1000 60 2) recentf-snapshot))
