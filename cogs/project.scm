(require "steel/logging/log.scm")

(require "helix/editor.scm")
(require "helix/misc.scm")

(provide get-project-root
         set-project-root!
         get-project-helix
         in-project-helix
         set-project-helix!)

(define PROJECT-ROOT "./")
(define PROJECT-HELIX #f)

(define (get-project-root)
  PROJECT-ROOT)

(define (set-project-root! path)
  (if (ends-with? path "/")
      (set! PROJECT-ROOT path)
      (set! PROJECT-ROOT (string-append path "/"))))

(define (get-project-helix-path)
  (if PROJECT-HELIX
    PROJECT-HELIX
    (string-append PROJECT-ROOT ".helix/")))

(define (get-project-helix)
  (define path (get-project-helix-path))
  
  (unless (path-exists? path)
    (create-directory! path))
  
  path)

(define (in-project-helix path)
  (string-append (get-project-helix) path))

(define (set-project-helix! path)
  (if (ends-with? path "/")
      (set! PROJECT-HELIX path)
      (set! PROJECT-HELIX (string-append path "/"))))

;;;;;;;;;;;;; Perform default project location picking ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEFAULT_PROJECT_ROOT "./")
(define DEFAULT_PROJECT_HELIX "~/.config/helix/")

;; Figure out where root of project should be
(define (find-project-root)
  (define curr-dir (current-directory))

  (define (exists-and-is-dir? path)
    (and (path-exists? path) (is-dir? path)))
  
  ;; Given dir <<curr>>, if it contains a <<find>> directory, return it.
  ;; the result of calling helper (parent-name curr).
  (define (helper curr find)
    (cond
      [(string=? curr "") #f] ;; Didn't find after searching all the way to root, return false.
      [(exists-and-is-dir? (string-append curr "/" find)) curr]
      [else (helper (parent-name curr) find)]))

  (define ancestor-with-helix (helper curr-dir ".helix"))
  (define ancestor-with-jj    (helper curr-dir ".jj"))
  (define ancestor-with-git   (helper curr-dir ".git"))

  (cond
    [ancestor-with-helix (cons ancestor-with-helix #f)]
    [ancestor-with-jj    (cons ancestor-with-jj #f)]
    [ancestor-with-git   (cons ancestor-with-git #f)]
    [else (cons DEFAULT_PROJECT_ROOT DEFAULT_PROJECT_HELIX)])
  )

(let ((project-root-and-helix (find-project-root)))
  (define project-root (car project-root-and-helix))
  (define project-helix (cdr project-root-and-helix))

  ; (log/info! . (string-append "project-root is " project-root))
  ; (log/info! . (string-append "project-helix is " project-helix))
  
  (set-project-root! project-root)
  (if project-helix (set-project-helix! project-helix))
)
