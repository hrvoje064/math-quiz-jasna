#lang racket

(require racket/string)
(require racket/file)

;;; Fixing secref hyperlinks in stabdalone binary distribution
;;; ==========================================================

;; (define home-path "file:///home/hrvoje064/Projects/math-quiz/scribblings/math-quiz.html")

(define (fix-html path home-path)
  (let* ((new-path (string-append "file://" path))
         (new-html
          (map (lambda (line) (string-replace line home-path new-path))      
               (file->lines path #:mode 'text)))
         (check-file
          (string-append
           (substring path 0 (- (string-length path) 14)) "fixed-secref.txt")))
    (display-lines-to-file new-html path #:mode 'text #:exists 'replace)
    (display-lines-to-file
     '((FIXED-SECREF-LINKS)) check-file #:mode 'text #:exists 'replace)))

(provide fix-html)
