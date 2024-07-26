#lang racket

(define ordinal '(ordinal)) ; show only

(define (spell-ordinal n-str)
  (let* ((len (string-length n-str))
         (lastd (string-ref n-str (sub1 len)))
         (butlastd (if (> len 1) (string-ref n-str (- len 2)) #\0)))
    (string-append n-str
                   (cond
                     ((char=? butlastd #\1) "th")
                     (else
                      (case lastd
                        ((#\1) "st")
                        ((#\2) "nd")
                        ((#\3) "rd")
                        ((#\4 #\5 #\6 #\7 #\8 #\9 #\0) "th")))))))

(define (get-problem-ordinal)
  (let* ((n-choice '((10 20) (20 150) (0 50) (1 5) (11 15)))
         (len (length n-choice))
         (num (apply random (list-ref n-choice (random len))))
         (x (number->string num)))
    (values x ordinal (spell-ordinal x))))

(define (check-postfix ord)
  (member ord '("th" "st" "nd" "rd")))

#;(define (test-ord n)
    (if (zero? n)
        'done
        (let-values ([(x op y) (get-problem-ordinal)])
          (println (list x (car op) y))
          (test-ord (sub1 n)))))

(module+ test
  (require rackunit)
  (check-equal? (spell-ordinal "0") "0th")
  (check-equal? (spell-ordinal "1") "1st")
  (check-equal? (spell-ordinal "2") "2nd")
  (check-equal? (spell-ordinal "3") "3rd")
  (check-equal? (spell-ordinal "11") "11th")
  (check-equal? (spell-ordinal "12") "12th")
  (check-equal? (spell-ordinal "13") "13th")
  (check-equal? (spell-ordinal "21") "21st")
  (check-equal? (spell-ordinal "32") "32nd")
  (check-equal? (spell-ordinal "103") "103rd")
  (check-equal? (spell-ordinal "211") "211th")
  (check-equal? (spell-ordinal "112") "112th"))
  
;;; Exports
;;; ====================================================

(provide get-problem-ordinal check-postfix)



