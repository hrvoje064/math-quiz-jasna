#lang racket

;;; Time conversion functions 

(define (min->time min)
  (let-values ([(h m) (quotient/remainder min 60)])
    (let ((hs (number->string h))
          (ms (if (< m 10)
                  (string-append "0" (number->string m))
                  (number->string m))))
      (string-append hs ":" ms))))

(define (time->min time)
  (let* ((tl (string-split time ":"))
         (h (string->number (first tl)))
         (m (string->number (second tl))))
    (+ (* h 60) m)))

(define (result->min hmstr)
  (let ((hml (map (lambda (s) (substring s 0 (sub1 (string-length s))))
                  (string-split hmstr))))
    (cond
      ((not (= (length hml) 2)) #f)
      (else
       (let ((h (string->number (first hml)))
             (m (string->number (second hml))))
         (and (number? h) (number? m) (+ (* h 60) m)))))))

(define (minstr->hmstr mstr)
  (let-values ([(h m) (quotient/remainder (string->number mstr) 60)])
    (string-append (number->string h) "h " (number->string m) "m")))

(module+ test
  (require rackunit)
  (check-equal? (min->time 207) "3:27")
  (check-equal? (min->time 9) "0:09")
  (check-equal? (time->min "0:09") 9)
  (check-equal? (time->min "11:17") 677)
  (check-equal? (result->min "3h 27m") 207)
  (check-equal? (result->min "   0h    9M  ") 9)
  (check-equal? (result->min "   0H    09M  ") 9)
  (check-equal? (result->min "3h 03m") 183)
  (check-equal? (result->min "11h 9m") 669)
  (check-equal? (result->min "1 9m") #f)
  (check-equal? (result->min "1 15") #f)
  (check-equal? (minstr->hmstr "7") "0h 7m")
  (check-equal? (minstr->hmstr "347") "5h 47m")
  (check-equal? (minstr->hmstr "683") "11h 23m")  
  )

;;; Export
;;; ===============================================================

(provide min->time time->min result->min minstr->hmstr)
