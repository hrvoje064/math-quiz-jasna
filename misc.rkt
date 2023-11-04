#lang racket

(require racket/unsafe/ops)

;;; Functions required by more than one module

;;; for GAPESA
;;; ================================================================
(define (append-shuffle L . Lists)
  "Append with shuffle"
  (define (shuffle L1 L2)
    (cond
      ((null? L2) L1)
      ((null? L1) L2)
      (else
       (cons (car L1) (cons (car L2) (shuffle (cdr L1) (cdr L2)))))))
  (foldl (lambda (L R) (shuffle L R)) L Lists))

;;; for clock & fractions
;;; ================================================================
(define (strip-spaces strng)
  (define (strip lst)
    (cond ((null? lst) lst)
          ((eq? (car lst) #\space) (strip (cdr lst)))
          (else (cons (car lst) (strip (cdr lst))))))
  (list->string (strip (string->list strng))))

;;; for sequence
;;; ================================================================
(define *n-s* (make-base-namespace))

(define (apply-map f lol)
  (eval `(,map ,f ,@(map (lambda (l) `(quote ,l)) lol)) *n-s*))

;;; How about this? Also works, about the same speed
;(define (apply-map f lol)
;    (eval (cons map (cons f (map (lambda (l) `(quote ,l)) lol))) *n-s*))

(define (up-down lst)
  (let* ((rev-lst '(#f #t #f))
         (reverse? (list-ref rev-lst (random (length rev-lst)))))
    (if reverse?
        (reverse lst)
        lst)))

;;; for division and GAPESA /
;;; ===============================================================
;;; approximately equal (for division)
;;; only counts exponent digits after decimal point
;;; returns number-string truncated to decimal places (exponent)
(define (approx=ndp input calc exponent)
  (let ((in-int (exact-floor (* input (expt 10 exponent))))
        (calc-int (exact-floor (* calc (expt 10 exponent)))))
    (cond
      ((= in-int calc-int) (truncate-result input exponent))
      ((> (abs (- calc-int in-int)) 1) #f)
      (else
       (compare (string->list (number->string input))
                (string->list (number->string (exact->inexact calc)))
                input exponent)))))

(define (compare input calc in exponent)
  (cond ((and (char=? (car input) #\.) (char=? (car calc) #\.))
         (compare-decimal (cdr input) (cdr calc) exponent in))
        ((char=? (car input) (car calc)) (compare (cdr input) (cdr calc) in exponent))
        (else #f)))

(define (compare-decimal input calc n in)
  (cond
    ((zero? n) (truncate-result in))
    ((or (empty? input) (empty? calc)) #f)
    ((char=? (car input) (car calc))
     (compare-decimal (cdr input) (cdr calc) (sub1 n) in))
    (else #f)))

(define (truncate-result input exponent)
  (let* ((in-whole-len (string-length (number->string (exact-floor input))))
         (in-str (number->string input))
         (in-len (string-length in-str)))
    (substring in-str 0 (min (+ in-whole-len 1 exponent) in-len))))

(module+ test
  (require rackunit)
  (check-equal? (truncate-result 1.23456 3) "1.234")
  (check-equal? (truncate-result .00019999 5) "0.00019")
  (check-equal? (truncate-result 9.999999999 4) "9.9999")
  (check-equal? (approx=ndp 1.234567 1.23456999 5) "1.23456")
  (check-equal? (approx=ndp 9.9999999 9.9991234 3) "9.999")
  (check-equal? (approx=ndp .1001999 0.10012345 4) "0.1001")
  (check-equal? (append-shuffle '(1 2 3 4 5) '(10 20 30) '(100 200 300 400 500))
                '(100 10 200 1 300 20 400 2 500 30 3 4 5))
  (check-equal? (strip-spaces "  123   / 456 : 005 ") "123/456:005")
  (check-equal? (apply-map list '((1 2 3) (a b c) (10 20 30)))
                  '((1 a 10) (2 b 20) (3 c 30)))
  (check-equal? (apply-map * '((1 2 3) (2 2 2) (10 20 30))) '(20 80 180))
  (check-equal? (apply-map + '((1 2 3))) '(1 2 3))
  (check-equal? (apply-map (compose1 reverse list) '((a b c) (d e f)))
                '((d a) (e b) (f c)))
  )

;;; for ABC sort & GAPESA
;;; ====================================================================

(define (nth! before lst n) ; finding nth item and removing it from list
  (cond
    ((zero? n) (unsafe-set-immutable-cdr! before (cdr lst))
               (car lst))
    (else
     (nth! (cdr before) (cdr lst) (sub1 n)))))

(define (list-copy lst) ; utility procedure - copy the spine of list
  (if (null? lst)
      null
      (cons (car lst) (list-copy (cdr lst)))))

(module+ test
  (let ((TL (list 'handle 1 2 3)))
    (check-equal?
     (nth! TL (cdr TL) 0) 1)
    (check-equal? TL '(handle 2 3))
    (check-equal?
     (nth! TL (cdr TL) 1) 3)
    (check-equal? TL '(handle 2))
    (check-equal?
     (nth! TL (cdr TL) 0) 2)
    (check-equal? TL '(handle))))

;;; Infix evaluator
;;; =========================================================

(define (evaluate x)
  "A quick & dirty infix expression evaluator, used for result calculation"
  (cond
    ((number? x) x)
    ((null? (cdr x)) (evaluate (car x)))
    ((pair? (car x)) (evaluate (cons (evaluate (car x)) (cdr x))))
    (else
     (evaluate (cons (calc (take x 3)) (cdddr x))))))

(define (calc t) ((operation (second t)) (first t) (evaluate (third t))))

(define d/v (string->symbol (string (integer->char 247)))) ; division character
(define arith-ops (list (cons '+ +) (cons '- -) (cons '* *) (cons d/v /)))
(define (operation x) (cdr (assq x arith-ops)))

(module+ test
  (require rackunit)
  (check-equal? (evaluate '(1 + 2 + 3 - 4 + 5 - 6 + 7)) 8)
  (check-equal? (evaluate '((1 + 2) + (3 - 4) + (5 - 6 + 7))) 8)
  (check-equal? (evaluate `((((1 + 2) * 2) + (((3 - 4) + (5 - 6 + 7)) * 2)) ,d/v 2)) 8)
  (check-equal? (evaluate `((((1 + 2) * 3) + (((3 - 4) + (5 - 6 + 7)) * 3)) ,d/v 7)) 24/7)
  (check-=
   (evaluate `((((1 + 2) * 3 ,d/v 3 * 3) + (((3 - 4) + (5 - 6 + 7 + 3 - (6 ,d/v 2))) * 3))
               ,d/v 7 * 5 - 55.)) -37.857142857 0.0000000002)
  (check-exn exn:fail? (lambda () (evaluate '( 3 + 4 / 5)))) ; / is not valid in evaluator
  ) 

;;; Exports
;;; =================================================================

(provide (all-defined-out))
