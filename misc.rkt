#lang racket

(require racket/unsafe/ops)

;;; Functions required by more than one module

(define (list2string lst)
  "remove outer parentheses"
  (if (null? lst)
      ""
      (string-append (format "~a " (car lst))
                     (list2string (cdr lst)))))

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
         (compare-decimal (cdr input) (cdr calc) exponent in exponent))
        ((char=? (car input) (car calc))
         (compare (cdr input) (cdr calc) in exponent))
        (else #f)))

(define (compare-decimal input calc n in exponent)
  (cond
    ((zero? n) (truncate-result in exponent))
    ((or (empty? input) (empty? calc)) #f)
    ((char=? (car input) (car calc))
     (compare-decimal (cdr input) (cdr calc) (sub1 n) in exponent))
    (else #f)))

(define (truncate-result input exponent)
  (let* ((in-whole-len (string-length (number->string (exact-floor input))))
         (in-str (number->string input))
         (in-len (string-length in-str))
         (result (substring in-str 0 (min (+ in-whole-len 1 exponent) in-len))))
    (if (eq? (string-ref result (sub1 (string-length result))) #\.)
        (substring result 0 (sub1 (string-length result)))
        result)))

(module+ test
  (require rackunit)
  (check-equal? (truncate-result 1.23456 1) "1.2")
  (check-equal? (truncate-result 1.2345 0) "1")
  (check-equal? (truncate-result 1.23456 3) "1.234")
  (check-equal? (truncate-result .00019999 5) "0.00019")
  (check-equal? (truncate-result 9.999999999 4) "9.9999")
  (check-equal? (approx=ndp 9.99999999 9.999999998 0) "9")
  (check-equal? (approx=ndp 1.234567 1.23456999 5) "1.23456")
  (check-equal? (approx=ndp 9.9999999 9.9991234 3) "9.999")
  (check-equal? (approx=ndp .1001999 0.10012345 4) "0.1001")
  (check-false (approx=ndp 9.9998999 9.9999999 4))
  (check-equal? (append-shuffle '(1 2 3 4 5) '(10 20 30) '(100 200 300 400 500))
                '(100 10 200 1 300 20 400 2 500 30 3 4 5))
  (check-equal? (strip-spaces "  123   / 456 : 005 ") "123/456:005")
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

;;; Infix evaluator, with precedence and left/right associativity
;;; =============================================================

;;; this is for displaying special math symbols, as printed in math books
(define // (string->symbol (string (integer->char 247)))) ; division character
(define V (string->symbol (string (integer->char 8730)))) ; root character
;;; this also complicates the evaluator, as the same equation is used to display
;;; the equation itself, and to calculate the result.

(define (evaluate expr)
  "Infix evaluator respecting precedence and left or right associativity"
  (define operations
    (list (cons '^ (lambda (x y) (expt y x)))
          (cons V (lambda (x y) (expt y (/ 1 x))))
          (cons '* *) (cons // /) (cons '+ +) (cons '- -)))
  (define (get-op sym) (cdr (assq sym operations)))

  (define (handle-parens expr)
    "Dealing with parentheses first"
    (if (null? expr)
        null
        (if (pair? (car expr))
            (cons (evaluate (car expr)) (handle-parens (cdr expr)))
            (cons (car expr) (handle-parens (cdr expr))))))

  (define (right-associative ops expr)
    "Priming right associative operations (^)"
    (reverse (left-associative ops (reverse expr))))

  (define (left-associative ops expr) ; left associative operations
    "Dealing with left associative operations (V * // + -)"
    (if (null? expr)
        expr
        (let ((op (memq (second expr) ops)))
          (if op
              (cons ((get-op (car op)) (first expr) (third expr)) (cdddr expr))
              (cons (first expr) (cons (second expr)
                                       (left-associative ops (cddr expr))))))))

  (define (evaluate expr)
    "Precedence: (), ^(right assoc), V(left assoc),
    * or /(left assoc), + or -(left assoc)" 
    (cond
      ((not (null? (filter pair? expr)))
       (evaluate (handle-parens expr)))
      ((findf (lambda (x) (memq x `(^ ,V))) expr)
       (if (eq? '^ (findf (lambda (x) (memq x `(^ ,V))) expr))
           (evaluate (right-associative `(^) expr))
           (evaluate (left-associative `(,V) expr))))
      ((findf (lambda (x) (memq x `(* ,//))) expr)
       (evaluate (left-associative `(* ,//) expr)))
      ((findf (lambda (x) (memq x '(+ -))) expr)
       (evaluate (left-associative '(+ -) expr)))
      ((> (length expr) 1) (error "illegal expression in" 'evaluate expr))
      (else (car expr))))

  (evaluate expr))

(module+ test
  (require rackunit)
  (check-equal? (evaluate `(13)) 13)
  (check-equal? (evaluate '(1 + 2 + 3 - 4 + 5 - 6 + 7)) 8)
  (check-equal? (evaluate '((1 + 2) + (3 - 4) + (5 - 6 + 7))) 8)
  (check-equal? (evaluate `((((1 + 2) * 2) + (((3 - 4) + (5 - 6 + 7)) * 2)) ,// 2)) 8)
  (check-equal? (evaluate `((((1 + 2) * 3) + (((3 - 4) + (5 - 6 + 7)) * 3)) ,// 7)) 24/7)
  (check-equal? (evaluate `(2 * 3 * 4 + 3 * 4 * 5 + 4 * 5 * 6))
                (evaluate `((2 * 3 * 4) + (3 * 4 * 5) + (4 * 5 * 6))))
  (check-=
   (evaluate `((((1 + 2) * 3 ,// 3 * 3) + (((3 - 4) + (5 - 6 + 7 + 3 - (6 ,// 2))) * 3))
               ,// 7 * 5 - 55.)) -37.857142857 0.0000000002)
  (check-=
   (evaluate `(((1 + 2) * 3 ,// 3 * 3 + (3 - 4 + (5 - 6 + 7 + 3 - 6 ,// 2)) * 3)
               ,// 7 * 5 - 55.)) -37.857142857 0.0000000002) 
  (check-exn exn:fail? (lambda () (evaluate `( 3 + 4 / 5)))) ; / is not valid in evaluator
  (check-exn exn:fail? (lambda () (evaluate `( 3 + 4 // 5)))) ; // is not valid in evaluator
  (check-equal? (evaluate `(1 + 2 + (3 ^ 3))) 30)
  (check-equal? (evaluate `((10 - 8) * (2 ^ (2 ^ 2)) - 10)) 22)
  (check-equal? (evaluate `((10 - 8) * 2 ^ 3 ^ 2 - 10)) 1014)
  (check-equal? (evaluate `((10 - 8) * ((2 ^ 3) ^ 2) - 10)) 118)
  (check-equal? (evaluate `((10 - 8) * (2 ^ (3 ^ 2)) - 10)) 1014)
  (check-equal? (evaluate `((10 - 8) ^ 3 * (2 ^ (3 ^ 2)) - 10)) 4086)
  (check-equal? (evaluate `((10 - 8) ^ 3 * (2 ^ (3 ^ 2)) - (10 ^ 3))) 3096)
  (check-equal? (evaluate `((((10 - 8) ^ 3) * (2 ^ (3 ^ 2))) - (10 ^ 3))) 3096)
  (check-equal? (evaluate `((10 - 8) ^ 3 * 2 ^ (3 ^ 2) - 10 ^ 3)) 3096)
  (check-equal? (evaluate `((10 - 8) ^ 3 * 2 ^ 3 ^ 2 - 10 ^ 3)) 3096)
  (check-equal? (evaluate `(10 + 27 ,// 3 ^ (1 + 2) * 2 - 12)) 0)
  (check-equal? (evaluate `((6 - 1) + ((27 ,// (3 ^ 3)) * (7 - 4)))) 8)
  (check-equal? (evaluate `((6 - 1) + 27 ,// 3 ^ 3 * (7 - 4))) 8)
  (check-equal? (evaluate `(6 - (1 + 26) ,// 3 ^ 3 * (7 - 4) + 16 ,// 4 * 2)) 11)
  (check-equal? (evaluate `(1 + 2 + 4 ,// 4 ^ (3 - 3) * 4 ,// 2 ^ 4 - 3)) 1)
  (check-equal? (evaluate `(9 + 2 ^ 3 ^ 2 ^ 3 ,// 2 ^ 6561 - 2 * 5)) 0)
  (check-= (evaluate `(8 ^ 2 * 3,V 512 ,// 2 ^ 3 ^ 2)) 1 0.0000000001)
  (check-equal? (evaluate `(10 + 2 ^ 3 ^ 2 ,// 2,V 2 ^ 2,V 256 - 32)) 10)
  (check-= (evaluate `(10 + 2 ^ 3 ^ 2 ,// (2,V 2) ^ 2,V 256 - 32)) 10 0.0000000001)
  (check-equal? (evaluate `(10 + 2 ^ 3 ^ 2 ,// 2,V (2 ^ 2),V 256 - 32)) 10)
  (check-= (evaluate `(3,V ((2 + 4) ^ 2 - (1 + 2 - 1) ^ 2) ,// 3,V 32))
           1 0.000001)
  (check-= (evaluate `(5,V 1024 ,// 2 ^ 10)) 0.00390625 0.00000001)
  (check-equal? (evaluate `(3,V 8 ,// 2,V (2 ^ 2))) 1.0)
  (check-= (evaluate `(3,V 8 ,// 2,V 2 ^ 2)) 1 0.0000000001)
  )

;;; Exports
;;; =================================================================

(provide (all-defined-out))
