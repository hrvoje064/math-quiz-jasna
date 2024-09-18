#lang racket

;; v5.1

(require "misc.rkt") ; for definition of //

;;; Fraction & multiplication / division table domain tables
;;; and get-problem functions operating on them
;;; ========================================================

;;; All this is transfered from math-quiz.rkt
(define *max-slices* 10) ; max size of pie-slice (denominator)
(define *max-denominator* 15) ; Max numerator & denominator in fraction comparison
(define *max*table* 10) ; max size for multiplication / division tables
(define *n* 20) ; default number of problems in exercise

(struct state (question problems mistakes) #:mutable)
;; initialize question to 1, problems to *n*, mistakes to 0
(define *state* (state 1 *n* 0))
(struct problem (x op y z) #:mutable) ; z is late entry for Before/After clock
(define *problem* (problem #f #f #f #f))

(define plus (cons '+ +))
(define minus (cons '- -))
(define mult (cons '* *))
(define div (cons // /))
(define fract (cons 'red-parts/all-parts /))
(define comp<=> '(<=>)) ; only show for comparisons. User has to input operation
(provide mult div fract comp<=>)

(define (initialize-problem x op y)
  (set-problem-x! *problem* x)
  (set-problem-op! *problem* op)
  (set-problem-y! *problem* y))

;;; This is to pacify Racket compiler. These global values must be changed
;;; here, and not in the main file that imports them
(define (setn! n v) (set! *n* v))
(define (setmt! n v) (set! *max*table* v))
(define (setms! n v) (set! *max-slices* v))
(define (setmd! n v) (set! *max-denominator* v))

;;; Exporting back to math-quiz.rkt
(provide *max-slices* *max*table* *n* *max-denominator*
         initialize-problem setn! setmt! setms! setmd! plus minus)
(provide (struct-out state) (struct-out problem) *state* *problem*)

;;; Exporting new functions to math-quiz
(provide clear-persistent-tables get-problem10*10 get-problem100/10
         get-problem-fraction get-problem-fraction<=> get-problem-fraction-full<=>
         get-problem<=>fract get-problem<=>fract1 get-problem*f get-problem/quotf7
         get-problem-f5 get-problem-f6 get-problem-f7 get-problem-f8 get-problem-f9)

;;; get-problem functions with persistent table state
;;; ==========================================================

(module tables racket
  (provide pairs sett! clear-tables)
  (define table null)
  (define (pairs) table)
  (define (sett! v) (set! table v))
  (define (clear-tables) (set! table null)))

(require 'tables)

;;; aux functions
;;; =====================================================================

(define lower-c 1/25) ; lower limit to start checking proximity of fractions
(define upper-c 1/5) ; stop checking

(define fn1 caar)
(define fn2 caadr)
(define fd1 cadar)
(define fd2 cadadr)

(define (make-all-pairs n)
  (shuffle
   (combinations (shuffle (combinations (build-list n add1) 2)) 2)))

(define (make-table limit)
  (shuffle
   (append
    (combinations (build-list limit add1) 2)
    (build-list limit (λ (i) (list (add1 i) (add1 i)))))))

(define (interleave l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    (else (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2)))))))

;;; ===============================================================
;;; Fraction comparison tables
;;; ===============================================================

(define (group-num-den-rest pairs)
  (define (insert p r)
    (let ((num (first r)) (den (second r)) (rest (third r)))
      (cond ((= (fn1 p) (fn2 p)) (list (cons p num) den rest))
            ((= (fd1 p) (fd2 p)) (list num (cons p den) rest))
            (else (list num den (cons p rest))))))                             
  (let* ((groups (foldr insert (list null null null) pairs))
         (nums (shuffle (first groups)))
         (dens (shuffle (second groups)))
         (rest (shuffle (third groups))))
    (values nums dens rest)))

(define (same-numerators n)
  (let-values ([(numerators d r) (group-num-den-rest (make-all-pairs n))])
    numerators))

(define (same-denominators n)
  (let-values ([(n denominators r) (group-num-den-rest (make-all-pairs n))])
    denominators))

(define (mix-nums-dens n)
  (let-values ([(numerators denominators r)
                (group-num-den-rest (make-all-pairs n))])
    (interleave numerators denominators)))

(define (closest-fractions v pairs)
  (define (insert p r)
    (let ((chosen (first r)) (rest (second r)))
      (cond ((< (abs (- (/ (fn1 p) (fd1 p)) (/ (fn2 p) (fd2 p)))) v)
             (list (cons p chosen) rest))
            (else (list chosen (cons p rest))))))                             
  (let* ((groups (foldr insert (list null null) pairs))
         (close (shuffle (first groups)))
         (rest (shuffle (second groups))))
    (values close rest)))

(define (closest v acc pairs)
  (let-values ([(close rest) (closest-fractions v pairs)])
    (if (and (> v upper-c) (< (length close) 10))
        (closest (+ 1/100 v) (append acc close) rest)
        (if (< (length close) 30)
            (append close rest)
            close))))

;;; =============================================================
;;; Fraction arithmetic tables
;;; =============================================================

;; (1) (zero? (modulo d1 d2)), (2) (> (gcd d1 d2) 1, (3) rest
(define (group-div-gcd-rest pairs)
  (define (insert p r)
    (let ((ddiv (first r)) (dgcd (second r)) (rest (third r))
                           (d1 (fd1 p)) (d2 (fd2 p)))
      (cond ((= d1 d2) (list ddiv dgcd rest))
            ((zero? (modulo (max d1 d2) (min d1 d2)))
             (list (cons p ddiv) dgcd rest))
            ((> (gcd d1 d2) 1) (list ddiv (cons p dgcd) rest))
            (else (list ddiv dgcd (cons p rest))))))                             
  (let* ((groups (foldr insert (list null null null) pairs))
         (divisibles (shuffle (first groups)))
         (gcds (shuffle (second groups)))
         (rest (shuffle (third groups))))
    (values divisibles gcds rest)))

(define (d1-in-d2 n)
  (let-values ([(d1ind2 g r) (group-div-gcd-rest (make-all-pairs n))])
    d1ind2))

(define (d1-gcd-d2 n)
  (let-values ([(d dgcd r) (group-div-gcd-rest (make-all-pairs n))])
    dgcd))

(define (d1-nothing-d2 n)
  (let-values ([(d g rest) (group-div-gcd-rest (make-all-pairs n))])
    rest))
 
(define (d1-mix-d2 n)
  (let-values ([(ddiv dgcd rest) (group-div-gcd-rest (make-all-pairs n))])
    (interleave (interleave ddiv dgcd) rest)))

;; get-problem functions
  
(define (get-problem10*10)
  (when (null? (pairs)) (sett! (make-table *max*table*)))
  (let ((pair (car (pairs))))
    (sett! (cdr (pairs)))
    (initialize-problem (first pair) mult (second pair))))

(define (get-problem100/10)
  (when (null? (pairs)) (sett! (make-table *max*table*)))
  (let* ((pair (car (pairs)))
         (x (first pair))
         (y (second pair)))
    (sett! (cdr (pairs)))
    (initialize-problem (* x y) div y)))

;; graphical fractions
(define (get-problem-fraction) ; just any fraction - reading only
  (when (null? (pairs))
    (sett! (make-table *max-slices*)))
  (let ((pair (car (pairs))))
    (sett! (cdr (pairs)))
    (initialize-problem (first pair) fract (second pair))))

;; graphical fractions
(define (get-problem-fraction<=>) ; either same denominators or same numerators
  (when (null? (pairs)) (sett! (mix-nums-dens *max-slices*)))
  (fraction<=> comp<=>))

(define (fraction<=> op) ; help function
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x op y)))

;; graphical fractions
(define (get-problem-fraction-full<=>)
  (when (null? (pairs))
    (sett! (closest lower-c null (make-all-pairs *max-slices*))))
  (fraction<=> comp<=>))

;; textual comparison

(define (get-problem<=>fract) ; either same denominators or same numerators
  (when (null? (pairs)) (sett! (mix-nums-dens *max-denominator*)))
  (fraction<=> comp<=>))

;; textual comparison
(define (get-problem<=>fract1) ; different numerators & denominators
  (when (null? (pairs))
    (sett! (closest lower-c null (make-all-pairs *max-slices*))))
  (fraction<=> comp<=>))
  
;;; arithmetic problems
;;; (+ -) functions

(define (fractions+-)
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b))))
         (op-list (list minus plus minus plus minus)) ; minus weighted 3/5
         (op (list-ref op-list (random (length op-list)))))
    (sett! (cdr (pairs)))
    (initialize-problem x op y)))

(define (get-problem-f5)
  (when (null? (pairs))
    (sett! (same-denominators *max-denominator*)))
  (fractions+-))

(define (get-problem-f6)
  (when (null? (pairs))
    (sett! (d1-in-d2 *max-denominator*)))
  (fractions+-))

(define (get-problem-f7)
  (when (null? (pairs))
    (sett! (d1-gcd-d2 *max-denominator*)))
  (fractions+-))

(define (get-problem-f8)
  (when (null? (pairs))
    (sett! (d1-nothing-d2 *max-denominator*)))
  (fractions+-))
  
(define (get-problem-f9)
  (when (null? (pairs))
    (sett! (d1-mix-d2 *max-denominator*)))
  (fractions+-))

;;; *

(define (get-problem*f)
  (when (null? (pairs))
    (sett! (d1-nothing-d2 *max-denominator*)))
  (fraction<=> mult))

;;; /

(define (get-problem/quotf7)
  (when (null? (pairs))
    (sett! (d1-nothing-d2 *max-denominator*)))
  (fraction<=> div))

;; cleaning the store
(define clear-persistent-tables clear-tables)

;;; testing
;;; ========================================================

(define mt '((1 1) (3 3) (4 4) (1 2) (4 5) (3 5) (1 4) (2 4) (1 5)
                   (2 3) (2 2) (2 5) (3 4) (5 5) (1 3)))

(define mallp ; (make-all-pairs 4)
  '(((2 4) (1 2))
    ((2 4) (1 3))
    ((1 3) (3 4))
    ((2 4) (3 4))
    ((1 4) (3 4))
    ((1 3) (1 2))
    ((1 4) (1 2))
    ((1 4) (2 3))
    ((2 4) (2 3))
    ((3 4) (1 2))
    ((1 4) (1 3))
    ((1 4) (2 4))
    ((2 3) (1 3))
    ((2 3) (1 2))
    ((2 3) (3 4))))

(define d1ind2 ; (d1-in-d2 5)
  '(((3 4) (1 2)) ((2 4) (1 2)) ((1 2) (1 4))))

(define d1gcdd2 ; (d1-gcd-d2 6)
  '(((3 6) (1 4))
    ((3 4) (5 6))
    ((1 4) (1 6))
    ((3 4) (1 6))
    ((2 4) (5 6))
    ((2 4) (4 6))
    ((1 4) (4 6))
    ((3 6) (2 4))
    ((2 4) (1 6))
    ((3 4) (2 6))
    ((2 4) (2 6))
    ((3 4) (4 6))
    ((3 4) (3 6))
    ((1 4) (2 6))
    ((1 4) (5 6))))

(define d1nd2 ; (d1-nothing-d2 4)
  '(((1 3) (1 2))
    ((1 3) (1 4))
    ((1 3) (2 4))
    ((1 3) (3 4))
    ((2 3) (3 4))
    ((1 2) (2 3))
    ((1 4) (2 3))
    ((2 3) (2 4))))

(define d1mxd2 ; (d1-mix-d2 4)
  '(((1 2) (3 4))
    ((2 3) (3 4))
    ((1 4) (1 2))
    ((1 3) (1 2))
    ((2 4) (1 2))
    ((2 3) (1 2))
    ((1 3) (2 4))
    ((1 3) (3 4))
    ((2 3) (1 4))
    ((1 3) (1 4))
    ((2 3) (2 4))))

(define clse ; (closest lower-c null (make-all-pairs 4))
  '(((1 2) (2 4))
    ((1 3) (2 3))
    ((1 3) (1 2))
    ((1 4) (3 4))
    ((1 2) (2 3))
    ((1 4) (1 2))
    ((1 2) (3 4))
    ((1 3) (3 4))
    ((1 4) (1 3))
    ((3 4) (2 3))
    ((1 3) (2 4))
    ((3 4) (2 4))
    ((1 4) (2 4))
    ((1 4) (2 3))
    ((2 3) (2 4))))

(define samenum ; (same-numerators 5)
  '(((1 3) (1 5))
    ((1 4) (1 5))
    ((1 2) (1 5))
    ((2 3) (2 5))
    ((1 2) (1 3))
    ((2 3) (2 4))
    ((1 4) (1 3))
    ((2 5) (2 4))
    ((1 4) (1 2))
    ((3 4) (3 5))))

(define sameden ; (same-denominators 5)
  '(((2 4) (3 4))
    ((1 4) (2 4))
    ((2 5) (3 5))
    ((4 5) (3 5))
    ((1 4) (3 4))
    ((1 5) (2 5))
    ((1 5) (4 5))
    ((2 3) (1 3))
    ((4 5) (2 5))
    ((1 5) (3 5))))

(define mixnd ; (mix-nums-dens 4)
  '(((1 2) (1 4))
    ((3 4) (1 4))
    ((1 3) (1 2))
    ((1 3) (2 3))
    ((1 3) (1 4))
    ((2 4) (1 4))
    ((2 4) (2 3))
    ((2 4) (3 4))))

(define (all-in tl rl)
  (if (null? tl)
      #t
      (let* ((a (car tl))
             (b (list (second a) (first a))))
        (and (or (member a rl) (member b rl))
             (all-in (cdr tl) rl)))))

(define (denominator-not-1 table)
  (if (null? table)
      #t
      (let ((a (caar table))
            (b (cadar table)))
        (and (> (second a) 0) (> (second b) 0)
             (denominator-not-1 (cdr table))))))

(module+ test
  (require rackunit)
  (check-false (all-in (cons (list 17 17) mt) (make-table 5)))
  (check-true (all-in mt (make-table 5)))
  (check-equal? (length (make-table 5)) 15)
  (check-equal? (length (make-all-pairs 10)) 990)
  (check-true (all-in mallp (make-all-pairs 4)))
  (check-true (all-in d1ind2 (d1-in-d2 5)))
  (check-true (all-in d1gcdd2 (d1-gcd-d2 6)))
  (check-true (all-in d1nd2 (d1-nothing-d2 4)))
  (check-true (all-in d1mxd2 (d1-mix-d2 4)))
  (check-true (all-in clse (closest lower-c null (make-all-pairs 4))))
  (check-true (all-in samenum (same-numerators 5)))
  (check-true (all-in sameden (same-denominators 5)))
  (check-true (all-in mixnd (mix-nums-dens 4)))
  (check-equal? (length (d1-mix-d2 18)) 10812)
  (check-equal? (length (closest lower-c null (make-all-pairs 18))) 928)
  )

;;; =====================================================================

