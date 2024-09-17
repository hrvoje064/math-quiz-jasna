#lang racket

;; v5.0.3

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
         get-problem<=>fract get-problem<=>fract1 get-problem-f1
         get-problem-f2 get-problem*f get-problem/quotf7)

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

(define (make-table limit)
  (shuffle
   (append
    (combinations (build-list limit add1) 2)
    (build-list limit (λ (i) (list (add1 i) (add1 i)))))))

(define (make-table2 slices)
  (define (tuples lst)
    (cond
      ((null? lst) null)
      ((null? (cdr lst)) (list (list (car lst) (car lst)))) ; enabling equal
      (else
       (cons (list (car lst) (cadr lst)) (tuples (cddr lst))))))  
  (define (make-n-d-table x n acc)
    (define (tablen i acc1)
      (if (> i n)
          acc1
          (tablen (add1 i) (if (<= i x) acc1 (cons (list x i) acc1)))))
    (define (tabled i acc1)
      (if (> i n)
          acc1
          (tabled (add1 i) (if (>= i x) acc1 (cons (list i x) acc1)))))  
    (if (> x n)
        acc
        (make-n-d-table (add1 x) n
                        (cons (shuffle (tabled 1 null))
                              (cons (shuffle (tablen 2 null)) acc)))))
  (shuffle
   (apply append (map tuples (make-n-d-table 2 slices null)))))

(define (get-fraction-full-table slices)
  (let ((pairs (shuffle (combinations (build-list slices add1) 2))))
    (define (closest xf lst acc)
      (if (null? lst)
          (second acc)
          (let* ((yf (/ (caar lst) (cadar lst)))
                 (diff (abs (- yf xf))))
            (if (and (> diff 0) (< diff (car acc)))
                (closest xf (cdr lst) (list diff (car lst)))
                (closest xf (cdr lst) acc)))))   
    (define (tuples x pairs)
      (if (null? pairs)
          (list (list x x))
          (cons
           (list x (closest (/ (car x) (cadr x)) pairs (list 1 x)))
           (tuples (car pairs) (cdr pairs)))))
    (tuples (car pairs) (cdr pairs))))

(define (make-table-hard+- n)
  (let* ((t (shuffle (combinations (build-list n add1) 2))))
    (define (find-pairs f x pairs acc reject)
      (if (null? pairs)
          (values acc reject)
          (let* ((p (findf (λ (p) (f (list x p))) pairs))
                 (new-pairs (remove p pairs)))
            (if (null? new-pairs)
                (if p (list (cons (list x p) acc) reject)
                    (values acc (cons x reject)))
                (if p
                    (find-pairs f (car new-pairs) (cdr new-pairs)
                                (cons (list x p) acc) reject)
                    (find-pairs f (car pairs) (cdr pairs) acc (cons x reject)))))))
    (let-values ([(done to-do) (find-pairs rem=0? (car t) (cdr t) null null)])
      (let-values ([(done1 to-do1)
                    (find-pairs gcd<>1? (car to-do) (cdr to-do) done null)])
        (interleave (reverse done1) (make-pairs to-do1))))))

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
(define (get-problem-fraction)
  (when (null? (pairs))
    (sett! (filter (λ (p) (<= (first p) (second p)))
                   (make-table *max-slices*))))
  (let ((pair (car (pairs))))
    (sett! (cdr (pairs)))
    (initialize-problem (first pair) fract (second pair))))

;; graphical fractions
(define (get-problem-fraction<=>)
  (when (null? (pairs)) (sett! (make-table2 *max-slices*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x comp<=> y)))

;; graphical fractions
(define (get-problem-fraction-full<=>)
  (when (null? (pairs))
    (sett! (get-fraction-full-table *max-slices*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x comp<=> y)))

;; textual comparison
(define (get-problem<=>fract)
  (when (null? (pairs)) (sett! (make-table2 *max-denominator*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x comp<=> y)))

;; textual comparison
(define (get-problem<=>fract1)
  (when (null? (pairs))
    (sett! (get-fraction-full-table *max-denominator*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x comp<=> y)))

;; arithmetic problems
(define (get-problem-f1)
  (when (null? (pairs))
    (sett! (map swap-nd (make-table2 *max-denominator*))))
  (let* ((op-list (list minus plus minus plus minus))
         (op (list-ref op-list (random (length op-list)))) ; minus weighted 3/5 
         (pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x op y)))

(define (get-problem-f2)
  (when (null? (pairs))
    (sett! (make-table-hard+- *max-denominator*)))
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

(define (get-problem*f)
  (when (null? (pairs))
    (sett! (get-fraction-full-table *max-denominator*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x mult y)))

(define (get-problem/quotf7)
  (when (null? (pairs))
    (sett! (get-fraction-full-table *max-denominator*)))
  (let* ((pair (car (pairs)))
         (a (car pair))
         (b (cadr pair))
         (x (string-append
             (number->string (car a)) "/" (number->string (cadr a))))
         (y (string-append
             (number->string (car b)) "/" (number->string (cadr b)))))
    (sett! (cdr (pairs)))
    (initialize-problem x div y)))

(define clear-persistent-tables clear-tables)

;; local helper functions
(define (swap-nd pair)
  (let ((a (car pair)) (b (cadr pair)))
    (if (not (= (cadr a) (cadr b)))
        (list (list (cadr a) (car a))
              (list (cadr b) (car b)))
        pair)))

(define (rem=0? pair)
  (let* ((a (car pair))
         (b (cadr pair))
         (da (cadr a))
         (db (cadr b)))
    (and (not (= da db))
         (zero? (modulo (max da db) (min da db))))))

(define (gcd<>1? pair)
  (let* ((a (car pair))
         (b (cadr pair))
         (da (cadr a))
         (db (cadr b)))
    (and (not (= da db))
         (not (= 1 (gcd da db))))))

(define (make-pairs lst)
  (if (or (null? lst) (null? (cdr lst)))
      null
      (cons (list (car lst) (cadr lst))
            (make-pairs (cddr lst)))))

(define (interleave l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    (else (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2)))))))
    
;;; testing
;;; ========================================================

(define mt '((1 1) (3 3) (4 4) (1 2) (4 5) (3 5) (1 4) (2 4) (1 5)
                   (2 3) (2 2) (2 5) (3 4) (5 5) (1 3)))

(define (all-in tl rl)
  (if (null? tl)
      #t
      (and (member (car tl) rl) (all-in (cdr tl) rl))))

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
  (check-false (all-in (cons (list (list 5 1) (list 4 1)) mt) (make-table2 6)))
  (check-true (denominator-not-1 (make-table2 6)))
  (check-equal? (length (make-table2 6)) 15)
  (check-equal? (length (get-fraction-full-table 10)) 45)
  (check-true (denominator-not-1 (get-fraction-full-table 10)))
  (check-true (denominator-not-1 (make-table-hard+- 10)))
  (check-equal? (length (make-table-hard+- 10)) 21)
  )

;;; =====================================================================
