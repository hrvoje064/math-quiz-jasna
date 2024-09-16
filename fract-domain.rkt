#lang racket

;; v5.0

(require "misc.rkt") ; for definition of //

;;; Fraction & multiplication / division table domain tables
;;; and get-problem functions operating on them
;;; ========================================================

;;; All this is transfered from math-quiz.rkt
(define *max-slices* 10) ; max size of pie-slice (denominator)
(define *comparison-fract-max* 12) ; Max numerator & denominator in fraction comparison
(define *max*table* 10) ; max size for multiplication / division tables
(define *n* 20) ; default number of problems in exercise

(struct state (question problems mistakes) #:mutable)
;; initialize question to 1, problems to *n*, mistakes to 0
(define *state* (state 1 *n* 0))
(struct problem (x op y z) #:mutable) ; z is late entry for BA clock
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


;;; Exporting back to math-quiz.rkt
(provide *max-slices* *max*table* *n* *comparison-fract-max*
         initialize-problem setn! setmt! setms! plus minus)
(provide (struct-out state) (struct-out problem) *state* *problem*)
(provide clear-persistent-tables get-problem10*10 get-problem100/10
         get-problem-fraction get-problem-fraction<=> get-problem-fraction-full<=>
         get-problem<=>fract get-problem<=>fract1 get-problem-f1
         get-problem-f2 get-problem*f get-problem/quotf7)

;;; get-problem functions with persistent table state
;;; ==========================================================

;;; Dummy definitions to be set inside let with real definitions
(define clear-persistent-tables #f)
(define get-problem10*10 #f)
(define get-problem100/10 #f)
(define get-problem-fraction #f)
(define get-problem-fraction<=> #f)
(define get-problem-fraction-full<=> #f)
(define get-problem<=>fract #f)
(define get-problem<=>fract1 #f)
(define get-problem-f1 #f)
(define get-problem-f2 #f)
(define get-problem*f #f)
(define get-problem/quotf7 #f)
;; for testunit only - not exported
(define make-table-test #f)
(define make-table2-test #f)
(define get-fraction-full-test #f)

(let ((pairs null))

  ;; aux functions
  
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
      (shuffle (tuples (car pairs) (cdr pairs)))))

  ;; get-problem functions
  
  (define (get-problem10*)
    (when (null? pairs) (set! pairs (make-table *max*table*)))
    (let ((pair (car pairs)))
      (set! pairs (cdr pairs))
      (initialize-problem (first pair) mult (second pair))))

  (define (get-problem100/)
    (when (null? pairs) (set! pairs (make-table *max*table*)))
    (let* ((pair (car pairs))
           (x (first pair))
           (y (second pair)))
      (set! pairs (cdr pairs))
      (initialize-problem (* x y) div y)))

  ;; graphical fractions
  (define (get-problem-fract)
    (when (null? pairs)
      (set! pairs (filter (λ (p) (<= (first p) (second p)))
                          (make-table *max-slices*))))
    (let ((pair (car pairs)))
      (set! pairs (cdr pairs))
      (initialize-problem (first pair) fract (second pair))))

  ;; graphical fractions
  (define (get-problem-fract<=>)
    (when (null? pairs) (set! pairs (make-table2 *max-slices*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x comp<=> y)))

  ;; graphical fractions
  (define (get-problem-fract-full<=>)
    (when (null? pairs)
      (set! pairs (get-fraction-full-table *max-slices*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x comp<=> y)))

  ;; textual comparison
  (define (get-problem<=>)
    (when (null? pairs) (set! pairs (make-table2 *comparison-fract-max*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x comp<=> y)))

  ;; textual comparison
  (define (get-problem1<=>)
    (when (null? pairs)
      (set! pairs (get-fraction-full-table *comparison-fract-max*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x comp<=> y)))

  ;; arithmetic problems
  (define (get-problem-f1+-)
    (when (null? pairs)
      (set! pairs (map swap-nd (make-table2 *comparison-fract-max*))))
    (let* ((op-list (list minus plus minus plus minus))
           (op (list-ref op-list (random (length op-list)))) ; minus weighted 3/5 
           (pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x op y)))

  (define (get-problem-f2+-)
    (when (null? pairs)
      (set! pairs (get-fraction-full-table *comparison-fract-max*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b))))
           (op-list (list minus plus minus plus minus)) ; minus weighted 3/5
           (op (list-ref op-list (random (length op-list)))))
      (set! pairs (cdr pairs))
      (initialize-problem x op y)))

  (define (get-problem*frac)
    (when (null? pairs)
      (set! pairs (get-fraction-full-table *comparison-fract-max*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x mult y)))

  (define (get-problem/quotfrac7)
    (when (null? pairs)
      (set! pairs (get-fraction-full-table *comparison-fract-max*)))
    (let* ((pair (car pairs))
           (a (car pair))
           (b (cadr pair))
           (x (string-append
               (number->string (car a)) "/" (number->string (cadr a))))
           (y (string-append
               (number->string (car b)) "/" (number->string (cadr b)))))
      (set! pairs (cdr pairs))
      (initialize-problem x div y)))

  (define (clear-persistent)
    (set! pairs null))

  ;; local helper function
  (define (swap-nd pair)
    (let ((a (car pair)) (b (cadr pair)))
      (if (not (= (cadr a) (cadr b)))
          (list (list (cadr a) (car a))
                (list (cadr b) (car b)))
          pair)))

  ;; setting up real functions
  
  (set! get-problem10*10 get-problem10*)
  (set! get-problem100/10 get-problem100/)
  (set! get-problem-fraction get-problem-fract)
  (set! get-problem-fraction<=> get-problem-fract<=>)
  (set! get-problem-fraction-full<=> get-problem-fract-full<=>)
  (set! clear-persistent-tables clear-persistent)
  (set! get-problem<=>fract get-problem<=>)
  (set! get-problem<=>fract1 get-problem1<=>)
  (set! get-problem-f1 get-problem-f1+-)
  (set! get-problem-f2 get-problem-f2+-)
  (set! get-problem*f get-problem*frac)
  (set! get-problem/quotf7 get-problem/quotfrac7)
  ;; for rackunit
  (set! make-table-test make-table)
  (set! make-table2-test make-table2)
  (set! get-fraction-full-test get-fraction-full-table)

  )

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
  (check-false (all-in (cons (list 17 17) mt) (make-table-test 5)))
  (check-true (all-in mt (make-table-test 5)))
  (check-equal? (length (make-table-test 5)) 15)
  (check-false (all-in (cons (list (list 5 1) (list 4 1)) mt) (make-table2-test 6)))
  (check-true (denominator-not-1 (make-table2-test 6)))
  (check-equal? (length (make-table2-test 6)) 15)
  (check-equal? (length (get-fraction-full-test 10)) 45)
  (check-true (denominator-not-1 (get-fraction-full-test 10)))
  )



