#lang racket

(require "misc.rkt")

;;; ===============================================================
;;; Missing number in a sequence (IQ) problem for math-quiz program
;;; ===============================================================

(define IQ-seq-limit 21)
(define IQ-seq-limit-fib 12)

;;; easy exercises - just incrementing by a constant (1 - 5) and reverse

(define (get-inc)
  (let* ((inc-lst (list (random 1 3) (random 1 6)))
         (inc (list-ref inc-lst (random (length inc-lst)))))
    inc))

(define (get-inc-dec)
  (let* ((inc-dec-lst
          (map (lambda (p) (cons (first p) (list (- (second p)))))
               (combinations '(5 4 3 2 1) 2)))
         (inc-dec (list-ref inc-dec-lst (random (length inc-dec-lst)))))
    inc-dec))

(define (get-sequence-1inc begin)
  (let ((inc (get-inc)))
    (build-list 5 (lambda (i) (+ begin (* i inc))))))

(define (get-sequence-1+- acc inc dec n)
  (if (zero? n)
      (reverse acc)
      (get-sequence-1+- (cons (+ inc (car acc)) acc) dec inc (sub1 n))))

(define (get-sequence-1)
  (let ((begin (random 0 IQ-seq-limit)))
    (if (even? (random 5)) ; inc version 3/5 weighted
        (get-sequence-1inc begin)
        (let ((inc-dec (get-inc-dec)))
          (get-sequence-1+- (list begin) (first inc-dec) (second inc-dec) 4)))))

(module+ test
  (require rackunit)
  (check-equal? (map (lambda (p) (cons (first p) (list (- (second p)))))
                     (combinations '(5 4 3 2 1) 2))
                '((5 -4) (5 -3) (5 -2) (5 -1) (4 -3) (4 -2) (4 -1) (3 -2) (3 -1) (2 -1)))
  )

(define (get-sequence-1-2)
  (let* ((inc (get-inc))
         (start (random 0 IQ-seq-limit))
         (lst (build-list 5 (lambda (i) (+ start (* i inc))))))
    lst))

;;; medium exercises increment is incrementing

(define inc-inc1 '(0 0 1 3 6)) ; increment-increment by 1

(define (get-sequence-2)
  (let* ((seq1 (get-sequence-1-2))
         (inc (random 1 4)) ; 1 - 3 inc
         (inc-lst (make-list inc inc-inc1))
         (seq2 (add-sequences seq1 inc-lst)))
    seq2))

(define (add-sequences seq lsts)
  (apply map + (cons seq lsts)))

(module+ test
  (check-equal? (add-sequences '(18 20 22 24 26) (make-list 1 '(0 0 1 3 6)))
                '(18 20 23 27 32))
  (check-equal? (add-sequences '(18 20 22 24 26) (make-list 3 '(0 0 1 3 6)))
                '(18 20 25 33 44))
  )

;;; tough sequences; Fibonacci, prime, inc=exp inc=exp */+ incremented, * n

(define (exp-inc*sequence) ; tough
  (let ((begin (random 0 IQ-seq-limit))
        (inc (random 1 4))) ; max 3
    (build-list 5 (lambda (i) (+ begin (* i (* i inc)))))))

(define (exp-inc+sequence) ; tough
  (let ((begin (random 0 IQ-seq-limit))
        (inc (random 1 4))) ; max 3
    (build-list 5 (lambda (i) (+ begin (* i (+ i inc)))))))

(define (fibonacci n acc1 acc2 seq)
  (if (zero? n)
      (cons acc1 seq)
      (fibonacci (- n 1) acc2 (+ acc1 acc2) (cons acc1 seq))))

(define (fibonacci-sequence n ac1 ac2)
  (reverse (fibonacci n ac1 ac2 '())))

(define (expt-sequence)
  (let* ((begin (random 0 5))
         (exponent (random 2 4)) ; max 3
         (seq (build-list 5 (lambda (i) (+ i begin)))))
    (map (lambda (x) (expt x exponent)) seq)))

(define (get-primes n)
  (let ((pl (cdr (build-list n (lambda (i) (add1 i))))))
    (define (sieve acc nl)
      (if (null? nl)
          (reverse acc)
          (let ((new-l (filter (lambda (n) (not (zero? (modulo n (car acc))))) nl)))
            (sieve (cons (car new-l) acc) (cdr new-l)))))
    (sieve '(2) pl)))

(define (prime-sequence)
  (let ((primes (get-primes 100)))
    (take (drop primes (random (- (length primes) 5))) 5)))

(module+ test
  (check-equal? (fibonacci-sequence IQ-seq-limit-fib 0 1)
                '(0 1 1 2 3 5 8 13 21 34 55 89 144))
  (check-equal? (fibonacci-sequence IQ-seq-limit-fib 4 7)
                '(4 7 11 18 29 47 76 123 199 322 521 843 1364))
  (let ()
    (random-seed 1)
    (check-equal? (expt-sequence) '(8 27 64 125 216))
    (check-equal? (exp-inc*sequence) '(2 5 14 29 50))
    (check-equal? (exp-inc+sequence) '(19 23 29 37 47))
    (check-equal? (get-sequence-2) '(10 13 17 22 28))
    (check-equal? (prime-sequence) '(71 73 79 83 89))
    (check-equal? (get-sequence-3) '(1 3 9 19 33))
    (check-equal? (get-sequence-3) '(7 11 18 29 47))
    (check-equal? (get-sequence-3) '(7 11 17 25 35))
    (check-equal? (get-sequence-3) '(3 6 15 30 51))
    (check-equal? (get-sequence-3) '(6 10 16 24 34))
    (check-equal? (get-sequence-3) '(2 6 8 14 22))
    (check-equal? (get-sequence-3) '(7 9 13 19 27)))
  )
  
(define (get-sequence-3)
  (let* ((fibs (lambda ()
                 (take (drop (fibonacci-sequence IQ-seq-limit-fib 0 1)
                             (random (- IQ-seq-limit-fib 4))) 5)))
         (acc1 (random 2 5))
         (acc2 (random (add1 acc1) 8))
         (fibs-x (lambda ()
                   (take (drop (fibonacci-sequence IQ-seq-limit-fib acc1 acc2)
                               (random (- IQ-seq-limit-fib 4))) 5)))
         (seq-lst
          (list fibs-x exp-inc*sequence fibs exp-inc+sequence
                fibs-x expt-sequence prime-sequence))
         (chosen (list-ref seq-lst (random (length seq-lst)))))
    (chosen)))

;;; Export

(provide get-sequence-1 get-sequence-2 get-sequence-3)
