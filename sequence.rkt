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

(define (get-sequence-1)
  (let* ((inc (get-inc))
         (start (random 0 IQ-seq-limit))
         (lst (build-list 5 (lambda (i) (+ start (* i inc))))))
    lst))

;;; medium exercises increment is incrementing

(define inc-inc1 '(0 0 1 3 6)) ; increment-increment by 1

(define (get-sequence-2)
  (let* ((seq1 (get-sequence-1))
         (inc (random 1 4)) ; 1 - 3 inc
         (inc-lst (make-list inc inc-inc1))
         (seq2 (add-sequences seq1 inc-lst)))
    seq2))

(define (add-sequences seq lsts)
  (apply map + (cons seq lsts)))

(module+ test
  (require rackunit)
  (check-equal? (add-sequences '(18 20 22 24 26) (make-list 1 '(0 0 1 3 6)))
                '(18 20 23 27 32))
  (check-equal? (add-sequences '(18 20 22 24 26) (make-list 3 '(0 0 1 3 6)))
                '(18 20 25 33 44))
  )

;;; tough sequences; Fibonacci, inc=exp inc=exp */+ incremented, * n

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
    (check-equal? (get-sequence-2) '(10 13 17 22 28)))
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
          (list fibs-x exp-inc*sequence fibs exp-inc+sequence fibs-x expt-sequence))
         (chosen (list-ref seq-lst (random (length seq-lst)))))
    (chosen)))

;;; Export

(provide get-sequence-1 get-sequence-2 get-sequence-3)
