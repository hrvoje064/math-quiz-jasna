#lang racket

(require "misc.rkt")

(define operators '(- + - + -))
(define *ops-level* 1)
(define (set-ops-level! v) (set! *ops-level* v))

(define n-op-list12 '(2 1 2 2 2 1 2))
(define n-op-list3 '(3))

(define (parse-input in)
  (let* ((term+result (string-split in "="))
         (result (string->number (string-trim (last term+result))))
         (term
          (apply
           append
           (map
            string-split
            (map string-trim
                 (apply append
                        (map (λ (x) (string-split x "]"))
                             (string-split (first term+result) "[")))))))
         (n-terms (length (string-split in)))
         (verify?
          (case *ops-level*
            ((1) (>= n-terms 5))
            ((2) (= n-terms 9))
            (else (error 'parse-input "*ops-level* invalid value")))))
    (define (operator? x)
      (memq x '(+ -)))
    (define (num-or-sym x)
      (let ((n (string->number x)))
        (cond
          (n)
          ((member x '("+" "-")) (string->symbol x))
          (else #f))))
    (define (good-term term p p2)
      (if (null? term)
          #t
          (and (p (car term)) (good-term (cdr term) p2 p))))
    (let ((term-solution (map num-or-sym term)))
      ;(println term-solution)
      (if (or (not verify?)
              (memq #f term-solution)
              (not (good-term term-solution number? operator?))) ; student tried to cheat!
          #f
          (evaluate term-solution)))))

(define (get-problem-operators)
  (let* ((nop-list (case *ops-level*
                     ((1) n-op-list12)
                     ((2) n-op-list3)
                     (else (error 'get-problem-operators "*ops-level* out of range"))))
         (n-ops (list-ref nop-list (random (length nop-list)))))
    (define (get-term n acc)
      (if (zero? n)
          acc
          (let ((op (list-ref operators (random (length operators))))
                (val (evaluate acc)))
            (when (< val 4) (set! op '+))
            (if (eq? op '-)
                (get-term (sub1 n)
                          (append acc (cons op (list (random 2 (min 100 (sub1 val)))))))
                (get-term (sub1 n)
                          (append acc (cons op (list (random 9 100)))))))))
    (let* ((term (get-term n-ops (list (random 39 100))))
           (result (evaluate term))
           (expanded (append term (list '= result)))
           (answer
            (apply
             string-append
             (map (λ (x) (if (number? x)
                             (number->string x)
                             (string-append " " (symbol->string x) " "))) expanded)))
           (numbers (filter number? (cdr (reverse expanded))))
           (input
            (apply
             string-append
             (map (λ (x) (cond ((number? x) (number->string x))
                               ((symbol? x) (string-append " " (symbol->string x) " "))
                               (else (string-append " " x " "))))
                  (map (λ (x) (if (or (eq? x '-) (eq? x '+)) "[]" x)) expanded)))))
      (list numbers input answer result))))

(define (invalid-answer-ops term-str) ; for tests in math-quiz.rkt
  (let ((term-lst (string->list term-str)))
    (if (memv #\+ term-lst)
        (list->string (filter (λ (x) (not (char=? x #\+))) term-lst))
        (list->string (filter (λ (x) (not (char=? x #\-))) term-lst)))))

(define (wrong-answer-ops term-str) ; for tests in math-quiz.rkt
  (let ((term-lst (string->list term-str)))
    (if (memv #\+ term-lst)
        (list->string (map (λ (x) (if (char=? x #\+) #\- x)) term-lst))
        (list->string (map (λ (x) (if (char=? x #\-) #\+ x)) term-lst)))))  

(provide parse-input get-problem-operators wrong-answer-ops invalid-answer-ops
         *ops-level* set-ops-level!)

;;; ===================================================

;;; test terms
(define term31 "77 [-] 15 [+] 5 [-] 33 = 34") ; ok
(define term32 " 77 -] 15   [+ 5 - 33  =    34 ") ; ok
(define term33 "77 - 15  +  5 - 33 = 34") ; ok
(define term11 "22 +[] [12] = [34]") ; ok
(define term12 "22 + [12] = [34]") ; ok
(define term13 "22 [*] [12] = [34]") ; invalid
(define term14 "22 [-] = 34") ; invalid
(define term15 "22 12 = 34") ; invalid
(define term34 "77 - 15 + 5 -x 33 = 34") ; invalid
(define term35 "77 [*] 15 / 5 -x 33 = 34") ; invalid
(define term36 "77 [] 15 [+] 5 [-] 33 = 34") ; invalid
(define term37 "77 [+] [-] 15 + 33 = 34") ; invalid
(define term21 "22 + 22 - 10 = 34") ; ok
(define term22 "22 [+] 22 [-] 10 = 34") ; ok
(define term23 "22 + 22 [] 10 = 34") ; invalid
(define term24 "22 + 22 * 10 = 34") ; invalid
(define term25 "22 + - 10 = 34") ; invalid

(module+ test
  (require rackunit)

  (check-exn exn:fail? (λ () (set-ops-level! 3) (get-problem-operators)))
  (check-exn exn:fail? (λ () (set-ops-level! 3) (parse-input term31)))
  (check-not-exn (λ () (set-ops-level! 2)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (set-ops-level! 1)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))

  (check-not-exn (λ () (set-ops-level! 2)))
  (check-eqv? (parse-input term31) 34)
  (check-eqv? (parse-input term32) 34)
  (check-eqv? (parse-input term33) 34)
  (check-false (parse-input term34))
  (check-false (parse-input term35))
  (check-false (parse-input term36))
  (check-false (parse-input term37))

  (check-not-exn (λ () (set-ops-level! 1)))
  (check-eqv? (parse-input term21) 34)
  (check-eqv? (parse-input term21) 34)
  (check-eqv? (parse-input term22) 34)
  (check-false (parse-input term23))
  (check-false (parse-input term24))
  (check-false (parse-input term25))

  (check-eqv? (parse-input term11) 34)
  (check-eqv? (parse-input term12) 34)
  (check-false (parse-input term13))
  (check-false (parse-input term14))
  (check-false (parse-input term15))
    
  (check-false (parse-input "34"))

  (check-not-exn (λ () (set-ops-level! 1)))
  )

