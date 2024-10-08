#lang racket

(require "misc.rkt")

(define operators '(- + - + -))

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
         (verify (length (string-split in))))
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
      (if (or (< verify 5)
              (memq #f term-solution)
              (not (good-term term-solution number? operator?))) ; student tried to cheat!
          #f
          (evaluate term-solution)))))

(define (get-problem-operators)
  (let* ((nop-list '(3 2 1 2 3))
         (n-ops (list-ref nop-list (random (length nop-list)))))
    (define (get-term n acc)
      (if (zero? n)
          acc
          (let ((op (list-ref operators (random (length operators))))
                (val (evaluate acc)))
            (when (< val 4) (set! op '+))
            (if (eq? op '-)
                (get-term (sub1 n)
                          (append acc (cons op (list (random 2 (sub1 val))))))
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

(provide parse-input get-problem-operators wrong-answer-ops invalid-answer-ops)

;;; ===================================================

;;; test terms
(define term "77 [-] 15 [+] 5 [-] 33 = 34")
(define term1 " 77 -] 15   [+ 5 - 33  =    34 ")
(define term2 "77 - 15  +  5 - 33 = 34")
(define term3 "22 +[] [12] = [34]")
(define term4 "77 - 15 + 5 -x 33 = 34")
(define term5 "77 [*] 15 / 5 -x 33 = 34")
(define term6 "77 [] 15 [+] 5 [-] 33 = 34")
(define term7 "77 [+] [-] 15 + 33 = 34")

(module+ test
  (require rackunit)

  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))

  (check-eqv? (parse-input term) 34)
  (check-eqv? (parse-input term1) 34)
  (check-eqv? (parse-input term2) 34)
  (check-eqv? (parse-input term3) 34)
  (check-false (parse-input "34"))
  (check-false (parse-input term4))
  (check-false (parse-input term5))
  (check-false (parse-input term6))
  (check-false (parse-input term7))
  )

