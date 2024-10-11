#lang racket

(require "misc.rkt")

(define operators '(- + - + -))
(define *ops-level* 1)
(define (set-ops-level! v) (set! *ops-level* v))

(define (parse-operator in)
  (let ((op (string-trim in)))
    (if (and (= (string-length op) 1) (member op '("+" "-")))
        op
        #f)))

(define (all? lst)
  (if (null? lst)
      #t
      (and (car lst) (all? (cdr lst)))))

(define (parse-ops-input prompts raw-ops)
  (define (parse ps ops)
    (if (null? (cddr ps))
        (list (string-append (car ps) " ") (cadr ps))
        (cons (car ps) (cons (car ops) (parse (cdr ps) (cdr ops))))))
  (let ((operators (map parse-operator raw-ops)))
    (if (not (all? operators))
        #f
        (let ((ops (map (λ (x) (string-append " " x " ")) operators)))
          (if (= (- (length prompts) (length ops)) 2)
              (apply string-append (parse prompts ops))
              #f)))))

(define (get-problem-operators)
  (let ((n-ops *ops-level*))
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
           (operators (map symbol->string (filter symbol? term)))
           (prompts
            (append (map number->string (filter number? term))
                    (list (string-append "=  " (number->string result)))))
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
      (list numbers input answer (list operators prompts result)))))

(define (invalid-answer1-ops y) ; for tests in math-quiz.rkt
  (let ((operators (car y)))
    (cons "*" (cdr operators))))

(define (invalid-answer2-ops y) ; for tests in math-quiz.rkt
  (cdar y)) 
  
(define (wrong-answer-ops y) ; for tests in math-quiz.rkt
  (let ((operators (car y)))
    (if (string=? (car operators) "+")
        (cons "-" (cdr operators))
        (cons "+" (cdr operators)))))

      
(provide parse-ops-input get-problem-operators wrong-answer-ops set-ops-level!
         invalid-answer1-ops invalid-answer2-ops *ops-level*)

;;; ===================================================

(module+ test
  (require rackunit)

  (check-not-exn (λ () (set-ops-level! 1)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (set-ops-level! 2)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (set-ops-level! 3)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (get-problem-operators)))
  (check-not-exn (λ () (set-ops-level! 1)))

  (check-equal? (parse-ops-input '("10" "20" "= 30") '("+")) "10 + 20 = 30")
  (check-equal? (parse-ops-input '("1" "2" "3" "= 6") '("+" "+")) "1 + 2 + 3 = 6")
  (check-equal? (parse-ops-input '("100" "20" "30" "5" "= 55") '("-" "-" "+"))
                "100 - 20 - 30 + 5 = 55")

  (check-false (parse-ops-input '("10" "20" "= 30") '()))
  (check-false (parse-ops-input '("10" "20" "= 30") '("*")))
  (check-false (parse-ops-input '("1" "2" "3" "= 6") '("+")))
  (check-false (parse-ops-input '("1" "2" "3" "= 6") '("+" "x")))
  (check-false (parse-ops-input '("100" "20" "30" "5" "= 55") '("-" "+")))
  (check-false (parse-ops-input '("100" "20" "30" "5" "= 55") '("-" "/" "+")))

  )

