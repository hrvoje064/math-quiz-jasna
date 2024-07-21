#lang racket

(define *digits* "01234056789")
(define rounding '(round)) ; show only

(define (get-problem-round)
  (let* ((lst (shuffle (string->list *digits*)))
         (n (random 3 (sub1 (length lst))))
         (x1 (string->number (list->string (drop lst n)))))
    (if (< x1 10)
        (get-problem-round)
        (let* ((xs (number->string x1))
               (dc (string-ref
                    xs (random (sub1 (string-length xs)))))
               (num-string (string-append "0" xs)))
          (if (char=? dc #\0)
              (get-problem-round)
              (let* ((x0 (split-in-3 num-string dc))
                     (x (if (string=? (car x0) "0")
                            (cons "" (cdr x0))
                            x0))                           
                     (y (nearest-round num-string dc))
                     (op rounding))
                (list x op y)))))))

(define (nearest-round ns dc)
  (let* ((cs (char-d->string dc))
         (tripple (split-in-3 ns dc))
         (bow (first tripple))
         (aft (third tripple))
         (aft0 (make-string (string-length aft) #\0)))
    (cond
      ((char=? dc #\9)
       (if (>= (string->number
                (char-d->string (string-ref aft 0))) 5)
           (begin
             (set! bow (number->string (add1 (string->number bow))))
             (set! aft (string-append "0" aft0)))
           (set! aft (string-append "9" aft0))))
      ((<= (string->number
            (char-d->string (string-ref aft 0))) 4)
       (set! aft (string-append cs aft0)))
      (else
       (set! aft (string-append
                  (number->string
                   (add1 (string->number cs))) aft0))))
    (string->number (string-append bow aft))))

(define (char-d->string c)
  (number->string (- (char->integer c) 48)))

(define (split-in-3 number-s digit-c)
  (let* ((ds (char-d->string digit-c))
         (h-t (string-split number-s ds)))
    (append (list (number->string (string->number (first h-t))))
            (list ds) (cdr h-t))))
       
#;(define (testr n)
    (if (zero? n)
        'done
        (begin
          (println (get-problem-round))
          (testr (sub1 n)))))

(module+ test
  (require rackunit)
  (check-equal? (nearest-round "0971230" #\9) 1000000)
  (check-equal? (nearest-round "090023" #\9) 90000)
  (check-equal? (nearest-round "010456" #\1) 10000)
  (check-equal? (nearest-round "093028" #\3) 93000)
  (check-equal? (nearest-round "010456" #\4) 10500)
  (check-equal? (nearest-round "090570" #\5) 90600))

;;; Exports
;;; ====================================================

(provide get-problem-round)
