#lang racket

(require "misc.rkt")

;;; ================================================================
;;; Roman Numerals Exercises
;;; ================================================================

;;; Arabic to Roman

(define rn-list
  '((1 . "I") (2 . "II") (3 . "III") (4 . "IV") (5 . "V")
              (6 . "VI") (7 . "VII") (8 . "VIII") (9 . "IX")
              (10 . "X") (20 . "XX") (30 . "XXX") (40 . "XL") (50 . "L")
              (60 . "LX") (70 . "LXX") (80 . "LXXX") (90 . "XC")
              (100 . "C") (200 . "CC") (300 . "CCC") (400 . "CD") (500 . "D")
              (600 . "DC") (700 . "DCC") (800 . "DCCC") (900 . "CM")
              (1000 . "M") (2000 . "MM") (3000 . "MMM") (0 . "")))

(define (roman n)
  (let* ((ones (remainder n 10))
         (tens (remainder (- n ones) 100))
         (hundreds (remainder (- n ones tens) 1000))
         (thousands (- n ones tens hundreds)))
    (foldr
     (lambda (d rn) (string-append (cdr (assoc d rn-list)) rn)) ""
     (list thousands hundreds tens ones))))

;;; Roman to Arabic - redundant code

;;; math-quiz never uses this code because the arabic number is always known beforehand
;;; this code is included just for completeness sake, but is not exported

(define an-list
  (cdr (map (lambda (x) (cons (cdr x) (car x))) (reverse rn-list))))

(define (arabic rns)
  (if (zero? (string-length rns))
      0
      (let ((prefix (findf (string-prefix1? rns) (map car an-list))))
        (+ (cdr (assoc prefix an-list)) (arabic (string-trim rns prefix))))))

(define (string-prefix1? str)
  (lambda (prefix) (string-prefix? str prefix)))

(module+ test
  (require rackunit)
  (check-equal? (roman 3948) "MMMCMXLVIII")
  (check-equal? (roman 3094) "MMMXCIV")
  (check-equal? (roman 2888) "MMDCCCLXXXVIII")
  (check-equal? (arabic "MMMCMXLVII") 3947)
  (check-equal? (arabic "MXCIV") 1094)
  (check-equal? (arabic "MMDCCCLXXXIX") 2889))

;;; Export
;;; ================================================================

(provide roman)

