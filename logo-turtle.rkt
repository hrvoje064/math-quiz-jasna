#lang racket

(require 2htdp/universe)
(require pict)
(require graphics/value-turtles)

;;; Drawings for math-quiz, fractions & clock 

;;; =================================================================
;;; Fractions
;;; =================================================================

;; prepare to draw a box around pie-chart
(define logo
  (set-pen-color (turn 90 (move 65 (turn 90 (move 65 (turtles 300 200)))))
                 "white"))

;; draw a box to prevent cropping pie-chart image
(define (logo-box n world)
  (if (zero? n)
      (turn 90 (move 65 (turn 90 (move 67 world))))
      (logo-box (sub1 n) (turn 90 (set-pen-color (draw 130 world) "white")))))

;; drawing a pie-chart
(define (n-slice r d n theta world)
  (cond ((zero? d) (clean world))
        ((zero? n)
         (n-slice r (sub1 d) 0 theta
                  (slice r theta
                         (turn 90
                               (draw r
                                     (set-pen-width
                                      (set-pen-color world "black") 5))))))
        (else
         (n-slice r (sub1 d)(sub1 n) theta
                  (slice r theta
                         (turn 90
                               (draw r
                                     (set-pen-width
                                      (set-pen-color world "red") 5))))))))
;; drawing one slice
(define (slice r theta world)
  (if (<= theta 0)
      (turn 180 (draw r (turn 90 world)))
      (slice r (sub1 theta) (draw 1 (turn 1 world)))))

;; return turtles-world with pie-chart drawn
;; if startup call, return a dummy world
(define (get-turtles *used-numbers* numerator denominator)
  (if (null? *used-numbers*) ; startup call
      (let ((denominator 1) (numerator 1))
        (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo)))
      (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo))))

;; return turtles-world with 2 pie-charts drawn
;; if startup call, return a dummy world
(define (get-turtles-left *used-numbers* x)
  (if (null? *used-numbers*) ; startup call
      (let ((denominator 1) (numerator 1))
        (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo)))
      (let* ((n/d (map string->number (string-split x "/")))
             (numerator (car n/d))
             (denominator (cadr n/d)))
        (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo)))))

(define (get-turtles-right *used-numbers* y)
  (if (null? *used-numbers*) ; startup call
      (let ((denominator 1) (numerator 1))
        (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo)))
      (let* ((n/d (map string->number (string-split y "/")))
             (numerator (car n/d))
             (denominator (cadr n/d)))
        (n-slice 60 denominator numerator (/ 360 denominator) (logo-box 4 logo)))))

(define (fraction-canvas-callback canvas dc *fraction-level* *used-numbers* x y)
  ;; clearing previous pie-chart
  (send dc draw-bitmap
        (pict->bitmap
         (disk 150 #:color "white" #:border-color "white"))
        70 46)
  (send dc draw-bitmap
        (pict->bitmap
         (disk 150 #:color "white" #:border-color "white"))
        290 46)
  ;; drawing new pie-chart
  (case *fraction-level*
    ((1) (send dc draw-bitmap
               (pict->bitmap (turtles-pict
                              (get-turtles *used-numbers* x y))) 80 55))
    ((2 3 4) (send dc draw-bitmap
                 (pict->bitmap (turtles-pict
                                (get-turtles-left *used-numbers* x))) 80 55)
           (send dc draw-bitmap
                 (pict->bitmap (turtles-pict
                                (get-turtles-right *used-numbers* y))) 300 55))
    (else (error '*fraction-level*))))

;;; =================================================================
;;; Clock
;;; =================================================================

;;; funcions for drawing clock

(define logo1
  (set-pen-color (turn 90 (move 130 (turn 90 (move 130 (turtles 400 300)))))
                 "white"))

;; draw a box to prevent cropping pie-chart image
(define (clock-box n world)
  (if (zero? n)
      (turn 180 (move 130 (turn 90 (move 130 world))))
      (clock-box (sub1 n) (turn 90 (set-pen-color (draw 260 world) "white")))))

(define clock-world
  (set-pen-color (turn -90 (move 115 (clock-box 4 logo1))) "gray"))

(define (draw-circle r theta world)
  (if (<= theta 0)
      (turn 180 (move r (turn -90 world)))
      (draw-circle r (sub1 theta) (draw 2 (turn -1 world)))))

(define (draw-hub r theta world)
  (if (<= theta 0)
      (turn 180 (move r (turn -90 world)))
      (draw-hub r (- theta 16) (draw 1 (turn -16 world)))))

(define (minute-marks r theta r-delta m-theta world)
  (if (<= theta 0)
      world
      (minute-marks
       r (- theta m-theta) r-delta m-theta
       (turn (- m-theta)
             (turn 180 (move r (turn 180 (draw r-delta (move (- r r-delta) world))
                                     )))))))

(define (hour-hand r h m world)
  (let ((theta (+ (* h 30) (/ m 2)))
        (d (* r 3/5)))
    (turn (+ 180 theta)
          (move d
                (turn 180
                      (draw d (set-pen-color (turn (- theta) world) "purple")))))))
    
(define (minute-hand r m world)
  (let ((theta (* m 360/60))
        (d (* r 3/4)))
    (turn (+ 180 theta)
          (move d
                (turn 180
                      (draw d (set-pen-color (turn (- theta) world) "black")))))))

(define (draw-clock r h m)
  (let* ((world1
          (set-pen-color (draw-circle r 360 (set-pen-width clock-world 5)) "black"))
         (world2 (minute-marks r 360 5 (/ 360 60) (set-pen-width world1 2)))
         (world3 (minute-marks r 360 7 (/ 360 12) (set-pen-width world2 3)))
         (world4 (minute-hand r m (set-pen-width world3 4)))
         (world5 (hour-hand r h m (set-pen-width world4 7)))
         (world6 ; drawing hub
          (draw-hub 8 360 (turn -90 (draw 7 (set-pen-color world5 "black"))))))
    (clean world6)))

;; return turtles-world with pie-chart drawn
;; if startup call, return a dummy world
(define (get-clock-turtles *used-numbers* hour minute)
  (if (null? *used-numbers*) ; startup call
      (let ((hour 0) (minute 0))
        (draw-clock 114 hour minute))  
      (draw-clock 114 hour minute)))

(define (clock-canvas-callback canvas dc *used-numbers* x y)
  ;; clearing previous clock
  (send dc draw-bitmap
        (pict->bitmap
         (disk 250 #:color "white" #:border-color "white"))
        70 46)
  ;; drawing new clock
  (send dc draw-bitmap
        (pict->bitmap (turtles-pict
                       (get-clock-turtles *used-numbers* x y))) 80 30))

;;; Export
;;; ==========================================================

(provide clock-canvas-callback fraction-canvas-callback)
