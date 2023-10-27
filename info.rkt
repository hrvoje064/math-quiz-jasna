#lang info
(define name "Math-quiz")
(define gracket-launcher-names '("Racket Math Quiz"))
(define collection "math-quiz")
(define deps '("drracket"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/math-quiz.scrbl")))
(define pkg-desc "Math quiz for grade 1,2,3 ...")
(define version "1.0")
(define pkg-authors '(Hrvoje Blazevic))
(define license '(Apache-2.0 OR MIT))
;;; making sure Racket package repo tests don't fail/time out
;;; ther's nothing in main.rkt -- it just starts the GUI
(define test-omit-paths '("main.rkt"))