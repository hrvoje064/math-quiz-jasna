#lang info
(define name "math-quiz")
(define gracket-launcher-libraries '("main.rkt"))
(define gracket-launcher-names '("math-quiz"))
(define collection "math-quiz")
(define deps '("gui-pkg-manager-lib"
               "net-lib"
               "drracket"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/math-quiz.scrbl" ())))
(define pkg-desc "Math quiz for grade 1,2,3 ...")
(define version "4.4.10")
(define pkg-authors '(Hrvoje Blazevic))
(define license '(Apache-2.0 OR MIT))
;;; making sure Racket package repo tests don't fail/time out
;;; there's nothing in main.rkt -- it just starts the GUI
(define test-omit-paths '("main.rkt"))
