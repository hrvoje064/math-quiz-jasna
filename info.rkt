#lang info
(define collection "math-quiz")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/math-quiz.scrbl" ())))
(define pkg-desc "Math quiz for grade 1,2,3 ...")
(define version "1.0")
(define pkg-authors '(Hrvoje Blazevic))
(define license '(Apache-2.0 OR MIT))
