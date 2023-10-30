#lang racket/gui

;(module+ test
;  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>

;(module+ main
;  ;; (Optional) main submodule. Put code here if you need it to be executed when
;  ;; this file is run using DrRacket or the `racket` executable.  The code here
;  ;; does not run when this file is required by another module. Documentation:
;  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
;  )

(require "math-quiz.rkt")
;;; Starting the GUI
;;; ===========================================================
(send main-window show #t)
;;; disable popup menus
(disable/enable-popup-window-menu #f)
