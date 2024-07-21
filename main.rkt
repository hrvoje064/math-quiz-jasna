#lang racket/gui

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>

(require "math-quiz.rkt")
;;; Starting the GUI
;;; ===========================================================
(send main-window show #t)
;;; disable popup menus
(disable/enable-popup-window-menu #f)
