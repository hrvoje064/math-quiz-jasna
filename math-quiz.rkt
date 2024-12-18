#lang racket/gui

;;; Math Quiz, v5.4.9

(require net/sendurl)
(require racket/runtime-path)
(require pkg/gui)
(require pkg/lib)

(require "docs.rkt") ; docs.rkt file must be in the same directory as math-quiz
(require "word-problems.rkt") ; data for ABC-sort, and text-problems
(require "misc.rkt") ; functions required by several modules
(require "logo-turtle.rkt") ; clock & fractions drawings
(require "roman.rkt") ; Roman numerals conversion
(require "sequence.rkt") ; sequence problems
(require "html-fix.rkt") ; repairing secref hyperlinks in html docs
(require "rounding.rkt") ; rounding numbers on random position
(require "ordinal.rkt") ; spelling ordinal numbers
(require "time.rkt") ; time conversions
(require "domain-tables.rkt") ; persistent state for get-problem functions
(require "operators.rkt") ; missing operators

(define *speed-factor* 1) ; reduce or increase allotted time
(define *left-number* 700) ; Max size-1 of left number
(define *left-digit* 8) ; Max size-1 of left digit of left number
(define *exponent* 2) ; regulating division result precision (2 places)
(define *min-break-ex* 5) ; min number of exercises to enable suspend break button
(define *max-penalty-exercises* 5) ; a reasonable punishment for exceeding time
(define *max-roman-number* 100) ; default max roman number - can be adjusted to 3999
(define *max-toy-price* 300) ; default max price for cash return exercise
(define *payment* 500) ; default ammount paid by custommer
(define *cash-input-size* 30) ; width of each denomination input field

(define *sequence-difficulty* 1) ; difficulty level for sequence missing number
(define *cheat-flag* #f) ; set if cheat button used for sequence level 3(4)
(define *level+-* 1) ; (1 2d limited +) (2 2d limited +-) (3 3d unlimited +-)
(define *level/quot* 1) ; (1, 1d divisor) (2, 2d divisor)
(define *level** 2) ; (1 multiples of 10, 2 ordinary multiplication)
(define *comparison-level* 1) ; (1 integers) (2 limited fractions up to 12)
(define *clock-level* 1) ; (1 reading clock) (2 before/after 60, 30, 20, 15 minutes)
(define *fraction-level* 1) ; 1 reading fractions, 2 comparing fractions graphically
(define *max-skip-increment* 2) ; skip counting increment
(define *gapesa-level* 1) ; default word problem level, addition only
(define *Carea-level* 1) ; default circumference/area level
(define *findX-level* 1) ; default level for finding missing operand
(define *time-level* 1) ; default level for time exercises
(define *word-problem* #f) ; original word problem set
(define word-problem #f) ; copy of word problem set

(define get-problem #f) ; problem composing function
(define setup #f) ; problem setup function
(define do-math #f) ; which operation to run arithemetic or others
(define equal= #f) ; equal function (returns a number->string in (/) exercises)
(define get-sequence #f) ; type of sequence for IQ test
(define bell1 bell) ; bell can be disabled for tests

(define *time-start* #f)
(define *allowed-time* #f)
(define *time-factor* #f) ; minutes per problem
(define *wiwi-time* #f) ; pause time
(define *wiwi-start* #f)
(define *wiwi* #f) ; pause switch
(define *exec-button* #f) ; calc/comp button switch
(define *time-flag* #f) ; elapsed time flag for text exercises
(define *canvas-flag* #f) ; startup for empty canvas
(define *peso* #f) ; distinguishing between USD & Peso exercises
(define *before/after-clock* #f) ; time increments for before/after clock

(define *used-numbers* '()) ; avoiding repeating numbers
(define *max-used-pairs* #f) ; max allowed number of pairs in *used-numbers*

;;; Initializing fonts
;;; ==========================================================

;;; Font sizes (defaults if no j-m-q.init file
(define button-fnt-size (box #f))
(define msg-fnt-size (box #f))
(define msg-b-fnt-size (box #f))
(define doc-fnt-size (box #f))
(define about-fnt-size (box #f))
(define report-fnt-size (box #f))
(define *min-font* 7)
(define *max-font* 16)
(define all-fonts-delta #f)

(define all-font-list
  (list button-fnt-size msg-fnt-size msg-b-fnt-size doc-fnt-size
        about-fnt-size report-fnt-size))
(define all-font-defaults '(10 12 13 11 10 11)) ; fonts in points
  
(define *font-init-file* ".jmq-fontsrc")

;; At startup initializing fonts
(define (initialize-fonts)
  (if (file-exists? *font-init-file*)
      (begin
        #;(display (format "Loading font init file ~a~n" *font-init-file*))
        (call-with-input-file *font-init-file*
          (lambda (in)
            (with-handlers
                ([exn:fail?
                  (lambda (exn)
                    #;(display (format "Loading init file failed. File corrupted!~n"))
                    #;(displayln "Setting default fonts")
                    (set-default-fonts))])
              (for-each
               (lambda (f v) (set-box! f (legal-font (string->number v))))
               all-font-list
               (read-init-file in))))))
      (begin
        #;(display
           (format "Font init file ~a not found!~nSetting default fonts.~n"
                   *font-init-file*))
        (set-default-fonts))))

(define (read-init-file in)
  (define (mk-font-lst lst)
    (let ((v (read-line in 'any)))
      (if (eof-object? v)
          lst
          (cons v (mk-font-lst lst)))))
  (mk-font-lst '()))

(define (legal-font size)
  (if (and (number? size) (exact? size) (>= size *min-font*) (<= size *max-font*))
      size
      (error 'legal-font)))

(define (truncate-font-size size)
  (cond ((> size *max-font*) *max-font*)
        ((< size *min-font*) *min-font*)
        (else size)))

(define (set-default-fonts)
  (for-each (lambda (f v) (set-box! f v)) all-font-list all-font-defaults))
        
;; When user changes font size in preferences menu
(define (update-all-fonts delta)
  (with-handlers ([exn:fail? (lambda (exn) (set-default-fonts))])
    (call-with-output-file *font-init-file*
      (lambda (out)
        (for-each
         (lambda (f) (set-box! f (truncate-font-size (+ delta (unbox f))))
           (writeln (unbox f) out))
         all-font-list))
      #:mode 'text #:exists 'replace)))

;;; Disabling black terminal on Windows
;;; ==========================================================

(define (remap-port port flag)
  (cond
    [(eq? (system-type) 'unix) (if flag port (open-output-nowhere))]
    [(eq? (system-type) 'windows) (open-output-nowhere)]
    [(terminal-port? port) port]
    [else
     (open-output-nowhere)]))

(parameterize ([current-output-port (remap-port (current-output-port) #t)]
               [current-error-port  (remap-port (current-error-port) #f)])
  ;;; Initialize!
  (initialize-fonts))

;;; Dictionary for Alphabetic sort
;;; ==========================================================

(define dict null) ; working dictionary
(define words #f) ; number of words remaining in dict

(define (pick-n n start end) ; picking-out N words from dict 
  (cond
    ((zero? n) null)
    (else
     (set! words (sub1 words))
     (cons (nth! dict (cdr dict) (random start end))
           (pick-n (sub1 n) start end)))))

;;; ==============================================================
;;; built in operations, print form and exec form
;;; quot/rem ; just avoiding multiple values problem
(define (quot/rem x y)
  (let-values ([(res rem) (quotient/remainder x y)])
    (list res rem)))

(define divr (cons // quot/rem)) ; div with remainder
(define comp> (cons '> >))
(define comp< (cons '< <))
(define comp= (cons '= =))
(define is-odd (cons 'odd odd?))
(define is-even (cons 'even even?))
(define odd/even '(odd/even)) ; show only
(define IQ '(iq)) ; show only
(define A2R '(Arabic->Roman)) ; show only
(define R2A '(Roman->Arabic)) ; show only
(define MONEY '(Cash-returned)) ; show only
(define ABC '(Sorted)) ; show only
(define B/B/A '(bba)) ; show only
(define pvalue '(pvalue)) ; show only
(define clock '(:)) ; show only
(define run cdr)
(define show car)

;;; GUI part
;;; =================================================================

(define start-button-width 150)
(define start-button-height 30)

;;; fonts colors & message strings
;;; =================================================================

(define op-start-label "            ")
(define prompt-msg-label"Click one of Exercise buttons to run the exercises      ")
(define prompt-msg-label-again (string-append (string-trim prompt-msg-label) " again"))
(define input-label " ") ; 1 space to minimize input field???
(define *report-line-length* 52) ; length of reporting stub for GAPESA problems

(define message-font (make-object font%
                       (unbox msg-fnt-size) 'modern 'normal 'semibold))

(define message-bold-font
  (send the-font-list find-or-create-font
        (unbox msg-b-fnt-size); Size
        "Modern" ; Font face
        'default ; Font family 
        'normal ; Font style (italic...)
        'semibold)) ; Font weight

(define button-font
  (send the-font-list find-or-create-font
        (unbox button-fnt-size); Size
        "DejaVu Sans Mono" ; Font face
        'default ; Font family 
        'normal ; Font style (italic...)
        'semibold)) ; Font weight

;;; Text displayed in report canvas, instructions & about pane 
(define text-lines (new text% [auto-wrap #t]))
(define doc-instructions (new text% [auto-wrap #t]))
(define about-text (new text% [auto-wrap #t]))

(define style-delta-font-doc-size
  (make-object style-delta% 'change-size (unbox doc-fnt-size)))
(define style-delta-font-doc1-family
  (make-object style-delta% 'change-family 'modern))
(define style-delta-font-doc1-weight
  (make-object style-delta% 'change-weight 'semibold))
(send doc-instructions change-style style-delta-font-doc-size)

(define style-delta-font-doc2-family
  (make-object style-delta% 'change-family 'roman))
(define style-delta-font-doc2-weight
  (make-object style-delta% 'change-weight 'normal))

(define style-delta-font-about-size
  (make-object style-delta% 'change-size (unbox about-fnt-size)))
(define style-delta-font-about-family
  (make-object style-delta% 'change-family 'roman))
(send about-text change-style style-delta-font-about-size)

(define style-delta-font-report-size
  (make-object style-delta% 'change-size (unbox report-fnt-size)))
(define style-delta-font-report-family
  (make-object style-delta% 'change-family 'modern))
(send text-lines change-style style-delta-font-report-size)
(send text-lines change-style style-delta-font-report-family)

;; GAPESA prompt
(define text-prompt (new text% [auto-wrap #t]))
(send text-prompt change-style style-delta-font-report-size)
(send text-prompt change-style style-delta-font-report-family)

;; Text color
;; ======================================================

(define (fg-style name)
  (define s (make-object style-delta% 'change-normal-color))
  (send s set-delta-foreground name)
  s)

(define-syntax-rule (define-fg-styles [name color] ...)
  (define-values (name ...) (values (fg-style color) ...)))

(define-fg-styles
  [style-delta-red "red"]
  [style-delta-black "black"]
  [style-delta-blue "blue"]
  [style-delta-green "Forest Green"])

;; ===============================================================

;;; Changing all fonts up to +3 or -3
;; single font  (set-box! font size))

(define (change-font-size delta)
  (for-each (lambda (fnt) (set-box! fnt (+ (unbox fnt) delta)))
            all-font-list)) ; write this to the .jmq-fontrc file

;;; Main Window & geometry
;;; ================================================================

(provide main-window)
(define main-window (new frame%
                         [label "Jasna's math quiz"]
                         [width 790]
                         [height 600]
                         [alignment '(left top)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define main-pane (new horizontal-pane%
                       [parent main-window]
                       [alignment '(left top)]
                       [min-width 790]
                       [min-height 600]
                       [vert-margin 0]
                       [horiz-margin 0]                         
                       [stretchable-width #t]
                       [stretchable-height #t]))

;;; vertical main panes

(define v-pane-left (new vertical-pane%
                         [parent main-pane]
                         [vert-margin 0]
                         [horiz-margin 0]
                         [alignment '(left top)]
                         [min-width 590]
                         [min-height 600]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define v-pane-right (new vertical-pane%
                          [parent main-pane]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center top)]
                          [min-width 200]
                          [min-height 600]
                          [stretchable-width #f]
                          [stretchable-height #t]))

;;; horizontal panes 

(define h-pane-input (new horizontal-pane%
                          [parent v-pane-left]
                          [alignment '(left top)]
                          [min-width 570]
                          [min-height 40]
                          [vert-margin 5]
                          [horiz-margin 10]                         
                          [stretchable-width #t]
                          [stretchable-height #f]))

(define h-pane-prompt (new horizontal-pane%
                           [parent h-pane-input]
                           [alignment '(right center)]
                           [min-width 245]
                           [min-height 40]
                           [vert-margin 0]
                           [horiz-margin 0]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-result (new horizontal-pane%
                           [parent h-pane-input]
                           [alignment '(left center)]
                           [min-width 340]
                           [min-height 40]
                           [vert-margin 0]
                           [horiz-margin 0]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-status (new horizontal-pane%
                           [parent v-pane-left]
                           [alignment '(center top)]
                           [min-width 580]
                           [min-height 20]
                           [vert-margin 0]
                           [horiz-margin 5]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-report (new horizontal-pane%
                           [parent v-pane-left]
                           [alignment '(center top)]
                           [min-width 582]
                           [min-height 530]
                           [vert-margin 0]
                           [horiz-margin 4]                         
                           [stretchable-width #t]
                           [stretchable-height #t]))

;;; vertical inner panes

(define v-pane-stop (new vertical-pane%
                         [parent v-pane-right]
                         [alignment '(center center)]
                         [min-width 200]
                         [min-height 40]
                         [vert-margin 0]
                         [horiz-margin 0]                         
                         [stretchable-width #f]
                         [stretchable-height #f])) ; #t

(define v-pane-start (new vertical-pane%
                          [parent v-pane-right]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center top)]
                          [min-width 200]
                          [min-height 560]
                          [stretchable-width #f]
                          [stretchable-height #t])) ; #t

(define v-pane-pause (new vertical-pane%
                          [parent v-pane-start]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center center)]
                          [min-width 200]
                          [min-height 60]
                          [stretchable-width #f]
                          [stretchable-height #f])) ; #t

(define v-start-arithmetic (new vertical-pane%
                                [parent v-pane-start]
                                [vert-margin 5]
                                [horiz-margin 5]
                                [alignment '(center top)]
                                [min-width 190]
                                [min-height 240]
                                [stretchable-width #f]
                                [stretchable-height #f])) ; #t

;;; giving it scrolling space for future expansion
(define v-start-popup (new vertical-panel%
                           [parent v-pane-start]
                           [vert-margin 7]
                           [horiz-margin 5]
                           [alignment '(center bottom)]
                           [spacing 2]
                           [min-width 190]
                           [min-height 256]
                           [style '(border auto-vscroll)]
                           [stretchable-width #f]
                           [stretchable-height #t]))

;;; Menu
;;; =================================================================
(define main-menu-bar (new menu-bar%
                           [parent main-window]))

;;; Setup menu

(define setup-menu (new menu%
                        [parent main-menu-bar]
                        [label "Setup"]))

(define set-n-exercises (new menu-item%
                             [label "Set number of Exercises"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-n-dialog show #t))]))

(define set-+-level (new menu-item%
                         [label "Set + - difficulty level: integers or fractions"]
                         [parent setup-menu]
                         [callback
                          (lambda (mi e)
                            (send slider-+-dialog show #t))]))

(define set-max-table (new menu-item%
                           [label "Set max factor for * ÷ table"]
                           [parent setup-menu]
                           [callback
                            (lambda (mi e)
                              (send slider-10*-dialog show #t))]))

(define set-*-level (new menu-item%
                         [label "Set * level: 1, multiples of 10; 2, 3d*; 3, fractions"]
                         [parent setup-menu]
                         [callback
                          (lambda (mi e)
                            (send slider-100*-dialog show #t))]))

(define set-/quot-level (new menu-item%
                             [label "Set integer/fraction ÷ difficulty level"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-/quot-dialog show #t))]))

(define set-left-number (new menu-item%
                             [label "Set max size of numbers"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-left-dialog show #t))]))

(define set-speed-% (new menu-item%
                         [label "Set % of inc/dec allotted time"]
                         [parent setup-menu]
                         [callback
                          (lambda (mi e)
                            (send slider-speed-dialog show #t))]))

(define set-/-precision (new menu-item%
                             [label "Set division precision"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-precision-dialog show #t))]))

(define set-comparison-level (new menu-item%
                                  [label "Set comparison type: integer or fraction"]
                                  [parent setup-menu]
                                  [callback
                                   (lambda (mi e)
                                     (send slider-comparison-dialog show #t))]))

(define set-fraction-denominator (new menu-item%
                                      [label "Set max fraction denominator"]
                                      [parent setup-menu]
                                      [callback
                                       (lambda (mi e)
                                         (send slider-fraction-denom-dialog show #t))]))

(define set-sequence-level (new menu-item%
                                [label "Set sequence difficulty level"]
                                [parent setup-menu]
                                [callback
                                 (lambda (mi e)
                                   (send slider-sequence-dialog show #t))]))

(define set-fraction-slices (new menu-item%
                                 [label "Set number of fraction slices"]
                                 [parent setup-menu]
                                 [callback
                                  (lambda (mi e)
                                    (send slider-fraction-dialog show #t))]))

(define set-fraction-level (new menu-item%
                                [label "Set fraction level: read or compare"]
                                [parent setup-menu]
                                [callback
                                 (lambda (mi e)
                                   (send slider-fraction<>dialog show #t))]))

(define set-clock-level (new menu-item%
                             [label "Set clock level: tell time or before/after time"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-clock-dialog show #t))]))

(define set-max-roman-number (new menu-item%
                                  [label "Set max Roman number"]
                                  [parent setup-menu]
                                  [callback
                                   (lambda (mi e)
                                     (send slider-roman-dialog show #t))]))

(define set-skip-increment (new menu-item%
                                [label "Set max skip-count increment/decrement"]
                                [parent setup-menu]
                                [callback
                                 (lambda (mi e)
                                   (send slider-skip-dialog show #t))]))

(define set-text-level (new menu-item%
                            [label "Set GAPESA level"]
                            [parent setup-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-text-dialog show #t))]))

(define set-circumference-level (new menu-item%
                                     [label "Set Perimeter/Area level"]
                                     [parent setup-menu]
                                     [callback
                                      (lambda (mi e)
                                        (send slider-Carea-dialog show #t))]))

(define set-findX-level (new menu-item%
                             [label "Set missing Operand level"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-findX-dialog show #t))]))

(define set-operators-level (new menu-item%
                                 [label "Set missing Operators level"]
                                 [parent setup-menu]
                                 [callback
                                  (lambda (mi e)
                                    (send slider-ops-dialog show #t))]))

(define set-time-level (new menu-item%
                            [label "Set time calculation level"]
                            [parent setup-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-time-dialog show #t))]))

(define slider-n-dialog (new dialog%
                             [label "Set"]
                             [parent main-window]
                             [width 300] ; 250
                             [height 80]
                             [style '(close-button)]
                             [alignment '(right top)]))

(define slider-+-dialog (new dialog%
                             [label "Set"]
                             [parent main-window]
                             [width 400]
                             [height 80]
                             [style '(close-button)]
                             [alignment '(right top)]))

(define slider-/quot-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 320]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-10*-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define slider-100*-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 250]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define slider-left-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 300]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define slider-speed-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 360]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-precision-dialog (new dialog%
                                     [label "Set"]
                                     [parent main-window]
                                     [width 250]
                                     [height 80]
                                     [style '(close-button)]
                                     [alignment '(right top)]))

(define slider-comparison-dialog (new dialog%
                                      [label "Set"]
                                      [parent main-window]
                                      [width 250]
                                      [height 80]
                                      [style '(close-button)]
                                      [alignment '(right top)]))

(define slider-sequence-dialog (new dialog%
                                    [label "Set"]
                                    [parent main-window]
                                    [width 250]
                                    [height 80]
                                    [style '(close-button)]
                                    [alignment '(right top)]))

(define slider-fraction-dialog (new dialog%
                                    [label "Set"]
                                    [parent main-window]
                                    [width 250]
                                    [height 80]
                                    [style '(close-button)]
                                    [alignment '(right top)]))

(define slider-fraction-denom-dialog (new dialog%
                                          [label "Set"]
                                          [parent main-window]
                                          [width 280]
                                          [height 80]
                                          [style '(close-button)]
                                          [alignment '(right top)]))

(define slider-fraction<>dialog (new dialog%
                                     [label "Set"]
                                     [parent main-window]
                                     [width 250]
                                     [height 80]
                                     [style '(close-button)]
                                     [alignment '(right top)]))

(define slider-clock-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-roman-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 800]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-skip-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 400]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define slider-text-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 600]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define slider-Carea-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 640]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-findX-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 400]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-ops-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 400]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))  

(define slider-time-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 400]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define exercises-slider (new slider%
                              [label "number of exercises"]
                              [min-value 1]
                              [max-value 30]
                              [parent slider-n-dialog]
                              [init-value 20]
                              [callback
                               (lambda (s e)
                                 (setn! *n* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define level-+-slider (new slider%
                            [label "+ - exercises; 0-3 only positive, 4 negative, \
5-9 fractions"]
                            [min-value 0]
                            [max-value 9]
                            [parent slider-+-dialog]
                            [init-value *level+-*]
                            [callback
                             (lambda (s e)
                               (set! *level+-* (send s get-value)))]
                            [style '(vertical-label horizontal)]))

(define level-/quot-slider
  (new slider%
       [label "level of integer/fraction ÷ exe; all even levels with remainder; l7 fractions"]
       [min-value 1]
       [max-value 7]
       [parent slider-/quot-dialog]
       [init-value *level/quot*]
       [callback
        (lambda (s e)
          (set! *level/quot* (send s get-value)))]
       [style '(vertical-label horizontal)]))

(define max-table-slider (new slider%
                              [label "max factor * / table"]
                              [min-value 5]
                              [max-value 12]
                              [parent slider-10*-dialog]
                              [init-value 10]
                              [callback
                               (lambda (s e)
                                 (setmt! *max*table* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define 100-*-slider (new slider%
                          [label "* level: 1, multiples of 10; 2, 3d*; 3, fractions"]
                          [min-value 1]
                          [max-value 3]
                          [parent slider-100*-dialog]
                          [init-value 2]
                          [callback
                           (lambda (s e)
                             (set! *level** (send s get-value)))]
                          [style '(vertical-label horizontal)]))

(define left-slider (new slider%
                         [label "max size of numbers"]
                         [min-value 100]
                         [max-value 900]
                         [parent slider-left-dialog]
                         [init-value *left-number*]
                         [callback
                          (lambda (s e)
                            (let ((val (send s get-value)))
                              (set! *left-number* val)
                              (set! *left-digit* (quotient val 100))))] ; was add1
                         [style '(vertical-label horizontal)]))

(define speed-slider (new slider%
                          [label "% of dec/inc allotted time"]
                          [min-value 50]
                          [max-value 200]
                          [parent slider-speed-dialog]
                          [init-value (* 100 *speed-factor*)]
                          [callback
                           (lambda (s e)
                             (set! *speed-factor* (/ (send s get-value) 100)))]
                          [style '(vertical-label horizontal)]))

(define precision-slider (new slider%
                              [label "(/) precision - digits after (.)"]
                              [min-value 0]
                              [max-value 7]
                              [parent slider-precision-dialog]
                              [init-value *exponent*]
                              [callback
                               (lambda (s e)
                                 (set! *exponent* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define comparison-slider (new slider%
                               [label "1 Integer <-> Fraction 2 easy, 3 hard"]
                               [min-value 1]
                               [max-value 3]
                               (parent slider-comparison-dialog)
                               [init-value *comparison-level*]
                               [callback
                                (lambda (s e)
                                  (set! *comparison-level* (send s get-value)))]
                               [style '(vertical-label horizontal)]))

(define sequence-slider (new slider%
                             [label "Sequence difficulty level;  4=cheat"]
                             [min-value 1]
                             [max-value 4]
                             (parent slider-sequence-dialog)
                             [init-value *sequence-difficulty*]
                             [callback
                              (lambda (s e)
                                (set! *sequence-difficulty* (send s get-value)))]
                             [style '(vertical-label horizontal)]))

(define fraction-slider (new slider%
                             [label "Number of fraction slices"]
                             [min-value 5]
                             [max-value 12]
                             (parent slider-fraction-dialog)
                             [init-value *max-slices*]
                             [callback
                              (lambda (s e)
                                (setms! *max-slices* (send s get-value)))]
                             [style '(vertical-label horizontal)]))

(define fraction-denom-slider (new slider%
                                   [label "Maximum fraction denominator"]
                                   [min-value 6]
                                   [max-value 18]
                                   (parent slider-fraction-denom-dialog)
                                   [init-value *max-denominator*]
                                   [callback
                                    (lambda (s e)
                                      (setmd! *max-denominator* (send s get-value)))]
                                   [style '(vertical-label horizontal)]))

(define fraction<>slider (new slider%
                              [label "1 read, 2-3 easy compare, 4 compare"]
                              [min-value 1]
                              [max-value 4]
                              (parent slider-fraction<>dialog)
                              [init-value *fraction-level*]
                              [callback
                               (lambda (s e)
                                 (set! *fraction-level* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define clock-slider (new slider%
                          [label "1 <- tell time : before/after time 2 -> 5"]
                          [min-value 1]
                          [max-value 5]
                          (parent slider-clock-dialog)
                          [init-value *clock-level*]
                          [callback
                           (lambda (s e)
                             (set! *clock-level* (send s get-value)))]
                          [style '(vertical-label horizontal)]))

(define roman-slider (new slider%
                          [label "Maximum Roman Number"]
                          [min-value 10]
                          [max-value 3999]
                          (parent slider-roman-dialog)
                          [init-value *max-roman-number*]
                          [callback
                           (lambda (s e)
                             (set! *max-roman-number* (send s get-value)))]
                          [style '(vertical-label horizontal)]))

(define skip-slider (new slider%
                         [label "Max Skip Count Increment"]
                         [min-value 2]
                         [max-value 10]
                         (parent slider-skip-dialog)
                         [init-value *max-skip-increment*]
                         [callback
                          (lambda (s e)
                            (set! *max-skip-increment* (send s get-value)))]
                         [style '(vertical-label horizontal)]))

(define text-slider (new slider%
                         [label
                          (format
                           "GAPESA level: 1+ ,  2- ,  3+ or - ,  4+- , 5mix +- ,  \
6* , 7~a easy + fractions, 8*~a easy, 9~a ,  10*~a" // // // //)]
                         [min-value 1]
                         [max-value 10]
                         (parent slider-text-dialog)
                         [init-value *gapesa-level*]
                         [callback
                          (lambda (s e)
                            (set! *gapesa-level* (send s get-value)))]
                         [style '(vertical-label horizontal)]))

(define Carea-slider (new slider%
                          [label
                           (format
                            "Perimeter/Area level: 1 P-easy, 2 P-hard, 3 P-mix, 4 A-easy, \
5 P/A-easy, 6 A-hard, 7 A-mix, 8 P/A-hard")]
                          [min-value 1]
                          [max-value 8]
                          (parent slider-Carea-dialog)
                          [init-value *Carea-level*]
                          [callback
                           (lambda (s e)
                             (set! *Carea-level* (send s get-value)))]
                          [style '(vertical-label horizontal)]))

(define findX-slider (new slider%
                          [label
                           (format
                            "Missing operand level: 1 + -, 2 * /, 3 + - * /")]
                          [min-value 1]
                          [max-value 3]
                          (parent slider-findX-dialog)
                          [init-value *findX-level*]
                          [callback
                           (lambda (s e)
                             (set! *findX-level* (send s get-value)))]
                          [style '(vertical-label horizontal)]))

(define ops-slider (new slider%
                        [label
                         (format
                          "Missing operators level (+ -): 1 (1 ops), 2 (2 ops), 3 (3 ops)")]
                        [min-value 1]
                        [max-value 3]
                        (parent slider-ops-dialog)
                        [init-value *ops-level*]
                        [callback
                         (lambda (s e)
                           (set-ops-level! (send s get-value)))]
                        [style '(vertical-label horizontal)]))  

(define time-slider (new slider%
                         [label
                          (format
                           "time calculation level: 1 easy, 2 hard, 3 mixed")]
                         [min-value 1]
                         [max-value 3]
                         (parent slider-time-dialog)
                         [init-value *time-level*]
                         [callback
                          (lambda (s e)
                            (set! *time-level* (send s get-value)))]
                         [style '(vertical-label horizontal)]))

(define clear-reports (new menu-item%
                           [label "Clear all reports"]
                           [parent setup-menu]
                           [callback
                            (lambda (mi e)
                              (send text-lines erase)
                              (send text-lines change-style
                                    style-delta-font-report-size)
                              (send text-lines change-style
                                    style-delta-font-report-family))]))

;;; ==================================================================

;;; Preferences menu

(define preferences-menu (new menu%
                              [parent main-menu-bar]
                              [label "Preferences"]))

;;; Font submenues
;;; All fonts

(define set-all-fonts (new menu-item%
                           [label "Inc/Dec all fonts"]
                           [parent preferences-menu]
                           [callback
                            (lambda (mi e)
                              (send slider-all-dialog show #t)
                              ;; user didn't change slider
                              (if (or (not (number? all-fonts-delta))
                                      (zero? all-fonts-delta))
                                  (set! all-fonts-delta #f)
                                  (begin
                                    (update-all-fonts all-fonts-delta)
                                    (set! all-fonts-delta 0)
                                    (send set-all-fonts enable #f))))]))

(define slider-all-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define all-slider (new slider%
                        [label "Inc / Dec all font sizes
Restart program immediately after"]
                        [min-value -3]
                        [max-value +3]
                        [parent slider-all-dialog]
                        [init-value 0]
                        [callback
                         (lambda (s e)
                           (set! all-fonts-delta (send s get-value)))]
                        [style '(vertical-label horizontal)]))

;;; Individual fonts

(define set-status-msg-font (new menu-item%
                                 [label "Set status line font"]
                                 [parent preferences-menu]
                                 [callback
                                  (lambda (mi e)
                                    (send slider-status-dialog show #t))]))

(define slider-status-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define status-slider (new slider%
                           [label "Status line font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-status-dialog]
                           [init-value (unbox msg-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! msg-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

(define set-input-font (new menu-item%
                            [label "Set input line font"]
                            [parent preferences-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-input-dialog show #t))]))

(define slider-input-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define input-slider (new slider%
                          [label "Input line font size"]
                          [min-value *min-font*]
                          [max-value *max-font*]
                          [parent slider-input-dialog]
                          [init-value (unbox msg-b-fnt-size)]
                          [callback
                           (lambda (s e)
                             (set-box! msg-b-fnt-size (send s get-value))
                             (update-all-fonts 0))]
                          [style '(vertical-label horizontal)]))

(define set-doc-font (new menu-item%
                          [label "Set documentation font"]
                          [parent preferences-menu]
                          [callback
                           (lambda (mi e)
                             (send slider-doc-dialog show #t))]))

(define slider-doc-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define doc-slider (new slider%
                        [label "Documentation font size"]
                        [min-value *min-font*]
                        [max-value *max-font*]
                        [parent slider-doc-dialog]
                        [init-value (unbox doc-fnt-size)]
                        [callback
                         (lambda (s e)
                           (set-box! doc-fnt-size (send s get-value))
                           (update-all-fonts 0))]
                        [style '(vertical-label horizontal)]))

(define set-about-font (new menu-item%
                            [label "Set about font"]
                            [parent preferences-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-about-dialog show #t))]))

(define slider-about-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define about-slider (new slider%
                          [label "About font size"]
                          [min-value *min-font*]
                          [max-value *max-font*]
                          [parent slider-about-dialog]
                          [init-value (unbox about-fnt-size)]
                          [callback
                           (lambda (s e)
                             (set-box! about-fnt-size (send s get-value))
                             (update-all-fonts 0))]
                          [style '(vertical-label horizontal)]))

(define set-report-font (new menu-item%
                             [label "Set report canvas font"]
                             [parent preferences-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-report-dialog show #t))]))

(define slider-report-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define report-slider (new slider%
                           [label "Report canvas font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-report-dialog]
                           [init-value (unbox report-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! report-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

(define set-button-font (new menu-item%
                             [label "Set button font"]
                             [parent preferences-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-button-dialog show #t))]))

(define slider-button-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define button-slider (new slider%
                           [label "Button font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-button-dialog]
                           [init-value (unbox button-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! button-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

;;; ===================================================================

;;; Lost & found menu

(define retrieve-menu (new menu%
                           [parent main-menu-bar]
                           [label "Lost and Found"]))

(define show-compare-window-menu (new menu-item%
                                      [label "Show Comparison Window"]
                                      [parent retrieve-menu]
                                      [callback
                                       (lambda (mi e)
                                         (when (eq? *exec-button* compare-button)
                                           (send input-dialog show #t)))]))

(define show-odd-even-window-menu (new menu-item%
                                       [label "Show Odd/Even Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* odd-even-button)
                                            (send odd-even-dialog show #t)))]))

(define show-sequence-window-menu (new menu-item%
                                       [label "Show Sequence Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* sequence-button)
                                            (send sequence-dialog show #t)))]))

(define show-a2r-window-menu (new menu-item%
                                  [label "Show Arabic to Roman Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* a2r-button)
                                       (send a2r-dialog show #t)))]))

(define show-r2a-window-menu (new menu-item%
                                  [label "Show Roman to Arabic Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* r2a-button)
                                       (send r2a-dialog show #t)))]))

(define show-money-window-menu (new menu-item%
                                    [label "Show Return Change Window"]
                                    [parent retrieve-menu]
                                    [callback
                                     (lambda (mi e)
                                       (when (eq? *exec-button* money-button)
                                         (send money-dialog show #t)))]))

(define show-ABC-window-menu (new menu-item%
                                  [label "Show ABC-sort Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* ABC-button)
                                       (send ABC-dialog show #t)))]))

(define show-skip-window-menu (new menu-item%
                                   [label "Show skip-counting Window"]
                                   [parent retrieve-menu]
                                   [callback
                                    (lambda (mi e)
                                      (when (eq? *exec-button* skip-button)
                                        (send skip-dialog show #t)))]))

(define show-operators-window-menu
  (new menu-item%
       [label "Show missing Operators Window"]
       [parent retrieve-menu]
       [callback
        (lambda (mi e)
          (cond
            ((eq? *exec-button* ops-button1) (send ops-dialog1 show #t))
            ((eq? *exec-button* ops-button2) (send ops-dialog2 show #t))
            ((eq? *exec-button* ops-button3) (send ops-dialog3 show #t))
            (else
             (error 'show-operators-window-menu
                    "invalid value *exec-button*"))))]))

(define show-text-window-menu (new menu-item%
                                   [label "Show GAPESA Window"]
                                   [parent retrieve-menu]
                                   [callback
                                    (lambda (mi e)
                                      (when (eq? *exec-button* text-button)
                                        (send text-dialog show #t)))]))

(define show-bba-window-menu (new menu-item%
                                  [label "Show before between after Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* bba-button)
                                       (send bba-dialog show #t)))]))

(define show-pvalue-window-menu (new menu-item%
                                     [label "Show position value Window"]
                                     [parent retrieve-menu]
                                     [callback
                                      (lambda (mi e)
                                        (when (eq? *exec-button* pvalue-button)
                                          (send pvalue-dialog show #t)))]))

(define show-round-window-menu (new menu-item%
                                    [label "Show round Window"]
                                    [parent retrieve-menu]
                                    [callback
                                     (lambda (mi e)
                                       (when (eq? *exec-button* round-button)
                                         (send round-dialog show #t)))]))

(define show-ord-window-menu (new menu-item%
                                  [label "Show ordinal Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* ord-button)
                                       (send ord-dialog show #t)))]))

(define show-fraction-window-menu (new menu-item%
                                       [label "Show fractions Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* fraction-button)
                                            (send fraction-dialog show #t)))]))

(define show-clock-window-menu (new menu-item%
                                    [label "Show clock Window"]
                                    [parent retrieve-menu]
                                    [callback
                                     (lambda (mi e)
                                       (when (eq? *exec-button* clock-button)
                                         (send clock-dialog show #t)))]))

;;; Help menu

(define help-menu (new menu%
                       [parent main-menu-bar]
                       [label " Help "]))

(define menu-item-doc (new menu-item%
                           [label "Documentation"]
                           [parent help-menu]
                           [callback
                            (lambda (mi e)
                              (send doc-dialog show #t))]))

;;; Scribbling part
;;; ================================================================

(define remote? #f)
;; fallback base-path
(define base-path "file:///home/hrvoje064/Projects/math-quiz/scribblings/math-quiz.html")

(define-runtime-path docs-path "scribblings")
(define docs-path-string
  (let* ((str (path->string docs-path))
         (len (string-length str)))
    (substring str 0 (- len 12))))

;; base-path - path to math-quiz/scribblings on my computer
;; used for correcting secref links on stadalone binary package
(let ((check-base-path 
       (string-append docs-path-string "/scribblings/base-path.txt")))
  (cond
    ((file-exists? check-base-path)
     (set! base-path (car (file->lines check-base-path #:mode 'text))))
    (else
     (let ((b-path (string-append "file://" (path->string (current-directory))
                                  "scribblings/math-quiz.html")))
       (display-lines-to-file
        (list b-path) check-base-path #:mode 'text #:exists 'replace)
       (set! base-path b-path)))))

(define package-dir (pkg-directory "math-quiz"))
(when package-dir 
  (set! package-dir (path->string package-dir))
  (when (string=? package-dir docs-path-string)
    (set! remote? #t))) ; running from installed package (probably???)

(define (find-docs docs-path-string)
  (let ((index-path (string-append docs-path-string "/doc/math-quiz/index.html"))
        (mathq-path (string-append docs-path-string "/scribblings/math-quiz.html"))
        (check-path (string-append docs-path-string "/scribblings/fixed-secref.txt")))
    (cond
      ((file-exists? index-path) index-path)
      ((file-exists? check-path) (set! remote? #f) mathq-path)
      ((file-exists? mathq-path) (set! remote? #f)
                                 (fix-html mathq-path base-path)
                                 mathq-path)
      (else
       (string-append docs-path-string "/math-quiz.html")
       (set! remote? #f))))) ; docs not available???

(define menu-item-html (new menu-item%
                            [label "HTML Documentation"]
                            [parent help-menu]
                            [callback
                             (lambda (mi e)
                               (send-url/file
                                (find-docs docs-path-string)))]))

;;; =================================================================

(define doc-dialog (new dialog%
                        [label "Math Quiz -- Instructions"]
                        [parent main-window]
                        [width 780]
                        [height 660]
                        [style '(close-button)]
                        [alignment '(right top)]))

(define doc-canvas (new editor-canvas%
                        [parent doc-dialog]
                        [editor doc-instructions]
                        [label "Instructions"]
                        [min-width 760]
                        [min-height 640]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [style '(no-hscroll auto-vscroll no-focus)]))

(define menu-item-about (new menu-item%
                             [label "About Math Quiz"]
                             [parent help-menu]
                             [callback
                              (lambda (mi e)
                                (send about-dialog show #t))]))

(define about-dialog (new dialog%
                          [label "About Math Quiz"]
                          [parent main-window]
                          [width 540]
                          [height 520]
                          [style '(close-button)]
                          [alignment '(right top)]))

(define about-canvas (new editor-canvas%
                          [parent about-dialog]
                          [editor about-text]
                          [label "About"]
                          [min-width 520]
                          [min-height 500]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [style '(no-hscroll auto-vscroll no-focus)]))

;;; Update math-quiz
;;; ================================================================

(define (check-update-menu)
  (unless remote?
    (send menu-item-update enable #f)))

(define menu-item-update (new menu-item%
                              [label "Update math-quiz"]
                              [parent help-menu]
                              [callback
                               (lambda (mi e)
                                 (make-pkg-installer #:package-to-offer
                                                     "math-quiz"))]))

(check-update-menu) ; disable Help->Update... menu, not in a package

;;; Arithmetic problems
;;; ==================================================================                      

(define operation-msg (new message%
                           [parent h-pane-prompt]
                           [font message-bold-font]
                           [label op-start-label]
                           [vert-margin 10]
                           [horiz-margin 12]
                           [stretchable-width #f]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define number-input (new text-field%
                          [parent h-pane-result]
                          [font message-bold-font]
                          [label "=  "]
                          [init-value input-label]
                          [enabled #f]
                          [min-width 150]
                          [min-height 30]
                          [vert-margin 6]
                          [horiz-margin 10]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define prompt-msg (new message%
                        [parent h-pane-status]
                        [label prompt-msg-label]
                        [font message-font]
                        [min-width 360]
                        [stretchable-width #t]
                        [vert-margin 2]
                        [horiz-margin 12]))
;;; ================================================================

;;; Comparison problems
;;; ================================================================

(define input-dialog (new frame%
                          [label "Comparison questions"]
                          [parent main-window]
                          [width 320]
                          [height 60]
                          [border 10]
                          [style '(no-resize-border)]
                          [alignment '(left center)]))

(define input-pane (new horizontal-pane%
                        [parent input-dialog]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define left-prompt (new message%
                         [parent input-pane]
                         [font message-bold-font]
                         [label ""]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [auto-resize #t]))

(define comparison-input (new text-field%
                              [parent input-pane]
                              [font message-bold-font]
                              [label ""]
                              [init-value input-label]
                              [enabled #t]
                              [min-width 30]
                              [min-height 30]
                              [vert-margin 10]
                              [horiz-margin 10]
                              [stretchable-width #f]
                              [stretchable-height #f]))

(define right-prompt (new message%
                          [parent input-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [stretchable-width #f]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define compare-button (new button%
                            [parent input-pane]
                            [label "Check"]
                            [font button-font]
                            [min-height start-button-height]
                            [enabled #f]
                            [vert-margin 10]
                            [horiz-margin 10]
                            [style '(border)]
                            [callback
                             (lambda (button event)
                               (let ((input (send comparison-input get-value)))
                                 (send comparison-input set-value "")
                                 (math-quiz-type (string-trim input))))]))
;;; ================================================================

;;; Roman Numerals problems
;;; ================================================================

;;; Arabic to Roman
(define a2r-dialog (new frame%
                        [label "Arabic to Roman questions"]
                        [parent main-window]
                        [width 420]
                        [height 60]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define a2r-pane (new horizontal-pane%
                      [parent a2r-dialog]
                      [vert-margin 10]
                      [horiz-margin 10]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define a2r-prompt (new message%
                        [parent a2r-pane]
                        [font message-bold-font]
                        [label ""]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [stretchable-width #f]
                        [stretchable-height #f]
                        [auto-resize #t]))

(define a2r-input (new text-field%
                       [parent a2r-pane]
                       [font message-bold-font]
                       [label ""]
                       [init-value input-label]
                       [enabled #t]
                       [min-width 200]
                       [min-height 30]
                       [vert-margin 10]
                       [horiz-margin 10]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define a2r-button (new button%
                        [parent a2r-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input (send a2r-input get-value)))
                             (send a2r-input set-value "")
                             (math-quiz-type (string-upcase
                                              (string-trim input)))))]))

;;; Roman to Arabic
(define r2a-dialog (new frame%
                        [label "Roman to Arabic questions"]
                        [parent main-window]
                        [width 420]
                        [height 60]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define r2a-pane (new horizontal-pane%
                      [parent r2a-dialog]
                      [vert-margin 10]
                      [horiz-margin 10]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define r2a-prompt (new message%
                        [parent r2a-pane]
                        [font message-bold-font]
                        [label ""]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [stretchable-width #f]
                        [stretchable-height #f]
                        [auto-resize #t]))

(define r2a-input (new text-field%
                       [parent r2a-pane]
                       [font message-bold-font]
                       [label ""]
                       [init-value input-label]
                       [enabled #t]
                       [min-width 60]
                       [min-height 30]
                       [vert-margin 10]
                       [horiz-margin 10]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define r2a-button (new button%
                        [parent r2a-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input (send r2a-input get-value)))
                             (send r2a-input set-value "")
                             (math-quiz-type (string-trim input))))]))
;;; ================================================================

;;; Money problems
;;; ================================================================

(define money-dialog (new frame%
                          [label "Return Change questions"]
                          [parent main-window]
                          [width 810]
                          [height 120]
                          [border 10]
                          [style '(no-resize-border)]
                          [alignment '(left center)]))

(define money-pane (new horizontal-pane%
                        [parent money-dialog]
                        [vert-margin 10]
                        [horiz-margin 5]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define money-prompt (new message%
                          [parent money-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define money-input-100 (new text-field%
                             [parent money-pane]
                             [font message-font]
                             [label "100"]
                             [init-value input-label]
                             [enabled #t]
                             [min-width *cash-input-size*]
                             [min-height 30]
                             [vert-margin 10]
                             [horiz-margin 5]
                             [stretchable-width #t]
                             [stretchable-height #f]))

(define money-input-50 (new text-field%
                            [parent money-pane]
                            [font message-font]
                            [label "50"]
                            [init-value input-label]
                            [enabled #t]
                            [min-width *cash-input-size*]
                            [min-height 30]
                            [vert-margin 10]
                            [horiz-margin 5]
                            [stretchable-width #t]
                            [stretchable-height #f]))

(define money-input-20 (new text-field%
                            [parent money-pane]
                            [font message-font]
                            [label "20"]
                            [init-value input-label]
                            [enabled #t]
                            [min-width *cash-input-size*]
                            [min-height 30]
                            [vert-margin 10]
                            [horiz-margin 5]
                            [stretchable-width #t]
                            [stretchable-height #f]))

(define money-input-10 (new text-field%
                            [parent money-pane]
                            [font message-font]
                            [label "10"]
                            [init-value input-label]
                            [enabled #t]
                            [min-width *cash-input-size*]
                            [min-height 30]
                            [vert-margin 10]
                            [horiz-margin 5]
                            [stretchable-width #t]
                            [stretchable-height #f]))

(define money-input-5 (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label "5"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-1 (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label "1"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-q (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label ". q"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-d (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label "d"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-n (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label "n"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-c (new text-field%
                           [parent money-pane]
                           [font message-font]
                           [label "c"]
                           [init-value input-label]
                           [enabled #t]
                           [min-width *cash-input-size*]
                           [min-height 30]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define money-input-list
  (list money-input-100 money-input-50 money-input-20
        money-input-10 money-input-5 money-input-1 money-input-q
        money-input-d money-input-n money-input-c))

(define (clear-money-inputs) ; taking erasing inputs out of callback function
  (for-each (lambda (x) (send x set-value "")) money-input-list))

(define money-button (new button%
                          [parent money-pane]
                          [label "Check"]
                          [font button-font]
                          [min-height start-button-height]
                          [enabled #f]
                          [vert-margin 10]
                          [horiz-margin 20]
                          [style '(border)]
                          [callback
                           (lambda (button event)
                             (let ((input
                                    (map (lambda (x)
                                           (string->number
                                            (string-trim (send x get-value))))
                                         money-input-list)))
                               (math-quiz-type
                                ;; throwing away everything but numbers
                                (map (lambda (x) (if (number? x) x 0)) input))))]))

;;; ================================================================
;;; ABC-sort problems
;;; ================================================================

(define ABC-dialog (new frame%
                        [label "Alphabetical Sorting questions"]
                        [parent main-window]
                        [width 810]
                        [height 120]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define ABC-pane (new horizontal-pane%
                      [parent ABC-dialog]
                      [vert-margin 10]
                      [horiz-margin 5]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define ABC-input-1 (new text-field%
                         [parent ABC-pane]
                         [font message-font]
                         [label " "]
                         [init-value input-label]
                         [enabled #t]
                         [min-width *cash-input-size*]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define ABC-prompt-1 (new message%
                          [parent ABC-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define ABC-input-2 (new text-field%
                         [parent ABC-pane]
                         [font message-font]
                         [label "|"]
                         [init-value input-label]
                         [enabled #t]
                         [min-width *cash-input-size*]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define ABC-prompt-2 (new message%
                          [parent ABC-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define ABC-input-3 (new text-field%
                         [parent ABC-pane]
                         [font message-font]
                         [label "|"]
                         [init-value input-label]
                         [enabled #t]
                         [min-width *cash-input-size*]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define ABC-prompt-3 (new message%
                          [parent ABC-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define ABC-input-4 (new text-field%
                         [parent ABC-pane]
                         [font message-font]
                         [label "|"]
                         [init-value input-label]
                         [enabled #t]
                         [min-width *cash-input-size*]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define ABC-prompt-4 (new message%
                          [parent ABC-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define ABC-input-5 (new text-field%
                         [parent ABC-pane]
                         [font message-font]
                         [label "|"]
                         [init-value input-label]
                         [enabled #t]
                         [min-width *cash-input-size*]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define ABC-prompt-5 (new message%
                          [parent ABC-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define ABC-input-list
  (list ABC-input-1 ABC-input-2 ABC-input-3 ABC-input-4 ABC-input-5))

(define (clear-ABC-inputs) ; taking erasing inputs out of callback function
  (for-each (lambda (x) (send x set-value "")) ABC-input-list))

(define ABC-button (new button%
                        [parent ABC-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 20]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input
                                  (map (lambda (x) (string->number
                                                    (string-trim
                                                     (send x get-value))))
                                       ABC-input-list)))
                             (math-quiz-type
                              ;; throwing away everything but numbers
                              (map (lambda (x)
                                     (if (number? x) x 0)) input))))]))

;;; ===============================================================
;;; Missing Operators problems
;;; ===============================================================

(define ops-dialog1 (new frame%
                         [label "Missing Operator (1) problems"]
                         [parent main-window]
                         [width 280]
                         [height 120]
                         [border 10]
                         [style '(no-resize-border)]
                         [alignment '(left center)]))

(define ops-dialog2 (new frame%
                         [label "Missing Operators (2) problems"]
                         [parent main-window]
                         [width 320]
                         [height 120]
                         [border 10]
                         [style '(no-resize-border)]
                         [alignment '(left center)]))

(define ops-dialog3 (new frame%
                         [label "Missing Operators (3) problems"]
                         [parent main-window]
                         [width 400]
                         [height 120]
                         [border 10]
                         [style '(no-resize-border)]
                         [alignment '(left center)]))

(define ops-pane1 (new horizontal-pane%
                       [parent ops-dialog1]
                       [vert-margin 10]
                       [horiz-margin 5]
                       [alignment '(center center)]
                       [stretchable-width #t]
                       [stretchable-height #t]))

(define ops-pane2 (new horizontal-pane%
                       [parent ops-dialog2]
                       [vert-margin 10]
                       [horiz-margin 5]
                       [alignment '(center center)]
                       [stretchable-width #t]
                       [stretchable-height #t]))

(define ops-pane3 (new horizontal-pane%
                       [parent ops-dialog3]
                       [vert-margin 10]
                       [horiz-margin 5]
                       [alignment '(center center)]
                       [stretchable-width #t]
                       [stretchable-height #t]))

(define ops-prompt1-1 (new message%
                           [parent ops-pane1]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input1-1 (new text-field%
                          [parent ops-pane1]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt1-2 (new message%
                           [parent ops-pane1]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-prompt1-3 (new message%
                           [parent ops-pane1]
                           [font message-bold-font]
                           [label " "]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-button1 (new button%
                         [parent ops-pane1]
                         [label "Check"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input
                                   (map
                                    (λ (x) (string-trim (send x get-value)))
                                    (list ops-input1-1))))
                              (clear-ops-inputs)
                              (math-quiz-type input)))]))

(define ops-prompt2-1 (new message%
                           [parent ops-pane2]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input2-1 (new text-field%
                          [parent ops-pane2]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt2-2 (new message%
                           [parent ops-pane2]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input2-2 (new text-field%
                          [parent ops-pane2]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt2-3 (new message%
                           [parent ops-pane2]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-prompt2-4 (new message%
                           [parent ops-pane2]
                           [font message-bold-font]
                           [label " "]
                           [vert-margin 10]
                           [horiz-margin 20]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-button2 (new button%
                         [parent ops-pane2]
                         [label "Check"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input
                                   (map
                                    (λ (x) (string-trim (send x get-value)))
                                    (list ops-input2-1 ops-input2-2))))
                              (clear-ops-inputs)
                              (math-quiz-type input)))]))

(define ops-prompt3-1 (new message%
                           [parent ops-pane3]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input3-1 (new text-field%
                          [parent ops-pane3]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt3-2 (new message%
                           [parent ops-pane3]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input3-2 (new text-field%
                          [parent ops-pane3]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt3-3 (new message%
                           [parent ops-pane3]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-input3-3 (new text-field%
                          [parent ops-pane3]
                          [font message-font]
                          [label ""]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 24]
                          [min-height 30]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define ops-prompt3-4 (new message%
                           [parent ops-pane3]
                           [font message-bold-font]
                           [label ""]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-prompt3-5 (new message%
                           [parent ops-pane3]
                           [font message-bold-font]
                           [label " "]
                           [vert-margin 10]
                           [horiz-margin 5]
                           [stretchable-width #t]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define ops-button3 (new button%
                         [parent ops-pane3]
                         [label "Check"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input
                                   (map
                                    (λ (x) (string-trim (send x get-value)))
                                    (list ops-input3-1 ops-input3-2 ops-input3-3))))
                              (clear-ops-inputs)
                              (math-quiz-type input)))]))

(define ops-input-list
  (list ops-input1-1 ops-input2-1 ops-input2-2 ops-input3-1
        ops-input3-2 ops-input3-3))

(define (clear-ops-inputs) ; taking erasing inputs out of callback function
  (for-each (lambda (x) (send x set-value "")) ops-input-list))

;;; ===============================================================
;;; Skip counting problems & operators problems
;;; ===============================================================

(define skip-dialog (new frame%
                         [label "Skip counting questions"]
                         [parent main-window]
                         [width 810]
                         [height 120]
                         [border 10]
                         [style '(no-resize-border)]
                         [alignment '(left center)]))

(define skip-pane (new horizontal-pane%
                       [parent skip-dialog]
                       [vert-margin 10]
                       [horiz-margin 5]
                       [alignment '(center center)]
                       [stretchable-width #t]
                       [stretchable-height #t]))

(define skip-input (new text-field%
                        [parent skip-pane]
                        [font message-font]
                        [label " "]
                        [init-value ""]
                        [enabled #t]
                        [min-width 500]
                        [min-height 30]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [stretchable-width #t]
                        [stretchable-height #f]))

(define skip-prompt (new message%
                         [parent skip-pane]
                         [font message-bold-font]
                         [label "skip by: "]
                         [vert-margin 10]
                         [horiz-margin 5]
                         [stretchable-width #t]
                         [stretchable-height #f]
                         [auto-resize #t]))

(define skip-button (new button%
                         [parent skip-pane]
                         [label "Check"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 20]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input
                                   (extract-numbers (send skip-input get-value))))
                              (math-quiz-type input)))]))

(define (extract-numbers n-str)
  (map (compose string->number string-trim) (string-split n-str ",")))

;;; ================================================================

;;; Text problems
;;; ================================================================

(define text-dialog (new frame%
                         [label "GAPESA questions"]
                         [parent main-window]
                         [width 780]
                         [height 240]
                         [border 10]
                         [style '(no-resize-border)]
                         [alignment '(left center)]))

(define text-pane (new vertical-pane%
                       [parent text-dialog]
                       [vert-margin 10]
                       [horiz-margin 10]
                       [alignment '(center center)]
                       [stretchable-width #f]
                       [stretchable-height #t]))

(define text-canvas (new editor-canvas%
                         [parent text-pane]
                         [editor text-prompt]
                         [label "GAPESA"]
                         [min-width 720]
                         [min-height 140]
                         [vert-margin 10]
                         [horiz-margin 20]
                         [style '(no-hscroll auto-vscroll no-focus)]))

(define text-input (new text-field%
                        [parent text-pane]
                        [font message-font]
                        [label "Answer: "]
                        [init-value ""]
                        [enabled #f]
                        [min-width 80]
                        [min-height 30]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [stretchable-width #f]
                        [stretchable-height #f]))

(define text-button (new button%
                         [parent text-pane]
                         [label "Check"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 20]
                         [horiz-margin 100]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input (string-trim (send text-input get-value))))
                              (when *time-flag*
                                (let ((result (result->min input)))
                                  (if result
                                      (set! input (number->string result))
                                      (set! input (string-append " " input " ")))))
                              (send text-input set-value "")
                              (math-quiz-type input)))]))

;;; ================================================================

;;; Odd - Even problems
;;; ================================================================

(define odd-even-dialog (new frame%
                             [label "Odd/Even questions"]
                             [parent main-window]
                             [width 280]
                             [height 60]
                             [border 10]
                             [style '(no-resize-border)]
                             [alignment '(left center)]))

(define odd-even-pane (new horizontal-pane%
                           [parent odd-even-dialog]
                           [min-width 280]
                           [min-height 60]
                           [vert-margin 0]
                           [horiz-margin 0]
                           [alignment '(left center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define odd-even-pane-left (new horizontal-pane%
                                [parent odd-even-pane]
                                [min-width 80]
                                [min-height 60]
                                [vert-margin 10]
                                [horiz-margin 10]
                                [alignment '(right center)]
                                [stretchable-width #t]
                                [stretchable-height #t]))

(define odd-even-pane-right (new horizontal-pane%
                                 [parent odd-even-pane]
                                 [min-width 200]
                                 [min-height 60]
                                 [vert-margin 10]
                                 [horiz-margin 10]
                                 [alignment '(left center)]
                                 [stretchable-width #t]
                                 [stretchable-height #t]))

(define odd-even-prompt (new message%
                             [parent odd-even-pane-left]
                             [font message-bold-font]
                             [label ""]
                             [vert-margin 10]
                             [horiz-margin 10]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [auto-resize #t]))

(define odd-even-radio-box (new radio-box%
                                [label "   "]
                                [choices '("odd" "even")]
                                [parent odd-even-pane-right]
                                [callback
                                 (lambda (rb e) (void))]))

(define odd-even-button (new button%
                             [parent odd-even-pane-right]
                             [label "Check"]
                             [font button-font]
                             [min-height start-button-height]
                             [enabled #f]
                             [vert-margin 10]
                             [horiz-margin 30]
                             [style '(border)]
                             [callback
                              (lambda (button event)
                                (let ((input
                                       (send odd-even-radio-box get-item-label
                                             (send odd-even-radio-box
                                                   get-selection))))
                                  (math-quiz-type (string-trim input))))]))

;;; =================================================================
;;; Sequence (IQ) problems
;;; =================================================================

(define sequence-dialog (new frame%
                             [label "Sequence questions"]
                             [parent main-window]
                             [width 320]
                             [height 60]
                             [border 10]
                             [style '(no-resize-border)]
                             [alignment '(left center)]))

(define sequence-pane (new horizontal-pane%
                           [parent sequence-dialog]
                           [vert-margin 10]
                           [horiz-margin 10]
                           [alignment '(center center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define left-sequence-prompt (new message%
                                  [parent sequence-pane]
                                  [font message-bold-font]
                                  [label ""]
                                  [vert-margin 10]
                                  [horiz-margin 5]
                                  [stretchable-width #f]
                                  [stretchable-height #f]
                                  [auto-resize #t]))

(define sequence-input (new text-field%
                            [parent sequence-pane]
                            [font message-bold-font]
                            [label ""]
                            [init-value input-label]
                            [enabled #t]
                            [min-width 90]
                            [min-height 30]
                            [vert-margin 6]
                            [horiz-margin 5]
                            [stretchable-width #f]
                            [stretchable-height #f]))

(define right-sequence-prompt (new message%
                                   [parent sequence-pane]
                                   [font message-bold-font]
                                   [label ""]
                                   [vert-margin 10]
                                   [horiz-margin 5]
                                   [stretchable-width #f]
                                   [stretchable-height #f]
                                   [auto-resize #t]))

(define sequence-button (new button%
                             [parent sequence-pane]
                             [label "Check"]
                             [font button-font]
                             [min-height start-button-height]
                             [enabled #f]
                             [vert-margin 10]
                             [horiz-margin 10]
                             [style '(border)]
                             [callback
                              (lambda (button event)
                                (let ((input (send sequence-input get-value)))
                                  (send sequence-input set-value input-label)
                                  (math-quiz-type (string-trim input))))]))

(define sequence-cheat-button (new button%
                                   [parent sequence-pane]
                                   [label "Cheat"]
                                   [font button-font]
                                   [min-height start-button-height]
                                   [enabled #f]
                                   [vert-margin 10]
                                   [horiz-margin 5]
                                   [style '(border)]
                                   [callback
                                    (lambda (button event)
                                      (send sequence-input set-value 
                                            (number->string
                                             (fourth (problem-x *problem*))))
                                      (set! *cheat-flag* #t)
                                      (send button enable #f))]))

;;; =================================================================

;;; Before Between After problems
;;; =================================================================

(define bba-dialog (new frame%
                        [label "Before Between After questions"]
                        [parent main-window]
                        [width 320]
                        [height 60]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define bba-pane (new horizontal-pane%
                      [parent bba-dialog]
                      [vert-margin 10]
                      [horiz-margin 10]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define left-bba-prompt (new message%
                             [parent bba-pane]
                             [font message-bold-font]
                             [label ""]
                             [vert-margin 10]
                             [horiz-margin 5]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [auto-resize #t]))

(define bba-input (new text-field%
                       [parent bba-pane]
                       [font message-bold-font]
                       [label ""]
                       [init-value input-label]
                       [enabled #t]
                       [min-width 60]
                       [min-height 30]
                       [vert-margin 6]
                       [horiz-margin 5]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define right-bba-prompt (new message%
                              [parent bba-pane]
                              [font message-bold-font]
                              [label ""]
                              [vert-margin 10]
                              [horiz-margin 5]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define bba-button (new button%
                        [parent bba-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 20]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input (send bba-input get-value)))
                             (send bba-input set-value input-label)
                             (math-quiz-type (string-trim input))))]))

;;; ==================================================================

;;; Position Value problems
;;; ==================================================================

(define pvalue-dialog (new frame%
                           [label "Position value questions"]
                           [parent main-window]
                           [width 300]
                           [height 120]
                           [border 10]
                           [style '(no-resize-border)]
                           [alignment '(left center)]))

(define pvalue-pane (new horizontal-pane%
                         [parent pvalue-dialog]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [alignment '(center center)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define pvalue-th-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-hu-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-te-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-on-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-radio-box (new radio-box%
                              [label "   "]
                              [choices '("ones" "tens" "hundreds" "thousands")]
                              [parent pvalue-pane]
                              [callback
                               (lambda (rb e) (void))]))

(define pvalue-button (new button%
                           [parent pvalue-pane]
                           [label "Check"]
                           [font button-font]
                           [min-height start-button-height]
                           [enabled #f]
                           [vert-margin 10]
                           [horiz-margin 30]
                           [style '(border)]
                           [callback
                            (lambda (button event)
                              (let ((input
                                     (send pvalue-radio-box get-selection)))
                                (math-quiz-type (number->string input))))]))

;;; ==================================================================
;;; Round problems
;;; ==================================================================

(define round-dialog (new frame%
                          [label "Rounding number questions"]
                          [parent main-window]
                          [width 500]
                          [height 120]
                          [border 10]
                          [style '(no-resize-border)]
                          [alignment '(left center)]))

(define round-pane (new horizontal-pane%
                        [parent round-dialog]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define round-head-prompt (new message%
                               [parent round-pane]
                               [font message-bold-font]
                               [label " "]
                               [vert-margin 10]
                               [horiz-margin 0]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [auto-resize #t]))

(define round-digit-prompt (new message%
                                [parent round-pane]
                                [font message-bold-font]
                                [label " "]
                                [vert-margin 10]
                                [horiz-margin 0]
                                [stretchable-width #f]
                                [stretchable-height #f]
                                [auto-resize #t]))

(define round-tail-prompt (new message%
                               [parent round-pane]
                               [font message-bold-font]
                               [label " "]
                               [vert-margin 10]
                               [horiz-margin 0]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [auto-resize #t]))

(define round-input (new text-field%
                         [parent round-pane]
                         [font message-bold-font]
                         [label " "]
                         [init-value input-label]
                         [enabled #f]
                         [min-width 150]
                         [min-height 30]
                         [vert-margin 6]
                         [horiz-margin 10]
                         [stretchable-width #f]
                         [stretchable-height #f]))

(define round-button (new button%
                          [parent round-pane]
                          [label "Check"]
                          [font button-font]
                          [min-height start-button-height]
                          [enabled #f]
                          [vert-margin 10]
                          [horiz-margin 30]
                          [style '(border)]
                          [callback
                           (lambda (button event)
                             (let ((input (send round-input get-value)))
                               (send round-input set-value "")
                               (math-quiz-type (string-trim input))))]))

;;; ==================================================================
;;; Ordinal problems
;;; ==================================================================

(define ord-dialog (new frame%
                        [label "Ordinal number questions"]
                        [parent main-window]
                        [width 380]
                        [height 120]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define ord-pane (new horizontal-pane%
                      [parent ord-dialog]
                      [vert-margin 10]
                      [horiz-margin 10]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define ord-prompt (new message%
                        [parent ord-pane]
                        [font message-bold-font]
                        [label " "]
                        [vert-margin 10]
                        [horiz-margin 0]
                        [stretchable-width #f]
                        [stretchable-height #f]
                        [auto-resize #t]))

(define ord-input (new text-field%
                       [parent ord-pane]
                       [font message-bold-font]
                       [label " "]
                       [init-value input-label]
                       [enabled #f]
                       [min-width 100]
                       [min-height 30]
                       [vert-margin 6]
                       [horiz-margin 10]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define ord-button (new button%
                        [parent ord-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 30]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input (send ord-input get-value)))
                             (send ord-input set-value "")
                             (math-quiz-type (string-trim input))))]))

;;; ==================================================================

(define no-mouse-canvas%
  (class editor-canvas% ; The base class is editor-canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (void)) ; do nothing
    (super-new)))

(define text-output (new no-mouse-canvas%
                         [parent h-pane-report]
                         [editor text-lines]
                         [label "Problem list"]
                         [min-width 540]
                         [min-height 530]
                         [vert-margin 6]
                         [horiz-margin 2]
                         [style '(no-hscroll auto-vscroll no-focus)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define stop-button (new button%
                         [parent v-pane-stop]
                         [label "Stop"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 30]
                         [callback
                          (lambda (button event) (reset))]))

(define calc-button (new button%
                         [parent h-pane-result]
                         [label "Calculate"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 6]
                         [horiz-margin 30]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input (send number-input get-value)))
                              (send number-input set-value "")
                              (math-quiz-type (string-trim input))))]))

(define wiwi-button (new button%
                         [parent v-pane-pause]
                         [label "Pause"]
                         [font button-font]
                         [enabled #f]
                         [vert-margin 30]
                         [horiz-margin 6]
                         [min-width start-button-width]
                         [min-height start-button-height]
                         [callback
                          (lambda (button event)
                            (cond
                              (*wiwi* ; pause was on
                               (set! *wiwi* #f) ; enable for next set of exercises
                               (set! *wiwi-time* (- (current-seconds) *wiwi-start*))
                               (send text-lines insert
                                     (format "Break done   - time is running~n"))
                               (send text-lines change-style style-delta-black)
                               (send button enable #f)
                               ; resume exercise
                               (send *exec-button* enable #t)
                               (if (eq? *exec-button* calc-button)
                                   (send number-input enable #t)
                                   (disable/enable-input-fields #t))
                               (send button set-label "Pause")
                               (disable/enable-set/font-menu #f)) ; disable doc/about/update menu
                              (else
                               (set! *wiwi* #t) ; set pause to on
                               (set! *wiwi-start* (current-seconds)) ; mark time
                               (send text-lines change-style style-delta-blue)
                               (send text-lines insert
                                     (format "On the break -  time suspended~n"))
                               ;; enabling doc/about/update menu
                               (send menu-item-doc enable #t)
                               (send menu-item-about enable #t)
                               (send menu-item-update enable #t)
                               (check-update-menu) ; disable anyway?
                               ; disable all buttons and input fields
                               (send *exec-button* enable #f)
                               (disable/enable-input-fields #f) ; seq cheat button
                               (send number-input enable #f)
                               (disable/enable-input-fields #f)
                               (send button set-label "Resume"))))]))
                            
(define start/-button (new button%
                           [parent v-start-arithmetic]
                           [label " ÷ "]
                           [font button-font]
                           [min-width start-button-width]	 
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start/))]))

(define start/quot-button (new button%
                               [parent v-start-arithmetic]
                               [label "int or fract ÷"]
                               [font button-font]
                               [min-width start-button-width]	 
                               [min-height start-button-height]
                               [enabled #t]
                               [vert-margin 6]
                               [horiz-margin 6]
                               [callback
                                (lambda (button event) (start/quot))]))

(define start100/-button (new button%
                              [parent v-start-arithmetic]
                              [label "100/10"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start100/))]))

(define start*-button (new button%
                           [parent v-start-arithmetic]
                           [label " * "]
                           [font button-font]
                           [min-width start-button-width]
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start*))]))

(define start10*-button (new button%
                             [parent v-start-arithmetic]
                             [label "10*10"]
                             [font button-font]
                             [min-width start-button-width]
                             [min-height start-button-height]
                             [enabled #t]
                             [vert-margin 6]
                             [horiz-margin 6]
                             [callback
                              (lambda (button event) (start10*))]))

(define start+-button (new button%
                           [parent v-start-arithmetic]
                           [label " + - "]
                           [font button-font]
                           [min-width start-button-width]
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start+-))]))

(define start<=>button (new button%
                            [parent v-start-popup]
                            [label "< = >"]
                            [font button-font]
                            [min-width start-button-width]
                            [min-height start-button-height]
                            [enabled #t]
                            [vert-margin 6]
                            [horiz-margin 6]
                            [callback
                             (lambda (button event) (start<=>))]))

(define start-odd/even-button (new button%
                                   [parent v-start-popup]
                                   [label "odd even"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-odd/even))]))

(define start-sequence-button (new button%
                                   [parent v-start-popup]
                                   [label "sequence"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-sequence))]))

(define start-bba-button (new button%
                              [parent v-start-popup]
                              [label " B B A "]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-bba))]))

(define start-pvalue-button (new button%
                                 [parent v-start-popup]
                                 [label "PosVal"]
                                 [font button-font]
                                 [min-width start-button-width]
                                 [min-height start-button-height]
                                 [enabled #t]
                                 [vert-margin 6]
                                 [horiz-margin 6]
                                 [callback
                                  (lambda (button event) (start-pvalue))]))

(define start-round-button (new button%
                                [parent v-start-popup]
                                [label "Round"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-round))]))

(define start-ord-button (new button%
                              [parent v-start-popup]
                              [label "Ordinals"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-ord))]))

(define start-fraction-button (new button%
                                   [parent v-start-popup]
                                   [label "fractions"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-fraction))]))

(define start-clock-button (new button%
                                [parent v-start-popup]
                                [label "clock"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-clock))]))

(define start-a2r-button (new button%
                              [parent v-start-popup]
                              [label "N->Roman"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-a2r))]))

(define start-r2a-button (new button%
                              [parent v-start-popup]
                              [label "Roman->N"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-r2a))]))

(define start-money-button (new button%
                                [parent v-start-popup]
                                [label "cash USD"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-money))]))

(define start-money-p-button (new button%
                                  [parent v-start-popup]
                                  [label "cash Peso"]
                                  [font button-font]
                                  [min-width start-button-width]
                                  [min-height start-button-height]
                                  [enabled #t]
                                  [vert-margin 6]
                                  [horiz-margin 6]
                                  [callback
                                   (lambda (button event) (start-money-p))]))

(define start-ABC-button (new button%
                              [parent v-start-popup]
                              [label "ABC sort"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-ABC))]))

(define start-skip-button (new button%
                               [parent v-start-popup]
                               [label "skip+count"]
                               [font button-font]
                               [min-width start-button-width]
                               [min-height start-button-height]
                               [enabled #t]
                               [vert-margin 6]
                               [horiz-margin 6]
                               [callback
                                (lambda (button event) (start-skip))]))

(define start-skip-neg-button (new button%
                                   [parent v-start-popup]
                                   [label "skip-count"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-skip-neg))]))

(define start-findX-button (new button%
                                [parent v-start-popup]
                                [label "missing X"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-findX))]))

(define start-operators-button (new button%
                                    [parent v-start-popup]
                                    [label "operators"]
                                    [font button-font]
                                    [min-width start-button-width]
                                    [min-height start-button-height]
                                    [enabled #t]
                                    [vert-margin 6]
                                    [horiz-margin 6]
                                    [callback
                                     (lambda (button event) (start-operators))]))

(define start-time-button (new button%
                               [parent v-start-popup]
                               [label "time"]
                               [font button-font]
                               [min-width start-button-width]
                               [min-height start-button-height]
                               [enabled #t]
                               [vert-margin 6]
                               [horiz-margin 6]
                               [callback
                                (lambda (button event) (start-time))]))

(define start-text-button (new button%
                               [parent v-start-popup]
                               [label "GAPESA"]
                               [font button-font]
                               [min-width start-button-width]
                               [min-height start-button-height]
                               [enabled #t]
                               [vert-margin 6]
                               [horiz-margin 6]
                               [callback
                                (lambda (button event) (start-text))]))

(define start-Carea-button (new button%
                                [parent v-start-popup]
                                [label "Perimeter/Area"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-Carea))]))

;;; ===============================================================

;;; callback functions
;;; ================================================================

(define (start+-)
  (case *level+-*
    ((0) (set! *time-factor* 1/4) ; minutes per problem
         (set! *max-used-pairs* (kombinations 9 2))
         (set! get-problem get-problem-fast+-) ; setting the function
         (send text-lines insert
               (format "---------   fast plus minus exercises   ---------~n")))
    ((1) (set! *time-factor* 1/3) ; minutes per problem
         (set! get-problem get-problem-2d+) ; setting the function
         (send text-lines insert
               (format "----------   limited plus exercises   ----------~n"))) 
    ((2) (set! *time-factor* 1/2)
         (set! get-problem get-problem-2d+-)
         (send text-lines insert
               (format "-------   limited plus minus exercises   -------~n")))
    ((3) (set! *time-factor* 3/2)
         (set! get-problem get-problem+-) ; setting the function         
         (send text-lines insert
               (format "-------   plus minus exercises positive  -------~n")))
    ((4) (set! *time-factor* 3/2)
         (set! get-problem get-problem--) ; setting the function         
         (send text-lines insert
               (format "---------   minus exercises negative   ---------~n")))
    ((5) (set! *time-factor* 1/2)
         (set! get-problem get-problem-f5) ; setting the function         
         (send text-lines insert
               (format "-----  (+ -) exercises l5 (d1=d2) fractions  ---~n")))
    ((6) (set! *time-factor* 3/2)
         (set! get-problem get-problem-f6) ; setting the function         
         (send text-lines insert
               (format "-----  (+ -) exercises l6 (d1/d1) fractions  ---~n")))
    ((7) (set! *time-factor* 5/2)
         (set! get-problem get-problem-f7) ; setting the function         
         (send text-lines insert
               (format "---- (+ -) exercises l7 (d1&d2/n) fractions  ---~n")))
    ((8) (set! *time-factor* 5/2)
         (set! get-problem get-problem-f8) ; setting the function         
         (send text-lines insert
               (format "----=  (+ -) exercises l8 tough fractions   ----~n")))
    ((9) (set! *time-factor* 5/2)
         (set! get-problem get-problem-f9) ; setting the function         
         (send text-lines insert
               (format "----=  (+ -) exercises l9 mixed fractions   ----~n")))
    (else (error 'start+-level)))
  (set! do-math do-math+) ; set arithmetic operation
  (set! setup setup-arithmetic) ; setup function
  (set! equal= =) ; setting simple equality test
  (set! *used-numbers* '())
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start10*)
  (when (or (< *max*table* 5) (> *max*table* 12))
    (error 'max*table-size))
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "------   multiplication table exercises   ------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem10*10)
  (set! setup setup-arithmetic) ; setup function
  (set! equal= =) ; setting simple equality test
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start*)
  (case *level**
    ((1)
     (set! *time-factor* 1/2) ; minutes per problem
     (send text-lines insert
           (format "-----   * with multiples of 10 exercises   -----~n"))
     (set! get-problem get-problem*10))
    ((2)
     (set! *time-factor* 2) ; minutes per problem
     (send text-lines insert
           (format "---------   multiplication exercises   ---------~n"))
     (set! get-problem get-problem*))
    ((3)
     (set! *time-factor* 2) ; minutes per problem
     (send text-lines insert
           (format "---------   * exercises with fractions  ---------~n"))
     (set! get-problem get-problem*f))
    (else (error 'start*level)))    
  (set! do-math do-math+) ; set arithmetic operation
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= =) ; setting simple equality test
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start100/)
  (when (or (< *max*table* 5) (> *max*table* 12))
    (error 'max*table-size))
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "---------   division table exercises   ---------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem100/10)
  (set! setup setup-arithmetic) ; setup function
  (set! equal= =) ; setting simple equality test
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start/)
  (set! *time-factor* 3) ; minutes per problem
  (send text-lines insert
        (format "------------   division exercises   ------------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem/)
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= approx=) ; setting approximation equal to 3 decimals
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start/quot)
  (case *level/quot*
    ((1) (set! *time-factor* 2) ; minutes per problem
         (send text-lines insert
               (format "-------   integer division exercises l1  -------~n"))
         (set! equal= =)
         (set! get-problem get-problem/quot1))
    ((2) (set! *time-factor* 2) ; minutes per problem
         (send text-lines insert
               (format "------   integer division + remainder l2  ------~n"))
         (set! equal= rem=)
         (set! get-problem get-problem/quotr2))
    ((3) (set! *time-factor* 5/2) ;minutes per problem
         (send text-lines insert
               (format "-------   integer division exercises l3  -------~n"))
         (set! equal= =)
         (set! get-problem get-problem/quot3))
    ((4) (set! *time-factor* 5/2) ; minutes per problem
         (send text-lines insert
               (format "------   integer division + remainder l4  ------~n"))
         (set! equal= rem=)
         (set! get-problem get-problem/quotr4))
    ((5) (set! *time-factor* 3) ;minutes per problem
         (send text-lines insert
               (format "-------   integer division exercises l5  -------~n"))
         (set! equal= =)
         (set! get-problem get-problem/quot5))
    ((6) (set! *time-factor* 3) ; minutes per problem
         (send text-lines insert
               (format "------   integer division + remainder l6  ------~n"))
         (set! equal= rem=)
         (set! get-problem get-problem/quotr6))
    ((7) (set! *time-factor* 2) ; minutes per problem
         (send text-lines insert
               (format "-------   fraction division exercises l7  -------~n"))
         (set! equal= =)
         (set! get-problem get-problem/quotf7))         
    (else (error "quotient-level")))     
  (set! do-math do-math+) ; set arithmetic operation
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  ;(set! equal= =) ; setting approximation equal to 3 decimals
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start<=>)
  (case *comparison-level*
    ((1) (set! *time-factor* 1/3) ; minutes per problem
         (send text-lines insert
               (format "-------   comparison integer exercises   -------~n"))
         (set! get-problem get-problem<=>))
    ((2) (set! *time-factor* 1/2) ; minutes per problem
         (send text-lines insert
               (format "-----   comparison fraction exercises easy  ----~n"))
         (set! get-problem get-problem<=>fract))
    ((3) (set! *time-factor* 3/2) ; minutes per problem
         (send text-lines insert
               (format "-----   comparison fraction exercises hard  ----~n"))
         (set! get-problem get-problem<=>fract1))
    (else (error 'start-comparison)))
  (set! *used-numbers* '())    
  (set! do-math do-math>) ; set non arithmetic operation
  (set! setup setup-comparison) ; setup function
  (send input-dialog create-status-line)
  (send input-dialog set-status-text "> = <")
  (send input-dialog show #t)
  (send comparison-input enable #t)
  (start-quiz *n* 0))

(define (start-odd/even)
  (set! *time-factor* 1/5) ; minutes per problem
  (send text-lines insert
        (format "------------   odd even exercises   ------------~n"))
  (set! do-math do-math-odd/even) ; set non arithmetic operation
  (set! get-problem get-problem-odd-even)
  (set! setup setup-odd-even) ; setup function
  (set! *used-numbers* '())
  (send odd-even-dialog show #t)
  (start-quiz *n* 0))

(define (start-sequence)
  (send sequence-cheat-button enable #f)
  (set! *cheat-flag* #f)
  (case *sequence-difficulty*
    ((1) (set! *time-factor* 1)
         (send text-lines insert
               (format "-----------   sequence exercises l1  -----------~n")))
    ((2) (set! *time-factor* 2)
         (send text-lines insert
               (format "-----------   sequence exercises l2  -----------~n")))
    ((3 4) (set! *time-factor* 3)
           (send text-lines insert
                 (format "----------   sequence exercises l3(4)  ---------~n")))
    (else (error '*sequence-difficulty*)))
  (set! equal= =)
  (set! do-math do-math-sequence) ; set non arithmetic operation
  (set! get-problem get-problem-sequence)
  (set! setup setup-sequence) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 30) ; max number of exercises allowed
  (send sequence-dialog show #t)
  (send sequence-input enable #t)
  (start-quiz *n* 0))

(define (start-bba)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "------   before between after exercises   ------~n"))
  (send bba-dialog create-status-line)
  (set! equal= =)
  (set! do-math do-math-bba) ; set non arithmetic operation
  (set! get-problem get-problem-bba)
  (set! setup setup-bba) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 30) ; max number of exercises allowed
  (send bba-dialog show #t)
  (send bba-input enable #t)
  (start-quiz *n* 0))

(define (start-a2r)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "-------    Arabic to Roman  exercises    ------~n"))
  (send a2r-dialog create-status-line)
  (set! equal= string=?)
  (set! do-math do-math-a2r) ; set non arithmetic operation
  (set! get-problem get-problem-a2r)
  (set! setup setup-a2r) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* *max-roman-number*) ; max numbers used before resetting
  (send a2r-dialog show #t)
  (send a2r-input enable #t)
  (start-quiz *n* 0))

(define (start-r2a)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "-------    Roman to Arabic  exercises    ------~n"))
  (send r2a-dialog create-status-line)
  (set! equal= =)
  (set! do-math do-math-r2a) ; set non arithmetic operation
  (set! get-problem get-problem-r2a)
  (set! setup setup-r2a) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* *max-roman-number*) ; max numbers used before resetting
  (send r2a-dialog show #t)
  (send r2a-input enable #t)
  (start-quiz *n* 0))

(define (start-money)
  (set! *peso* #f) ; for dime enabling
  (set! *time-factor* 5/2) ; minutes per problem
  (send text-lines insert
        (format "--------    Return Change exercises USD  -------~n"))
  (send money-dialog create-status-line)
  (send money-dialog set-status-text
        (string-append
         "David paid 500$. Return exact amount to him. "
         " Maximum 4 of the same denomination allowed!  "
         " ( q=25 d=10 n=5 c=1 cents )"))
  (set! equal= cents=)
  (set! do-math do-math-money) ; set non arithmetic operation
  (set! get-problem get-problem-money)
  (set! setup setup-money) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 100) ; max numbers used before resetting
  (send money-dialog show #t)
  (enable-disable-money-inputs #t)
  (start-quiz *n* 0))

(define (start-money-p)
  (set! *peso* #t) ; for dime disabling
  (set! *time-factor* 5/2) ; minutes per problem
  (send text-lines insert
        (format "--------   Return Change exercises Peso  -------~n"))
  (send money-dialog create-status-line)
  (send money-dialog set-status-text
        (string-append
         "David paid 500 Pesos.  Return exact amount to him. "
         " Maximum 4 of the same denomination allowed!  "
         " ( q=25 n=5 c=1 centavos )"))
  (set! equal= cents=)
  (set! do-math do-math-money) ; set non arithmetic operation
  (set! get-problem get-problem-money)
  (set! setup setup-money) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 100) ; max numbers used before resetting
  (send money-dialog show #t)
  (enable-disable-money-inputs #f) ; disabling dimes
  (enable-disable-money-inputs-p #t)
  (start-quiz *n* 0))

(define (start-ABC)
  (set! dict (list-copy *dictionary*)) ; initialize working dictionary
  (set! words (length dict)) ; initialize number of words remaining
  (set! *time-factor* 2) ; minutes per problem
  (send text-lines insert
        (format "--------    Sort words alphabetically   --------~n"))
  (send ABC-dialog create-status-line)
  (send ABC-dialog set-status-text
        (string-append
         "Sort the words by entering numbers from 1-5"
         " in the corresponding input fields."))
  (set! equal= equal?)
  (set! do-math do-math-ABC) ; set non arithmetic operation
  (set! get-problem get-problem-ABC)
  (set! setup setup-ABC) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 50) ; max sets of 5 words used before resetting
  (send ABC-dialog show #t)
  (enable-disable-ABC-inputs #t)
  (start-quiz *n* 0))

(define (start-skip)
  (set! *time-factor* 2) ; minutes per problem
  (send text-lines insert
        (format "--------    Skip counting (+) exercise    -------~n"))
  (send skip-dialog create-status-line)
  (send skip-dialog set-status-text
        (string-append
         "Continue entering minimum 9 additional numbers,\
 adding the skip value."
         " Separate numbers by a comma, as in: 2,4,6, ..."))
  (set! equal= check-skip-counting)
  (set! do-math do-math-skip) ; set non arithmetic operation
  (set! get-problem get-problem-skip)
  (set! setup setup-skip) ; setup function
  (set! *used-numbers* '())
  (send skip-dialog show #t)
  (send skip-input enable #t)
  (start-quiz *n* 0))

(define (start-skip-neg)
  (set! *time-factor* 5/2) ; minutes per problem
  (send text-lines insert
        (format "--------    Skip counting (-) exercise    -------~n"))
  (send skip-dialog create-status-line)
  (send skip-dialog set-status-text
        (string-append
         "Continue entering minimum 9 additional numbers,\
 subtracting the skip value."
         " Separate numbers by a comma, as in: 8,6,4, ..."))
  (set! equal= check-skip-counting)
  (set! do-math do-math-skip) ; set non arithmetic operation
  (set! get-problem get-problem-skip-neg)
  (set! setup setup-skip) ; setup function
  (set! *used-numbers* '())
  (send skip-dialog show #t)
  (send skip-input enable #t)
  (start-quiz *n* 0))

(define (start-operators)
  (case *ops-level*
    ((1) (set! *time-factor* 1/2) ; minutes per problem
         (send text-lines insert
               (format "-----  Missing operators l1 (+ -) exercise  -----~n"))
         (set! setup setup-operators1) ; setup function
         (send ops-dialog1 create-status-line)
         (send ops-dialog1 set-status-text
               " Enter operators  ( +  or  - )  inside input field.")
         (send ops-dialog1 show #t)
         (send ops-input1-1 enable #t))
    ((2) (set! *time-factor* 3/2) ; minutes per problem
         (send text-lines insert
               (format "-----  Missing operators l2 (+ -) exercise  -----~n"))
         (set! setup setup-operators2) ; setup function
         (send ops-dialog2 create-status-line)
         (send ops-dialog2 set-status-text
               " Enter operators  ( +  or  - )  inside input fields.")
         (send ops-dialog2 show #t)
         (send ops-input2-1 enable #t)
         (send ops-input2-2 enable #t))
    ((3) (set! *time-factor* 5/2) ; minutes per problem
         (send text-lines insert
               (format "-----  Missing operators l3 (+ -) exercise  -----~n"))
         (set! setup setup-operators3) ; setup function
         (send ops-dialog3 create-status-line)
         (send ops-dialog3 set-status-text
               " Enter operators  ( +  or  - )  inside input fields.")
         (send ops-dialog3 show #t)
         (send ops-input3-1 enable #t)
         (send ops-input3-2 enable #t)
         (send ops-input3-3 enable #t))
    (else (error 'start-operators "*ops-level* invalid value")))
  (set! equal= equal?)
  (set! do-math do-math-operators) ; set non arithmetic operation
  (set! get-problem get-problem-ops)
  (set! *used-numbers* '())
  (start-quiz *n* 0))

(define (start-text)
  (define ty #f)
  (case *gapesa-level*
    ((1) (set! *word-problem* (cons 'handle (shuffle word+problems)))
         (set! *time-factor* 3) (set! ty "     +    "))
    ((2) (set! *word-problem* (cons 'handle (shuffle word-problems)))
         (set! *time-factor* 7/2) (set! ty "    -     "))
    ((3) (set! *word-problem*
               (cons 'handle (append-shuffle word+problems word-problems)))
         (set! *time-factor* 7/2) (set! ty "  + or -  "))
    ((4) (set! *word-problem* (cons 'handle (shuffle word+-problems)))
         (set! *time-factor* 4) (set! ty "    +-    "))
    ((5) (set! *word-problem*
               (cons 'handle (append-shuffle word+problems word-problems
                                             word+-problems)))
         (set! *time-factor* 4) (set! ty "mix + +- -"))
    ((6) (set! *word-problem* (cons 'handle (shuffle word*problems)))
         (set! *time-factor* 5) (set! ty "    *     "))
    ((7) (set! *word-problem* (cons 'handle (shuffle word/problems-easy)))
         (set! *time-factor* 5) (set! ty " / easy   "))
    ((8) (set! *word-problem*
               (cons 'handle (append-shuffle word/problems-easy word*problems)))
         (set! *time-factor* 5) (set! ty " */ easy  "))    
    ((9) (set! *word-problem* (cons 'handle (shuffle word/problems)))
         (set! *time-factor* 6) (set! ty "    /     ")
         (set! equal= approx=)) ; division precision set to 3 decimals, no rounding
    ((10) (set! *word-problem*
                (cons 'handle (append-shuffle word*problems word/problems)))
          (set! *time-factor* 6) (set! ty "    */    ")
          (set! equal= approx=)) ; division precision set to 3 decimals, no rounding
    (else (error *gapesa-level*)))
  (send text-dialog set-label "GAPESA questions")
  (send show-text-window-menu set-label "Show GAPESA Window")
  (send text-lines insert
        (format "------  GAPESA problems ~a exercise  ----~n" ty))
  (send text-dialog create-status-line)
  (send text-dialog set-status-text
        (string-append
         "Read the problem, understand the question, formulate the Equation,"
         " calculate the result, and enter it into the input field."))
  (unless (> *gapesa-level* 8)
    (set! equal= =))
  (set! do-math do-math-text) ; set non arithmetic operation
  (set! get-problem get-problem-text)
  (set! setup setup-text) ; setup function
  (set! word-problem (list-copy *word-problem*)) ; fresh copy
  (send text-dialog show #t)
  (send text-input enable #t)
  (start-quiz *n* 0))

(define (start-Carea)
  (define ty #f)
  (case *Carea-level*
    ((1) (set! *word-problem* (cons 'handle (shuffle circumference1)))
         (set! *time-factor* 3) (set! ty "level-1"))
    ((2) (set! *word-problem* (cons 'handle (shuffle circumference2)))
         (set! *time-factor* 4) (set! ty "level-2"))
    ((3) (set! *word-problem*
               (cons 'handle (append-shuffle circumference1 circumference2)))
         (set! *time-factor* 4) (set! ty "level-3"))
    ((4) (set! *word-problem* (cons 'handle (shuffle area1)))
         (set! *time-factor* 3) (set! ty "level-4 "))
    ((5) (set! *word-problem*
               (cons 'handle (append-shuffle circumference1 area1)))
         (set! *time-factor* 3) (set! ty "level-5 "))
    ((6) (set! *word-problem* (cons 'handle (shuffle area2)))
         (set! *time-factor* 5) (set! ty "level-6 "))
    ((7) (set! *word-problem*
               (cons 'handle (append-shuffle area1 area2)))
         (set! *time-factor* 4) (set! ty "level-7 "))
    ((8) (set! *word-problem*
               (cons 'handle (append-shuffle circumference2 area2)))
         (set! *time-factor* 5) (set! ty "level-8 "))
    (else (error *Carea-level*)))
  (case *Carea-level*
    ((1 2 3) 
     (send text-lines insert
           (format "----   Perimeter problems ~a exercise   ---~n" ty))
     (send text-dialog set-label "Perimeter questions")
     (send show-text-window-menu set-label "Show Perimeter Window"))
    ((4 6 7)
     (send text-lines insert
           (format "------   Area problems ~a exercise   -----~n" ty))
     (send text-dialog set-label "Area questions")
     (send show-text-window-menu set-label "Show Area Window"))
    ((5 8)
     (send text-lines insert
           (format "--  Perimeter/Area problems ~a exercise  --~n" ty))
     (send text-dialog set-label "Perimeter/Area questions")
     (send show-text-window-menu set-label "Show Perimeter/Area Window")))
  (send text-dialog create-status-line)
  (send text-dialog set-status-text
        (string-append
         "Read the problem, understand the question, formulate the Equation,"
         " calculate the result, and enter it into the input field."))
  (set! equal= approx=)
  (set! do-math do-math-text) ; set non arithmetic operation
  (set! get-problem get-problem-text)
  (set! setup setup-text) ; setup function
  (set! word-problem (list-copy *word-problem*)) ; fresh copy
  (send text-dialog show #t)
  (send text-input enable #t)
  (start-quiz *n* 0))

(define (start-findX)
  (define ty #f)
  (case *findX-level*
    ((1) (set! *word-problem* (cons 'handle (shuffle operand+-)))
         (set! *time-factor* 2) (set! ty "level-1"))
    ((2) (set! *word-problem* (cons 'handle (shuffle operand*/)))
         (set! *time-factor* 3) (set! ty "level-2"))
    ((3) (set! *word-problem*
               (cons 'handle (append-shuffle operand+- operand*/)))
         (set! *time-factor* 3) (set! ty "level-3"))
    (else (error *findX-level*)))
  (send text-lines insert
        (format "--  Missing operand problems ~a exercise  --~n" ty))
  (send text-dialog set-label "Missing operand questions")
  (send show-text-window-menu set-label "Show Missing operand Window")
  (send text-dialog create-status-line)
  (send text-dialog set-status-text
        (string-append
         "Read the problem, understand the question, formulate the Equation,"
         " calculate the result, and enter it into the input field."))
  (set! equal= =)
  (set! do-math do-math-text) ; set non arithmetic operation
  (set! get-problem get-problem-text)
  (set! setup setup-text) ; setup function
  (set! word-problem (list-copy *word-problem*)) ; fresh copy
  (send text-dialog show #t)
  (send text-input enable #t)
  (start-quiz *n* 0))

(define (start-time)
  (set! *time-flag* #t)
  (define ty #f)
  (case *time-level*
    ((1) (set! *word-problem* (cons 'handle (shuffle time-passed1)))
         (set! *time-factor* 3) (set! ty "level-1"))
    ((2) (set! *word-problem* (cons 'handle (shuffle time-passed2)))
         (set! *time-factor* 4) (set! ty "level-2"))
    ((3) (set! *word-problem*
               (cons 'handle (append-shuffle time-passed1 time-passed2)))
         (set! *time-factor* 4) (set! ty "level-3"))
    (else (error *time-level*)))
  (send text-lines insert
        (format "---  Time elapsed problems ~a exercise  ---~n" ty))
  (send text-dialog set-label "Time elapsed questions")
  (send show-text-window-menu set-label "Show Time elapsed Window")
  (send text-dialog create-status-line)
  (send text-dialog set-status-text
        (string-append
         "Read the problem, calculate result, and enter it into the input field."
         "  Example input: 5h 15m. No spaces between 5 h or 15 m!"))
  (set! equal= =)
  (set! do-math do-math-text) ; set non arithmetic operation
  (set! get-problem get-problem-text)
  (set! setup setup-text) ; setup function
  (set! word-problem (list-copy *word-problem*)) ; fresh copy
  (send text-dialog show #t)
  (send text-input enable #t)
  (start-quiz *n* 0))

;;; ==============================================================

(define (cents= n1 n2)
  (< (abs (- n1 n2)) 0.001))

(define (enable-disable-money-inputs flag)
  (for-each (lambda (x) (send x enable flag))
            (list money-input-100 money-input-50 money-input-20 money-input-10
                  money-input-5 money-input-1 money-input-q
                  money-input-d money-input-n money-input-c)))

(define (enable-disable-money-inputs-p flag)
  (for-each (lambda (x) (send x enable flag))
            (list money-input-100 money-input-50 money-input-20 money-input-10
                  money-input-5 money-input-1 money-input-q
                  money-input-n money-input-c)))

(define (enable-disable-ABC-inputs flag)
  (for-each (lambda (x) (send x enable flag))
            (list ABC-input-1 ABC-input-2 ABC-input-3 ABC-input-4 ABC-input-5)))

(define (start-pvalue)
  (set! *time-factor* 1/5) ; minutes per problem
  (send text-lines insert
        (format "---------   position value exercises   ---------~n"))
  (set! equal= =)
  (set! do-math do-math-pvalue) ; set non arithmetic operation
  (set! get-problem get-problem-pvalue)
  (set! setup setup-pvalue) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 90)
  (send pvalue-dialog show #t)
  (start-quiz *n* 0))

(define (start-round)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "-----------   round number exercises   ----------~n"))
  (send round-dialog create-status-line)
  (send round-dialog set-status-text
        "Round on RED digit.    Rules: next digit >= 5, round up!")
  (set! equal= =)
  (set! do-math do-math-round) ; set non arithmetic operation
  (set! get-problem get-problem-rounding)
  (set! setup setup-round) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 90)
  (send round-dialog show #t)
  (start-quiz *n* 0))

(define (start-ord)
  (set! *time-factor* 1/4) ; minutes per problem
  (send text-lines insert
        (format "----------   ordinal number exercises   ---------~n"))
  (send ord-dialog create-status-line)
  (send ord-dialog set-status-text
        "Enter ordinal number.  Example: for prompt 5, enter 5th")
  (set! equal= string=?)
  (set! do-math do-math-ord) ; set non arithmetic operation
  (set! get-problem get-problem-ord)
  (set! setup setup-ord) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 90)
  (send ord-dialog show #t)
  (start-quiz *n* 0))

(define (start-fraction)
  (send fraction-dialog create-status-line)
  (disable/enable-input-fields #f)
  (case *fraction-level*
    ((1) (set! *time-factor* 1/3) ; minutes per problem
         (set! equal= =)
         (send text-lines insert
               (format "-----------   fraction exercises l1  -----------~n"))
         (set! do-math do-math-fraction) ; set fraction reading operation
         (send fraction-dialog set-status-text
               " enter red-slices/all-slices into the left input field")
         (send fraction-input-left enable #t)
         (set! get-problem get-problem-fraction))
    ((2 3)
     (set! get-problem get-problem-fraction<=>)
     (case *fraction-level*
       ((2) (set! *time-factor* 2/3) ; minutes per problem
            (send text-lines insert
                  (format "-------   fraction comparison exercises l2  -----~n"))
            (send fraction-dialog set-status-text
                  " enter one of < = > into the middle input field")
            (set! do-math do-math>) ; set fraction comparing operation
            (send fraction-input enable #t))
       ((3) (set! *time-factor* 1) ; minutes per problem
            (send text-lines insert
                  (format "-------   fraction comparison exercises l3  -----~n"))
            (send fraction-dialog set-status-text
                  " enter: left fraction,  < = >,\
  right fraction, into input fields")         
            (set! do-math do-math-fract>) ; set fraction comparing operation
            (disable/enable-input-fields #t))))
    ((4)
     (set! *time-factor* 3/2) ; minutes per problem
     (send text-lines insert
           (format "-------   fraction comparison exercises l4  -----~n"))
     (send fraction-dialog set-status-text
           " enter: left fraction,  < = >,\
  right fraction, into input fields")         
     (set! get-problem get-problem-fraction-full<=>)
     (set! do-math do-math-fract>) ; set fraction comparing operation
     (disable/enable-input-fields #t)))
  (send fraction-dialog show #t)
  (set! setup setup-fraction) ; setup function
  (set! *used-numbers* '())
  (set! *canvas-flag* #t) ; not a void call
  (start-quiz *n* 0))

(define (start-clock)
  (send clock-dialog create-status-line)
  (case *clock-level*
    ((1) (set! *time-factor* 1/2) ; minutes per problem
         (send text-lines insert
               (format "--------------  clock exercises l1 -------------~n"))
         (set! do-math do-math-clock) ; set non arithmetic operation
         (send clock-dialog set-status-text "enter  hr:mn")
         (set! get-problem get-problem-clock)
         (set-problem-z! *problem* '(#f #f ""))) ; clearing promt message   
    ((2 3 4 5)
     (let ((level #f))
       (case *clock-level*
         ((2) (set! *before/after-clock* '(-60 60))
              (set! level "l2"))
         ((3) (set! *before/after-clock* '(-30 -60 -30 30 60 30))
              (set! level "l3"))
         ((4) (set! *before/after-clock* '(-60 -30 -15 60 30 15))
              (set! level "l4"))
         ((5) (set! *before/after-clock*
                    (remove-duplicates
                     (map (lambda (x) (if (zero? (random 2)) x (* -1 x)))
                          (map (lambda (x) (- x (modulo x 10)))
                               (build-list 60 (lambda (x) (random (+ 10 x) 601)))
                               ))))
              (set! level "l5")))
       (set! *time-factor* 3/2) ; minutes per problem
       (send text-lines insert
             (format "-------  clock before/after exercises ~a -------~n" level)))
     (set! do-math do-math-clock-BA)
     (send clock-dialog set-status-text "enter new time in  hr:mn")
     (set! get-problem get-problem-clock-BA))
    (else (error '*clock-level*)))
  (set! equal= clock=)
  (set! setup setup-clock) ; setup function
  (set! *used-numbers* '())
  (set! *canvas-flag* #t) ; not a void call
  (set! *max-used-pairs* 120)
  (send clock-dialog show #t)
  (send clock-input enable #t)
  (start-quiz *n* 0))

(define (disable/enable-start-buttons t/f)
  (for-each (lambda (b) (send b enable t/f))
            (list start+-button start10*-button start*-button
                  start100/-button start/-button start/quot-button start<=>button
                  start-odd/even-button start-sequence-button start-ord-button
                  start-bba-button start-pvalue-button start-fraction-button
                  start-clock-button start-a2r-button start-r2a-button
                  start-money-button start-money-p-button start-ABC-button
                  start-skip-button start-skip-neg-button start-text-button
                  start-Carea-button start-findX-button start-round-button
                  start-time-button start-operators-button)))

(define (disable/enable-set/font-menu t/f)
  (for-each (lambda (m) (send m enable t/f))
            (list set-n-exercises set-+-level set-max-table set-left-number
                  set-speed-% set-/-precision set-sequence-level set-/quot-level
                  set-fraction-slices clear-reports set-max-roman-number
                  set-all-fonts set-comparison-level set-status-msg-font
                  set-input-font set-doc-font set-about-font set-report-font
                  set-button-font set-clock-level set-fraction-level
                  set-skip-increment set-text-level set-circumference-level
                  menu-item-doc menu-item-about menu-item-update
                  set-findX-level set-time-level set-*-level
                  set-fraction-denominator set-operators-level))
  (check-update-menu))

(provide disable/enable-popup-window-menu)
(define (disable/enable-popup-window-menu t/f)
  (for-each (lambda (menu) (send menu enable t/f))
            (list show-compare-window-menu show-odd-even-window-menu
                  show-sequence-window-menu show-bba-window-menu
                  show-pvalue-window-menu show-fraction-window-menu
                  show-clock-window-menu show-a2r-window-menu
                  show-r2a-window-menu show-money-window-menu
                  show-ABC-window-menu show-skip-window-menu
                  show-text-window-menu show-round-window-menu
                  show-ord-window-menu show-operators-window-menu)))

(define (disable/enable-dialog-show t/f)
  (for-each (lambda (window) (send window show t/f))
            (list odd-even-dialog input-dialog sequence-dialog bba-dialog
                  pvalue-dialog round-dialog fraction-dialog clock-dialog
                  a2r-dialog r2a-dialog money-dialog ABC-dialog skip-dialog
                  ops-dialog1 ops-dialog2 ops-dialog3 text-dialog ord-dialog)))

(define (disable/enable-input-fields t/f)
  (for-each (lambda (input) (send input enable t/f))
            (list comparison-input sequence-input bba-input clock-input
                  a2r-input r2a-input skip-input text-input round-input
                  ord-input))
  (enable-disable-ops-inputs t/f)
  (enable-disable-sequence-cheat-button t/f)
  (enable-disable-ABC-inputs t/f)
  (enable-disable-fraction-inputs t/f)
  (if (and t/f *peso*)
      (enable-disable-money-inputs-p #t)
      (enable-disable-money-inputs t/f)))

(define (enable-disable-ops-inputs flag)
  (for-each (λ (x) (send x enable flag))
            (list ops-input1-1 ops-input2-1 ops-input2-2 ops-input3-1
                  ops-input3-2 ops-input3-3)))

(define (enable-disable-sequence-cheat-button flag)
  (send sequence-cheat-button enable #f)
  (when (and flag (= *sequence-difficulty* 4) (not *cheat-flag*))
    (send sequence-cheat-button enable #t)))

(define (enable-disable-fraction-inputs flag)
  ; first disable all
  (let ((fields (list fraction-input-left fraction-input fraction-input-right)))
    (for-each (lambda (input) (send input enable #f)) fields)
    ; enable required
    (when flag
      (case *fraction-level*
        ((1) (send fraction-input-left enable #t))
        ((2) (send fraction-input enable #t)) ; middle
        ((3 4) (for-each (lambda (input) (send input enable #t)) fields))
        (else (error 'fraction-inputs))))))

;;; Math quiz part
;;; ===============================================================

(define (start-quiz [n (state-problems *state*)] [err 0])
  (send stop-button enable #t)
  (disable/enable-start-buttons #f)
  (disable/enable-set/font-menu #f)
  (send number-input set-value "")
  (initialize-state n err)
  (when (>= n *min-break-ex*) ; if less than 5, no break allowed
    (send wiwi-button enable #t))
  (set! *wiwi* #f)
  (set! *wiwi-time* 0) ; time used for toilet :-)
  (set! *time-start* (current-seconds))
  (set! *allowed-time* (exact-ceiling (* n *time-factor* *speed-factor*)))
  (send text-lines change-style style-delta-blue)
  (send text-lines insert (msg1 n))
  (send text-lines change-style style-delta-black)
  (send text-lines insert (msg-separator))
  (send text-output set-editor text-lines)
  (setup))

(define (setup-arithmetic)
  (set! *exec-button* calc-button)
  (send show-compare-window-menu enable #f)
  (send calc-button enable #t)
  (send number-input enable #t)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send operation-msg set-label (msg8 (problem-x *problem*)
                                      (show (problem-op *problem*))
                                      (problem-y *problem*))))

(define (setup-comparison)
  (set! *exec-button* compare-button)
  (send show-compare-window-menu enable #t)
  (send compare-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (let ((left (if (number? (problem-x *problem*))
                  (number->string (problem-x *problem*))
                  (problem-x *problem*)))
        (right (if (number? (problem-y *problem*))
                   (number->string (problem-y *problem*))
                   (problem-y *problem*))))                 
    (send left-prompt set-label left)
    (send right-prompt set-label right)))

(define (setup-a2r)
  (set! *exec-button* a2r-button)
  (send show-a2r-window-menu enable #t)
  (send a2r-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send a2r-prompt set-label (number->string (problem-x *problem*))))

(define (setup-r2a)
  (set! *exec-button* r2a-button)
  (send show-r2a-window-menu enable #t)
  (send r2a-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send r2a-prompt set-label (problem-x *problem*)))

(define (setup-money)
  (set! *exec-button* money-button)
  (send show-money-window-menu enable #t)
  (send money-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send money-prompt set-label
        (string-append "Price: " (decimal-points (problem-x *problem*)))))

(define (setup-ABC)
  (set! *exec-button* ABC-button)
  (send show-ABC-window-menu enable #t)
  (send ABC-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send ABC-prompt-1 set-label (string-append "< "(first (problem-x *problem*))))
  (send ABC-prompt-2 set-label (string-append "< "(second (problem-x *problem*))))
  (send ABC-prompt-3 set-label (string-append "< "(third (problem-x *problem*))))
  (send ABC-prompt-4 set-label (string-append "< "(fourth (problem-x *problem*))))
  (send ABC-prompt-5 set-label (string-append "< "(fifth (problem-x *problem*)))))

(define (setup-operators1)
  (set! *exec-button* ops-button1)
  (send show-operators-window-menu enable #t)
  (send ops-button1 enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send ops-prompt1-1 set-label (first (second (problem-y *problem*))))
  (send ops-prompt1-2 set-label (second (second (problem-y *problem*))))
  (send ops-prompt1-3 set-label (third (second (problem-y *problem*)))))

(define (setup-operators2)
  (set! *exec-button* ops-button2)
  (send show-operators-window-menu enable #t)
  (send ops-button2 enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send ops-prompt2-1 set-label (first (second (problem-y *problem*))))
  (send ops-prompt2-2 set-label (second (second (problem-y *problem*))))
  (send ops-prompt2-3 set-label (third (second (problem-y *problem*))))
  (send ops-prompt2-4 set-label (fourth (second (problem-y *problem*)))))

(define (setup-operators3)
  (set! *exec-button* ops-button3)
  (send show-operators-window-menu enable #t)
  (send ops-button3 enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send ops-prompt3-1 set-label (first (second (problem-y *problem*))))
  (send ops-prompt3-2 set-label (second (second (problem-y *problem*))))
  (send ops-prompt3-3 set-label (third (second (problem-y *problem*))))
  (send ops-prompt3-4 set-label (fourth (second (problem-y *problem*))))
  (send ops-prompt3-5 set-label (fifth (second (problem-y *problem*)))))
  
(define (setup-skip)
  (set! *exec-button* skip-button)
  (send show-skip-window-menu enable #t)
  (send skip-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send skip-prompt
        set-label (string-append "skip by " (number->string (problem-op *problem*))))
  (send skip-input set-value
        (string-append (number->string (problem-x *problem*)) ",")))

(define (decimal-points n)
  (define (dps nlst)
    (if (char=? (car nlst) #\.)
        (if (< (length (cdr nlst)) 2)
            (append nlst '(#\0))
            nlst)
        (cons (car nlst) (dps (cdr nlst)))))
  (list->string (dps (string->list (number->string n)))))

(define (setup-text)
  (set! *exec-button* text-button)
  (send show-text-window-menu enable #t)
  (send text-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (send text-prompt erase) ; clear canvas
  (get-problem)
  (send text-prompt insert (problem-x *problem*)))

(define (setup-odd-even)
  (set! *exec-button* odd-even-button)
  (send show-odd-even-window-menu enable #t)
  (send odd-even-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send odd-even-prompt set-label (number->string (problem-x *problem*))))

(define (setup-sequence)
  (set! *exec-button* sequence-button)
  (set! get-sequence (choose-sequence-level))
  (send show-sequence-window-menu enable #t)
  (send sequence-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send left-sequence-prompt set-label
        (format "~a  ~a  ~a"
                (first (problem-x *problem*))
                (second (problem-x *problem*))
                (third (problem-x *problem*))))
  (send right-sequence-prompt set-label
        (number->string (first (problem-y *problem*))))
  (when (= *sequence-difficulty* 4)
    (set! *cheat-flag* #f)
    (send sequence-cheat-button enable #t)))

(define (setup-bba)
  (set! *exec-button* bba-button)
  (send show-bba-window-menu enable #t)
  (send bba-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (send left-bba-prompt set-label input-label) ; clearing old prompt
  (send right-bba-prompt set-label input-label)  
  (get-problem)
  (case (show (problem-op *problem*))
    ((after) 
     (send left-bba-prompt set-label (number->string (first (problem-x *problem*)))))
    ((between)
     (send left-bba-prompt set-label (number->string (first (problem-x *problem*))))
     (send right-bba-prompt set-label (number->string (third (problem-x *problem*)))))
    ((before)
     (send right-bba-prompt set-label (number->string (third (problem-x *problem*)))))
    (else (error 'setup-bba))))

(define (setup-pvalue)
  (set! *exec-button* pvalue-button)
  (send show-pvalue-window-menu enable #t)
  (send pvalue-button enable #t)
  (send pvalue-th-prompt set-color "black")
  (send pvalue-hu-prompt set-color "black")
  (send pvalue-te-prompt set-color "black")
  (send pvalue-on-prompt set-color "black")
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send pvalue-th-prompt set-label (first (problem-x *problem*)))
  (send pvalue-hu-prompt set-label (second (problem-x *problem*)))
  (send pvalue-te-prompt set-label (third (problem-x *problem*)))
  (send pvalue-on-prompt set-label (fourth (problem-x *problem*)))
  (case (show (problem-op *problem*))
    ((thousands) 
     (send pvalue-th-prompt set-color "red"))
    ((hundreds)
     (send pvalue-hu-prompt set-color "red"))
    ((tens)
     (send pvalue-te-prompt set-color "red"))
    ((ones)
     (send pvalue-on-prompt set-color "red"))
    (else (error 'setup-pvalue))))

(define (setup-round)
  (set! *exec-button* round-button)
  (send show-round-window-menu enable #t)
  (send round-button enable #t)
  (send round-head-prompt set-color "black")
  (send round-digit-prompt set-color "black")
  (send round-tail-prompt set-color "black")
  (send number-input enable #f)
  (send round-input enable #t)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (let ((hdt (problem-x *problem*)))
    (send round-head-prompt set-label (first hdt))
    (send round-digit-prompt set-label (second hdt))
    (send round-tail-prompt set-label (third hdt)))
  (send round-digit-prompt set-color "red"))

(define (setup-ord)
  (set! *exec-button* ord-button)
  (send show-ord-window-menu enable #t)
  (send ord-button enable #t)
  (send number-input enable #f)
  (send ord-input enable #t)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send ord-prompt set-label (problem-x *problem*)))

(define (setup-fraction)
  (set! *exec-button* fraction-button)
  (send show-fraction-window-menu enable #t)
  (send fraction-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send fraction-canvas on-paint))

(define (setup-clock)
  (set! *exec-button* clock-button)
  (send show-clock-window-menu enable #t)
  (send clock-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send clock-prompt set-label (third (problem-z *problem*)))
  (send clock-canvas on-paint))

(define (choose-sequence-level)
  (case *sequence-difficulty*
    ((1) get-sequence-1)
    ((2) get-sequence-2)
    ((3 4) get-sequence-3)
    (else (error 'get-sequence-level))))

(define (initialize-state problems errors)
  (set-state-question! *state* 1)
  (set-state-problems! *state* problems)
  (set-state-mistakes! *state* errors))

(define (push-used! x)
  (unless (memv x *used-numbers*)
    (set! *used-numbers* (cons x *used-numbers*))))

(define (check-used x op y [max 85])
  (cond ((and (member x *used-numbers* =) (member y *used-numbers* =))
         (when (>= (length *used-numbers*) max)
           (set! *used-numbers* '()))
         (get-problem)) ; look for new set of numbers
        (else
         (initialize-problem x op y)
         (push-used! x)
         (push-used! y))))

(define (check-used-pairs x op y)
  (let ((xy (cons x y)) (yx (cons y x)))
    (if (or (member xy *used-numbers* equal?)
            (member yx *used-numbers* equal?))
        (begin
          (check-overflow) ; free used number pairs if all used
          (get-problem)) ; look for new set of numbers
        (begin
          (initialize-problem x op y)
          (push-used! xy)
          (push-used! yx)))))

(define (check-used-sequence x op y)
  (if (member x *used-numbers* equal?)
      (begin
        (check-overflow) ; free used number pairs if all used
        (get-problem)) ; look for new set of numbers
      (begin
        (initialize-problem x op y)
        (push-used! x))))

(define (check-used-ABC x op y)
  (initialize-problem x op y)
  (set! *used-numbers* (cons "ABC" *used-numbers*))
  (check-overflow) ; check if (* 5 *used-numbers*) words removed from dict
  (when (null? *used-numbers*)
    (set! dict (list-copy *dictionary*)) ; after overflow repopulate dict
    (set! words (length dict)))) ; and restore word count
      
(define (check-overflow)
  (when (>= (length *used-numbers*) *max-used-pairs*)
    (set! *used-numbers* '()))) ; free used pairs of numbers
        
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

(define (kombinations n m)
  (quotient (fact n) (fact (- n m))))

;;; Setting problems for different Start buttons
;;; (+/-), (10*10), (*), (100/10), (/) , (<=>)...

(define (get-problem-2d+)
  (define (get-left)
    (let ((left (random 13 (random 25 100))))
      (if (memv left *used-numbers*)
          (get-left)
          left)))
  (define (get-right left)
    (let* ((ones-l (modulo left 10))
           (ones-r (random 0 (- 10 ones-l)))
           (tens (random 1 (- 11 (quotient left 10)))) ; limiting sum to 112
           (right (+ (* tens 10) ones-r)))
      right))
  (let* ((left (get-left))
         (right (get-right left)))
    (check-used left plus right)))

(define (get-problem-2d+-)
  (define (get-left)
    (let ((left (random 23 100)))
      (if (memv left *used-numbers*)
          (get-left)
          left)))
  (define (get-right left)
    (let* ((ones-l (modulo left 10))
           (ones-r (random 0 (add1 ones-l)))
           (tens (random 1 (/ (- left ones-l) 10)))
           (right (+ (* tens 10) ones-r)))
      right))
  (let* ((op-list (list minus plus minus))
         (op (list-ref op-list (random (length op-list)))))
    (if (eq? (show op) '-)
        (let* ((left (get-left))
               (right (get-right left)))
          (check-used left op right))
        (get-problem-2d+))))

(define (get-problem-fast+-)
  (define (get-right left)
    (if (> left 9)
        (random 1 10)
        (random 1 left)))
  (let* ((op-list (list minus plus minus plus minus))
         (op (list-ref op-list (random (length op-list)))))
    (if (eq? (show op) '-)
        (let* ((left (random 6 19))
               (right (get-right left)))
          (check-used-pairs left op right))
        (let* ((left (random 5 10))
               (right (random 1 10)))
          (check-used-pairs left op right)))))

(define (get-problem+-)
  (let* ((op-list (list minus plus minus plus minus))
         (op (list-ref op-list (random 0 (length op-list)))) ; minus weighted 3/5
         (x (get-left-number (random 13 *left-number*) op))) ; min 2 for (-)
    (let ((y (if (eq? (show op) '+)
                 (get-right-number+ x (random 1 *left-number*))
                 (get-right-number- x (random 1 x)))))
      (check-used x op y))))

(define (get-problem--)
  (let* ((op minus)
         (x (get-left-number (random 13 *left-number*) op))) ; min 2 for (-)
    (let ((y (+ x (random 1 100))))
      (check-used x op y))))

;;; Multiplication & division problems with local tables
;;; ================================================================
;;; this was all transfered to fract-domain.rkt
;;; ================================================================

(define (get-problem*)
  (let ((op mult)
        (x (random 2 *left-number*)) ; range between 2 to *left-number* - 1
        (y (random 2 *left-number*))) ; range between 2 to *left-number* - 1
    (check-used x op y)))

(define (get-problem*10)
  "Multiplying with multiples of 10"
  (let* ((op mult)
         (x (random 2 *left-number*)) ; range between 2 to *left-number* - 1
         (ys (list 10 20 30 40 50 10
                   100 200 300 400 100
                   1000 2000 3000 10 100 1000 10000))
         (y (list-ref ys (random (length ys)))))
    (check-used x op y)))

(define (get-problem/)
  (let* ((op div)
         ; same algorithm as for (-)
         (x1 (get-left-number- (random 0 *left-digit*)))
         (x (if (<= x1 2) (add1 x1) x1))
         ; choose randomly from 2 up to Min of left number and (x)
         (y1 (random 2 (min *left-number* x)))
         (decide '(#f #t #f #t #f))
         (y (if (and (> y1 50) (list-ref decide (random (length decide))))
                (quotient y1 2)
                y1)))
    (check-used x op y)))

(define (get-problem/quot1)
  (let* ((op div)
         (q (random 5 100))
         (y (random 2 10))
         (x (* q y)))
    (if (<= x *left-number*)
        (check-used x op y)
        (get-problem/quot1))))

(define (get-problem/quotr2)
  (let* ((op divr)
         (q (random 5 100))
         (y (random 2 10))
         (x (* q y))
         (r (+ x (random 1 y))))
    (if (<= r *left-number*)
        (check-used r op y)
        (get-problem/quotr2))))

(define (get-problem/quot3)
  (let* ((op div)
         (q (random 5 100))
         (y (random 11 40))
         (x (* q y))
         (r (+ x (random 1 y))))
    (if (<= x *left-number*)
        (check-used x op y)
        (get-problem/quot3))))

(define (get-problem/quotr4)
  (let* ((op divr)
         (q (random 5 100))
         (y (random 11 40))
         (x (* q y))
         (r (+ x (random 1 y))))
    (if (<= r *left-number*)
        (check-used r op y)
        (get-problem/quotr4))))

(define (get-problem/quot5)
  (let* ((op div)
         (q (random 3 42))
         (y (random 15 50))
         (x (* q y)))
    (check-used x op y)))

(define (get-problem/quotr6)
  (let* ((op divr)
         (q (random 3 42))
         (y (random 15 50))
         (x (* q y))
         (r (+ x (random 1 y))))
    (check-used r op y)))

(define (get-problem<=>)
  (let* ((op comp<=>) ; not a real operation
         (x (random 13 *left-number*))
         (incr '(10 5 1 0 -10 -5 -1))
         (y (+ x (list-ref incr (random 0 (length incr))))))
    (check-used x op y)))

(define (get-problem-odd-even)
  (let* ((op odd/even) ; not a real operation
         (rn-list (list (random 73 *left-number*)
                        (random 53 *left-number*)
                        (random 0 50)
                        (random 0 10)))
         (x (list-ref rn-list (random 0 (length rn-list)))))
    (check-used x op x)))

(define (get-problem-sequence)
  (let* ((op IQ) ; not a real operation
         (seq (up-down (get-sequence)))
         (x (take seq 4))
         (y (list(last seq))))
    (check-used-sequence x op y)))

(define (get-problem-a2r)
  (let* ((op A2R) ; not a real operation
         (an (random 1 (add1 *max-roman-number*)))
         (rn (roman an)))
    (check-used-sequence an op rn)))

(define (get-problem-r2a)
  (let* ((op R2A) ; not a real operation
         (an (random 1 (add1 *max-roman-number*)))
         (rn (roman an)))
    (check-used-sequence rn op an)))

(define (get-problem-money)
  (let* ((op MONEY) ; not a real operation
         (toy-price (get-toy-price))
         (cash-return (/ (round (* 100 (- *payment* toy-price))) 100.0)))
    (check-used-sequence toy-price op cash-return)))

(define (get-toy-price)
  (let ((toy-dollar (number->string (random 30 *max-toy-price*)))
        (toy-cents (number->string (random 0 100))))
    (string->number (string-append toy-dollar "." toy-cents))))

(define (get-problem-ABC)
  (let* ((op ABC) ; not a real operation
         (start (random 0 (- words 66)))
         (end (random (+ start 30) (random (+ start 31) (- words 4))))
         (chosen-words (map symbol->string (pick-n 5 start end))))
    (check-used-ABC chosen-words op (sort chosen-words string<?))))

(define (get-problem-skip)
  (let ((incr (random (- *max-skip-increment* (skip-variation))
                      (add1 *max-skip-increment*))) ; choosing increment
        (start (random 100)))
    (check-used start incr start)))

(define (skip-variation)
  (if (= (state-question *state*) 1) ; first question, stick to chosen increment
      0 ; no variation
      (case *max-skip-increment*
        ((2 3) 0)
        ((4 5 6) 1)
        ((7 8 9 10) 2)
        (else (error 'max-skip-increment)))))

(define (get-problem-skip-neg)
  (let* ((incr (* -1 (random (- *max-skip-increment* (skip-variation))
                             (add1 *max-skip-increment*)))) ; choosing increment
         (rand-start (* (abs incr) 10))
         (start (random rand-start (+ rand-start 70))))
    (check-used start incr start)))

(define (get-problem-text)
  (let* ((len (sub1 (length word-problem))) ; discounting 'handle
         (wp (nth! word-problem (cdr word-problem) (random len)))
         (text (first wp))
         (rangel (second wp))
         (test (third wp))
         (equation (fourth wp)))
    (define (parse-rangel range)
      (let ((r-length (length range)))
        (if (= r-length 2)
            (apply random range)
            (list-ref range (random r-length)))))
    (define (get-inputs)
      (let ((inputs
             (do ([parameters
                   (map parse-rangel rangel)
                   (map parse-rangel rangel)])
               ((apply test parameters) parameters))))
        (let*-values
            ([(formula-show formula-calc) (apply equation inputs)]
             [(result) (evaluate formula-calc)])
          (cond
            ((< result 0) (get-inputs))
            (else
             (when *time-flag* (set! inputs (map min->time inputs))); changing to hh:mm
             (let ((problem (apply format text inputs)))
               (set-problem-x! *problem* problem)
               (set-problem-y! *problem* result)
               (set-problem-op! *problem* formula-show)
               #;(println (truncate-result (problem-y *problem*)
                                           *exponent*)) ; for quick checking
               ))))))
    (when (= len 1) ; last problem consumed
      (set! word-problem
            (cons 'handle (list-copy (shuffle (cdr *word-problem*)))))) ; restore problem set
    (get-inputs)))

(define (get-problem-ops)
  (let* ((input (get-problem-operators))
         (numbers (car input)))
    (check-used-sequence
     numbers (list (second input) (third input)) (last input)))
  (set-problem-x! *problem* (first (problem-op *problem*)))
  (set-problem-op! *problem* (second (problem-op *problem*))))
    
;;; ==========================================================

(define (get-problem-bba)
  (let* ((op (choose-bba-op))
         (n (random 39 *left-number*))
         (bfr (if (and (zero? (random 4)) (> n 100))
                  (- n (modulo n 100))
                  (- n (modulo n 10))))
         (aftr (sub1 bfr))
         (x (case (show op)
              ((before) (list (- bfr 2) (- bfr 1) bfr))
              ((after) (list aftr (+ 1 aftr) (+ 2 aftr)))
              (else (list n (+ 1 n) (+ 2 n)))))
         (y (second x)))
    (send bba-dialog set-status-text (symbol->string (show op)))
    (check-used-sequence x op y)))

(define (get-problem-pvalue)
  (let* ((x (map number->string (get-pvalue-number 4 '())))
         (y (random (length x)))
         (op (choose-pvalue-op y))) ; for show
    (check-used-sequence x op y)))

(define (get-problem-rounding)
  (let* ((problem-set (get-problem-round))
         (x (first problem-set))
         (op (second problem-set))
         (y (third problem-set)))
    (check-used-sequence x op y)))

(define (get-problem-ord)
  (let-values ([(x op y) (get-problem-ordinal)])
    (check-used-sequence x op y)))
        
(define (get-problem-clock)
  (let* ((m-list (list (* (random 12) 5) (random 60)))
         (minute (list-ref m-list (random (length m-list)))) 
         (hour (random 12))
         (op clock))
    (check-used-pairs hour op minute)))

(define (get-problem-clock-BA)
  (let ((ba (list-ref *before/after-clock*
                      (random (length *before/after-clock*)))))
    (get-problem-clock)
    (set-problem-z! *problem* (clock-BA-time ba))))

(define (clock-BA-time delta)
  (let* ((h (problem-x *problem*))
         (m (problem-y *problem*))
         (new-time (modulo (+ m (* h 60) delta 720) 720))
         (hrs (quotient new-time 60))
         (min (modulo new-time 60))
         (b/a (if (< delta 0) "before" "after"))
         (m/h (string-append
               (number->string (quotient (abs delta) 60)) "h "
               (number->string (modulo (abs delta) 60)) "m ")))
    (list hrs min (string-append m/h b/a))))

(define (get-pvalue-number n acc)
  (let ((digit (random 1 10)))
    (cond ((zero? n) acc)
          ((memq digit acc) (get-pvalue-number n acc))
          (else (get-pvalue-number (sub1 n) (cons digit acc))))))

(define (choose-pvalue-op y)
  (case y
    ((0) '(ones 0))
    ((1) '(tens 1))
    ((2) '(hundreds 2))
    ((3) '(thousands 3))
    (else (error 'choose-pvalue))))

(define (choose-bba-op)
  (case (random 6)
    ((0 3 5) '(before))
    ((2) '(between))
    ((1 4) '(after))
    (else (error 'choose-bba-op))))

;;; rem= equal with remainder
(define (rem= input calc)
  (cond
    ((not (list? input)) #f)
    ((< (length input) 2) #f)
    (else
     (let ((res (string->number (string-trim (first input))))
           (rem (string->number (string-trim (second input)))))
       (and (= (first calc) res) (= (second calc) rem))))))

;;; approx=ndp - result equal to number of decimal places (ndp)
(define (approx= input calc)
  (approx=ndp input calc *exponent*))

(define (clock= hour minute h m)
  (and (= hour h) (= minute m)))

(define (mq-string->number string op)
  (let ((num (string->number string)))
    (if num
        num
        (if (list? ((run op) 2 2))
            (let ((numr (string-split string "r")))
              (if (= (length numr) 2)
                  numr
                  #f))
            #f))))

(define (do-math+ string xn-s op yn-s out) 
  (let* ((num (mq-string->number string op))
         (x (if (number? xn-s) xn-s (string->number xn-s)))
         (y (if (number? yn-s) yn-s (string->number yn-s)))
         (result (and num (equal= num ((run op) x y)))))
    (cond
      (result
       (if (string? result)
           (send text-lines insert (msg6 x op y result))
           (send text-lines insert (msg6 x op y num)))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-arithmetic))) ; continue with next exercise
      (num
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y num))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math-fract> stringl x op y out) ; level 3
  (let* ((op (check-<=> (second stringl)))
         (xr (string->number (first stringl)))
         (yr (string->number (third stringl)))
         (xq (string->number x)) (yq (string->number y))
         (result
          (cond ((not (and op xr yr)) (cons #f 'dummy))
                ((not (= xr xq)) (cons #f 'left))
                ((not (= yr yq)) (cons #f 'right))
                (else (cons ((run op) xq yq) 'comparison)))))
    (cond
      ((car result)
       (send text-lines insert (msg6 x op y (display-<=> (second stringl))))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      ((eq? (cdr result) 'left)
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x '(left) "" (first stringl)))
       (send text-lines change-style style-delta-black))
      ((eq? (cdr result) 'right)
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 y '(right) "" (third stringl)))
       (send text-lines change-style style-delta-black))
      ((eq? (cdr result) 'comparison)
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y (display-<=> (second stringl))))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 stringl))
       (send text-lines change-style style-delta-black)))
    (send out set-editor text-lines)))

(define (do-math> string x op y out) ; level 1 & 2
  (let* ((op (check-<=> string))
         (result (and op
                      (if (string? x)
                          ((run op) (string->number x) (string->number y))
                          ((run op) x y)))))
    (cond
      (result
       (send text-lines insert (msg6 x op y (display-<=> string)))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      (op
       (bell1) ; just testing (play-sound "switch.oga" #f)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y (display-<=> string)))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-odd/even string x op y out)
  (let* ((op (check-odd/even string))
         (result (and op ((run op) x))))
    (cond
      (result
       (send text-lines insert (msg-odd/even x op y ""))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-odd-even))) ; continue with next exercise
      (op
       (bell1) ; just testing (play-sound "switch.oga" #f)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-odd/even x op y "not"))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-sequence string x op y out)
  (let* ((missing-number-input (string->number string))
         (sequence-number (fourth x))
         (result (and missing-number-input
                      (equal= missing-number-input sequence-number))))
    (cond
      (result
       (send text-lines insert (msg-sequence x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-sequence))) ; continue with next exercise
      (missing-number-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-sequence x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math-a2r string x op y out)
  (let* ((roman-number-input string)
         (roman-number y)
         (result (and roman-number-input
                      (equal= roman-number-input roman-number))))
    (cond
      (result
       (send text-lines insert (msg-roman x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-a2r))) ; continue with next exercise
      ((not (string=? roman-number-input ""))
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-roman-error x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math-r2a string x op y out)
  (let* ((arabic-number-input (string->number string))
         (arabic-number y)
         (result (and arabic-number-input
                      (equal= arabic-number-input arabic-number))))
    (cond
      (result
       (send text-lines insert (msg-roman x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      (arabic-number-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-roman-error x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-text string x op y out)
  (let* ((input (string->number string))
         (solution y)
         (result (and input (equal= input solution))))
    (cond
      (result
       (send text-lines insert (msg-text x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      (input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-text-error x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-money coins x op y out)
  (let* ((cash-input (calc-return coins))
         (result (and (number? cash-input) (equal= cash-input y))))
    (cond
      (result
       (clear-money-inputs) ; now clear input fields
       (send text-lines insert (msg-money x op y coins))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-money))) ; continue with next exercise
      ((or (and (number? cash-input) (not (zero? cash-input)))
           (string? cash-input))
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-money-error x op y cash-input))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (clear-money-inputs) ; whatever is in there is garbage
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 "Empty Input"))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-ABC indices x op y out)
  (let* ((sum-idxs (apply + indices))
         (idx-word (map cons indices x))
         (sorted-words (map cdr (sort idx-word #:key car <)))
         (result (equal= sorted-words y)))
    (cond
      ((and result (equal? '(1 2 3 4 5) (sort indices <)))
       (clear-ABC-inputs) ; now clear input fields
       (send text-lines insert (msg-ABC x op sorted-words))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-ABC))) ; continue with next exercise
      ((not (zero? sum-idxs))
       (bell1)
       (cond
         ((not (equal? '(1 2 3 4 5) (sort indices <)))
          (send text-lines change-style style-delta-green)
          (send text-lines insert (msg-ABC-error-index x op indices)))
         (else
          (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
          (send text-lines change-style style-delta-red)
          (send text-lines insert (msg-ABC-error x op sorted-words))))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (clear-ABC-inputs) ; whatever is in there is garbage
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 "Empty Input"))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-skip n-list x op y out)
  (let ((result (equal= n-list)))
    (cond
      ((and result (boolean? result))
       (send skip-input set-value "") ; now clear input field
       (send text-lines insert (msg-skip n-list))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      ((number? result)
       (send text-lines change-style style-delta-green)
       (send text-lines insert
             (string-append
              "Only " (number->string result)
              " numbers entered! Error not counted.\n"))
       (send text-lines change-style style-delta-black))
      ((not (list? result)) ; number or string
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (cond
         ((not result)
          (let ((position (problem-z *problem*)))
            (send text-lines insert
                  (format "Incorrect number(s) starting with ~a in ~a~n"
                          (car (findf pair? position)) position))))
         ((string? result)
          (send text-lines insert
                (string-append "Started with " result " insted of "
                               (number->string x) "\n")))
         (else
          error 'unknown-skip-error))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 n-list))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (check-skip-counting nums)
  (let* ((len (length nums)))
    (cond
      ((or (null? nums) (not (andmap number? nums))) (list 'input-error))
      ((not (= (problem-x *problem*) (car nums))) (number->string (car nums)))
      ((< len 10) len)
      ((test-increment nums (problem-op *problem*)))
      (else
       (set-problem-z! *problem*
                       (find-skip-error nums (problem-op *problem*)))
       #f))))

(define (find-skip-error nums delta)
  (if (not (= (- (cadr nums) (car nums)) delta))
      (cons (car nums) (cons (list (cadr nums)) (cddr nums)))
      (cons (car nums) (find-skip-error (cdr nums) delta))))
            
(define (test-increment lst inc)
  (if (null? (cdr lst))
      #t
      (and (= (+ (car lst) inc) (cadr lst))
           (test-increment (cdr lst) inc))))

(define (calc-return input-0-list)
  (if (findf (lambda (x) (> x 4)) input-0-list)
      "ERROR - more than 4 of the same denomination"        
      (let ((cash-list
             (map * input-0-list '(100 50 20 10 5 1 1/4 1/10 1/20 1/100))))
        (exact->inexact (apply + cash-list)))))

(define (do-math-operators operators x op y out)
  (let* ((term-list (second y))
         (term-string (parse-ops-input term-list operators))
         (result (and term-string (equal= operators (first y)))))
    (cond
      (result
       (send text-lines insert (msg-ops x "equal" op))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup))) ; continue with next exercise
      (term-string
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-ops term-string "not =" op))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 operators))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-bba string x op y out)
  (let* ((missing-number-input (string->number string))
         (bba-number y)
         (result (and missing-number-input
                      (equal= missing-number-input bba-number))))
    (cond
      (result
       (send text-lines insert (msg-bba x "" op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-bba))) ; continue with next exercise
      (missing-number-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-bba x "not" op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-pvalue string x op y out)
  (let* ((reply-input (string->number string))
         (pos-number y)
         (result (and reply-input
                      (equal= reply-input pos-number))))
    (cond
      (result
       (send text-lines insert (msg-pvalue result string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-pvalue))) ; continue with next exercise
      (reply-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-pvalue result string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-round string x op y out)
  (let* ((reply-input (string->number string))
         (result
          (and reply-input (equal= reply-input y))))
    (cond
      (result
       (send text-lines insert (msg-round result string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-round))) ; continue with next exercise
      (reply-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-round result string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-ord string x op y out)
  (let* ((len (string-length string))
         (postfix (substring string (max 0 (- len 2)) len))
         (prefix (substring string 0 (max 0 (- len 2))))
         (reply-input (and (string=? x prefix)
                           (check-postfix postfix)))
         (result (and reply-input (equal= string y))))
    (cond
      (result
       (send text-lines insert (msg-ord result string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-ord))) ; continue with next exercise
      (reply-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-ord result string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-fraction string x op y out)
  (let* ((fraction-input (string->number string))
         (fraction-number ((run op) x y))
         (result (and fraction-input
                      (equal= fraction-input fraction-number))))
    (cond
      (result
       (send text-lines insert (msg6 x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-fraction))) ; continue with next exercise
      (fraction-input
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-clock string x op y out)
  (let* ((time-list (string->time-lst string))
         (result (and (cons? time-list)
                      (equal= (first time-list) (second time-list) x y))))
    (cond
      (result
       (send text-lines insert (msg6 x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-clock))) ; continue with next exercise
      ((cons? time-list)
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-clock-BA string x op y out)
  (let* ((time-list (string->time-lst string))
         (answer (problem-z *problem*))
         (result (and (cons? time-list)
                      (equal= (first time-list) (second time-list)
                              (first answer) (second answer)))))
    (cond
      (result
       (send text-lines insert (msg6 x op y string (third answer)))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-clock))) ; continue with next exercise
      ((cons? time-list)
       (bell1)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y string (third answer)))
       (send text-lines change-style style-delta-black))
      (else
       (bell1)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (string->time-lst str)
  (let ((lst (string-split str ":")))
    (cond ((not (= (length lst) 2)) str)
          ((or (not (string->number (first lst)))
               (not (string->number (second lst)))) str)
          (else
           (let ((h (modulo (string->number (first lst)) 12))
                 (m (string->number (second lst))))
             (list h m))))))       

;;; driving logic

(define (math-quiz-type input) ; dispatch on do-math function
  (do-math input (problem-x *problem*) (problem-op *problem*)
           (problem-y *problem*) text-output)
  (math-quiz input))

(define (math-quiz input)
  "Ask the user a series of math problems. Mistakes trigger additional problems.
If allotted time for solving problems is exceeded,
then additional problems are given."
  (when (> (state-question *state*) (state-problems *state*))
    (let ((run-time (running-time))
          (mistakes (state-mistakes *state*)))
      (send text-lines change-style style-delta-green)
      (send text-lines insert (msg2 mistakes))
      (send text-lines change-style style-delta-black)
      (cond
        ((> run-time *allowed-time*)   
         (send text-lines change-style style-delta-green)
         (send text-lines insert (msg4 run-time))
         (send text-lines change-style style-delta-black)
         (start-quiz (penalty-time run-time) mistakes))
        ((> mistakes 0) (start-quiz (penalty-mistakes mistakes) 0))
        (else
         (send text-lines change-style style-delta-green)
         (send text-lines insert (msg5))
         (send text-lines change-style style-delta-black)
         (send text-lines insert (msg-separator)) ; keeping records af all exercises
         (send prompt-msg set-label prompt-msg-label-again)
         (send operation-msg set-label op-start-label)
         (send number-input set-value input-label)
         (send number-input enable #f)
         (disable/enable-start-buttons #t)
         (disable/enable-set/font-menu #t)
         ; program was not restarted! set-all-fonts will not function properly
         (when all-fonts-delta (send set-all-fonts enable #f))
         (send wiwi-button enable #f)
         (send *exec-button* enable #f)
         (disable/enable-popup-window-menu #f)
         (disable/enable-dialog-show #f)
         (send stop-button enable #f)
         (clear-persistent-tables)
         (set! *time-flag* #f))))))

(define (reset)
  "Stopping the set of exercises and resetting"
  (send text-lines change-style style-delta-blue)
  (send text-lines insert (msg-stop))
  (send text-lines change-style style-delta-black)
  (send text-lines insert (msg-separator)) ; keeping records af all exercises
  (send prompt-msg set-label prompt-msg-label-again)
  (send operation-msg set-label op-start-label)
  (set! *time-flag* #f) ; setting text problems for normal operation
  (blank-input-fields) ; erase any residual inputs
  (clear-money-inputs)
  (clear-ABC-inputs)
  (clear-persistent-tables)
  (send number-input enable #f)
  (disable/enable-start-buttons #t)
  (disable/enable-set/font-menu #t)
  ; program was not restarted! set-all-fonts will not function properly
  (when all-fonts-delta (send set-all-fonts enable #f))
  (send wiwi-button enable #f)
  (send wiwi-button set-label "Pause") ; just making sure
  (send *exec-button* enable #f)
  (disable/enable-popup-window-menu #f)
  (disable/enable-dialog-show #f)
  (send stop-button enable #f))

(define (blank-input-fields)
  (let ((input-fields
         (append
          ops-input-list
          (list comparison-input sequence-input bba-input fraction-input-left
                fraction-input-right fraction-input clock-input a2r-input r2a-input
                round-input ord-input text-input number-input))))
    (for-each (lambda (input) (send input set-value input-label)) input-fields)))

(define (check-<=> string)
  (case string
    (("=") comp=)
    ((">") comp>)
    (("<") comp<)
    (else #f)))

(define (check-odd/even string)
  (case string
    (("odd") is-odd)
    (("even") is-even)
    (else (error 'display-odd/even))))

(define (display-<=> string)
  (case string
    (("=") "equal")
    ((">") "greater")
    (("<") "smaller")
    (else (error 'display-<=>))))

(define (display-sequence string n)
  (if (equal= (string->number string) n)
      (format "missing number is ~a~a" string
              (if *cheat-flag* "; Cheated!" ""))
      (format "missing number is not ~a" string)))
  
;;; original lisp code

(define (penalty-mistakes mistakes) 
  "Adjust additional exercises based on mistakes"
  (if (> mistakes 4)
      *max-penalty-exercises*
      mistakes))
    
(define (penalty-time time)
  "Adjust number of additional exercises proportionaly based on
minutes exceeded, and alowed time for each exercise,
but limited by *max-penalty-exercises*"
  (min *max-penalty-exercises*
       (max 1 ; return at least 1 penalty exercise (0 crashes the program)
            (exact-round (/ (- time *allowed-time*)
                            *time-factor* *speed-factor*)))))

(define (running-time)
  "Get time used so far - exclude wiwi time"
  (round (/ (- (current-seconds) *time-start* *wiwi-time*) 60)))

(define (get-left-number left op)
  (if (eq? (show op) '+)
      left
      (get-left-number- (random 0 *left-digit*)))) ; this can yield 0 !

;; weighting 0 in middle digit 1/3.777...???
(define (get-left-number- digit1)
  (let ((digit2 (list-ref `(,(random 0 10) 0 ,(random 0 10)) (random 0 3)))
        (digit3 (random 0 10)))
    (let ((x (+ (* digit1 100) (* digit2 10) digit3)))
      (if (< x 2) ; making sure get-right-number- does not spin forever
          (get-left-number- (random 0 *left-digit*))
          x))))

(define (get-right-number+ left right) right)

(define (get-right-number- left right)
  (cond
    ;((= left 1) 0) ; infinite loop, get clean set of numbers
    ((zero? (ones right))
     (get-right-number- left (random  1 left))) ; avoiding zero
    (else right)))

(define (ones n)
  (remainder n 10))

;;; I/O part
;;; =============================================================

(define (msg1 n)
  (format "You have ~a minute(s) to complete ~a question(s).~n"
          *allowed-time* n))

(define (msg-separator)
  (format "________________________________________________~n~n"))

(define (msg2 mistakes)
  (format "~nAll questions solved, with ~a mistake(s) along the way.~n"
          mistakes))

(define (msg3 i n time)
  (format "Question number ~a of ~a --- Time used: ~a of ~a minutes."
          i n time *allowed-time*))

(define (msg4 time)
  (string-append
   (format "~nYou exceeded allowed time for exercises!~n")
   (format "Allowed time was ~a minutes, and you did it in ~a minutes.~n"
           *allowed-time* time)
   (format "You will get ~a additional exercise(s).~n" (penalty-time time))))

(define (msg5)
  (format "Bravo, exercise COMPLETED successfully.~n"))

(define (msg6 x op y result [z ""]) ; z is for BA-clock
  (format "~a ~a ~a  ~a = ~a~n" x (show op) y z result))

(define (msg-odd/even x op y result)
  (format "~a is ~a ~a~n" x result (show op)))

(define (msg-roman x op y result)
  (format "~a ~a ~a~n" x (show op) result))

(define (msg-roman-error x op y result)
  (format "~a ~a is not ~a~n" x (show op) result))

(define (msg-text x op y result)
  (if *time-flag*
      (calc-stub x "=" (minstr->hmstr result)) 
      (calc-stub x "=" (truncate-result (string->number result) *exponent*))))

(define (msg-text-error x op y result)
  (let ((line2 (format "~n~a \\=" (if (string? op) op (list2string op)))))
    (if *time-flag*
        (calc-stub x line2 (minstr->hmstr result))
        (calc-stub x line2 (truncate-result (string->number result) *exponent*)))))

(define (calc-stub text content result)
  "Printing first line of the problem, making sure that it is not longer than
  *report-line-length*. If it is, removing words from the end of line."
  (define (trim-line line)
    (if (> (string-length line) *report-line-length*)
        (trim-line
         (apply string-append
                (map (lambda (w) (string-append w " "))
                     (reverse (cdr (reverse (string-split line " ")))))))
        line))      
  (let* ((line1 (car (string-split text "\n")))
         (len (string-length line1)))
    (if (> len *report-line-length*)
        (format "~a ~a ~a~n"
                (string-append (trim-line line1)  "...")
                content result)
        (format "~a ~a ~a~n"
                (string-append line1 " ...") content result))))

(define (msg-money x op y result)
  (format "Correct change returned: ~a ~a~n" (decimal-points y)
          (if *peso* "Peso" "$")))

(define (msg-money-error x op y result)
  (if (number? result)
      (format "Change return is not: ~a ~a~n"
              (decimal-points result) (if *peso* "Peso" "$"))
      (format "~a~n" result)))

(define (msg-ABC x op sorted)
  (format "~a ~a = ~a~n" x (show op) sorted))

(define (msg-ABC-error x op sorted)
  (format "~a ~a \\= ~a~n" x (show op) sorted))

(define (msg-ABC-error-index x op idx)
  (format "~a ~a index error ~a~n" x (show op) idx))

(define (msg-skip n-list)
  (let* ((incr (problem-op *problem*))
         (sign (if (negative? incr) "" "+")))
    (format "Correct (~a~a) skip-count: ~a~n" sign incr n-list)))

(define (msg-sequence x op y result)
  (format "~a  ~a  ~a  ~a  ~a  -> ~a~n"
          (first x) (second x) (third x) result (first y)
          (display-sequence result (fourth x))))

(define (msg-bba x yes/no op y result)
  (case (show op)
    ((before) (format "~a is ~a before ~a~n" result yes/no (third x)))
    ((between) (format "~a is ~a between ~a and ~a~n"
                       result yes/no (first x) (third x)))
    ((after) (format "~a is ~a after ~a~n" result yes/no (first x)))
    (else (error 'msg-bba))))

(define (msg-ops x yes/no input)
  (format "~a is ~a to: ~a~n" x yes/no input))

(define (msg-pvalue result input)
  (let ((x (problem-x *problem*))
        (pos (show (problem-op *problem*))))
    (if result
        (format "for number ~a~a~a~a ~a is ~a~n"
                (first x) (second x) (third x) (fourth x)
                pos (pad-pos (list-ref x (- 3 (cadr (problem-op *problem*))))
                             (problem-y *problem*)))
        (format "for number ~a~a~a~a ~a is not ~a~n"
                (first x) (second x) (third x) (fourth x)
                (show (choose-pvalue-op (string->number input)))
                (pad-pos (list-ref x (- 3 (cadr (problem-op *problem*))))
                         (problem-y *problem*))))))

(define (msg-round result input)
  (let* ((x (problem-x *problem*)))
    (if result
        (format "number ~a~a~a rounded on ~a is ~a~n"
                (first x) (second x) (third x) (second x) input)
        (format "number ~a~a~a rounded on ~a is not ~a~n"
                (first x) (second x) (third x) (second x) input))))

(define (msg-ord result input)
  (let* ((x (problem-x *problem*)))
    (if result
        (format "number ~a to ordinal is ~a~n" x input)
        (format "number ~a to ordinal is not ~a~n" x input))))

(define (msg7 x op y result [z ""]) ; z is for BA-clock
  (format "~a ~a ~a  ~a \\= ~a~n" x (show op) y z result))

(define (msg8 x op y)
  (format "~a ~a ~a" x op y))

(define (msg9 err-input)
  (format "invalid input ( ~a ) - error will not be counted!~n" err-input))

(define (msg-stop)
  (format "~nExecution of exercises was stopped!~n"))

(define (pad-pos n pos)
  (string-append n (make-string pos #\0)))

;;; ==================================================================
;;; Fractions problems
;;; ==================================================================

(define (fraction-callback canvas dc)
  (fraction-canvas-callback
   canvas dc *fraction-level* *canvas-flag*
   (problem-x *problem*) (problem-y *problem*)))

(define fraction-dialog (new frame%
                             [label "Fractions questions"]
                             [parent main-window]
                             [width 560] ; 560
                             [height 300]
                             [border 10]
                             [alignment '(left center)]))

(define fraction-canvas (new canvas%
                             [parent fraction-dialog]
                             [label "red=numerator/all=denominator"]
                             [min-width 380] ;300
                             [min-height 250]
                             [vert-margin 10]
                             [horiz-margin 10]
                             [style '(border no-focus)]
                             [paint-callback fraction-callback]))

(define fraction-pane (new horizontal-pane%
                           [parent fraction-dialog]
                           [min-width 380] ; 300
                           [vert-margin 10]
                           [horiz-margin 16] ; 76
                           [alignment '(right center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define fraction-input-left (new text-field%
                                 [parent fraction-pane]
                                 [font message-bold-font]
                                 [label "left"]
                                 [init-value input-label]
                                 [enabled #t]
                                 [min-width 100]
                                 [min-height 30]
                                 [vert-margin 10]
                                 [horiz-margin 20]
                                 [stretchable-width #f]
                                 [stretchable-height #f]))

(define fraction-input (new text-field%
                            [parent fraction-pane]
                            [font message-bold-font]
                            [label ">=<"]
                            [init-value input-label]
                            [enabled #t]
                            [min-width 80]
                            [min-height 30]
                            [vert-margin 10]
                            [horiz-margin 10]
                            [stretchable-width #f]
                            [stretchable-height #f]))

(define fraction-input-right (new text-field%
                                  [parent fraction-pane]
                                  [font message-bold-font]
                                  [label "right"]
                                  [init-value input-label]
                                  [enabled #t]
                                  [min-width 120]
                                  [min-height 30]
                                  [vert-margin 10]
                                  [horiz-margin 20]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))

(define fraction-button
  (new button%
       [parent fraction-pane]
       [label "Check"]
       [font button-font]
       [min-height start-button-height]
       [enabled #f]
       [vert-margin 10]
       [horiz-margin 10] ;30
       [style '(border)]
       [callback
        (lambda (button event)
          (case *fraction-level*
            ((1) (let ((input (send fraction-input-left get-value))
                       (a-text (send fraction-input-left get-editor)))
                   (send a-text erase) ; why only here?
                   (send fraction-input-left set-value input-label)
                   (math-quiz-type (strip-spaces input))))
            ((2) (let ((input (send fraction-input get-value))
                       (a-text (send fraction-input get-editor)))
                   (send a-text erase) ; why only here?
                   (send fraction-input set-value input-label)
                   (math-quiz-type (strip-spaces input))))
            ((3 4)
             (let* ((inputs (list
                             fraction-input-left
                             fraction-input
                             fraction-input-right))
                    (input
                     (map (lambda (x) (send x get-value)) inputs))
                    (list-text
                     (map (lambda (x) (send x get-editor)) inputs)))
               (for-each (lambda (x) (send x erase)) list-text)
               (for-each (lambda (x) (send x set-value input-label))
                         inputs)
               (math-quiz-type
                (map (lambda (x) (strip-spaces x)) input))))
            (else (error 'fraction-level))))]))
                                                                           
;;; =================================================================
;;; Clock problems
;;; =================================================================

(define (clock-callback canvas dc)
  (clock-canvas-callback
   canvas dc *used-numbers* (problem-x *problem*) (problem-y *problem*)))
  
(define clock-dialog (new frame%
                          [label "Clock questions"]
                          [parent main-window]
                          [width 440]
                          [height 360]
                          [border 10]
                          [alignment '(left center)]))

(define clock-canvas (new canvas%
                          [parent clock-dialog]
                          [label "hr:mn"]
                          [min-width 420]
                          [min-height 320]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [style '(border no-focus)]
                          [paint-callback clock-callback]))

(define clock-pane (new horizontal-pane%
                        [parent clock-dialog]
                        [min-width 420]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define clock-prompt (new message%
                          [parent clock-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 5]
                          [stretchable-width #t]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define clock-input (new text-field%
                         [parent clock-pane]
                         [font message-bold-font]
                         [label ""]
                         [init-value input-label]
                         [enabled #t]
                         [min-width 70]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 30]
                         [stretchable-width #f]
                         [stretchable-height #f]))

(define clock-button (new button%
                          [parent clock-pane]
                          [label "Check"]
                          [font button-font]
                          [min-height start-button-height]
                          [enabled #f]
                          [vert-margin 10]
                          [horiz-margin 30]
                          [style '(border)]
                          [callback
                           (lambda (button event)
                             (let ((input (send clock-input get-value))
                                   (a-text (send clock-input get-editor)))
                               (send a-text erase) ; why only here?
                               (send clock-input set-value input-label)
                               (math-quiz-type (strip-spaces input))))]))

;;; =================================================================
;;; Instructions - text only
;;; =================================================================
       
(send doc-instructions change-style style-delta-font-doc1-family)
(send doc-instructions change-style style-delta-font-doc1-weight)
(send doc-instructions insert instructions1)

(send doc-instructions change-style style-delta-font-doc2-family)
(send doc-instructions change-style style-delta-font-doc2-weight)
(send doc-instructions insert instructions2)

;;; About

(send about-text change-style style-delta-font-about-family)
(send about-text insert about1)

(void 
 (send doc-instructions scroll-to-position 0)
 (send about-text scroll-to-position 0))

;;; ==================================================================
;;; Tests for the main logic
;;; ==================================================================

(provide enable-bell)
(define (disable-bell) (set! bell1 void))
(define  (enable-bell) (set! bell1 bell))

(define (set+-level! v) (set! *level+-* v))
(define (set*level! v) (set! *level** v))
(define (set-quot-level! v) (set! *level/quot* v))
(define (set-comparison-level! v) (set! *comparison-level* v))
(define (set-sequence! v) (set! *sequence-difficulty* v))
(define (set-fract-level! v) (set! *fraction-level* v))
(define (set-clock! v) (set! *clock-level* v))
(define (set-max-rn! v) (set! *max-roman-number* v))
(define (set-max-skip! v) (set! *max-skip-increment* v))
(define (set-findX! v) (set! *findX-level* v))
(define (set-time-level! v) (set! *time-level* v))
(define (set-gapesa-level! v) (set! *gapesa-level* v))
(define (set-Carea-level! v) (set! *Carea-level* v))

(define (ord-err str)
  (let* ((cut (- (string-length str) 2))
         (digits (substring str 0 cut))
         (ords (substring str cut))
         (wrong-ords (second (member ords '("st" "nd" "rd" "th" "st")))))
    (string-append digits wrong-ords)))

(define (return-cash money)
  (reverse
   (car  
    (foldl (λ (d r)
             (let ((dl (first r))
                   (rest (second r)))
               (let-values ([(n newr) (quotient/remainder rest d)])
                 (list (cons n dl) newr))))
           (list null (inexact->exact (round (* 100 money))))
           '(10000 5000 2000 1000 500 100 25 10 5 1)))))
    
(define (index-words unsorted sorted)
  (let ((indexed (map cons '(1 2 3 4 5) sorted)))
    (map (λ (w)
           (car (findf
                 (λ (is) (string=? w (cdr is))) indexed))) unsorted)))

(module+ test
  (require rackunit)

  (check-not-exn (λ () (disable-bell))) ; pacifying bell

  (test-case
   "start (+ -) tests"
   (check-not-exn (λ () (set+-level! 0) (start+-)))
   (check-not-exn (λ () (set+-level! 9) (start+-)))
   (check-exn exn:fail? (λ () (set+-level! 10) (start+-)))
   (check-not-exn (λ () (set+-level! 8) (start+-)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (number->string
                        ((run op) (string->number x) (string->number y)))))))
   (check-eq? (+ 1 (state-mistakes *state*))
              (begin (start+-)
                     (let ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (number->string
                         (+ 1/10
                            ((run op) (string->number x) (string->number y))))))
                     (state-mistakes *state*)))
   (check-not-exn (λ () (set+-level! 1) (reset))))

  (test-case
   "start (*) tests"
   (check-not-exn (λ () (set*level! 1) (start*)))
   (check-exn exn:fail? (λ () (set*level! 4) (start*)))
   (check-not-exn (λ () (set*level! 3) (start*)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (number->string
                        ((run op) (string->number x) (string->number y)))))))
   (check-eq? (+ 1 (state-mistakes *state*))
              (begin (start*)
                     (let ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (number->string
                         (+ 1/10
                            ((run op) (string->number x) (string->number y))))))
                     (state-mistakes *state*)))
   (check-eq? (begin (set*level! 1) (start*) (+ 0 (state-mistakes *state*)))
              (let ((x (problem-x *problem*))
                    (y (problem-y *problem*))
                    (op (problem-op *problem*)))
                (math-quiz-type
                 (number->string
                  ((run op) x y)))
                (state-mistakes *state*)))
   (check-not-exn (λ () (set*level! 2) (reset))))

  (test-case
   "start (integer/fraction /) tests"
   (check-not-exn (λ () (set-quot-level! 1) (start/quot)))
   (check-exn exn:fail? (λ () (set-quot-level! 8) (start/quot)))
   (check-not-exn (λ () (set-quot-level! 4) (start/quot)
                    (let* ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*))
                           (res ((run op) x y)))
                      (math-quiz-type
                       (string-append (number->string (car res)) "r"
                                      (number->string (cadr res)))))))
   (check-eq? (+ 1 (state-mistakes *state*))
              (begin (start/quot)
                     (let* ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*))
                            (res ((run op) x y)))
                       (math-quiz-type
                        (string-append (number->string (car res)) "r"
                                       (number->string (+ 7 (cadr res))))))
                     (state-mistakes *state*)))
   (check-eq? (begin (set-quot-level! 7) (start/quot) (+ 0 (state-mistakes *state*)))
              (let ((x (problem-x *problem*))
                    (y (problem-y *problem*))
                    (op (problem-op *problem*)))
                (math-quiz-type
                 (number->string
                  ((run op) (string->number x) (string->number y))))
                (state-mistakes *state*)))
   (check-not-exn (λ () (set-quot-level! 1) (reset))))
   
  (test-case
   "start (/) tests" ; no levels here
   (check-not-exn (λ () (start/)))
   (check-not-exn (λ () (start/)
                    (let* ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*))
                           (res ((run op) x y)))
                      (math-quiz-type (number->string res)))))
   (check-eq? (begin (start/) ; correct answer
                     (let* ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*))
                            (res ((run op) x y)))
                       (math-quiz-type
                        (string-append (number->string res))))
                     (state-mistakes *state*)) 0)
   (check-eq? (begin (start/) ; wrong answer
                     (let* ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*))
                            (res ((run op) x y)))
                       (math-quiz-type
                        (string-append (number->string (+ res 0.1)))))
                     (state-mistakes *state*)) 1)
   (check-not-exn (λ () (start/) (set-quot-level! 1) (reset))))

  (test-case
   "start (*/) table tests"
   (check-exn exn:fail? (λ () (setmt! *max*table* 4) (start10*)))
   (check-exn exn:fail? (λ () (setmt! *max*table* 13) (start100/)))
   (check-not-exn (λ () (setmt! *max*table* 10) (start10*)))
   (check-not-exn (λ () (setmt! *max*table* 12) (start100/)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (number->string
                        ((run op) x y))))))
   (check-eq? (begin (start10*) ; wrong answer
                     (let ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (number->string
                         (+ 10 ((run op) x y)))))
                     (state-mistakes *state*)) 1)
   (check-eq? (begin (start100/) ; correct answer
                     (let ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (number->string ((run op) x y))))
                     (state-mistakes *state*)) 0)
   
   (check-not-exn (λ () (setmt! *max*table* 10) (reset))))

  (test-case
   "start comparison (integer/fraction) tests"
   (check-exn exn:fail? (λ () (set-comparison-level! 4) (start<=>)))
   (check-not-exn (λ () (set-comparison-level! 2) (start<=>)))
   (check-not-exn (λ () (set-comparison-level! 1) (start<=>)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (cond ((> x y) ">")
                             ((< x y) "<")
                             (else "="))))))
   (check-eq? (begin (start<=>) ; wrong answer (integers)
                     (let ((x (problem-x *problem*))
                           (y (problem-y *problem*))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (cond ((> x y) "<")
                              ((< x y) "=")
                              (else ">"))))
                     (state-mistakes *state*)) 1)
   (check-eq? (begin (set-comparison-level! 2) (start<=>) ; correct answer
                     (let ((x (string->number (problem-x *problem*)))
                           (y (string->number (problem-y *problem*)))
                           (op (problem-op *problem*)))
                       (math-quiz-type
                        (cond ((> x y) ">")
                              ((< x y) "<")
                              (else "="))))
                     (state-mistakes *state*)) 0) 
   (check-not-exn (λ () (set-comparison-level! 1) (reset))))

  (test-case
   "start Odd/Even tests"
   (check-not-exn (λ () (start-odd/even)))
   (check-not-exn (λ () (reset))))
   
  (test-case
   "start Position Value tests"
   (check-not-exn (λ () (start-pvalue)))
   (check-not-exn (λ () (reset))))

  (test-case
   "start sequence tests"
   (check-exn exn:fail? (λ () (set-sequence! 5) (start-sequence)))
   (check-not-exn (λ () (set-sequence! 2) (start-sequence)))
   (check-not-exn (λ () (set-sequence! 1) (start-sequence)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string (last x))))))
   (check-not-eqv? (begin (set-sequence! 3) (start-sequence) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ (last x) 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-sequence! 3) (start-sequence) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string (last x))))
                      (state-mistakes *state*)) 0)  
   (check-not-eqv? (begin (set-sequence! 4) (start-sequence) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ (last x) 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-sequence! 4) (start-sequence) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string (last x))))
                      (state-mistakes *state*)) 0)
      (check-not-eqv? (begin (set-sequence! 2) (start-sequence) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ (last x) 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-sequence! 2) (start-sequence) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string (last x))))
                      (state-mistakes *state*)) 0)
      (check-not-eqv? (begin (set-sequence! 1) (start-sequence) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ (last x) 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-sequence! 1) (start-sequence) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string (last x))))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-sequence! 1) (reset))))

  (test-case
   "start Before Between After tests"
   (check-not-exn (λ () (start-bba)))
   (check-not-exn (λ () (start-bba)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))))
   (check-not-eqv? (begin (start-bba) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ y 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-bba) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))

  (test-case
   "start Round tests"
   (check-not-exn (λ () (start-round)))
   (check-not-exn (λ () (start-round)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))))
   (check-not-eqv? (begin (start-round) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (+ y 1))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-round) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))

  (test-case
   "start Ordinal numbers tests"
   (check-not-exn (λ () (start-ord)))
   (check-not-exn (λ () (start-ord)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type y))))
   (check-not-eqv? (begin (start-ord) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (ord-err y)))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-ord) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type y))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))

  (test-case
   "start Fractions (graphical) tests"
   (check-exn exn:fail? (λ () (set-fract-level! 5) (start-fraction)))
   (check-not-exn (λ () (set-fract-level! 1) (start-fraction)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (string-append (number->string x) "/"
                                      (number->string y))))))
   (check-not-exn (λ () (reset)))
   (check-not-exn (λ () (set-fract-level! 2) (start-fraction)
                    (let ((x (string->number (problem-x *problem*)))
                          (y (string->number (problem-y *problem*)))
                          (op (problem-op *problem*)))
                      (math-quiz-type
                       (cond ((> x y) ">")
                             ((< x y) "<")
                             (else "="))))))
   (check-not-exn (λ () (reset)))             
   (check-not-eqv? (begin (set-fract-level! 3) (start-fraction) ; wrong answer
                          (let ((x (string->number (problem-x *problem*)))
                                (y (string->number (problem-y *problem*)))
                                (op (problem-op *problem*)))
                            (math-quiz-type (list
                                             (number->string x)
                                             (cond ((> x y) "=")
                                                   ((< x y) ">")
                                                   (else "<"))
                                             (number->string y))))
                          (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset)))
   (check-eqv? (begin (set-fract-level! 4) (start-fraction) ; correct answer
                      (let ((x (string->number (problem-x *problem*)))
                            (y (string->number (problem-y *problem*)))
                            (op (problem-op *problem*)))
                        (math-quiz-type (list
                                         (number->string x)
                                         (cond ((> x y) ">")
                                               ((< x y) "<")
                                               (else "="))
                                         (number->string y))))
                      (state-mistakes *state*)) 0)   
   (check-not-exn (λ () (reset))))
  
  (test-case
   "start Clock tests"
   (check-exn exn:fail? (λ () (set-clock! 6) (start-clock)))
   (check-not-exn (λ () (set-clock! 1) (start-clock)
                    (let ((x (number->string (problem-x *problem*)))
                          (y (number->string (problem-y *problem*)))
                          (op (problem-op *problem*)))
                      (math-quiz-type (string-append x ":" y)))))
   (check-not-eqv? (begin (set-clock! 3) (start-clock) ; wrong answer
                          (let ((zh (number->string (first (problem-z *problem*))))
                                (zm (number->string (second (problem-z *problem*)))))
                            (math-quiz-type (string-append zh ":" "61")))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-clock! 5) (start-clock) ; correct answer
                      (let ((zh (number->string (first (problem-z *problem*))))
                            (zm (number->string (second (problem-z *problem*)))))
                        (math-quiz-type (string-append zh ":" zm)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-clock! 1) (reset))))

  (test-case
   "start Arabic to Roman numerals tests"
   (check-not-exn (λ () (set-max-rn! 3999) (start-a2r)))
   (check-not-exn (λ () (start-a2r)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*)))
                      (math-quiz-type y))))
   (check-not-eqv? (begin (start-a2r) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*)))
                            (math-quiz-type "VX"))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-a2r) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*)))
                        (math-quiz-type y))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-max-rn! 100) (reset))))
  
  (test-case
   "start Roman to Arabic numerals tests"
   (check-not-exn (λ () (set-max-rn! 3999) (start-r2a)))
   (check-not-exn (λ () (start-r2a)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*)))
                      (math-quiz-type (number->string y)))))
   (check-not-eqv? (begin (start-r2a) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*)))
                            (math-quiz-type "4000"))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-r2a) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*)))
                        (math-quiz-type (number->string y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-max-rn! 100) (reset))))

  (test-case
   "start Cash tests"
   (check-not-exn (λ () (start-money)))
   (check-not-exn (λ () (start-money-p)))
   (check-not-exn (λ () (start-money)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*)))
                      (math-quiz-type (return-cash y)))))
   (check-not-eqv? (begin (start-money-p) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*)))
                            (math-quiz-type '(4 4 4 4 4 4 4 0 4 4)))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-money) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*)))
                        (math-quiz-type (return-cash y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))
  
  (test-case
   "start Alphabetical sorting tests"
   (check-not-exn (λ () (start-ABC)))
   (check-not-exn (λ () (start-ABC)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*)))
                      (math-quiz-type (index-words x y)))))
   (check-not-eqv? (begin (start-ABC) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*)))
                            (math-quiz-type
                             (let* ((idxs (index-words x y))
                                    (a (first idxs))
                                    (b (second idxs)))
                               (cons b (cons a (cddr idxs))))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-ABC) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*)))
                        (math-quiz-type (index-words x y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))

  (test-case
   "start Skip (+ -) counting tests"
   (check-not-exn (λ () (set-max-skip! 10) (start-skip)))
   (check-not-exn (λ () (set-max-skip! 6) (start-skip-neg)))
   (check-not-exn (λ () (set-max-skip! 3) (start-skip)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (inc (problem-op *problem*)))
                      (math-quiz-type
                       (build-list 10 (λ (i) (+ x (* i inc))))))))
   (check-not-exn (λ () (set-max-skip! 5) (start-skip-neg)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (inc (problem-op *problem*)))
                      (math-quiz-type
                       (build-list 10 (λ (i) (+ x (* i inc))))))))
   (check-not-eqv? (begin (set-max-skip! 6) (start-skip-neg) ; wrong answer
                          (let ((x (add1 (problem-x *problem*)))
                                (y (problem-y *problem*))
                                (inc (problem-op *problem*)))
                            (math-quiz-type
                             (build-list 10 (λ (i) (+ x (* i inc))))))
                          (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-max-skip! 7) (start-skip) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (inc (problem-op *problem*)))
                            (math-quiz-type
                             (cons (- x inc)
                                   (build-list 9 (λ (i) (+ x (* i inc)))))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-max-skip! 4) (start-skip) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (inc (problem-op *problem*)))
                        (math-quiz-type
                         (build-list 11 (λ (i) (+ x (* i inc))))))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (reset))))
  
  (test-case
   "start missing Operand tests"
   (check-exn exn:fail? (λ () (set-findX! 4) (start-findX)))
   (check-not-exn (λ () (set-findX! 2) (start-findX)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))))
   (check-not-eqv? (begin (set-findX! 3) (start-findX) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-findX! 1) (start-findX) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-findX! 1) (reset))))

  (test-case
   "start Time tests"
   (check-exn exn:fail? (λ () (set-time-level! 4) (start-time)))
   (check-not-exn (λ () (set-time-level! 1) (start-time)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))))
   (check-not-eqv? (begin (set-time-level! 3) (start-time) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-time-level! 2) (start-time) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (state-mistakes *state*)) 0)
   (check-not-exn (λ () (set-time-level! 1) (reset))))

  (test-case
   "start GAPESA (word problems) tests"
   (check-exn exn:fail? (λ () (set-gapesa-level! 11) (start-text)))
   (check-not-exn (λ () (set-gapesa-level! 1) (start-text)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))
                    (reset)))
   (check-not-eqv? (begin (set-gapesa-level! 3) (start-text) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-gapesa-level! 4) (start-text) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-gapesa-level! 6) (start-text) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-gapesa-level! 7) (start-text) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-gapesa-level! 7) (start-text) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)  
   (check-eqv? (begin (set-gapesa-level! 8) (start-text) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-gapesa-level! 9) (start-text) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-gapesa-level! 9) (start-text) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-gapesa-level! 10) (start-text) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)   
   (check-not-exn (λ () (set-gapesa-level! 1) (reset))))

  (test-case
   "start Perimeter/Area tests"
   (check-exn exn:fail? (λ () (set-Carea-level! 9) (start-Carea)))
   (check-not-exn (λ () (set-Carea-level! 1) (start-Carea)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (number->string y)))
                    (reset)))
   (check-eqv? (begin (set-Carea-level! 2) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0) 
   (check-not-eqv? (begin (set-Carea-level! 2) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 3) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 3) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 4) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 4) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 5) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 5) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 6) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 6) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 7) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 7) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-not-eqv? (begin (set-Carea-level! 8) (start-Carea) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (number->string (add1 y))))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (set-Carea-level! 8) (start-Carea) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (number->string y)))
                      (reset)
                      (state-mistakes *state*)) 0)  
   (check-not-exn (λ () (set-Carea-level! 1) (reset))))

  (test-case
   "start operator tests"
   (check-exn exn:fail? (λ () (set-ops-level! 4) (start-operators)))
   (check-not-exn (λ () (set-ops-level! 1)))
   (check-not-exn (λ () (start-operators)
                    (let ((x (problem-x *problem*))
                          (y (problem-y *problem*))
                          (op (problem-op *problem*)))
                      (math-quiz-type (car y)))
                    (reset)))
   (check-not-exn (λ () (set-ops-level! 1)))
   (check-eqv? (begin (start-operators) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (car y)))
                      (reset)
                      (state-mistakes *state*)) 0) 
   (check-not-eqv? (begin (start-operators) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (wrong-answer-ops y)))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer1-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer2-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)  
   (check-not-exn (λ () (set-ops-level! 2)))   
   (check-eqv? (begin (start-operators) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (car y)))
                      (reset)
                      (state-mistakes *state*)) 0) 
   (check-not-eqv? (begin (start-operators) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (wrong-answer-ops y)))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer1-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer2-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)  
   (check-not-exn (λ () (set-ops-level! 3)))
   (check-eqv? (begin (start-operators) ; correct answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (car y)))
                      (reset)
                      (state-mistakes *state*)) 0) 
   (check-not-eqv? (begin (start-operators) ; wrong answer
                          (let ((x (problem-x *problem*))
                                (y (problem-y *problem*))
                                (op (problem-op *problem*)))
                            (math-quiz-type (wrong-answer-ops y)))
                          (reset)
                          (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer1-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)
   (check-eqv? (begin (start-operators) ; invalid answer
                      (let ((x (problem-x *problem*))
                            (y (problem-y *problem*))
                            (op (problem-op *problem*)))
                        (math-quiz-type (invalid-answer2-ops y)))
                      (reset)
                      (state-mistakes *state*)) 0)   
   (check-not-exn (λ () (set-ops-level! 1) (reset))))
      
  ;(fail-check "just testing")
  ) 
