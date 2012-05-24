#lang racket
;; Prog 2 Asg #20: State, Part II
; Andrew Amis
; Started: 5.16.12
; Ended: ?
; http://fellowhuman.com/gbk/2012/05/16/prog-2-asg-20-state-part-ii/

(require picturing-programs
         test-engine/racket-tests
         htdp/gui)

;; Data Def.: A TL-color is either 'green, 'yellow, or 'red. 

;; State Variable: 
;; current-color : TL-color
;; to keep track of the current color of the traffic light
(define current-color 'red)

;; Contract: next :  ->  void

;; Purpose: the function always produces (void)

;; Effect: to change current-color from 'green to 'yellow, 
;; 'yellow to 'red, and 'red to 'green

;; Header: omitted for this particular example

;; Examples: 
;; if current-color is 'green and we evaluate (next), then current-color is
;    'yellow
; if current-color is 'yellow and we evaluate (next), then current-color is
;    'red
; if current-color is 'red and we evaluate (next), then current-color is 'green

;; Template: data-directed on state-variable that is to be mutated
;; (define (f)
;;   (cond
;;     [(symbol=? 'green current-color) (set! current-color ...)]
;;     [(symbol=? 'yellow current-color) (set! current-color ...)]
;;     [(symbol=? 'red current-color) (set! current-color ...)]))

;; Definition:
(define (next)
  (set! current-color
        (cond
          [(symbol=? 'green current-color) 'yellow]
          [(symbol=? 'yellow current-color) 'red]
          [(symbol=? 'red current-color) 'green])))
  
;; Tests:
(check-expect
 (begin (set! current-color 'green) (next) (symbol=? current-color 'yellow))
 true)
(check-expect
 (begin (set! current-color 'yellow) (next) (symbol=? current-color 'red))
 true)
(check-expect
 (begin (set! current-color 'red) (next) (symbol=? current-color 'green))
 true)

(define (dim color factor)
  (let ([fxn {λ (x) (max 0 (- x factor))}])
    (make-color
     (fxn (color-red color))
     (fxn (color-green color))
     (fxn (color-blue color)))))

;; display-light : world -> image
; Draws the traffic light as an image
(define (display-light world)
  (above
   (circle 10 'solid (dim (make-color #xff 0 0)
                          (if (symbol=? current-color 'red) 0 80)))
   (circle 10 'solid (dim (make-color #xff #xff 0)
                          (if (symbol=? current-color 'yellow) 0 80)))
   (circle 10 'solid (dim (make-color 00 #xff 0)
                          (if (symbol=? current-color 'green) 0 80)))))

(check-expect (begin (set! current-color 'green)
                     (display-light false))
              (above
               (circle 10 'solid (make-color 175 0 0))
               (circle 10 'solid (make-color 175 175 0))
               (circle 10 'solid (make-color 0 255 0))))
(check-expect (begin (set! current-color 'green)
                     (next)
                     (display-light false))
              (above
               (circle 10 'solid (make-color 175 0 0))
               (circle 10 'solid (make-color 255 255 0))
               (circle 10 'solid (make-color 0 175 0))))
(check-expect (begin (set! current-color 'green)
                     (next)
                     (next)
                     (display-light false))
              (above
               (circle 10 'solid (make-color 255 0 0))
               (circle 10 'solid (make-color 175 175 0))
               (circle 10 'solid (make-color 0 175 0))))


(define (key-handler world event)
  (begin (next)
         world))

(define (start-light)
  (big-bang false
   (to-draw display-light)
   (on-key key-handler)))


#|
key-handler modifies state.
display-light merely reads state.
|#



#|
In the color guessing game...
master changes state
master-check reads state

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 37.1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants:

;; the legitimate colors 
(define COLORS
  (list 'black 'white 'red 'blue 'green 'gold 'pink 'orange 'purple 'navy))

(define *guess-counter* 0)

(define-syntax-rule (incrementer ident)
  {λ ()
    (set! ident (add1 ident))})

(define incr-guess (incrementer *guess-counter*))

;; the number of colors
(define COL# (length COLORS))

;; Data Definition:
;; A color is a symbol on COLORS. 


;; target1, target2 : color 
;; the two variables represent the two colors that the first player chose
(define target1 (first COLORS))
(define target2 (first COLORS))

;; random-pick : listof[X] -> X
; Picks randomly from a list.
(define (random-pick list)
  (list-ref list (random (length list))))

;; master :  ->  void
;; effect: set target1 and target2 to two randomly chosen items from COLORS
(define (master)
  (begin
    (set! target1 (random-pick COLORS))
    (set! target2 (random-pick COLORS))))

#;(define (xor a b) (and (or a b) (not (and a b))))

;; check-color : symbol -> symbol
; Checks whether a guess is correct.
(define (check-color target1 target2 guess1 guess2)
  (cond
    [(and (symbol=? guess1 target1)
          (symbol=? guess2 target2)) 'Perfect]
    [(or (symbol=? guess1 target1)
         (symbol=? guess2 target2)) 'OneColorAtCorrectPosition]
    [(or (symbol=? guess1 target2)
         (symbol=? guess2 target1)) 'OneColorOccurs]
    [else 'NothingCorrect]))

(check-expect (check-color 'red 'blue 'red 'blue) 'Perfect)
(check-expect (check-color 'red 'blue 'green 'blue) 'OneColorAtCorrectPosition)
(check-expect (check-color 'red 'blue 'red 'green) 'OneColorAtCorrectPosition)
(check-expect (check-color 'red 'blue 'green 'red) 'OneColorOccurs)
(check-expect (check-color 'red 'blue 'blue 'green) 'OneColorOccurs)
(check-expect (check-color 'red 'blue 'yellow 'green) 'NothingCorrect)

;; master-check : color color  ->  symbol
;; to determine how many colors at how many positions are guessed correctly
;; The function defers to check-color, the solution of exercise 5.1.5.
(define (master-check guess1 guess2)
  (let ([result (begin (incr-guess)
                       (check-color guess1 guess2 target1 target2))])
    (if (symbol=? result 'Perfect)
        (begin (master)
               (list result *guess-counter*))
        result)))

#|

Diagram of memory access:


master ---w---> target[1|2] ---r---> master-check
   ^                                           |
   |------------------- calls -----------------



|#

(define guess1 'black)
(define guess2 'black)


(define button-list1 (map {λ (color)
                            (make-button
                             (symbol->string color)
                             {λ (ev)
                               (begin (set! guess1 color)
                                      true)})} COLORS))
(define button-list2 (map {λ (color)
                            (make-button
                             (symbol->string color)
                             {λ (ev)
                               (begin (set! guess2 color)
                                      true)})} COLORS))

(define output (make-message "Output"))

(define submit (make-button "Submit" {λ (ev)
                                       (draw-message
                                        output
                                        (format "~s"
                                         (master-check guess1 guess2)))}))
(define reset (make-button "Reset" {λ (ev)
                                       (begin (master)
                                              true)}))

(define (run-game)
  (show-window
   (create-window
    (list
     button-list1
     button-list2
     (list submit reset output)))))
                                                               

(test)