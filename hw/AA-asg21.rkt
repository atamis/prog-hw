;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #21: Interval Data
; Andrew Amis
; 11.6.11
; http://fellowhuman.com/gbk/2011/11/03/prog-1-asg-21-interval-data/

(require picturing-programs)

(define cat1
  (overlay
   (text "Cat" 14 'black)
   (circle 20 'solid 'brown)))
(define cat2
  (overlay
   (text "Cat" 14 'black)
   (circle 20 'solid 'tan)))

(define H 200)
(define W 200)

(define MT (empty-scene W H))

;;;;;;;;;;;;;;;;;;;;;;; Exercise 36 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-cat : world -> image
; Draws cat1 on MT where y is half MT and x moves from right to left and repeats
(define (draw-cat w)
  (place-image cat1
               (remainder (* 3 w) W)
               (/ H 2)
               MT))
(check-expect (draw-cat 0) (place-image cat1 0 100 MT))
(check-expect (draw-cat 1) (place-image cat1 3 100 MT))
(check-expect (draw-cat 2) (place-image cat1 6 100 MT))

#;(big-bang 0
            (on-tick add1)
            (to-draw draw-cat))

;;;;;;;;;;;;;;;;;;;;;;; Exercise 37 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-cat2 : world -> image
; Draws cat1 on MT where y is half MT and x moves from right to left and repeats
; When w is odd, a lighter cat is used.
(define (draw-cat2 w)
  (place-image (if (odd? (* 3 w)) cat2 cat1)
               (remainder (* 3 w) W)
               (/ H 2)
               MT))
(check-expect (draw-cat2 0) (place-image cat1 0 100 MT))
(check-expect (draw-cat2 1) (place-image cat2 3 100 MT))
(check-expect (draw-cat2 2) (place-image cat1 6 100 MT))
(check-expect (draw-cat2 3) (place-image cat2 9 100 MT))

#;(big-bang 0
            (on-tick add1)
            (to-draw draw-cat2))

;;;;;;;;;;;;;;;;;;;;;;; Exercise 38 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; guage-color : number -> color
; If the number is less than 10, red. Less than 25, yellow, otherwise, green.
(define (guage-color n)
  (cond
    ([< n 10] 'red)
    ([< n 25] 'yellow)
    (else 'green)))
(check-expect (guage-color 100) 'green)
(check-expect (guage-color 50) 'green)
(check-expect (guage-color 24) 'yellow)
(check-expect (guage-color 4) 'red)

;;;;;;;;;;;;;;;;;;;;End Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; progress-bar : number(width) number(height) number(%) -> image
;; Takes a width, height, and percentage (between 0 and 1)
;; and produces a progress bar.
(define (progress-bar width height n%)
  (beside
   (rectangle (* width n%) height 'solid (guage-color (* 100 n%)))
   (rectangle (* width (- 1 n%)) height 'solid "black")))

;; happiness-tick : world -> world
; Subtract 0.1 from the world.
(define (happiness-tick w)
  (- w 0.1))
(check-expect (happiness-tick 100) 99.9)
(check-expect (happiness-tick 99.9) 99.8)
(check-expect (happiness-tick 99.8) 99.7)

;; happiness-stop : world -> boolean
; When to quite the world.
(define (happiness-stop w)
  (>= 0 w))
(check-expect (happiness-stop 100) false)
(check-expect (happiness-stop 0) true)
(check-expect (happiness-stop -1) true)

;; happiness-draw : world -> image
; Produces a progress bar from the world.
(define (happiness-draw w)
  (progress-bar W 10 (/ w 100)))
(check-expect (happiness-draw 100) (progress-bar W 10 1))
(check-expect (happiness-draw 50) (progress-bar W 10 0.5))
(check-expect (happiness-draw 0) (progress-bar W 10 0))

(big-bang 100.0
          (on-tick happiness-tick)
          (to-draw happiness-draw)
          (stop-when happiness-stop))





