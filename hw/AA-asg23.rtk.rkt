;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg23.rtk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #23: Itemizations
; Andrew Amis
; 11.10.11
; http://fellowhuman.com/gbk/2011/11/10/prog-1-asg-23-itemizations/
; Exercises 43-46

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 43 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A LR (short for: launching rocket) is one of:
; - "resting"
; - non-negative number
; interp. a rocket on the ground or the height of a rocket in flight 

; Examples:
; "resting"
; 1...inf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 44 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (string=? "resting" x) fails when given a number.
; (and (string? x) (string=? "resting" x))

; Their third clause includes 0, while the data definition doesn't. A more
; accurate clause would be:
; (> x 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 45 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; physical constants
(define HEIGHT 300)
(define WIDTH  100)
(define YDELTA 3)

; graphical constants
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red")) ; use your favorite image
(define ROCKET-CENTER (/ (image-height ROCKET) 2))

;; place-rocket : number -> image
; Takes the x location of the rocket and adds it to the background.
(define (place-rocket x)
  (place-image ROCKET 10 (- x ROCKET-CENTER) BACKG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 46 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; LRCD -> LRCD
; raise the rocket by YDELTA if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

(define (show x)
  (cond
    [(string? x)
     (place-image ROCKET 10 (- HEIGHT ROCKET-CENTER) BACKG)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-image ROCKET
                               10 (- HEIGHT ROCKET-CENTER)
                               BACKG))]
    [(>= x 0)
     (place-image ROCKET 10 (- x ROCKET-CENTER) BACKG)]))
(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT ROCKET-CENTER) BACKG))
 
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 ROCKET-CENTER) BACKG))

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)



(define (main1 s)
  (big-bang s 
            (to-draw show)
            (on-key launch)
            (on-tick fly 0.1)))


;; at-top : lrcd -> boolean
; Checks whether the rocket is out of view.
(define (at-top x)
  (cond
    [(string? x) false]
    [(< x HEIGHT) false]
    [(>= x HEIGHT) true]))

(check-expect (at-top "resting") false)
(check-expect (at-top -3) false)
(check-expect (at-top -2) false)
(check-expect (at-top -1) false)
(check-expect (at-top 10) false)
(check-expect (at-top (add1 HEIGHT)) true)

(define (main2 s)
  (big-bang s
            (to-draw show)
            (on-key launch)
            (stop-when at-top)
            (on-tick fly 0.1)))

; When it reaches the top, the countdown starts all over again.
