;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-BH-simple-animations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| Programming 1 Animations Lab
   Andrew Amis
   Brian Hemmert
|#

(require picturing-programs)

(define X 300)
(define Y 100)
(define sun-x-loc (* X 2/3))

(define MT (empty-scene X Y))

;; sunset : number -> image
;; Makes a sunset
(define (sunset fn)
  (place-image
   (circle 10 "solid" "red")
   sun-x-loc fn
   MT))

;; sunrise : number -> image
;; Makes the sun rise
(define (sunrise fn)
  (place-image
   (circle 10 "solid" "yellow")
   sun-x-loc (- (+ Y 1) fn)
   MT))

;; repeating-sunset : number -> image
;; Makes the sun set repeatedly
(define (repeating-sunset fn)
  (place-image
   (circle 10 "solid" "red")
   sun-x-loc (remainder fn Y)
   MT))

;; smooth-repeating-sunset : number -> image
;; A repeating, smooth, sunset.
(define (smooth-repeating-sunset fn)
  (place-image
   (circle 10 "solid" "red")
   sun-x-loc (remainder fn (+ 10 Y))
   MT))

;; overlapping-sunset : number -> image
;; An overlapping multi-sun sunset.
(define (overlapping-sunset fn)
  (place-image
   (circle 10 "solid" "red")
   sun-x-loc (- (remainder fn (+ 10 (* Y 2))) 10)
   (place-image
    (circle 10 "solid" "red")
    sun-x-loc (- (remainder (- fn Y) (+ 10 (* Y 2))) 10)
    MT)))

;; Done only by Andrew

;; wave : number -> image
;; Moves a green cricle in a sine wave.
(define (wave fn)
  (place-image
   (circle 10 "solid" "green")
   (remainder fn X) (+ (/ Y 2) (sin fn))
   MT))


;; pendulum-sine-wave : number -> number
;; Makes the pendulum sine wave, with a few modifications.
(define (pendulum-sine-wave fn)
  (* (sin (/ fn 10)) 100))
  
;; pendulum-x : number -> image
;; Provides the x value for the pendulum function for a given frame number
(define (pendulum-x fn)
  (+ (/ X 2) (pendulum-sine-wave fn)) )


;; pendulum-y : number -> image
;; Provides the y value for the pendulum function for a given frame number
(define (pendulum-y fn)
  (+ (/ Y 2) (real-part (* -0.01 (expt (pendulum-sine-wave fn) 2)))))

;; pendulum : number -> image
;; Moves a circle in a pendulum movement
(define (pendulum fn)
  (place-image
   (circle 10 "solid" "blue")
   (pendulum-x fn)
   (pendulum-y fn)
   (scene+line
    MT
    (/ X 2) 0
    (pendulum-x fn) (pendulum-y fn)
    "black")))
               

;; Note: to represent more accurate pendulum motion would require more math than I know.
;; To represent perfect or near perfect pendulum motion, one would need vectors
;; of some kind.


(animate overlapping-sunset)