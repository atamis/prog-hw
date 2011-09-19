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
   sun-x-loc (remainder fn (* Y 2))
   (place-image
    (circle 10 "solid" "red")
    sun-x-loc (- (remainder fn (* Y 2)) 100)
    MT)))

(animate overlapping-sunset)