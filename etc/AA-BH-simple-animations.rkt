;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-BH-simple-animations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| Programming 1 Animations Lab
   Andrew Amis
   Brian Hemmert
|#

(require picturing-programs)

(define X 100)
(define Y 100)

(define MT (empty-scene X Y))

;; sunset : number -> image
;; Makes a sunset
(define (sunset fn)
  (place-image
   (circle 10 "solid" "red")
   (/ X 2) fn
   MT))

;; sunrise : number -> image
;; Makes the sun rise
(define (sunrise fn)
  (place-image
   (circle 10 "solid" "yellow")
   (/ X 2) (- 101 fn)
   MT))

;; repeating-sunset : number -> image
;; Makes the sun set repeatedly
(define (repeating-sunset fn)
  (place-image
   (circle 10 "solid" "red")
   (/ X 2) (remainder fn 100)
   MT))

;; smooth-repeating-sunset : number -> image
;; A repeating, smooth, sunset.
(define (smooth-repeating-sunset fn)
  (place-image
   (circle 10 "solid" "red")
   (/ X 2) (remainder fn 110)
   MT))

(animate smooth-repeating-sunset)