;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA+WL-big-bang-sunset) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Andrew Amis and Willow Lark
; 10.14.11
; Big Bang simple animations
; http://fellowhuman.com/gbk/2011/10/14/friday-10-14-classwork-animations-revisited/

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The dimensions of our scene:
(define W 200)
(define H 200)

;; Size and x-coordinate of the sun:
(define RADIUS 10)
(define SUN-X (/ W 2))

;; Image constants:
(define BG (rectangle W H "solid" "white"))
(define SUN (circle RADIUS "solid" "orange"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "The world" is a number, representing the y-coordinate of the sun.
;; For example, if the sun hasn't started to rise, this is the world:
(define starting-world (- RADIUS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; At every clock tick, we want the sun to rise a little bit, so we will use
;; this function:

;; next-world : world -> world
;; Produces the next world, in which the sun has moved down a little bit.
(define (next-world w)
  ; w       | world | 3
  ; returns | world | 4
  (add1 w))
(check-expect (next-world 50) 51)
(check-expect (next-world starting-world) (add1 starting-world))

;; off-screen? : world -> boolean
;; True iff the animation is done.  In this program, the animation is done when
;; the sun has dropped off the screen.
(define (off-screen? w)
  ; w       | world   | 3
  ; returns | boolean | false
  (>= w (+ H RADIUS)))
(check-expect (off-screen? 0) false)
(check-expect (off-screen? (/ H 2)) false)
(check-expect (off-screen? (+ H RADIUS)) true)

;; draw : world -> image
;; Produces a picture of the current world.
(define (draw w)
  ; w       | world | 3
  ; returns | image | (place-image SUN (/ 2 W) 3 BG)
  (place-image SUN SUN-X w BG))

(check-expect (draw starting-world) (place-image SUN SUN-X starting-world BG))
(check-expect (draw 45) (place-image SUN SUN-X 45 BG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Run, prog, run! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This expression starts the animation going:

(big-bang starting-world
          (check-with number?)    ;; Verify that the world's always a number.
          (on-tick next-world)    ;; At each tick, call next-world.
          (to-draw draw)          ;; Use draw to redraw the scene at each tick.
          (stop-when off-screen?) ;; If off-screen? returns true for the world,
          )                       ;;   stop the animation.