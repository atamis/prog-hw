;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname asg7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Andrew Amis
;; http://fellowhuman.com/gbk/2011/09/21/prog-1-asg-7-function-definition-practice/

(require picturing-programs)

;; bulls-eye : number color(outer) color(inner) -> image
;; Makes a bullseye of radius radius, with some circles
(define (bulls-eye radius outer inner)
  (overlay
   (circle (* radius 1/3) "solid" outer)
   (circle (* radius 2/3) "solid" inner)
   (circle (* radius 3/3) "solid" outer)))

(beside (bulls-eye 50 "blue" "green")
        (bulls-eye 50 "red" "black")
        (bulls-eye 50 "green" "red"))

;; horizontal-progress-bar : number(width) number(height) number(%) -> image
;; Takes a width, height, and percentage (between 0 and 1)
;; and produces a progress bar.
(define (horizontal-progress-bar width height n%)
  (beside
   (rectangle (* width n%) height "solid" "red")
   (rectangle (* width (- 1 n%)) height "solid" "black")))

(horizontal-progress-bar 100 10 0.235234234234234234234)

;; test-progress-bar : number -> image
;; Takes ticks and produces an image with the apropriate progress bar
(define (test-progress-bar ticks)
  (horizontal-progress-bar 100 10 (/ (remainder (abs (- 280 ticks)) 280) 280)))

(animate test-progress-bar)


;; Bonus:
;; If you have an already defined identifier that you want to use
;; in the function, if any of the argument identifiers have the same
;; name as the external definition, the parameter will be used 
;; instead of the external definition, so one should be careful.