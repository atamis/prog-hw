;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #22: Conditionals
; Andrew Amis
; 11.7.11
; http://fellowhuman.com/gbk/2011/11/07/prog-1-asg-22-cond-and-def-by-choices/
; Exercises 15.5.6, 15.8.3, and 15.8.5.
; Exercises 17.1.2 and 17.1.6.

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.5.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; card-refund number -> number
; Returns the refund a credit card company might give you.
(define (card-refund money)
  (cond
    [(<= money 500)
     (* 0.0025 money)]
    [(<= money 1500)
     (+
      (card-refund 500)
      (* 0.005 (- money 500)))]
    [(<= money 2500)
     (+
      (card-refund 1500)
      (* 0.005 (- money 1500)))]
    [else
     (+
      (card-refund 2500)
      (* 0.01 (- money 2500)))]))
(check-within (card-refund 400) 1 0.1)
(check-within (card-refund 1400) 5.75 0.1)
(check-within (card-refund 2500) 11.25 0.1)
(check-within (card-refund 10000) 86.25 0.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.8.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; big? (number | string) -> boolean.
; Returns true if the number is at least 1000 and for strings, at least 10
; character long.
(define (big? x)
  (cond
    [(string? x) (>= (string-length x) 10)]
    [(number? x) (>= x 100)]
    [else (error "Argument was neither a number nor a string")]))

(check-expect (big? 10) false)
(check-expect (big? 100) true)
(check-expect (big? 200) true)
(check-expect (big? "nope") false)
(check-expect (big? "This is a test") true)
(check-expect (big? "1234567890") true)
(check-error (big? false) "Argument was neither a number nor a string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.8.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert->number : (number | string) -> number
; Converts a string or a number to a number.
(define (convert->number x)
  (cond
    [(number? x) x]
    [(string? x) (string->number x)]
    (else (error "Argument was neither a number nor a string"))))

(check-expect (convert->number 1) 1)
(check-expect (convert->number "3") 3)
(check-error (convert->number false) "Argument was neither a number nor a string")

;; smart-add : (number | string) (number | string) -> number
; Adds the 2 arguments together, ensuring they are numbers by converting them.
(define (smart-add x1 x2)
  (+ (convert->number x1) (convert->number x2)))
(check-expect (smart-add 1 1) 2)
(check-expect (smart-add 1 "1") 2)
(check-expect (smart-add "1" "1") 2)
(check-expect (smart-add "12" "14") 26)
(check-error (smart-add false 2) "Argument was neither a number nor a string")
(check-error (smart-add 2 true) "Argument was neither a number nor a string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; correct-image : number -> image
; Returns the image that goes with the number. Like a hash-map, only a function.
; Expects a number between 1 and 3
(define (correct-image n)
  (cond
    [(= n 1) (square 20 'solid 'red)]
    [(= n 2) (square 20 'solid 'green)]
    [(= n 3) (square 20 'solid 'blue)]
    [else (error "Argument wasn't between 1 and 3")]))

(check-expect (correct-image 1) (square 20 'solid 'red))
(check-expect (correct-image 2) (square 20 'solid 'green))
(check-expect (correct-image 3) (square 20 'solid 'blue))
(check-error (correct-image 4) "Argument wasn't between 1 and 3")

;; slideshow-animate : number -> image
; Runs a slideshow.
(define (slideshow-animate w)
  (correct-image (add1 (remainder w 3))))
(check-expect (slideshow-animate 0) (correct-image 1))
(check-expect (slideshow-animate 1) (correct-image 2))
(check-expect (slideshow-animate 2) (correct-image 3))
(check-expect (slideshow-animate 3) (correct-image 1))

#;(big-bang 0
            (on-tick add1 0.1) ; When 0.01, this produces interesting colors.
            (to-draw slideshow-animate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.1.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A light-color is any of the strings "green", "yellow", or "red".

(define radius 30) ; Radius of the traffic lights

; change-light :  light-color -> light-color
(define (change-light color)
  ; color                           light-color
  (cond [(string=?  color "green")  "yellow"]
        [(string=?  color "yellow") "red"]
        [(string=?  color "red")    "green"]))

(check-expect (change-light "green") "yellow")
(check-expect (change-light "yellow") "red")
(check-expect (change-light "red") "green")

;; draw-tl : light-color light-color -> image
; Draws a single traffic light.
(define (draw-tl lc target-lc)
  (circle radius 'solid
          (if (string=? lc target-lc)
              lc
              "black")))

(check-expect (draw-tl "green" "green") (circle 30 'solid "green"))
(check-expect (draw-tl "red" "green") (circle 30 'solid "black"))

; show-light :  light-color -> image
(define (show-light color)
  ; color          light-color
  (above
   (draw-tl color "red")
   (draw-tl color "yellow")
   (draw-tl color "green")))

(big-bang "red"
          (check-with string?)
          (on-draw show-light)
          (on-tick change-light 5))
