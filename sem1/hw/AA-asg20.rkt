;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #20: Boolean Review, part 2)
; Andrew Amis
; 11.1.11
; http://fellowhuman.com/gbk/2011/11/01/prog-1-asg-20-boolean-review-part-2/
; 13.4.1, 13.4.2, 13.5.1, 13.7.3, 13.7.4, 13.7.8 and 13.7.13
; In Section 13.8, what is the author trying to say about and and or?

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.4.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; much-older : number(person1) number(person2) -> boolean
; Determines whether person1 is "much older" (10 years older) than person2.
(define (much-older person1 person2)
  (>= person1 (+ 10 person2)))

(check-expect (much-older 20 10) true)
(check-expect (much-older 15 10) false)
(check-expect (much-older 20 0) true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.4.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; within-distance : number(x) number(y) number(distance) -> boolean
; Determins whether the given x y coordinate is within distance of 0,0.
(define (within-distance x y distance)
  (> distance (sqrt (+ (expt x 2)
                       (expt y 2)))))

(check-expect (within-distance 3 3 5) true)
(check-expect (within-distance 0 0 1) true)
(check-expect (within-distance 3 3 2) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.5.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-green-triangle? : image -> boolean
; Checks whether a given image is a green triangle of size 10.
(define (is-green-triangle? image)
  (image=? image
           (triangle 10 'solid 'green)))
(check-expect (is-green-triangle? (triangle 10 'solid 'green)) true)
(check-expect (is-green-triangle? (triangle 12 'solid 'green)) false)
(check-expect (is-green-triangle? (square 4 'outline 'red)) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.7.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; teenage? : number -> boolean
; From the given ages, determines if somebody is a teenager between the ages of
; 13 and 20.
(define (teenage? age)
  (and (>= age 13)
       (<= age 20)))
(check-expect (teenage? 10) false)
(check-expect (teenage? 13) true)
(check-expect (teenage? 18) true)
(check-expect (teenage? 20) true)
(check-expect (teenage? 21) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.7.4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-or-over-100? : number -> boolean
; Checks whether the given number is greater than 100 or negative.
(define (negative-or-over-100? n)
  (or (< n 0)
      (> n 100)))
(check-expect (negative-or-over-100? 40) false)
(check-expect (negative-or-over-100? 100) false)
(check-expect (negative-or-over-100? 0) false)
(check-expect (negative-or-over-100? 101) true)
(check-expect (negative-or-over-100? -1) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.7.8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not-13? : number -> boolean
; Checks whether a number is not 13.
(define (not-13? n)
  (not (= 13 n)))
(check-expect (not-13? 13) false)
(check-expect (not-13? 14) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;13.7.13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-two-same-pics? : image image image -> boolean
; Determines whether 2 of the given pics are the same
(define (any-two-same-pics? i1 i2 i3)
  (or (image=? i1 i2)
      (image=? i1 i3)
      (image=? i2 i3)))

(define msquare (square 10 'solid 'red))
(define mcircle (circle 10 'solid 'blue))
(define mtriangle (triangle 10 'solid 'green))

(check-expect (any-two-same-pics? msquare msquare mcircle) true)
(check-expect (any-two-same-pics? mcircle msquare mcircle) true)
(check-expect (any-two-same-pics? msquare mcircle mcircle) true)
(check-expect (any-two-same-pics? msquare msquare msquare) true)
(check-expect (any-two-same-pics? msquare mtriangle mcircle) false)


; That they are macros. It also warns us (a little) against premature 
; optomization.