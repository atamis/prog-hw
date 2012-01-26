;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #25
; Andrew Amis
; 11.16.11
; http://fellowhuman.com/gbk/2011/11/16/prog-1-asg-25-posns/
; 20.4.2 and 20.4.4-20.4.7.
; 20.5.2, 20.5.4, and 20.5.5.
; Bonus: 20.5.6.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.4.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slope : posn posn -> (number | false)
; Returns the slope of 2 points, or, if slope is undefined, false.
(define (slope p1 p2)
  (if (not (= 0 (- (posn-x p2) (posn-x p1))))
      (/  (- (posn-y p2) (posn-y p1))
          (- (posn-x p2) (posn-x p1)))
      false))

(check-expect (slope (make-posn 0 0) (make-posn 1 1)) 1)
(check-expect (slope (make-posn 5 5) (make-posn 0 10)) -1)
(check-expect (slope (make-posn 1 1) (make-posn 1 10)) false)
(check-within (slope (make-posn 5 1) (make-posn 1 10)) -2.25 0.1)

; above-diagonal? : posn -> boolean
; Takes a posn and tells whether it is above the line y=x if that line were
; drawn on a screen.
(define (above-diagonal? posn)
  (if (false? (slope (make-posn 0 0) posn))
      false
      (< (slope (make-posn 0 0) posn) 1)))

(check-expect (above-diagonal? (make-posn 10 10)) false)
(check-expect (above-diagonal? (make-posn 10 11)) false)
(check-expect (above-diagonal? (make-posn 0 0)) false)
(check-expect (above-diagonal? (make-posn 3 2)) true)
(check-expect (above-diagonal? (make-posn -3 -2)) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.4.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; coordinate-difference : posn -> number
; Finds the difference between the coordinates. Also finds the distance this
; point is from tye y=x line.
(define (coordinate-difference posn)
  (abs (- (posn-x posn)
          (posn-y posn))))

(check-expect (coordinate-difference (make-posn 10 11)) 1)
(check-expect (coordinate-difference (make-posn 4 2)) 2)
(check-expect (coordinate-difference (make-posn 10 5)) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.4.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distance : posn posn -> number
; Calculates the distance between the two posns.
(define (distance here there)
  (sqrt (+ (sqr (- (posn-x here) (posn-x there)))
           (sqr (- (posn-y here) (posn-y there))))))

(check-within (distance (make-posn 3 0) (make-posn 3 4)) 4 0.1)
(check-within (distance (make-posn 0 0) (make-posn 0 10)) 10 0.1)
(check-within (distance (make-posn 3 4) (make-posn 1 10)) 6.32 0.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.4.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; posn=? : posn posn -> boolean
; Checks numerically whether the 2 posns are equal.
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) true)
(check-expect (posn=? (make-posn 1 1) (make-posn 2 1)) false)
(check-expect (posn=? (make-posn 1 1) (make-posn 1 2)) false)
(check-expect (posn=? (make-posn 1 1) (make-posn 2 2)) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.4.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distance-to-origin : (number | posn) -> number
; Calculates the distance from the given point/number to the appropriate origin.
(define (distance-to-origin x)
  (cond
    [(posn? x) (sqrt (+ (sqr (posn-x x))
                        (sqr (posn-y x))))]
    [(number? x) (abs x)]
    [else (error 'distance-to-origin "should be number or posn.")]))

(check-within (distance-to-origin (make-posn 0 10)) 10 0.1)
(check-within (distance-to-origin (make-posn 0 4)) 4 0.1)
(check-within (distance-to-origin 3) 3 0.1)
(check-within (distance-to-origin -3) 3 0.1)
(check-error (distance-to-origin false)
             "distance-to-origin: should be number or posn.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.5.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swap-x-y : posn -> posn
; Swaps the x and y of the given posn and produces a new posn.
(define (swap-x-y posn)
  (make-posn (posn-y posn) (posn-x posn)))
(check-expect (swap-x-y (make-posn 3 4)) (make-posn 4 3))
(check-expect (swap-x-y (make-posn 2 5)) (make-posn 5 2))
(check-expect (swap-x-y (make-posn 10 10)) (make-posn 10 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.5.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scale-posn : number posn -> posn
; Multiplies the x and the y by the provided number
(define (scale-posn n posn)
  (make-posn (* n (posn-x posn))
             (* n (posn-y posn))))

(check-expect (scale-posn 3 (make-posn 2 5)) (make-posn 6 15))
(check-expect (scale-posn 10 (make-posn 2 5)) (make-posn 20 50))
(check-expect (scale-posn -1 (make-posn 2 5)) (make-posn -2 -5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.5.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-posns : posn posn -> posn
; Adds the x and y of the posns, and makes a new posn from those x and y values.
(define (add-posns p1 p2)
  (make-posn 
   (+ (posn-x p1) (posn-x p2))
   (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns (make-posn 10 10) (make-posn 1 1)) (make-posn 11 11))
(check-expect (add-posns (make-posn 3 5) (make-posn 12 14)) (make-posn 15 19))
(check-expect (add-posns (make-posn 3 5) (make-posn 0 0)) (make-posn 3 5))
(check-expect (add-posns (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (add-posns (make-posn -1 -2) (make-posn 3 4)) (make-posn 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.5.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sub-posns : posn posn -> posn
; Subtracts the x and y values of the second posn from the first posn.
(define (sub-posns p1 p2)
  (make-posn 
   (- (posn-x p1) (posn-x p2))
   (- (posn-y p1) (posn-y p2))))

(check-expect (sub-posns (make-posn 10 10) (make-posn 1 1)) (make-posn 9 9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 12 14)) (make-posn -9 -9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 0 0)) (make-posn 3 5))
(check-expect (sub-posns (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (sub-posns (make-posn -1 -2) (make-posn 3 4)) (make-posn -4 -6))

;; new-sub-posns : posn posn -> posn
; Subtracts the x and the y values of the scond posn from the first posn.
(define (new-sub-posns p1 p2)
  (add-posns p1 (scale-posn -1 p2)))

(check-expect (new-sub-posns (make-posn 10 10) (make-posn 1 1))
              (make-posn 9 9))
(check-expect (new-sub-posns (make-posn 3 5) (make-posn 12 14))
              (make-posn -9 -9))
(check-expect (new-sub-posns (make-posn 3 5) (make-posn 0 0))
              (make-posn 3 5))
(check-expect (new-sub-posns (make-posn 0 0) (make-posn 0 0))
              (make-posn 0 0))
(check-expect (new-sub-posns (make-posn -1 -2) (make-posn 3 4))
              (make-posn -4 -6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 20.5.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; new-distance : posn posn -> number
; Calculates the distance between the two posns.
(define (new-distance here there)
  (distance-to-origin (sub-posns here there)))

(check-within (new-distance (make-posn 3 0) (make-posn 3 4)) 4 0.1)
(check-within (new-distance (make-posn 0 0) (make-posn 0 10)) 10 0.1)
(check-within (new-distance (make-posn 3 4) (make-posn 1 10)) 6.32 0.1)
