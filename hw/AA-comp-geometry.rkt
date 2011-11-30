;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-comp-geometry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1: Computational geometry functions
; Andrew Amis
; 11.21.11
; http://fellowhuman.com/gbk/2011/11/21/prog-1-computational-geometry-functions/

(require picturing-programs)

;                                                   
;                                                   
;                                                   
;    ;;;     ;        ;         ;;;;;  ;   ;  ;   ; 
;   ;   ;    ;        ;         ;      ;   ;  ;   ; 
;   ;   ;    ;        ;         ;      ;   ;  ;;  ; 
;   ;   ;    ;     ;;;;         ;       ; ;   ; ; ; 
;   ;   ;    ;    ;   ;         ;;;;     ;    ;  ;; 
;   ;   ;    ;    ;   ;         ;       ; ;   ;   ; 
;   ;   ;    ;    ;   ;         ;      ;   ;  ;   ; 
;   ;   ;    ;    ;   ;         ;      ;   ;  ;   ; 
;    ;;;     ;;    ;;;;         ;      ;   ;  ;   ; 
;                                                   
;                                                   
;                                                   

(define π pi)

;; radians->degrees : number -> number
; Converts radians to degrees.
(define (radians->degrees rad)
  (* rad (/ 180 π)))
(check-within (radians->degrees π) 180 0.1)
(check-within (radians->degrees (* 2 π)) 360 0.1)
(check-within (radians->degrees (/ π 2)) 90 0.1)
(check-within (radians->degrees (/ π 4)) 45 0.1)

;; degrees->radians : number -> number
; Converts from degrees to radians
(define (degrees->radians deg)
  (* deg (/ π 180)))
(check-within (degrees->radians 180) π 0.1)
(check-within (degrees->radians 360) (* 2 π) 0.1)
(check-within (degrees->radians 720) (* 4 π) 0.1)
(check-within (degrees->radians 45) (/ π 4) 0.1)
(check-within (degrees->radians 90) (/ π 2) 0.1)

;; posn=? : posn posn -> boolean
; Checks numerically whether the 2 posns are equal.
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) true)
(check-expect (posn=? (make-posn 1 1) (make-posn 2 1)) false)
(check-expect (posn=? (make-posn 1 1) (make-posn 1 2)) false)
(check-expect (posn=? (make-posn 1 1) (make-posn 2 2)) false)

;; slope : posn posn -> (number | false)
; Returns the slope of 2 points, or, if slope is undefined, false.
(define (slope p1 p2)
  (if (not (= 0 (- (posn-x p2) (posn-x p1))))
      (/  (- (posn-y p2) (posn-y p1))
          (- (posn-x p2) (posn-x p1)))
      false))

;; distance : posn posn -> number
; Calculates the distance between the two posns.
(define (distance here there)
  (sqrt (+ (sqr (- (posn-x here) (posn-x there)))
           (sqr (- (posn-y here) (posn-y there))))))

(check-within (distance (make-posn 3 0) (make-posn 3 4)) 4 0.1)
(check-within (distance (make-posn 0 0) (make-posn 0 10)) 10 0.1)
(check-within (distance (make-posn 3 4) (make-posn 1 10)) 6.32 0.1)

(check-expect (slope (make-posn 0 0) (make-posn 1 1)) 1)
(check-expect (slope (make-posn 5 5) (make-posn 0 10)) -1)
(check-expect (slope (make-posn 1 1) (make-posn 1 10)) false)
(check-within (slope (make-posn 5 1) (make-posn 1 10)) -2.25 0.1)

;; scale-posn : number posn -> posn
; Multiplies the x and the y by the provided number
(define (scale-posn n posn)
  (make-posn (* n (posn-x posn))
             (* n (posn-y posn))))

(check-expect (scale-posn 3 (make-posn 2 5)) (make-posn 6 15))
(check-expect (scale-posn 10 (make-posn 2 5)) (make-posn 20 50))
(check-expect (scale-posn -1 (make-posn 2 5)) (make-posn -2 -5))

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

;; new-sub-posns : posn posn -> posn
; Subtracts the x and the y values of the scond posn from the first posn.
(define (sub-posns p1 p2)
  (add-posns p1 (scale-posn -1 p2)))


(check-expect (sub-posns (make-posn 10 10) (make-posn 1 1))
              (make-posn 9 9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 12 14))
              (make-posn -9 -9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 0 0))
              (make-posn 3 5))
(check-expect (sub-posns (make-posn 0 0) (make-posn 0 0))
              (make-posn 0 0))
(check-expect (sub-posns (make-posn -1 -2) (make-posn 3 4))
              (make-posn -4 -6))


;                                                          
;                                                          
;                                                          
;   ;   ;                       ;;;;;                 
;   ;   ;                       ;                      
;   ;;  ;                       ;                        
;   ; ; ;   ;;;   ; ; ;         ;      ;   ;  ;;;;         
;   ;  ;;  ;   ;  ; ; ;         ;;;;    ; ;   ;   ;        
;   ;   ;  ;;;;;  ; ; ;         ;        ;    ;   ;        
;   ;   ;  ;      ; ; ;         ;        ;    ;   ;        
;   ;   ;  ;   ;  ; ; ;         ;       ; ;   ;   ;        
;   ;   ;   ;;;    ; ;          ;      ;   ;  ;   ;        
;                                                          
;                                                          
;                                                          

;; posn=~? : posn posn number -> boolean
; Checks whether 2 posns are equal within the range of toleration provided.
(define (posn=~? p1 p2 err)
  (and
   (=~ (posn-x p1) (posn-x p2) err)
   (=~ (posn-y p1) (posn-y p2) err)))

(check-expect (posn=~? (make-posn 10 10) (make-posn 10 10) 1) true)
(check-expect (posn=~? (make-posn 10.111111 10) (make-posn 10 10) 1) true)
(check-expect (posn=~? (make-posn 10.111111 10.5) (make-posn 10 10) 0.5) true)
(check-expect (posn=~? (make-posn 1 10.5) (make-posn 1 10) 0.5) true)

;; rotate-posn : posn number -> posn
; Rotates a posn the given number of degrees around the origin.
(define (rotate-posn p theta)
  (make-posn
   (- (* (posn-x p) (cos theta)) (* (posn-y p) (sin theta)))
   (+ (* (posn-x p) (sin theta)) (* (posn-y p) (cos theta)))))

(check-expect
 (posn=~? (rotate-posn (make-posn 4 0) (/ π 2)) (make-posn 0 4) 0.1)
 true)
(check-expect
 (posn=~? (rotate-posn (make-posn 0 10) (/ π 2)) (make-posn -10 0) 0.1) true)
(check-expect
 (posn=~? (rotate-posn (make-posn -10 0) (/ π 2)) (make-posn 0 -10) 0.1)
 true)
(check-expect
 (posn=~? (rotate-posn (rotate-posn (make-posn 0 10) (/ π 2)) (/ π 2))
          (rotate-posn (make-posn 0 10) π) 0.1)
 true)
(check-expect
 (posn=~? (rotate-posn (make-posn 4 0) (/ (* π 13) 18))
          (make-posn -2.57 3.06) 0.1)
 true)

(check-within (posn-x (rotate-posn (make-posn 3 0) (/ π 4))) 2.12 0.01)


;; rotate/center : posn posn number -> posn
; Rotates the first posn around the second posn by the provided number of
; radians
(define (rotate/center posn center theta)
  (add-posns (rotate-posn (sub-posns posn center) theta) center))

(check-expect
 (posn=~? (rotate/center (make-posn 3.5 0.62) (make-posn 0.74 1.8) (/ π 4))
          (make-posn 3.53 2.92) 0.1) true)
(check-expect
 (posn=~? (rotate/center (make-posn 3.5 0.62)
                         (make-posn 2 2)
                         (/ (* π 13) 18))
          (make-posn 2.09 4.04) 0.1) true)
(check-expect
 (posn=~? (rotate/center (make-posn 4.5 4.28)
                         (make-posn 0.34 2.04)
                         (/ (* π 23) 18))
          (make-posn -0.68 -2.66) 0.1) true)


;; dilate-posn : posn posn number -> posn
; Dilates the first posn form the second by the number. This is just scale-posn
; with a center
(define (dilate-posn posn center scale)
  (add-posns
   (scale-posn scale (sub-posns posn center))
   center))

(check-expect (dilate-posn (make-posn 2 2) (make-posn 2 4) 2) (make-posn 2 0))
(check-expect (dilate-posn (make-posn 2 0) (make-posn 2 2) 3) (make-posn 2 -4))
(check-expect (dilate-posn (make-posn 0 0) (make-posn 2 2) 3) (make-posn -4 -4))

;; draw-vec : posn color image -> image
; Draws a line connecting the origin to the posn of the given color on the
; given background.
(define (draw-vec posn color img)
  (scene+line img
              0 0
              (posn-x posn) (posn-y posn)
              color))

(check-expect (draw-vec (make-posn 2 2) 'green (empty-scene 20 20))
              (scene+line (empty-scene 20 20) 0 0 2 2 'green))
(check-expect (draw-vec (make-posn 3 2) 'green (empty-scene 20 20))
              (scene+line (empty-scene 20 20) 0 0 3 2 'green))
(check-expect (draw-vec (make-posn 30 30) 'blue (empty-scene 20 20))
              (scene+line (empty-scene 20 20) 0 0 30 30 'blue))

;; semiperimeter : number number number -> number
; Caculate the semiperimeter of a triangle with the given side lengths.
(define (semiperimeter a b c)
  (/ (+ a b c) 2))
(check-expect (semiperimeter 10 5 5) 10)
(check-expect (semiperimeter 12 5 5) 11)
(check-expect (semiperimeter 12 2 5) 9.5)

;; triangle-area/heron : number number number -> numer
; Returns the area of a triangle with the given side lengths using Hero's
; formula.
(define (triangle-area/heron a b c)
  (sqrt (* (semiperimeter a b c)
           (- (semiperimeter a b c) a)
           (- (semiperimeter a b c) b)
           (- (semiperimeter a b c) c))))
(check-within (triangle-area/heron 10 10 14.14) 50 0.1)
(check-within (triangle-area/heron 20 10 22.36) 100 0.1)
(check-within (triangle-area/heron 20 5 20.62) 50 0.1)


;; triangle-area : posn posn posn -> number
; Using triangle-area/heron, calculates the area of a triangle from the given
; points
(define (triangle-area p1 p2 p3)
  (triangle-area/heron
   (distance p1 p2)
   (distance p2 p3)
   (distance p1 p3)))

(check-within (triangle-area (make-posn 0 0)
                             (make-posn 0 10)
                             (make-posn 10 0)) 50 0.1)
(check-within (triangle-area (make-posn 0 0)
                             (make-posn 10 0)
                             (make-posn 0 10)) 50 0.1)
(check-within (triangle-area (make-posn 0 0)
                             (make-posn 5 0)
                             (make-posn 0 10)) 25 0.1)


;; cross-product : posn posn -> number
; Returns the z value of the cross product of the provided posns assuming their
; z values are 0.
(define (cross-product-z p1 p2)
  (- (* (posn-x p1) (posn-y p2))
     (* (posn-x p2) (posn-y p1))))

;; same-side? : posn posn posn posn -> boolean
; Returns true if the last 2 posns are on the same side of the line defined by
; the first 2 posns
(define (same-side? a b p1 p2)
  (>= (* (cross-product-z (sub-posns b a) (sub-posns p1 a))
         (cross-product-z (sub-posns b a) (sub-posns p2 a))) 0))

(check-expect (same-side? (make-posn 0 0) (make-posn 10 10)
                          (make-posn 1 0) (make-posn 2 0)) true)
(check-expect (same-side? (make-posn 0 0) (make-posn 10 10)
                          (make-posn 1 0) (make-posn 0 2)) false)
(check-expect (same-side? (make-posn 0 0) (make-posn 10 10)
                          (make-posn 1 1) (make-posn 2 2)) true)


;; inside-triangle? : posn posn posn posn -> boolean
; Checks whether the first posn is inside the triangle defined by the last 3
; posns
(define (inside-triangle? p a b c)
  (and (same-side? b c p a)
       (same-side? a c p b)
       (same-side? a b p c)))


(check-expect (inside-triangle? (make-posn 2 2)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 5 5)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 0 3)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 5 6)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) false)
(check-expect (inside-triangle? (make-posn 5 0)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 5 -1)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) false)
(check-expect (inside-triangle? (make-posn -1 0)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) false)
(check-expect (inside-triangle? (make-posn 0 0)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 0 10)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)
(check-expect (inside-triangle? (make-posn 10 0)
                                (make-posn 0 0)
                                (make-posn 0 10)
                                (make-posn 10 0)) true)

