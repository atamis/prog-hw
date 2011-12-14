;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg29-structs-and-choices) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #29: structs and choices
; Andrew Amis
; 12/14/11
; http://fellowhuman.com/gbk/2011/12/13/prog-1-asg-29-structs-and-choices/
; Exercize 21.7.4, 21.7.10, 21.8.3, 21.8.9 and 21.8.10

(require picturing-programs)

;; circle : posn number
(define-struct circ (loc radius))
; circle-loc : posn representing the circle's location in a plane
; circle-radius: radius of the circle

#;(define (fun-for-circ circ)
    ... (circ-loc circ)
    ... (circ-radius circ) ...)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercize 21.7.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; distance : posn posn -> number
; Calculates the distance between the two posns.
(define (distance here there)
  (sqrt (+ (sqr (- (posn-x here) (posn-x there)))
           (sqr (- (posn-y here) (posn-y there))))))

(check-within (distance (make-posn 3 0) (make-posn 3 4)) 4 0.1)
(check-within (distance (make-posn 0 0) (make-posn 0 10)) 10 0.1)
(check-within (distance (make-posn 3 4) (make-posn 1 10)) 6.32 0.1)

;; circles-overlap? : circle circle -> boolean
; Do these circles overlap? Assumes that circles touching are overlapping
(define (circles-overlap? c1 c2)
  (<= (distance (circ-loc c1) (circ-loc c2))
      (+ (circ-radius c1)
         (circ-radius c1))))
(check-expect (circles-overlap?
               (make-circ (make-posn 0 0) 5)
               (make-circ (make-posn 10 0) 5)) true)
(check-expect (circles-overlap?
               (make-circ (make-posn 0 0) 5)
               (make-circ (make-posn 10 10) 5)) false)
(check-expect (circles-overlap?
               (make-circ (make-posn 0 0) 5)
               (make-circ (make-posn 0 0) 5)) true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercize 21.7.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draworld : color image
(define-struct draworld (color img))
; draworld-color : color selected
; draworld-img : image being drawn

(define (dot color) (circle 4 'solid color))

(define selector (above
                  (square 20 'solid 'red)
                  (square 20 'solid 'green)
                  (square 20 'solid 'blue)
                  (square 20 'outline 'black)))


;; drawview : draworld -> image
; Draws the world for draworld
(define (drawview dw)
  (overlay/align 'left 'top
                 selector
                 (draworld-img dw)))
(check-expect (drawview (make-draworld 'red (empty-scene 100 100)))
              (overlay/align 'left 'top selector (empty-scene 100 100)))
(check-expect (drawview (make-draworld 'green (empty-scene 100 100)))
              (overlay/align 'left 'top selector (empty-scene 100 100)))

;; drawmouse : draworld number number mouse-event -> draworld
; Handles mouse for draworld.
(define (drawmouse world x y event)
  

(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 5 "move")
              (make-draworld 'red (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 5 "mouse-down")
              (make-draworld 'red (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 25 "mouse-down")
              (make-draworld 'green (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 45 "mouse-down")
              (make-draworld 'blue (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         55 55 "mouse-down")
              (make-draworld 'red (place-image (dot 'red)
                                               55 55
                                               (empty-scene 100 100))))


