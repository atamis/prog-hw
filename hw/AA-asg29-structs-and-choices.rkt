;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg29-structs-and-choices) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #29: structs and choices
; Andrew Amis
; 12/14/11
; http://fellowhuman.com/gbk/2011/12/13/prog-1-asg-29-structs-and-choices/
; Exercize 21.7.4, 21.7.10, 21.8.3, 21.8.9 and 21.8.10

(require picturing-programs)


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
  (if (mouse=? event "button-down")
      (if (and (< 0 x 20)
           (< 0 y 80))
          (make-draworld
           (cond
             [(< 0 y 20) 'red]
             [(< 20 y 40) 'green]
             [(< 40 y 60) 'blue]
             [(< 60 y 80) 'white])
           (draworld-img world))
      (make-draworld
       (draworld-color world)
       (place-image (dot (draworld-color world))
                    x y
                    (draworld-img world))))
      world))
  

(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 5 "move")
              (make-draworld 'red (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 5 "button-down")
              (make-draworld 'red (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 25 "button-down")
              (make-draworld 'green (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 45 "button-down")
              (make-draworld 'blue (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         5 65 "button-down")
              (make-draworld 'white (empty-scene 100 100)))
(check-expect (drawmouse (make-draworld 'red (empty-scene 100 100))
                         55 55 "button-down")
              (make-draworld 'red (place-image (dot 'red)
                                               55 55
                                               (empty-scene 100 100))))


#;(define (drawmouse2 world x y event)
  (if (mouse=? event "move")
      (if (and (< 0 x 20)
           (< 0 y 80))
          (make-draworld
           (cond
             [(< 0 y 20) 'red]
             [(< 20 y 40) 'green]
             [(< 40 y 60) 'blue]
             [(< 60 y 80) 'white])
           (draworld-img world))
      (make-draworld
       (draworld-color world)
       (place-image (dot (draworld-color world))
                    x y
                    (draworld-img world))))
      world))

(define (draworld-main color)
  (big-bang (make-draworld color (empty-scene 500 500))
            (on-mouse drawmouse)
            (to-draw drawview)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercize 21.8.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Develop a data definition to represent circles in the plane. |#
 
;; A circ is a (make-circ posn positive-number):
(define-struct circ (loc radius))
;; where loc is the circle's location in the plane
;;   and radius is its radius (in pixels).
 
;; Example:
(define circ1 (make-circ (make-posn 50 75) 10))
 
;; Template: circ -&gt; ???
#;(define (fun-for-circ a-circ)
    ... (circ-loc a-circ)
    ... (circ-radius a-circ) ...)
 
 
#| Develop a data definition to represent rectangles in the plane. |#
 
;; A rect is a (make-rect posn positive-number positive-number):
(define-struct rect (top-left w h))
;; where top-left is the top-left corner of the rectangle, and
;;       w and h are the rectangle's width and height.
;; (Assume the rectangle's sides are parallel to the coordinate axes.
 
;; Example:
#;(define rect1 (make-rect (make-posn 100 50) 25 30))
 
;; Template: rect -&gt; ???
#;(define (fun-for-rect a-rect)
    ... (rect-top-left a-rect)
    ... (rect-w a-rect)
    ... (rect-h a-rect) ...)
 
;;;;;;;;;;;;
 
;; Develop the function rect-area that computes a rectangle's area.

;; shape is either a circ or a rect

;; rect-area : rect -&gt; positive-number
;; computes the area of a rectangle represented by a rect
#;(define (rect-area a-rect)
  ... (rect-top-left a-rect)
  ... (rect-w a-rect)
  ... (rect-h a-rect) ...)
 
#;(check-expect (rect-area rect1) (* 25 30))

;; contained? : shape posn -> boolean
; Returns whether the posn is inside the shape
(define (contained? shape posn)
  (cond
    [(circ? shape) (<= (distance (circ-loc shape) posn) (circ-radius shape))]
    [(rect? shape) (and (< (- ;; Use greater than and less than
    [else (error "not a shape")]))

(check-expect (contained? (make-circ (make-posn 50 75) 10) (make-posn 55 75))
              true)
(check-expect (contained? (make-circ (make-posn 0 0) 10) (make-posn 5 5))
              true)
(check-expect (contained? (make-circ (make-posn 0 0) 10) (make-posn 0 10))
              true)
(check-expect (contained? (make-circ (make-posn 0 0) 10) (make-posn 0 11))
              false)
(check-expect (contained? (make-rect (make-posn 0 10) 10 10) (make-posn 5 5))
              true)
(check-expect (contained? (make-rect (make-posn 0 10) 10 10) (make-posn 10 0))
              true)
(check-expect (contained? (make-rect (make-posn 0 10) 10 10) (make-posn 10 10))
              true)
(check-expect (contained? (make-rect (make-posn 0 10) 10 10) (make-posn 5 5))
              true)
(check-expect (contained? (make-rect (make-posn 0 10) 10 10) (make-posn 20 20))
              false)
(check-error (contained? false (make-posn 20 20)) "not a shape")