#lang racket

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA


;; mp : number number -> line
; Makes a point.
(define (mp x y) (cons x y))


;; x : point -> number
; Returns the x portion of a point
(define (x point)
  (car point))


;; y : point -> number
; Returns the y portion of a point
(define (y point)
  (cdr point))

;; ml : point point -> line
; Makes a line
(define (ml p1 p2)
  (cons p1 p2))

;; mlp : number number number number -> line
; Makes a line from 4 numbers, representing the coordinates of the points.
(define (mlp x1 y1 x2 y2)
  (ml
   (mp x1 y1)
   (mp x2 y2)))


(define (p1 line)
  (car line))

(define (p2 line)
  (cdr line))

(struct posn (x y)
  #:extra-constructor-name make-posn)


(define X 500)
(define Y 500)

(define BG (empty-scene X Y))


(define POINTS
  (build-list 10
              (lambda (x)
                (mp (random X) (random Y)))))

(define DOT (circle 2 'solid 'red))

;; add-point : point image -> image
; Draw the point on the image.
(define (add-point point image)
  ; points  | point | (mp 10 10)
  ; returns | image | (place-image DOT 10 10 image)
  (place-image
   DOT
   (x point)
   (y point)
   image))

;; draw-points : points -> image
; Draw all the points in the list on the image
(define (draw-points points image)
  ; points  | list of points | (list (mp 10 10) (mp 20 20))
  ; returns | image          | (place-image DOT 
  (foldl
   (lambda (point building-image)
     (add-point point building-image))
   image
   points))

(define (all-lines points)
  (letrec
      ([f
        (lambda (points lines)
          (if (empty? points)
              lines
              (f (cdr points)
                 (foldl
                  (lambda (point1 result)
                    (foldl
                     (lambda (point2 result)
                       (if (and
                            (not (= (x point1) (x point2)))
                            (not (= (y point1) (y point2))))
                        (cons (ml point1 point2) result)
                        result))
                     result
                     points))
                  lines
                  points))))])
    (f points '())))

;; draw-all-lines : list image : image
(define (draw-all-lines points image)
    (foldl
     (lambda (line result)
       (scene+line result
                   (x (p1 line))
                   (y (p1 line))
                   (x (p2 line))
                   (y (p2 line))
                   'black))
     image
     (all-lines points)))

(draw-points POINTS (draw-all-lines POINTS BG))
