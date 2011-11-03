#lang racket

(require picturing-programs)


;; shear : image -> image
; Applies a shear transformation to a given image.
(define (shear image scale)
  (build-image (* (image-width image) (+ (abs scale) 2))
               (image-width image)
               (lambda (x y)
                 (get-pixel-color
                  (inexact->exact (round (+ x (* (- scale) y))))
                  y image))))


(define sq (square 20 'solid 'red))
(define ci (circle 20 'solid 'blue))
(define tr (triangle 20 'solid 'green))

(shear ci -2)
(shear ci 3)
(shear ci 4)