#lang racket

(require picturing-programs
         test-engine/racket-tests)

;; normalize : number -> number
; Normalizes the number to a 0-255 range
(define (normalize n max)
  (round (* 255 (/ n max))))

;; within-distance : number(x) number(y) number(distance) -> boolean
; Determins whether the given x y coordinate is within distance of 0,0.
(define (within-distance x y distance)
  (> distance (sqrt (+ (expt x 2)
                       (expt y 2)))))

(check-expect (within-distance 3 3 5) true)
(check-expect (within-distance 0 0 1) true)
(check-expect (within-distance 3 3 2) false)


(check-within (normalize 50 100) 128 0.1)
(check-within (normalize 33 100) 84 0.1)

; fuzz : image -> image
(define (fuzz pic)
  (local [; near-pixel : num(x) num(y) -> color
          (define (near-pixel x y)
            (get-pixel-color (+ x -3 (random 7))
                             (+ y -3 (random 7))
                             pic))]
    (build-image (image-width pic)
                 (image-height pic)
                 near-pixel)))

(build3-image 250 250
                    (lambda (x y) (normalize (* x x) 62500))
                    (lambda (x y) y)
                    (lambda (x y) 0))
(fuzz (build-image 250 250
                   (lambda (x y)
                     (make-color
                      (cond
                        [(within-distance x y 100) 200]
                        [(within-distance x y 200) 100]
                        [(within-distance x y 250) 50]
                        [else 0])
                      (if (> 25 (random 255)) (normalize (* 2 x) 500)  0)
                      x))))

(test)