#lang racket

(require test-engine/racket-tests
         picturing-programs)

(define-struct posn (x y)
  #:transparent)



(define (midpoint p1 p2)
  (make-posn
   (/ (+ (posn-x p1) (posn-x p2)) 2)
   (/ (+ (posn-y p1) (posn-y p2)) 2)))




(define (spern image p1 p2 p3 n colors)
  (if (= n 0)
      image
      (add-line ;; Draw this triangle
       (add-line
        (add-line
         (spern ;; Draw the sub triangles
          (spern
           (spern 
            image
            p3
            (midpoint p2 p3)
            (midpoint p3 p1) (sub1 n) (cdr colors))
           p2
           (midpoint p1 p2)
           (midpoint p2 p3) (sub1 n) (cdr colors))
          p1
          (midpoint p1 p3)
          (midpoint p1 p2) (sub1 n) (cdr colors))
         (posn-x p3) (posn-y p3)
         (posn-x p1) (posn-y p1)
         (car colors))
        (posn-x p2) (posn-y p2)
        (posn-x p3) (posn-y p3)
        (car colors))
       (posn-x p1) (posn-y p1)
       (posn-x p2) (posn-y p2)
       (car colors))))

(define (grey n) (make-color n n n))

(define (arrange n)
  (reverse (build-list n {λ (x) (- 255 (* (/ 255 n) x))})))

(define (draw p1)
  (spern (empty-scene 1000 700)
         (make-posn 10 10)
         (make-posn 10 680)
         (make-posn 900 300)
         10
         (map grey (map round (arrange 10)))))

(big-bang (make-posn 10 10)
          (to-draw draw)
          (on-mouse {λ (world x y event) (make-posn x y)}))