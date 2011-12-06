#lang racket

(require test-engine/racket-tests
         graphics/turtles)

(define (algae-system l)
  (foldr
   (lambda (x accum)
     (cond
       [(symbol=? x 'a)
        (cons 'a
              (cons 'b accum))]
       [(symbol=? x 'b) (cons 'a accum)]))
   '()
   l))

(check-expect (algae-system '()) '())
(check-expect (algae-system '(a)) '(a b))
(check-expect (algae-system '(b)) '(a))
(check-expect (algae-system '(a b)) '(a b a))
(check-expect (algae-system '(a b a)) '(a b a a b))

(define (multi-algae list n)
  (if (= n 0)
      list
      (multi-algae (algae-system list) (sub1 n))))


(define (koch-curve l)
  (foldr
   (lambda (x accum)
     (cond
       [(symbol=? x 'F)
        (list* 'F '+ 'F '− 'F '+ 'F '+ 'F '- 'F '-
               accum)]
        [else (cons x accum)]))
   '()
   l))

(turtles true)

(define DISTANCE 5)
(define ANGLE (/ pi 2))

(define (multi-koch list n)
  (if (= n 0)
      list
      (multi-koch (koch-curve list) (sub1 n))))

(for ([i (multi-koch '(F) 6)])
  (cond
    [(symbol=? i 'F) (draw DISTANCE)]
    [(symbol=? i '+) (turn/radians ANGLE)]
    [(symbol=? i '-) (turn/radians (- ANGLE))]))

(test)

