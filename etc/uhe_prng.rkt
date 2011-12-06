#lang racket

(require test-engine/racket-tests)

;; mash : data -> number
; Hash written for javascript

(define (mash data)
  (let ([n 4022871197])
    (for ([x (string->list data)])
      (set! n (+ n (char->integer x)))
      (let ([h (* 0.02519603282416938 n)])
        (set! n (floor h))
        (set! h (- h n))
        (set! h (* n h))
        (set! n (floor h))
        (set! h (- h n))
        (set! n (* 4294967296 h))
        (displayln n)))
    (* 2.3283064365386963e-10 (floor n))))



(mash "test")

(check-within (mash "This is awesome") 0.3718118635006249 0.00001)
(check-within (mash "This is a test") 0.5120114255696535 0.00001)

(test)