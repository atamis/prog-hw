#lang racket
; Prog 3 Asg #3: Running Time, O(), and Vectors
; Andrew Amis
; 9.13.12
; http://fellowhuman.com/gbk/2012/09/12/prog-3-asg-3-running-time
;                                                          -big-oh-and-vectors/

(require test-engine/racket-tests)


#|
                  29.2.1

f(n) = n^2 + n
g(n) = n^2

f(n) <= c * g(n)

bigEnough = 7
c = 1

f(7) = 56
g(7) = 49
7 is big enough.

                  29.2.2

f(n) = 2^n

g(n) = 1000 · n
c = 1
bigEnough = 14

If input is between 3 and 12, f(n) is getter.

                  29.2.2

f(n) = n log n

g(n) = n^2

f(n) in O(g)?

f(n) <= g(n) is always true, so yes.

g(n) in O(f)?

See above. No.

|#


(define (! n)  ;; Nat -> Nat
  (cond [(zero? n) 1]
        [(> n 0)   (* n (! (sub1 n)))]))

(! 10)
(* 10 (! 9))
(* 10 (* 9 (! 8)))
(* 10 (* 9 (* 8 (! 7))))
(* 10 (* 9 (* 8 (* 7 (! 6)))))
(* 10 (* 9 (* 8 (* 7 (* 6 (! 5))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (! 4)))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (! 3))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (! 2)))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (* 2 (! 1))))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (* 2 1)))))))))

; ! is O(n).


#|
f(n) = n^3

g(n) = n^2

assume f is in O(g).

f(n) <= 1 * g(n)

n^3 = n^2
/n
n^2 = n
/n
n = 1

at n = 1, f(n) = g(n).

However, at n+1, f(n+1) > g(n+1), so f is not in O(g).
|#


(define G2
  '((a h)
    (b g)
    (c d)
    (d g e)
    (e h g d)
    (f d)
    (g e b d)
    (h a e)))

(define vG2
  (vector
   '(7)
   '(6)
   '(3)
   '(6 5)
   '(7 6 3)
   '(3)
   '(4 1 3)
   '(0 4)))


;; neighbors : node graph  ->  (listof node)
;; to lookup the node in graph
(define (neighbors graph node)
  (if (vector? graph)
      (vector-ref graph node)
      (let [(result (assoc node graph))]
        (if result
            (cdr result)
            (error "node not found")))))

(check-expect (neighbors vG2 0)
              '(7))
(check-expect (neighbors vG2 4)
              '(7 6 3))
(check-expect (neighbors G2 'a) '(h))
(check-expect (neighbors G2 'e) '(h g d))



;; find-path : digraph vertex vertex
; Find a path between the 2 given vertices in the given digraph.
(define (find-path graph v w)
  (let [(visited empty)]
    (letrec [(internal
              (λ (cur soln)
                (begin (set! visited (cons cur visited))
                       (if (eq? cur w)
                           (cons cur soln)
                           (foldl-replacement
                            (remove* visited (neighbors graph cur))
                            cur soln)
                           ; Using foldl tends to find a shorter path, but
                           ; takes longer.
                           ; see (find-path G1 'A 'F) and (find-path G2 'a 'd)
                           #;(foldl {lambda (x acc)
                                    (let [(c (internal x (cons cur soln)))]
                                      (if c
                                          c
                                          acc))}
                                  false
                                  (remove* visited (neighbors graph cur)))
                           )
                       ))
              )
             (foldl-replacement
              {λ (nodes cur soln)
                (cond
                  [(empty? nodes) false]
                  [else
                   (let [(c (internal (car nodes) (cons cur soln)))]
                     (if c
                         c
                         (foldl-replacement (cdr nodes) cur soln)))])})
             ]
      (let ([result (internal v empty)])
        (if result
            (reverse result)
            false)))))

(check-expect (find-path vG2 '0 '3) '(0 7 4 6 3))
(check-expect (find-path G2 'a 'd) '(a h e g d))

(when false
  (time (for ([i (in-range 1000000)])
          (find-path vG2 '0 '3)))
  (time (for ([i (in-range 1000000)])
          (find-path G2 'a 'd))))

; 29.3.3 is uncessary, as G2 (and by extension, vG2) is already cyclic.




;; vector-sum : (vectorof number)  ->  number
;; to compute the sum of the numbers in v
(define (vector-sum v) 
  (vector-sum-aux v (vector-length v)))

;; vector-sum-aux : (vectorof number) N  ->  number
;; to sum the numbers in v with index in [0, i)
(define (vector-sum-aux v i) 
  (cond
    [(zero? i) 0]
    [else (+ (vector-ref v (sub1 i)) 
	     (vector-sum-aux v (sub1 i)))]))

;; It adds them up in order, first to last, more or less.

;; lr-vector-sum adds them up in reverse order. It adds the last 2 numbers, then
; adds that number to the 3rd last number, and on and on.


;; Running time of sum is O(N).
; Running time of vector-sum is O(N).
; Running time of list-sum is, being pessimistic, O(2N).


;; norm : vector -> number
; Calculates the square root of the sum of the squares of the numbers
(define (norm vec)
  (sqrt (for/fold
            ([i 0])
          ([x vec])
          (+ (sqr x) i))))

(check-expect (norm (vector 10)) 10)
(check-within (norm (vector 10 4)) (sqrt 116) 0.01)
              


;; vector-contains-doll : vec -> (number | false)
; If the vector contains 'doll, it returns the index number of 'doll. Otherwise,
; false.
(define (vector-contains-doll vec)
  (for/fold ([r false]) ([x vec] [i (in-range (vector-length vec))])
    (cond
      [r r]
      [(eq? 'doll x) i]
      [else r])))

(check-expect (vector-contains-doll (vector 1 2 3)) false)
(check-expect (vector-contains-doll (vector 1 2 3 'doll)) 3)

(test)
