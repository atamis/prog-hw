#lang racket
;; Asg #1: Intro to Graphs
; Andrew Amis
; 8.31.12
; http://fellowhuman.com/gbk/2012/08/29/prog-3-asg-1-intro-to-graphs/

(require test-engine/racket-tests)

;; A Digraph      is a listof[Neighborhood].
;; A Neighborhood is a (cons Vertex listof[Vertex]), where the listof[Vertex]
;;                contains all neighbors of the Vertex.
;; A Vertex       is a symbol, the vertex's name.
;; Example:
(define G1
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))
;; This says, for example, that there are edges going from A to B and from
;; A to E, and that there are no edges coming out of D.

(define G2
  '((a h)
    (b g)
    (c d)
    (d g e)
    (e h g d)
    (f d)
    (g e b d)
    (h a e)))

(define G3
  '((a c)
    (b d)
    (c e)
    (d f)
    (e g)
    (f h)
    (g i)
    (h a)
    (i b)))

;; neighbors : digraph vertex
; Returns the neighbors of the given vertex.
(define (neighbors graph v)
  (let [(result (assoc v graph))]
    (if result
        (cdr result)
        (error "node not found"))))
(check-expect (neighbors G1 'A) '(B E))
(check-expect (neighbors G1 'B) '(E F))
(check-error (neighbors G1 'Z) "node not found")


;; lots-of-neighbors : digraph listof[vertices]
; Gets all the neighbors of the given vertices.
(define (lots-of-neighbors graph verts)
  (remove-duplicates (foldl {λ (vert acc) (append (neighbors graph vert) acc)}
                            empty
                            verts))
  #;(cond
      [(empty? verts) empty]
      [else (append
             (neighbors graph (car verts))
             (lots-of-neighbors graph (cdr verts)))]))

(check-expect (lots-of-neighbors G1 '(A B)) '(E F B))

;; find-path : digraph vertex vertex
; Find a path between the 2 given vertices in the given digraph.
(define (find-path graph v w)
  (let [(visited empty)]
    (letrec [(internal
              (λ (cur soln)
                (begin (set! visited (cons cur visited))
                       (if (symbol=? cur w)
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


(check-expect (find-path G1 'A 'A) '(A))
(check-expect (find-path G1 'A 'B) '(A B))
(check-expect (find-path G1 'A 'F) '(A B E F))
(check-expect (find-path G1 'A 'C) '(A B E C))
(check-expect (find-path G1 'G 'F) false)
(check-expect (find-path G2 'a 'd) '(a h e g d))
(check-expect (find-path G3 'a 'h) '(a c e g i b d f h))

(test)

