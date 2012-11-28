#lang typed/racket

(require "env.rkt") ; used in alpha-equivalent?.

(provide constant?
         decl?
         primitive? user-prim? system-prim?
         predicate-prim? value-prim? effect-prim? effect-free-prim?
         )

(: disjoin ((Any -> Boolean) * -> (Any -> Boolean)))
(define (disjoin . p?*)
  (cond
    [(empty? p?*) (lambda (x) #f)]
    [else
     (λ (x)
       (or ((first p?*) x) ((apply disjoin (rest p?*)) x)))]))

(: constant? (Any -> Boolean))
;; true iff the argument is a constant in the input language
(define constant?
  (disjoin empty? number? char? boolean?))

(: decl? (Any -> Boolean))
;; true iff the argument is a single declaration/binding of a let- or
;; letrec-expr
(define (decl? x)
  (match x
    [(list (? symbol? lhs) rhs)
     #t]
    [otherwise #f]))

;                                          
;                                          
;                                          
;                     ;                    
;                                          
;                                          
;   ; ;;;    ; ;;;  ;;;    ; ;; ;;   ;;;;  
;   ;;   ;   ;;  ;    ;    ;; ;; ;  ;    ; 
;   ;    ;   ;   ;    ;    ;  ;  ;  ;      
;   ;    ;   ;        ;    ;  ;  ;   ;;    
;   ;    ;   ;        ;    ;  ;  ;     ;;  
;   ;    ;   ;        ;    ;  ;  ;       ; 
;   ;   ;    ;        ;    ;  ;  ;  ;    ; 
;   ;;;;     ;        ;;;  ;  ;  ;   ;;;;  
;   ;                                      
;   ;                                      
;   ;                                      
;                                          


;; Four types of primitives:
(define-type PrimType (U 'not 'test 'value 'effect))

;; Type predicates for primitives:

(: primitive? (Any -> Boolean))
(define (primitive? x) (or (user-prim? x) (system-prim? x)))

(: user-prim? (Any -> Boolean))
(define (user-prim? x) (and (assq x list-of-user-primitives) #t))

(: system-prim? (Any -> Boolean))
(define (system-prim? x) (and (assq x list-of-system-primitives) #t))

(: predicate-prim? (Any -> Boolean))
(: value-prim? (Any -> Boolean))
(: effect-prim? (Any -> Boolean))
(define-values (predicate-prim? value-prim? effect-prim?)
  (let ()
    (define (make-prim-pred prim-type)
      (λ (x)
        (cond
          [(or (assq x list-of-user-primitives)
               (assq x list-of-system-primitives))
           => (lambda (a) (eq? (third a) prim-type))]
          [else #f])))
    (values (make-prim-pred 'test)
            (make-prim-pred 'value)
            (make-prim-pred 'effect))))

(: effect-free-prim? (Any -> Boolean))
(define (effect-free-prim? x)
  (and (primitive? x) (not (effect-prim? x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: list-of-user-primitives (Listof (List Symbol Natural PrimType)))
;; These are all the primitives supported by the compiler's input language.
;; (Note that set! is considered separately.)
(define list-of-user-primitives
  '(
   ; not is a special case
    (not 1 not)

   ; predicates
    (< 2 test)
    (<= 2 test)
    (= 2 test)
    (eq? 2 test)
    (zero? 1 test)

    (boolean? 1 test)
    (char? 1 test)
    (integer? 1 test)
    (procedure? 1 test)
    (vector? 1 test)
    (pair? 1 test)
    (cons? 1 test)
    (null? 1 test)
    (empty? 1 test)

   ; value-producing
    (+ 2 value)
    (- 2 value)
    (* 2 value)
    (add1 1 value)
    (sub1 1 value)

    (cons 2 value)
    (make-vector 1 value)

    (car 1 value)
    (cdr 1 value)
    (vector-length 1 value)
    (vector-ref 2 value)
    (char->integer 1 value)
    (void 0 value)

   ; side-effecting
    (vector-set! 3 effect)
   ))

(: list-of-system-primitives (Listof (List Symbol Natural PrimType)))
;; To be added later: primitives that will be used in the compiler's output.
(define list-of-system-primitives empty)

