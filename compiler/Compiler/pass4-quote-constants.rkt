#lang racket

(require "support/helpers.rkt")

(provide pass4 quote-constants)

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))

#|
Pass 4: quote-constants : Lang3 -> Lang4

Input language, Lang3:

    <Exp> ::= (begin <NB-Exp> <NB-Exp>+)
           | <NB-Exp>

 <NB-Exp> ::= <Const>            <-- removed by this pass
           | (quote <Lit>)
           | <Var>
           | (set! <Var> <Exp>)
           | (if <Exp> <Exp> <Exp>)
           | (let (<Decl>*) <Exp>)
           | (letrec (<Decl>*) <Exp>)
           | (lambda (<Var>*) <Exp>)
           | (<Prim> <Exp>*)
           | (<Exp> <Exp>*)

  <Decl> ::= (<Var> <Exp>)

Output language, Lang4:

    <Exp> ::= (begin <NB-Exp> <NB-Exp>+)
           | <NB-Exp>

 <NB-Exp> ::= (quote <Lit>)
           | <Var>
           | (set! <Var> <Exp>)
           | (if <Exp> <Exp> <Exp>)
           | (let (<Decl>*) <Exp>)
           | (letrec (<Decl>*) <Exp>)
           | (lambda (<Var>*) <Exp>)
           | (<Prim> <Exp>*)
           | (<Exp> <Exp>*)

  <Decl> ::= (<Var> <Exp>)
|#

;; quote-constants : Lang3:Exp -> Lang4:Exp
(define (quote-constants expr)
  (define (qc x) ; Lang3:Exp -> Lang4:Exp
    (match x
      [(list 'begin e1 e2) `(begin ,(qcnb e1) ,(qcnb e2))]
      [_ (qcnb x)]))
  
  (define (qcnb x) ; Lang3:NB-Exp -> Lang4:NB-Exp
    (match x
      [(? constant? c)   (list 'quote c)]
      [(list 'quote lit) x]
      [(? symbol? id)    id]
      [(list 'set! (? symbol? lhs) rhs)
       `(set! ,lhs ,(qc rhs))]
      [(list 'if test e1 e2)
       `(if ,(qc test) ,(qc e1) ,(qc e2))]
      [(list 'let (list (list (? symbol? lhs) rhs) ...) body)
       `(let ,(map (λ (L R) (list L (qc R))) lhs rhs) ,(qc body))]
      [(list 'letrec (list (list (? symbol? lhs) rhs) ...) body)
       `(letrec ,(map (λ (L R) (list L (qc R))) lhs rhs)
          ,(qc body))]
      [(list 'lambda (list (? symbol? formals) ...) body)
       `(lambda ,formals ,(qc body))]
      [(list (? primitive? prim) args ...)
       `(,prim ,@(map qc args))]
      [(list proc args ...)
       (map qc (cons proc args))]
      [_ (error 'quote-constants
                "expected a Lang3, but found ~s in processing input expr ~s"
                x expr)]))
  (qc expr))

(define pass4 quote-constants)
(define this-pass pass4)

(module+ test
  
  (compiler-tests/equal? quote-constants
    (5                     '5)
    ('5                    '5)
    (#f                    '#f)
    ('#f                   '#f)
    ((if #t 5 6)           (if '#t '5 '6))
    ((if #t (if #f 7 8) 6) (if '#t (if '#f '7 '8) '6))
    ((let ([x 0])  (begin (set! x (add1 x)) x))
     (let ([x '0]) (begin (set! x (add1 x)) x)))
    ((begin 1 2 3) (begin '1 '2 '3))
    ((let ([a 1][b 2]) (begin a b a)) (let ([a '1][b '2]) (begin a b a)))
    ((let ([a 1]) a)
     (let ([a '1]) a))
    ((let ([a 1] [b 2])  (begin (set! b  4) (+ a b)))
     (let ([a '1][b '2]) (begin (set! b '4) (+ a b))))
    ((letrec ([! (lambda (n) (if (= n  0)  1 (* n (! (- n  1)))))])
       (+ (!  5)  1))
     (letrec ([! (lambda (n) (if (= n '0) '1 (* n (! (- n '1)))))])
       (+ (! '5) '1)))
    ((letrec ([f (lambda (n) (f n))])
       (begin (set! f (lambda (n) (+  1 n))) (set! f (lambda (n) (+  2 n)))
              (f  5)))
     (letrec ([f (lambda (n) (f n))])
       (begin (set! f (lambda (n) (+ '1 n))) (set! f (lambda (n) (+ '2 n)))
              (f '5))))
    ((let ([! (void)][x 5])
       (begin (set! ! (lambda (n) (if (= n  0)  1 (* n (! (- n  1))))))
              (set! x (+  1 x))
              (! x)))
     (let ([! (void)][x '5])
       (begin (set! ! (lambda (n) (if (= n '0) '1 (* n (! (- n '1))))))
              (set! x (+ '1 x))
              (! x))))
    ((cons  5 (cons  6 (cons  7 (cons  8  ()))))
     (cons '5 (cons '6 (cons '7 (cons '8 '())))))
    ))