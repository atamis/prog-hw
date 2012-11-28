#lang racket

(require "support/helpers.rkt"
         racket/trace)

(provide pass3 flatten-begin)

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))

#|
(define-struct box (b))

; Takes a list and a predicate and recusively boxes all the sublists that
; matches the predicate.
(define (box-pred lst pred)
  (cond
    [(empty? lst) empty]
    [(pred (car lst)) (cons (box (car lst))
                            (box-pred (cdr lst) pred))
      ]
    [else
      (cons (if (list? (car lst))
              (box-pred (car lst) pred)
              (car lst))
            (box-pred (cdr lst) pred))
      ]
    )
  )

(define (unbox lst)
  (cond
    [(empty? lst) empty]
    [(list? lst)
     (map unbox lst)
     ]
    [else
      (cons
        (if (box? (car lst))
          (box-b (car lst))
          lst
          )
        (unbox (cdr lst))
        )
      ]
    )
  )

(print '(begin (begin 4 5 6) 7 (55 60) (begin 8 (begin 9 0))))
(newline)

(unbox (box-pred '(begin (begin 4 5 6) 7 (55 60) (begin 8 (begin 9 0)))
          (lambda (x) (not (and (list? x) (eq? (car x) 'begin))))))

|#

#|
Pass 3: flatten-begin : Lang2 -> Lang3

This pass removes nested begin-expressions, and removes unary
begin-expressions; in its output, a begin-expression contains two or more
subexpressions, neither of which may also be a begin expression.  For
example,

       (begin (begin 4 5 6) 7 (begin 8 (begin 9 0)))
              ==> (begin 4 5 6 7 8 9 0)
and
      (begin 6)
              ==> 6

Input language, Lang2:

<Exp>  ::= <Const>
         | (quote <Lit>)
         | <Var>
         | (set! <Var> <Exp>)
         | (if <Exp> <Exp> <Exp>)
         | (begin <Exp> <Exp>*)     <--  modified by this pass.
         | (let (<Decl>*) <Exp>)
         | (letrec (<Decl>*) <Exp>)
         | (lambda (<Var>*) <Exp>)
         | (<Prim> <Exp>*)
         | (<Exp> <Exp>*)
<Decl> ::= (<Var> <Exp>)

The output language grammar, Lang3, is:

    <Exp> ::= (begin <NB-Exp> <NB-Exp>+)
           | <NB-Exp>

 <NB-Exp> ::= <Const>
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

|#

(define (flatten-begin expr)
  (define (fb x)
    (match x
      [(? constant? c)   x]
      [(list 'quote lit) x]
      [(? symbol? id)    x]
      [(list 'set! (? symbol? lhs) rhs)
       `(set ,lhs ,(fb rhs))
       ]
      [(list 'if test e1 e2)
       `(if ,(fb test) ,(fb e1) ,(fb e2))
        ]
      [(list 'begin e1)
       (fb e1)
       ]
      [(list 'begin e1 e* ...)
       (let ([exprs (foldl
                      (lambda (nb-expr accum)
                        (if (and (list? nb-expr)
                                 (eq? 'begin (car nb-expr))
                                 )
                          (append (cdr nb-expr) accum)
                          (cons nb-expr accum))
                        )
                      empty
                      (reverse (map flatten-begin (cons e1 e*))))])
         (cons 'begin exprs)
         )
       ]
      #;[(list 'let (list (list (? symbol? lhs) rhs) ...) body)
       ...]
      #;[(list 'letrec (list (list (? symbol? lhs) rhs) ...) body)
       ...]
      #;[(list 'lambda (list (? symbol? formals) ...) body)
       ...]
      #;[(list (? primitive? prim) args ...)
       ...]
      #;[(list proc args ...)
       ...]
      #;[something-else
       (error 'flatten-begin
              (string-append
               "found ~s in processing input expr ~s"
               " (likely the prior pass's fault!)")
              x
              expr)]))
  ;; entry point:
  (fb expr))

#;(trace myflat)

(flatten-begin '(begin (begin 4 5 6) 7 (if #t 55 60) (begin 8 (begin 9 0))))

(define pass3 flatten-begin)
(define this-pass pass3)

(module+ test
  
  (compiler-tests/equal? flatten-begin
    (5                     5)
    ((if #t 5 6)           (if #t 5 6))
    ((begin 5 5)            (begin 5 5))
    ((begin 5 (begin 6 7))            (begin 5 6 7))
    ; TO DO: add your tests here.
    ))
