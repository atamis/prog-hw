#lang racket

(require "support/helpers.rkt")

(provide pass2 remove-implicit-begin/when/unless)

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))


#|
Pass 2: remove-implicit-begin/when/unless

Input language is Lang1, as defined in Pass 1:

<Exp>  ::= <Const>
         | (quote <Lit>)
         | <Var>
         | (set! <Var> <Exp>)
         | (when <Exp> <Exp>)              <-- removed in Lang2
         | (unless <Exp> <Exp>)            <-- removed in Lang2
         | (if <Exp> <Exp> <Exp>)
         | (begin <Exp> <Exp>*)
         | (let (<Decl>*) <Exp> <Exp>*)    <-- changed in Lang2
         | (letrec (<Decl>*) <Exp> <Exp>*) <-- changed in Lang2
         | (lambda (<Var>*) <Exp> <Exp>*)  <-- changed in Lang2
         | (<Prim> <Exp>*)
         | (<Exp> <Exp>*)
<Decl> ::= (<Var> <Exp>)

This pass adds explicit (begin ...) wrappings around all expressions where
the (begin ...) is implied, and converts every when- and unless-expression
into an if-expression with a (void) third sub-expression (i.e., (void) for
the "else" case).  It should NOT add a (begin ...) where a let/letrec/λ has
only one body expression.

Output language, Lang2:

<Exp>  ::= <Const>
         | (quote <Lit>)
         | <Var>
         | (set! <Var> <Exp>)
         | (if <Exp> <Exp> <Exp>)
         | (begin <Exp> <Exp>*)
         | (let (<Decl>*) <Exp>)
         | (letrec (<Decl>*) <Exp>)
         | (lambda (<Var>*) <Exp>)
         | (<Prim> <Exp>*)
         | (<Exp> <Exp>*)
<Decl> ::= (<Var> <Exp>)
|#

;; remove-implicit-begin/when/unless : Lang1 -> Lang2
(define (remove-implicit-begin/when/unless expr)
  ;; simplify : Lang1 -> Lang2
  ;; main recursive helper, named for brevity
  (define (simplify x)
    (match x
      [(? constant? c)   x]
      [(list 'quote lit) x]
      [(? symbol? id)    x]
      [(list 'set! (? symbol? lhs) rhs)
       `(set! ,lhs ,(simplify rhs))]
      [(list 'when test result)
       `(if ,(simplify test)
          ,(simplify result)
          (void))
       ]
      [(list 'unless test result)
       `(if ,(not (simplify test))
          ,(simplify result)
          (void))
       ]
      [(list 'if test e1 e2)
       `(if ,(simplify test) ,(simplify e1) ,(simplify e2))]
      [(list 'begin e1 e* ...)
       `(begin ,@(map simplify (cons e1 e*)))
       ]
      [(list 'let (list (list (? symbol? lhs) rhs) ...) b bs ...)
       `(let ,(map simplify-decl lhs rhs)
          ,(simplify/maybe-wrap b bs))]
      [(list 'letrec (list (list (? symbol? lhs) rhs) ...) b bs ...)
       `(let ,(map simplify-decl lhs rhs)
          ,(simplify/maybe-wrap b bs))
       ]
      [(list 'lambda (list (? symbol? formals) ...) b bs ...)
       `(lambda ,(map simplify-decl formals)
          ,(simplify/maybe-wrap b bs))
       ]
      [(list (? primitive? prim) args ...)
       (cons prim (map simplify args))]
      [(list proc args ...)
       (cons (simplify proc) (map simplify args))]
      [something-else
       (error 'remove-implicit-begin/when/unless
              (string-append
               "found ~s in processing input expr ~s"
               " (likely the prior pass's fault!)")
              x
              expr)]))
  ;; simplify-decl : Id Lang1 -> Lang2-Decl
  ;; helper for simplifying the RHS of a <Decl> from a let or letrec.
  (define (simplify-decl lhs rhs)
    `(,lhs ,(simplify rhs))) ;; TO DO
  
  ;; Lang1 Listof[Lang1] -> Lang2
  ;; since we don't know if the body need to be wrapped in a begin, this
  ;; helper simplifies the given bodies, decides if (begin ...) is necessary,
  ;; and wraps the bodies with it if so.
  (define (simplify/maybe-wrap b bs)
    (if (empty? bs)
      b
      (cons 'begin (cons b bs)))
    ) ;; TO DO
  
  ;; entry point:
  (simplify expr))

;; Aliases:
(define pass2 remove-implicit-begin/when/unless)
(define this-pass pass2)

(module+ test
  
  (compiler-tests/equal? remove-implicit-begin/when/unless
    (5                     5)
    ((if #t 5 6)           (if #t 5 6))
    ((let ([x 0])
       (set! x (add1 x))
       x)
     (let ([x 0])
       (begin (set! x (add1 x))
         x)))
    ((let ([x 0])
       x)
     (let ([x 0])
         x))
    ; TO DO: add your tests here.
    ))
