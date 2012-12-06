#lang racket

(require "support/helpers.rkt"
         racket/set)

(provide pass5 uncover-assigned/referenced)

(module+ test
         (require rackunit
                  "support/compiler-testing.rkt"))

#|
Pass 4: uncover-assigned/referenced : Lang4 -> Lang5

Input language, Lang4:

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

This pass tags all let, letrec, and lambda expressions that bind variables
that will later be modified by set!, and all that bind variables that are
referenced.

In doing so it modifies the let, letrec, and lambda variants of <NB-Exp>:

<Tagged-Exp> ::= (tag (asgd <Var>*) (refd <Var>*) <Exp>)
<NB-Exp> ::= (let (<Decl>*) <Tagged-Exp>)
| (letrec (<Decl>*) <Tagged-Exp>)
| (lambda (<Var>*) <Tagged-Exp>)

When finished, the pass wraps the result in a syntax definition that strips
all but the <Exp> from a <Tagged-exp> when the pass output is evaluated.
This requires one more change to the output grammar:

<Prog> ::= (let () <Defs> <Exp>)

<Defs> hereafter (i.e., in this and future passes) will always refer to a
begin-expr containing definitions necessary to execute output programs.)

So, here is the entire output language, Lang5:

<Prog> ::= (let () <Defs> <Exp>)

<Exp> ::= (begin <NB-Exp> <NB-Exp>+)
| <NB-Exp>

<NB-Exp> ::= (quote <Lit>)
| <Var>
| (set! <Var> <Exp>)
| (if <Exp> <Exp> <Exp>)
| (let (<Decl>*) <Tagged-Exp>)
| (letrec (<Decl>*) <Tagged-Exp>)
| (lambda (<Var>*) <Tagged-Exp>)
| (<Prim> <Exp>*)
| (<Exp> <Exp>*)

<Tagged-Exp> ::= (tag (asgd <Var>*) (refd <Var>*) <Exp>)

<Decl> ::= (<Var> <Exp>)


Implementation advice:
* Read up on the racket/set library; set-subtract, set-union, set-add, and
set-intersect are all useful here, as is list->seteq.
* Develop a helper function to map uar over a list of Exps and take the
unions of all the asgd vars and all the refd vars.
* Be conscious of when you need a set in list form and when you need a
list in set form.
* When testing, remember that our use of sets may affect the ordering of
identifiers within each (asgd ...) and (refd ...) form in a <Tagged-Exp>.
|#

;; uncover-assigned/referenced : Lang4:Exp -> Lang5:Exp
;; As described above.
(define (uncover-assigned/referenced expr)
  (define (multi-uar lst [old-assg-vars (seteq)] [old-ref-vars (seteq)])
    (define (muar lst [old-assg-vars (seteq)] [old-ref-vars (seteq)])
      (for/fold
        ([output-expr empty]
         [assg-vars old-assg-vars]
         [ref-vars old-ref-vars])
        ([expr lst])
        (let-values ([(new-expr new-assg-vars new-ref-vars) (uar expr)])
          (values (cons new-expr output-expr)
                  (set-union new-assg-vars assg-vars)
                  (set-union new-ref-vars ref-vars)
                  )
          ))
      )
    (muar (reverse lst) old-assg-vars old-ref-vars)
    )


  ;; usr : Lang4:Exp -> Lang5:Exp Set[Id] Set[Id]
  ;; produce the Lang5 expression equivalent to x, and the sets of
  ;; assigned and referenced variables.
  (define (uar x)
    (match x
           [(list 'quote lit)
            (values x (seteq) (seteq))]
           [(? symbol? id)
            (values x (seteq) (seteq id))]
           [(list 'set! (? symbol? id) rhs)
            (let-values ([(new-expr assg-vars ref-vars) (uar rhs)])
              (values `(set! ,id ,new-expr)
                      (set-add assg-vars id)
                      ref-vars
                      ))]
           [(list 'if test e1 e2)
            (let-values ([(new-exprs assg-vars ref-vars)
                          (multi-uar (list test e1 e2))]
                         )
              (values
                (cons 'if new-exprs)
                assg-vars
                ref-vars
                )
              )]
           [(list 'begin e* ...)
            (let-values ([(new-exprs assg-vars ref-vars)
                          (multi-uar e*)]
                         )
              (values
                (cons 'begin new-exprs)
                assg-vars
                ref-vars
                )
              )
            ]
           [(list 'let (list (list (? symbol? Ls) Rs) ...) body)
            (let-values
              ([(Rs-expr rs-assg rs-refs) (multi-uar Rs)]
               [(new-expr assg-vars ref-vars ) (uar body )]
               )
              (values
                `(let
                   ,(map list Ls Rs-expr)
                   (tag
                     (asgd ,@(set->list (set-intersect assg-vars (list->seteq Ls))))
                     (refd ,@(set->list (set-intersect ref-vars (list->seteq Ls))))
                     ,new-expr
                     )
                   )
                (set-subtract (set-union assg-vars rs-assg) (list->seteq Ls))
                (set-subtract (set-union ref-vars rs-refs) (list->seteq Ls))
                )
              )
            ]
           ; The processing for letrec is similar to that for let, except
           ; that assigned vars occurring in binding expressions are
           ; matched with the letrec's bindings where possible, not
           ; automatically passed on to some enclosing level.
           [(list 'letrec (list (list (? symbol? Ls) Rs) ...) body)
            (let-values
              ([(Rs-expr rs-assg-vars rs-ref-vars) (multi-uar Rs)]
               [(new-expr assg-vars ref-vars ) (uar body )]
               )
              (values
                `(letrec
                   ,(map list Ls Rs-expr)
                   (tag
                     (asgd ,@(set->list (set-intersect assg-vars (list->seteq Ls))))
                     (refd ,@(set->list (set-intersect ref-vars (list->seteq Ls))))
                     ,new-expr
                     )
                   )
                (set-subtract (set-union assg-vars rs-assg-vars) (list->seteq Ls))
                (set-subtract (set-union ref-vars rs-ref-vars) (list->seteq Ls))
                )
              )
            ]
           [(list 'lambda (list (? symbol? formals) ...) body)
            (let-values ([(body-expr body-assg body-ref) (uar body)]
                         )
              (values
                `(lambda
                   ,formals
                   (tag
                     (asgd ,@(set->list (set-intersect body-assg (list->seteq formals))))
                     (refd ,@(set->list (set-intersect body-ref (list->seteq formals))))
                     ,body-expr
                     )
                   )
                (set-subtract body-assg (list->seteq formals))
                (set-subtract body-ref (list->seteq formals))
                )
              )
            ]

           [(list (? primitive? prim) args ...)
            (let-values ([(new-exprs new-assgs new-refs) (multi-uar args)])
              (values (cons prim args) new-assgs new-refs)
              )]
           [(list proc args ...)
            (multi-uar
              (cons proc args)
              )]
           [_
             (error 'uncover-assigned/referenced
                    "expected Lang4 expr, but encountered ~s in input program ~s"
                    x expr)]))

  (wrap
    ,(let-values ([(new-expr assigned-vars referenced-vars) (uar expr)])
       new-expr)))


(define pass5 uncover-assigned/referenced)
(define this-pass pass5)

(define-syntax-rule (wrap e)
                    `(let ()
                       (begin
                         (define-syntax tag
                           (syntax-rules (asgd refd)
                             [(_ (asgd SVar (... ...)) (refd RVar (... ...)) Expr)
                              Expr])))
                       e))


(module+ test
         (compiler-tests/equal?
           uncover-assigned/referenced
           ('5  ,(wrap '5))
           ('#f ,(wrap '#f))
           ('x  ,(wrap 'x))
           ((set! x '5) ,(wrap (set! x '5)))
           ((let ([x '5])
              x)

            ,(wrap (let ([x '5])
                     (tag (asgd) (refd x) x)
                     ))
            )

           ((let ([x '5])
              (set! x '6))

            ,(wrap (let ([x '5])
                     (tag (asgd x) (refd)
                          (set! x '6))
                     ))
            )


           ((let ([x '5])
              (let ([y '7])
                (+ y x)))

            ,(wrap (let ([x '5])
                     (tag (asgd) (refd x)
                          (let ([y '7])
                            (tag (asgd) (refd y)
                                 (+ y x))
                            )
                          )))
            )

           ((let ([z '10])
              (letrec ([x '5]
                       [y (set! x '6)]) ; TODO: make into a letrec definition.
                (+ y x)))

            ,(wrap
               (let ([z '10])
                 (tag (asgd) (refd)
                      (letrec ([x '5]
                               [y (set! x '6)])
                        (tag (asgd) (refd x y)
                             (+ y x))
                        ) )
                 ))
            )

           (
            (lambda (x)
              x
              )

            ,(wrap
               (lambda (x)
                 (tag
                   (asgd)
                   (refd x)
                   x
                   )
                 )
               )
            )

           (
            (let ([x '6])
              (lambda (x)
                (set! x '5)
                ))

            ,(wrap
               (let ([x '6])
                 (tag
                   (asgd)
                   (refd)
                   (lambda (x)
                     (tag
                       (asgd x)
                       (refd)
                       (set! x '5)
                       )
                     )))
               )
            )

           (
            (let ([y '6])
              (lambda (x)
                (set! y '5)
                ))

            ,(wrap
               (let ([y '6])
                 (tag
                   (asgd y)
                   (refd)
                   (lambda (x)
                     (tag
                       (asgd y)
                       (refd)
                       (set! y '5)
                       )
                     )))
               )
            )

           ((let ([z '10])
              (let ([x '5]
                    [y (set! z '6)])
                (+ y x)))

            ,(wrap
               (let ([z '10])
                 (tag (asgd z) (refd)
                      (let ([x '5]
                            [y (set! z '6)])
                        (tag (asgd) (refd x y)
                             (+ y x))
                        ) )
                 ))
            )


           ((let ([z '10])
              (let ([x '5]
                    [y (set! z '6)])
                (+ y x z)))

            ,(wrap
               (let ([z '10])
                 (tag (asgd z) (refd z)
                      (let ([x '5]
                            [y (set! z '6)])
                        (tag (asgd) (refd x y)
                             (+ y x z))
                        ) )
                 ))
            )

           ((letrec ([z '10])
              (letrec ([x '5]
                       [y (set! z '6)])
                (+ y x z)))

            ,(wrap
               (letrec ([z '10])
                 (tag (asgd z) (refd z)
                      (letrec ([x '5]
                               [y (set! z '6)])
                        (tag (asgd) (refd x y)
                             (+ y x z))
                        ) )
                 ))
            )
           ))


