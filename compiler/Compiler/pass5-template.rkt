#lang racket

(require "support/helpers.rkt")

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
       (error 'usr "unimplemented")]
      [(list 'if test e1 e2)
       (error 'usr "unimplemented")]
      [(list 'begin e* ...)
       (error 'usr "unimplemented")]
      [(list 'let (list (list (? symbol? Ls) Rs) ...) body)
       (error 'usr "unimplemented")]
      ; The processing for letrec is similar to that for let, except
      ; that assigned vars occurring in binding expressions are
      ; matched with the letrec's bindings where possible, not
      ; automatically passed on to some enclosing level.
      [(list 'letrec (list (list (? symbol? Ls) Rs) ...) body)
       (error 'usr "unimplemented")]
      [(list 'lambda (list (? symbol? formals) ...) body)
       (error 'usr "unimplemented")]
      [(list (? primitive? prim) args ...)
       (error 'usr "unimplemented")]
      [(list proc args ...)
       (error 'usr "unimplemented")]
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
  (compiler-tests/equal? uncover-assigned/referenced
    ('5  ,(wrap '5))
    ('#f ,(wrap '#f))
    ))