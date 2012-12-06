#lang racket

(require "support/helpers.rkt"
         "support/env.rkt")

(provide pass6 enforce-letrec-rule)

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))

#|

An introduction:

Scheme's specification of letrec imposes this essential restriction:

  It must be possible to evaluate each of the expressions e_1 ... e_n
  in (letrec ([x_1 e_1] ... [x_n e_n]) body ...) without evaluating a
  reference or assignment to any of the variables x_1 ... x_n.

(References/assignments to those variables may occur in e_1 ... e_n, but they
may not be evaluated until after control has entered the body of the letrec.)
We'll call this "the letrec rule."  It is intended to prevent fragile code
like these two expressions:
  (letrec ([x 1] [y (+ x 1)]) (list x y))
  (letrec ([y (+ x 1)] [x 1]) (list x y))
It *seems* they should evaluate to '(1 2), but given that the order of
evaluation for the two <Decl>s in the letrec isn't specified, it's possible
for x to be undefined when (+ x 1) is evaluated.  So this is a violation of the
letrec rule, and as language implementors we should detect it and warn the
programmer rather than try to guess the intended meaning.

The *next* pass we write is going to perform an optimization by moving code
around, such that we won't necessarily be able to tell if the letrec rule is
being violated, so in *this* pass we will insert checks that will let the
runtime system tell if the letrec rule is being violated.

We could insert an explicit check for the undefined/void value before each
reference and assignment in the RHS expressions of the letrec, but that would
produce many more checks than we need, and would also obfuscate the code such
that it would be impossible to perform the optimizations we'll be implementing.
Instead, we have a better solution based on two observations:
  1) We can use a boolean flag to indicate whether a letrec-variable may be
     touched.
  2) We need only one such flag for each letrec-expression in the program.
|#

;; So, our transformation of letrec looks roughly like this:
#;#;#;
(letrec ([x_1 e_1] ... [x_n e_n]) body)
==> 
(let ([valid? #f])   ;; where valid? is a fresh identifier
  (letrec ([x_1 (recur-on e_1)]
           ...
           [x_n (recur-on e_n)])
    (begin (set! valid? #t)
           (recur-on body))))
#|
Validity checks can take the place of variable refs, looking like this:
x ==> (begin (if valid? (void) (error "detected letrec violation"))
             x)

The conversion for assignments is similar.

Our implementation will insert validity checks only where needed, and if none
are determined to be needed, it will not introduce the valid? flag. The
difficulty is, of course, in telling where the checks are needed. Consider:
|#
#;
(letrec ([x 0]
         [f (let ([g (lambda () x)])
              g)])
  <body>)
;; versus:
#;
(letrec ([x 0]
         [f (let ([g (lambda () x)])
              (h g))]) ;; where h is defined elsewhere
  <body>)
#|
In the first, the references to both g and x are safe, since the thunk is not
invoked. In the second, we don't know if h will invoke g, so we must check the
reference to x.

Here is our strategy; much more detail is in Sec. 2.3 of the paper on the
algorithm, "Fixing Letrec: A Faithful Yet Efficient Implementation of Scheme's
Recursive Binding Construct," by Waddell, Dybvig, and Sarkar.

Variables have three states: protected, unprotected, and protectable.  The
state is tracked via extra arguments to the recursion: a list of unprotected
variables, and a list of protectable variables. (Any in neither list are
considered protected.)  Here is how the state transitions work:

* All letrec-variables begin in the protectable state when we begin processing
  a RHS expression from the letrec that binds them.

* We move all protectable vars into the protected state when entering a lambda-
  expression.

* We move all protectable vars into the unprotected state when entering an
  unsafe context.

* We do not change the state of any var when entering a safe context (that is
  not a lambda-expression).

This is how the variable states determine whether to insert checks around
variable refs & assignments:  an "unsafe use" is a ref or assignment to a
variable that is unprotected or protectable.  Checks are inserted around all
unsafe uses.

Whether the right side of a let or letrec is safe (for protectable variables
bound outside the let or letrec) depends on whether the left-hand-side
variables are referenced in the body of the letrec.  This means we have to
check the body first.  So, we process the body and then partition the LHS
variables according to whether they have any unsafe uses in the body.  Each
RHS is considered unsafe if its corresponding LHS variable is used unsafely
in the body, and safe otherwise.

The input language is Lang5:

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

The output language is the same; the program structure still fits the grammar
of Lang5.
|#

;; In the below, a Flag is an identifier (i.e., symbol) representing a Boolean
;; valid? flag variable.

;; enforce-letrec-rule : Lang5:Prog -> Lang5:Prog
(define (enforce-letrec-rule prog)
  
  ;; elr : Exp Set[Id] Set[Id] Env[Id => Id] --> Exp Setof[Id] Setof[Flag]
  ;; x is the expression to be processed; unprotected and protectable are
  ;; sets of letrec-bound variables; and flags is an env mapping letrec-bound
  ;; variables to their valid? flags.
  ;; This function produces three values:
  ;;   * the new expression
  ;;   * the set of all variables that were referenced/assigned unsafely (i.e.,
  ;;     while unprotected or protectable) in the expression
  ;;   * the set of flags that were referenced (for determining whether each
  ;;     letrec needs the flag generated).
  (define (elr x unprotected protectable flags)
    (match x
      [(list 'begin e es ...)
       (error "unimplemented")]
      [_ (elr/nb x unprotectable protectable flags)]))
  
  ;; elr/nb : Exp Set[Id] Set[Id] Env[Id => Id] --> Exp Setof[Id] Setof[Flag]
  (define (elr/nb x unprotectable protectable flags)
    (match x
      [(list 'quote lit) (error "unimplemented")]
      [(? symbol? x) (error "unimplemented")]
      [(list 'set! (? symbol? id) rhs)
       (error "unimplemented")]
      [(list 'if test e1 e2)
       (error "unimplemented")]
      [(list 'let (list (list (? symbol? Ls) Rs) ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       (error "unimplemented")]
      [(list 'letrec (list (list (? symbol? Ls) Rs) ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       (error "unimplemented")]
      [(list 'lambda (list (? symbol? formals) ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       (error "unimplemented")]
      [(list (? primitive? prim) args ...)
       (error "unimplemented")]
      [(list proc args ...)
       (error "unimplemented")]
      [_
       (error 'enforce-letrec-rule
              "expected Lang5 program, but encountered ~s in input program ~s"
              x prog)]))
  
  ;; check : Exp Flag -> Expr
  ;; wrap a check around the given variable reference or assignment, using the
  ;; given flag.
  (define (check e valid?)
    `(begin (if ,valid? (void) (error "detected letrec violation")) ,e))
  
  ;; map/union :
  ;;    (W X Y Z -> W' Seteq[Id] Seteq[Id]) Listof[W]
  ;;    --> Listof[W'] Seteq[Id] Seteq[Id]
  ;; map f over xs, returning the list of all first return values and the
  ;; unions of all second and of all third return values.
  (define (map/union f xs)
    (for/fold ([new-xs '()][ids1 (seteq)][ids2 (seteq)]) ([x (reverse xs)])
      (let-values ([(new-x s1 s2) (f x)])
        (values (cons new-x new-xs) (set-union ids1 s1) (set-union ids2 s2)))))
  
  ;; unwrap : Lang5:Prog -> Lang5:Exp
  (define (unwrap prog)
    (match prog
      [(list 'let '() stx-def e) e]))
  (elr (unwrap prog) empty empty (empty-env)))


;; wrap: wrap an <Exp> of Lang5 in the (let () ...) form required by the
;; language.
(define-syntax-rule (wrap e)
  `(let ()
     (begin
       (define-syntax tag
         (syntax-rules (asgd refd)
           [(_ (asgd SVar (... ...)) (refd RVar (... ...)) Expr)
            Expr])))
     e))

(define pass6 enforce-letrec-rule)
(define this-pass pass6)

(module+ test
  (compiler-tests/equal? enforce-letrec-rule
    (,(wrap '5) ,(wrap '5))
    ))

#;(let ([f '#f])
    (letrec ([x '0]
             [y (begin (set! f (lambda () (add1 x)))
                       (f))])
      (list x y)))