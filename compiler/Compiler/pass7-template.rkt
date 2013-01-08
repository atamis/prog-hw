#lang racket

(require "support/helpers.rkt"
         "support/untyped-helpers.rkt"
         "support/env.rkt")

(provide pass7 remove-impure-letrec)

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))

#|
The goal of this pass is to simplify all letrecs so that they bind only
lambdas.  This kind of letrec is sometimes referred to as "fix", and it
can be compiled into simpler code than the mixed letrec that Scheme &
Racket permit.

The pass is trivial for everything except set!-, begin-, let-, lambda-, and
letrec-expressions, so I have provided the code for all forms other than
those.  Here is how a letrec-expression is processed:

 1) Process the body and RHS expressions recursively.
 2) Partition all the <Decl>s into several sets:
     * [x_u e_u] : Decls with unreferenced LHS
     * [x_s e_s] : "simple" decls (defined below)
     * [x_l e_l] : Decls whose RHS (e_l) is a lambda-expression
     * [x_c e_c] : "complex" decls (i.e., those which do not fall
                   into any of the above categories)
 3) Construct an expression by nesting lets and letrecs, as follows
    (omitting the (tag ...) wrappers here for clarity; they should be in
    the pass's actual output):

     (let ([x_s e_s] ...
           [x_c (void)] ...)
       (letrec ([x_l e_l] ...)
         (begin e_u ...
                (let ([x_tmp e_c] ...)
                  (begin (set! x_c x_tmp) ...)))
                new-body)))
    where x_t ... are fresh temporary variables, one per x_c.  The pass
    should avoid generating any begin-expression that is unnecessary, and
    avoid generating the inner let if there are no complex bindings. (Don't
    worry about going further than that in avoiding code generation; we'll
    clean up any binding-less lets or letrecs shortly.)

 4) Since we eliminate the bindings for unreferenced letrec-variables, we
    also will eliminate any assignments to unreferenced letrec-variables,
    transforming (set! x_u e) into e.

    As a "bonus" optimization, you might consider eliminating unreferenced
    let-variables, and assignments to them, as well; it's no harder, and
    you'll be carrying an environment of unreferenced variables anyway.

 5) Finally, after this pass we won't have further need for the lists of
    referenced variables, so we will replace <Tagged-Exp>s as well:

     (tag (asgd a1 a2 ...) (refd r1 r2 ...) body)

     ==> (settable (a1 a2 ...) body)

    (Yes, we're hanging on to the list of assigned variables; we'll need them
    in pass 8, after which we can throw away the wrapper. (Just don't litter.))

Some things you'll need to know for partitioning:

 * We can use the (tag ...) form we created earlier to determine
which variables are unreferenced and which are unassigned.
 * We'll consider bindings to be "simple" if there is no assignment to the
   LHS variable, and the RHS is one of the following:
    - a quoted literal
    - a reference to a variable bound somewhere else (not by this letrec)
    - an if-expr, begin-expr, or call to an effect-free primitive (which you
      can identify using the effect-free-prim? function in support/helpers.rkt)
      whose sub-expressions are all simple

So, your tasks are:

 * Write tests to cover all the clauses.
 * Complete a function simple? (template is given below) to test if an expr is
   simple.
 * Use the (trivial) function lambda? to test if an expr is a lambda-expr.
 * Complete remove-impure-letrec by writing the unfinished cases.

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

Note that in the previous pass, we introduced calls to error, therefore
error is now a primitive in the language.  Our implementation of error
here will be slightly different from Racket's, to compensate for our
lack of symbols and strings as data: it will take a list of chars and
halt execution, displaying that list as a string.

The output language is Lang6:

      <Prog> ::= (let () <Defs> <Exp>)

       <Exp> ::= (begin <NB-Exp> <NB-Exp>+)
               | <NB-Exp>

    <NB-Exp> ::= (quote <Lit>)
               | <Var>
               | (set! <Var> <Exp>)
               | (if <Exp> <Exp> <Exp>)
               | (let (<Decl>*) <Tagged-Exp>)
               | (letrec (<Proc-Decl>*) <Tagged-Exp>)  <- changed by this pass
               | (lambda (<Var>*) <Tagged-Exp>)
               | (<Prim> <Exp>*)
               | (<Exp> <Exp>*)

<Tagged-Exp> ::= (tag (asgd <Var>*) (refd <Var>*) <Exp>)

      <Decl> ::= (<Var> <Exp>)
 <Proc-Decl> ::= [<Id> (lambda (<Var>*) <Tagged-Exp>)]

|#


;; remove-impure-letrec : Lang5:Prog -> Lang6:Prog
;; as described above, simplifies letrec-expressions so they contain only 
;; lambdas as RHSs.
(define (remove-impure-letrec prog)
  
  ;; ril : Lang5:Exp Set[Id] -> Lang6:Exp
  ;; given an expr and the set of unreferenced vars currently
  ;; in scope, process the expr as described above.
  (define (ril x unrefd unasgd)
    (match x
      [(list 'begin e es ...)
       ]
      [_ (ril/nb x unrefd unasgd)]))
  
  ;; Lang5:NB-Exp Set[Id] -> Lang6:Exp
  (define (ril/nb x unrefd)
    (define (recur x) (ril x unrefd))
    (match x
      [(list 'quote lit) x]
      [(? symbol? x)     x]
      [(list 'set! (? symbol? id) rhs)
       #f]
      [(list 'if test e1 e2)
       `(if ,(recur test) ,(recur e1) ,(recur e2))]
      [(list 'let (list (and (list (? symbol? Ls) Rs) decls)
                        ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       #f]
      [(list 'letrec (list (and (list (? symbol? Ls) Rs) decls) ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       #f]
      [(list 'lambda (list (? symbol? formals) ...)
             (list 'tag (list 'asgd asgd-ids ...) (list 'refd refd-ids ...)
                   body))
       #f]
      [(list (or 'set! 'if 'let 'letrec 'lambda) anything-else ...)
       ;; This case is just to help you track down problems when testing.
       (error 'remove-impure-letrec
              "expected Lang5 program, but encountered ~s in input program ~s"
              x prog)]
      [(list (? effect-free-prim? prim) args ...)
       (cons prim (map recur args))]
      [(list (? primitive? prim) args ...)
       (cons prim (map recur args))]
      [(list proc args ...)
       (cons (recur proc) (map recur args))]
      [_
       (error 'remove-impure-letrec
              "expected Lang5 program, but encountered ~s in input program ~s"
              x prog)])

  ;; NEList[Id] NEList[Lang6:Exp] NEList[Id] -> Lang6:NB-Exp
  ;; constructs the let-expression that binds the temps and contains the set!s
  ;; for complex decls
  (define (make-complex-let Ls Rs temps)
    (if (empty? (cdr Ls))
        `(let ([,(first temps) ,(first Rs)])
           (settable () (set! ,(first Ls) ,(first temps))))
        `(let ,(map (lambda (t R) `[,t ,R]) temps Rs)
           (settable ()
             (begin ,@(map (lambda (L t) `(set! ,L ,t)) Ls temps))))))

  (wrap6 ,(ril (unwrap prog))))

;; simple? : Lang6:Exp Set[Id] -> Boolean
;; true iff e is simple with respect to the given set of vars from the LHS
;; of the letrec whose RHS e is a part of.
(define (simple? e Ls)
  (match e
    [(list 'quote lit)               'ack]
    [(? symbol? x)                   'ack]
    [(list 'set! (? symbol? id) rhs) 'ack]
    [(list 'begin e es ...)          'ack]
    [(list 'if e1 e2 e3)             'ack]
    [(list 'lambda formals settables/body)      'ack]
    [(list 'let decls settables/body)           'ack]
    [(list 'letrec decls settables/body)        'ack]
    [(list (? effect-free-prim? prim) args ...) 'ack]
    [(list (? primitive? prim) args ...)        'ack]
    [(list proc args ...)                       'ack]
    ))

;; lambda? : Lang6:Exp -> Boolean
;; true iff it's a lambda-expr
(define (lambda? x) (and (pair? x) (eq? (car x) 'lambda)))


(define pass7 remove-impure-letrec)
(define this-pass pass7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for wrapping & unwrapping output and input programs

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

;; wrap6: wrap an <Exp> of Lang6 in the (let () ...) form required by the
;; language.
(define-syntax-rule (wrap6 e)
  `(let ()
     (begin
       (define-syntax settable
         (syntax-rules ()
           [(_ (SVar (... ...)) Expr)
            Expr])))
     e))

;; unwrap : Lang5:Prog -> Lang5:Exp
(define (unwrap prog)
  (match prog
    [(list 'let '() stx-def e) e]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This time, I'm giving you some tests to get you started: several to
;; illustrate some of the basic behaviors, and one complex one that might be
;; trickier to get right.  (Bear in mind the caveat that you may have to
;; reorder decls in your tests, but only insofar as it doesn't change the
;; test program's semantics.)

(module+ test
  (compiler-tests/equal? remove-impure-letrec
    (,(wrap '5)               ,(wrap6 '5))
    (,(wrap '#f)              ,(wrap6 '#f))
    (,(wrap (begin '1 '2 '3)) ,(wrap6 (begin '1 '2 '3)))
    (,(wrap (if '#t '1 '2))   ,(wrap6 (if '#t '1 '2)))
    (,(wrap (let ([x '0])
              (tag (asgd x) (refd x)
                   (begin (set! x '23) x))))
     ,(wrap6 (let ([x '0]) (settable (x) (begin (set! x '23) x)))))
    (,(wrap (let ([x '0])
              (tag (asgd x) (refd)
                   (begin (set! x '23) '23))))
     ,(wrap6 (let () (settable () (begin '0 '23 '23)))))
    (,(wrap (let ([x '0]) (tag (asgd) (refd x) x)))
     ,(wrap6 (let ([x '0]) (settable () x))))
    (,(wrap (letrec ([x '0]) (tag (asgd) (refd x) x)))
     ,(wrap6 (let ([x '0]) (settable () (letrec () (settable () x))))))
    ; Test removal of let-bound unref'd vars and of set! on unreferenced vars:
    (,(wrap (let ([x '5])
              (tag (asgd x) (refd)
                   (begin (set! x '45) (set! x '46) '#t))))
     ,(wrap6 (let ()
               (settable () (begin '5 '45 '46 '#t)))))
    ; Test for proper flattening of begin sub-expressions:
    (,(wrap (let ([x '5]
                  [z '0])
              (tag (asgd x z) (refd z)
                   (begin (set! x (begin (set! z (add1 z))
                                         (set! z (add1 z))
                                         z))
                          (set! x (begin (set! z (add1 z))
                                         (set! z (add1 z))
                                         z))
                          (set! z (add1 z))
                          z))))
     ,(wrap6 (let ([z '0])
               (settable (z)
                 (begin '5
                        (set! z (add1 z))
                        (set! z (add1 z))
                        z
                        (set! z (add1 z))
                        (set! z (add1 z))
                        z
                        (set! z (add1 z))
                        z)))))
    ) ;; end of tests that can be decided w/o alpha-equivalent?.

  (compiler-tests/aeq remove-impure-letrec
    lang6
    ;; Imperative factorial -- with all types of bindings:
    (,(wrap (letrec ([c '5]
                     [unused '8]
                     [*n* '0]
                     [*acc* '100]
                     [!-help
                      (lambda ()
                        (tag (asgd) (refd)
                             (if (zero? *n*)
                                 '1
                                 (begin (set! *acc* (* *acc* *n*))
                                        (set! *n* (sub1 *n*))
                                        (!-help)))))]
                     [! (lambda (n)
                          (tag (asgd) (refd n)
                               (begin (set! *n* n)
                                      (set! *acc* '1)
                                      (!-help))))])
              (tag (asgd *n* *acc*) (refd c *n* *acc* !-help !)
                   (! c))))
     ,(wrap6 (let ([c '5]
                   [*n* (void)]
                   [*acc* (void)])
               (settable (*n* *acc*)
                 (letrec ([!-help (lambda ()
                                    (settable ()
                                      (if (zero? *n*)
                                          '1
                                          (begin (set! *acc* (* *acc* *n*))
                                                 (set! *n* (sub1 *n*))
                                                 (!-help)))))]
                          [! (lambda (n)
                               (settable ()
                                 (begin (set! *n* n)
                                        (set! *acc* '1)
                                        (!-help))))])
                   (settable ()
                     (begin '8
                            (let ([*n*0 '0]
                                  [*acc*0 '100])
                              (settable ()
                                (begin (set! *n* *n*0)
                                       (set! *acc* *acc*0))))
                            (! c))))))))
    )
  )

