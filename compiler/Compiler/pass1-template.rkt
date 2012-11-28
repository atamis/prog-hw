#lang racket

(require "support/env.rkt" "support/helpers.rkt")

(module+ test
  (require rackunit
           "support/compiler-testing.rkt"))

(define (make-new-names names env)
  (extend-env names
              (map gensym names)
              #;(build-list
                (length names)
                (lambda (x) (gensym)))
              env))

(define (reverse-lookup b env)
  (lookup b (map (lambda (x) (cons (cdr x) (car x))) env)))

;;; Pass 1: rename-vars/verify-legal
;
; Input and output language, Lang1:
;
; <Exp>  ::= <Const>
;          | (quote <Lit>)
;          | <Var>
;          | (set! <Var> <Exp>)
;          | (when <Exp> <Exp>)
;          | (unless <Exp> <Exp>)
;          | (if <Exp> <Exp> <Exp>)
;          | (begin <Exp> <Exp>*)
;          | (let (<Decl>*) <Exp> <Exp>*)
;          | (letrec (<Decl>*) <Exp> <Exp>*)
;          | (lambda (<Var>*) <Exp> <Exp>*)
;          | (<Prim> <Exp>*)
;          | (<Exp> <Exp>*)
; <Decl> ::= (<Var> <Exp>)
;
; The list of acceptable primitives (<Prim>) is described above in the
; definition of list-of-user-primitives; constants (<Const>), recognized by the
; procedure constant?, include the empty list, characters, booleans, and
; integers.
;
; This pass ensures that an input program is legitimate (insofar as it matches
; the grammar and contains no references OR assignments to unbound identifiers)
; and renames all variables so that each variable is uniquely named.

;; rename-vars/verify-legal : Lang1 -> Lang1
;; Replaces all identifiers so every variable is uniquely named within the
;; program, and verifies that the form of the program is valid, per the above.
(define (rename-vars/verify-legal expr)
  ;; rv/vl : S-expr Env[Sym => Sym] -> Lang1
  ;; Replaces all identifiers so every variable is uniquely named within the
  ;; program, and verifies that the form of the program is valid.  env maps
  ;; ids to renamed ids.
  (define (rv/vl x env)
    #|(print x)(newline)
    (print env)(newline)(newline)|#
    (match x
      [(? constant? x) x]
      [(list 'quote x) x]
      [(? symbol? x)
        (let ([lookup-result (lookup x env)])
          (if lookup-result
            lookup-result
              (raise (make-exn:fail:contract:variable
                     (format "(rv/vl) Unbound identifier: ~s" x)
                     (current-continuation-marks)
                     x))))
       ]
      [(list 'set! (? symbol? lhs) rhs)
       `(set! ,(rv/vl lhs env) ,(rv/vl rhs env))
       ]
      [(list 'when test result)
       `(when ,(rv/vl test env) ,(rv/vl result env))
       ]
      [(list 'unless test result)
       `(unless ,(rv/vl test env) ,(rv/vl result env))
       ]
      [(list 'if test e1 e2)
       `(if ,(rv/vl test env) ,(rv/vl e1 env) ,(rv/vl e2 env))
       ]
      [(list 'begin e1 e* ...)
       `(begin
             ,@(map (lambda (e-n)
                    ; This might be a problem, but you can't just make
                    ; bindings with set, so env won't be shared between
                    ; begin branches.
                    (rv/vl e-n env))
                  (cons e1 e*))
             )
       ]
      [(list 'let (list (list (? symbol? lhs) rhs) ...) e1 e* ...)
       (let ([new-env (make-new-names lhs env)])
         `(let ,(map list (map (lambda (x) (lookup x new-env)) lhs)
                          (map (lambda (x) (rv/vl x env)) rhs))
             ,@(map (lambda (e-n)
                    ; This might be a problem, but you can't just make
                    ; bindings with set, so env won't be shared between
                    ; begin branches.
                    (rv/vl e-n new-env))
                     (cons e1 e*))
             )
         )
       ]
      [(list 'letrec (list (list (? symbol? lhs) rhs) ...) e1 e* ...)
       (let ([new-env (make-new-names lhs env)])
         `(letrec ,(map list (map (lambda (x) (lookup x new-env)) lhs)
                          (map (lambda (x) (rv/vl x new-env)) rhs))
             ,@(map (lambda (e-n)
                    ; This might be a problem, but you can't just make
                    ; bindings with set, so env won't be shared between
                    ; begin branches.
                    (rv/vl e-n new-env))
                     (cons e1 e*))
             )
         )
       ]
      [(list 'lambda (list (? symbol? formals) ...) e1 e* ...)
       (let ([new-env (make-new-names formals env)])
        `(lambda ,(map (lambda (e-n) (rv/vl e-n new-env)) formals)
             ,@(map (lambda (e-n) (rv/vl e-n new-env)) (cons e1 e*))))
       ]
      [(list (? primitive? prim) args ...)
       `(,prim ,@(map (lambda (x) (rv/vl x env)) args))
       ]
      [(list proc args ...)
         (list (rv/vl proc env)
               (first (map (lambda (x) (rv/vl x env)) args)))
       ]
      [something-else
       (raise (make-exn:fail
               (if (eq? something-else expr)
                   (format "(rv/vl) malformed input program ~s" something-else)
                   (format "(rv/vl) malformed expr ~s in input program:\n~s"
                           something-else expr))
               (current-continuation-marks)))]))
  (rv/vl expr (empty-env)))

;; alpha-equivalent? : Lang1 Lang1 -> Lang1
;; determines whether the two given programs are equivalent up to renaming
;; of bound variables.
(define (alpha-equivalent? e1 e2)
  ;; Lang1 Lang1 Env[Sym => Sym] -> Boolean
  ;; env maps bound variables of e1 to bound variables of e2.
  (define (aeq? e1 e2 env)
    #|(print e1)(newline)
    (print e2)(newline)
    (print env)(newline)(newline)|#
    (match (list e1 e2)
      [(list (? constant? x)
             (? constant? y))
       (eq? x y)
       ]
      [(list (list 'quote x)
             (list 'quote y))
       (eq? x y)
       ]
      [(list (? symbol? x)
             (? symbol? y))
       (if (and (not (lookup x env)) (not (lookup y env)))
         false ; Unbound identifier error here?
         (or (eq? x y)
             (eq? (lookup x env) y)
             (eq? x (lookup y env)))
         )
       ]
      [(list (list 'set! (? symbol? lhs1) rhs1)
             (list 'set! (? symbol? lhs2) rhs2))
       (and (aeq? lhs1 lhs2 env)
            (aeq? rhs1 rhs2 env))
       ]
      [(list (list 'when test1 result1)
             (list 'when test2 result2))
       (and (aeq? test1 test2 env)
            (aeq? result1 result2 env))
       ]
      [(list (list 'unless test1 result1)
             (list 'unless test2 result2))
       (and (aeq? test1 test2 env)
            (aeq? result1 result2 env))
       ]
      [(list (list 'if test1 e11 e21)
             (list 'if test2 e12 e22))
       (and (aeq? test1 test2 env)
            (aeq? e11 e12 env)
            (aeq? e21 e22 env))
       ]
      [(list (list 'begin e11 e1* ...)
             (list 'begin e12 e2* ...))
       (andmap {lambda (expr1 expr2) (aeq? expr1 expr2 env)}
               (cons e11 e1*)
               (cons e12 e2*))
       ]
      [(list (list 'let (list (list (? symbol? lhs1) rhs1) ...)
                   e11 e1* ...)
             (list 'let (list (list (? symbol? lhs2) rhs2) ...)
                   e12 e2* ...))
       (if (not (=
                 (length (cons e11 e1*))
                 (length (cons e12 e2*))))
         false
         (let ([new-env (extend-env lhs1 lhs2 env)])
           (andmap {lambda (expr1 expr2) (aeq? expr1 expr2 new-env)}
                   (cons e11 e1*)
                   (cons e12 e2*))
           ))
         
       ]
      #;[(list 'letrec (list (list (? symbol? lhs) rhs) ...) e1 e* ...)
       ...]
      #;[(list 'lambda (list (? symbol? formals) ...) e1 e* ...)
       ...]
      #;[(list (? primitive? prim) args ...)
       ...]
      [(list (list proc1 args1 ...)
             (list proc2 args2 ...))
       (andmap {lambda (expr1 expr2) (aeq? expr1 expr2 env)}
               (cons proc1 args1)
               (cons proc2 args2))
       ]
      [something-else
        false
       (raise (make-exn:fail
               (if (eq? something-else e2)
                   (format "(aeq?) malformed input program ~s" something-else)
                   (format "(aeq?) malformed expr ~s in input program:\n~s"
                           something-else e2))
               (current-continuation-marks)))]))
  (aeq? e1 e2 (empty-env)))


(alpha-equivalent? (rename-vars/verify-legal (quote (let ((x 0)) (set! x (add1 x)) x))) (quote (let ((x 0)) (begin (set! x (add1 x)) x))))


;; Aliases:
(define pass1 rename-vars/verify-legal)
(define this-pass pass1)

;; Test code follows.

(module+ test
  
  (compiler-tests/aeq rename-vars/verify-legal
    (5                     5)
    ((if #t 5 6)           (if #t 5 6))
    ((if #t (if #f 7 8) 6) (if #t (if #f 7 8) 6))
    ((when #f 42)          (when #f 42))
    ((unless #f 42)        (unless #f 42))
    ((let ([x 0])
       (set! x (add1 x))
       x)
     (let ([x 0])
       (set! x (add1 x))
       x))
    ((let ([x 0])
       (set! x (add1 x))
       x)
     (let ([x 0])
       (set! x (add1 x))
              x))
    ((let ([a 1][b 2]) (begin a b))
     (let ([a 1][b 2]) (begin a b)))
    ((let ([a 1]) a)
     (let ([a 1]) a))
    ((let ([a 1][b 2]) (set! b 4) (+ a b))
     (let ([a 1][b 2]) (set! b 4) (+ a b)))
    ((letrec ([! (lambda (n) (if (= n 0) 1 (* n (! (- n 1)))))])
       (+ (! 5) 1))
     (letrec ([! (lambda (n) (if (= n 0) 1 (* n (! (- n 1)))))])
       (+ (! 5) 1)))
    ((let ([! (void)])
       (set! ! (lambda (n) (if (= n 0) 1 (* n (! (- n 1))))))
       (+ (! 5) 1))
     (let ([! (void)])
       (set! ! (lambda (n) (if (= n 0) 1 (* n (! (- n 1))))))
       (+ (! 5) 1)))
    )
  
  )