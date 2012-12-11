#lang racket

(require "env.rkt" "helpers.rkt")

(provide alpha-equivalent?
         alpha-equivalent/lang5?
         check-aeq?
         compiler-tests/aeq
         compiler-tests/equal?)
(require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test shorthand

(require rackunit)

(define-simple-check (check-aeq? e1-compiled e2 lang)
  (case lang
    [(lang1 lang2 lang3) (alpha-equivalent? e1-compiled e2)]
    [(lang5) (alpha-equivalent/lang5? e1-compiled e2)]))

(define-for-syntax ((compile/check-aeq? this-pass [lang 'lang1]) loc e1 e2)
  (with-syntax ([e1 e1][e2 e2][e1-out #'e1-out][lang lang])
    (with-syntax ([the-check
                   (syntax/loc loc (check-aeq? e1-out `e2 lang))]
                  #;[pass-name (syntax->datum #'this-pass)])
      #`(let ([e1-out (#,this-pass `e1)])
          (with-check-info* (list (make-check-expression
                                   '(check-aeq? (#,this-pass `e1) `e2 lang))
                                  (make-check-params (list `e1 `e2))
                                  (make-check-info 'pass-output e1-out))
            (λ () the-check))))))

;; compiler-tests/aeq:
;; Main entry for checks based on alpha-equivalence.  Syntax:
#;
(compiler-tests/aeq <PASS-NAME>
  [<LANG-NAME>]
  (<TEST-EXPR> <EXPECTED-EXPR>)
  ...)

(define-syntax (compiler-tests/aeq stx)
  (syntax-case stx ()
    [(_ this-pass language test ...)
     (identifier? #'language)
     (with-syntax ([((in out) ...) #'(test ...)])
       (let ([locs (syntax->list #'(test ...))])
         (with-syntax ([(new-test ...)
                        (map (compile/check-aeq? #'this-pass
                                                 (syntax->datum #''language))
                             locs
                             (syntax->list #'(in  ...))
                             (syntax->list #'(out ...)))])
           #'(begin new-test ...))))]
    [(_ this-pass test ...)
     (with-syntax ([((in out) ...) #'(test ...)])
       (let ([locs (syntax->list #'(test ...))])
         (with-syntax ([(new-test ...)
                        (map (compile/check-aeq? #'this-pass)
                             locs
                             (syntax->list #'(in  ...))
                             (syntax->list #'(out ...)))])
           #'(begin new-test ...))))]))

(define-syntax (compiler-tests/equal? stx)
  (syntax-case stx ()
    [(_ this-pass test ...)
     (with-syntax ([((in out) ...) #'(test ...)])
       (let ([locs (syntax->list #'(test ...))])
         (define (make-check loc stx)
           (syntax-case stx ()
             [(e1 e2)
              (with-syntax ([the-check (syntax/loc loc (check-equal? e1-out `e2))])
                #'(let ([e1-out (this-pass `e1)])
                    (with-check-info*
                        (list (make-check-expression
                               '(check-equal? (this-pass `e1) `e2))
                              (make-check-params (list `e1 `e2))
                              (make-check-info 'pass-output e1-out))
                      (λ () the-check))))]))
         (with-syntax ([(new-test ...)
                        (map make-check
                             locs
                             (syntax->list #'((in out) ...)))])
           #'(begin new-test ...))))]))


;;;;;;;;;;;;;;;;;;;;; Alpha-equivalence ;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit))
;(require racket/trace)

; A TestableExpr is an expression of Lang1, Lang2, or Lang3.

;; alpha-equivalent? : TestableExpr TestableExpr -> Boolean
;; determines whether the two given programs are equivalent up to renaming
;; of bound variables.  I.e., they may differ in identifiers, but must have
;; identical structure, such that for some function Î±(v) from identifiers
;; in e1 to identifiers in e2, e1[subst Î±(v) for all v] = e2.
(define (alpha-equivalent? e1 e2)
  ;; aeq? : Lang1 Lang1 Env[Sym => Sym] Listof[Sym] -> Boolean
  ;; env maps bound variables of e1 to bound variables of e2.
  ;; e2-bound-ids is the list of all variables in scope at the current point
  ;; in e2.
  (define (aeq? e1 e2 env e2-bound-ids)
    ;; id=? : Sym Sym -> Boolean
    ;; true iff id1 and id2 are both free and equal or both bound at
    ;; corresponding points in the program (i.e., if both reference the same
    ;; place in e1 and e2's respective structures).
    (define (id=? id1 id2)
      (let ([id1-renamed/f (lookup id1 env)]
            [id2-bound? (memq id2 e2-bound-ids)])
        (if id1-renamed/f ; id1 is bound
            (and id2-bound?       (eq? id2 id1-renamed/f)) ; both are bound
            (and (not id2-bound?) (eq? id1 id2)))))        ; both are free
    (define recur-aeq? (curryr aeq? env e2-bound-ids)) ; convenience
    (match (list e1 e2)
      [(list (? constant? c1) (? constant? c2)) (equal? c1 c2)]
      [(list (list 'quote x1) (list 'quote x2)) (equal? x1 x2)]
      [(list (? symbol? x1) (? symbol? x2))
       (id=? x1 x2)]
      [(list (list 'set! (? symbol? lhs) rhs)
             (list 'set! (? symbol? lhs2) rhs2))
       (and (id=? lhs lhs2)
            (recur-aeq? rhs rhs2))]
      [(list (list 'when test result)
             (list 'when test2 result2))
       (and (recur-aeq? test test2) (recur-aeq? result result2))]
      [(list (list 'unless test result)
             (list 'unless test2 result2))
       (and (recur-aeq? test test2) (recur-aeq? result result2))]
      [(list (list 'if test thn els)
             (list 'if test2 thn2 els2))
       (and (recur-aeq? test test2)
            (recur-aeq? thn thn2)
            (recur-aeq? els els2))]
      [(list (list 'begin b bs ...)
             (list 'begin b2 bs2 ...))
       (and (recur-aeq? b b2)
            (= (length bs) (length bs2))
            (andmap recur-aeq? bs bs2))]
      [(list (list 'let (list (list (? symbol? lhs)  rhs) ...)  b  bs  ...)
             (list 'let (list (list (? symbol? lhs2) rhs2) ...) b2 bs2 ...))
       (and (= (length lhs) (length lhs2))
            (= (length bs) (length bs2))
            (andmap recur-aeq? rhs rhs2)
            (let ([new-env (extend-env lhs lhs2 env)])
              (and (andmap (curryr aeq? new-env (append lhs2 e2-bound-ids))
                           (cons b bs)
                           (cons b2 bs2)))))]
      [(list (list 'letrec (list (list (? symbol? lhs)  rhs)  ...) b  bs  ...)
             (list 'letrec (list (list (? symbol? lhs2) rhs2) ...) b2 bs2 ...))
       (and (= (length lhs) (length lhs2))
            (= (length bs)  (length bs2))
            (let ([new-env (extend-env lhs lhs2 env)]
                  [new-ids (append lhs2 e2-bound-ids)])
              (let ([recur-aeq? (curryr aeq? new-env new-ids)])
                (and (andmap recur-aeq? rhs rhs2)
                     (recur-aeq? b b2)
                     (andmap recur-aeq? bs bs2)))))]
      [(list (list 'lambda (list (? symbol? formals)  ...) b  bs ...)
             (list 'lambda (list (? symbol? formals2) ...) b2 bs2 ...))
       (and (= (length formals) (length formals2))
            (= (length bs) (length bs2))
            (let ([new-env (extend-env formals formals2 env)]
                  [new-bound-ids (append formals2 e2-bound-ids)])
              (and (aeq? b b2 new-env new-bound-ids)
                   (andmap (curryr aeq? new-env new-bound-ids)
                           bs bs2))))]
      [(list (list (? primitive? prim)  args  ...)
             (list (? primitive? prim2) args2 ...))
       (and (eq? prim prim2)
            (= (length args) (length args2))
            (andmap recur-aeq?
                    args
                    args2))]
      [(list (list proc  args  ...)
             (list proc2 args2 ...))
       (and (aeq? proc proc2 env e2-bound-ids)
            (= (length args) (length args2))
            (andmap recur-aeq?
                    args
                    args2))]
      [something-else #f]))
  ;(trace aeq?)
  (aeq? e1 e2 (empty-env) empty))


;; alpha-equivalent? : Lang5:TestableExpr Lang5:TestableExpr -> Boolean
;; determines whether the two given programs are equivalent up to renaming
;; of bound variables.  I.e., they may differ in identifiers, but must have
;; identical structure, such that for some function Î±(v) from identifiers
;; in e1 to identifiers in e2, e1[subst Î±(v) for all v] = e2.
(define (alpha-equivalent/lang5? prog1 prog2)
  ;; aeq? : Lang1 Lang1 Env[Sym => Sym] Listof[Sym] -> Boolean
  ;; env maps bound variables of e1 to bound variables of e2.
  ;; e2-bound-ids is the list of all variables in scope at the current point
  ;; in e2.
  (define (aeq? e1 e2 env e2-bound-ids)
    (match (list e1 e2)
      [(list (list 'begin b bs ...)
             (list 'begin b2 bs2 ...))
       (and (aeq/nb-exp? b b2 env e2-bound-ids)
            (= (length bs) (length bs2))
            (andmap (curryr aeq/nb-exp? env e2-bound-ids) bs bs2))]
      [otherwise (aeq/nb-exp? e1 e2 env e2-bound-ids)]))
    
    (define (aeq/nb-exp? e1 e2 env e2-bound-ids)
      (define recur-aeq? (curryr aeq? env e2-bound-ids)) ; convenience

      ;; id=? : Sym Sym -> Boolean
      ;; true iff id1 and id2 are both free and equal or both bound at
      ;; corresponding points in the program (i.e., if both reference the same
      ;; place in e1 and e2's respective structures).
      (define (id=? id1 id2 [env env] [e2-bound-ids e2-bound-ids])
        (let ([id1-renamed/f (lookup id1 env)]
              [id2-bound? (memq id2 e2-bound-ids)])
          (if id1-renamed/f ; id1 is bound
              (and id2-bound?       (eq? id2 id1-renamed/f)) ; both are bound
              (and (not id2-bound?) (eq? id1 id2)))))        ; both are free
      (match (list e1 e2)
        [(list (list 'quote x1) (list 'quote x2)) (equal? x1 x2)]
        [(list (? symbol? x1) (? symbol? x2))     (id=? x1 x2)]
        [(list (list 'set! (? symbol? lhs) rhs)
               (list 'set! (? symbol? lhs2) rhs2))
         (and (id=? lhs lhs2)
              (recur-aeq? rhs rhs2))]
        [(list (list 'if test thn els)
               (list 'if test2 thn2 els2))
         (and (recur-aeq? test test2)
              (recur-aeq? thn thn2)
              (recur-aeq? els els2))]
        [(list (list 'let (list (list (? symbol? lhs)  rhs) ...)
                     (list 'tag (list 'asgd a1-ids ...) (list 'refd r1-ids ...)
                           b))
               (list 'let (list (list (? symbol? lhs2) rhs2) ...)
                     (list 'tag (list 'asgd a2-ids ...) (list 'refd r2-ids ...)
                           b2)))
         (and (= (length lhs) (length lhs2))
              (= (length a1-ids) (length a2-ids))
              (= (length r1-ids) (length r2-ids))
              (let ([env (extend-env lhs lhs2 env)]
                    [e2-bound-ids (append lhs2 e2-bound-ids)])
                ;; note that now, the recur-aeq? bound above does NOT refer to
                ;; the env and e2-bound-ids in scope here!
                (and (andmap (curryr id=? env e2-bound-ids) a1-ids a2-ids)
                     (andmap (curryr id=? env e2-bound-ids) r1-ids r2-ids)
                     (andmap recur-aeq? rhs rhs2)
                     (aeq? b b2 env e2-bound-ids))))]
        [(list (list 'letrec (list (list (? symbol? lhs)  rhs)  ...)
                     (list 'tag (list 'asgd a1-ids ...) (list 'refd r1-ids ...)
                           b))
               (list 'letrec (list (list (? symbol? lhs2) rhs2) ...)
                     (list 'tag (list 'asgd a2-ids ...) (list 'refd r2-ids ...)
                           b2)))
         (and (= (length lhs) (length lhs2))
              (= (length a1-ids) (length a2-ids))
              (= (length r1-ids) (length r2-ids))
              (let ([env (extend-env lhs lhs2 env)]
                    [e2-bound-ids (append lhs2 e2-bound-ids)])
                ;; note that now, the recur-aeq? bound above does NOT refer to
                ;; the env and e2-bound-ids in scope here!
                (and (andmap (curryr id=? env e2-bound-ids) a1-ids a2-ids)
                     (andmap (curryr id=? env e2-bound-ids) r1-ids r2-ids)
                     (let ([recur-aeq? (curryr aeq? env e2-bound-ids)])
                       (and (andmap recur-aeq? rhs rhs2)
                            (recur-aeq? b b2))))))]
        [(list (list 'lambda (list (? symbol? formals)  ...)
                     (list 'tag (list 'asgd a1-ids ...) (list 'refd r1-ids ...)
                           b))
               (list 'lambda (list (? symbol? formals2) ...)
                     (list 'tag (list 'asgd a2-ids ...) (list 'refd r2-ids ...)
                           b2)))
         (let ([env (extend-env formals formals2 env)]
               [e2-bound-ids (append formals2 e2-bound-ids)])
           (and (= (length formals) (length formals2))
                (= (length a1-ids) (length a2-ids))
                (= (length r1-ids) (length r2-ids))
                (andmap (curryr id=? env e2-bound-ids) a1-ids a2-ids)
                (andmap (curryr id=? env e2-bound-ids) r1-ids r2-ids)
                (aeq? b b2 env e2-bound-ids)))]
        [(list (list (? primitive? prim)  args  ...)
               (list (? primitive? prim2) args2 ...))
         (and (eq? prim prim2)
              (= (length args) (length args2))
              (andmap recur-aeq?
                      args
                      args2))]
        [(list (list proc  args  ...)
               (list proc2 args2 ...))
         (and (aeq? proc proc2 env e2-bound-ids)
              (= (length args) (length args2))
              (andmap recur-aeq?
                      args
                      args2))]
        [something-else #f]))
    (trace aeq? aeq/nb-exp?)
  
  ;; unwrap : Lang5:Prog -> Lang5:Exp
  (define (unwrap prog)
    (match prog
      [(list 'let '() stx-def e) e]))
  
  (aeq? (unwrap prog1) (unwrap prog2) (empty-env) empty))


(module+ test
  
  (define-syntax-rule (wrap e)
  `(let ()
     (begin
       (define-syntax tag
         (syntax-rules (asgd refd)
           [(_ (asgd SVar (... ...)) (refd RVar (... ...)) Expr)
            Expr])))
     e))
  
  ;; test for lang5:
  (compiler-tests/aeq identity
    lang5
    (,(wrap '5) ,(wrap '5))
    (,(wrap '#f) ,(wrap '#f))
    (,(wrap (let ([x '3])
              (tag (asgd x) (refd x)
                   (begin (set! x (add1 x)) x))))
     ,(wrap (let ([z '3])
              (tag (asgd z) (refd z)
                   (begin (set! z (add1 z)) z)))))
    (,(wrap (letrec ([! (lambda (n)
                          (tag (asgd) (refd n)
                               (if (zero? n) '1 (* n (! (- n '1))))))])
              (tag (asgd) (refd !)
                   (! '6))))
     ,(wrap (letrec ([fact
                      (lambda (x)
                        (tag (asgd) (refd x)
                             (if (zero? x) '1 (* x (fact (- x '1))))))])
              (tag (asgd) (refd fact)
                   (fact '6))))))
  
  ;; tests for lang1-lang3
  (check-true (alpha-equivalent? '(lambda (x) x) '(lambda (x) x)))
  (check-true (alpha-equivalent? '(lambda (x) x) '(lambda (y) y)))
  (check-false (alpha-equivalent? '(lambda (x) x) '(lambda (y) x)))
  (check-true (alpha-equivalent? '(if (((lambda (x y)
                                          (lambda (z) (= (+ x y) z)))
                                        2 3)
                                       4)
                                      100
                                      200)
                                 '(if (((lambda (x z)
                                          (lambda (y) (= (+ x z) y)))
                                        2 3)
                                       4)
                                      100
                                      200)))
  (check-false (alpha-equivalent? '(if (((lambda (x y)
                                           (lambda (z) (= (+ x y) z)))
                                         2 3)
                                        4)
                                       100
                                       200)
                                  '(if (((lambda (x z)
                                           (lambda (y) (= (+ x y) z)))
                                         2 3)
                                        4)
                                       100
                                       200)))
  (check-false (alpha-equivalent? 'a 'b))
  (check-false (alpha-equivalent? '(lambda (x) y) '(lambda (y) y)))
  (check-false (alpha-equivalent? '(lambda (x) x) '(lambda (y) x)))
  (check-true (alpha-equivalent? '(lambda (x) y) '(lambda (z) y)))
  (check-true (alpha-equivalent?
               '(letrec ([fact (lambda (n)
                                 (if (zero? n) 1 (* n (fact (sub1 n)))))])
                  (fact 5))
               '(letrec ([! (lambda (x)
                              (if (zero? x)
                                  1
                                  (* x (! (sub1 x)))))])
                  (! 5))))
  (check-false (alpha-equivalent?
                '(letrec ([fact (lambda (n)
                                  (if (zero? n) 1 (* n (fact n))))])
                   (fact 5))
                '(letrec ([! (lambda (x)
                               (if (zero? x)
                                   1
                                   (* x (! (sub1 x)))))])
                   (! 5))))
  (check-true (alpha-equivalent? '(set! x 3) '(set! x 3)))
  (check-false (alpha-equivalent? '(set! x 3) '(set! y 3)))
  (check-true (alpha-equivalent? '(begin (set! x 3) x) '(begin (set! x 3) x)))
  (check-false (alpha-equivalent? '(begin (set! x 3) x) '(begin (set! y 3) y)))
  (check-true (alpha-equivalent? '(let ([x 0])
                                    (lambda () (set! x (add1 x)) x))
                                 '(let ([y 0])
                                    (lambda () (set! y (add1 y)) y))))
  (check-true (alpha-equivalent? '(cons 5 '(a b c d)) '(cons 5 '(a b c d))))
  (check-false (alpha-equivalent? '(cons 5 '(a b c d)) '(cons 5 '(a b c))))
  (check-true (alpha-equivalent? '(let ([ctr 0])
                                    (lambda (x)
                                      (when (> x 0)
                                        (set! ctr (add1 ctr))
                                        (f (sub1 x)))
                                      ctr))
                                 '(let ([c 0])
                                    (lambda (n)
                                      (when (> n 0)
                                        (set! c (add1 c))
                                        (f (sub1 n)))
                                      c))))
  (check-true (alpha-equivalent? '(letrec ([x 5][y 6]) (* x y))
                                 '(letrec ([a 5][b 6]) (* a b))))
  (check-true (alpha-equivalent? '(letrec ([f (let ([ctr 0])
                                                (lambda (x)
                                                  (when (> x 0)
                                                    (begin
                                                      (set! ctr (add1 ctr))
                                                      (f (sub1 x))))
                                                  ctr))])
                                    (f 10))
                                 '(letrec ([g (let ([c 0])
                                                (lambda (n)
                                                  (when (> n 0)
                                                    (begin
                                                      (set! c (add1 c))
                                                      (g (sub1 n))))
                                                  c))])
                                    (g 10))))
  (check-false (alpha-equivalent? '(letrec ([f (let ([ctr 0])
                                                 (lambda (x)
                                                   (when (> x 0)
                                                     (begin
                                                       (set! ctr (add1 ctr))
                                                       (f (sub1 x))))
                                                   ctr))])
                                     (f 10))
                                  '(letrec ([g (let ([c 0])
                                                 (lambda (n)
                                                   (when (> n 0)
                                                     (begin
                                                       (set! c (add1 c))
                                                       (f (sub1 n))))
                                                   n))])
                                     (g 10))))
  )