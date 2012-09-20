#lang plai
;; login : 269525206

;; TODO
; Add interp-specific error for division by zero

(print-only-errors true)

(define binops (make-hash
                (list
                 (cons '+ +)
                 (cons '- -)
                 (cons '* *)
                 (cons '/ /))))

(define banned (append (hash-keys binops)
                       (list 'with 'if0 'fun)))

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

;; interp-binop : symbol CFWAE-Value CFWAE-Value -> CFWAE-Value
; Takes 2 values and applies them to an operator.
(define (interp-binop sym x y)
  (if (and (numV? x) (numV? y))
      ((hash-ref binops sym) (numV-n x) (numV-n y))
      (error "Not a number")))

(test (interp-binop '+ (numV 4) (numV 4)) 8)
(test (interp-binop '- (numV 4) (numV 4)) 0)
(test (interp-binop '/ (numV 4) (numV 2)) 2)
(test (interp-binop '* (numV 4) (numV 3)) 12)
(test/exn (interp-binop '+ (numV 5)
                        (closureV (list 'a) (num 4) (mtEnv)))
          "Not a number")
(test/exn (interp-binop '+ (closureV (list 'a) (num 4) (mtEnv))
                        (numV 5))
          "Not a number")

;; lookup : Env symbol -> CFWAE-Value
; Looks up the id in the environment, returning a CFWAE, or throwing an error.
(define (lookup env id)
  (type-case Env env
    [mtEnv () (error "Unbound identifier:" id)]
    [anEnv (name value env)
           (if (symbol=? name id)
               value
               (lookup env id))]))

(test/exn (lookup (mtEnv) 'a) "Unbound identifier: a")
(test (lookup (anEnv 'a (numV 4) (mtEnv)) 'a) (numV 4))
(test (lookup (anEnv 'a (numV 4)
                     (anEnv 'b (numV 5) (mtEnv))) 'b) (numV 5))

;; interp-internal : CFWAE Env -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp-internal expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op lhs rhs)
           (let ([l (interp-internal lhs env)]
                 [r (interp-internal rhs env)])
             (if (and (eq? op /) (= 0 (numV-n r)))
                 (error "Division by zero error")
                 (if (or (not (numV? l))
                         (not (numV? r)))
                     (error "Binop non-numerical argument error")
                     (numV (op
                            (numV-n l)
                            (numV-n r))))))]
    [with (lob body)
          (interp-internal
           body 
           (foldl {λ (binding new-env)
                    (anEnv (binding-name binding)
                           (interp-internal (binding-named-expr binding) env)
                           new-env)}
                  env
                  lob))]
    [id (name)
        (lookup env name)]
    [if0 (i then else)
         (let ([r (interp-internal i env)])
           (if (not (numV? r))
               (error "Conditionals require a number")
               (if (= (numV-n r) 0)
                   (interp-internal then env)
                   (interp-internal else env))))]
    [fun (args body)
         (closureV args body env)]
    [app (func args)
           (let ([f-value (interp-internal func env)])
           (if (not (closureV? f-value))
               (error "Not a function")
               (if (not (= (length (closureV-params f-value))
                           (length args)))
                   (error "Function arity error")
                   (interp-internal
                    (closureV-body f-value)
                    (foldl {λ (binding new-env)
                             (anEnv (binding-name binding)
                                    (interp-internal
                                     (binding-named-expr binding) env)
                                    new-env)}
                           env
                           (for/list
                               ([arg args]
                                [arg-name (closureV-params f-value)])
                             (binding arg-name arg)
                             )
                           )))))]))

; Numbers
(test (interp-internal (num 4)  mtEnv) (numV 4))

; Operations
(test (interp-internal (binop + (num 4) (num 4))  (mtEnv)) (numV 8))
(test (interp-internal (binop - (num 4) (num 4))  (mtEnv)) (numV 0))
(test (interp-internal (binop / (num 4) (num 2))  (mtEnv)) (numV 2))
(test (interp-internal (binop * (num 4) (num 2))  (mtEnv)) (numV 8))
(test/exn (interp-internal (binop / (num 5) (num 0)) (mtEnv))
          "Division by zero error")
(test/exn (interp-internal (binop + (num 1) (fun (list 'x) (id 'x))) (mtEnv))
          "Binop non-numerical argument error")

; Ids
(test (interp-internal (id 'a) (anEnv 'a (numV 4) (mtEnv))) (numV 4))

; With
(test (interp-internal (with (list (binding 'a (num 4))) (id 'a))  (mtEnv))
      (numV 4))
(test (interp-internal (with (list (binding 'a (num 4))
                                   (binding 'b (num 5)))
                             (binop + (id 'a) (id 'b)))  (mtEnv))
      (numV 9))

; If0
(test (interp-internal (if0 (num 0) (num 4) (num 5)) (mtEnv)) (numV 4))
(test/exn (interp-internal (if0 (fun '(a b) (id 'a))
                                (num 4) (num 5)) (mtEnv))
          "Conditionals require a number")

; Fun
(test (interp-internal (fun '(a b c) (binop + (id 'a) (id 'b))) (mtEnv))
      (closureV '(a b c) (binop + (id 'a) (id 'b)) (mtEnv)))
(test (interp-internal (fun '(a b c) (binop + (id 'a) (id 'b)))
                       (anEnv 'a (numV 4) (mtEnv)))
      (closureV '(a b c) (binop + (id 'a) (id 'b)) (anEnv 'a (numV 4) (mtEnv))))

; App
(test (interp-internal (app (fun '(a b c) (binop + (id 'a) (id 'b)))
                            (list (num 1) (num 2) (num 3)))
                       (mtEnv))
      (numV 3))
(test/exn (interp-internal (app
         (fun (list 'self 'n) (app (id 'self) (list (id 'self) (id 'n))))
         (list
          (fun
           (list 'self 'n)
           (if0
            (id 'n)
            (num 0)
            (binop
             +
             (id 'n)
             (app (id 'self) (list (binop - (id 'n) (num 1)))))))
          (num 10))) (mtEnv))
      "Function arity error")
(test/exn (interp-internal (app (num 5) (list (num 3))) (mtEnv))
          "Not a function")


;; interp : CFWAE -> CFWAE-Value
;; This procuedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp expr)
  (interp-internal expr (mtEnv)))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    [(empty? sexp) (error "Parse error")]
    [(member sexp banned) (error "Parse error")]
    [(list? sexp)
     (let ([f (car sexp)])
       (cond
         [(member f banned)
          (cond
            [(hash-has-key? binops f)
             (if (not (= 2 (length (cdr sexp))))
                 (error "Parse error")
                 (binop (hash-ref binops f)
                        (parse (cadr sexp)) (parse (caddr sexp))))]
            [(symbol=? f 'if0)
             (if (not (= (length sexp) 4))
                 (error "Parse error")
                 (make-if0 (parse (cadr sexp))
                           (parse (caddr sexp))
                           (parse (cadddr sexp))))]
            [(symbol=? f 'with)
             (if (or (not (= (length sexp) 3))
                     (not (= (length (remove-duplicates
                                      (map car
                                           (cadr sexp))))
                             (length (cadr sexp))))
                     (not (andmap symbol? (map car (cadr sexp)))))
                 (error "Parse error")
                 (make-with (for/list
                                ([b (cadr sexp)])
                              (binding (car b) (parse (cadr b))))
                            (parse (caddr sexp))))]
            [(symbol=? f 'fun)
             (if (or (not (= (length sexp) 3))
                     (not (andmap symbol? (cadr sexp))))
                 (error "Parse error")
                 (fun (cadr sexp) (parse (caddr sexp))))]
            )]
         [else
          (let ([fun-v (parse (car sexp))]
                [args (map parse (cdr sexp))])
                (app fun-v args))]
         ))
     ]
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [else (error "Parse error")]))




(for ([x banned])
  (test/exn (parse x) "Parse error"))

(test/exn (parse '()) "Parse error")
(test (parse 4) (num 4))
(test (parse '{+ 4 4}) (binop + (num 4) (num 4)))
(test (parse '{- 4 4}) (binop - (num 4) (num 4)))
(test (parse '{* 4 4}) (binop * (num 4) (num 4)))
(test (parse '{/ 4 4}) (binop / (num 4) (num 4)))
(test/exn (parse '{/ 4 4 5}) "Parse error")
(test/exn (parse '{/ 4}) "Parse error")
(test (parse 'a) (id 'a))
(test (parse '{if0 0 4 5}) (if0 (num 0) (num 4) (num 5)))
(test/exn (parse '{if0 0 5}) "Parse error")
(test (parse '{with {{a 4} {b 5}} {+ a b}})
      (with (list (binding 'a (num 4))
                  (binding 'b (num 5)))
            (binop + (id 'a) (id 'b))))
(test (parse '(with ((f (fun (x y) (+ x y)))) (f (f 1 2) 3)))
      (with
       (list (binding 'f (fun (list 'x 'y) (binop + (id 'x) (id 'y)))))
       (app (id 'f) (list (app (id 'f) (list (num 1) (num 2))) (num 3)))))
(test/exn (parse '(with ((x 5) (y 6) (x 3)) x)) "Parse error")
(test/exn (parse (quote (with (("sym" 1)) 5))) "Parse error")
(test (parse '{fun {a b c} {+ a b}})
      (fun '(a b c) (binop + (id 'a) (id 'b))))
(test (parse '{{fun {a b c} {+ a b}} 4 5})
      (app (fun '(a b c) (binop + (id 'a) (id 'b))) (list (num 4) (num 5))))
(test/exn (parse (quote (fun (a b 5) (+ a b)))) "Parse error")
#;(test/exn (parse (quote (5 3))) "Parse error")


;; Big tests
#;(interp (parse
           '((fun (x) (fun (n) (if0 n
                                    1
                                    (* n ((x x) (- n 1))))))
             (fun (x) (fun (n) (if0 n
                                    1
                                    (* n ((x x) (- n 1)))))))))

(test (interp (parse '{with {{x 5} {z 6}}
                            {{fun {z} {+ z x}} 3}})) (numV 8))
(test (interp (parse '{with {{x 5}}
                            {{fun {y} {+ y x}} 3}})) (numV 8))
#;(test (interp (parse
                 '(((fun (x) (fun (n) (if0 n
                                           1
                                           (* n ((x x) (- n 1))))))
                    (fun (x) (fun (n) (if0 n
                                           1
                                           (* n ((x x) (- n 1))))))))))
        3628800)

(test (interp
 (parse
  '(with
   ((y (fun (proc) ((fun (x) (proc (fun (arg) ((x x) arg))))
                    (fun (x) (proc (fun (arg) ((x x) arg))))))))
   (with ((fact (fun (rec) (fun (n) (if0 n
                                         1
                                         (* n (rec (+ n -1))))))))
         (with ((! (y fact))) (! 5)))))) (numV 120))