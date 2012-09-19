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
                 (numV (op
                        (numV-n l)
                        (numV-n r)))))]
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
              (interp-internal (closureV-body f-value)
                      (foldl {λ (binding new-env)
                               (anEnv (binding-name binding)
                                      (interp-internal
                                       (binding-named-expr binding) env)
                                      new-env)}
                             env
                             (for/list ([arg args]
                                        [arg-name (closureV-params f-value)])
                               (binding arg-name arg)
                               )
                             )))]))

; Numbers
(test (interp-internal (num 4)  mtEnv) (numV 4))

; Operations
(test (interp-internal (binop + (num 4) (num 4))  (mtEnv)) (numV 8))
(test (interp-internal (binop - (num 4) (num 4))  (mtEnv)) (numV 0))
(test (interp-internal (binop / (num 4) (num 2))  (mtEnv)) (numV 2))
(test (interp-internal (binop * (num 4) (num 2))  (mtEnv)) (numV 8))
(test/exn (interp-internal (binop / (num 5) (num 0)) (mtEnv))
                           "Division by zero error")

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

;; interp : CFWAE -> CFWAE-Value
;; This procuedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp expr)
  (interp-internal expr (mtEnv)))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (match sexp
      [(list 'if0 pred then else)
       (make-if0 (parse pred) (parse then) (parse else))]
    [(list 'with (list (list names vals) ...) body)
     (make-with (for/list
                  ([name names] [val vals])
                (binding name (parse val)))
              (parse body))]
    [(list 'fun (list args ...) body)
     (fun args (parse body))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '/ l r) (binop / (parse l) (parse r))]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list func args ...) (app (parse func) (map parse args))]
    [x (cond
         [(number? x) (num x)]
         [(symbol? x) (id x)]
         [else
           (error "Parse error")])]))

(test (parse 4) (num 4))
(test (parse '{+ 4 4}) (binop + (num 4) (num 4)))
(test (parse '{- 4 4}) (binop - (num 4) (num 4)))
(test (parse '{* 4 4}) (binop * (num 4) (num 4)))
(test (parse '{/ 4 4}) (binop / (num 4) (num 4)))
(test (parse 'a) (id 'a))
(test (parse '{if0 0 4 5}) (if0 (num 0) (num 4) (num 5)))
(test (parse '{with {{a 4} {b 5}} {+ a b}})
             (with (list (binding 'a (num 4))
                         (binding 'b (num 5)))
                   (binop + (id 'a) (id 'b))))
(test (parse '{fun {a b c} {+ a b}})
      (fun '(a b c) (binop + (id 'a) (id 'b))))
(test (parse '{{fun {a b c} {+ a b}} 4 5})
      (app (fun '(a b c) (binop + (id 'a) (id 'b))) (list (num 4) (num 5))))


;; Big tests
(interp (parse
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
(test (interp (parse
               '(((fun (x) (fun (n) (if0 n
                                        1
                                        (* n ((x x) (- n 1))))))
                  (fun (x) (fun (n) (if0 n
                                        1
                                        (* n ((x x) (- n 1))))))))))
      3628800)

