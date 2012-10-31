#lang plai-typed

(require "typed-lang2.rkt")
#;(require (typed-in racket [findf : (('a -> boolean) (listof 'a)
                                                    -> (optionof 'a))]))
(require (typed-in racket/list
                    [remove-duplicates :
                                       ((listof 'a) -> (listof 'a))]))


#;(define-syntax let-values
    (syntax-rules ()
      [(_ ([(v ...) e]) body)
       (local [(define-values (v ...) e)]
         body)]))

(remove-duplicates (list 4 4 5 6 7 8))
(define (car (l : (listof 'a))) : 'a
             (first l))
(define (cdr (l : (listof 'a))) : 'a
             (rest l))

(define-type Cell
  [cell (location : Location) (value : ValueC)])

(define-type-alias Store (listof Cell))

(define-type Result
  [v*s (value : ValueC) (store : Store)])

(define-type StoreEnv
  [e*s (env : Env) (store : Store)])

(define-type ListResult
  [vs*s (values : (listof ValueC)) (store : Store)])


;; Get the next free location in the store. Finds the highest store and
; increments by one.
(define (get-location (store : Store)) : Location
  (add1 (foldl {lambda (x acc) (if (> x acc) x acc)} 
               0
               (map cell-location store))))

(test (get-location (list (cell 4 (NumV 4)))) 5)
(test (get-location (list (cell 3 (NumV 4))
                          (cell 4 (NumV 4)))) 5)

; Replace the value of a location in the store with a new value.
(define (store-replace (loc : Location) (new-val : ValueC)
                       (store : Store)) : Store
  (map {lambda (c)
         (if (= (cell-location c) loc)
             (cell loc new-val)
             c)}
       store))
  
  
;; Extend the envronment and store with a new binding for the current
; environment.
(define (extend-env (id : symbol) (b : ValueC)
                    (env : Env) (store : Store)) : StoreEnv
  (let ([next-loc (get-location store)])
    (e*s
     (cons (bind id next-loc) env)
     (cons (cell next-loc b) store))))

(define (findf (fxn : ('a -> boolean))
               (lst : (listof 'a))) : (optionof 'a)
  (cond
    [(empty? lst) (none)]
    [(fxn (car lst)) (some (car lst))]
    [else (findf fxn (cdr lst))]))
     
;; Associates an id with a Location in the store.
(define (id->Location (id : symbol) (env : Env)) : (optionof Location)
  (type-case (optionof Binding)
    (findf (lambda (x) (symbol=? (bind-name x) id)) env)
    [none () (none)]
    [some (v) (some (bind-value v))]))


    

(define (Location->value (loc : Location) (store : Store)) : (optionof ValueC)
  (type-case (optionof Cell)
    (findf (lambda (x) (= (cell-location x) loc))
           store)
    [none () (none)]
    [some (v) (some (cell-value v))]))

(test (Location->value 3
                       (list (cell 3 (NumV 4))
                             (cell 4 (NumV 3)))) (some (NumV 4)))


(define (op->func (op : symbol)) : ('number 'number -> ValueC)
  (case op
    ['num+
     {lambda (x y) (NumV (+ x y))}
     ]
    ['num- {lambda (x y) (NumV (- x y))}]
    ['== {lambda (x y) (if (eq? x y) (TrueV) (FalseV))}]
    ['< {lambda (x y) (if (< x y) (TrueV) (FalseV))}]
    ['> {lambda (x y) (if (> x y) (TrueV) (FalseV))}]))


(define (tagof (v : ValueC)) : string
  (type-case ValueC v
    [ObjectV (fs) "object"]
    [ClosureV (a b e) "function"]
    [NumV (n) "number"]
    [StrV (s) "string"]
    [TrueV () "boolean"]
    [FalseV () "boolean"]))
         
(define (meta-equal (arg1 : ValueC) (arg2 : ValueC)) : ValueC
  (if (if (not (string=? (tagof arg1) (tagof arg2)))
          false
          (type-case ValueC arg1
            [NumV (n) (= n (NumV-n arg2))]
            [StrV (s) (string=? s (StrV-s arg2))]
            ; TODO: add more types
            [else
             (interp-error (string-append "Unhandled type: "
                                          (tagof arg1)))]))
      (TrueV)
      (FalseV)))

(test (meta-equal (NumV 4) (NumV 4)) (TrueV))
(test (meta-equal (NumV 4) (NumV 5)) (FalseV))
(test (meta-equal (NumV 4) (StrV "test")) (FalseV))
(test (meta-equal (StrV "test") (StrV "test")) (TrueV))
(test (meta-equal (StrV "test") (StrV "asdf")) (FalseV))
  

;(define (fieldc->fieldv (field : FieldP)) : FieldV
  
(define (fields-unique? (fields : (listof FieldC))) : boolean
  (let ([lst (map fieldC-name
                  fields)])
    (= (length lst) (length (remove-duplicates lst)))))

(test (fields-unique? (list
                 (fieldC "x" (NumC 22))
                 (fieldC "x" (NumC 22)))) false)

(define (object-has-field? (obj : ValueC) (field : string)) : boolean
  (type-case (optionof FieldV)
                       (findf {lambda (x) (string=? (fieldV-name x) field)}
                              (ObjectV-fields obj))
                       [some (s) true]
                       [none () false]))
      

(define (interp-fields [fieldcs : (listof FieldC)] [env : Env] [store : Store])
  (cond
    [(empty? fieldcs) (values empty store)]
    [else
     (letrec ([result (interp-full (fieldC-value (car fieldcs))
                                   env store)]
              [fieldv (fieldV (fieldC-name (car fieldcs)) (v*s-value result))])
       (local [(define-values (new-fieldcs new-store) (interp-fields
                                                       (cdr fieldcs)
                                                       env
                                                       (v*s-store result)))]
         (values (cons fieldv
                       new-fieldcs)
                 new-store)))]))
            

(define (2map (fxn : ('a 'b -> 'c)) (l1 : (listof 'a)) (l2 : (listof 'b)))
  : (listof 'c)
  (cond
    [(empty? l1) empty]
    [else
     (cons (fxn (car l1) (car l2))
           (2map fxn (cdr l1) (cdr l2)))]))

#;(define (multi-interp [exprs : (listof ExprC)]
                      [env : Env]
                      [store : Store]) : ((listof ValueC) * Store)
  (cond
    [(empty? exprs) (values empty store)]
    [else
     (local ([define-values (exprps store)
               (multi-interp (cdr exprs) env store)])
       (let ([result (interp-full (car exprs) env store)])
         (values (cons (v*s-value result) exprps)
                 (v*s-store result))))]))

#;(define (multi-interp [exprs : (listof ExprC)]
                      [env : Env]
                      [store : Store]) : (list (listof ValueC) Store)
  (cond
    [(empty? exprs) (list empty store)]
    [else
     (local ([define recur-result
               (multi-interp (cdr exprs) env store)])
       (let ([result (interp-full (car exprs) env (second recur-result))])
         (values (cons (v*s-value result) (first recur-result))
                 (v*s-store result))))]))

(define (multi-interp [exprs : (listof ExprC)]
                      [env : Env]
                      [store : Store]) : ListResult
  (cond
    [(empty? exprs) (vs*s empty store)]
    [else
     (local ([define recur-result
               (multi-interp (cdr exprs) env store)])
       (let ([result (interp-full (car exprs) env (vs*s-store recur-result))])
         (vs*s (cons (v*s-value result) (vs*s-values recur-result))
                 (v*s-store result))))]))



(define (2foldl (fxn : ('a 'b 'c -> 'c)) (init : 'c) (l1 : (listof 'a)) (l2 : (listof 'b)))
  : 'c
  (cond
    [(empty? l1) init]
    [else
     (fxn (car l1) (car l2) (2foldl fxn init (cdr l1) (cdr l2)))
     #;(cons (fxn (car l1) (car l2))
           (2map fxn (cdr l1) (cdr l2)))]))

#;(2foldl {lambda (a b c) (+ a (+ b c))}
        0
        (list 1 2 3 4 5 5)
        (list 1 2 3 4 5 67))





(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
    [NumC (n) (v*s (NumV n) store)]
    [StrC (s) (v*s (StrV s) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]
    [ErrorC (expr) (interp-error (pretty-value (v*s-value
                                                (interp-full expr env store))))]
    [IdC (id)
           (type-case (optionof Location) (id->Location id env)
             [none () (interp-error (string-append
                                     "Unbound identifier: "
                                     (symbol->string id)))]
             [some (v)
                   (type-case (optionof ValueC) (Location->value v store)
                     [none ()
                           (interp-error
                            "Pointer to nothing, variable out of scope?")]
                     [some (v)
                           (v*s v
                                store)])])]
    
    [Set!C (id value)
           (let ([value (interp-full value env store)]
                 [loc (type-case (optionof Location)
                        (id->Location id env)
                        [none ()
                              (interp-error
                               (string-append
                                     "Unbound identifier: "
                                     (symbol->string id)))
                              #;(get-location store)]
                        [some (v) v])])
             (v*s (v*s-value value) (store-replace loc
                                       (v*s-value value)
                                       (v*s-store value))))]
    
    [IfC (cond then e)
         (let ([icond (interp-full cond env store)])
           (interp-full (if (not (FalseV? (v*s-value icond)))
                            then e)
                        env (v*s-store icond)))]
         
         
    [LetC (id bind body)
          (letrec ([interp-bind (interp-full bind env store)]
                   [result (extend-env id (v*s-value interp-bind) env
                                       (v*s-store interp-bind))])
            (interp-full body (e*s-env result)
                         (e*s-store result)))]
    
    [SeqC (e1 e2)
          (let ([results (interp-full e1 env store)])
            (interp-full e2 env (v*s-store results)))]
    
    [Prim1C (op arg)
            (let ([iarg (interp-full arg env store)])
              (case op
                ['print
                 (begin (display (pretty-value (v*s-value iarg)))
                        iarg)]
                ['tagof
                 (v*s (StrV (tagof (v*s-value iarg)))
                      (v*s-store iarg))]))]
    
    [Prim2C (op arg1 arg2)
            (letrec ([iarg1 (interp-full arg1 env store)]
                     [iarg2 (interp-full arg2 env (v*s-store iarg1))])
              (cond
                [(symbol=? op 'string+)
                  (v*s (StrV (string-append
                         (StrV-s (v*s-value iarg1))
                         (StrV-s (v*s-value iarg2))))
                       (v*s-store iarg2))]
                [(symbol=? op '==)
                 (v*s (meta-equal (v*s-value iarg1)
                                  (v*s-value iarg2))
                      (v*s-store iarg2))]
                 
                 [else (let ([varg1 (type-case ValueC (v*s-value iarg1)
                                 [NumV (n) n]
                                 [else (interp-error (string-append
                                                      "Bad argument to "
                                                      (symbol->string op)))])]
                        [varg2 (type-case ValueC (v*s-value iarg2)
                                 [NumV (n) n]
                                 [else (interp-error (string-append
                                                      "Bad argument to "
                                                      (symbol->string op)))])])
                    (v*s ((op->func op) varg1 varg2)
                         (v*s-store iarg2)))]))]
    
    [ObjectC (fields)
             (local [(define-values (new-fields new-store)
                       (interp-fields
                        (if (fields-unique? fields)
                            fields
                            (interp-error "Multiply-defined fields")) env store))]
               (v*s (ObjectV new-fields)
                    #;(map
                       {lambda (fieldc)
                         (type-case FieldC fieldc
                           [fieldC (name value)
                                   (let ([result
                                          (interp-full value env store)])
                                     ;; TODO: what to do with store?
                                     (fieldV name (v*s-value result)))])}
                       (if (fields-unique? fields)
                           fields
                           (interp-error "Multiply-defined fields")))
                    new-store))]
    [GetFieldC (obj field)
               (letrec ([object (interp-full obj env store)]
                        [ifield (interp-full field env (v*s-store object))])
                 (if (not (ObjectV? (v*s-value object)))
                     (interp-error
                      (string-append
                       "Non-object in field lookup: "
                       (pretty-value
                        (v*s-value object))))
                     (if (not (StrV? (v*s-value ifield)))
                         (interp-error (string-append
                                        "Non-string in field lookup: " 
                                        (pretty-value (v*s-value ifield))))
                         (v*s (type-case (optionof FieldV)
                                (findf
                                 {lambda (field)
                                   (string=? (fieldV-name field)
                                             (StrV-s (v*s-value ifield)))}
                                 (ObjectV-fields (v*s-value object)))
                                [none () (interp-error
                                          (string-append
                                           "Field not found: "
                                           (pretty-value (v*s-value ifield))))]
                                [some (v) (fieldV-value v)])
                              
                              (v*s-store ifield))
                         )))]
    [SetFieldC (obj field value)
               (letrec ([object (interp-full obj env store)]
                        [ifield (interp-full field env (v*s-store object))]
                        [ivalue (interp-full value env (v*s-store ifield))])
                 (if (not (ObjectV? (v*s-value object)))
                     (interp-error
                      (string-append
                       "Non-object in field update: "
                       (pretty-value
                        (v*s-value object))))
                     (if (not (StrV? (v*s-value ifield)))
                         (interp-error
                          (string-append
                           "Non-string in field update: "
                           (pretty-value
                            (v*s-value ifield))))
                         (v*s (ObjectV
                               (if (object-has-field?
                                    (v*s-value object)
                                    (StrV-s (v*s-value ifield)))
                                   (map
                                    {lambda (fieldv)
                                      (fieldV
                                       (fieldV-name fieldv)
                                       (if (string=?
                                            (fieldV-name fieldv)
                                            (type-case ValueC (v*s-value ifield)
                                              [StrV (s) s]
                                              [else
                                               (interp-error
                                                (string-append
                                                 "Non-string in field update: "
                                                 (pretty-value
                                                  (v*s-value ifield))))]))
                                           (v*s-value ivalue)
                                           (fieldV-value fieldv)))}
                                    (ObjectV-fields (v*s-value object))
                                    )
                                   (cons (fieldV (StrV-s (v*s-value ifield))
                                                 (v*s-value ivalue))
                                         (ObjectV-fields (v*s-value object)))
                                   ))
                              (v*s-store ivalue)))))]
    
    [FuncC (args body)
          (v*s (ClosureV args body env)
               store)]
    
    [AppC (func args)
          (letrec ([ifunc (interp-full func env store)]
                   [function (v*s-value ifunc)])
            (if (not (ClosureV? function))
                (interp-error
                 (string-append "Applied a non-function: "
                                (pretty-value function)))
                (let ([listresult (multi-interp args (ClosureV-env function)
                                                (v*s-store ifunc))])
                  (if (not (= (length args) (length (ClosureV-args function))))
                      (interp-error "Application failed with arity mismatch")
                      (letrec
                          ([arg-names (ClosureV-args function)]
                           
                           [iargs (vs*s-values listresult)]
                           [new-store (vs*s-store listresult)]
                           [folded-arguments (2foldl
                                              {lambda (name arg envstore)
                                                (extend-env name arg
                                                            (e*s-env envstore)
                                                            (e*s-store envstore))}
                                              (e*s env store)
                                              arg-names iargs)]
                           [new-env (e*s-env folded-arguments)]
                           [final-store (e*s-store folded-arguments)])
                        (interp-full
                         (ClosureV-body function)
                         new-env
                         final-store))))))]
                            
                      
                    
          
    #;[else (interp-error (string-append "Haven't covered a case yet:"
                                       (to-string exprC)))]))


(define subtact-order
  (LetC 'sdfg (NumC 10)
        (LetC 'zxcv (NumC 4)
              (LetC 'qwer (NumC 3)
                    (LetC 'asdf (NumC 2)
                          (IfC
                           (IfC
                            (Prim2C '== (Prim1C 'tagof (IdC 'asdf))
                                    (StrC "number"))
                            (IfC (Prim2C '== (Prim1C 'tagof (IdC 'qwer))
                                         (StrC "number"))
                                 (IfC (Prim2C '== (Prim1C 'tagof (IdC 'zxcv))
                                              (StrC "number"))
                                      (IfC (Prim2C '== (Prim1C 'tagof
                                                               (IdC 'sdfg))
                                                   (StrC "number"))
                                           (TrueC)
                                           (FalseC))
                                      (FalseC))
                                 (FalseC))
                            (FalseC))
                           (Prim2C 'num- (Prim2C 'num- (Prim2C 'num- (IdC 'sdfg)
                                                             (IdC 'zxcv))
                                                (IdC 'qwer))
                                   (IdC 'asdf))
                           (ErrorC (StrC "Bad arguments to -"))))))))

(define (good-interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))

(good-interp subtact-order)
#;(good-interp (GetFieldC
                (NumC 5)
                (StrC "foo")
                ))
(good-interp (SetFieldC
              (ObjectC (list (fieldC "x" (NumC 5))))
              (StrC "y")
              (StrC "y exists")))
(good-interp (GetFieldC (SetFieldC
                         (ObjectC (list (fieldC "x" (NumC 5))))
                         (StrC "y")
                         (StrC "y exists")) (StrC "y")))
(multi-interp (list (NumC 5)) empty empty)
(interp-full (NumC 5) empty empty)


(define (bad-interp [exprC : ExprC]) : ValueC
  (begin (display exprC)
         (FalseV)))

(define interp good-interp)
