#lang plai-typed

(require "typed-lang.rkt")

(define (car (l : (listof 'a))) : 'a
             (first l))
(define (cdr (l : (listof 'a))) : 'a
             (rest l))


(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "var-" (to-string n))))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof ExprC))
                      (body : ExprC)) : ExprC
  (cond [(empty? ids) body]
        [(cons? ids)
         (LetC (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))

;; check-type builds an expression that checks the type of the expression
;; given as an argument
(define (check-type (expr : ExprC) (type : string)) : ExprC
  (Prim2C '== (Prim1C 'tagof expr) (StrC type)))

;; and builds up an and expression from its two pieces
(define (and (expr1 : ExprC) (expr2 : ExprC)) : ExprC
  (IfC expr1 expr2 (FalseC)))

;; all builds up a series of ands over the expression arguments
(define (all (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (exp result) (and exp result)) (TrueC) exprs))

;; map-subtract builds an expression that maps 'num- over a list of expressions
(define (map-subtract (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C 'num- result expr)) (first exprs) (rest exprs)))


(define (desugar-subtract (args : (listof ExprP))) : ExprC
  (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
      (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
           (map-subtract id-exps)
           (ErrorC (StrC "Bad arguments to -"))))))


(define (desugar-field (field : FieldP)) : FieldC
  (fieldC (fieldP-name field) (desugar (fieldP-value field))))

(define (LHS-object (lhs : LHS)) : ExprP
  (type-case LHS lhs
    [BracketLHS (obj field)
                obj]
    [DotLHS (obj field)
            obj]
    [IdLHS (id)
           (error 'lhs "IdLHS has no object")]))

(define (LHS-field (lhs : LHS)) : ExprC
  (type-case LHS lhs
    [BracketLHS (obj field)
                (desugar field)]
    [DotLHS (obj field)
            (StrC (symbol->string field))]
    [IdLHS (id)
           (StrC (symbol->string id))]))

;; Takes a symbol for the operator and a list of arguments, and desugars them into a
; nested series of Prim2Cs with the given symbol as their operator.
(define (prim-desugar (op : symbol) (args : (listof ExprC))) : ExprC
  (cond
    [(empty? args) (ErrorC (StrC "Empty list for prim op"))]
    [(= (length args) 2)
     (let ([des-car-args (car args)])
       (Prim2C op
               des-car-args (second args)))]
    [else
     (let ([des-car-args (car args)])
       (Prim2C op des-car-args
               (prim-desugar op (cdr args))))]))
  
(prim-desugar 'num+ (map NumC (list 5 4 3)))
  
  
(define (working-desugar (exprP : ExprP)) : ExprC
    (type-case ExprP exprP
      
      [NumP (n) (NumC n)]
      [StrP (s) (StrC s)]
      [TrueP () (TrueC)]
      [FalseP () (FalseC)]
      [IdP (name)
           (IdC name)]
      ;; Fill in more cases here...
      
      #;[PrimP (op args)
               (case op
                 ['- (cond
                       [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
                       [(< 0 (length args)) (desugar-subtract args)])])]
      [SeqP (es)
            (cond
              [(= (length es) 1)
               (SeqC (TrueC) (desugar (car es)))]
              [else
               (SeqC (desugar (car es)) (desugar (SeqP (cdr es))))])]
      
      [WhileP (test body)
              ;; dummy-fun will tell us it was called if we do so accidentally
              (local ([define dummy-fun (FuncC (list) (ErrorC (StrC "Dummy function")))])
                (IfC (desugar test)
                     
                     ;; while-var will hold the actual function once we tie
                     ;; everything together
                     (LetC 'while-var dummy-fun
                           (LetC 'while-func
                                 
                                 ;; this function does the real work - it runs the body of
                                 ;; the while loop, then re-runs it if the test is false, and
                                 ;; stops if its true
                                 (FuncC (list)
                                        (LetC 'temp-var
                                              (desugar body)
                                              (IfC (desugar test)
                                                   (AppC (IdC 'while-var) (list))
                                                   (IdC 'temp-var))))
                                 
                                 ;; The Set!C here makes sure that 'while-var will resolve
                                 ;; to the right value later, and the AppC kicks things off
                                 (SeqC (Set!C 'while-var (IdC 'while-func))
                                       (AppC (IdC 'while-var) (list)))))
                     
                     (FalseC)))]
      [ObjectP (fields)
               (ObjectC (map desugar-field
                             fields))]
      [DotP (obj field)
            (GetFieldC (desugar obj) (StrC (symbol->string field)))]
      [BracketP (obj field)
                (GetFieldC (desugar obj) (desugar field))]
      [AssignP (lhs value)
               (SetFieldC (desugar (LHS-object lhs))
                          (LHS-field lhs) (desugar value))]
      
      [PrimAssignP (op lhs value)
                   (let ([id (string->symbol (StrC-s (LHS-field lhs)))])
                     (Set!C id
                            (desugar 
                             (PrimP op (list (IdP id) value)))))]
      
      [PrimP (op args)
             (cond
               [(symbol=? op '-)
                (cond
                  [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
                  [(< 0 (length args)) (desugar-subtract args)])]
               [(or (symbol=? op '<)
                    (symbol=? op '>))
                (if (> (length args) 2)
                    (ErrorC (StrC "Bad primop"))
                    (Prim2C op (desugar (first args)) (desugar (second args))))]
               [(symbol=? op '+)
                (let ([dargs (map desugar args)])
                  (LetC 'first-arg (first dargs)
                        (IfC (Prim2C '== (Prim1C 'tagof (IdC 'first-arg))
                                     (StrC "number"))
                             (if (= (length dargs) 2)
                                 (Prim2C '+num (IdC 'first-arg)
                                         (second dargs))
                                 (Prim2C '+num (IdC 'first-arg)
                                         (prim-desugar '+num (rest dargs))))
                             (if (= (length dargs) 2)
                                 (Prim2C '+str (IdC 'first-arg)
                                         (second dargs))
                                 (Prim2C '+str (IdC 'first-arg)
                                         (prim-desugar '+num (rest dargs)))))))]
                              
                              
                
                [else
                 (cond
                  [(empty? args) (ErrorC (StrC "Empty list for prim op"))]
                  [(= (length args) 1)
                   (Prim1C op (desugar (car args)))]
                  [(= (length args) 2)
                   (let ([des-car-args (desugar (car args))])
                     (Prim2C (convert-op op des-car-args)
                             des-car-args (desugar (second args))))]
                  [else
                   (let ([des-car-args (desugar (car args))])
                     (Prim2C (convert-op op des-car-args) des-car-args
                             (desugar (PrimP op (cdr args)))))])])]
      
      [DefvarP (id bind body)
               (LetC id (desugar bind) (desugar body))]
      
      [else (ErrorC (StrC (string-append "Haven't desugared a case yet:\n"
                                         (to-string exprP))))]))

(define (broken-desugar (exprP : ExprP)) : ExprC
  (ErrorC (StrC (string-append "Haven't desugared a case yet:\n"
                               (to-string exprP)))))

(define desugar working-desugar)

(desugar (BracketP (ObjectP (list (fieldP "awesome" (NumP 4)))) (StrP "awesome")))
(desugar (BracketP (SeqP (list (ObjectP (list (fieldP "awesome" (NumP 4))))))
                   (StrP "awesome")))

(test (desugar (ObjectP (list (fieldP "awesome" (NumP 4)))))
      (ObjectC (list (fieldC "awesome" (NumC 4)))))
(test (desugar (DotP (ObjectP (list (fieldP "awesome" (NumP 4)))) 'awesome))
      (GetFieldC (ObjectC (list (fieldC "awesome" (NumC 4)))) (IdC 'awesome)))
(test (desugar (BracketP (ObjectP (list (fieldP "awesome" (NumP 4)))) (StrP "awesome")))
      (GetFieldC (ObjectC (list (fieldC "awesome" (NumC 4)))) (IdC 'awesome)))

(desugar (DotP (ObjectP (list (fieldP "x" (NumP 5)))) 'x))
(desugar (PrimP '+ (map {lambda (nump) (PrimP 'print (list nump))}
                    (map NumP (list 5 4 3)))))
(desugar (PrimP '+ (map NumP (list 5 4 3))))
(desugar (PrimP '+ (list (NumP 1) (NumP 2))))
(NumC? (NumC 4))