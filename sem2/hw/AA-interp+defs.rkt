;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-interp+defs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #17: Interpreters, Part 2
; Andrew Amis
; Started: 4.25.12
; Ended: 4.26.12
; http://fellowhuman.com/gbk/2012/04/17/prog-2-asg-interpreter-with-defs/
; 17.7.1 through 17.7.4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.7.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An expression is one of...
;       number
;       symbol representing a variable
;       (make-add expression expression)
;       (make-sub expression expression)
;       (make-mul expression expression)
;       (make-div expression expression)
;       (make-fun symbol expression)

(define-struct add (left right))
(define-struct sub (left right))
(define-struct mul (left right))
(define-struct div (left right))
(define-struct fun (name expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.7.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a function (fun) is a struct...
(define-struct function (name argument body))
;; where
; name is a symbol representing the name of the function
; argument is the name of the argument being passed
; and body is an expression

; (define (f x) (+ 3 x))
(define f (make-function 'f 'x (make-add 3 'x)))

; (define (g x) (* 3 x))
(define g (make-function 'g 'x (make-mul 3 'x)))

; (define (h u) (f (* 2 u)))
(define h (make-function 'h 'u (make-mul 2 'u)))

; (define (i v) (+ (* v v) (* v v)))
(define y (make-function 'y 'v (make-mul 'v 'v)))

; (define (k w) (* (h w) (i w)))
(define k (make-function 'k 'w (make-fun 'y 'w)))

; (define (ident x) x)
(define ident (make-function 'ident 'x 'x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.7.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-left : expr -> expr
; Returns the left hand of an expression
(define (expr-left expr)
  (cond
    [(add? expr) (add-left expr)]
    [(sub? expr) (sub-left expr)]
    [(mul? expr) (mul-left expr)]
    [(div? expr) (div-left expr)]))

(check-expect (expr-left (make-add 1 10)) 1)
(check-expect (expr-left (make-sub 2 9)) 2)
(check-expect (expr-left (make-mul 3 8)) 3)
(check-expect (expr-left (make-div 4 7)) 4)


;; expr-right : expr -> expr
; Returns the right hand of an expression
(define (expr-right expr)
  (cond
    [(add? expr) (add-right expr)]
    [(sub? expr) (sub-right expr)]
    [(mul? expr) (mul-right expr)]
    [(div? expr) (div-right expr)]))

(check-expect (expr-right (make-add 1 10)) 10)
(check-expect (expr-right (make-sub 2 9)) 9)
(check-expect (expr-right (make-mul 3 8)) 8)
(check-expect (expr-right (make-div 4 7)) 7)

(define expr1 (make-add 10 -10))
(define expr2 (make-add (make-mul 20 3) 33))
(define expr3 (make-mul 3.14 (make-mul 'r 'r)))
(define expr4 (make-add (make-mul 9/5 'c) 32))
(define expr5 (make-add (make-mul 3.14 (make-mul 'o 'o))
                        (make-mul 3.14 (make-mul 'i 'i))))

;; apply-expr : {expr -> expr} expr -> expr
; Applies the given function to each side of the expression, returning a new
; expression
(define (apply-expr fxn expr)
  (cond
    [(number? expr) (fxn expr)]
    [(symbol? expr) (fxn expr)]
    [(add? expr) (make-add (fxn (expr-left expr))
                           (fxn (expr-right expr)))]
    [(sub? expr) (make-sub (fxn (expr-left expr))
                           (fxn (expr-right expr)))]
    [(mul? expr) (make-mul (fxn (expr-left expr))
                           (fxn (expr-right expr)))]
    [(div? expr) (make-div (fxn (expr-left expr))
                           (fxn (expr-right expr)))]
    [(fun? expr) (make-fun (fun-name expr)
                           (fxn (fun-expr expr)))]))
(check-expect (apply-expr add1 1) 2)
(check-expect (apply-expr symbol? 'a) true) ; Tricksy hobbits
(check-expect (apply-expr add1 (make-add 1 10)) (make-add 2 11))
(check-expect (apply-expr add1 (make-sub 2 9)) (make-sub 3 10))
(check-expect (apply-expr add1 (make-mul 3 8)) (make-mul 4 9))
(check-expect (apply-expr add1 (make-div 4 7)) (make-div 5 8))

;; subst : symbol number expression -> expression
; Replaces the given symbol with the given number in the given expression
(define (subst sym num expr)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (if (symbol=? sym expr)
                        num
                        expr)]
    [(expr? expr) (apply-expr (lambda (expr) (subst sym num expr)) expr)]))

(check-expect (subst 'r 3 expr3) (make-mul 3.14 (make-mul 3 3)))
(check-expect (subst 'c 10 expr4) (make-add (make-mul 9/5 10) 32))
(check-expect (subst 'o 5 expr5) (make-add (make-mul 3.14 (make-mul 5 5))
                                           (make-mul 3.14 (make-mul 'i 'i))))
(check-expect (subst 'i 2 (subst 'o 5 expr5))
              (make-add (make-mul 3.14 (make-mul 5 5))
                        (make-mul 3.14 (make-mul 2 2))))

;; expr? : any -> boolean
; Determines whether this thing is an expression
(define (expr? x)
  (or (number? x)
      (symbol? x)
      (add? x)
      (sub? x)
      (mul? x)
      (div? x)
      (fun? x)))
(check-expect (expr? 1) true)
(check-expect (expr? false) false)
(check-expect (expr? (make-add 1 10)) true)
(check-expect (expr? (make-sub 2 9)) true)
(check-expect (expr? (make-mul 3 8)) true)
(check-expect (expr? (make-div 4 7)) true)
(check-expect (expr? "test") false)
(check-expect (expr? 'test) true)

;; evaluate-with-one-def : numeric-expression function -> number
; Evaluates the given numeric expression.
(define (evaluate-with-one-def expr fun)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (error "Not a numeric expression")]
    [(add? expr) (+ (evaluate-with-one-def (expr-left expr) fun)
                    (evaluate-with-one-def (expr-right expr) fun))]
    [(sub? expr) (- (evaluate-with-one-def (expr-left expr) fun)
                    (evaluate-with-one-def (expr-right expr) fun))]
    [(mul? expr) (* (evaluate-with-one-def (expr-left expr) fun)
                    (evaluate-with-one-def (expr-right expr) fun))]
    [(div? expr) (/ (evaluate-with-one-def (expr-left expr) fun)
                    (evaluate-with-one-def (expr-right expr) fun))]
    [(fun? expr) (if (symbol=? (function-name fun) (fun-name expr))
                     (evaluate-with-one-def
                      (subst (function-argument fun)
                             (evaluate-with-one-def (fun-expr expr) fun)
                             (function-body fun))
                      fun)
                     (error "Undefined function"))]))

(check-expect (evaluate-with-one-def expr1 ident) 0)
(check-expect (evaluate-with-one-def expr2 ident) 93)
(check-expect (evaluate-with-one-def (make-sub 4 1) ident) 3)
(check-expect (evaluate-with-one-def (make-div 4 2) ident) 2)
(check-error (evaluate-with-one-def expr3 ident) "Not a numeric expression")
(check-expect (evaluate-with-one-def (make-fun 'f 3) f) 6)
(check-error (evaluate-with-one-def (make-fun 'g 3) f) "Undefined function")
(check-expect (evaluate-with-one-def (make-fun 'y 3) y) 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 17.7.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-first : {X -> boolean} listof[X] -> X
; Finds the first item in the list for which the function returns true.
(define (find-first fxn list)
  (cond
    [(empty? list) (error "Not found")]
    [(cons? list)
     (if (fxn (car list))
         (car list)
         (find-first fxn (cdr list)))]))

(check-expect (find-first even? '(5 7 9 3 4)) 4) 
(check-expect (find-first odd? '(5 7 9 3 4)) 5)
(check-error (find-first boolean? '(5 7 9 3 4)) "Not found")

;; evaluate-with-defs : expression listof[functions] -> number
; Evaluates the expression with all the definitions.
(define (evaluate-with-defs expr functions)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (error "Not a numeric expression")]
    [(add? expr) (+ (evaluate-with-defs (expr-left expr) fun)
                    (evaluate-with-defs (expr-right expr) fun))]
    [(sub? expr) (- (evaluate-with-defs (expr-left expr) fun)
                    (evaluate-with-defs (expr-right expr) fun))]
    [(mul? expr) (* (evaluate-with-defs (expr-left expr) fun)
                    (evaluate-with-defs (expr-right expr) fun))]
    [(div? expr) (/ (evaluate-with-defs (expr-left expr) fun)
                    (evaluate-with-defs (expr-right expr) fun))]
    [(fun? expr) 
     (let [(fun
            (find-first {λ (x) (symbol=? (function-name x)
                                         (fun-name expr))}
                        functions))]
       (evaluate-with-defs
        (subst (function-argument fun)
               (evaluate-with-defs (fun-expr expr) functions)
               (function-body fun))
        functions))]))

(define functions (list f g h y k ident))

(check-expect (evaluate-with-defs expr1 functions) 0)
(check-expect (evaluate-with-defs expr2 functions) 93)
(check-expect (evaluate-with-defs (make-sub 4 1) functions) 3)
(check-expect (evaluate-with-defs (make-div 4 2) functions) 2)
(check-error (evaluate-with-defs expr3 ident) "Not a numeric expression")
(check-expect (evaluate-with-defs (make-fun 'f 3) functions) 6)
(check-expect (evaluate-with-defs (make-fun 'g 3) functions) 9)
(check-expect (evaluate-with-defs (make-fun 'y 3) functions) 9)
(check-expect (evaluate-with-defs (make-fun 'k 3) functions) 9)
(check-error (evaluate-with-defs (make-fun 'does-not-exist 3)
                                 functions) "Not found")