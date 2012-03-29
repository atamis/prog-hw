;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-arithmetic-eval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #13: Evaluating Arithmetic
; Andrew Amis
; Started: 3.28.11
; Ended: 3.29.11
; http://fellowhuman.com/gbk/2012/03/27/prog-2-asg-evaluating-arithmetic/



;; An expression is one of...
;       number
;       symbol representing a variable
;       (make-add expression expression)
;       (make-sub expression expression)
;       (make-mul expression expression)
;       (make-div expression expression)

(define-struct add (left right))
(define-struct sub (left right))
(define-struct mul (left right))
(define-struct div (left right))

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

;; expr? : any -> boolean
; Determines whether this thing is an expression
(define (expr? x)
  (or (number? x)
      (symbol? x)
      (add? x)
      (sub? x)
      (mul? x)
      (div? x)))
(check-expect (expr? 1) true)
(check-expect (expr? false) false)
(check-expect (expr? (make-add 1 10)) true)
(check-expect (expr? (make-sub 2 9)) true)
(check-expect (expr? (make-mul 3 8)) true)
(check-expect (expr? (make-div 4 7)) true)
(check-expect (expr? "test") false)
(check-expect (expr? 'test) true)


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
                           (fxn (expr-right expr)))]))
(check-expect (apply-expr add1 1) 2)
(check-expect (apply-expr symbol? 'a) true) ; Tricksy hobbits
(check-expect (apply-expr add1 (make-add 1 10)) (make-add 2 11))
(check-expect (apply-expr add1 (make-sub 2 9)) (make-sub 3 10))
(check-expect (apply-expr add1 (make-mul 3 8)) (make-mul 4 9))
(check-expect (apply-expr add1 (make-div 4 7)) (make-div 5 8))

;; Template
#;(define (fun-for-expr expr)
    (cond
      [(add? expr) (expr-left expr)
                   (expr-right expr)]
      [(sub? expr) (expr-left expr)
                   (expr-right expr)]
      [(mul? expr) (expr-left expr)
                   (expr-right expr)]
      [(div? expr) (expr-left expr)
                   (expr-right expr)]))

#;(define (fun-for-expr expr)
    (cond
      [(number? expr) ...]
      [(symbol? expr) ...]
      [(expr? expr) (expr-left expr)
                    (expr-right expr)]))

#;(define (fun-for-expr expr)
    (cond
      [(number? expr) ...]
      [(symbol? expr) ...]
      [(add? expr) (expr-left expr)
                   (expr-right expr)]
      [(sub? expr) (expr-left expr)
                   (expr-right expr)]
      [(mul? expr) (expr-left expr)
                   (expr-right expr)]
      [(div? expr) (expr-left expr)
                   (expr-right expr)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.4.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define expr1 (make-add 10 -10))
(define expr2 (make-add (make-mul 20 3) 33))
(define expr3 (make-mul 3.14 (make-mul 'r 'r)))
(define expr4 (make-add (make-mul 9/5 'c) 32))
(define expr5 (make-add (make-mul 3.14 (make-mul 'o 'o))
                        (make-mul 3.14 (make-mul 'i 'i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.4.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; numeric? : expression -> boolean
; Checks whether an expression is numeric, that is, does not contain any
; variables.
(define (numeric? expr)
  (cond
    [(number? expr) true]
    [(symbol? expr) false]
    [(expr? expr) (and (numeric? (expr-left expr))
                       (numeric? (expr-right expr)))]))

(check-expect (numeric? expr1) true)
(check-expect (numeric? expr2) true)
(check-expect (numeric? expr3) false)
(check-expect (numeric? expr4) false)
(check-expect (numeric? expr5) false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.4.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a numeric-expression is...
; an expression for which the function numeric? returns true, or an expression
; that contains no variables.

;; evaluate-expression : numeric-expression -> number
; Evaluates the given numeric expression.
(define (evaluate-expression expr)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (error "Not a numeric expression")]
    [(add? expr) (+ (evaluate-expression (expr-left expr))
                    (evaluate-expression (expr-right expr)))]
    [(sub? expr) (- (evaluate-expression (expr-left expr))
                    (evaluate-expression (expr-right expr)))]
    [(mul? expr) (* (evaluate-expression (expr-left expr))
                    (evaluate-expression (expr-right expr)))]
    [(div? expr) (/ (evaluate-expression (expr-left expr))
                    (evaluate-expression (expr-right expr)))]))

(check-expect (evaluate-expression expr1) 0)
(check-expect (evaluate-expression expr2) 93)
(check-expect (evaluate-expression (make-sub 4 1)) 3)
(check-expect (evaluate-expression (make-div 4 2)) 2)
(check-error (evaluate-expression expr3) "Not a numeric expression")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.4.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An S-expression (s-exp) is either
;; a number, a symbol, a string, a boolean, or a listof[s-exp].
;;
;; A listof[s-exp] is either empty or (cons s-exp listof[s-exp]).


;; sexp->expr : s-exp -> expression
; Converts a given sexp to an expression.
(define (sexp->expr sexp)
  (cond
    [(number? sexp) sexp]
    [(or (equal? sexp 'false)
         (equal? sexp 'true)
         (string? sexp))
     (error "Includes non-exprs in s-exp")]
    [(symbol? sexp) sexp]
    [(cons? sexp)
     (let ([op (car sexp)]
           [left (sexp->expr (cadr sexp))]
           [right (sexp->expr (caddr sexp))])
       (cond
         [(symbol=? op '+) (make-add left right)]
         [(symbol=? op '-) (make-sub left right)]
         [(symbol=? op '*) (make-mul left right)]
         [(symbol=? op '/) (make-div left right)]))]))

(check-expect (sexp->expr 3) 3)
(check-expect (sexp->expr 'x) 'x)
(check-expect (sexp->expr '(+ 1 1)) (make-add 1 1))
(check-expect (sexp->expr '(- 1 1)) (make-sub 1 1))
(check-expect (sexp->expr '(* 1 1)) (make-mul 1 1))
(check-expect (sexp->expr '(/ 1 1)) (make-div 1 1))
(check-expect (sexp->expr '(+ 10 -10)) expr1)
(check-expect (sexp->expr '(+ (* 20 3) 33)) expr2)
(check-expect (sexp->expr '(* 3.14 (* r r))) expr3)
(check-expect (sexp->expr '(+ (* 9/5 c) 32)) expr4)
(check-expect (sexp->expr '(+ (* 3.14 (* o o)) (* 3.14 (* i i)))) expr5)

(check-error (sexp->expr '(+ false 1)) "Includes non-exprs in s-exp")


;; expr->sexp : expression -> s-exp
; Converts an expression into a s-exp
(define (expr->sexp expr)
  (cond
    [(number? expr) expr]
    [(symbol? expr) expr]
    [else
     (let ([left (expr->sexp (expr-left expr))]
           [right (expr->sexp (expr-right expr))])
       (list (cond
               [(add? expr) '+]
               [(sub? expr) '-]
               [(mul? expr) '*]
               [(div? expr) '/])
             left
             right))]))
(check-expect (expr->sexp 1) 1)
(check-expect (expr->sexp 'a) 'a)
(check-expect (expr->sexp expr1) '(+ 10 -10))
(check-expect (expr->sexp expr2) '(+ (* 20 3) 33))
(check-expect (expr->sexp expr3) '(* 3.14 (* r r)))
(check-expect (expr->sexp expr4) '(+ (* 9/5 c) 32))
(check-expect (expr->sexp expr5) '(+ (* 3.14 (* o o)) (* 3.14 (* i i))))
(check-expect (expr->sexp (make-sub 10 -10)) '(- 10 -10))
(check-expect (expr->sexp (make-div 10 -10)) '(/ 10 -10))