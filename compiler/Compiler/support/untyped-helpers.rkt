#lang racket

(provide build-begin
         wrap wrap6 unwrap)

;; NEListof[Expr] -> Expr
;; Converts the list of exprs to a begin-expression with no immediately-nested
;; begin-exprs, or to a single NB-exp if there is only one expr in exprs.
(define (build-begin exprs)
  (let ([es (foldr cons-or-append empty exprs)])
    (if (empty? (rest es))
        (first es)
        (cons 'begin es))))

;; Expr Listof[Expr] -> NEListof[Expr]
(define (cons-or-append e ls)
  (match e
    [(list 'begin e* ...)
     (append e* ls)]
    [e
     (cons e ls)]))

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

;; unwrap : Prog -> Exp
;; Remove the definition wrapper from a program; i.e., transform
;;   (let () <Defs> ... <Exp>)
;; into
;;   <Exp>
(define (unwrap prog)
  (match prog
    [(list 'let '() stx-def e) e]))

