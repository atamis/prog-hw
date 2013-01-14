#lang racket

(provide build-begin
         wrap wrap6 wrap9 unwrap)

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

;; wrap9: wrap an <Exp> of Lang9 in the required let-form:
(define-syntax-rule (wrap9 e)
  `(let ()
     (begin (define-syntax free
              (syntax-rules ()
                [(_ (FVar (... ...)) Expr)
                 Expr])))
     e))

;; wrap10:
(define-syntax-rule (wrap10 e)
  `(let ()
     (begin
       ;; iota : Nat -> Listof[Nat]
       ;; Produces the list of natural numbers less than n, in ascending order.
       (define iota (curryr build-list identity))
       
       ;; Representation of a closure:
       ;;   (procval-code pv) refers to the body of the closure
       ;;   (procval-env  pv) refers to a vector representing the captured
       ;;                     environment's vars that occur free in the code
       (define-struct procval (code env) #:transparent)
       
       (define-syntax-rule (anonymous-call e0 e1 (... ...))
         (let ([t e0]) ((procval-code t) t e1 (... ...))))
       
       (define-syntax bind-free
         (lambda (x)
           (syntax-case x ()
             [(_ (cp fv (... ...)) body)
              (with-syntax ([(i (... ...)) (iota (length #'(fv (... ...))))])
                #'(let ()
                    (define-syntax fv
                      (syntax-id-rules ()
                        [fv (vector-ref (procval-env cp) i)]))
                    (... ...)
                    body))])))
       (define-syntax fill-closure
         (lambda (x)
           (syntax-case x ()
             [(_ cl) #'(void)]
             [(_ cl free (... ...))
              (with-syntax ([(i (... ...))
                             (iota (length #'(free (... ...))))])
                #'(begin
                    (vector-set! (procval-env cl) i free)
                    (... ...)))])))
       (define-syntax closure
         (lambda (x)
           (syntax-case x ()
             [(_ code free (... ...))
              (with-syntax ([n (length #'(free (... ...)))])
                #'(let ([cl
                         (make-procval code (make-vector n))])
                    (fill-closure cl free (... ...))
                    cl))])))
       (define-syntax closure-letrec
         (lambda (x)
           (syntax-case x ()
             [(_ ((cl (closure code free (... ...))) (... ...)) body)
              (with-syntax ([(n (... ...))
                             (map length #'((free (... ...)) (... ...)))])
                #'(let ([cl (make-procval code (make-vector n))]
                        (... ...))
                    (fill-closure cl free (... ...))
                    (... ...)
                    body))])))
       )
     e))

;; unwrap : Prog -> Exp
;; Remove the definition wrapper from a program; i.e., transform
;;   (let () <Defs> ... <Exp>)
;; into
;;   <Exp>
(define (unwrap prog)
  (match prog
    [(list 'let '() stx-def e) e]))

