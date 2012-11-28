#lang typed/racket

(provide empty-env extend-env lookup)

(define-type (Env A B) (Listof (Pairof A B)))

(: empty-env (All (A B) (-> (Env A B))))
(define (empty-env) '())

(: extend-env (All (A B) ((Listof A) (Listof B) (Env A B) -> (Env A B))))
(define (extend-env vars vals env)
  (if (= (length vars) (length vals))
      (append ((inst map (Pairof A B) A B) cons vars vals) env)
      (error 'extend-env
             "expected equal quantities of vars and values; given ~s and ~s"
             vars vals)))

(: lookup (All (A B) (A (Env A B) -> (Option B))))
(define (lookup var env)
  (cond
    [(assq var env) => cdr]
    [else #f]))