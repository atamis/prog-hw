#lang racket

(define (rpn-parse input)
  (car (rpn-parse-internal input '())))


;; This is a tail-recursive function. We store state and pass it on in the
; "stack" argument.
(define (rpn-parse-internal input stack)
  (if (empty? input) ; When we are finished, return the stack
      stack
      (let ([current (car input)]) ; Get the current item
        (cond ; Deal with the current item
          
          
          [(cons? current) ; If it's a list, recurse
           (rpn-parse-internal
            ; Make sure it doesn't operate on the same input as we did...
            (cdr input)
            ; Add the results from the recursion to the stack
            (cons (rpn-parse current) stack))]
          
          ; If it's a number, add it to the stack and recurse
          [(number? current)
           (rpn-parse-internal
            (cdr input)
            (cons current stack))]
          ; Otherwise, it's a command
          [else
           ; This is a little inflexible. It assumes that
           ; all operations will always need 2 arguments.
           (let* ([x (car stack)]
                  [y (car (cdr stack))])
             (rpn-parse-internal
              (cdr input)
              (cons
               (cond
                 [(symbol=? current '+) (+ x y)]
                 [(symbol=? current '-) (- x y)]
                 [(symbol=? current '*) (* x y)]
                 [(symbol=? current '/) (/ x y)]
                 [(symbol=? current '^) (expt x y)]
                 [else (current x y)])
               ; We cddr the stack to remove the numbers we operated on.
               (cddr stack))))]))))


#;(provide
   rpn-parse
   rp)
