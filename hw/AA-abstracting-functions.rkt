;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname AA-abstracting-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #7: Abstracting Functions
; Andrew Amis
; Started: 3/7/12
; Ended: ?
; http://fellowhuman.com/gbk/2012/03/05/prog-2-asg-7-abstracting-functions/
; Exercises 188, 189, and 190, with modifications.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 188 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter predicate l)
  (cond
    [(empty? l) empty]
    [else (cond
            [(predicate (first l))
             (cons (first l) (filter predicate (rest l)))]
            [else
             (filter predicate (rest l))])]))

;; small : lon number