;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-2compound) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #16: Two Compound Inputs
; Andrew Amis
; Started: 4.18.12
; Ended: ?
; http://fellowhuman.com/gbk/2012/04/16/prog-2-asg-two-compound-inputs/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; phone-record is a struct...
(define-struct phone-record (name number))
; representing a record in a phone book in which...
;   name is a string
;   number is a number

;; zip : listof[string] listof[number] -> listof[phone-record]
; Takes a list of strings and a list of numbers and produces a list of
; phone-records made by combining the first item of the list of names
; with the first item from the list of numbers, etc.
(define (zip slist nlist)
  (cond
    [(empty? slist) empty]
    [(cons? slist)
     (cons
      (make-phone-record (car slist) (car nlist))
      (zip (cdr slist) (cdr nlist)))]))

(check-expect (zip empty empty) empty)
(check-expect (zip (list "test") (list 4)) (list (make-phone-record "test" 4)))
(check-expect (zip (list "test" "awesome" "asdf")
                   (list 4 9 3))
              (list (make-phone-record "test" 4)
                    (make-phone-record "awesome" 9)
                    (make-phone-record "asdf" 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.6.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct card (id hours))
(define-struct worker (id name wage))
(define-struct wage (id wage))

;; hours->wages2 : listof[worker] listof[card] -> listof[wage]
; Computes wages for each employee
(define (hours->wages2 workers cards)
  (letrec ([fxn (λ (workers cards)
                  cards)])
  (fxn (sort workers (λ (a b) (< (worker-id a) (worker-id b))))
       (sort cards (λ (a b) (< (card-id a) (card-id b)))))))
(hours->wages2 (list (make-worker 1 "Alice" 40)
                     (make-worker 2 "Bob" 40)
                     (make-worker 3 "Carol" 45))
               (list (make-card 3 100)
                     (make-card 1 50)
                     (make-card 2 75)))
(check-expect (hours->wages2 (list (make-worker 1 "Alice" 40)
                                   (make-worker 2 "Bob" 40)
                                   (make-worker 3 "Carol" 45))
                             (list (make-card 3 100)
                                   (make-card 1 50)
                                   (make-card 2 75)))
              (list (make-wage 1 2000)
                    (make-wage 2 3750)
                    (make-wage 3 4500)))
