;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; insert : number lon -> lon
; Inserts n into lst sucht that n <= the rest of the numbers producing a sorted
; list
(define (insert n lst)
  (cond
    [(empty? lst) (list n)]
    [(cons? lst)
     (if (<= n (first lst))
         (cons n lst)
         (cons (first lst) (insert n (rest lst))))]))
         
(check-expect (insert 3 '(4 5 6)) '(3 4 5 6))
(check-expect (insert 3 '(2 4 5 6)) '(2 3 4 5 6))
(check-expect (insert 5 empty) '(5))

;; sort-lon : lon -> lon
; Sorts the lon in ascending order
(define (sort-lon lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst)
     (insert (car lst)
             (sort-lon (cdr lst)))]))

(check-expect (sort-lon '(5 7 2 9 3 -1 6))
              '(-1 2 3 5 6 7 9))
(check-expect (sort-lon empty)
              empty)
(check-expect (sort-lon '(3))
              '(3))


