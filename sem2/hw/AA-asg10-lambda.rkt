;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-asg10-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #10: lambda
; Andrew Amis
; 3.20.12
; http://fellowhuman.com/gbk/2012/03/19/prog-2-asg-10-lambda/

;; any-over-5? : listof[number] -> boolean
; Returns true if the list contains a number greater than 5.
(define (any-over-5? lon)
  (ormap (λ (x) (> x 5)) lon))

(check-expect (any-over-5? '(1 2 3 4 1 2 3 4 5 2 1)) false)
(check-expect (any-over-5? '(1 2 3 4 1 2 3 4 5 2 21)) true)
(check-expect (any-over-5? '(1 2 3 4 1 2 3 4 5 2 6)) true)


;; all-shorter-than-10? : listof[string] -> boolean
; Returns true if all the strings in a list are shorter than 10.
(define (all-shorter-than-10? strings)
  (andmap (λ (string) (< (string-length string) 10)) strings))

(check-expect (all-shorter-than-10? '("123467890"
                                      "123467890"
                                      "123467890"
                                      "123467890"
                                      "123467890")) true)
(check-expect (all-shorter-than-10? '("123467890"
                                      "123467890"
                                      "123467890"
                                      "123467890"
                                      "123467890d")) false)



;; flip : {X Y -> Z} -> {Y X -> Z}
; Takes a function and returns that function with the arguments reversed.
(define (flip fxn)
  (lambda (y x) (fxn x y)))
(check-expect ((flip cons) '(1 2 3 4) 10) '(10 1 2 3 4))
(check-expect ((flip +) 3 10) 13)
(check-expect ((flip /) 2 10) 5)

;; negate : {X -> boolean} -> {X -> boolean}
; Takes a function which returns a boolean and negates it, returning true when
; false, false when true, etc.
(define (negate fxn)
  (lambda (x) (not (fxn x))))
(check-expect ((negate boolean?) true) false)
(check-expect ((negate string?) "test") false)
(check-expect ((negate string?) 3) true)