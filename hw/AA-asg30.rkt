;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg30) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #30
; Andrew Amis
; 1/10/12
; http://fellowhuman.com/gbk/2012/01/09/prog-1-asg-30-lists-and-more-structs/
; Exercises 107, 109, 111, 112, 115

; Suppose you’re writing a program for trading on the stock market.  Develop a
; data definition for stocks, which includes the company’s name (e.g., “Texas;
; Instruments”), the ticker symbol (e.g., “TXN”), the current “bid” price at
; which people are willing to buy, and the current “ask” price at which owners
; are willing to sell.  (Your definition should, of course, include a template
; for functions that process stock data, and some example data — you may find
; Yahoo! Finance or Google Finance helpful, and you can search for companies by
; entering the company name in the “quotes” search box. Here’s an example
; company listing from Yahoo, and another from Google.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 107 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define planets (list "mercury" "venus" "earth" "mars" "jupiter" "saturn"
                      "uranus" "neptune"))
(define meal (list "eggs" "toast" "orange juice"))
(define colors (list "red" "green" "blue")) ; Don't need any more colors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 109 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A List-of-bools is one of:
; – empty
; – (cons boolean List-of-names)
; interp. a List-of-bools represents a list of boolean values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 111 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
It produces the same answer because they are roughly equivalent. The or
function can be used instead of the cond because or is a special case that
doesn't necessarily evaluate all its arguments.

I prefer the "or" method to the cond because it is shorter and cleaner.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 112 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; contains? : list-of-strings string -> boolean
; Returns true if the given list contains the given string.
(define (contains? list name)
  (cond [(empty? list) false]
        [(cons? list)
         (if (string=? (car list) name)
             true
             (contains? (cdr list) name))]))

(check-expect (contains? (list "a" "b" "a" "c") "a") true)
                          (check-expect (contains? (list "a" "b" "a" "c") "d") false)
                          (check-expect (contains? (list "a" "b" "d" "c") "d") true)
                          
                          
                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 115 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          
                          ;; pos? -> list-of-numbers -> boolean
                          ; Returns whether all the numbers in a list of numbers are positive.
                          (define (pos? lst)
                            (cond [(empty? lst) true]
                                  [(cons? lst)
                                   (if (positive? (car lst))
                                       (pos? (cdr lst))
                                       false)]))
                          
                          (check-expect (pos? (list 1 2 3 4 5 5 3 2 1 3 41 1 4 123 12 3 4 1)) true)
                          (check-expect (pos? (list 1 2 3 4 5 5 3 2 1 3 41 -1 4 123 12 3 4 1)) false)
                          (check-expect (pos? empty) true)
                          
                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Stocks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          
                          ;; stock : string string number number
                          (define-struct stock (name ticker bid ask))
                          ; name : string of the name of the stock
                          ; ticker : string containing ticker symbol
                          ; bid : number, bid price of the stock.
                          ; ask : number, asking price of the stock
                          
                          (define intel (make-stock "Intel Corporation" "INTC" 25.43 25.89))
                          
                          ; Templates
                          #;(define (fun-for-stock stk)
                              (stock-name stk) ... (stock-ticker stk) ...
                              (stock-bid stk) .. (stock-ask stk))
                          
                          #;(define (fun-for-stock stk)
                              (make-stock
                               (stock-name stk)
                               (stock-ticker stk)
                               (stock-bid stk)
                               (stock-ask stk)))