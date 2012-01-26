;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname all_hallows_eve) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Unnumbered assignment
; Andrew Amis
; 10.31.11 (All Hallow's Eve)
; Handout

(define movie-cost 180) ; Flat cost of one showing of one movie
(define customer-cost 0.04) ; Cost of 1 customer of a movie

;; customers : number -> number
; Takes a number representing the cost of the ticket and returns the number of
; customers. Cost should be in $.
(define (customers price)
  (+ 120 (* (/ -15 0.1) (- price 5))))

(check-expect (customers 5) 120)
(check-expect (customers 5.2) 90)
(check-expect (customers 4.5) 195)
(check-expect (customers 4) 270)

;; income : number -> number
; Takes the cost of a ticket and returns the projected gross income.
(define (income price)
  (* price (customers price)))

(check-within (income 5.00) 600 0.1)
(check-within (income 5.20) 468 0.1)
(check-within (income 4.5) 877.5 0.1)
(check-within (income 4) 1080 0.1)

;; cost : number -> number
; Takes number of customers and returns the cost of the showing.
(define (cost customers)
  (+ (* customer-cost customers) movie-cost))
     
(check-within (cost 120) 184.8 0.01)
(check-within (cost 90) 183.6 0.01)
(check-within (cost 195) 187.8 0.01)
(check-within (cost 270) 190.8 0.01)

;; profit : number -> number
; Calculates the profit for a given price.
(define (profit price)
  (- (income price) (cost (customers price))))

(check-within (profit 5.00) 415.2 0.01)
(check-within (profit 5.20) 284.4 0.01)
(check-within (profit 4.50) 689.7 0.01)
(check-within (profit 4.00) 889.2 0.01)