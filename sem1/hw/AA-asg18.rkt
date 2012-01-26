#lang racket

(require (prefix-in plot: plot)
         test-engine/racket-tests)

(define staff-cost 450) ; Cost of pilots, attendents, etc.
(define fuel 2.999) ; Cost of jet fuel.
(define plane-weight 50000) ; Weight of the plane.
(define person-weight 250) ; Weight of one person, including luggage.
(define staff-number 4) ; Number of staff
(define flight-length 700) ; Miles

;; purchasers : number(price) -> number
; Returns the number of passengers for a given ticket price. Returns a value
; between 0 and 180.
(define (passengers price) 
  (max (min (+ staff-number (+ (* price -4/10) 200)) 180) 0))
(check-expect (passengers 200) 124)
(check-expect (passengers 210) 120)
(check-expect (passengers 190) 128)
(check-expect (passengers 100) 164)
(check-expect (passengers 50) 180)
(check-expect (passengers 30) 180)
(check-expect (passengers 600) 0)

;; fuel-cost : number(weight) -> number
; Calculates the cost of fuel for a given weight.
; This function should be given only the weight of the passengers/staff. Plane
; weight will be added.
(define (fuel-cost price)
  (* (* 0.01
        (+ (passengers price) 4) ;; Add 4 for the staff.
        flight-length)
     fuel))
(check-within (fuel-cost 200) 2687 1)
(check-within (fuel-cost 100) 3526 1)
(check-within (fuel-cost 600) 83 1)

;; airline-profit : number(price) -> number
; Calculates the profit earned from selling tickets at a given price.
; price * people - (fuel + salary)
(define (airline-profit price)
  (- (* (passengers price) price)
     (+ (fuel-cost price)
        staff-cost)))

(check-within (airline-profit 200) 21662 1)
(check-within (airline-profit 250) 23282 1)
(check-within (airline-profit 600) -533 1)

(plot:plot
 (plot:line airline-profit)
 #:x-min 0
 #:x-max 600
 #:y-min 0
 #:y-max 30000)

;; $265 is the best price. At that point, the airline makes $23378.714

(map (lambda (x) 
       (cons x (airline-profit x)))
     (build-list 10
                 (lambda (x)
                   (+ x 260))))

(test)