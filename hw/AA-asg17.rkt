; Andrew Amis
; 10.25.11
; Assignment 17
; http://fellowhuman.com/gbk/2011/10/23/prog-1-asg-17-design-reuse-part-2/
; 11.5.2 and 11.6.1-11.6.2

#lang racket
(require (prefix-in plot: plot)
         picturing-programs
         test-engine/racket-tests)

;;;;;; Utility functions

;; do-times : number function -> void?
; Calls the function the specified number of times. Function should take no 
; arguments.
(define (do-times n f)
  (unless (= n 0) (f) (do-times (sub1 n) f)))



;                                            
;                                            
;                                            
;     ;      ;           ;;;;;          ;;;  
;    ;;     ;;           ;             ;   ; 
;     ;      ;           ;                 ; 
;     ;      ;           ;;;;              ; 
;     ;      ;               ;            ;  
;     ;      ;               ;           ;   
;     ;      ;               ;          ;    
;     ;      ;     ;;    ;   ;   ;;    ;     
;    ;;;    ;;;    ;;     ;;;    ;;    ;;;;; 
;                                            
;                                            
;                                            
; Constants for the road-trip-cost problem:
(define MILES-PER-GALLON #i28)
(define PRICE-PER-GALLON 2.459)
(define MOTEL-PRICE-PER-NIGHT 20) ;; Used to be 40. Changed.
(define CAR-RENTAL-FIXED-FEE 10)
(define CAR-RENTAL-PER-DAY 29.95)
(define CAR-RENTAL-PER-MILE 0.10)
; gas-needed :  number (miles) -> number
(define (gas-needed miles)
  ; miles               a number
  ; MILES-PER-GALLON    a number
  (/ miles MILES-PER-GALLON))

(check-within (gas-needed 0) 0 .01)
(check-within (gas-needed 28) 1 .01)
(check-within (gas-needed 56) 2 .01)
(check-within (gas-needed 77) 2.75 .01)
(check-within (gas-needed 358) 12.78 .01)
; cost-of-gallons :  number (gallons) -> number
(define (cost-of-gallons gallons)
  ; gallons             number
  ; PRICE-PER-GALLON    number
  (* gallons PRICE-PER-GALLON))


(check-within (cost-of-gallons 0) 0 .01)
(check-within (cost-of-gallons 1) 2.459 .01)
(check-within (cost-of-gallons 2) 4.918 .01)
(check-within (cost-of-gallons 2.75) 6.76225 .01)
; gas-cost :  number (miles) -> number
(define (gas-cost miles)
  ; miles      number
  (cost-of-gallons (gas-needed miles)))

(check-within (gas-cost 0) 0 .01)
(check-within (gas-cost 28) 2.459 .01) ; i.e.  one gallon
(check-within (gas-cost 56) 4.918 .01) ; i.e.  two gallons
(check-within (gas-cost 77) 6.76 .01) ; 2-3/4 gal; use calculator
(check-within (gas-cost 358) 31.44 .01) ; yecch; use calculator

; nights-in-motel :  number (days) -> number
; Assumes the number of days is a positive integer.
(define (nights-in-motel days)
  ; days      a number
  (- days 1))
(check-expect (nights-in-motel 1) 0)
(check-expect (nights-in-motel 2) 1)
(check-expect (nights-in-motel 38) 37)

; motel-cost :  number (days) -> number
; Assumes the number of days is a positive integer.
(define (motel-cost days)
  ; days
  ; MOTEL-PRICE-PER-NIGHT
  ; (nights-in-motel days)
  (* MOTEL-PRICE-PER-NIGHT (nights-in-motel days)))
(check-expect (motel-cost 1) 0)
(check-expect (motel-cost 2) 20)
(check-expect (motel-cost 38) 740)

; rental-cost :  number (miles) number (days) -> number
; rental-cost :  number (miles) number (days) -> number
(define (rental-cost miles days)
  ; miles
  ; days
  ; CAR-RENTAL-FIXED-FEE
  ; CAR-RENTAL-PER-DAY
  ; CAR-RENTAL-PER-MILE
  ; (* days CAR-RENTAL-PER-DAY)
  ; (* miles CAR-RENTAL-PER-MILE)
  (+ (* days CAR-RENTAL-PER-DAY)
     (* miles CAR-RENTAL-PER-MILE)
     CAR-RENTAL-FIXED-FEE))

(check-within (rental-cost 0 1) 39.95 0.01)
(check-within (rental-cost 0 2) 69.90 0.01)
(check-within (rental-cost 100 1) 49.95 0.01)
(check-within (rental-cost 100 2) 79.90 0.01)
(check-within (rental-cost 28 1) 42.75 0.01)
(check-within (rental-cost 77 2) 77.60 0.01)
(check-within (rental-cost 358 3) 135.65 0.01)

; road-trip-cost :  number (miles) number (days) -> number
; road-trip-cost :  number (miles) number (days) -> number
(define (road-trip-cost miles days)
  ; miles
  ; days
  ; (gas-cost miles)
  ; (motel-cost days)
  ; (rental-cost miles days)
  (+ (gas-cost miles)
     (motel-cost days)
     (rental-cost miles days)))

(check-within (road-trip-cost 0 1) 39.95 .01) ; the gas and motels are 0
(check-within (road-trip-cost 0 2) 89.9 .01) ; gas still 0, motel $40
(check-within (road-trip-cost 28 1) 45.209 .01)
; $42.75 for car, $0 for motel, $2.459 for gas
(check-within (road-trip-cost 77 2) 104.36 .01)
; $77.60 for car, c.  $6.76 for gas, $40 for motel
(check-within (road-trip-cost 358 3) 207.09 .01)
; $135.65 for car, c.  $31.44 for gas, $80 for motel


;                                            
;                                            
;                                            
;     ;      ;             ;;            ;   
;    ;;     ;;            ;             ;;   
;     ;      ;           ;               ;   
;     ;      ;           ;;;;            ;   
;     ;      ;           ;   ;           ;   
;     ;      ;           ;   ;           ;   
;     ;      ;           ;   ;           ;   
;     ;      ;     ;;    ;   ;   ;;      ;   
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

(define roof-height 20)
(define style 'outline)

;; door : color -> image
; Creates a door of the given color.
(define (door color)
  ; color: the color of the door
  (rectangle 20 5 style color))


(check-expect (door 'green) (rectangle 20 5 'outline 'green))

;; half-roof : number(width) color -> image
; Builds the right half a roof. Assumes that width should not be modified.
; That is, width should represent half of the expected roof width.
(define (half-roof width color)
  ; width: the width of this half of the roof
  ; color: the color of the roof
  (right-triangle width roof-height style color))
(check-expect (half-roof 10 'blue) (right-triangle 10 roof-height 'outline 'blue))
(check-expect (half-roof 100 'gree) (right-triangle 100 roof-height style 'gree))
(check-expect (half-roof 50 'blue) (right-triangle 50 roof-height style 'blue))

;; build-roof : number(width) color -> image
; Builds a roof of a given width and color. Uses roof-height.
; Uses 2 right triangles.
(define (build-roof width color)
  ; width: the width of the whole roof
  ; color: the color of the house
  (beside
   (rotate-ccw (right-triangle roof-height (/ width 2) 'outline color))
   (right-triangle (/ width 2) roof-height 'outline color)))

(check-expect (build-roof 10 'blue) (beside
                                     (rotate-ccw (right-triangle roof-height 5 'outline 'blue))
                                     (right-triangle 5 roof-height 'outline 'blue)))
(check-expect (build-roof 30 'blue) (beside
                                     (rotate-ccw (right-triangle roof-height 15 'outline 'blue))
                                     (right-triangle 15 roof-height 'outline 'blue)))


;; build-house : number(height) number(width) color -> image
; Builds a house.
(define (build-house height width color)
  (overlay/align 'middle 'bottom (door color)
                 (above (build-roof width color)
                        (rectangle width height style color))))
(check-expect (build-house 100 100 'blue)
              (overlay/align 'middle 'bottom (door 'blue)
                             (above (build-roof 100 'blue)
                                    (rectangle 100 100 style 'blue))))
(check-expect (build-house 100 50 'blue)
              (overlay/align 'middle 'bottom (door 'blue)
                             (above (build-roof 50 'blue)
                                    (rectangle 50 100 style 'blue))))


;                                            
;                                            
;                                            
;     ;      ;             ;;           ;;;  
;    ;;     ;;            ;            ;   ; 
;     ;      ;           ;                 ; 
;     ;      ;           ;;;;              ; 
;     ;      ;           ;   ;            ;  
;     ;      ;           ;   ;           ;   
;     ;      ;           ;   ;          ;    
;     ;      ;     ;;    ;   ;   ;;    ;     
;    ;;;    ;;;    ;;     ;;;    ;;    ;;;;; 
;                                            
;                                            
;                                            

(define staff-cost 450) ; Cost of pilots, attendents, etc.
(define fuel 2.999) ; Cost of jet fuel.
(define plane-weight 50000) ; Weight of the plane.
(define person-weight 250) ; Weight of one person, including luggage.

;; purchasers : number(price) -> number
; Returns the number of passengers for a given ticket price.
; When ticket price increases by 10, passengers decrease by 4.
(define (purchasers price)
  (+ (* price -4/10) 200))
(check-expect (purchasers 200) 120)
(check-expect (purchasers 210) 116)
(check-expect (purchasers 190) 124)

;; fuel-cost : number(weight) -> number
; Calculates the cost of fuel for a given weight.
; This function should be given only the weight of the passengers/staff. Plane
; weight will be added.
(define (fuel-cost price)
  (* (/ (+ (* (+ (purchasers price) 4) ; For the attendents
              person-weight)
           plane-weight) 20) fuel))
(check-within (fuel-cost 200) 12145 1)
(check-within (fuel-cost 100) 13645 1)
(check-within (fuel-cost 600) 6147 1)

;; airline-profit : number(price) -> number
; Calculates the profit earned from selling tickets at a given price.
; price * people - (fuel + salary)
(define (airline-profit price)
  (- (* (purchasers price) price)
     (+ (fuel-cost price)
        staff-cost)))

(check-within (airline-profit 200) 11404 1)
(check-within (airline-profit 250) 13153 1)
(check-within (airline-profit 600) -30597 1)

(plot:plot
 (plot:line airline-profit)
 #:x-min 0
 #:x-max 460
 #:y-min 0
 #:y-max 15000)

;; Maxes out at roughly $267, at a total profit of $13293.115
;; You need $87 per seat to make it cost efficient.


;                                            
;                                            
;                                            
;     ;      ;             ;;           ;;;  
;    ;;     ;;            ;            ;   ; 
;     ;      ;           ;                 ; 
;     ;      ;           ;;;;            ;;  
;     ;      ;           ;   ;             ; 
;     ;      ;           ;   ;             ; 
;     ;      ;           ;   ;             ; 
;     ;      ;     ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

;; random-size : -> number
; Generates a random number between 10 and 30 inclusive.
(define (random-size)
  (+ 10 ; Adds 10, so now random number between 10 and 30
     (+ 1 (random 20)))) ; A random number between 1 and 20

;; Just to be sure...
(check-range (random-size) 10 30)(check-range (random-size) 10 30)
(check-range (random-size) 10 30)(check-range (random-size) 10 30)
(check-range (random-size) 10 30)(check-range (random-size) 10 30)
(check-range (random-size) 10 30)(check-range (random-size) 10 30)
(check-range (random-size) 10 30)(check-range (random-size) 10 30)
(check-range (random-size) 10 30)(check-range (random-size) 10 30)

;; surround-box : image color
; Surrounds the image with a rectangle 10 pixels taller and 6 pixels wider.
(define (surround-box image color)
  (overlay image
           (rectangle
            (+ 10 (image-width image))
            (+ 6 (image-height image))
            'outline color)))

(check-expect (surround-box (square 50 'solid 'black) 'blue)
              (overlay (square 50 'solid 'black)
                       (rectangle 60 56 'outline 'blue)))
(check-expect (surround-box (rectangle 50 30 'solid 'black) 'green)
              (overlay (rectangle 50 30 'solid 'black)
                       (rectangle 60 36 'outline 'green)))

;; text-color-size : color size -> image
; Appends size to your color, then draws it in that color
(define (text-color-size color size)
  (text (string-append color " " (number->string size)) size color))

(check-expect (text-color-size "green" 15) (text "green 15" 15 "green"))
(check-expect (text-color-size "blue" 30) (text "blue 30" 30 "blue"))
(check-expect (text-color-size "red" 20) (text "red 20" 20 "red"))


;; text-surround : color size -> image
; See text-color-size. Then see surround-box. Combines them in the obvious way.
(define (text-surround color size)
  (surround-box (text-color-size color size) color))
(check-expect (text-surround "green" 10)
              (surround-box (text-color-size "green" 10) "green"))
(check-expect (text-surround "blue" 20)
              (surround-box (text-color-size "blue" 20) "blue"))
(check-expect (text-surround "red" 30)
              (surround-box (text-color-size "red" 30) "red"))


;; color-box : color -> image
; Randomly choose a font size, appends that to the color, makes an image of that
; string in the random size, then surrounds it with a box of the 10 pixels
; wider and 6 pixels taller.
(define (color-box color)
  (text-surround color (random-size)))
; I can't test this, but I tested everything else.


 
(test)