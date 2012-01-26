#lang racket

;; Assignment 16
; Andrew Amis
; 10.20.11
; http://fellowhuman.com/gbk/2011/10/19/prog-1-asg-16-design-reuse-part-1/
; 11.2.2 and 11.3.1-11.3.6


(require test-engine/racket-tests)

;                                            
;                                            
;                                            
;     ;      ;            ;;;           ;;;  
;    ;;     ;;           ;   ;         ;   ; 
;     ;      ;               ;             ; 
;     ;      ;               ;             ; 
;     ;      ;              ;             ;  
;     ;      ;             ;             ;   
;     ;      ;            ;             ;    
;     ;      ;     ;;    ;       ;;    ;     
;    ;;;    ;;;    ;;    ;;;;;   ;;    ;;;;; 
;                                            
;                                            
;                                            

(define PRICE-PER-GALLON 2.459)
(define MILES-PER-GALLON #i28)
; gas-cost :  number (miles) -> number
#|(define (gas-cost miles)
  ; miles               a number
  ; MILES-PER-GALLON    a fixed number I know I’ll need
  ; PRICE-PER-GALLON    ditto
  (* PRICE-PER-GALLON (/ miles MILES-PER-GALLON))
  )
"Examples of gas-cost:"
(check-within (gas-cost 0) 0 .01)
(check-within (gas-cost 50) 2.459 .01) ; i.e.  one gallon
(check-within (gas-cost 100) 4.918 .01) ; i.e.  two gallons
(check-within (gas-cost 275) 13.524 .01) ; 2-3/4 gal; use calculator
(check-within (gas-cost 358) 17.613 .01) ; yecch; use calculator

|#
;;Commented because it conflicts with furthur functions.
;                                            
;                                            
;                                            
;     ;      ;            ;;;            ;   
;    ;;     ;;           ;   ;          ;;   
;     ;      ;               ;           ;   
;     ;      ;             ;;            ;   
;     ;      ;               ;           ;   
;     ;      ;               ;           ;   
;     ;      ;               ;           ;   
;     ;      ;     ;;    ;   ;   ;;      ;   
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

;; gas-needed :  number (miles) -> number
; Gas needed to travel some number of miles.
(define (gas-needed miles)
  (/ miles MILES-PER-GALLON))

(check-within (gas-needed 0) 0 .01)
(check-within (gas-needed 28) 1 .01)
(check-within (gas-needed 56) 2 .01)
(check-within (gas-needed 77) 2.75 .01)
(check-within (gas-needed 358) 12.785 .01) ;; Changed because it wasn't working

;; cost-of-gallons :  number (gallons) -> number
; Cost of some gallons of gas
(define (cost-of-gallons gallons)
  (* gallons PRICE-PER-GALLON))

(check-within (cost-of-gallons 0) 0 .01)
(check-within (cost-of-gallons 1) 2.459 .01)
(check-within (cost-of-gallons 2) 4.918 .01)
(check-within (cost-of-gallons 2.75) 6.76225 .01)
;; gas-cost :  number (miles) -> number
; Cost of gas to travel some miles.
(define (gas-cost miles)
  (cost-of-gallons (gas-needed miles)))

(check-within (gas-cost 0) 0 .01)
(check-within (gas-cost 28) 2.459 .01) ; i.e.  one gallon
(check-within (gas-cost 56) 4.918 .01) ; i.e.  two gallons
(check-within (gas-cost 77) 6.76 .01) ; 2-3/4 gal; use calculator
(check-within (gas-cost 358) 31.44 .01) ; yecch; use calculator


;                                            
;                                            
;                                            
;     ;      ;            ;;;           ;;;  
;    ;;     ;;           ;   ;         ;   ; 
;     ;      ;               ;             ; 
;     ;      ;             ;;              ; 
;     ;      ;               ;            ;  
;     ;      ;               ;           ;   
;     ;      ;               ;          ;    
;     ;      ;     ;;    ;   ;   ;;    ;     
;    ;;;    ;;;    ;;     ;;;    ;;    ;;;;; 
;                                            
;                                            
;                                            

;; circle-area : number -> number
; Calculates the area of a circle from the given radius.
(define (circle-area radius)
  ; radius | number | 10
  ; returns| number | #i314.1592653589793
  (* pi (expt radius 2)))


(check-within (circle-area 10) 314 1)
(check-within (circle-area 15) 706 1)
(check-within (circle-area 17) 907 1)

;; cylinder-volume : number(radius) number(height) -> number
; Returns the volume of a cylinder of given radius and height
(define (cylinder-volume radius height)
  ; radius  | number | 10
  ; height  | number | 10 
  ; returns | number | #i314.1592653589793
  (* height (circle-area radius)))

(check-within (cylinder-volume 10 10) 3141 1)
(check-within (cylinder-volume 4 5) 251 1)
(check-within (cylinder-volume 100 50) 1570796 1)



;                                            
;                                            
;                                            
;     ;      ;            ;;;           ;;;  
;    ;;     ;;           ;   ;         ;   ; 
;     ;      ;               ;             ; 
;     ;      ;             ;;            ;;  
;     ;      ;               ;             ; 
;     ;      ;               ;             ; 
;     ;      ;               ;             ; 
;     ;      ;     ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

;; circle-perimeter : number -> number
; Calculates the perimeter of a circle from the radius
(define (circle-perimeter radius)
  ; radius | number | 10
  ; returns| number | #i62.83185307179586
  (* 2 pi radius))

(check-within (circle-perimeter 10) 62 1)
(check-within (circle-perimeter 12) 75 1)
(check-within (circle-perimeter 120) 753 1)

;; tube-surface-area : number(radius) number(height) -> number
; Calculates outside (or inside) surface area of a tube.
(define (tube-surface-area radius height)
  (* (circle-perimeter radius) height))

(check-within (tube-surface-area 10 10) 628 1)
(check-within (tube-surface-area 100 10) 6283 1)
(check-within (tube-surface-area 4 5) 125 1)

;; Surface Area = 2(Area of top) + (perimeter of top)* height

;; cylinder-area : number(radius) number(height) -> number
; Returns the surface area of a cylinder of given radius and height.
(define (cylinder-area radius height)
  (+ (* 2 (circle-area radius))
     (tube-surface-area radius height)))
(check-within (cylinder-area 10 10) 1256 1)
(check-within (cylinder-area 10 100) 6911 1)
(check-within (cylinder-area 4 5) 226 1)


;                                            
;                                            
;                                            
;     ;      ;            ;;;             ;  
;    ;;     ;;           ;   ;           ;;  
;     ;      ;               ;          ; ;  
;     ;      ;             ;;          ;  ;  
;     ;      ;               ;         ;;;;;;
;     ;      ;               ;            ;  
;     ;      ;               ;            ;  
;     ;      ;     ;;    ;   ;   ;;       ;  
;    ;;;    ;;;    ;;     ;;;    ;;       ;  
;                                            
;                                            
;                                            

;; inner-tube + outer-tube + 2 * donuts

;; donut-area : number(inner-radius) number(outer-radius) -> number
; Returns the area of a donut of a given inner and outer radius.
(define (donut-area inner-radius outer-radius)
  (- (circle-area outer-radius)
     (circle-area inner-radius)))

(check-within (donut-area 4 10) 263 1)
(check-within (donut-area 5 10) 235 1)
(check-within (donut-area 10 10) 0 1)

;; pipe-area : number(inner-radius) number(outer-radius) number(height)
; Caculates the area of a pipe of given inner and outer radii and height.
(define (pipe-area inner-radius outer-radius height)
  (* height
     (donut-area inner-radius outer-radius)))

(check-within (pipe-area 4 10 10) 2638 1)
(check-within (pipe-area 5 10 2) 471 1)
(check-within (pipe-area 10 10 10) 0 1)


;                                            
;                                            
;                                            
;     ;      ;            ;;;          ;;;;; 
;    ;;     ;;           ;   ;         ;     
;     ;      ;               ;         ;     
;     ;      ;             ;;          ;;;;  
;     ;      ;               ;             ; 
;     ;      ;               ;             ; 
;     ;      ;               ;             ; 
;     ;      ;     ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

(define PRIMARY-TAX-RATE 0.005) ; 0.5%

;; tax-rate : number(salary) -> number
; Calculates the tax rate for a given salary.
(define (tax-rate salary)
  (* PRIMARY-TAX-RATE (/ salary 1000)))

(check-within (tax-rate 40000) 0.2 0.01)
(check-within (tax-rate 50000) 0.25 0.01)
(check-within (tax-rate 0) 0.0 0.01)


;; tax-payed : number(salary) -> number
; Returns the tax payed for a given salary.
(define (tax-payed salary)
  (* salary (tax-rate salary)))

(check-within (tax-payed 40000) 8000 1)
(check-within (tax-payed 50000) 12500 1)
(check-within (tax-payed 100000) 50000 1)

;; net-pay : number(salary) -> number
; Returns the net pay of somebody with the given salary.
(define (net-pay salary)
  (- salary (tax-payed salary)))

(check-within (net-pay 40000) 32000 1)
(check-within (net-pay 50000) 37500 1)
(check-within (net-pay 100000) 50000 1)


;                                            
;                                            
;                                            
;     ;      ;            ;;;            ;;  
;    ;;     ;;           ;   ;          ;    
;     ;      ;               ;         ;     
;     ;      ;             ;;          ;;;;  
;     ;      ;               ;         ;   ; 
;     ;      ;               ;         ;   ; 
;     ;      ;               ;         ;   ; 
;     ;      ;     ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

(require plot)

(plot
 (line net-pay)
  #:x-min -1	 	 	 	 
  #:x-max 200000
  #:y-min -1	 	 	 	 
  #:y-max 100000)

;; When tax rate is 0.5%, gross pay caps at 100000
;; When tax rate is 0.6%, gross pay caps at 83333.3...

(test)