;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Assignment 16
; Andrew Amis
; 10.20.11
; http://fellowhuman.com/gbk/2011/10/19/prog-1-asg-16-design-reuse-part-1/
; 11.2.2 and 11.3.1-11.3.6


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
(define MILES-PER-GALLON #i50)
; gas-cost :  number (miles) -> number
(define (gas-cost miles)
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

; gas-needed :  number (miles) -> number
"Examples of gas-needed"
(check-within (gas-needed 0) 0 .01)
(check-within (gas-needed 28) 1 .01)
(check-within (gas-needed 56) 2 .01)
(check-within (gas-needed 77) 2.75 .01)
(check-within (gas-needed 358) 12.8 .01)
; cost-of-gallons :  number (gallons) -> number
"Examples of cost-of-gallons:"
(check-within (cost-of-gallons 0) 0 .01)
(check-within (cost-of-gallons 1) 2.459 .01)
(check-within (cost-of-gallons 2) 4.918 .01)
(check-within (cost-of-gallons 2.75) 6.76225 .01)
; gas-cost :  number (miles) -> number
"Examples of gas-cost:"
(check-within (gas-cost 0) 0 .01)
(check-within (gas-cost 28) 2.459 .01) ; i.e.  one gallon
(check-within (gas-cost 56) 4.918 .01) ; i.e.  two gallons
(check-within (gas-cost 77) 6.76 .01) ; 2-3/4 gal; use calculator
(check-within (gas-cost 358) 31.44 .01) ; yecch; use calculator