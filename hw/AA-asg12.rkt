;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #12 - Andrew Amis - 10/4/11
;; http://fellowhuman.com/gbk/2011/10/04/prog-1-asg-12/

(require picturing-programs)


;                                          
;                                          
;  ;;;;;;;         ;;;;;;;           ;;;;  
;        ;               ;          ;    ; 
;       ;               ;                ; 
;      ;               ;                 ; 
;     ;               ;                 ;  
;    ;               ;                 ;   
;    ;               ;                ;    
;   ;               ;                ;     
;   ;               ;               ;      
;   ;         ;;    ;         ;;    ;      
;   ;         ;;    ;         ;;    ;;;;;; 
;                                          
;                                          
;                                          
;                                          

; rect-perimeter : number number -> number
; Returns the parimeter of the rectangle with the width and height specified
; in the function call
(check-expect (rect-perimeter 10 10) 40)
(check-expect (rect-perimeter 20 10) 60)
(check-expect (rect-perimeter 100 100) 400)
(define (rect-perimeter length width)
  ; length | number | 10
  ; width  | number | 10
  ; returns| number | 40
  (+ length length width width))


;                                          
;                                          
;  ;;;;;;;         ;;;;;;;           ;;;;  
;        ;               ;          ;    ; 
;       ;               ;                ; 
;      ;               ;                 ; 
;     ;               ;                 ;  
;    ;               ;               ;;;;  
;    ;               ;                   ; 
;   ;               ;                    ; 
;   ;               ;                    ; 
;   ;         ;;    ;         ;;        ;; 
;   ;         ;;    ;         ;;    ;;;;   
;                                          
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


;                                          
;                                          
;  ;;;;;;;         ;;;;;;;              ;  
;        ;               ;             ;;  
;       ;               ;             ; ;  
;      ;               ;              ; ;  
;     ;               ;              ;  ;  
;    ;               ;              ;   ;  
;    ;               ;             ;    ;  
;   ;               ;              ;;;;;;;;
;   ;               ;                   ;  
;   ;         ;;    ;         ;;        ;  
;   ;         ;;    ;         ;;        ;  
;                                          
;                                          
;                                          
;                                          

;; area-of-circle : number -> number
; Calculates the area of a circle from the given radius.
(define (area-of-circle radius)
  ; radius | number | 10
  ; returns| number | #i314.1592653589793
  (* pi (expt radius 2)))


(check-within (area-of-circle 10) 314 1)
(check-within (area-of-circle 15) 706 1)
(check-within (area-of-circle 17) 907 1)

;                                                  
;                                                  
;  ;;;;;;;         ;;;;;;;            ;         ;  
;        ;               ;           ;;        ;;  
;       ;               ;           ; ;       ; ;  
;      ;               ;              ;       ; ;  
;     ;               ;               ;      ;  ;  
;    ;               ;                ;     ;   ;  
;    ;               ;                ;    ;    ;  
;   ;               ;                 ;    ;;;;;;;;
;   ;               ;                 ;         ;  
;   ;         ;;    ;         ;;      ;         ;  
;   ;         ;;    ;         ;;    ;;;;;       ;  
;                                                  
;                                                  
;                                                  
;

;; at-most-10 : number -> number
; Returns which ever is smaller, the number or 10.
(define (at-most-10 number)
  ; number | number | 4
  ; returns| number | 4
  (min number 10))

(check-expect (at-most-10 4) 4)
(check-expect (at-most-10 10) 10)
(check-expect (at-most-10 50) 10)
(check-expect (at-most-10 -4) -4)


;                                                  
;                                                  
;  ;;;;;;;         ;;;;;;;            ;     ;;;;;; 
;        ;               ;           ;;     ;      
;       ;               ;           ; ;     ;      
;      ;               ;              ;     ;      
;     ;               ;               ;     ;;;;   
;    ;               ;                ;         ;  
;    ;               ;                ;          ; 
;   ;               ;                 ;          ; 
;   ;               ;                 ;          ; 
;   ;         ;;    ;         ;;      ;         ;  
;   ;         ;;    ;         ;;    ;;;;;   ;;;;   
;                                                  
;                                                  
;                                                  
;                                                  

;; celsius->kelvin : number -> number
; Converts from degrees celsius to degrees kelvin
(define (celsius->kelvin celsius)
  ; celsius | number | 100
  ; returns | number | ~373
  (+ celsius 273.15))


(check-within (celsius->kelvin -273.15) 0 0.01)
(check-within (celsius->kelvin 100) 373 1)
(check-within (celsius->kelvin 200) 473.15 1)


;                                                  
;                                                  
;  ;;;;;;;         ;;;;;;;            ;      ;;;;  
;        ;               ;           ;;     ;      
;       ;               ;           ; ;    ;       
;      ;               ;              ;    ;       
;     ;               ;               ;    ; ;;;;  
;    ;               ;                ;    ;;    ; 
;    ;               ;                ;    ;     ; 
;   ;               ;                 ;    ;     ; 
;   ;               ;                 ;    ;     ; 
;   ;         ;;    ;         ;;      ;     ;   ;  
;   ;         ;;    ;         ;;    ;;;;;    ;;;   
;                                                  
;                                                  
;                                                  
;                                                  

;; fahrenheit->celsius : number -> number
; Takes fahrenheit and converts it to celsius
(define (fahrenheit->celsius fahrenheit)
  ; fahrenheit | number | 100
  ; returns    | number | ~37.7
  (* (- fahrenheit 32) 5/9))

(check-within (fahrenheit->celsius 100) 37 1)
(check-within (fahrenheit->celsius 200) 93 1)
(check-within (fahrenheit->celsius 300) 148 1)


;                                                  
;                                                  
;  ;;;;;;;         ;;;;;;;            ;    ;;;;;;; 
;        ;               ;           ;;          ; 
;       ;               ;           ; ;         ;  
;      ;               ;              ;        ;   
;     ;               ;               ;       ;    
;    ;               ;                ;      ;     
;    ;               ;                ;      ;     
;   ;               ;                 ;     ;      
;   ;               ;                 ;     ;      
;   ;         ;;    ;         ;;      ;     ;      
;   ;         ;;    ;         ;;    ;;;;;   ;      
;                                                  
;                                                  
;                                                  
;                                                  

;; fahrenheit->kelvin : number -> number
; Converts from fahrenheit to kelvin.
(define (fahrenheit->kelvin fahrenheit)
  ; fahrenheit | number | 0
  ; returns    | number | ~ 255
  (celsius->kelvin (fahrenheit->celsius fahrenheit)))

(check-within (fahrenheit->kelvin 100) 310 1)
(check-within (fahrenheit->kelvin 0) 255 1)
(check-within (fahrenheit->kelvin 1000) 810 1)


;                                                  
;                                                  
;  ;;;;;;;         ;;;;;;;           ;;;;    ;;;;  
;        ;               ;          ;    ;  ;    ; 
;       ;               ;                ;       ; 
;      ;               ;                 ;       ; 
;     ;               ;                 ;       ;  
;    ;               ;                 ;     ;;;;  
;    ;               ;                ;          ; 
;   ;               ;                ;           ; 
;   ;               ;               ;            ; 
;   ;         ;;    ;         ;;    ;           ;; 
;   ;         ;;    ;         ;;    ;;;;;;  ;;;;   
;                                                  
;                                                  
;                                                  
;                                                  

;; Static bar width for the bar graph. Number.
(define BAR-WIDTH 20)

;; bar-graph : number number number number -> image
; Produces a bar graph with bars in red, blue, green, and yellow of the given
; heights
(define (bar-graph red blue green yellow)
  ; red    | number | 10
  ; blue   | number | 10
  ; green  | number | 10
  ; yellow | number | 10
  ; returns| image  | (beside/align 'bottom
  ;                          (rectangle BAR-WIDTH 10 'solid 'red)
  ;                          (rectangle BAR-WIDTH 10 'solid 'blue)
  ;                          (rectangle BAR-WIDTH 10 'solid 'green)
  ;                          (rectangle BAR-WIDTH 10 'solid 'yellow))
  (beside/align 'bottom
                (rectangle BAR-WIDTH red 'solid 'red)
                (rectangle BAR-WIDTH blue 'solid 'blue)
                (rectangle BAR-WIDTH green 'solid 'green)
                (rectangle BAR-WIDTH yellow 'solid 'yellow)))

(check-expect (bar-graph 10 20 30 40)
              (beside/align 'bottom
                            (rectangle BAR-WIDTH 10 'solid 'red)
                            (rectangle BAR-WIDTH 20 'solid 'blue)
                            (rectangle BAR-WIDTH 30 'solid 'green)
                            (rectangle BAR-WIDTH 40 'solid 'yellow)))
(check-expect (bar-graph 40 20 90 40)
              (beside/align 'bottom
                            (rectangle BAR-WIDTH 40 'solid 'red)
                            (rectangle BAR-WIDTH 20 'solid 'blue)
                            (rectangle BAR-WIDTH 90 'solid 'green)
                            (rectangle BAR-WIDTH 40 'solid 'yellow)))
(check-expect (bar-graph 100 200 300 400)
              (beside/align 'bottom
                            (rectangle BAR-WIDTH 100 'solid 'red)
                            (rectangle BAR-WIDTH 200 'solid 'blue)
                            (rectangle BAR-WIDTH 300 'solid 'green)
                            (rectangle BAR-WIDTH 400 'solid 'yellow)))