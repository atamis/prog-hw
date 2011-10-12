;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Andrew Amis
; 10/12/11
; Assignment 14
; http://fellowhuman.com/gbk/2011/10/11/prog-1-asg-14-string-functions/
; 9.2.2-9.2.5 and 9.2.7-9.2.8.

(require picturing-programs)

;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;             ; 
;   ;   ;             ;             ; 
;   ;   ;            ;             ;  
;    ;;;;           ;             ;   
;       ;          ;             ;    
;      ;    ;;    ;       ;;    ;     
;    ;;     ;;    ;;;;;   ;;    ;;;;; 
;                                     
;                                     
;                                     

;; repeat : string -> string
; Takes in a string and returns that string appended to itself
(define (repeat string)
  ; string  | string | "test"
  ; returns | string | "testtest"
  (string-append string string))

(check-expect (repeat "test") "testtest")
(check-expect (repeat "") "")
(check-expect (repeat "this is a test") "this is a testthis is a test")


;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;             ; 
;   ;   ;             ;           ;;  
;   ;   ;            ;              ; 
;    ;;;;           ;               ; 
;       ;          ;                ; 
;      ;    ;;    ;       ;;    ;   ; 
;    ;;     ;;    ;;;;;   ;;     ;;;  
;                                     
;                                     
;                                     


;; chop-first-char : string -> string
; Removes the first character of the given string
(define (chop-first-char string)
  ; string  | string | "test"
  ; returns | string | "est"
  (substring string 1))
(check-expect (chop-first-char "test") "est")
(check-expect (chop-first-char "awesome") "wesome")
(check-expect (chop-first-char "blagoyavich") "lagoyavich")


;                                     
;                                     
;                                     
;    ;;;           ;;;             ;  
;   ;   ;         ;   ;           ;;  
;   ;   ;             ;          ; ;  
;   ;   ;             ;         ;  ;  
;   ;   ;            ;          ;;;;;;
;    ;;;;           ;              ;  
;       ;          ;               ;  
;      ;    ;;    ;       ;;       ;  
;    ;;     ;;    ;;;;;   ;;       ;  
;                                     
;                                     
;                                     

;; first-char : string -> string
; Returns the first character in the string
(define (first-char string)
  ; string  | string | "test"
  ; returns | string | "t"
  (substring string 0 1))

(check-expect (first-char "test") "t")
(check-expect (first-char "awesome") "a")
(check-expect (first-char "yes, I would like some more cake") "y")


;                                     
;                                     
;                                     
;    ;;;           ;;;          ;;;;; 
;   ;   ;         ;   ;         ;     
;   ;   ;             ;         ;     
;   ;   ;             ;         ;;;;  
;   ;   ;            ;              ; 
;    ;;;;           ;               ; 
;       ;          ;                ; 
;      ;    ;;    ;       ;;    ;   ; 
;    ;;     ;;    ;;;;;   ;;     ;;;  
;                                     
;                                     
;                                     

;; last-half : string -> string
; Returns the last half of the string.
; When dealing with odd-lengthed strings, it moves the split to the left.
(define (last-half string)
  ; string  | string | "test"
  ; returns | string | "st"
  (substring string (ceiling (/ (string-length string) 2))))

(check-expect (last-half "test") "st")
(check-expect (last-half "yes") "s")
(check-expect (last-half "there") "re")
(check-expect (last-half "") "")


;                                     
;                                     
;                                     
;    ;;;           ;;;          ;;;;; 
;   ;   ;         ;   ;             ; 
;   ;   ;             ;             ; 
;   ;   ;             ;            ;  
;   ;   ;            ;            ;   
;    ;;;;           ;             ;   
;       ;          ;              ;   
;      ;    ;;    ;       ;;      ;   
;    ;;     ;;    ;;;;;   ;;      ;   
;                                     
;                                     
;                                     

;; number->image : number -> image
; Draws a number as some blue, 18 pt text
(define (number->image number)
  ; number  | number | 10
  ; returns | image  | (text "10" 18 'blue)
  (text (number->string number) 18 'blue))
         
(check-expect (number->image 10) (text "10" 18 'blue))
(check-expect (number->image 100000000000) (text "100000000000" 18 'blue))
(check-expect (number->image 1/3) (text "1/3" 18 'blue))


;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;         ;   ; 
;   ;   ;             ;          ;;;  
;   ;   ;            ;          ;   ; 
;    ;;;;           ;           ;   ; 
;       ;          ;            ;   ; 
;      ;    ;;    ;       ;;    ;   ; 
;    ;;     ;;    ;;;;;   ;;     ;;;  
;                                     
;                                     
;                                     

;; digits : number -> number
; Returns the number of digits in the supplied number. Does not deal with
; decimals or fractions
(define (digits number)
  ; number  | number | 10
  ; returns | number | 2
  (string-length (number->string number))) 

(check-expect (digits 0) 1)
(check-expect (digits 100000000) 9)
(check-expect (digits (expt 10 10000)) 10001)
