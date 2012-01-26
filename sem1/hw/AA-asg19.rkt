;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Prog 1 Asg #19: Boolean Review
; Andre Amis
; 10.31.11
; http://fellowhuman.com/gbk/2011/10/31/prog-1-asg-19-review-of-boolean-functions/

; 13.2.3-13.2.5, 13.3.4, and 13.3.5.


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;           ;;;  
;    ;;    ;   ;         ;   ;         ;   ; 
;     ;        ;             ;             ; 
;     ;      ;;              ;           ;;  
;     ;        ;            ;              ; 
;     ;        ;           ;               ; 
;     ;        ;          ;                ; 
;     ;    ;   ;   ;;    ;       ;;    ;   ; 
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

;; is-nintendo? : string -> boolean
; Test whether a string is equal to "nintendo".
(define (is-nintendo? string) (string=? string "nintendo"))
(check-expect (is-nintendo? "test") false)
(check-expect (is-nintendo? "nintendo") true)


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;             ;  
;    ;;    ;   ;         ;   ;           ;;  
;     ;        ;             ;          ; ;  
;     ;      ;;              ;         ;  ;  
;     ;        ;            ;          ;;;;;;
;     ;        ;           ;              ;  
;     ;        ;          ;               ;  
;     ;    ;   ;   ;;    ;       ;;       ;  
;    ;;;    ;;;    ;;    ;;;;;   ;;       ;  
;                                            
;                                            
;                                            

;; empty-string? : string -> boolean
; Tests whether a string is equal to ""
(define (empty-string? string) (string=? string ""))
(check-expect (empty-string? "test") false)
(check-expect (empty-string? "") true)


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;          ;;;;; 
;    ;;    ;   ;         ;   ;         ;     
;     ;        ;             ;         ;     
;     ;      ;;              ;         ;;;;  
;     ;        ;            ;              ; 
;     ;        ;           ;               ; 
;     ;        ;          ;                ; 
;     ;    ;   ;   ;;    ;       ;;    ;   ; 
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

;; in-first-half? : string -> boolean
; Checks whether the first letter of the string is less than "n"
(define (in-first-half? string) (string<? string "n"))
(check-expect (in-first-half? "a") true)
(check-expect (in-first-half? "z") false)
(check-expect (in-first-half? "n") false)


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;             ;  
;    ;;    ;   ;         ;   ;           ;;  
;     ;        ;             ;          ; ;  
;     ;      ;;            ;;          ;  ;  
;     ;        ;             ;         ;;;;;;
;     ;        ;             ;            ;  
;     ;        ;             ;            ;  
;     ;    ;   ;   ;;    ;   ;   ;;       ;  
;    ;;;    ;;;    ;;     ;;;    ;;       ;  
;                                            
;                                            
;                                            

;; under-a-dollar : number -> boolean
; Checks whether the given number is under a dollar.
(define (under-a-dollar n) (< n 1.00))
(check-expect (under-a-dollar 0.3) true)
(check-expect (under-a-dollar 4) false)
(check-expect (under-a-dollar 1.00) false)



;                                            
;                                            
;                                            
;     ;     ;;;           ;;;          ;;;;; 
;    ;;    ;   ;         ;   ;         ;     
;     ;        ;             ;         ;     
;     ;      ;;            ;;          ;;;;  
;     ;        ;             ;             ; 
;     ;        ;             ;             ; 
;     ;        ;             ;             ; 
;     ;    ;   ;   ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;;    ;;     ;;;    ;;     ;;;  
;                                            
;                                            
;                                            

;; is-17? : number -> boolean
; Checks whether the given number is 17.
(define (is-17? n) (= n 17))
(check-expect (is-17? 17) true)
(check-expect (is-17? 16) false)
