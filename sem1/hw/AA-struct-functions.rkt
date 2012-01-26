;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-struct-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #27b
; Andrew Amis
; 12.7.11
; http://fellowhuman.com/gbk/2011/12/07/prog-1-asg-27b-functions-on-structs/
; Exercises 21.4.3, 21.4.4, 21.4.7, 21.4.10, and 21.5.3.



;                                                                        
;                                                                        
;                                                                        
;   ;;;;                   ;                  ;;;;;                      
;   ;   ;                  ;                  ;                          
;   ;   ;                                     ;                          
;   ;;;;    ;;;    ;;;;    ;    ;;;;          ;      ;   ;  ;;;;    ;;;  
;   ;   ;  ;   ;  ;   ;    ;    ;   ;         ;;;;    ; ;   ;   ;  ;   ; 
;   ;   ;  ;;;;;  ;   ;    ;    ;   ;         ;        ;    ;   ;   ;;;  
;   ;   ;  ;      ;   ;    ;    ;   ;         ;        ;    ;   ;      ; 
;   ;   ;  ;   ;  ;   ;    ;    ;   ;         ;       ; ;   ;   ;  ;   ; 
;   ;;;;    ;;;    ;;;;    ;    ;   ;         ;      ;   ;  ;   ;   ;;;  
;                     ;                                                  
;                 ;   ;                                                  
;                  ;;;                                                   


(define-struct person (first last age))
; make-person :  string(first) string(last) number(age) -> person
; person-first :  person -> string
; person-last :  person -> string
; person-age :  person -> number
; person?  :  object -> boolean

;; Prog 1 Asg #27: define-struct
; Andrew Amis
; 12.6.11
; http://fellowhuman.com/gbk/2011/12/05/prog-1-asg-27-define-struct/
; Exercises 21.3.1, 21.3.2, 21.3.4, 21.3.5, and 21.3.8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.3.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my-posn : number number
; A my-posn holds 2 numbers representing the x and y coordinates for a point in
; 2d space
(define-struct my-posn (x y))

; Template for my-posn:
#;(define (fun-for-my-posn my-posn)
    ... (my-posn-x my-posn) ...
    ... (my-posn-y my-posn) ...)
; (make-my-posn 4 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.3.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cd : string string number number
; A cd holds information identifying a CD. Title and artist are strings, while
; year is a number representing the year it was recorded, while track-number
; is a number that records the number of tracks on the CD.
(define-struct cd (title artist year track-number))

; Template for cd
#;(define (fun-for-cd cd)
    ... (cd-title cd) ...
    ... (cd-artist cd) ...
    ... (cd-year cd) ...
    ... (cd-track-number) ...)

; (make-cd "Dark Side of the Moon" "Pink Floyd" 1973 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.3.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; course : string string number number
; A course holds information on a course at school. Name and teacher are self-
; explanatory strings. Room is a the room number, while time is the hour upon
; which the class meets.
(define-struct course (name teacher room time))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.3.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; basketball-player : string team number
; A basketball-player holds information on a basketball player. The name is
; string containing the name of the player. The team is an enumeration
; identifying the team that this player. This enumeration is assumed to be
; defined elsewhere, and is implementation-specific. The number is this player's
; jersey number.
(define-struct basketball-player (name team number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.3.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; time : number number number
(define-struct time (hours minutes seconds))
; hours -> number representing time after midnight (24 hour clock)
; minutes -> number representing minutes after the hour
; seconds -> number representing seconds after the minute

;                                                          
;                                                          
;                                                          
;   ;;;;;             ;         ;;;;;                      
;   ;                 ;         ;                          
;   ;                 ;         ;                          
;   ;      ;;;;    ;;;;         ;      ;   ;  ;;;;    ;;;  
;   ;;;;   ;   ;  ;   ;         ;;;;    ; ;   ;   ;  ;   ; 
;   ;      ;   ;  ;   ;         ;        ;    ;   ;   ;;;  
;   ;      ;   ;  ;   ;         ;        ;    ;   ;      ; 
;   ;      ;   ;  ;   ;         ;       ; ;   ;   ;  ;   ; 
;   ;;;;;  ;   ;   ;;;;         ;      ;   ;  ;   ;   ;;;  
;                                                          
;                                                          
;                                                          


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.4.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rec-before-1980? : cd -> boolean
; Returns whether the given cd was made before 1980
(define (rec-before-1980? cd)
  (< (cd-year cd) 1980))
(check-expect (rec-before-1980? (make-cd "a" "a" 1980 1)) false)
(check-expect (rec-before-1980? (make-cd "a" "a" 1979 1)) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.4.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; older? : person person -> boolean
(define (older? p1 p2)
  (> (person-age p1) (person-age p2)))
(check-expect (older? (make-person 'a 'a 1) (make-person 'a 'a 0)) true)
(check-expect (older? (make-person 'a 'a 0) (make-person 'a 'a 1)) false)
(check-expect (older? (make-person 'a 'a 1) (make-person 'a 'a 1)) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.4.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fullname : person -> string
; Returns a person's full name
(define (fullname p)
  (string-append (person-first p) (person-last p)))
(check-expect (fullname (make-person "a" "a" 1)) "aa")
(check-expect (fullname (make-person "b" "a" 1)) "ba")
(check-expect (fullname (make-person "a" "b" 1)) "ab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.4.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; seconds-since-midnight : time -> umber
; Returns seconds elapsed since midnight
(define (seconds-since-midnight t)
  (+ (* 60 60 (time-hours t))
     (* 60 (time-minutes t))
     (time-seconds t)))
(check-expect (seconds-since-midnight (make-time 0 0 10)) 10)
(check-expect (seconds-since-midnight (make-time 1 1 10)) 3670)
(check-expect (seconds-since-midnight (make-time 7 5 10)) 25510)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 21.5.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; birthday : person -> person
; Increments a person's age (presumably used on a peson's birthday)
(define (birthday p)
  (make-person (person-first p)
               (person-last p)
               (add1 (person-age p))))
(check-expect (birthday (make-person 'a 'a 1)) (make-person 'a 'a 2))
(check-expect (birthday (make-person 'a 'a 2)) (make-person 'a 'a 3))
(check-expect (birthday (make-person 'a 'a 4)) (make-person 'a 'a 5))
(check-expect (birthday (make-person 'a 'a 10000)) (make-person 'a 'a 10001))