;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-define-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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