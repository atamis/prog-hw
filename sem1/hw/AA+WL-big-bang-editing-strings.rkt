;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA+WL-big-bang-editing-strings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Andrew Amis and Willow Lark
; 10.14.11
; Text entry box
; http://fellowhuman.com/gbk/2011/10/14/friday-10-14-classwork-animations-revisited/

(require picturing-programs)

#|
Problem:
Design a program that keeps track of all key strokes. The program should
display the accumulated key strokes as red text in the 11 point font.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model

;; A World is a string, representing the sequence of keys pressed since
;; big-bang was called.
#|
Sample worlds:
""
"asdf"
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants:

(define FONT-COLOR "red")
(define FONT-SIZE 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controls:


;; chop-last-letter : string -> string
; Removes the last letter from the string
(define (chop-last-letter string)
  ; string  | string | "test"
  ; returns | string | "tes"
  (substring string 0 (max (- (string-length string) 1) 0)))

(check-expect (chop-last-letter "test") "tes")
(check-expect (chop-last-letter "awesome") "awesom")
(check-expect (chop-last-letter "asdf") "asd")
(check-expect (chop-last-letter "") "")

;; chop-first-char : string -> string
; Removes the first character of the given string
(define (chop-first-char string)
  ; string  | string | "test"
  ; returns | string | "est"
  (substring string 1))
(check-expect (chop-first-char "test") "est")
(check-expect (chop-first-char "awesome") "wesome")
(check-expect (chop-first-char "blagoyavich") "lagoyavich")

;; A KeyEvent is a string representing one press of a button on the keyboard.
;; Examples:  "a", "b", "7", "left", "up", "\b"

;; remember-keys : World KeyEvent -> World
;; adds the new keystroke to the existing world (string)
(define (remember-keys world key-event)
  (if (key=? key-event "\b")
      (chop-last-letter world)
   (string-append world key-event)))
(check-expect (remember-keys "" "a") "a")
(check-expect (remember-keys "a" "x") "ax")
(check-expect (remember-keys "abc" "left") "abcleft")
(check-expect (remember-keys "abc" "\b") "ab")
(check-expect (remember-keys "" "\b") "")

;; empty-string : string -> boolean
; Tests whether the string is empty (0 char length.)
(define (empty-string? string)
  ;; Inventory 
  ;  string => string => "a"
  ;  right answer => false
  (= (string-length string) 0))

(check-expect (empty-string? "3") false)
(check-expect (empty-string? "asdfasdfasdf") false)
(check-expect (empty-string? "") true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View:

;; draw-text : World -> Image
;; draws the world (string) as text of the color and size determined above
;; by FONT-COLOR and FONT-SIZE.
(define (draw-text world)
  (text world FONT-SIZE FONT-COLOR))
(check-expect (draw-text "abcd") (text "abcd" FONT-SIZE FONT-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run:
(big-bang " "
          (on-key remember-keys)
          (to-draw draw-text 500 40)
          (on-tick chop-first-char 1)
          (stop-when empty-string?))

;;;;;;;;;;;;;;;



