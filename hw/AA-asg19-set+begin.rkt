#lang racket
;; Prog 2 Asg #19: set! and begin
; Andrew Amis
; Started: 5.14.12
; Ended: 5.15.12
; http://fellowhuman.com/gbk/2012/05/14/prog-2-asg-19-set-and-begin/

(require test-engine/racket-tests
         htdp/gui)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.1.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 is invalid.
; 2 is invalid
; 3 is invalid


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1*1, or, 1.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define x 3)
(define y 5)

(begin 
  (set! x y)
  (set! y (+ y 2))
  (set! x 3)
  (list x y))


; [x = 5]
(begin
  (set! y (+ y 2))
  (set! x 3)
  (list x y))

; [x = 5][y = 7]
(begin
  (set! x 3)
  (list x y))

; [x = 3][y = 7]
(begin
  (list x y))

; [x = 3][y = 7]
(begin
  '(3 7))

; [x = 3][y = 7]
'(3 7)

4 time periods
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define x 1)
(define y 1)

; [x = 1] [y = 1]
(begin (set! x (+ x 1))
       (set! y (- y 1))
       (* x y))

; [x = 2] [y = 1]
(begin (set! y (- y 1))
       (* x y))

; [x = 2] [y = 0]
(begin (* x y))

; [x = 2] [y = 0]
(begin 0)

; [x = 2] [y = 0]
0

3 time periods
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define x 3)
(define y 5)

; [x = 3] [y = 5]
(begin 
  (set! x y)
  (set! y (+ y 2))
  (set! x 3)
  (list x y))

; [x = 5] [y = 5]
(begin
  (set! y (+ y 2))
  (set! x 3)
  (list x y))

; [x = 5] [y = 7]
(begin
  (set! x 3)
  (list x y))

; [x = 3] [y = 7]
(begin
  (list x y))

; [x = 3] [y = 7]
(begin
  '(3 7))

; [x = 3] [y = 7]
'(3 7)

4 time periods
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.3.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define x 3)

(define (increase-x)
  (begin
    (set! x (+ x 1))
    x))

;; [x = 3]
(increase-x)
(increase-x)
(increase-x)

;; [x = 4]
4
(increase-x)
(increase-x)

;; [x = 5]
4
5
(increase-x)

;; [x = 6]
4
5
6

Result above.

increase-x increases x by 1, then returns the current value of x.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.3.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define x 0)

(define (switch-x)
  (begin
    (set! x (- x 1))
    x))

; [x = 0]
(switch-x)
(switch-x)
(switch-x)

; [x = -1]
-1
(switch-x)
(switch-x)

; [x = -2]
-1
-2
(switch-x)

; [x = -3]
-1
-2
-3

Result above.

switch-x simple decrements x, and returns the new value.
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 35.3.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define x 0)

(define y 1)

(define (change-to-3 z)
  (begin
    (set! y 3)
    z))

; [x = 0] [y = 1]
(change-to-3 x)

; [x = 0] [y = 3]
0

Sets y to 3, returns the value given to it.
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Phonebook ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *phone-book* : Listof[Entry]
;; This state variable tracks the name/number entries in a phone book.
(define *phone-book* empty)

;; An Entry is a (make-entry String Number):
(define-struct entry (name number)
  #:transparent)
;; where name is a person's name
;; and number is that person's phone number.

;; Example entries:
(define Ed (make-entry "Ed" 5551234))
(define Al (make-entry "Al" 5556789))
(define Ann (make-entry "Ann" 5559652))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Service #1: Look up a person in the phone book.

;; lookup : String -> Entry or false
;; Finds the phone-book entry for the given name; returns the entry, or false
;; if no entry is found.
(define (lookup name) (lookup/internal *phone-book* name))

;; lookup/internal : Listof[Entry] String -> Entry or false
;; Returns the first entry in the given phone book with the given name,
;; or false if no such entry exists.
(check-expect (lookup/internal (list Ed Al) "Al") Al)
(check-expect (lookup/internal empty "Al") false)
(check-expect (lookup/internal (list Al Ed Ann) "Ann") Ann)
(define (lookup/internal book name)
  (cond [(empty? book) false]
        [(cons? book)
         (if (string=? name (entry-name (first book)))
             (first book)
             (lookup/internal (rest book) name))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Service #2: Add a person to the phone book.

;; add-to-book! : String Number -> void
;; EFFECT: Adds an entry (for the given name and number) to *phone-book*.
(define (add-to-book! name num)
  (set! *phone-book* (cons (make-entry name num) *phone-book*)))


;; remove-from-book! : String -> void
; Removes the given name from the phone book.
(define (remove-from-book! name)
  (set! *phone-book* (filter {λ (entry)
                               (not (string=? (entry-name entry) name))}
                             *phone-book*)))


(define output (make-message "Output"))
(define spacer (make-message " "))

(define search (make-text "Name"))
(define name (make-text "Name"))
(define number (make-text "Number"))

(define search-button
  (make-button "Search"
               {λ (e)
                 (draw-message output
                               (let ([results (lookup (text-contents search))])
                                 (if results
                                     (format "~s" results)
                                     "Not found")))
                 }))
(define remove-button
  (make-button "Remove" {λ (e) (begin
                                 (remove-from-book! (text-contents search))
                                 (draw-message output
                                               "Removed"))}))
(define add-button
  (make-button "Add" {λ (e)
                       (begin
                         (add-to-book! (text-contents name)
                                       (text-contents number))
                         (draw-message output "Added"))
                       }))




(define (run)
  (show-window
   (create-window
    (list 
     (list search search-button remove-button)
     (list name number add-button)
     (list output)
     (list spacer)))))

(test)
(run)