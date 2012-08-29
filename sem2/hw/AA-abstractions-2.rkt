;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-abstractions-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #8: Abstractions, Part 2
; Andrew Amis
; Started: 3.12.12
; Ended: ?
; http://fellowhuman.com/gbk/2012/03/12/prog-2-abstractions-part-2/
; Exercises 196-198

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 196 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; tabulate : {number -> number} number -> list[of-numbers]
; Builds a list by applying the function to the numbers 0 to n.
(define (tabulate fxn n)
  (cond
    [(= n 0) (list (fxn 0))]
    [else
     (cons (fxn n)
           (tabulate fxn (sub1 n)))]))
(check-expect (tabulate add1 0) '(1))
(check-expect (tabulate add1 1) '(2 1))
(check-expect (tabulate add1 10) '(11 10 9 8 7 6 5 4 3 2 1))


;; tab-sqr : n -> list[of-numbers]
; Builds a list by applying sqr on the numbers 0 to n.
(define (tab-sqr n)
  (tabulate sqr n))
(check-expect (tab-sqr 3) '(9 4 1 0))
(check-expect (tab-sqr 10) '(100 81 64 49 36 25 16 9 4 1 0))


;; tab-tan : n -> list[of-numbers]
; Builds a list by applying tan on the numbers 0 to n.
(define (tab-tan n)
  (tabulate tan n))
(check-within (tab-tan 3) '(#i-0.1425465430742778
                            #i-2.185039863261519
                            #i1.557407724654902
                            0) 0.01)
(check-within (tab-tan 10) `(#i0.6483608274590867
                             #i-0.4523156594418099
                             #i-6.799711455220378
                             #i0.8714479827243188
                             #i-0.29100619138474915
                             #i-3.380515006246585
                             #i1.1578212823495775
                             #i-0.1425465430742778
                             #i-2.185039863261519
                             #i1.557407724654902
                             0) 0.01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 167 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fold1 : {x x -> x} x list[of-x] -> x
; Recurses through list and runs the given function on the current number as
; well as the results of the function when run on the rest of the list.
(define (fold1 fxn base l)
  (cond
    [(empty? l) base]
    [else
     (fxn (car l)
          (fold1 fxn base (cdr l)))]))

(check-expect (fold1 + 0 '(1 2 3 4 5 6)) 21)
(check-expect (fold1 * 1 '(1 2 3 4 5 6)) 720)

;; sum : list[of-numbers] -> number
; Adds the numbers in the list together
(define (sum lst)
  (fold1 + 0 lst))

(check-expect (sum '(1 2 3 4 5 6)) 21)
(check-expect (sum '(1 2 3 4 7 5 6)) 28)

;; product : list[of-numbers] -> number
; Multiplies the given list together.
(define (product lst)
  (fold1 * 1 lst))
(check-expect (product '(1 2 3 4 5 6)) 720)
(check-expect (product '(1 2 7 3 4 5 6)) 5040)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 168 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; fold2 : {x y -> y} y list[of-x] -> y
; Recurses through list and runs the given function on the current number as
; well as the results of the function when run on the rest of the list.
(define (fold2 fxn base l)
  (cond
    [(empty? l) base]
    [else
     (fxn (car l)
          (fold2 fxn base (cdr l)))]))

(check-expect (fold2 + 0 '(1 2 3 4 5 6)) 21)
(check-expect (fold2 * 1 '(1 2 3 4 5 6)) 720)

; Posn Image -> Image
(define (place-dot p img)
  (place-image dot
               (posn-x p) (posn-y p)
               img))

; graphical constants:    
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

;; image* : list[posns] -> image
; Draws the given points onto an empty canvas.
(define (image* lst)
  (fold2 place-dot emt lst))

(check-expect (image* (list (make-posn 0 0)))
              (place-dot (make-posn 0 0) emt))
(check-expect (image* (list (make-posn 3 4)))
              (place-dot (make-posn 3 4) emt))
(check-expect (image* (list (make-posn 0 0)
                            (make-posn 3 4)))
              (place-dot (make-posn 3 4) (place-dot (make-posn 0 0) emt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; More ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ident x) x)

;; Use build-list to create:
; the lists (list 0 ... 3) and (list 1 ... 4);
(check-expect (build-list 4 ident)
              (list 0 1 2 3))

(check-expect (build-list 4 add1)
              (list 1 2 3 4))

; the list (list .1 .01 .001 .0001);
(check-expect (build-list 4 (lambda (x) (expt 10 (- (add1 x)))))
              (list .1 .01 .001 .0001))

;; evens : n -> list[numbers]
; Gives you the first n even numbers.
(define (evens n)
  (build-list n (lambda (x) (* (add1 x) 2))))

(check-expect (evens 1) '(2))
(check-expect (evens 2) '(2 4))
(check-expect (evens 3) '(2 4 6))
(check-expect (evens 4) '(2 4 6 8))


;; tabulate2 : {number -> number} number -> list[of-numbers]
; See tabulate above.
(define (tabulate2 fxn n)
  (reverse (build-list (add1 n) fxn)))

(check-expect (tabulate2 add1 0) '(1))
(check-expect (tabulate2 add1 1) '(2 1))
(check-expect (tabulate2 add1 10) '(11 10 9 8 7 6 5 4 3 2 1))

;; convert-euro : list[dollars] -> list[euros]
; Converts each dollar ammount to euros.
(define (convert-euro lst)
  (map (lambda (x) (* x 1.22)) lst))
(check-expect (convert-euro (list 1)) (list 1.22))
(check-expect (convert-euro (list 1 2)) (list 1.22 2.44))

;; convertFC : list[fahrenheit] -> list[celsius]
; Converts a list of Fahrenheit temperatues into celsius.
(define (convertFC lst)
  (map (lambda (x) (* (- x 32) 5/9)) lst))
(check-within (convertFC '(1)) '(-17.222) 0.01)
(check-within (convertFC '(1 2)) '(-17.222 -16.667) 0.01)

;; move-all : list[posns] -> list[posns]
; Moves all the posns 3 units to the right
(define (move-all lst)
  (map (lambda (x) (make-posn (+ 3 (posn-x x))
                              (posn-y x))) lst))
(check-expect (move-all (list (make-posn 0 0))) (list (make-posn 3 0)))
(check-expect (move-all (list (make-posn 0 0)
                              (make-posn 4 5)))
              (list (make-posn 3 0) (make-posn 7 5)))
