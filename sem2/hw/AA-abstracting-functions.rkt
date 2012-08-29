;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname AA-abstracting-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #7: Abstracting Functions
; Andrew Amis
; Started: 3/7/12
; Ended: 3/8/12
; http://fellowhuman.com/gbk/2012/03/05/prog-2-asg-7-abstracting-functions/
; Exercises 188, 189, and 190, with modifications.

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 188 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; __filter : {number -> boolean} list
(define (__filter predicate l)
  (cond
    [(empty? l) empty]
    [else (cond
            [(predicate (first l))
             (cons (first l) (filter predicate (rest l)))]
            [else
             (filter predicate (rest l))])]))

(check-expect (__filter false? empty) empty)
(check-expect (__filter false? (list true false false true))
              (list false false))
(check-expect (__filter false? '(true true true)) empty)
#|
;; If I had lambdas, I'd do it like this: 
;; less-than : number -> {number -> boolean}
; Returns a function which takes a number and returns true if the number
; is less than the argument of this function.
(define (less-than-equal num)
  (lambda (x) (<= x num)))
(check-expect ((less-than-equal 10) 4) true)
(check-expect ((less-than-equal 10) 20) false)
(check-expect ((less-than-equal 10) 10) true)
(define (small lon num)
  (__filter (less-than-equal num) lon))
|#

;; small? : number -> boolean
; Returns whether the number is less than or equal to 10
(define (small? n)
  (<= n 10))
(check-expect (small? 10) true)
(check-expect (small? 9) true)
(check-expect (small? 11) false)

;; small : lon number -> lon
; Returns all the number less than or equal to the number from the list.
(define (small lon)
  (__filter small? lon))
(check-expect (small '(1 2 3 4 5 6 7 8 9 10))
              '(1 2 3 4 5 6 7 8 9 10))
(check-expect (small '(1 3 4 5 6 7 8 9 10 11))
              '(1 3 4 5 6 7 8 9 10))
(check-expect (small '(11 12 13 14 15 16))
              empty)

;; large? : number
; Returns whether a number is greater than or equal to 10
(define (large? num)
  (>= num 10))
(check-expect (large? 10) true)
(check-expect (large? 9) false)
(check-expect (large? 11) true)


;; large : lon number -> lon
; Returns a list where all the numbers less than 10 have been removed.
(define (large lon)
  (__filter large? lon))
(check-expect (large '(1 2 3 4 5 6 7 8 9 10))
              '(10))
(check-expect (large '(1 3 4 5 6 7 8 9 10 11))
              '(10 11))
(check-expect (large '(1 2 3 4))
              empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 189 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extreme : {number number -> boolean} NELoN -> number
; Returns the most extreme (determined by the predicate) number from the list.
; The predicate should take 2 numbers and return true if the first is "better"
; than the second
(define (extreme predicate l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (cond
       [(predicate (first l) (extreme predicate (rest l)))
        (first l)]
       [else
        (extreme predicate (rest l))])]))

(check-expect (extreme < '(1 2 3 4 5 6)) 1)
(check-expect (extreme > '(1 2 3 4 5 6)) 6)

;; inf-1 : NELoP -> number
; Returns the smallest number in the list
(define (inf-1 l)
  (extreme < l))

(check-expect (inf-1 '(1 2 3 4 5 6)) 1)

;; sup-1 : NELoP -> number
; Returns the largest number in the list
(define (sup-1 l)
  (extreme > l))

(check-expect (sup-1 '(1 2 3 4 5 6)) 6)


; The conds can be combined into one cond. You don't need that cond []
; expressions


;; extreme-2 : {number number -> number} NELoN -> number
; Returns the most extreme (determined by the predicate) number from the list.
; The predicate should take 2 numbers and return the better of the 2.
(define (extreme-2 pred l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (pred (car l) (extreme-2 pred (cdr l)))]))

(check-expect (extreme-2 min '(1 2 3 4 5 6)) 1)
(check-expect (extreme-2 max '(1 2 3 4 5 6)) 6)


;; inf-2 : NELoP -> number
; Returns the smallest number in the list
(define (inf-2 l)
  (extreme-2 min l))

(check-expect (inf-2 '(1 2 3 4 5 6)) 1)

;; sup-1 : NELoP -> number
; Returns the largest number in the list
(define (sup-2 l)
  (extreme-2 max l))

(check-expect (sup-2 '(1 2 3 4 5 6)) 6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; negative-color : color -> color
(define (negative-color color)
  (make-color (- 255 (color-red color))
              (- 255 (color-green color))
              (- 255 (color-blue color))))

(check-expect (negative-color (make-color 0 0 0)) (make-color 255 255 255))

;; replace-color : {color -> color} listof[colors] -> listof[colors]
; Replaces each color in the list with the result of the function on that color.
(define (replace-colors fxn colors)
  (cond [(empty? colors) empty]
        [(cons? colors)
         (cons (fxn (car colors))
               (replace-colors fxn (cdr colors)))]))



(check-expect (replace-colors negative-color (list (make-color 0 0 0)))
              (list (make-color 255 255 255)))


;; negate-all : listof[colors] -> listof[colors]
; Returns the photo negative.
(define (negate-all colors)
  (replace-colors negative-color colors))

(check-expect (negate-all (list (make-color 0 0 0)))
              (list (make-color 255 255 255)))


;; remove-red : color -> color
(define (remove-red color)
  (make-color 0 (color-green color)
              (color-blue color)))

;; remove-all-red : listof[colors] -> listof[colors]
; Removes all the red from an image.
(define (remove-all-red colors)
  (replace-colors remove-red colors))

(check-expect (remove-all-red (list (make-color 255 255 255)
                                    (make-color 255 255 255)))
              (list (make-color 0 255 255) (make-color 0 255 255)))

;; process-image : {listof[colors] -> listof[colors] } image -> image
; Returns an image processed by the given function
(define (process-image fxn img)
  (color-list->bitmap (fxn (image->color-list img))
                      (image-width img)
                      (image-height img)))


(check-expect (process-image remove-all-red (square 10 'solid 'red))
              (square 10 'solid 'black))

;; photo-negative : image -> image
; Returns the negative image
(define (photo-negative img)
  (process-image negate-all img))

(check-expect (photo-negative (square 10 'solid 'red))
              (square 10 'solid 'cyan))



;; redless-image : image -> image
; Returns the image without red
(define (redless-image img)
  (process-image remove-all-red img))

(check-expect (redless-image (square 10 'solid 'red))
              (square 10 'solid 'black))