;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-asg9-local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #9: Local definitions
; Andrew Amis
; 3.19.12
; http://fellowhuman.com/gbk/2012/03/15/prog-2-asg-9-local-definitions/

(require picturing-programs)

;; remove-all-red : listof[colors] -> listof[colors]
; Removes all the red from an image.
(define (remove-all-red colors)
  (local [;; replace-color : {color -> color} listof[colors] -> listof[colors]
          ; Replaces each color in the list with the result of the function
          ; on that color.
          (define (replace-colors fxn colors)
            (map fxn colors))
          ;; remove-red : color -> color
          (define (remove-red color)
            (make-color 0 (color-green color)
                        (color-blue color)))]
    (replace-colors remove-red colors)))

(check-expect (remove-all-red (list (make-color 255 255 255)
                                    (make-color 255 255 255)))
              (list (make-color 0 255 255) (make-color 0 255 255)))

;; Because I used lambda when I first wrote evens and convertFC, adding local
; would be counter intuitive and would increase the size of each function.

;; evens : n -> list[numbers]
; Gives you the first n even numbers.
(define (evens n)
  (build-list n (lambda (x) (* (add1 x) 2))))

(check-expect (evens 1) '(2))
(check-expect (evens 2) '(2 4))
(check-expect (evens 3) '(2 4 6))
(check-expect (evens 4) '(2 4 6 8))

;; convertFC : list[fahrenheit] -> list[celsius]
; Converts a list of Fahrenheit temperatues into celsius.
(define (convertFC lst)
  (map (lambda (x) (* (- x 32) 5/9)) lst))
(check-within (convertFC '(1)) '(-17.222) 0.01)
(check-within (convertFC '(1 2)) '(-17.222 -16.667) 0.01)