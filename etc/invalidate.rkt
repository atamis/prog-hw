;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invalidate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

;; smallest-image-attribute : image -> number
;; Returns the smaller of the 2, image hieght or image width.
(define (smallest-number-attribute image)
  (min (image-height image) (image-width image)))

;; apropriate-text-size : image -> number
;; Calculate text size from image-height
(define (apropriate-text-size image)
  (round (- (/ (smallest-number-attribute image) 2) (/ (smallest-number-attribute image)8))))


;; voidify : image -> image
;; Prints void on the image, diagonally
(define (voidify image)
  (overlay (rotate 45 (text "void" (apropriate-text-size image) "red"))
           image))

(voidify (empty-scene 50 50))

;; 1,  2
;; 2,  4
;; 3,  6
;; 5,  11
;; 9,  19
;; 10, 22
;; 13, 29