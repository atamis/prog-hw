;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname asg5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Andrew Amis
;; http://www.fellowhuman.com/gbk/2011/09/14/prog-1-asg-5-defining-functions/

#|
How does a function definition look different from a variable definition?
     Surrounds the primary identfier with parentheses, and inside the parentheses, there are more secondary identifiers
What is a function header?  Give an example to show what function headers look like.
     It includes the define identifier, the primary identifers, and the arguments: (define (awesome thing)
What is a function body?  Where does it belong in a function definition?
      It is a single expression that comes after the function header and before the ending parenthesis.
Why would we define a function?  In other words, what’s the purpose of defining a function?
     To package code up into small bits that are easy to use, test, and maintain.
What is an argument?
     A specific value that a paremeter is assigned when the function is called.
What is a parameter?  How is it different from an argument?
     It is the "placeholder" or local binding that is set when the function is called.
|#

(require picturing-programs)

;; Excersize 4.2.1

;; vert-mirror-image : image -> image
;; Mirrors an image vertically
 (define (vert-mirror-image image)
   (above
    image
    (flip-vertical image)))
 
 (vert-mirror-image (circle 10 "solid" "blue"))
 (vert-mirror-image (triangle 10 "solid" "blue"))
 
 ;; Excersize 4.2.2
 
 ;; four-square : image -> image
 (define (four-square image)
   (beside
    (above image image)
    (above image image)))
 
 (four-square (circle 10 "solid" "blue"))
 (four-square (triangle 10 "solid" "blue"))
 
 ;; Excersize 4.2.4
 
 ;; surround : image(first) image(second) -> image
 (define (surround image1 image2)
   (beside image2 image1 image2))
 
 
 (surround (triangle 10 "solid" "blue") (triangle 10 "solid" "blue"))
 (surround (triangle 10 "solid" "blue") (circle 10 "solid" "blue"))
 (surround (circle 10 "solid" "blue") (triangle 10 "solid" "blue"))
 