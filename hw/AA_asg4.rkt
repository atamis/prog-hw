;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname asg4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Asg 4
;; Andrew Amis
;; http://www.fellowhuman.com/gbk/2011/09/13/prog-1-asg-4-image-practice/


(require picturing-programs)

;; Excersize 3.5.3
;; Play with these.

(define (my-crop-left image number)
  (rotate-cw (crop-bottom (rotate-ccw image) number)))

(define tmp-circle
  (circle 10 "solid" "blue"))

(define my-cropped 
  (my-crop-left tmp-circle 5))

(define cropped
  (crop-left tmp-circle 5))

my-cropped
cropped

(image=? 
 my-cropped
 cropped) ; => true

;; Excersize 3.5.4
;; An expression that gives the bottom 45 pixels of an image

(crop-top (circle 10 "solid" "blue") 5) ;; It's like (shoot (barrel-fish main-barrel)

;; Excersize 3.5.5
;; Get a quarter of an image. In this case, an ellipse

(crop-left (crop-bottom (ellipse 50 30 "solid" "green") 15) 25)

;; Excsersize 3.5.6
;; Not again...
(define ellipse-quarter
  (crop-left (crop-bottom (ellipse 50 30 "solid" "green") 15) 25))

(define ellipse-half
  (above ellipse-quarter (flip-vertical ellipse-quarter)))

(define stupid-ellipse
  (beside (flip-horizontal ellipse-half) ellipse-half))

stupid-ellipse
(ellipse 50 30 "solid" "green")
(image=? stupid-ellipse (ellipse 50 30 "solid" "green"))

;; Excersize 3.5.10

(define inner-border-size 4)
(define outer-border-size 6)

;; add-border : image number(size) string(color) -> image
;; Adds a border to the supplied image size px wide and color color
(define (add-border image size color)
  (overlay
   image
   (rectangle
    (+ size (image-width image))
    (+ size (image-height image))
    "solid" color)))

;; a-word : string -> image
;; Gives a string a background and border in some god awful colors
(define (a-word string)
  ;;(define word (text string)) I want to do this!
  (add-border
   (overlay
    (text string 15 "blue")
    (rectangle
     (+ inner-border-size (image-width (text string 15 "blue"))) ;; DRY is falling by the wayside. Without let, I can't do local bindings. Without begin, I can't define things in functions :(
     (+ inner-border-size (image-height (text string 15 "blue"))) "solid" "yellow"))
   outer-border-size "purple"))

(a-word "this")
(a-word "this is a test")
