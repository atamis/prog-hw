;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname asg11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

(define (pinwheel image)
  (above
   (beside
    image
    (rotate-cw image))
   (beside
    (rotate-ccw image)
    (rotate-180 image))))

(define thesquare (square 10 "solid" "blue"))
(define thecircle (circle 10 "solid" "green"))


(check-expect (pinwheel thecircle)
              (above
               (beside thecircle (rotate-cw thecircle))
               (beside (rotate-ccw thecircle) (rotate-180 thecircle))))
(check-expect (pinwheel thesquare)
              (above
               (beside thesquare (rotate-cw thesquare))
               (beside (rotate-ccw thesquare) (rotate-180 thesquare))))


(define (lollipop radius height color)
  (above
   (circle radius "solid" color)
   (rectangle 1 height "solid" color)))

(check-expect
 (lollipop 50 100 "blue")
 (above
  (circle 50 "solid" "blue")
  (rectangle 1 100 "solid" "blue")))
(check-expect
 (lollipop 30 40 "red")
 (above
  (circle 30 "solid" "red")
  (rectangle 1 40 "solid" "red")))


;; multi-pinwheel : image number
;; Recursively calls pinwheel on the given image max number of times
(define (multi-pinwheel image max)
  ;; image: the image to pinwheel (may have already been pinwheeled)
  ;; max: the number of times to pinwheel something (approaches 0)
  (if (= max 0)
      image
      (multi-pinwheel (pinwheel image) (- max 1))))

(define my-lollipop (lollipop 3 1 "black"))

(check-expect
 (multi-pinwheel my-lollipop 1)
 (pinwheel my-lollipop))
(check-expect
 (multi-pinwheel my-lollipop 2)
 (pinwheel (pinwheel my-lollipop)))
(check-expect
 (multi-pinwheel my-lollipop 3)
 (pinwheel (pinwheel (pinwheel my-lollipop))))
