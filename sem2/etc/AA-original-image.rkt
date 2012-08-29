;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-original-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

(define eye (overlay (underlay (ellipse 10 60 40 "red")
                               (ellipse 20 50 40 "red")
                               (ellipse 30 40 40 "red")
                               (ellipse 40 30 40 "red")
                               (ellipse 50 20 40 "red")
                               (ellipse 60 10 40 "red"))
                     (circle 30 "outline" "red")))

(define arrow (polygon (list (make-posn 0 0)
                             (make-posn -10 20)
                             (make-posn 60 0)
                             (make-posn -10 -20))
                       "solid"
                       "burlywood"))

(define up-arrow (rotate-ccw arrow))
(define down-arrow (rotate-cw arrow))

(define face 
  (add-curve (add-curve 
              (above
               (beside up-arrow up-arrow up-arrow)
               (beside (flip-horizontal arrow) eye eye arrow)
               (beside down-arrow down-arrow down-arrow))
              60 150 -30 1/3
              200 150 30 1/3
              "black")
  60 150 -50 1/3
  200 150 50 1/3
  "black"))
face