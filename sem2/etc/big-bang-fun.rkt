;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname big-bang-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Andrew Amis, 9.15.11
;; big-bang exploration

(require picturing-programs)

;; thunk : x -> x
;; Returns its one argument 
(define (ident x) x)

(define-struct ufo-state (locx locy vecx vecy ticks))

(define DELTA 10)
(define FRICTION 0.91)
(define WIDTH 600)
(define HEIGHT 600)


(define (apply-vector state)
  (make-ufo-state
   (modulo (round (+
                   (ufo-state-locx state)
                   (ufo-state-vecx state))) WIDTH)
   (modulo (round (+ 
                   (ufo-state-locy state)
                   (ufo-state-vecy state))) HEIGHT)
   (* (ufo-state-vecx state) FRICTION)
   (* (ufo-state-vecy state) FRICTION)
   (ufo-state-ticks state)))


(define (inspect-state state)
  (string-append
   (number->string (round (ufo-state-locx state))) " "
   (number->string (round (ufo-state-locy state))) " "
   (number->string (round (ufo-state-vecx state))) " "
   (number->string (round (ufo-state-vecy state))) " "
   (number->string (round (ufo-state-ticks state)))))

(check-expect (inspect-state (make-ufo-state 0 0 0 0 0)) "0 0 0 0 0")

(define (create-UFO-scene state)
  (scene+line (overlay/align
               "left" "top" 
               (text (inspect-state state) 10 "black") 
               (place-image UFO 
                            ; This makes the UFO wobble back and forth horizontally
                            (+ (* 10 (sin (* 1/6 (ufo-state-ticks state))))
                               (ufo-state-locx state)) ;; This makes the UFO wobble back and forth horizontally.
                            (ufo-state-locy state) 
                            (empty-scene WIDTH HEIGHT)))
              ; This code draws the acceleration line. scene-line is really
              ; annoying in that their first argment is the image to operate on,
              ; while place-image has the image as the last argument. This leads
              ; to confusing code.
              (ufo-state-locx state) (ufo-state-locy state)
              (+ (ufo-state-locx state) (ufo-state-vecx state))
              (+ (ufo-state-locy state) (ufo-state-vecy state))
              "black"))



;; increment-ticks : state -> state
;; Increments the ticks.
(define (increment-ticks state)
  (make-ufo-state
   (ufo-state-locx state)
   (ufo-state-locy state)
   (ufo-state-vecx state)
   (ufo-state-vecy state)
   (add1 (ufo-state-ticks state))))


(check-expect (increment-ticks (make-ufo-state 0 0 0 0 0))
              (make-ufo-state 0 0 0 0 1))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

(define (handle-tick state)
  (increment-ticks (apply-vector state)))

(define (handle-key state key)
  (cond
    [(key=? key "h") (make-ufo-state
                      (ufo-state-locx state)
                      (ufo-state-locy state)
                      (- (ufo-state-vecx state) DELTA)
                      (ident (ufo-state-vecy state))
                      (ufo-state-ticks state))]
    [(key=? key "l") (make-ufo-state
                      (ufo-state-locx state)
                      (ufo-state-locy state)
                      (+ (ufo-state-vecx state) DELTA)
                      (ident (ufo-state-vecy state))
                      (ufo-state-ticks state))]
    [(key=? key "j") (make-ufo-state
                      (ufo-state-locx state)
                      (ufo-state-locy state)
                      (ident (ufo-state-vecx state))
                      (+ (ufo-state-vecy state) DELTA)
                      (ufo-state-ticks state))]
    [(key=? key "k") (make-ufo-state
                      (ufo-state-locx state)
                      (ufo-state-locy state)
                      (ident (ufo-state-vecx state))
                      (- (ufo-state-vecy state) DELTA)
                      (ufo-state-ticks state))]
    [(key=? key "escape") (make-ufo-state 0 0 0 0 (ufo-state-ticks state))]
    [else state]))


(big-bang (make-ufo-state 50 50 10 10 0)
          (on-key handle-key)
          (to-draw create-UFO-scene)
          (on-tick handle-tick)
          (state true))