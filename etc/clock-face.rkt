;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname clock-face) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)


(define clock-radius 700)
(define face-color "black")
(define face-style "outline")

(define sec-hand-color "black")
(define sec-hand-length (* clock-radius 9/10))

(define minute-hand-color "black")
(define minute-hand-length (* clock-radius 2/3))

(define hour-hand-color "black")
(define hour-hand-length (* clock-radius 1/2))

(define ticks-per-seconds 28) ;; Normally 28


(define center-x clock-radius)
(define center-y clock-radius)

;; ticks->secs : number -> number
;; Converts animate ticks to seconds.
(define (ticks->secs ticks)
  (/ ticks ticks-per-seconds))

;; ticks->minutes : number -> number
;; Takes ticks and converts to minutes
(define (ticks->minutes ticks)
  (/ (ticks->secs ticks) 60))


;; ticks->hours : number -> number
;; Takes ticks and converts to hours
(define (ticks->hours ticks)
  (/ (ticks->minutes ticks) 60))

;; fractional-part : real -> real
;; Returns the fractional part of the argument.
(define (fractional-part x)
  (- x (round x)))

;; draw-arrow : degrees -> image
;; Takes degrees and returns a properly rotated and pinholed arrow.
(define (draw-arrow degrees length color)
  (rotate
     (- (modulo (round (- 180 degrees)) 360) (fractional-part degrees)) ;; Instead of round, we should use fraction->decimal
     (put-pinhole 0 0
                  (rectangle 1 length "solid" color))))
  

;; clock : number -> image
;; Takes the ticks and draws the appropriate clock face.
(define (clock fn)
  (clear-pinhole
   (overlay/pinhole
    (draw-arrow (* 6 (ticks->secs fn)) sec-hand-length sec-hand-color)
    (draw-arrow (* 6 (ticks->minutes fn)) minute-hand-length minute-hand-color)
    (draw-arrow (* 6 (ticks->hours fn)) hour-hand-length hour-hand-color)
    (circle clock-radius face-style face-color))))

(define final-ticks (big-bang 0 (on-tick add1 1) (to-draw clock)))


(string-append (number->string (ticks->secs final-ticks)) " secs")
(string-append (number->string (ticks->minutes final-ticks)) " minutes")
(string-append (number->string (ticks->hours final-ticks)) " hours")