#lang racket

(require picturing-programs
         test-engine/racket-tests
         (prefix-in plot: plot))


;; normalize : number -> number
; Normalizes the number to a 0-255 range
(define (normalize n max)
  (round (* 255 (/ n max))))

(check-within (normalize 50 100) 128 0.1)
(check-within (normalize 33 100) 84 0.1)

;; fractional-part : real -> real
;; Returns the fractional part of the argument.
(define (fractional-part x)
  (- x (floor x)))

;; guage-color : number -> color
; If the number is less than 10, red. Less than 25, yellow, otherwise, green.
(define (guage-color n)
  (cond
    ([< n 10] 'red)
    ([< n 25] 'yellow)
    (else 'green)))
(check-expect (guage-color 100) 'green)
(check-expect (guage-color 50) 'green)
(check-expect (guage-color 24) 'yellow)
(check-expect (guage-color 4) 'red)

;; progress-bar : number(width) number(height) number(%) -> image
;; Takes a width, height, and percentage (between 0 and 1)
;; and produces a progress bar.
(define (progress-bar width height n%)
  (beside
   (rectangle (* width n%) height 'solid (guage-color (* 100 n%)))
   (rectangle (* width (- 1 n%)) height 'solid "black")))

;; seconds-into-minute : number -> number
; Converts the seconds to seconds after the start of the minute. Returns a 
; number between 1 and 59.
(define (seconds-into-minute sec)
  (modulo (round sec) 60))
(check-expect (seconds-into-minute 0) 0)
(check-expect (seconds-into-minute 4) 4)
(check-expect (seconds-into-minute 60) 0)
(check-expect (seconds-into-minute 63) 3)

;; seconds->minutes : number -> number
; Converts seconds to minutes.
(define (seconds->minutes seconds)
  (/ seconds 60))
(check-within (seconds->minutes 60) 1 0.1)
(check-within (seconds->minutes 120) 2 0.1)
(check-within (seconds->minutes 50) 0.833 0.1)

;; minutes->hours : number -> number
; Converts minutes to hours.
(define (minutes->hours minutes)
  (/ minutes 60))

;; seconds->hours : number -> number
; Converts seconds to hours
(define (seconds->hours seconds)
  (minutes->hours (seconds->minutes seconds)))

;; minutes-into-hour : number -> number
; How many minutes into an hour are we for a given number of seconds?
(define (minutes-into-hour seconds)
  (+ (modulo (round (seconds->minutes seconds)) 60)
     (fractional-part (seconds->minutes seconds))))

(check-expect (minutes-into-hour 60) 1)
(check-expect (minutes-into-hour 120) 2)
(check-expect (minutes-into-hour 3660) 1)

;; hours-into-day : number -> number
; How many hours are we into this day?
(define (hours-into-day seconds)
  (+ (modulo (round (seconds->hours seconds)) 24)
     (fractional-part (seconds->hours seconds))))

(define (text-time ticks)
  (above
   (progress-bar 100 10 (/ (seconds-into-minute #;(* 30 ticks) (current-seconds)) 60))
   (progress-bar 100 10 (/ (minutes-into-hour #;(* 30 ticks) (current-seconds)) 60))
   (progress-bar 100 10 (/ (hours-into-day #;(* 30 ticks) (current-seconds)) 24))))


(plot:plot
 (plot:line seconds-into-minute)
 #:x-min 0
 #:x-max 1000
 #:y-min 0
 #:y-max 25)

(animate text-time)

(test)