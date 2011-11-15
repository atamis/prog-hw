;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #24: Errors
; Andrew Amis
; 11.15.11
; http://fellowhuman.com/gbk/2011/11/14/prog-1-asg-24-errors/
; 19.3.2-19.3.4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 19.3.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; chop-first-char : string -> string
; Removes the first character of the given string
(define (chop-first-char string)
  ; string  | string | "test"
  ; returns | string | "est"
  (if (= 0 (string-length string))
      (error 'chop-first-char "can’t chop from an empty string")
      (substring string 1)))
(check-expect (chop-first-char "test") "est")
(check-expect (chop-first-char "awesome") "wesome")
(check-expect (chop-first-char "blagoyavich") "lagoyavich")
(check-error (chop-first-char "")
             "chop-first-char: can’t chop from an empty string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 19.3.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first-char : string -> string
; Returns the first character in the string
(define (first-char string)
  ; string  | string | "test"
  ; returns | string | "t"
  (if (= 0 (string-length string))
      (error 'first-char "can't get first character of an empty string")
      (substring string 0 1)))

(check-expect (first-char "test") "t")
(check-expect (first-char "awesome") "a")
(check-expect (first-char "yes, I would like some more cake") "y")
(check-error (first-char "")
             "first-char: can't get first character of an empty string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 19.3.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct not-handled (x))

;; inspect : (string | number | boolean | posn) -> string
; Returns a string representing the argument. Handles a few types.
(define (inspect x)
  (cond
    [(string? x) x]
    [(number? x) (number->string x)]
    [(boolean? x) (if x "true" "false")]
    [(posn? x)
     (string-append "("
                    (inspect (posn-x x))
                    ", "
                    (inspect (posn-y x))
                    ")")]
    [else(error 'inspect "type not supported")]))
(check-expect (inspect 1) "1")
(check-expect (inspect 14) "14")
(check-expect (inspect "test") "test")
(check-expect (inspect true) "true")
(check-expect (inspect false) "false")
(check-expect (inspect (make-posn 10 10)) "(10, 10)")
(check-error (inspect (make-not-handled 10)) "inspect: type not supported")

;; safe-double : number -> number
; Doubles the given number, errors if not a number.
(define (safe-double n)
  (if (number? n)
      (* 2 n)
      (error 'safe-double (string-append (inspect n) " is not a number"))))
(check-expect (safe-double 4) 8)
(check-expect (safe-double 10) 20)
(check-error (safe-double "test") "safe-double: test is not a number")
(check-error (safe-double false) "safe-double: false is not a number")