;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #28: Musical Notes
; Andrew Amis
; 12.6.11
; http://fellowhuman.com/gbk/2011/12/06/prog-1-asg-28-musical-notes/


;; note : string number number
(define-struct note (letter octave duration))
; letter -> string representing letter of the note. A-G inclusive.
; octave -> number representing the octave of the note. 0-9 inclusive
; duration -> number representing the duration of the note in seconds.

; Template
#;(define (fun-for-note note)
    ... (note-letter note) ...
    ... (note-octave note) ...
    ... (note-duration note) ...)

; Another template
#;(define (fun-for-note note)
    (make-note (note-letter note)
               (note-octave note)
               (note-duration note)))
; (make-note "A" 1 4)

;; good-note : note -> note
; If the note is acceptable, returns the note. Otherwise, returns an error.
; Acceptable notes are those whose letter are A-G inclusive and whose octave
; is 0-9
(define (good-note note)
  (if (and (string? (note-letter note))
           (string<=? "A" (note-letter note) "G"))
      (if (and (number? (note-octave note))
               (<= 0 (note-octave note) 9))
          note
          (error "note error: octave wrong"))
      (error "note error: letter wrong")))
(check-expect (good-note (make-note "A" 3 1)) (make-note "A" 3 1))
(check-error (good-note (make-note "X" 3 1)) "note error: letter wrong")
(check-error (good-note (make-note "A" -1 1)) "note error: octave wrong")
(check-error (good-note (make-note "A" 10 1)) "note error: octave wrong")
(check-error (good-note (make-note -3 10 1)) "note error: letter wrong")

;; raise-octave : note -> note
; Raises the note one octave.
(define (raise-octave note)
  (good-note
   (make-note (note-letter note)
              (add1 (note-octave note))
              (note-duration note))))
(check-expect (raise-octave (make-note "A" 1 4)) (make-note "A" 2 4))
(check-expect (raise-octave (make-note "A" 1 5)) (make-note "A" 2 5))
(check-expect (raise-octave (make-note "D" 1 5)) (make-note "D" 2 5))
(check-expect (raise-octave (make-note "D" 5 5)) (make-note "D" 6 5))

;; same-pitch : note note -> boolean
; Returns true iff the letter and octave of the 2 notes are the same
