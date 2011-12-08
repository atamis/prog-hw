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

;; same-pitch? : note note -> boolean
; Returns true iff the letter and octave of the 2 notes are the same
(define (same-pitch? n1 n2)
  (and (string=? (note-letter n1) (note-letter n2))
       (= (note-octave n1) (note-octave n2))))
(check-expect (same-pitch? (make-note "A" 1 1) (make-note "A" 1 1)) true)
(check-expect (same-pitch? (make-note "A" 1 1) (make-note "A" 1 2)) true)
(check-expect (same-pitch? (make-note "A" 1 1) (make-note "A" 1 9)) true)
(check-expect (same-pitch? (make-note "B" 1 1) (make-note "A" 1 9)) false)
(check-expect (same-pitch? (make-note "B" 1 1) (make-note "A" 1 8)) false)
(check-expect (same-pitch? (make-note "A" 1 1) (make-note "A" 2 9)) false)

;; note<? : note note -> boolean
; Returns true if the pitch of the first note is lower than the pitch of the
; second
(define (note<? n1 n2)
  (cond
    [(< (note-octave n1) (note-octave n2)) true]
    [(> (note-octave n1) (note-octave n2)) false]
    [else
     (if (and (string<=? "C" (note-letter n1) "G")
              (string<=? "C" (note-letter n2) "G"))
         (string<=? (note-letter n1) (note-letter n2))
         (if (and (or (string=? (note-letter n1) "A")
                      (string=? (note-letter n1) "B"))
                  (or (string=? (note-letter n2) "A")
                      (string=? (note-letter n2) "B")))
             (string<? (note-letter n1) (note-letter n2))
             (not (string<? (note-letter n1) (note-letter n2)))))]))

(check-expect (note<? (make-note "C" 1 1) (make-note "C" 2 1)) true)
(check-expect (note<? (make-note "C" 3 1) (make-note "C" 4 1)) true)
(check-expect (note<? (make-note "C" 3 1) (make-note "C" 2 1)) false)
(check-expect (note<? (make-note "C" 1 1) (make-note "D" 1 1)) true)
(check-expect (note<? (make-note "C" 1 1) (make-note "E" 1 1)) true)
(check-expect (note<? (make-note "C" 1 1) (make-note "F" 1 1)) true)
(check-expect (note<? (make-note "C" 1 1) (make-note "G" 1 1)) true)
(check-expect (note<? (make-note "C" 1 1) (make-note "A" 1 1)) true)
(check-expect (note<? (make-note "C" 1 1) (make-note "B" 1 1)) true)
(check-expect (note<? (make-note "B" 1 1) (make-note "A" 1 1)) false)
(check-expect (note<? (make-note "B" 1 1) (make-note "C" 1 1)) false)
(check-expect (note<? (make-note "B" 1 1) (make-note "C" 2 1)) true)
(check-expect (note<? (make-note "F" 1 1) (make-note "E" 1 1)) false)

;; Mr. Johnson's tests for note<? (modified slightly)
(define c4-short (make-note "C" 4 .5))
(define c4-long (make-note "C" 4 2))
(define b3 (make-note "B" 3 .5))
(define a3 (make-note "A" 3 .5))
(define g3 (make-note "G" 3 .5))
(define c3 (make-note "C" 3 .5))

(check-expect (note<? c4-short g3) false)
(check-expect (note<? a3 b3) true)       ;; same octave, alpha order right
(check-expect (note<? b3 a3) false)
(check-expect (note<? c3 g3) true)
(check-expect (note<? g3 c3) false)
(check-expect (note<? g3 a3) true)       ;; same 8ve, alpha order wrong
(check-expect (note<? a3 g3) false)
(check-expect (note<? g3 b3) true)
(check-expect (note<? b3 g3) false)
(check-expect (note<? c3 b3) true)
(check-expect (note<? b3 c3) false)
(check-expect (note<? b3 b3) false)      ;; same note


;; augment : note -> note
; Takes a note and doubles its duration
(define (augment note)
  (make-note (note-letter note)
             (note-octave note)
             (* 2 (note-duration note))))

(check-expect (augment (make-note "C" 1 1)) (make-note "C" 1 2))
(check-expect (augment (make-note "C" 1 4)) (make-note "C" 1 8))
(check-expect (augment (make-note "C" 1 3.5)) (make-note "C" 1 7))

;; interval : note note
; A struct holding 2 notes representing a musical interval
(define-struct interval (f l))
; interval-f -> [f]irst note in the interval
; interval-l -> [l]ast note in the interval

#;(make-interval (make-note "C" 1 1) (make-note "C" 1 1))

; Templates
#;(define (fun-for-interval interval)
    ... (interval-f interval) ...
    ... (interval-l interval) ...)
#;(define (fun-for-interval interval)
    (make-interval
     (make-note (note-letter (interval-f interval))
                (note-octave (interval-f interval))
                (note-duration (interval-f interval)))
     (make-note (note-letter (interval-l interval))
                (note-octave (interval-l interval))
                (note-duration (interval-l interval)))))

;; rising? : interval -> boolean
; Boolean whether the interval is rising, whether the first note is lower than
; the second note.
(define (rising? interval)
  (note<? (interval-f interval) (interval-l interval)))
(check-expect (rising? (make-interval (make-note "C" 1 1) (make-note "C" 2 1)))
              true)
(check-expect (rising? (make-interval (make-note "C" 2 1) (make-note "C" 1 1)))
              false)

;; A note-letter is a one-character string containing a
;; capital letter between A and G (inclusive).
;;
;; A note-name is either a note-letter, or
;; (string-append note-letter "b") ;; for flat, or
;; (string-append note-letter "#") ;; for sharp.

;; Examples:
(define bflat "Bb")
(define asharp "A#")
(define d "D")
;; Template:
#;(define (fun-for-note-name nn)
  (cond
    [(note-letter? nn) ...]
    [else
     ... (string-ith nn 0)  ;; the letter
     ... (string-ith nn 1)  ;; the sharp (#) or flat (b)
     ...]))

;; note2 : note-letter number number
(define-struct note2 letter octave duration)
; note2-letter : a note-letter representing the letter of the note
; note2-octave : number representing the octave of the note
; note2-duration : number representing the duration of the note in seconds
; (make-note2 "A#" 4 3)
; Templates for note apply

