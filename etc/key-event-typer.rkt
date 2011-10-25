;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname key-event-typer) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

;; Constants
(define TEXT-SIZE 11)
(define TEXT-COLOR 'red)
(define TEXT-WIDTH (image-width (text "t" TEXT-SIZE TEXT-COLOR))) ; Not used

;; Struct for world
; (world-string (make-world "test" 3)) => "test"
; (world-loc (make-world "test" 3)) => 3
(define-struct world (string loc))


;; insert-at : string string number
; Inserts the second string number characters into the first string
(define (insert-at main-string sub-string number)
  ; main-string | string | "test"
  ; sub-string  | string | "a"
  ; number      | number | 3 
  ; returns     | string | "tesat"
  (if (< number (string-length main-string))
      (string-append (substring main-string 0 number) sub-string (substring main-string number))
      (string-append main-string sub-string)))


(check-expect (insert-at "test" "a" 3) "tesat")
(check-expect (insert-at "test" "a" 0) "atest")
(check-expect (insert-at "test" "a" 5) "testa")

;; delete-at : string number
; Deltes a character at the given position
(define (delete-at string position)
  (string-append
   (substring string 0 (max 0 (sub1 position)))
   (substring string position)))

(check-expect (delete-at "test" 4) "tes")
(check-expect (delete-at "test" 3) "tet")
(check-expect (delete-at "test" 0) "test")
(check-expect (delete-at "test" 1) "est")
(check-expect (delete-at "" 0) "")


;; Note: chop-last-letter is no longer used

;; chop-last-letter : string -> string
; Removes the last letter from the string
(define (chop-last-letter string)
  ; string  | string | "test"
  ; returns | string | "tes"
  (substring string 0 (max (- (string-length string) 1) 0)))

(check-expect (chop-last-letter "test") "tes")
(check-expect (chop-last-letter "awesome") "awesom")
(check-expect (chop-last-letter "asdf") "asd")
(check-expect (chop-last-letter "") "")

;; handle-keyboard : world key-event -> world
; Processes the world. Moves the cursor back and forth, adds and deletes
; characters, etc. Lots of test.
(define (handle-keyboard world key)
  ; world   | world     | "test"
  ; key     | key-event | "a"
  ; returns | world     | "testa"
  (cond
    [(key=? key "\b")
     (make-world
      (delete-at (world-string world) (world-loc world))
      (max 0 (sub1 (world-loc world))))]
    [(key=? key "left")
     (make-world (world-string world)
                 (max 0 (sub1 (world-loc world))))]
    [(key=? key "right")
     (make-world
      (world-string world)
      (min (string-length (world-string world))
           (add1 (world-loc world))))]
    [else
     (make-world
      (string-append (world-string world) key)
      (+ (string-length key) (world-loc world)))]))


;; So many possibilities...
(check-expect (handle-keyboard (make-world "tes"  3) "t")  (make-world "test" 4))
(check-expect (handle-keyboard (make-world "tes"  3) "up") (make-world "tesup" 5))
(check-expect (handle-keyboard (make-world ""     0) "t")  (make-world "t" 1))
(check-expect (handle-keyboard (make-world "test" 4) "\b") (make-world "tes" 3))
(check-expect (handle-keyboard (make-world "test" 4) "left")  (make-world "test" 3))
(check-expect (handle-keyboard (make-world "test" 3) "right") (make-world "test" 4))
(check-expect (handle-keyboard (make-world "test" 4) "right") (make-world "test" 4))
(check-expect (handle-keyboard (make-world "test" 0) "left")  (make-world "test" 0))
(check-expect (handle-keyboard (make-world ""     0) "right") (make-world "" 0))
(check-expect (handle-keyboard (make-world ""     0) "left") (make-world "" 0))

;; Note: character-width is not used

;; character-width : number(size) -> number
; Computes the width of a single character of a given size.
(define (character-width size)
  (image-width (text "t" size 'black)))

(check-expect (character-width 10) 3)
(check-expect (character-width 12) 3)
(check-expect (character-width 20) 6)

;; draw-world : world -> image
; Draws the world as an image. Draws the text to the image.
(define (draw-world world)
  (text (insert-at (world-string world) "|" (world-loc world)) ; Messy
        TEXT-SIZE TEXT-COLOR))



(check-expect (draw-world (make-world "test" 0))
              (text "|test" TEXT-SIZE TEXT-COLOR))
(check-expect (draw-world (make-world "awesome" 4))
              (text "awes|ome" TEXT-SIZE TEXT-COLOR))
(check-expect (draw-world (make-world "asdf" 4))
              (text "asdf|" TEXT-SIZE TEXT-COLOR))

(big-bang (make-world "" 0)
          (to-draw draw-world 300 20)
          (on-key handle-keyboard))