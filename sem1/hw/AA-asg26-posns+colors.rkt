;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg26-posns+colors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 1 Asg #26: posns & colors
; Andrew Amis
; 12.1.11
; http://fellowhuman.com/gbk/2011/11/28/prog-1-asg-26-posns-colors/

(require picturing-programs)


;; add-posns : posn posn -> posn
; Adds the x and y of the posns, and makes a new posn from those x and y values.
(define (add-posns p1 p2)
  (make-posn 
   (+ (posn-x p1) (posn-x p2))
   (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns (make-posn 10 10) (make-posn 1 1)) (make-posn 11 11))
(check-expect (add-posns (make-posn 3 5) (make-posn 12 14)) (make-posn 15 19))
(check-expect (add-posns (make-posn 3 5) (make-posn 0 0)) (make-posn 3 5))
(check-expect (add-posns (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (add-posns (make-posn -1 -2) (make-posn 3 4)) (make-posn 2 2))



;; Width of the window
(define WIDTH 300)

;; Height of the window
(define HEIGHT 300)

;; Background of WIDTH and HEIGHT
(define BACKGROUND (empty-scene WIDTH HEIGHT))

;; A dot
(define DOT (circle 3 "solid" "blue"))

;; show-picture :  posn -> image
; Show the picture
(define (show-picture where)
  (place-image DOT
               (posn-x where) (posn-y where)
               BACKGROUND))

(check-expect (show-picture (make-posn 15 12))
              (place-image DOT 15 12 BACKGROUND))
(check-expect (show-picture (make-posn 27 149))
              (place-image DOT 27 149 BACKGROUND))

;; handle-key :  posn key -> posn
; Handle a key event
(define (handle-key where key)
  (add-posns where (cond [(key=?  key "up")
                          (make-posn 0 -1)]
                         [(key=?  key "down")
                          (make-posn 0 1)]
                         [(key=?  key "left")
                          (make-posn -1 0)]
                         [(key=?  key "right")
                          (make-posn 1 0)]
                         [else (make-posn 0 0) ])))

(check-expect (handle-key (make-posn 12 19) "e") (make-posn 12 19))
; ignore "e" by returning the same model we were given
(check-expect (handle-key (make-posn 12 19) "left") (make-posn 11 19))
; move left by decreasing the x coordinate
(check-expect (handle-key (make-posn 12 19) "right") (make-posn 13 19))
(check-expect (handle-key (make-posn 12 19) "up") (make-posn 12 18))
; remember that positive y-values are down
(check-expect (handle-key (make-posn 12 19) "down") (make-posn 12 20))



(define (standard-bang t)
  (big-bang (make-posn (/ WIDTH 2) (/ HEIGHT 2))
            (check-with posn?)
            (on-draw show-picture)
            (on-key handle-key)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Excersize 20.6.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Modify handle-key. Modified version seen above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Excersize 20.6.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (stop-when-handle-key where key)
  (if (key=? "q" key)
      false
      (add-posns where
                 (cond [(key=?  key "up")
                        (make-posn 0 -1)]
                       [(key=?  key "down")
                        (make-posn 0 1)]
                       [(key=?  key "left")
                        (make-posn -1 0)]
                       [(key=?  key "right")
                        (make-posn 1 0)]
                       [else (make-posn 0 0) ]))))


(check-expect (stop-when-handle-key (make-posn 12 19) "e")
              (make-posn 12 19))
(check-expect (stop-when-handle-key (make-posn 12 19) "left")
              (make-posn 11 19))
(check-expect (stop-when-handle-key (make-posn 12 19) "right")
              (make-posn 13 19))
(check-expect (stop-when-handle-key (make-posn 12 19) "up")
              (make-posn 12 18))
(check-expect (stop-when-handle-key (make-posn 12 19) "down")
              (make-posn 12 20))
(check-expect (stop-when-handle-key (make-posn 12 19) "q")
              false)

;; posn-stop-when : world -> boolean
; Whether to stop the world or not.
(define (posn-stop-when world)
  (and (boolean? world)
       (boolean=? world false)))

(check-expect (posn-stop-when true) false)
(check-expect (posn-stop-when false) true)
(check-expect (posn-stop-when (make-posn 10 10)) false)

;; last-picture : world -> image
; The last image the world will ever show
(define (last-picture world)
  BACKGROUND)

(define (stop-when-bang t)
  (big-bang (make-posn (/ WIDTH 2) (/ HEIGHT 2))
            (stop-when posn-stop-when last-picture)
            (on-draw show-picture)
            (on-key stop-when-handle-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Excersize 20.6.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; show-picture :  posn -> image
; Show the picture
(define (wrapping-show-picture where)
  (place-image DOT
               (modulo (posn-x where) WIDTH)
               (modulo (posn-y where) HEIGHT)
               BACKGROUND))

(check-expect (wrapping-show-picture (make-posn 15 12))
              (place-image DOT 15 12 BACKGROUND))
(check-expect (wrapping-show-picture (make-posn 27 149))
              (place-image DOT 27 149 BACKGROUND))
(check-expect (wrapping-show-picture (make-posn 27 301))
              (place-image DOT 27 1 BACKGROUND))
(check-expect (wrapping-show-picture (make-posn 400 301))
              (place-image DOT 100 1 BACKGROUND))

(define (wrapping-bang t)
  (big-bang (make-posn (/ WIDTH 2) (/ HEIGHT 2))
            (stop-when posn-stop-when last-picture)
            (on-draw wrapping-show-picture)
            (on-key stop-when-handle-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; negative-color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-color : color -> color
; Returns the negative color in the photographic sense. Makes a new color
; by subtracting this color from 255
(define (negative-color color)
  (make-color
   (- 255 (color-red color))
   (- 255 (color-green color))
   (- 255 (color-blue color))))

(check-expect (negative-color (make-color 4 50 2)) (make-color 251 205 253))
(check-expect (negative-color (make-color 9 50 2)) (make-color 246 205 253))
(check-expect (negative-color (make-color 255 255 255)) (make-color 0 0 0))
(check-expect (negative-color (negative-color (make-color 9 50 2)))
              (make-color 9 50 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; red? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; red? : color -> boolean
; Returns true iff the color has only red in it
(define (red? c)
  (and (= (color-blue c) 0)
       (= (color-green c) 0)))


(check-expect (red? (make-color 3 0 0)) true)
(check-expect (red? (make-color 3 0 1)) false)
(check-expect (red? (make-color 50 0 0)) true)
(check-expect (red? (make-color 50 1 0)) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rectange-animation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world is a color

; Height and width
(define height 100)(define width 100)

;; colored-rectangle : color -> image
; Returns a colored rectangle
(define (colored-rectangle c)
  (rectangle width height
             'solid c))

(check-expect (colored-rectangle 'blue) (rectangle 100 100 'solid 'blue))
(check-expect (colored-rectangle (make-color 109 40 23))
              (rectangle 100 100 'solid (make-color 109 40 23)))

;; mess-with-color : color -> color
; Mutates color.
(define (mess-with-color c)
  (make-color
   (modulo (+ (color-red c) 1) 255)
   (modulo (color-blue c) 255)
   (modulo (- (color-green c) 1) 255)))

(check-expect (mess-with-color (make-color 0 0 0)) (make-color 1 0 254))
(check-expect (mess-with-color (make-color 10 0 3)) (make-color 11 3 254))

(define (color-animation c)
  (big-bang 0
            (on-draw colored-rectangle)
            (on-tick mess-with-color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Excersize 20.7.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; green->white : color -> color
; If fully green, returns white. Otherwise, returns the color unchanged.
(define (green->white x y c)
  (if (color=? c (make-color 0 255 0))
      (make-color 255 255 255)
      c))
(check-expect (green->white 0 0 (make-color 0 255 0)) (make-color 255 255 255))
(check-expect (green->white 0 0 (make-color 1 255 0)) (make-color 1 255 0))


;; replace-green-white : image -> image
; Replaces all fully green pixels with fully white pixels
(define (replace-green-white img)
  (map-image
   green->white
   img))

(check-expect (replace-green-white (rectangle 10 10 'solid 'green))
              (rectangle 10 10 'solid 'white))
(check-expect (replace-green-white (rectangle 10 10 'solid 'blue))
              (rectangle 10 10 'solid 'blue))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; negative ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; map-negative : number number color -> color
; Returns the negative color (see negative-color)
(define (map-negative x y color)
  (negative-color color))

;; negative : image -> image
; Computes the negative of an entire color
(define (negative image)
  (map-image
   map-negative
   image))

; It is the case because (color=? c (negative-color (negative-color c))) is true
; and negative is simply that function applied over a lot of colors as an image.