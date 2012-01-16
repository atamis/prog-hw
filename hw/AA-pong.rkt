;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Pong
; Andrew Amis
; Started 12/12/11
; A simple implementation of pong

(require picturing-programs)



;                                                   
;                                                   
;                                                   
;   ;   ;           ;                               
;   ;   ;           ;                               
;   ;   ;           ;                               
;   ;   ;   ;;;     ;    ;;;;    ;;;   ; ;;    ;;;  
;   ;;;;;  ;   ;    ;    ;   ;  ;   ;  ;;  ;  ;   ; 
;   ;   ;  ;;;;;    ;    ;   ;  ;;;;;  ;   ;   ;;;  
;   ;   ;  ;        ;    ;   ;  ;      ;          ; 
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;      ;   ; 
;   ;   ;   ;;;     ;;   ;;;;    ;;;   ;       ;;;  
;                        ;                          
;                        ;                          
;                                                   

;; scale-posn : number posn -> posn
; Multiplies the x and the y by the provided number
(define (scale-posn n posn)
  (make-posn (* n (posn-x posn))
             (* n (posn-y posn))))

(check-expect (scale-posn 3 (make-posn 2 5)) (make-posn 6 15))
(check-expect (scale-posn 10 (make-posn 2 5)) (make-posn 20 50))
(check-expect (scale-posn -1 (make-posn 2 5)) (make-posn -2 -5))


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


;; new-sub-posns : posn posn -> posn
; Subtracts the x and the y values of the scond posn from the first posn.
(define (sub-posns p1 p2)
  (add-posns p1 (scale-posn -1 p2)))


(check-expect (sub-posns (make-posn 10 10) (make-posn 1 1))
              (make-posn 9 9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 12 14))
              (make-posn -9 -9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 0 0))
              (make-posn 3 5))
(check-expect (sub-posns (make-posn 0 0) (make-posn 0 0))
              (make-posn 0 0))
(check-expect (sub-posns (make-posn -1 -2) (make-posn 3 4))
              (make-posn -4 -6))

;                                            
;                                            
;                                            
;   ;   ;             ;           ;          
;   ;; ;;             ;           ;          
;   ; ; ;             ;           ;          
;   ;   ;   ;;;    ;;;;   ;;;     ;     ;;;  
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;   ; 
;   ;   ;  ;   ;  ;   ;  ;;;;;    ;     ;;;  
;   ;   ;  ;   ;  ;   ;  ;        ;        ; 
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;   ; 
;   ;   ;   ;;;    ;;;;   ;;;     ;;    ;;;  
;                                            
;                                            
;                                            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Static Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Player 1 is on the right, player 2 is on the left.

;; Represents board dimensions (pixels)
(define BOARD-HEIGHT 500)
(define BOARD-WIDTH 1000)

;; Represents the flat background upon which the game will be drawn.
(define BACKGROUND (rectangle BOARD-WIDTH BOARD-HEIGHT 'solid 'black))

;; Foreground color (usually white)
(define FOREGROUND-COLOR 'green)

;; The ball we play the game with
(define BALL (circle 1 'solid FOREGROUND-COLOR))

;; Coefficient of friction used to slow down paddles
(define FRICTION 0.98)

;; Amount each keypress increases the paddle velocity
(define PADDLE-DELTA 2)

;; player1's paddle's x location relative to the width of the playing field
(define P1_PADDLE_LOC 15/16)

;; player2's paddle's x location
(define P2_PADDLE_LOC 1/16)

;; paddle width and height
(define PADDLE_WIDTH 10)
(define PADDLE_HEIGHT 100)

;; Use actual deflection for the paddles, or not
(define PADDLE_DEFLECTION? true)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dynamic Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#|
;; velocity : number number
(define-struct velocity (x y))
; where x and y represent horizontal and vertical velocity, respectively.
; (Positive numbers represent movement rightwards & downwards;
;  negatives represent leftwards & upwards.)
; Examples:
;    (make-posn -50 20) -- moving leftwards & down
; (make-velocity 10 -15) -- moving rightwards & up

;; Template:
;; fun-for-velocity : velocity -> ???
#;(define (fun-for-velocity v)
    ... (velocity-x v)
    ... (velocity-y v) ...)
#;(define (fun-for-velocity v)
    (make-velocity
     (velocity-x v)
     (velocity-y v)))
|#

;; A velocity is a posn, where x represents the horizontal and vertical velocity



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; paddle : number velocity
(define-struct paddle (y vec))
; paddle-x : the y location of the paddle. See P[1, 2]_PADDLE_LOC for the x
;            location
; paddle-vec : number representing the vertical velocity of the paddle.

#;(make-paddle 10 3)

; Templates
;; fun-for-paddle : paddle -> ???
#;(define (fun-for-paddle paddle)
    ... (paddle-y paddle)
    ... (paddle-vec paddle))

;; fun-for-paddle : paddle -> paddle
#;(define (fun-for-paddle paddle)
    (make-paddle (paddle-y paddle)
                 (paddle-vec paddle)))

;; paddle-posn : paddle number number -> posn
; Returns the posn location of the paddle. The 1st number, either 1 or 2,
; represents which player this paddle belongs to. The 2nd number is the width
; of the play field.
(define (paddle-posn paddle player width)
  (make-posn
   (* width (cond
              [(= player 1) P1_PADDLE_LOC]
              [(= player 2) P2_PADDLE_LOC]
              [else (error "player isn't 1 or 2")]))
   (paddle-y paddle)))

(check-expect (paddle-posn (make-paddle 10 3) 2 1000)
              (make-posn 62.5 10))
(check-expect (paddle-posn (make-paddle 10 3) 1 1000)
              (make-posn 937.5 10))
(check-expect (paddle-posn (make-paddle 50 3) 1 1000)
              (make-posn 937.5 50))
(check-error (paddle-posn (make-paddle 50 3) 0 1000)
             "player isn't 1 or 2")



;; move-paddle-up : paddle -> paddle
; Moves a paddle up. Technically speaking, it increments or decrements the
; internal velocity of the paddle.
(define (move-paddle-up paddle)
  (make-paddle (paddle-y paddle)
               (+ (paddle-vec paddle) PADDLE-DELTA)))


(check-expect (move-paddle-up (make-paddle 50 0))
              (make-paddle 50 (+ 0 PADDLE-DELTA)))
(check-expect (move-paddle-up (make-paddle 50 1))
              (make-paddle 50 (+ 1 PADDLE-DELTA)))
(check-expect (move-paddle-up (make-paddle 50 -1))
              (make-paddle 50 (+ -1 PADDLE-DELTA)))

;; move-paddle-down : paddle -> paddle
; Moves the paddle down.
(define (move-paddle-down paddle)
  (make-paddle (paddle-y paddle)
               (- (paddle-vec paddle) PADDLE-DELTA)))

(check-expect (move-paddle-down (make-paddle 50 0))
              (make-paddle 50 (- 0 PADDLE-DELTA)))
(check-expect (move-paddle-down (make-paddle 50 1))
              (make-paddle 50 (- 1 PADDLE-DELTA)))
(check-expect (move-paddle-down (make-paddle 50 -4))
              (make-paddle 50 (- -4 PADDLE-DELTA)))



;; apply-paddle-vector : paddle -> paddle
; Moves the paddle and applies friction as appropriate.
(define (apply-paddle-vector paddle)
  (make-paddle (+ (paddle-y paddle) (paddle-vec paddle))
               (* FRICTION (paddle-vec paddle))))

(check-expect (apply-paddle-vector (make-paddle 100 3))
              (make-paddle 103 (* FRICTION 3)))
(check-expect (apply-paddle-vector (make-paddle 100 -3))
              (make-paddle 97 (* FRICTION -3)))
(check-expect (apply-paddle-vector (make-paddle 50 50))
              (make-paddle 100 (* FRICTION 50)))

;; paddle-deflection : paddle -> paddle
; Deflects the paddle from the top and bottom edges of the screen.
(define (paddle-deflection paddle)
  (make-paddle
   (paddle-y paddle)
   (* (paddle-vec paddle)
      (cond
        [(and (> 0 (- (* 1/2 PADDLE_HEIGHT) (paddle-y paddle))) 
              (< (+ (* 1/2 PADDLE_HEIGHT) (paddle-y paddle)) BOARD-HEIGHT))
         ; If the edges of the paddle are between the top and
         ; bottom of the screen, regardless of vector, do
         ; nothing.
         1]
        [(or (and (< 0 (- (* 1/2 PADDLE_HEIGHT) (paddle-y paddle)))
                  (positive? (paddle-vec paddle)))
             (and (> (+ (* 1/2 PADDLE_HEIGHT) (paddle-y paddle)) BOARD-HEIGHT)
                  (negative? (paddle-vec paddle))))
         ; If we are are out of bounds, but returning to bounds,
         ; do nothing.
         1]
        [(or (and (< 0 (- (* 1/2 PADDLE_HEIGHT) (paddle-y paddle)))
                  (negative? (paddle-vec paddle)))
             (and (> (+ (* 1/2 PADDLE_HEIGHT) (paddle-y paddle)) BOARD-HEIGHT)
                  (positive? (paddle-vec paddle))))
         ; If we are are out of bounds, but not returning to
         ; bounds invert the vector.
         -1]
        [else 1]))))

(check-expect (paddle-deflection (make-paddle 100 5))
              (make-paddle 100 5))
(check-expect (paddle-deflection (make-paddle 100 -5))
              (make-paddle 100 -5))
(check-expect (paddle-deflection (make-paddle -60 5))
              (make-paddle -60 5))
(check-expect (paddle-deflection (make-paddle -60 -5))
              (make-paddle -60 5))
(check-expect (paddle-deflection (make-paddle 1060 -5))
              (make-paddle 1060 -5))
(check-expect (paddle-deflection (make-paddle 1100 5))
              (make-paddle 1100 -5))


;; stupid-paddle-deflection : paddle -> paddle
; Like paddle-deflection, but instead of deflecting, it simply doesn't allow
; paddles out of bounds.
(define (stupid-paddle-deflection p)
  (make-paddle (min (max (paddle-y p) 50) (- BOARD-HEIGHT 50))
               (paddle-vec p)))

(check-expect (stupid-paddle-deflection (make-paddle 100 5))
              (make-paddle 100 5))
(check-expect (stupid-paddle-deflection (make-paddle 100 -5))
              (make-paddle 100 -5))
(check-expect (stupid-paddle-deflection (make-paddle 50 5))
              (make-paddle 50 5))
(check-expect (stupid-paddle-deflection (make-paddle -60 -5))
              (make-paddle 50 -5))
(check-expect (stupid-paddle-deflection (make-paddle 950 -5))
              (make-paddle 450 -5))
(check-expect (stupid-paddle-deflection (make-paddle 1100 -5))
              (make-paddle 450 -5))


;; inside-paddle? : paddle number posn -> boolean
; Checks whether the given posn is inside the given paddle.
(define (inside-paddle? p player loc)
  (and (<= (- (posn-x (paddle-posn p player BOARD-WIDTH)) 5)
           (posn-x loc)
           (+ (posn-x (paddle-posn p player BOARD-WIDTH)) 5))
       (<= (- (posn-y (paddle-posn p player BOARD-WIDTH)) 50)
           (posn-y loc)
           (+ (posn-y (paddle-posn p player BOARD-WIDTH)) 50))))

(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 62.5 200)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 62.5 150)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 62.5 250)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 62.5 250)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 57.5 150 )) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 67.5 150)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 57.5 250)) true)
(check-expect (inside-paddle? (make-paddle 200 0) 2
                              (make-posn 67.5 250)) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ball : posn velocity
(define-struct ball (loc vec))
; ball-loc : posn representing the location of the posn
; ball-vec : velocity representing the velocity of the posn

#;(make-ball (make-posn 10 10)
             (make-posn 1 1))

; Templates
;; fun-for-ball : ball -> ???
#;(define (fun-for-ball ball)
    ... (ball-loc ball) ...
    ... (ball-vec ball) ...)

;; fun-for-ball : ball -> ball
#;(define (fun-for-ball ball)
    (make-ball (ball-loc ball)
               (ball-vec ball)))

;; Default ball. The ball we start play with
(define DEFAULT-BALL (make-ball (make-posn
                                 (/ BOARD-WIDTH 2)
                                 (/ BOARD-HEIGHT 2))
                                (make-posn 1 1)))


(define (ball-x ball) (posn-x (ball-loc ball)))
(define (ball-y ball) (posn-y (ball-loc ball)))

;; apply-ball-vector : ball -> ball
; Moves the given ball around on its vector.
(define (apply-ball-vector ball)
  (make-ball (add-posns (ball-loc ball) (scale-posn 3 (ball-vec ball)))
             (ball-vec ball)))

(check-expect (apply-ball-vector
               (make-ball (make-posn 10 10)
                          (make-posn 1 1)))
              (make-ball (make-posn 13 13)
                         (make-posn 1 1)))
(check-expect (apply-ball-vector
               (make-ball (make-posn 10 10)
                          (make-posn -1 1)))
              (make-ball (make-posn 7 13)
                         (make-posn -1 1)))
(check-expect (apply-ball-vector
               (make-ball (make-posn 10 10)
                          (make-posn -1 -1)))
              (make-ball (make-posn 7 7)
                         (make-posn -1 -1)))


;; ball-deflection : ball paddle paddle -> ball
; Deflects the ball based on the ball location and vector as well as paddle
; location
(define (ball-deflection ball p1paddle p2paddle)
  (make-ball (ball-loc ball)
             (cond
               [(<= (ball-y ball) 0)
                (cond ; Top wall
                  [(equal? (ball-vec ball) (make-posn 1 -1)) (make-posn 1 1)]
                  [(equal? (ball-vec ball) (make-posn -1 -1)) (make-posn -1 1)]
                  [else (ball-vec ball)])]
               [(>= (ball-y ball) BOARD-HEIGHT)
                (cond ; Bottom wall
                  [(equal? (ball-vec ball) (make-posn 1 1)) (make-posn 1 -1)]
                  [(equal? (ball-vec ball) (make-posn -1 1)) (make-posn -1 -1)]
                  [else (ball-vec ball)])]
               [(inside-paddle? p1paddle 1 (ball-loc ball))
                (cond ; First player's paddle (right side paddle)
                  [(equal? (ball-vec ball) (make-posn 1 -1)) (make-posn -1 -1)]
                  [(equal? (ball-vec ball) (make-posn 1 1)) (make-posn -1 1)]
                  [else (ball-vec ball)])]
               [(inside-paddle? p2paddle 2 (ball-loc ball))
                (cond ; Second player's paddle
                  [(equal? (ball-vec ball) (make-posn -1 -1)) (make-posn 1 -1)]
                  [(equal? (ball-vec ball) (make-posn -1 1)) (make-posn 1 1)]
                  [else (ball-vec ball)])]
               [else (ball-vec ball)])))

; Nothing should happen:
(check-expect (ball-deflection
               (make-ball (make-posn 10 10) (make-posn -1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 10 10) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 10 10) (make-posn 1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 10 10) (make-posn 1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 10 10) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 10 10) (make-posn -1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 10 10) (make-posn 1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 10 10) (make-posn 1 -1)))

;; It should deflect off the top wall
(check-expect (ball-deflection
               (make-ball (make-posn 5 0) (make-posn 1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 5 0) (make-posn 1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 5 0) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 5 0) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 5 0) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 5 0) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 5 -1) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 5 -1) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 5 -1) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 5 -1) (make-posn -1 1)))


; It should reflect off the second player's paddle
(check-expect (ball-deflection
               (make-ball (make-posn 62.5 10) (make-posn -1 -1))
               (make-paddle 50 3) (make-paddle 50 3))
              (make-ball (make-posn 62.5 10) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 62.5 10) (make-posn -1 1))
               (make-paddle 50 3) (make-paddle 50 3))
              (make-ball (make-posn 62.5 10) (make-posn 1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 67.5 100) (make-posn -1 1))
               (make-paddle 50 3) (make-paddle 50 3))
              (make-ball (make-posn 67.5 100) (make-posn 1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 67.5 100) (make-posn 1 1))
               (make-paddle 50 3) (make-paddle 50 3))
              (make-ball (make-posn 67.5 100) (make-posn 1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 62.5 10) (make-posn -1 1))
               (make-paddle 50 3) (make-paddle 50 3))
              (make-ball (make-posn 62.5 10) (make-posn 1 1)))


; It should reflect off the first player's paddle
(check-expect (ball-deflection
               (make-ball (make-posn 932.5 10) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 932.5 10) (make-posn -1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 932.5 10) (make-posn -1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 932.5 10) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 932.5 250) (make-posn -1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 932.5 250) (make-posn -1 1)))
(check-expect (ball-deflection
               (make-ball (make-posn 932.5 100) (make-posn 1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 932.5 100) (make-posn -1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 932.5 100) (make-posn 1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 932.5 100) (make-posn -1 1)))

; It should bounce off the bottom wall
(check-expect (ball-deflection
               (make-ball (make-posn 500 501) (make-posn 1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 500 501) (make-posn 1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 500 501) (make-posn 1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 500 501) (make-posn 1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 500 501) (make-posn -1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 500 501) (make-posn -1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 500 501) (make-posn -1 1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 500 501) (make-posn -1 -1)))
(check-expect (ball-deflection
               (make-ball (make-posn 500 501) (make-posn -1 -1))
               (make-paddle 100 3) (make-paddle 100 3))
              (make-ball (make-posn 500 501) (make-posn -1 -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; pstate : ball paddle paddle number number
(define-struct pstate (ball p1p p2p p1s p2s))
; pstate-ball : ball representing the ball in this game
; pstate-p1p : paddle, representing the paddle location and velocity of the 1st
;             players paddle.
; pstate-p2p : paddle, representing the paddle location and velocity of the 2nd
;             players paddle.
; pstate-p1s : non-negative number representing the 1st players score
; pstate-p2s : non-negative number representing the 2nd players score

#;(make-pstate (make-ball (make-posn 10 10)
                          (make-posn 1 1))
               (make-paddle 10 3)
               (make-paddle 10 3)
               0 0)

; Templates
;; fun-for-pstate : pstate -> ???
#;(define (fun-for-pstate pstate)
    ... (pstate-ball pstate) ...
    ... (pstate-p1p pstate) ...
    ... (pstate-p2p pstate) ...
    ... (pstate-p1s pstate) ...
    ... (pstate-p2s pstate))

;; fun-for-pstate : pstate -> pstate
#;(define (fun-for-pstate pstate)
    (make-pstate 
     (pstate-ball pstate)
     (pstate-p1p pstate)
     (pstate-p2p pstate)
     (pstate-p1s pstate)
     (pstate-p2s pstate)))





;                                                                               
;                                                                               
;                                                                               
;    ;;;                                        ;      ;                        
;   ;   ;                 ;                     ;      ;                        
;   ;   ;                 ;                     ;      ;                        
;   ;       ;;;   ;;;;   ;;;    ; ;;    ;;;     ;      ;     ;;;   ; ;;    ;;;  
;   ;      ;   ;  ;   ;   ;     ;;  ;  ;   ;    ;      ;    ;   ;  ;;  ;  ;   ; 
;   ;      ;   ;  ;   ;   ;     ;   ;  ;   ;    ;      ;    ;;;;;  ;   ;   ;;;  
;   ;      ;   ;  ;   ;   ;     ;      ;   ;    ;      ;    ;      ;          ; 
;   ;   ;  ;   ;  ;   ;   ;     ;      ;   ;    ;      ;    ;   ;  ;      ;   ; 
;    ;;;    ;;;   ;   ;    ;;   ;       ;;;     ;;     ;;    ;;;   ;       ;;;  
;                                                                               
;                                                                               
;                                                                               

;; handle-scoring : pstate -> pstate
; Checks whether the ball is in a scoring zone and increments the score as
; required
(define (handle-scoring pstate)
  (cond
    [(< (ball-x (pstate-ball pstate)) 0)
     ; Second player's goal
     (make-pstate 
      DEFAULT-BALL
      (pstate-p1p pstate)
      (pstate-p2p pstate)
      (add1 (pstate-p1s pstate))
      (pstate-p2s pstate))]
    [(< BOARD-WIDTH (ball-x (pstate-ball pstate)))
     ; First player's goal
     (make-pstate 
      DEFAULT-BALL
      (pstate-p1p pstate)
      (pstate-p2p pstate)
      (pstate-p1s pstate)
      (add1 (pstate-p2s pstate)))]
    [else
     pstate]))

;; Not in scoring zones.
(check-expect (handle-scoring
               (make-pstate (make-ball (make-posn 100 100) (make-posn -1 1))
                            (make-paddle 100 3.0)
                            (make-paddle 100 3.0)
                            0 0))
              (make-pstate (make-ball (make-posn 100 100) (make-posn -1 1))
                           (make-paddle 100 3.0)
                           (make-paddle 100 3.0)
                           0 0))

; In the second player's goal
(check-expect (handle-scoring
               (make-pstate (make-ball (make-posn -1 100) (make-posn -1 1))
                            (make-paddle 100 3.0)
                            (make-paddle 100 3.0)
                            0 0))
              (make-pstate DEFAULT-BALL
                           (make-paddle 100 3.0)
                           (make-paddle 100 3.0)
                           1 0))
; In the first player's goal
(check-expect (handle-scoring
               (make-pstate (make-ball (make-posn 1002 100) (make-posn -1 1))
                            (make-paddle 100 3.0)
                            (make-paddle 100 3.0)
                            0 0))
              (make-pstate DEFAULT-BALL
                           (make-paddle 100 3.0)
                           (make-paddle 100 3.0)
                           0 1))


;; handle-paddle : paddle -> paddle
; Handle the paddle. This obeys PADDLE_DEFLECTION?
(define (handle-paddle p)
  (if PADDLE_DEFLECTION?
      (paddle-deflection p)
      (stupid-paddle-deflection p)))


;; handle-keyboard : pstate key
; Handle keyboard input. Arrow keys are used for player 1, A and Z are used for
; player 2 to move paddles up and down.
(define (handle-keyboard pstate key)
  (make-pstate 
   (pstate-ball pstate)
   (cond
     ; Note that up and down are switched because move-paddle-up increments
     ; the vector, which actually moves the paddle down on the screen.
     [(key=? "down" key) (move-paddle-up (pstate-p1p pstate))]
     [(key=? "up" key) (move-paddle-down (pstate-p1p pstate))]
     [else (pstate-p1p pstate)])
   (cond
     [(key=? "z" key) (move-paddle-up (pstate-p2p pstate))]
     [(key=? "a" key) (move-paddle-down (pstate-p2p pstate))]
     [else (pstate-p2p pstate)])
   (pstate-p1s pstate)
   (pstate-p2s pstate)))

(check-expect (handle-keyboard
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 100 3)
                            (make-paddle 100 3)
                            0 0) "up")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-posn 1 1))
                           (make-paddle 100 (- 3 PADDLE-DELTA))
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 100 3)
                            (make-paddle 100 3)
                            0 0) "down")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-posn 1 1))
                           (make-paddle 100 (+ 3 PADDLE-DELTA))
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 100 3)
                            (make-paddle 100 3)
                            0 0) "a")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-posn 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 (- 3 PADDLE-DELTA))
                           0 0))
(check-expect (handle-keyboard
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 100 3)
                            (make-paddle 100 3)
                            0 0) "z")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-posn 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 (+ 3 PADDLE-DELTA))
                           0 0))
(check-expect (handle-keyboard
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 100 3)
                            (make-paddle 100 3)
                            0 0) "f")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-posn 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 3)
                           0 0))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; handle-tick : pstate
; Handles a single tick.
(define (handle-tick pstate)
  (handle-scoring (make-pstate 
                   (ball-deflection (apply-ball-vector (pstate-ball pstate))
                                    (pstate-p1p pstate)
                                    (pstate-p2p pstate))
                   (handle-paddle (apply-paddle-vector (pstate-p1p pstate)))
                   (handle-paddle (apply-paddle-vector (pstate-p2p pstate)))
                   (pstate-p1s pstate)
                   (pstate-p2s pstate))))

(check-expect (handle-tick (make-pstate (make-ball (make-posn 100 100)
                                                   (make-posn 1 1))
                                        (make-paddle 100 3)
                                        (make-paddle 100 3)
                                        0 0))
              (make-pstate (make-ball (make-posn 103 103)
                                      (make-posn 1 1))
                           (apply-paddle-vector (make-paddle 100 3))
                           (apply-paddle-vector (make-paddle 100 3))
                           0 0))

(check-expect (handle-tick (make-pstate (make-ball (make-posn 100 100)
                                                   (make-posn 1 -1))
                                        (make-paddle 100 10)
                                        (make-paddle 100 4)
                                        0 0))
              (make-pstate (make-ball (make-posn 103 97)
                                      (make-posn 1 -1))
                           (apply-paddle-vector (make-paddle 100 10))
                           (apply-paddle-vector (make-paddle 100 4))
                           0 0))

;                                     
;                                     
;                                     
;  ;     ;   ;                        
;  ;     ;   ;                        
;   ;   ;                             
;   ;   ;    ;     ;;;   ; ; ;   ;;;  
;   ;   ;    ;    ;   ;  ; ; ;  ;   ; 
;    ; ;     ;    ;;;;;  ; ; ;   ;;;  
;    ; ;     ;    ;      ; ; ;      ; 
;     ;      ;    ;   ;  ; ; ;  ;   ; 
;     ;      ;     ;;;    ; ;    ;;;  
;                                     
;                                     
;                                     

;; draw-scores : number number image -> image
; Draws the player's scores on the image. The 1st number being the 1st players
; score, the 2nd being the 2nd.
(define (draw-scores p1 p2 img)
  (place-image
   (text (number->string p2) 30 FOREGROUND-COLOR)
   (* 4/10 (image-width img)) (/ (image-height img) 10)
   (place-image
    (text (number->string p1) 30 FOREGROUND-COLOR)
    (* 6/10 (image-width img)) (/ (image-height img) 10)
    img)))

(check-expect (draw-scores 1 1 BACKGROUND)
              (place-image
               (text "1" 30 FOREGROUND-COLOR)
               400 50
               (place-image
                (text "1" 30 FOREGROUND-COLOR)
                600 50
                BACKGROUND)))
(check-expect (draw-scores 1 5 BACKGROUND)
              (place-image
               (text "5" 30 FOREGROUND-COLOR)
               400 50
               (place-image
                (text "1" 30 FOREGROUND-COLOR)
                600 50
                BACKGROUND)))
(check-expect (draw-scores 1 5 (rectangle 100 100 'solid 'black))
              (place-image
               (text "5" 30 FOREGROUND-COLOR)
               40 10
               (place-image
                (text "1" 30 FOREGROUND-COLOR)
                60 10
                (rectangle 100 100 'solid 'black))))





;; draw-ball : ball image -> image
; Draws the ball onto the image
(define (draw-ball ball img)
  (place-image BALL
               (posn-x (ball-loc ball))
               (posn-y (ball-loc ball))
               img))

(check-expect (draw-ball (make-ball (make-posn 10 10)
                                    (make-posn 1 1))
                         BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 10)
                                    (make-posn 4 90))
                         BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 12)
                                    (make-posn 4 90))
                         BACKGROUND)
              (place-image BALL 10 12 BACKGROUND))


;; draw-paddle : paddle number image -> image
; Draws a single paddle on the screen
(define (draw-paddle paddle player-number img)
  (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                          'solid FOREGROUND-COLOR)
               (posn-x (paddle-posn paddle player-number (image-width img)))
               (posn-y (paddle-posn paddle player-number (image-width img)))
               img))

(check-expect (draw-paddle (make-paddle 10 3) 2 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid FOREGROUND-COLOR)
                           62.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 2 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid FOREGROUND-COLOR)
                           62.5 100 BACKGROUND))
(check-expect (draw-paddle (make-paddle 10 3) 1 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid FOREGROUND-COLOR)
                           937.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 1 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid FOREGROUND-COLOR)
                           937.5 100 BACKGROUND))

;; draw-paddles : paddle paddle image -> image
; Draws the paddles on the image
(define (draw-paddles p1 p2 img)
  (draw-paddle p1 1
               (draw-paddle p2 2 img)))

(check-expect (draw-paddles (make-paddle 10 3)
                            (make-paddle 10 3)
                            BACKGROUND)
              (draw-paddle (make-paddle 10 3) 1
                           (draw-paddle (make-paddle 10 3) 2
                                        BACKGROUND)))
(check-expect (draw-paddles (make-paddle 100 3)
                            (make-paddle 10 3)
                            BACKGROUND)
              (draw-paddle (make-paddle 100 3) 1
                           (draw-paddle (make-paddle 10 3) 2
                                        BACKGROUND)))

;; pview : pstate -> image
; Convert a pong state to an image to display on screen
(define (pview pstate)
  (draw-paddles (pstate-p1p pstate)
                (pstate-p2p pstate)
                (draw-scores (pstate-p1s pstate) (pstate-p2s pstate)
                             (draw-ball (pstate-ball pstate) BACKGROUND))))


(check-expect (pview
               (make-pstate (make-ball (make-posn 100 100)
                                       (make-posn 1 1))
                            (make-paddle 10 3)
                            (make-paddle 10 3)
                            0 0))
              (draw-paddles
               (make-paddle 10 3)
               (make-paddle 10 3)
               (draw-scores 0 0
                            (draw-ball (make-ball
                                        (make-posn 100 100)
                                        (make-posn 1 1)) BACKGROUND))))



;                              
;                              
;                              
;   ;   ;                      
;   ;; ;;                      
;   ; ; ;                      
;   ;   ;   ;;;   ;;;;   ;   ; 
;   ;   ;  ;   ;  ;   ;  ;   ; 
;   ;   ;  ;;;;;  ;   ;  ;   ; 
;   ;   ;  ;      ;   ;  ;   ; 
;   ;   ;  ;   ;  ;   ;  ;  ;; 
;   ;   ;   ;;;   ;   ;   ;; ; 
;                              
;                              
;                              

; world is a boolean representing whether the person wants to play pong.

;; draw-menu : world -> image
; Draws the menu
(define (draw-menu world)
  (place-image
   (text "Arrow keys to move right paddle" 14 FOREGROUND-COLOR)
   (/ BOARD-WIDTH 2)
   (+ (* 3/4 BOARD-HEIGHT) 14)
   (place-image
    (text "A and Z to move left paddle" 14 FOREGROUND-COLOR)
    (/ BOARD-WIDTH 2)
    (* 3/4 BOARD-HEIGHT)
    (place-image
     (text "PONG" 60 FOREGROUND-COLOR)
     (/ BOARD-WIDTH 2)
     (/ BOARD-HEIGHT 4)
     (place-image (text "Press any key to play. Press escape to exit"
                        18 FOREGROUND-COLOR)
                  (/ BOARD-WIDTH 2)
                  (/ BOARD-HEIGHT 2)
                  BACKGROUND)))))

;; menu-keyboard-handler : world key -> world
; On escape, set to false. On any other key, return true.
(define (menu-keyboard-handler world key)
  (not (key=? key "escape")))

(check-expect (menu-keyboard-handler empty "a") true)
(check-expect (menu-keyboard-handler empty "escape") false)


;; play-pong? : anything -> boolean
; Displays a screen asking whether the person would like to play pong
(define (play-pong? x)
  (big-bang empty
            (stop-when boolean?)
            (to-draw draw-menu)
            (on-key menu-keyboard-handler)))




;                              
;                              
;                              
;   ;   ;           ;          
;   ;; ;;           ;          
;   ; ; ;                      
;   ;   ;   ;;;;    ;    ;;;;  
;   ;   ;  ;   ;    ;    ;   ; 
;   ;   ;  ;   ;    ;    ;   ; 
;   ;   ;  ;   ;    ;    ;   ; 
;   ;   ;  ;  ;;    ;    ;   ; 
;   ;   ;   ;; ;    ;    ;   ; 
;                              
;                              
;                              




(define (main x)
  (if (play-pong? empty)
      (big-bang (make-pstate
                 DEFAULT-BALL
                 (make-paddle 100 3.0)
                 (make-paddle 100 3.0)
                 0 0)
                (to-draw pview)
                (on-key handle-keyboard)
                (on-tick handle-tick)
                #;(state true))
      false))
