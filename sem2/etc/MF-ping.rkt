;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ping) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
;; Pong
; Andrew Amis
; Started 12/12/11
; A simple implementation of pong

(require picturing-programs)

#|
  MF: 
    arranged in top-down fashion 
    replaced esc functionality 
    eliminated redundancies in code and tests
    introduced a few auxiliary functions to eliminate comments (same for some tests)
  not done: 
    you need some inexactness in your numbers to enhance performance 
    your code contains too many constants that should be globalized 
|#

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

;; boolean -> pstate
;; play pong, allow player to quit with ESC at any time 
(define (main debug?)
  (big-bang world0
            (to-draw pview)
            (on-key handle-keyboard)
            (on-tick handle-tick)
            (state debug?)))

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

;; The ball we play the game with
(define BALL (circle 5 'solid 'white))

;; The default ball that the game starts with.

;; Coefficient of friction used to slow down paddles
(define FRICTION 0.98)

;; Amount each keypress increases the paddle velocity
(define PADDLE-DELTA 2)

;; player1's paddle's x location relative to the width of the playing field
(define P1-PADDLE-LOC 15/16)

;; player2's paddle's x location
(define P2-PADDLE-LOC 1/16)

;; paddle width and height
(define PADDLE-WIDTH 10)
(define PADDLE-HEIGHT 100)

;; Use actual deflection for the paddles, or not
(define PADDLE-DEFLECTION? true)

(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT 'solid 'white))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dynamic Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; paddle : number velocity
(define-struct paddle (y vec))
; paddle-x : the y location of the paddle. See P[1, 2]-PADDLE-LOC for the x
;            location
; paddle-vec : number representing the vertical velocity of the paddle.

(define sample-paddle (make-paddle 10 3))

;; ball : posn velocity
(define-struct ball (loc vec))
; ball-loc : posn representing the location of the posn
; ball-vec : velocity representing the velocity of the posn

(define sample-ball (make-ball (make-posn 10 10) (make-posn 1 1)))

;; pstate : ball paddle paddle number number
(define-struct pstate (ball p1p p2p p1s p2s))
; pstate-ball : ball representing the ball in this game
; pstate-p1p : paddle, representing the paddle location and velocity of the 1st
;             players paddle.
; pstate-p2p : paddle, representing the paddle location and velocity of the 2nd
;             players paddle.
; pstate-p1s : non-negative number representing the 1st players score
; pstate-p2s : non-negative number representing the 2nd players score

(define sample-pstate
  (make-pstate (make-ball (make-posn 10 10) (make-posn 1 1))
               (make-paddle 10 3)
               (make-paddle 10 3)
               0 0))

;; the following world is used as initial world 
(define world0
  (make-pstate (make-ball (make-posn 100 100) (make-posn -1 1))
               (make-paddle 100 3.0)
               (make-paddle 100 3.0)
               0 0))

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

;; handle-keyboard : pstate key
; Handle keyboard input. Arrow keys are used for player 1, A and Z are used for
; player 2 to move paddles up and down.
(define (handle-keyboard pstate key)
  (if (key=? key "escape")
      (stop-with pstate)
      (make-pstate 
       (pstate-ball pstate)
       (cond
         ;; MF: you should rename the functions 
         ; note: that up and down are switched because move-paddle-up increments
         ; the vector, which actually moves the paddle down on the screen.
         [(key=? "down" key) (paddle-up (pstate-p1p pstate))]
         [(key=? "up" key) (paddle-down (pstate-p1p pstate))]
         [else (pstate-p1p pstate)])
       (cond
         [(key=? "z" key) (paddle-up (pstate-p2p pstate))]
         [(key=? "a" key) (paddle-down (pstate-p2p pstate))]
         [else (pstate-p2p pstate)])
       (pstate-p1s pstate)
       (pstate-p2s pstate))))

(define pstate-handle0 
  (make-pstate (make-ball (make-posn 100 100)
                          (make-posn 1 1))
               (make-paddle 100 3)
               (make-paddle 100 3)
               0 0))

(check-expect (handle-keyboard pstate-handle0 "up")
              (make-pstate (make-ball (make-posn 100 100) (make-posn 1 1))
                           (make-paddle 100 (- 3 PADDLE-DELTA))
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard pstate-handle0 "down")
              (make-pstate (make-ball (make-posn 100 100) (make-posn 1 1))
                           (make-paddle 100 (+ 3 PADDLE-DELTA))
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard pstate-handle0 "a")
              (make-pstate (make-ball (make-posn 100 100) (make-posn 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 (- 3 PADDLE-DELTA))
                           0 0))
(check-expect (handle-keyboard pstate-handle0 "z")
              (make-pstate (make-ball (make-posn 100 100) (make-posn 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 (+ 3 PADDLE-DELTA))
                           0 0))
(check-expect (handle-keyboard pstate-handle0 "f") pstate-handle0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-tick : pstate
; Handles a single tick.
(define (handle-tick pstate)
  (handle-scoring 
   (make-pstate 
    (ball-deflection
     (ball-move (pstate-ball pstate)) (pstate-p1p pstate) (pstate-p2p pstate))
    (handle-paddle (paddle-move (pstate-p1p pstate)))
    (handle-paddle (paddle-move (pstate-p2p pstate)))
    (pstate-p1s pstate)
    (pstate-p2s pstate))))

(check-expect (handle-tick pstate-handle0)
              (make-pstate (make-ball (make-posn 103 103) (make-posn 1 1))
                           (paddle-move (make-paddle 100 3))
                           (paddle-move (make-paddle 100 3))
                           0 0))

(check-expect
 (handle-tick (make-pstate (make-ball (make-posn 100 100) (make-posn 1 -1))
                           (make-paddle 100 10)
                           (make-paddle 100 4)
                           0 0))
 (make-pstate (make-ball (make-posn 103 97) (make-posn 1 -1))
              (paddle-move (make-paddle 100 10))
              (paddle-move (make-paddle 100 4))
              0 0))

;; handle-scoring : pstate -> pstate
; Checks whether the ball is in a scoring zone and increments the score as
; required
(define (handle-scoring pstate)
  (cond
    [(in-second-players-goal pstate)
     (reset-p1s pstate (add1 (pstate-p1s pstate)) (pstate-p2s pstate))]
    [(in-first-players-goal pstate)
     (reset-p1s pstate (pstate-p1s pstate) (add1 (pstate-p2s pstate)))]
    [else pstate]))

;; pstate -> boolean 
(define (in-second-players-goal pstate)
  (< (ball-x (pstate-ball pstate)) 0))

;; pstate -> boolean 
(define (in-first-players-goal pstate)
  (< BOARD-WIDTH (ball-x (pstate-ball pstate))))

;; pstate nat nat -> pstate 
;; reset the ball to default and update score 
(define (reset-p1s pstate p1s p2s)
  (make-pstate DEFAULT-BALL (pstate-p1p pstate) (pstate-p2p pstate) p1s p2s))

(define pstate-no-goal
  (make-pstate (make-ball (make-posn 100 100) (make-posn -1 1))
               (make-paddle 100 3.0)
               (make-paddle 100 3.0)
               0 0))

(define pstate-in-goal1
  (make-pstate (make-ball (make-posn 1002 100) (make-posn -1 1))
               (make-paddle 100 3.0)
               (make-paddle 100 3.0)
               0 0))

(define pstate-in-goal2
  (make-pstate (make-ball (make-posn -1 100) (make-posn -1 1))
               (make-paddle 100 3.0)
               (make-paddle 100 3.0)
               0 0))

(define (pstate-default s1 s2)
  (make-pstate DEFAULT-BALL (make-paddle 100 3.0) (make-paddle 100 3.0) s1 s2))

(check-expect (handle-scoring pstate-no-goal) pstate-no-goal)
(check-expect (handle-scoring pstate-in-goal2) (pstate-default 1 0))
(check-expect (handle-scoring pstate-in-goal1) (pstate-default 0 1))

;; handle-paddle : paddle -> paddle
; Handle the paddle. This obeys PADDLE-DEFLECTION?
(define (handle-paddle p)
  (if PADDLE-DEFLECTION?
      (paddle-deflection p)
      (paddle-keep-inside p)))

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

;; pview : pstate -> image
; Convert a pong state to an image to display on screen
(define (pview pstate)
  (draw-paddles (pstate-p1p pstate)
                (pstate-p2p pstate)
                (draw-scores (pstate-p1s pstate) (pstate-p2s pstate)
                             (draw-ball (pstate-ball pstate) 
                                        BACKGROUND))))


(check-expect 
 (pview
  (make-pstate (make-ball (make-posn 100 100) (make-posn 1 1))
               (make-paddle 10 3)
               (make-paddle 10 3)
               0 0))
 (draw-paddles
  (make-paddle 10 3)
  (make-paddle 10 3)
  (draw-scores 0 0 
               (draw-ball (make-ball (make-posn 100 100) (make-posn 1 1))
                          BACKGROUND))))

;; draw-scores : number number image -> image
; Draws the player's scores on the image. The 1st number being the 1st players
; score, the 2nd being the 2nd.
(define (draw-scores p1 p2 img)
  (place-image
   (draw-score p2)
   (* 4/10 (image-width img)) (/ (image-height img) 10)
   (place-image
    (draw-score p1)
    (* 6/10 (image-width img)) (/ (image-height img) 10)
    img)))

;; number -> image 
(define (draw-score s)
  (text (number->string s) 30 'white))

(check-expect (draw-scores 1 1 BACKGROUND)
              (place-image (text "1" 30 'white)
                           400 50
                           (place-image (text "1" 30 'white) 600 50 BACKGROUND)))
(check-expect (draw-scores 1 5 BACKGROUND)
              (place-image (text "5" 30 'white)
                           400 50
                           (place-image (text "1" 30 'white) 600 50 BACKGROUND)))
(check-expect (draw-scores 1 5 (empty-scene 100 100 'black))
              (place-image (text "5" 30 'white)
                           40 10
                           (place-image (text "1" 30 'white) 
                                        60 10 
                                        (empty-scene 100 100 'black))))

;; draw-ball : ball image -> image
; Draws the ball onto the image
(define (draw-ball ball img)
  (place-image BALL (posn-x (ball-loc ball)) (posn-y (ball-loc ball)) img))

(check-expect (draw-ball (make-ball (make-posn 10 10) (make-posn 1 1)) BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 10) (make-posn 4 90)) BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 12) (make-posn 4 90)) BACKGROUND)
              (place-image BALL 10 12 BACKGROUND))

;; draw-paddle : paddle number image -> image
; Draws a single paddle on the screen
(define (draw-paddle paddle player-number img)
  (place-image PADDLE
               (posn-x (paddle-posn paddle player-number (image-width img)))
               (posn-y (paddle-posn paddle player-number (image-width img)))
               img))

(check-expect (draw-paddle (make-paddle 10 3) 2 BACKGROUND)
              (place-image PADDLE 62.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 2 BACKGROUND)
              (place-image PADDLE 62.5 100 BACKGROUND))
(check-expect (draw-paddle (make-paddle 10 3) 1 BACKGROUND)
              (place-image PADDLE 937.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 1 BACKGROUND)
              (place-image PADDLE 937.5 100 BACKGROUND))

;; draw-paddles : paddle paddle image -> image
; Draws the paddles on the image
(define (draw-paddles p1 p2 img)
  (draw-paddle p1 1 (draw-paddle p2 2 img)))

(check-expect (draw-paddles (make-paddle 10 3) (make-paddle 10 3) BACKGROUND)
              (draw-paddle (make-paddle 10 3) 1
                           (draw-paddle (make-paddle 10 3) 2 BACKGROUND)))
(check-expect (draw-paddles (make-paddle 100 3) (make-paddle 10 3) BACKGROUND)
              (draw-paddle (make-paddle 100 3) 1
                           (draw-paddle (make-paddle 10 3) 2
                                        BACKGROUND)))

;                                                  
;                                                  
;                                                  
;                       ;;      ;;    ;;           
;                        ;       ;     ;           
;                        ;       ;     ;           
;   ;; ;;    ;;;;    ;;; ;   ;;; ;     ;     ;;;;  
;    ;;  ;  ;    ;  ;   ;;  ;   ;;     ;    ;    ; 
;    ;   ;   ;;;;;  ;    ;  ;    ;     ;    ;;;;;; 
;    ;   ;  ;    ;  ;    ;  ;    ;     ;    ;      
;    ;   ;  ;   ;;  ;   ;;  ;   ;;     ;    ;      
;    ;;;;    ;;; ;;  ;;; ;;  ;;; ;; ;;;;;;;  ;;;;; 
;    ;                                             
;    ;                                             
;   ;;;                                            
;                                                  

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
              [(= player 1) P1-PADDLE-LOC]
              [(= player 2) P2-PADDLE-LOC]
              [else (error "player isn't 1 or 2")]))
   (paddle-y paddle)))

;; MF: the checks work in BSL because nm.5  is interpreted as exact rationals 
(check-expect (paddle-posn (make-paddle 10 3) 2 1000) (make-posn 62.5 10))
(check-expect (paddle-posn (make-paddle 10 3) 1 1000) (make-posn 937.5 10))
(check-expect (paddle-posn (make-paddle 50 3) 1 1000) (make-posn 937.5 50))
(check-error (paddle-posn (make-paddle 50 3) 0 1000) "player isn't 1 or 2")

;; move-paddle-up : paddle -> paddle
; Moves a paddle up. 
(define (paddle-up paddle)
  (make-paddle (paddle-y paddle) (+ (paddle-vec paddle) PADDLE-DELTA)))

(define paddle-50-0 (make-paddle 50 0))
(define paddle-50-1 (make-paddle 50 1))
(define paddle-50-4 (make-paddle 50 -4))

(check-expect (paddle-up paddle-50-0) (make-paddle 50 (+ 0 PADDLE-DELTA)))
(check-expect (paddle-up paddle-50-1) (make-paddle 50 (+ 1 PADDLE-DELTA)))
(check-expect (paddle-up paddle-50-4) (make-paddle 50 (+ -4 PADDLE-DELTA)))

;; move-paddle-down : paddle -> paddle
; Moves the paddle down.
(define (paddle-down paddle)
  (make-paddle (paddle-y paddle) (- (paddle-vec paddle) PADDLE-DELTA)))

(check-expect (paddle-down paddle-50-0) (make-paddle 50 (- 0 PADDLE-DELTA)))
(check-expect (paddle-down paddle-50-1) (make-paddle 50 (- 1 PADDLE-DELTA)))
(check-expect (paddle-down paddle-50-4) (make-paddle 50 (- -4 PADDLE-DELTA)))

;; apply-paddle-vector : paddle -> paddle
; Moves the paddle and applies friction as appropriate.
(define (paddle-move paddle)
  (make-paddle (+ (paddle-y paddle) (paddle-vec paddle))
               (* FRICTION (paddle-vec paddle))))

(check-expect (paddle-move paddle-50-0) (make-paddle 50 (* FRICTION 0)))
(check-expect (paddle-move paddle-50-1) (make-paddle (+ 50 1) (* FRICTION 1)))
(check-expect (paddle-move paddle-50-4) (make-paddle (+ 50 -4) (* FRICTION -4)))

;; paddle-deflection : paddle -> paddle
; Deflects the paddle from the top and bottom edges of the screen.
(define (paddle-deflection paddle)
  (make-paddle
   (paddle-y paddle)
   (* (paddle-vec paddle)
      (cond
        ;; MF: introduced auxiliary functions that make comments superfluous
        [(paddle-between-top-and-bottom paddle) 1]
        [(paddle-out-but-returning paddle)      1]
        [(paddle-out-and-not-returning paddle) -1]
        [else 1]))))

;; paddle -> boolean 
(define (paddle-between-top-and-bottom paddle)
  (and (> 0 (- (* 1/2 PADDLE-HEIGHT) (paddle-y paddle))) 
       (< (+ (* 1/2 PADDLE-HEIGHT) (paddle-y paddle)) BOARD-HEIGHT)))

;; paddle -> boolean
(define (paddle-out-but-returning paddle)
  (or (and (< 0 (- (* 1/2 PADDLE-HEIGHT) (paddle-y paddle)))
           (positive? (paddle-vec paddle)))
      (and (> (+ (* 1/2 PADDLE-HEIGHT) (paddle-y paddle)) BOARD-HEIGHT)
           (negative? (paddle-vec paddle)))))

;; paddle -> boolean 
(define (paddle-out-and-not-returning paddle)
  (or (and (< 0 (- (* 1/2 PADDLE-HEIGHT) (paddle-y paddle)))
           (negative? (paddle-vec paddle)))
      (and (> (+ (* 1/2 PADDLE-HEIGHT) (paddle-y paddle)) BOARD-HEIGHT)
           (positive? (paddle-vec paddle)))))

(check-expect (paddle-deflection (make-paddle 100 5)) (make-paddle 100 5))
(check-expect (paddle-deflection (make-paddle 100 -5)) (make-paddle 100 -5))
(check-expect (paddle-deflection (make-paddle -60 5)) (make-paddle -60 5))
(check-expect (paddle-deflection (make-paddle -60 -5)) (make-paddle -60 5))
(check-expect (paddle-deflection (make-paddle 1060 -5)) (make-paddle 1060 -5))
(check-expect (paddle-deflection (make-paddle 1100 5)) (make-paddle 1100 -5))

;; paddle -> paddle
; Like paddle-deflection, but instead of deflecting, it simply doesn't allow
; paddles out of bounds.
(define (paddle-keep-inside p)
  (make-paddle (min (max (paddle-y p) 50) (- BOARD-HEIGHT 50))
               (paddle-vec p)))

(check-expect (paddle-keep-inside (make-paddle 100 5)) (make-paddle 100 5))
(check-expect (paddle-keep-inside (make-paddle 100 -5)) (make-paddle 100 -5))
(check-expect (paddle-keep-inside (make-paddle 50 5)) (make-paddle 50 5))
(check-expect (paddle-keep-inside (make-paddle -60 -5)) (make-paddle 50 -5))
(check-expect (paddle-keep-inside (make-paddle 950 -5)) (make-paddle 450 -5))
(check-expect (paddle-keep-inside (make-paddle 1100 -5)) (make-paddle 450 -5))

;; paddle number posn -> boolean
; Checks whether the given posn is inside the given paddle.
;; MF: I would rewrite this function so that it consumes a paddle and a posn 
(define (paddle-in? p player loc)
  (and (<= (- (posn-x (paddle-posn p player BOARD-WIDTH)) 5)
           (posn-x loc)
           (+ (posn-x (paddle-posn p player BOARD-WIDTH)) 5))
       (<= (- (posn-y (paddle-posn p player BOARD-WIDTH)) 50)
           (posn-y loc)
           (+ (posn-y (paddle-posn p player BOARD-WIDTH)) 50))))

(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 62.5 200)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 62.5 150)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 62.5 250)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 62.5 250)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 57.5 150 )) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 67.5 150)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 57.5 250)) true)
(check-expect (paddle-in? (make-paddle 200 0) 2 (make-posn 67.5 250)) true)

;                                  
;                                  
;                                  
;  ;;                 ;;      ;;   
;   ;                  ;       ;   
;   ;                  ;       ;   
;   ; ;;;    ;;;;      ;       ;   
;   ;;   ;  ;    ;     ;       ;   
;   ;    ;   ;;;;;     ;       ;   
;   ;    ;  ;    ;     ;       ;   
;   ;;   ;  ;   ;;     ;       ;   
;  ;; ;;;    ;;; ;; ;;;;;;; ;;;;;;;
;                                  
;                                  
;                                  
;                                  

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
(define DEFAULT-BALL 
  (make-ball (make-posn (/ BOARD-WIDTH 2) (/ BOARD-HEIGHT 2)) (make-posn 1 1)))

(define (ball-x ball) (posn-x (ball-loc ball)))
(define (ball-y ball) (posn-y (ball-loc ball)))

;; apply-ball-vector : ball -> ball
; Moves the given ball around on its vector.
(define (ball-move ball)
  (make-ball (add-posns (ball-loc ball) (scale-posn 3 (ball-vec ball)))
             (ball-vec ball)))

(check-expect (ball-move (make-ball (make-posn 10 10) (make-posn 1 1))) 
              (make-ball (make-posn 13 13) (make-posn 1 1)))
(check-expect (ball-move (make-ball (make-posn 10 10) (make-posn -1 1)))
              (make-ball (make-posn 7 13) (make-posn -1 1)))
(check-expect (ball-move (make-ball (make-posn 10 10) (make-posn -1 -1)))
              (make-ball (make-posn 7 7) (make-posn -1 -1)))

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
               [(paddle-in? p1paddle 1 (ball-loc ball))
                (cond ; First player's paddle (right side paddle)
                  [(equal? (ball-vec ball) (make-posn 1 -1)) (make-posn -1 -1)]
                  [(equal? (ball-vec ball) (make-posn 1 1)) (make-posn -1 1)]
                  [else (ball-vec ball)])]
               [(paddle-in? p2paddle 2 (ball-loc ball))
                (cond ; Second player's paddle
                  [(equal? (ball-vec ball) (make-posn -1 -1)) (make-posn -1 1)]
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
  (make-posn (+ (posn-x p1) (posn-x p2))
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


(check-expect (sub-posns (make-posn 10 10) (make-posn 1 1)) (make-posn 9 9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 12 14)) (make-posn -9 -9))
(check-expect (sub-posns (make-posn 3 5) (make-posn 0 0)) (make-posn 3 5))
(check-expect (sub-posns (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (sub-posns (make-posn -1 -2) (make-posn 3 4)) (make-posn -4 -6))
