;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; Pong
; Andrew Amis
; Started 12/12/11
; A simple implementation of pong

(require picturing-programs)





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
(define BALL (circle 10 'solid 'white))

;; Coefficient of friction used to slow down paddles
(define FRICTION 0.99)

;; Amount each keypress increases the paddle velocity
(define PADDLE_DELTA 1)

;; player1's paddle's x location
(define P1_PADDLE_LOC (* 1/10 BOARD-WIDTH))

;; player2's paddle's x location
(define P2_PADDLE_LOC (* 9/10 BOARD-WIDTH))

;; paddle width and height
(define PADDLE_WIDTH 10)
(define PADDLE_HEIGHT 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dynamic Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; velocity : number number
(define-struct velocity (x y))
; where x and y represent horizontal and vertical velocity, respectively.
; (Positive numbers represent movement rightwards & downwards;
;  negatives represent leftwards & upwards.)
; Examples:
;    (make-velocity -50 20) -- moving leftwards & down
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
              [(= player 1) 15/16]
              [(= player 2) 1/16]
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



;; ball : posn velocity
(define-struct ball (loc vec))
; ball-loc : posn representing the location of the posn
; ball-vec : velocity representing the velocity of the posn

#;(make-ball (make-posn 10 10)
             (make-velocity 1 1))

; Templates
;; fun-for-ball : ball -> ???
#;(define (fun-for-ball ball)
    ... (ball-loc ball) ...
    ... (ball-vec ball) ...)

;; fun-for-ball : ball -> ball
#;(define (fun-for-ball ball)
    (make-ball (ball-loc ball)
               (ball-vec ball)))




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
                          (make-velocity 1 1))
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



;; apply-paddle-vector : paddle -> paddle
; Moves the paddle and applies friction as appropriate.
(define (apply-paddle-vector paddle)
  (make-paddle (+ (paddle-y paddle) (paddle-vec paddle))
               (* FRICTION (paddle-vec paddle))))

(check-expect (apply-paddle-vector (make-paddle 100 3))
              (make-paddle 103 2.97))
(check-expect (apply-paddle-vector (make-paddle 100 -3))
              (make-paddle 97 -2.97))
(check-expect (apply-paddle-vector (make-paddle 50 50))
              (make-paddle 100 49.5))


;; move-paddle-up : paddle -> paddle
; Moves a paddle up. Technically speaking, it increments or decrements the
; internal velocity of the paddle.
(define (move-paddle-up paddle)
  (make-paddle (paddle-y paddle)
               (+ (paddle-vec paddle) PADDLE_DELTA)))


(check-expect (move-paddle-up (make-paddle 50 0))
              (make-paddle 50 (+ 0 PADDLE_DELTA)))
(check-expect (move-paddle-up (make-paddle 50 1))
              (make-paddle 50 (+ 1 PADDLE_DELTA)))
(check-expect (move-paddle-up (make-paddle 50 -1))
              (make-paddle 50 (+ -1 PADDLE_DELTA)))

;; move-paddle-down : paddle -> paddle
; Moves the paddle down.
(define (move-paddle-down paddle)
  (make-paddle (paddle-y paddle)
               (- (paddle-vec paddle) PADDLE_DELTA)))

(check-expect (move-paddle-down (make-paddle 50 0))
              (make-paddle 50 (- 0 PADDLE_DELTA)))
(check-expect (move-paddle-down (make-paddle 50 1))
              (make-paddle 50 (- 1 PADDLE_DELTA)))
(check-expect (move-paddle-down (make-paddle 50 -4))
              (make-paddle 50 (- -4 PADDLE_DELTA)))


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

(check-expect (handle-keyboard (make-pstate (make-ball (make-posn 100 100)
                                                       (make-velocity 1 1))
                                            (make-paddle 100 3)
                                            (make-paddle 100 3)
                                            0 0) "up")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (make-paddle 100 2)
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard (make-pstate (make-ball (make-posn 100 100)
                                                       (make-velocity 1 1))
                                            (make-paddle 100 3)
                                            (make-paddle 100 3)
                                            0 0) "down")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (make-paddle 100 4)
                           (make-paddle 100 3)
                           0 0))
(check-expect (handle-keyboard (make-pstate (make-ball (make-posn 100 100)
                                                       (make-velocity 1 1))
                                            (make-paddle 100 3)
                                            (make-paddle 100 3)
                                            0 0) "a")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 2)
                           0 0))
(check-expect (handle-keyboard (make-pstate (make-ball (make-posn 100 100)
                                                       (make-velocity 1 1))
                                            (make-paddle 100 3)
                                            (make-paddle 100 3)
                                            0 0) "z")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 4)
                           0 0))
(check-expect (handle-keyboard (make-pstate (make-ball (make-posn 100 100)
                                                       (make-velocity 1 1))
                                            (make-paddle 100 3)
                                            (make-paddle 100 3)
                                            0 0) "f")
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (make-paddle 100 3)
                           (make-paddle 100 3)
                           0 0))

;; handle-tick : pstate
; Handles a single tick.
(define (handle-tick pstate)
  (make-pstate 
   (pstate-ball pstate)
   (apply-paddle-vector (pstate-p1p pstate))
   (apply-paddle-vector (pstate-p2p pstate))
   (pstate-p1s pstate)
   (pstate-p2s pstate)))

(check-expect (handle-tick (make-pstate (make-ball (make-posn 100 100)
                                                   (make-velocity 1 1))
                                        (make-paddle 100 3)
                                        (make-paddle 100 3)
                                        0 0))
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
                           (apply-paddle-vector (make-paddle 100 3))
                           (apply-paddle-vector (make-paddle 100 3))
                           0 0))

(check-expect (handle-tick (make-pstate (make-ball (make-posn 100 100)
                                                   (make-velocity 1 1))
                                        (make-paddle 100 10)
                                        (make-paddle 100 4)
                                        0 0))
              (make-pstate (make-ball (make-posn 100 100)
                                      (make-velocity 1 1))
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
   (text (number->string p1) 30 'white)
   (* 4/10 (image-width img)) (/ (image-height img) 10)
   (place-image
    (text (number->string p2) 30 'white)
    (* 6/10 (image-width img)) (/ (image-height img) 10)
    img)))

(check-expect (draw-scores 1 1 BACKGROUND)
              (place-image
               (text "1" 30 'white)
               400 50
               (place-image
                (text "1" 30 'white)
                600 50
                BACKGROUND)))
(check-expect (draw-scores 1 5 BACKGROUND)
              (place-image
               (text "1" 30 'white)
               400 50
               (place-image
                (text "5" 30 'white)
                600 50
                BACKGROUND)))
(check-expect (draw-scores 1 5 (rectangle 100 100 'solid 'black))
              (place-image
               (text "1" 30 'white)
               40 10
               (place-image
                (text "5" 30 'white)
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
                                    (make-velocity 1 1))
                         BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 10)
                                    (make-velocity 4 90))
                         BACKGROUND)
              (place-image BALL 10 10 BACKGROUND))
(check-expect (draw-ball (make-ball (make-posn 10 12)
                                    (make-velocity 4 90))
                         BACKGROUND)
              (place-image BALL 10 12 BACKGROUND))


;; draw-paddle : paddle number image -> image
; Draws a single paddle on the screen
(define (draw-paddle paddle player-number img)
  (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                          'solid 'white)
               (posn-x (paddle-posn paddle player-number (image-width img)))
               (posn-y (paddle-posn paddle player-number (image-width img)))
               img))

(check-expect (draw-paddle (make-paddle 10 3) 2 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid 'white)
                           62.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 2 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid 'white)
                           62.5 100 BACKGROUND))
(check-expect (draw-paddle (make-paddle 10 3) 1 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid 'white)
                           937.5 10 BACKGROUND))
(check-expect (draw-paddle (make-paddle 100 3) 1 BACKGROUND)
              (place-image (rectangle PADDLE_WIDTH PADDLE_HEIGHT
                                      'solid 'white)
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
                                       (make-velocity 1 1))
                            (make-paddle 10 3)
                            (make-paddle 10 3)
                            0 0))
              (draw-paddles
               (make-paddle 10 3)
               (make-paddle 10 3)
               (draw-scores 0 0
                            (draw-ball (make-ball
                                        (make-posn 100 100)
                                        (make-velocity 1 1)) BACKGROUND))))





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
  (big-bang (make-pstate (make-ball (make-posn 100 100)
                                    (make-velocity 1 1))
                         (make-paddle 100 3)
                         (make-paddle 100 3)
                         0 0)
            (to-draw pview)
            (on-key handle-keyboard)
            (on-tick handle-tick)
            (state true))
  )
