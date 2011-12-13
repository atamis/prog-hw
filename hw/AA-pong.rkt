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


;; Represents board dimensions (pixels)
(define BOARD-HEIGHT 500)
(define BOARD-WIDTH 1000)

;; Represents the flat background upon which the game will be drawn.
(define BACKGROUND (empty-scene BOARD-WIDTH BOARD-HEIGHT))

;; Coefficient of friction used to slow down paddles
(define FRICTION 0.99)

;; player1's paddle's x location
(define P1_PADDLE_LOC (* 1/10 BOARD-WIDTH))

;; player2's paddle's x location
(define P2_PADDLE_LOC (* 9/10 BOARD-WIDTH))


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
; paddle-vec : the velocity of the paddline. Velocity should only be up or down,
;              not side to side. This should be affected by friction.

#;(make-paddle 10 (make-velocity 0 3))

; Templates
;; fun-for-paddle : paddle -> ???
#;(define (fun-for-paddle paddle)
    ... (paddle-y paddle)
    ... (paddle-vec paddle))

;; fun-for-paddle : paddle -> paddle
#;(define (fun-for-paddle paddle)
    (make-paddle (paddle-y paddle)
                 (paddly-vec paddle)))

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
(define (fun-for-ball ball)
  (make-ball (ball-loc ball)
             (ball-vec ball)))




;; state : ball paddle paddle number number
(define-struct state (ball p1p p2p p1s p2s))
; state-ball : ball representing the ball in this game
; state-p1p : paddle, representing the paddle location and velocity of the 1st
;             players paddle.
; state-p2p : paddle, representing the paddle location and velocity of the 2nd
;             players paddle.
; state-p1s : non-negative number representing the 1st players score
; state-p2s : non-negative number representing the 2nd players score

#;(make-state (make-ball (make-posn 10 10)
                         (make-velocity 1 1))
              (make-paddle 10 (make-velocity 0 3))
              (make-paddle 10 (make-velocity 0 3))
              0 0)

; Templates
;; fun-for-state : state -> ???
#;(define (fun-for-state state)
    ... (state-ball state) ...
    ... (state-p1p state) ...
    ... (state-p2p state) ...
    ... (state-p1s state) ...
    ... (state-p2s state))

;; fun-for-state : state -> state
#;(define (fun-for-state state)
    (make-state 
     (state-ball state)
     (state-p1p state)
     (state-p2p state)
     (state-p1s state)
     (state-p2s state)))