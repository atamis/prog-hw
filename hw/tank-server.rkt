#lang racket

(require test-engine/racket-tests
         picturing-programs)

(define-struct posn (x y)
  #:transparent)



;                                                                 
;                                                                 
;                                                                 
;    ;;;                                                          
;   ;   ;                        ;                    ;           
;   ;   ;                        ;                    ;           
;   ;       ;;;   ;;;;    ;;;   ;;;     ;;;;  ;;;;   ;;;     ;;;  
;   ;      ;   ;  ;   ;  ;   ;   ;     ;   ;  ;   ;   ;     ;   ; 
;   ;      ;   ;  ;   ;   ;;;    ;     ;   ;  ;   ;   ;      ;;;  
;   ;      ;   ;  ;   ;      ;   ;     ;   ;  ;   ;   ;         ; 
;   ;   ;  ;   ;  ;   ;  ;   ;   ;     ;  ;;  ;   ;   ;     ;   ; 
;    ;;;    ;;;   ;   ;   ;;;     ;;    ;; ;  ;   ;    ;;    ;;;  
;                                                                 
;                                                                 
;                                                                 


(define MAX-HEALTH 100)
(define MISSILE-DAMAGE 10)
(define MISSILES-MOVE-MILLI 0.1)
(define ENABLE-TESTS false)
(define TANK-SIZE 10)



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

(define π pi)

(define-struct tank (name x y rot health)
  #:transparent)
;    name: iworld
;    x, y: coordinates
;    rot: rotation in degrees.
;    health: out of 100.

(define-struct missile (x y rot)
  #:transparent)
;    x, y: coordinates
;    rot: rotation of the missile. Use to determine the next location.


(define-struct world (tanks missiles start-time last-tick)
  #:transparent)
;  tanks: listof[tank] tanks currently in the game
;  missiles: listof[missile] missiles currently in flight
;  start-time: time the universe started, in milliseconds
;  last-tick: the last time the universe ticked, in milliseconds


;; degrees->radians : degrees -> radians
; Converts degrees to radians
(define (degrees->radians deg)
  (* deg (/ π 180)))

(check-within (degrees->radians 180) π 0.001)
(check-within (degrees->radians 90) (/ π 2) 0.001)


;; tank->sexpr : tank -> sexpr
; Renders a tank as a sexpr
(define (tank->sexpr t)
  `(tank ,(iworld-name (tank-name t))
         ,(tank-x t)
         ,(tank-y t)
         ,(tank-rot t)
         ,(tank-health t)))

(check-expect (tank->sexpr (make-tank iworld1 4 5 0 100))
              '(tank "iworld1" 4 5 0 100))
(check-expect (tank->sexpr (make-tank iworld2 10 30 0 50))
              '(tank "iworld2" 10 30 0 50))

;; tanks->sexpr : listof[tank] -> sexpr
; Renders a list of tanks as a sexpr.
(define (tanks->sexpr lot)
  (cons 'tanks (map tank->sexpr lot)))

(check-expect (tanks->sexpr empty)
              '(tanks))
(check-expect (tanks->sexpr (list (make-tank iworld1 4 5 0 100)))
              '(tanks (tank "iworld1" 4 5 0 100)))
(check-expect (tanks->sexpr (list (make-tank iworld1 4 5 0 100)
                                  (make-tank iworld2 10 30 0 50)))
              '(tanks (tank "iworld1" 4 5 0 100)
                      (tank "iworld2" 10 30 0 50)))

;; missile->sexpr : missile -> sexpr
; Renders a missile as a sexpr.
(define (missile->sexpr m)
  `(missile
    ,(missile-x m)
    ,(missile-y m)))

(check-expect (missile->sexpr (make-missile 10 15 45))
              '(missile 10 15))
(check-expect (missile->sexpr (make-missile 30 12 60))
              '(missile 30 12))
;; missiles->sexpr : listof[missile] -> sexpr
; Renders a list of missiles as a sexpr.
(define (missiles->sexpr lom)
  (cons 'missiles (map missile->sexpr lom)))

(check-expect (missiles->sexpr empty) '(missiles))
(check-expect (missiles->sexpr (list (make-missile 10 10 45)))
              '(missiles (missile 10 10)))
(check-expect (missiles->sexpr (list (make-missile 10 15 45)
                                     (make-missile 11 15 45)
                                     (make-missile 12 15 45)
                                     (make-missile 13 15 45)))
              '(missiles (missile 10 15)
                         (missile 11 15)
                         (missile 12 15)
                         (missile 13 15)))

;; world->sexpr : world -> sexpr
; Renders the world a sexpr
(define (world->sexpr world)
  `(world ,(tanks->sexpr (world-tanks world))
          ,(missiles->sexpr (world-missiles world))))

(check-expect (world->sexpr (make-world empty empty 0 0))
              '(world (tanks) (missiles)))
(check-expect (world->sexpr (make-world (list (make-tank iworld1 4 5 0 100))
                                        (list (make-missile 10 15 45))
                                        0 0))
              '(world (tanks (tank "iworld1" 4 5 0 100))
                      (missiles (missile 10 15))))
(check-expect (world->sexpr (make-world (list (make-tank iworld1 4 5 0 100)
                                              (make-tank iworld2 10 30 0 50))
                                        (list (make-missile 10 15 45)
                                              (make-missile 11 15 45)
                                              (make-missile 12 15 45)
                                              (make-missile 13 15 45))
                                        0 0))
              '(world (tanks (tank "iworld1" 4 5 0 100)
                             (tank "iworld2" 10 30 0 50))
                      (missiles (missile 10 15)
                                (missile 11 15)
                                (missile 12 15)
                                (missile 13 15))))

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

;; full-world->mail : iworld world -> mail
; Creates a mail message to update the given iworld as to the state of the
; world.
(define (full-world->mail recip world)
  (make-mail recip (world->sexpr world)))

(check-expect (full-world->mail
               iworld1
               (make-world (list (make-tank iworld1 4 5 0 100)
                                 (make-tank iworld2 10 30 0 50))
                           (list (make-missile 10 15 45)
                                 (make-missile 11 15 45)
                                 (make-missile 12 15 45)
                                 (make-missile 13 15 45))
                           0 0))
              (make-mail iworld1 '(world (tanks (tank "iworld1" 4 5 0 100)
                                                (tank "iworld2" 10 30 0 50))
                                         (missiles (missile 10 15)
                                                   (missile 11 15)
                                                   (missile 12 15)
                                                   (missile 13 15)))))
(check-expect (full-world->mail iworld3 (make-world empty empty 0 0))
              (make-mail iworld3 '(world (tanks) (missiles))))



;; new : world iworld -> bundle
; Handles new connections.
(define (new world iworld)
  (let ([new-world (make-world
                    (cons (make-tank iworld 10 10 0 MAX-HEALTH)
                          (world-tanks world))
                    (world-missiles world)
                    (world-start-time world)
                    (world-last-tick world))])
    (make-bundle
     new-world
     (list (full-world->mail iworld new-world))
     empty)))

(check-expect (new (make-world empty empty 0 0) iworld1)
              (make-bundle (make-world (list (make-tank iworld1 
                                                        10 10
                                                        0 MAX-HEALTH))
                                       empty 0 0)
                           (list (make-mail iworld1
                                            `(world (tanks
                                                     (tank "iworld1" 10 10 0
                                                           ,MAX-HEALTH))
                                                    (missiles))))
                           empty))
(check-expect (new (make-world (list (make-tank iworld1 
                                                10 10
                                                0 MAX-HEALTH))
                               empty 0 0) iworld2)
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH))
                                       empty 0 0)
                           (list (make-mail iworld2
                                            `(world (tanks
                                                     (tank "iworld2" 10 10 0
                                                           ,MAX-HEALTH)
                                                     (tank "iworld1" 10 10 0
                                                           ,MAX-HEALTH))
                                                    (missiles))))
                           empty))

;; missile-tank-hit? : number number number number number -> boolean
; x1, y1, is the first tank, theta is the angle of the tank, x2, y2 is the 2nd
; tank. Returns true if the missile hits the tank.
(define (missile-tank-hit? missile tank)
  (letrec ([x1 (missile-x missile)]
           [y1 (missile-y missile)]
           [θ (if (= (missile-rot missile) 0)
                  0.001 ;; Else we get division by zero errors.
                  (missile-rot missile))]
           [x2 (tank-x tank)]
           [y2 (tank-y tank)]
           [m (tan (degrees->radians θ))]
           [b1 (- y1 (* m x1))]
           [b2 (- y2 (* (- (/ 1 m)) x2))]
           [x3 (/ (* (- m) (- b1 b2)) (+ (sqr m) 1))]
           [y3 (+ (* m x3) b1)]
           [dist (sqrt (+ (sqr (- x3 x2)) (sqr (- y3 y2))))])
    (begin (when ENABLE-TESTS
             (display (format "~s\n" `(m ,m
                                     m2 ,(- (/ 1 m))
                                     b1 ,b1
                                     b2 ,b2
                                     x3 ,x3
                                     y3 ,y3
                                     dist ,dist))))
                   (< dist TANK-SIZE))))


(check-expect (missile-tank-hit? (make-missile -4 2 26)
                                 (make-tank iworld1 8 4 30 MAX-HEALTH)) true)
(check-expect (missile-tank-hit? (make-missile -10 5 45)
                                 (make-tank iworld1 8 4 30 MAX-HEALTH)) false)


;; handle-missiles : world -> world
; Handles missiles and the destruction of tanks. ;MISSILE-DAMAGE
(define (handle-missiles world)
  (make-world
   (map {λ (tank)
          (make-tank
           (tank-name tank)
           (tank-x tank)
           (tank-y tank)
           (tank-rot tank)
           (- (tank-health tank)
              (* MISSILE-DAMAGE
                 (foldl {λ (missile acc) (if (missile-tank-hit? missile tank)
                                      (+ 1 acc)
                                      acc)}
                 0
                 (world-missiles world)))))} (world-tanks world))
   empty
   (world-start-time world)
   (world-last-tick world)))

(check-expect (handle-missiles (make-world
                                (list (make-tank iworld1 8 4 30 MAX-HEALTH))
                                (list (make-missile -4 2 26)) 0 0))
              (make-world
               (list (make-tank iworld1 8 4 30 (- MAX-HEALTH MISSILE-DAMAGE)))
               empty 0 0))
(check-expect (handle-missiles (make-world
                                (list (make-tank iworld1 8 4 30 MAX-HEALTH))
                                (list (make-missile -4 2 26)
                                      (make-missile -10 5 45)) 0 0))
              (make-world
               (list (make-tank iworld1 8 4 30 (- MAX-HEALTH MISSILE-DAMAGE)))
               empty 0 0))

;; tick : world -> bundle
; Executed every tick. Sends the world to all players. Also does collision
; detection
(define (tick world)
  (let ([new-world (handle-missiles
                    (make-world (world-tanks world)
                                (world-missiles world)
                                (world-start-time world)
                                (if ENABLE-TESTS
                                    0
                                    (current-milliseconds))))])
    (make-bundle new-world
                 (map {λ (tank)
                        (full-world->mail (tank-name tank) new-world)}
                      (world-tanks new-world))
                 empty)))

(check-expect (tick (make-world empty empty 0 0))
              (make-bundle (make-world empty empty 0 0)
                           empty
                           empty))
(check-expect (tick (make-world (list (make-tank iworld1 
                                                 10 10
                                                 0 MAX-HEALTH))
                                empty 0 0))
              (make-bundle
               (make-world (list (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH))
                           empty 0 0)
               (list (full-world->mail iworld1 
                                       (make-world
                                        (list (make-tank iworld1 
                                                         10 10
                                                         0 MAX-HEALTH))
                                        empty 0 0)))
               empty))
(check-expect (tick (make-world (list 
                                 (make-tank iworld2 
                                            10 10
                                            0 MAX-HEALTH)
                                 (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH))
                                empty 0 0))
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH))
                                       empty 0 0)
                           (list (full-world->mail
                                  iworld2
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH))
                                              empty 0 0))
                                 (full-world->mail
                                  iworld1
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH))
                                              empty 0 0)))
                           
                           empty))


;; broadcast : listof[iworld] sexpr -> listof[mail]
; Bulk mailer. Broadcasts the given message to the given iworlds.
(define (broadcast iworlds message)
  (map {λ (iworld) (make-mail iworld message)} iworlds))


;; disconnect : world iworld -> bundle
; Handles the disconnection of a user.
; TODO: inform clients that person has left?
(define (disconnect world iworld)
  (let ([new-tanks (filter {λ (tank) (not (iworld=?
                                           (tank-name tank)
                                           iworld))}
                           (world-tanks world))])
    (make-bundle
     (make-world
      new-tanks
      (world-missiles world)
      (world-start-time world)
      (world-last-tick world))
     (broadcast (map tank-name new-tanks) `(disconnected ,(iworld-name iworld)))
     (list iworld))))

(check-expect (disconnect (make-world empty
                                      empty 0 0)
                          iworld1)
              (make-bundle (make-world empty empty 0 0)
                           empty
                           (list iworld1)))
(check-expect (disconnect (make-world (list
                                       (make-tank iworld1 
                                                  10 10
                                                  0 MAX-HEALTH))
                                      empty 0 0)
                          iworld1)
              (make-bundle (make-world empty empty 0 0)
                           empty
                           (list iworld1)))
(check-expect (disconnect (make-world (list
                                       (make-tank iworld1 
                                                  10 10
                                                  0 MAX-HEALTH)
                                       (make-tank iworld2 
                                                  10 10
                                                  0 MAX-HEALTH))
                                      empty 0 0)
                          iworld1)
              (make-bundle (make-world (list
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)) empty
                                                                  0 0)
                           (list (make-mail iworld2 '(disconnected "iworld1")))
                           (list iworld1)))


;; msg : world iworld sexpr -> bundle
; Responds to a message from a client.
(define (msg world iworld message)
  (let ([this (car (filter {λ (tank) (iworld=? (tank-name tank) iworld)}
                           (world-tanks world)))]
        [other (filter {λ (tank) (not (iworld=? (tank-name tank) iworld))}
                       (world-tanks world))])
    (cond
      [(and (cons? message)
            (symbol=? (car message) 'move))
       (make-bundle
        (make-world
         (cons (make-tank (tank-name this)
                          (cadadr message) ; x
                          (car (cddadr message)) ; y
                          (car (cdaddr message)) ; rot
                          (tank-health this))
               other)
         (world-missiles world)
         0 0)
        empty
        empty)]
      [(symbol=? 'shoot message)
       (let ([new-bullet (make-missile (tank-x this)
                                       (tank-y this)
                                       (tank-rot this))])
         (make-bundle
          (make-world
           (world-tanks world)
           (cons new-bullet
                 (world-missiles world))
           0 0)
          (broadcast (map tank-name other)
                     `(bullet ,(missile-x new-bullet)
                              ,(missile-y new-bullet)
                              ,(missile-rot new-bullet)))
          empty))]
      [else (make-bundle world empty empty)])))


#;(msg (make-world (list
                    (make-tank iworld1 10 10 0 MAX-HEALTH)
                    (make-tank iworld2 10 10 0 MAX-HEALTH))
                   empty)
       iworld1 '(move (coords 40 50) (rot 30)))

(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty
                               0 0)
                   iworld1 '(move (coords 40 50) (rot 30)))
              (make-bundle
               (make-world (list
                            (make-tank iworld1 40 50 30 MAX-HEALTH)
                            (make-tank iworld2 10 10 0 MAX-HEALTH))
                           empty
                           0 0)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty 0 0)
                   iworld2 '(move (coords 50 60) (rot 45)))
              (make-bundle
               (make-world (list
                            (make-tank iworld2 50 60 45 MAX-HEALTH)
                            (make-tank iworld1 10 10 0 MAX-HEALTH))
                           empty 0 0)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 20 20 0 MAX-HEALTH))
                               empty 0 0)
                   iworld2 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH)
                            (make-tank iworld2 20 20 0 MAX-HEALTH))
                           (list (make-missile 20 20 0)) 0 0)
               (list (make-mail iworld1 '(bullet 20 20 0)))
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty 0 0)
                   iworld1 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH)
                            (make-tank iworld2 10 10 0 MAX-HEALTH))
                           (list (make-missile 10 10 0)) 0 0)
               (list (make-mail iworld2 '(bullet 10 10 0)))
               empty))

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

;; world->string : world -> string
; Renders the world as a string
(define (world->string world)
  (format "~s" (world->sexpr world)))

(check-expect (world->string (make-world empty empty 0 0))
              "(world (tanks) (missiles))")

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


(define (r)
  (universe (make-world empty empty
                        (current-milliseconds)
                        (current-milliseconds))
            (on-new new)
            (on-tick tick)
            (on-disconnect disconnect)
            (on-msg msg)
            #;(to-string world->string)))


(when ENABLE-TESTS
  (test))
