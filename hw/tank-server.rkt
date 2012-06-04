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

(define-struct tank (name x y rot health last-hit)
  #:transparent)
;    name: iworld
;    x, y: coordinates
;    rot: rotation in degrees.
;    health: out of 100.
;    last-hit: last person to deal damage to this tank, iworld or empty.

(define-struct missile (x y rot owner)
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

(check-expect (tank->sexpr (make-tank iworld1 4 5 0 100 empty))
              '(tank "iworld1" 4 5 0 100))
(check-expect (tank->sexpr (make-tank iworld2 10 30 0 50 empty))
              '(tank "iworld2" 10 30 0 50))

;; tanks->sexpr : listof[tank] -> sexpr
; Renders a list of tanks as a sexpr.
(define (tanks->sexpr lot)
  (cons 'tanks (map tank->sexpr lot)))

(check-expect (tanks->sexpr empty)
              '(tanks))
(check-expect (tanks->sexpr (list (make-tank iworld1 4 5 0 100 empty)))
              '(tanks (tank "iworld1" 4 5 0 100)))
(check-expect (tanks->sexpr (list (make-tank iworld1 4 5 0 100 empty)
                                  (make-tank iworld2 10 30 0 50 empty)))
              '(tanks (tank "iworld1" 4 5 0 100)
                      (tank "iworld2" 10 30 0 50)))

;; missile->sexpr : missile -> sexpr
; Renders a missile as a sexpr.
(define (missile->sexpr m)
  `(missile
    ,(missile-x m)
    ,(missile-y m)
    ,(missile-rot m)))

(check-expect (missile->sexpr (make-missile 10 15 45 iworld1))
              '(missile 10 15 45))
(check-expect (missile->sexpr (make-missile 30 12 60 iworld1))
              '(missile 30 12 60))
;; missiles->sexpr : listof[missile] -> sexpr
; Renders a list of missiles as a false
(define (missiles->sexpr lom)
  (cons 'missiles (map missile->sexpr lom)))

(check-expect (missiles->sexpr empty) '(missiles))
(check-expect (missiles->sexpr (list (make-missile 10 10 45 iworld1)))
              '(missiles (missile 10 10 45)))
(check-expect (missiles->sexpr (list (make-missile 10 15 45 iworld1)
                                     (make-missile 11 15 45 iworld1)
                                     (make-missile 12 15 45 iworld1)
                                     (make-missile 13 15 45 iworld1)))
              '(missiles (missile 10 15 45)
                         (missile 11 15 45)
                         (missile 12 15 45)
                         (missile 13 15 45)))

;; world->sexpr : world -> sexpr
; Renders the world a sexpr
(define (world->sexpr world)
  `(world ,(tanks->sexpr (world-tanks world))
          ,(missiles->sexpr (world-missiles world))))

(check-expect (world->sexpr (make-world empty empty 0 0))
              '(world (tanks) (missiles)))
(check-expect (world->sexpr (make-world (list
                                         (make-tank iworld1 4 5 0 100 empty))
                                        (list (make-missile 10 15 45 iworld1))
                                        0 0))
              '(world (tanks (tank "iworld1" 4 5 0 100))
                      (missiles (missile 10 15 45))))
(check-expect (world->sexpr (make-world (list
                                         (make-tank iworld1 4 5 0 100 empty)
                                         (make-tank iworld2 10 30 0 50 empty))
                                        (list (make-missile 10 15 45 iworld1)
                                              (make-missile 11 15 45 iworld1)
                                              (make-missile 12 15 45 iworld1)
                                              (make-missile 13 15 45 iworld1))
                                        0 0))
              '(world (tanks (tank "iworld1" 4 5 0 100)
                             (tank "iworld2" 10 30 0 50))
                      (missiles (missile 10 15 45)
                                (missile 11 15 45)
                                (missile 12 15 45)
                                (missile 13 15 45))))

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
               (make-world (list (make-tank iworld1 4 5 0 100 empty)
                                 (make-tank iworld2 10 30 0 50 empty))
                           (list (make-missile 10 15 45 iworld1)
                                 (make-missile 11 15 45 iworld1)
                                 (make-missile 12 15 45 iworld1)
                                 (make-missile 13 15 45 iworld1))
                           0 0))
              (make-mail iworld1 '(world (tanks (tank "iworld1" 4 5 0 100)
                                                (tank "iworld2" 10 30 0 50))
                                         (missiles (missile 10 15 45)
                                                   (missile 11 15 45)
                                                   (missile 12 15 45)
                                                   (missile 13 15 45)))))
(check-expect (full-world->mail iworld3 (make-world empty empty 0 0))
              (make-mail iworld3 '(world (tanks) (missiles))))



;; new : world iworld -> bundle
; Handles new connections.
(define (new world iworld)
  (let ([new-world (make-world
                    (cons (make-tank iworld 10 10 0 MAX-HEALTH empty)
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
                                                        0 MAX-HEALTH empty))
                                       empty 0 0)
                           (list (make-mail iworld1
                                            `(world (tanks
                                                     (tank "iworld1" 10 10 0
                                                           ,MAX-HEALTH))
                                                    (missiles))))
                           empty))
(check-expect (new (make-world (list (make-tank iworld1 
                                                10 10
                                                0 MAX-HEALTH empty))
                               empty 0 0) iworld2)
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH empty)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH empty))
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
  (let* ([x1 (missile-x missile)]
         [y1 (missile-y missile)]
         [θ (if (= (missile-rot missile) 0)
                0.001 ;; Else we get division by zero errors.
                (missile-rot missile))]
         [x2 (tank-x tank)]
         [y2 (tank-y tank)]
         ;; Raphie made an incorrect assumption with relation to how degrees
         ; work. Specifically, he got it mixed up vertically, because he forgot
         ; that the graphics are flipped. Ideally, he'd fix it on his end, but
         ; the server doesn't deal with the the rotation anywhere, except here.
         ; One thing to change, vs. many, so it gets fixed on the server.
         [m (tan (degrees->radians (abs (- θ 360))))]
         [b1 (- y1 (* m x1))]
         [b2 (- y2 (* (- (/ 1 m)) x2))]
         [x3 (/ (* (- m) (- b1 b2)) (+ (sqr m) 1))]
         [y3 (+ (* m x3) b1)]
         [dist (sqrt (+ (sqr (- x3 x2)) (sqr (- y3 y2))))])
    (begin (when true
             (display (format "~s\n" `(x1 ,x1
                                          y1 ,y1
                                          x2 ,x2
                                          y2 ,y2
                                          θ ,θ
                                          m ,m
                                          m2 ,(- (/ 1 m))
                                          b1 ,b1
                                          b2 ,b2
                                          x3 ,x3
                                          y3 ,y3
                                          dist ,dist))))
           (< dist TANK-SIZE))))


(check-expect (missile-tank-hit? (make-missile -4 2 26 iworld1)
                                 (make-tank iworld1 8 4 30 MAX-HEALTH empty))
              true)
(check-expect (missile-tank-hit? (make-missile -20 5 90 iworld1)
                                 (make-tank iworld1 8 4 30 MAX-HEALTH empty))
              false)
(check-expect (missile-tank-hit?
               (make-missile 53.55122202512186 46.238437389184575 240 iworld1) 
               (make-tank iworld2 10 10 0 50 empty))
              false)


;; handle-missiles : world -> world
; Handles missiles and the destruction of tanks. ;MISSILE-DAMAGE
(define (handle-missiles world)
  (make-world
   (map {λ (tank)
          (let ([hits (foldl {λ (missile acc)
                               (if
                                (and (missile-tank-hit? missile tank)
                                     (not
                                      (iworld=?
                                       (missile-owner missile)
                                       (tank-name tank))))
                                (cons (missile-owner missile) acc)
                                acc)}
                             empty
                             (world-missiles world))])
            (make-tank
             (tank-name tank)
             (tank-x tank)
             (tank-y tank)
             (tank-rot tank)
             (- (tank-health tank)
                (* MISSILE-DAMAGE
                   (length hits)))
             (if (empty? hits)
                 (tank-last-hit tank)
                 (car hits))))} (world-tanks world))
   empty
   (world-start-time world)
   (world-last-tick world)))

(check-expect (handle-missiles (make-world
                                (list (make-tank iworld1 8 4 30 MAX-HEALTH
                                                 empty))
                                (list (make-missile -4 2 26 iworld2)) 0 0))
              (make-world
               (list (make-tank iworld1 8 4 30 (- MAX-HEALTH MISSILE-DAMAGE)
                                iworld2))
               empty 0 0))
(check-expect (handle-missiles (make-world
                                (list (make-tank iworld1 8 4 30 MAX-HEALTH
                                                 empty))
                                (list (make-missile -4 2 26 iworld1)) 0 0))
              (make-world
               (list (make-tank iworld1 8 4 30 MAX-HEALTH empty))
               empty 0 0))
(check-expect (handle-missiles (make-world
                                (list (make-tank iworld1 8 4 30 MAX-HEALTH
                                                 empty))
                                (list (make-missile -4 2 26 iworld2)
                                      (make-missile -10 5 45 iworld2)) 0 0))
              (make-world
               (list (make-tank iworld1 8 4 30 (- MAX-HEALTH MISSILE-DAMAGE)
                                iworld2))
               empty 0 0))

;; reset-dead-tanks : world -> world
; Take all the dead tanks, respawn them, reset health, and reset last-hit.
(define (reset-dead-tanks world)
  (make-world (map
               {λ (tank) (if (<= (tank-health tank) 0)
                             (make-tank (tank-name tank)
                                        10 10 0 MAX-HEALTH empty)
                             tank)}
               (world-tanks world))
              (world-missiles world)
              (world-start-time world)
              (world-last-tick world)))

(check-expect (reset-dead-tanks
               (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                 (make-tank iworld2 0 0 0 100 empty)
                                 (make-tank iworld3 0 0 0 100 empty))
                           empty 0 0))
              (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                (make-tank iworld2 0 0 0 100 empty)
                                (make-tank iworld3 0 0 0 100 empty))
                          empty 0 0))
(check-expect (reset-dead-tanks
               (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                 (make-tank iworld2 0 0 0 0 iworld1)
                                 (make-tank iworld3 0 0 0 100 empty))
                           empty 0 0))
              (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                (make-tank iworld2 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld3 0 0 0 100 empty))
                          empty 0 0))
(check-expect (reset-dead-tanks
               (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                 (make-tank iworld2 0 0 0 0 iworld1)
                                 (make-tank iworld3 0 0 0 -10 iworld2))
                           empty 0 0))
              (make-world (list (make-tank iworld1 0 0 0 100 empty)
                                (make-tank iworld2 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld3 10 10 0 MAX-HEALTH empty))
                          empty 0 0))

;; make-kill-messages : listof[tanks] -> listof[mail]
; Generate kill messages for all the dead tanks.
(define (make-kill-messages tanks)
  (let ([everybody (map tank-name tanks)])
    (foldl
     {λ (tank acc)
       (if (<= (tank-health tank) 0)
           (append (broadcast everybody `(kill ,(iworld-name
                                                 (tank-last-hit tank))
                                               ,(iworld-name
                                                 (tank-name tank))))
                   acc)
           acc)}
     empty
     tanks)))

(check-expect (make-kill-messages
               (list (make-tank iworld1 0 0 0 100 empty)
                     (make-tank iworld2 0 0 0 100 empty)
                     (make-tank iworld3 0 0 0 100 empty)))
              empty)
(check-expect (make-kill-messages
               (list (make-tank iworld1 0 0 0 100 empty)
                                 (make-tank iworld2 0 0 0 0 iworld1)
                                 (make-tank iworld3 0 0 0 100 empty)))
              (broadcast (list iworld1 iworld2 iworld3)
                         '(kill "iworld1" "iworld2")))
(check-expect (make-kill-messages
               (list (make-tank iworld1 0 0 0 100 empty)
                     (make-tank iworld2 0 0 0 0 iworld1)
                     (make-tank iworld3 0 0 0 -10 iworld2)))
              (append
               (broadcast (list iworld1 iworld2 iworld3)
                          '(kill "iworld2" "iworld3"))
               (broadcast (list iworld1 iworld2 iworld3)
                          '(kill "iworld1" "iworld2"))))
                          

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
    (make-bundle (reset-dead-tanks new-world)
                 (append
                  (make-kill-messages (world-tanks new-world))
                  (map {λ (tank)
                        (full-world->mail
                         (tank-name tank) 
                         (make-world (world-tanks new-world)
                                     (world-missiles world)
                                     (world-start-time new-world)
                                     (world-last-tick new-world)))}
                      (world-tanks new-world)))
                 empty)))

(check-expect (tick (make-world empty empty 0 0))
              (make-bundle (make-world empty empty 0 0)
                           empty
                           empty))
(check-expect (tick (make-world (list (make-tank iworld1 
                                                 10 10
                                                 0 MAX-HEALTH empty))
                                empty 0 0))
              (make-bundle
               (make-world (list (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH empty))
                           empty 0 0)
               (list (full-world->mail iworld1 
                                       (make-world
                                        (list (make-tank iworld1 
                                                         10 10
                                                         0 MAX-HEALTH empty))
                                        empty 0 0)))
               empty))
(check-expect (tick (make-world (list 
                                 (make-tank iworld2 
                                            10 10
                                            0 MAX-HEALTH empty)
                                 (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH empty))
                                empty 0 0))
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH empty)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH empty))
                                       empty 0 0)
                           (list (full-world->mail
                                  iworld2
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH empty)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH empty))
                                              empty 0 0))
                                 (full-world->mail
                                  iworld1
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH empty)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH empty))
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
                                                  0 MAX-HEALTH empty))
                                      empty 0 0)
                          iworld1)
              (make-bundle (make-world empty empty 0 0)
                           empty
                           (list iworld1)))
(check-expect (disconnect (make-world (list
                                       (make-tank iworld1 
                                                  10 10
                                                  0 MAX-HEALTH empty)
                                       (make-tank iworld2 
                                                  10 10
                                                  0 MAX-HEALTH empty))
                                      empty 0 0)
                          iworld1)
              (make-bundle (make-world (list
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH empty)) empty
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
                          (tank-health this)
                          (tank-last-hit this))
               other)
         (world-missiles world)
         0 0)
        empty
        empty)]
      [(symbol=? 'shoot message)
       (let ([new-bullet (make-missile (tank-x this)
                                       (tank-y this)
                                       (tank-rot this)
                                       iworld)])
         (make-bundle
          (make-world
           (world-tanks world)
           (cons new-bullet
                 (world-missiles world))
           0 0)
          ; TODO: renable seperate bullet messages?
          empty #;(broadcast (map tank-name other)
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
                                (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld2 10 10 0 MAX-HEALTH empty))
                               empty
                               0 0)
                   iworld1 '(move (coords 40 50) (rot 30)))
              (make-bundle
               (make-world (list
                             (make-tank iworld1 40 50 30 MAX-HEALTH empty)
                            (make-tank iworld2 10 10 0 MAX-HEALTH empty))
                           empty
                           0 0)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld2 10 10 0 MAX-HEALTH empty))
                               empty 0 0)
                   iworld2 '(move (coords 50 60) (rot 45)))
              (make-bundle
               (make-world (list
                            (make-tank iworld2 50 60 45 MAX-HEALTH empty)
                            (make-tank iworld1 10 10 0 MAX-HEALTH empty))
                           empty 0 0)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld2 20 20 0 MAX-HEALTH empty))
                               empty 0 0)
                   iworld2 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                            (make-tank iworld2 20 20 0 MAX-HEALTH empty))
                           (list (make-missile 20 20 0 iworld2)) 0 0)
               ; TODO: renable seperate bullet messages?
               empty #;(list (make-mail iworld1 '(bullet 20 20 0)))
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                                (make-tank iworld2 10 10 0 MAX-HEALTH empty))
                               empty 0 0)
                   iworld1 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH empty)
                            (make-tank iworld2 10 10 0 MAX-HEALTH empty))
                           (list (make-missile 10 10 0 iworld1)) 0 0)
               ; TODO: renable seperate bullet messages?
               empty #;(list (make-mail iworld2 '(bullet 10 10 0)))
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

(if ENABLE-TESTS
    (test)
    (r))
