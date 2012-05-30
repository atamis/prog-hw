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


(define-struct world (tanks missiles)
  #:transparent)


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

(check-expect (world->sexpr (make-world empty empty))
              '(world (tanks) (missiles)))
(check-expect (world->sexpr (make-world (list (make-tank iworld1 4 5 0 100))
                                        (list (make-missile 10 15 45))))
              '(world (tanks (tank "iworld1" 4 5 0 100))
                      (missiles (missile 10 15))))
(check-expect (world->sexpr (make-world (list (make-tank iworld1 4 5 0 100)
                                              (make-tank iworld2 10 30 0 50))
                                        (list (make-missile 10 15 45)
                                              (make-missile 11 15 45)
                                              (make-missile 12 15 45)
                                              (make-missile 13 15 45))))
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
                                 (make-missile 13 15 45))))
              (make-mail iworld1 '(world (tanks (tank "iworld1" 4 5 0 100)
                                                (tank "iworld2" 10 30 0 50))
                                         (missiles (missile 10 15)
                                                   (missile 11 15)
                                                   (missile 12 15)
                                                   (missile 13 15)))))
(check-expect (full-world->mail iworld3 (make-world empty empty))
              (make-mail iworld3 '(world (tanks) (missiles))))



;; new : world iworld -> bundle
; Handles new connections.
(define (new world iworld)
  (let ([new-world (make-world
                    (cons (make-tank iworld 10 10 0 MAX-HEALTH)
                          (world-tanks world))
                    (world-missiles world))])
    (make-bundle
     new-world
     (list (full-world->mail iworld new-world))
     empty)))

(check-expect (new (make-world empty empty) iworld1)
              (make-bundle (make-world (list (make-tank iworld1 
                                                        10 10
                                                        0 MAX-HEALTH))
                                       empty)
                           (list (make-mail iworld1
                                            `(world (tanks
                                                     (tank "iworld1" 10 10 0
                                                           ,MAX-HEALTH))
                                                    (missiles))))
                           empty))
(check-expect (new (make-world (list (make-tank iworld1 
                                                10 10
                                                0 MAX-HEALTH))
                               empty) iworld2)
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH))
                                       empty)
                           (list (make-mail iworld2
                                            `(world (tanks
                                                     (tank "iworld2" 10 10 0
                                                           ,MAX-HEALTH)
                                                     (tank "iworld1" 10 10 0
                                                           ,MAX-HEALTH))
                                                    (missiles))))
                           empty))


;; tick : world -> bundle
; Executed every tick. Sends the world to all players. Also does collision
; detection
(define (tick world)
  (let ([new-world world]) ;; Process world here
    (make-bundle new-world
                 (map {λ (tank)
                        (full-world->mail (tank-name tank) new-world)}
                      (world-tanks new-world))
                 empty)))

(check-expect (tick (make-world empty empty))
              (make-bundle (make-world empty empty)
                           empty
                           empty))
(check-expect (tick (make-world (list (make-tank iworld1 
                                                 10 10
                                                 0 MAX-HEALTH))
                                empty))
              (make-bundle
               (make-world (list (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH))
                           empty)
               (list (full-world->mail iworld1 
                                       (make-world
                                        (list (make-tank iworld1 
                                                         10 10
                                                         0 MAX-HEALTH))
                                        empty)))
               empty))
(check-expect (tick (make-world (list 
                                 (make-tank iworld2 
                                            10 10
                                            0 MAX-HEALTH)
                                 (make-tank iworld1 
                                            10 10
                                            0 MAX-HEALTH))
                                empty))
              (make-bundle (make-world (list 
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)
                                        (make-tank iworld1 
                                                   10 10
                                                   0 MAX-HEALTH))
                                       empty)
                           (list (full-world->mail
                                  iworld2
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH))
                                              empty))
                                 (full-world->mail
                                  iworld1
                                  (make-world (list 
                                               (make-tank iworld2 
                                                          10 10
                                                          0 MAX-HEALTH)
                                               (make-tank iworld1 
                                                          10 10
                                                          0 MAX-HEALTH))
                                              empty)))
                           
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
      (world-missiles world))
     (broadcast (map tank-name new-tanks) `(disconnected ,(iworld-name iworld)))
     (list iworld))))

(check-expect (disconnect (make-world empty
                                      empty)
                          iworld1)
              (make-bundle (make-world empty empty)
                           empty
                           (list iworld1)))
(check-expect (disconnect (make-world (list
                                       (make-tank iworld1 
                                                  10 10
                                                  0 MAX-HEALTH))
                                      empty)
                          iworld1)
              (make-bundle (make-world empty empty)
                           empty
                           (list iworld1)))
(check-expect (disconnect (make-world (list
                                       (make-tank iworld1 
                                                  10 10
                                                  0 MAX-HEALTH)
                                       (make-tank iworld2 
                                                  10 10
                                                  0 MAX-HEALTH))
                                      empty)
                          iworld1)
              (make-bundle (make-world (list
                                        (make-tank iworld2 
                                                   10 10
                                                   0 MAX-HEALTH)) empty)
                           (list (make-mail iworld2 '(disconnected "iworld1")))
                           (list iworld1)))


;; msg : world iworld sexpr -> bundle
; Responds to a message from a client.
(define (msg world iworld message)
  (let ([this (filter {λ (tank) (iworld=? (tank-name tank) iworld)}
                      (world-tanks world))]
        [other (filter {λ (tank) (not (iworld=? (tank-name tank) iworld))}
                       (world-tanks world))])
    (cond
      [(symbol=? (car message) 'move)
       (make-bundle
        (make-world
         (cons (make-tank (tank-name this)
                          (cadadr message) ; x
                          (car (cddadr message)) ; y
                          (car (cdaddr message)) ; rot
                          (tank-health this))
               other)
         (world-missiles world))
        empty
        empty)]
      [(symbol=? (car message 'bullet))
       (let ([new-bullet (make-missile (tank-x tank)
                                       (tank-y tank)
                                       (tank-rot tank))])
         (make-bundle
          (make-world
           (world-tanks world)
           (cons new-bullet
                 (world-missiles world))
           (broadcast other `(bullet (missile-x missile)
                                     (missile-y missile)
                                     (missile-rot missile)))
           empty)))])))

(msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty)
                   iworld1 '(move (coords 40 50) (rot 30)))

(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty)
                   iworld1 '(move (coords 40 50) (rot 30)))
              (make-bundle
               (make-world (list
                            (make-tank iworld1 30 40 30 MAX-HEALTH)
                            (make-tank iworld2 10 10 0 MAX-HEALTH))
                           empty)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty)
                   iworld2 '(move (coords 50 60) (rot 45)))
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH)
                            (make-tank iworld2 50 60 45 MAX-HEALTH))
                           empty)
               empty
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty)
                   iworld2 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH)
                            (make-tank iworld2 10 10 0 MAX-HEALTH))
                           empty)
               (list (make-mail iworld1 '(bullet 10 10 0)))
               empty))
(check-expect (msg (make-world (list
                                (make-tank iworld1 10 10 0 MAX-HEALTH)
                                (make-tank iworld2 10 10 0 MAX-HEALTH))
                               empty)
                   iworld1 'shoot)
              (make-bundle
               (make-world (list
                            (make-tank iworld1 10 10 0 MAX-HEALTH)
                            (make-tank iworld2 10 10 0 MAX-HEALTH))
                           (list (make-missile 10 10 0)))
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

(check-expect (world->string (make-world empty empty))
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


#;(define (r)
    (universe (make-world empty empty)
              (on-new new)
              (on-tick tick)
              (on-disconnect disconnect)
              (on-msg msg)
              (to-string world->string)))

(test)