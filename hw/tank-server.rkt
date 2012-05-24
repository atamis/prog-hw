#lang racket

(require test-engine/racket-tests
         picturing-programs)

(define-struct posn (x y)
  #:transparent)


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
;    name: string
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
  `(tank ,(tank-name t)
         ,(tank-x t)
         ,(tank-y t)
         ,(tank-rot t)
         ,(tank-health t)))

(check-expect (tank->sexpr (make-tank "name" 4 5 0 100))
              '(tank "name" 4 5 0 100))
(check-expect (tank->sexpr (make-tank "asdf" 10 30 0 50))
              '(tank "asdf" 10 30 0 50))

;; tanks->sexpr : listof[tank] -> sexpr
; Renders a list of tanks as a sexpr.
(define (tanks->sexpr lot)
  (cons 'tanks (map tank->sexpr lot)))

(check-expect (tanks->sexpr empty)
              '(tanks))
(check-expect (tanks->sexpr (list (make-tank "name" 4 5 0 100)))
              '(tanks (tank "name" 4 5 0 100)))
(check-expect (tanks->sexpr (list (make-tank "name" 4 5 0 100)
                                  (make-tank "asdf" 10 30 0 50)))
              '(tanks (tank "name" 4 5 0 100)
                      (tank "asdf" 10 30 0 50)))

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
(check-expect (world->sexpr (make-world (list (make-tank "name" 4 5 0 100))
                                        (list (make-missile 10 15 45))))
              '(world (tanks (tank "name" 4 5 0 100))
                      (missiles (missile 10 15))))
(check-expect (world->sexpr (make-world (list (make-tank "name" 4 5 0 100)
                                              (make-tank "asdf" 10 30 0 50))
                                        (list (make-missile 10 15 45)
                                              (make-missile 11 15 45)
                                              (make-missile 12 15 45)
                                              (make-missile 13 15 45))))
              '(world (tanks (tank "name" 4 5 0 100)
                             (tank "asdf" 10 30 0 50))
                      (missiles (missile 10 15)
                                (missile 11 15)
                                (missile 12 15)
                                (missile 13 15))))

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
              "(make-world (tanks) (missiles))")

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