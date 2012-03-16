;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Snake
; Andrew Amis
; 3.15.12

; Warning on randomness:
; This game has randomness in it. However, the randomness is integrated such
; that it would be incredibly difficult to properly encapsulate the randomness
; and test the game while the randomness remained. As such, I included a
; constant called RANDOM? to enable and disable randomness. When set to false,
; the game will use preset defaults for the random values. Food will always
; appear at 50, 50. Several tests will only pass under these circumstances. You
; can play the game with randomness disabled, but it makes it significantly less
; fun. I recomend setting RANDOM? to false. If you'd like to see all the tests
; pass, set RANDOM to false. This game ships with randomness disabled to allow
; proper testing.

(require picturing-programs)


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

;; General constants
(define WIDTH 100)
(define HEIGHT 100)
(define WORLD-SCALE 3)


;; View constants
(define BLOCK-SIZE 1)
(define SNAKE-COLOR 'black)
(define ARROW-COLOR 'black)
(define FOOD-COLOR 'orange)
(define TEXT-COLOR 'black)
(define TEXT-SIZE 12)

;; Control constants
(define move-right "right")
(define move-left "left")
(define move-up "up")
(define move-down "down")
(define RANDOM? false) ; This breaks tests.
(define DEFAULT-FOOD (make-posn 50 50))


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


;; scale-posn : number posn -> posn
; Multiplies the x and the y by the provided number
(define (scale-posn n posn)
  (make-posn (* n (posn-x posn))
             (* n (posn-y posn))))

(check-expect (scale-posn 3 (make-posn 2 5)) (make-posn 6 15))
(check-expect (scale-posn 10 (make-posn 2 5)) (make-posn 20 50))
(check-expect (scale-posn -1 (make-posn 2 5)) (make-posn -2 -5))


;; remove-last : NElist -> list
; Removes the last item in the list.
(define (remove-last lst)
  (cond
    [(empty? (cdr lst)) empty]
    [(cons? (cdr lst)) (cons (car lst) (remove-last (cdr lst)))]))
(check-expect (remove-last '(1 2 3 4 5 6 7)) '(1 2 3 4 5 6))
(check-expect (remove-last '(a b c d e)) '(a b c d))

;; posn=? : posn posn -> boolean.
; Checks to see if 2 posns are equal.
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
(check-expect (posn=? (make-posn 0 0) (make-posn 0 0)) true)
(check-expect (posn=? (make-posn 0 0) (make-posn 1 0)) false)


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

;; dir is either
;     'up
;     'down
;     'left
;     'right
; representing direction in an x-y plane in relation to the the screen

;; Template
#;(define (fun-for-dir dir)
    (cond
      [(symbol=? 'up) ...]
      [(symbol=? 'down) ...]
      [(symbol=? 'left) ...]
      [(symbol=? 'right) ...]))

;; dir->posn : dir -> posn
; Converts a dir to a posn-vector of magnitude 1.
(define (dir->posn dir)
  (cond
    [(symbol=? dir 'up) (make-posn 0 -1)]
    [(symbol=? dir 'down) (make-posn 0 1)]
    [(symbol=? dir 'left) (make-posn -1 0)]
    [(symbol=? dir 'right) (make-posn 1 0)]))

(check-expect (dir->posn 'up) (make-posn 0 -1))
(check-expect (dir->posn 'down) (make-posn 0 1))
(check-expect (dir->posn 'left) (make-posn -1 0))
(check-expect (dir->posn 'right) (make-posn 1 0))

;; dir-opposite? : dir dir -> boolean
; Checks whether 2 directions are opposite each other. i.e.: left and right, up
; and down
(define (dir-opposite? dir1 dir2)
  (let ([p1 (dir->posn dir1)]
        [p2 (dir->posn dir2)])
        (posn=? (make-posn 0 0) (add-posns p1 p2))))
(check-expect (dir-opposite? 'right 'left) true)
(check-expect (dir-opposite? 'left 'right) true)
(check-expect (dir-opposite? 'up 'down) true)
(check-expect (dir-opposite? 'down 'up) true)
(check-expect (dir-opposite? 'left 'up) false)
(check-expect (dir-opposite? 'left 'down) false)
(check-expect (dir-opposite? 'right 'down) false)
(check-expect (dir-opposite? 'right 'up) false)
(check-expect (dir-opposite? 'down 'right) false)
(check-expect (dir-opposite? 'down 'left) false)
(check-expect (dir-opposite? 'up 'left) false)
(check-expect (dir-opposite? 'up 'right) false)

;; world is either...
; a struct representing the game state
(define-struct world (snake dir food))
;  snake : list of posns representing the position of the segments of the snake
;  dir : direction the snake is heading in
;  food : location of food on the board
; or false, representing a world ready to quit.

(define world1 (make-world (list (make-posn 50 50)
                                 (make-posn 50 51)
                                 (make-posn 50 52))
                           'up
                           (list (make-posn 60 60))))

;; Template
#;(define (fun-for-world world)
    (world-snake world)
    (world-dir world)
    (world-food world))

#;(define (fun-for-world world)
    (make-world (world-snake world)
                (world-dir world)
                (world-food world)))



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



;; handle-keyboard : world key-event -> world
; Handles keyboard events.
(define (handle-keyboard world key)
  (if (or (key=? key "escape")
          (key=? key "q"))
      false
      (make-world
       (world-snake world)
       (if (or (key=? key move-right)
               (key=? key move-left)
               (key=? key move-up)
               (key=? key move-down))
           (cond
             [(dir-opposite? (string->symbol key) (world-dir world))
              (world-dir world)]
             [(key=? key move-right) 'right]
             [(key=? key move-left) 'left]
             [(key=? key move-down) 'down]
             [(key=? key move-up) 'up])
           (world-dir world))
       (world-food world))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               move-up)
              (make-world (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'up
                          (list (make-posn 60 60))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               move-left)
              (make-world (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'left
                          (list (make-posn 60 60))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               move-right)
              (make-world (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'right
                          (list (make-posn 60 60))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               move-down)
              (make-world (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'up
                          (list (make-posn 60 60))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               "d")
              (make-world (list
                           (make-posn 50 50)
                           (make-posn 50 51)
                           (make-posn 50 52))
                          'up
                          (list (make-posn 60 60))))
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               "escape")
              false)
(check-expect (handle-keyboard (make-world (list
                                            (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 52))
                                           'up
                                           (list (make-posn 60 60)))
                               "q")
              false)



;; move-snake : listof[posn] dir -> listof[posn]
; Moves the snake according to the given direction.
(define (move-snake snake dir)
  (cons (add-posns (car snake) (dir->posn dir))
        (remove-last snake)))
(check-expect (move-snake (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'up)
              (list (make-posn 50 49)
                    (make-posn 50 50)
                    (make-posn 50 51)))
(check-expect (move-snake (list (make-posn 50 49)
                                (make-posn 50 50)
                                (make-posn 50 51))
                          'up)
              (list (make-posn 50 48)
                    (make-posn 50 49)
                    (make-posn 50 50)))
(check-expect (move-snake (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'left)
              (list (make-posn 49 50)
                    (make-posn 50 50)
                    (make-posn 50 51)))

;; food-collision? : posn listof[posn] -> boolean
; Checks if the first posn (the snake's head) is ready to eat on of the food,
; the second argument, the list of posns.
(define (food-collision head lst)
  (ormap (lambda (food) (posn=? head food)) lst))
(check-expect (food-collision (make-posn 0 0) (list (make-posn 0 0)
                                                    (make-posn 0 1)
                                                    (make-posn 50 50)))
              true)
(check-expect (food-collision (make-posn 10 10) (list (make-posn 0 0)
                                                      (make-posn 0 1)
                                                      (make-posn 50 50)))
              false)
(check-expect (food-collision (make-posn 10 10) empty)
              false)
(check-expect (food-collision (make-posn 10 10) (list (make-posn 60 60)))
              false)
(check-expect (food-collision (make-posn 60 60) (list (make-posn 60 60)))
              true)

;; cull-food : posn listof[posn] -> listof[posn]
; Removes the food eaten by the snake
(define (cull-food head foods)
  (filter (lambda (food) (not (posn=? head food))) foods))
(check-expect (cull-food (make-posn 0 0) (list (make-posn 0 0)
                                               (make-posn 0 1)
                                               (make-posn 50 50)))
              (list (make-posn 0 1)
                    (make-posn 50 50)))
(check-expect (cull-food (make-posn 10 10) (list (make-posn 1 0)
                                                 (make-posn 0 1)
                                                 (make-posn 50 50)))
              (list (make-posn 1 0)
                    (make-posn 0 1)
                    (make-posn 50 50)))
(check-expect (cull-food (make-posn 60 60) (list (make-posn 60 60)))
              empty)

;; add-more-food : listof[posn] -> listof[posn]
; Adds food to the list of food provided
(define (add-more-food foods)
  (cons (if RANDOM?
            (make-posn (random WIDTH)
                       (random HEIGHT))
            DEFAULT-FOOD)
        foods))
      
(check-expect (add-more-food (list (make-posn 10 10)))
                             (list DEFAULT-FOOD
                                   (make-posn 10 10)))

;; expand-snake : listof[posn] dir -> listof[posn]
; Adds another body segment to the snake.
(define (expand-snake snake dir)
  (cons (add-posns (car snake) (dir->posn dir))
        snake))
(check-expect (expand-snake (list (make-posn 50 50)) 'up)
              (list (make-posn 50 49) (make-posn 50 50)))

;; handle-food : world -> world
; Finds food collision, counts food collision, extends snakes, adds more food
; elsewhere.
(define (handle-food world)
  (if (food-collision (car (world-snake world)) (world-food world))
      (make-world (expand-snake (world-snake world) (world-dir world))
                  (world-dir world)
                  (add-more-food
                   (cull-food (car (world-snake world)) (world-food world))))
      world))

(check-expect (handle-food (make-world (list (make-posn 60 60)
                                             (make-posn 60 61)
                                             (make-posn 60 62))
                                       'up
                                       (list (make-posn 60 60))))
              (make-world (list (make-posn 60 59)
                                (make-posn 60 60)
                                (make-posn 60 61)
                                (make-posn 60 62))
                          'up
                          (list DEFAULT-FOOD)))

;; snake-collide-snake? : listof[posn] -> boolean
; Checks whether the snake collides with itself. Expensive operation.
(define (snake-collide-snake? snake)
  (cond
    [(empty? snake) false]
    [(cons? snake)
     (or
      (ormap (lambda (posn) (posn=? (car snake) posn)) (cdr snake))
         (snake-collide-snake? (cdr snake)))]))

(check-expect (snake-collide-snake?
               (list (make-posn 0 0)))
              false)
(check-expect (snake-collide-snake?
               (list (make-posn 0 0)
                     (make-posn 0 1)))
              false)
(check-expect (snake-collide-snake?
               (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 1 0)))
              false)
(check-expect (snake-collide-snake?
               (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 0 1)
                     (make-posn 0 0)))
              true)
(check-expect (snake-collide-snake?
               (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 0 1)
                     (make-posn 1 1)))
              true)

;; snake-collide-walls? : listof[posn] -> boolean
; Checks whether the snake has collided with the walls.
(define (snake-collide-walls? snake)
  (let ([x (posn-x (car snake))]
        [y (posn-y (car snake))])
    (or
     (not (<= 0 x WIDTH))
     (not (<= 0 y HEIGHT)))))
(check-expect (snake-collide-walls? (list (make-posn 10 10))) false)
(check-expect (snake-collide-walls? (list (make-posn -1 10))) true)
(check-expect (snake-collide-walls? (list (make-posn -1 -1))) true)
(check-expect (snake-collide-walls? (list (make-posn 10 -1))) true)
(check-expect (snake-collide-walls? (list (make-posn (add1 WIDTH) 3))) true)
(check-expect (snake-collide-walls? (list (make-posn 3 (add1 HEIGHT)))) true)
(check-expect (snake-collide-walls? (list (make-posn (add1 WIDTH)
                                                     (add1 HEIGHT)))) true)

;; snake-collide? : listof[posn] -> boolean
; Checks whether the snake collides with anything.
(define (snake-collide? snake)
  (or (snake-collide-walls? snake)
      (snake-collide-snake? snake)))
(check-expect (snake-collide? (list (make-posn 10 10))) false)
(check-expect (snake-collide? (list (make-posn -1 10))) true)
(check-expect (snake-collide?
               (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 0 1)
                     (make-posn 1 1)))
              true)
(check-expect (snake-collide?
               (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 1 0)))
              false)
;; on-tick : world -> world
; Checks for snake self collision, checks for snake-food collision, moves the
; snake forward.
(define (handle-tick world)
  (if (snake-collide? (world-snake world))
      false
      (handle-food (make-world (move-snake
                                (world-snake world) (world-dir world))
                           (world-dir world)
                           (world-food world)))))

(check-expect (handle-tick (make-world (list (make-posn 50 50)
                                             (make-posn 50 51)
                                             (make-posn 50 52))
                                       'up
                                       (list (make-posn 60 60))))
              (make-world (list (make-posn 50 49)
                                (make-posn 50 50)
                                (make-posn 50 51))
                          'up
                          (list (make-posn 60 60))))
(check-expect (handle-tick (make-world (list (make-posn 60 60)
                                             (make-posn 60 61)
                                             (make-posn 60 62))
                                       'up
                                       (list (make-posn 60 59))))
              (make-world (list (make-posn 60 58)
                                (make-posn 60 59)
                                (make-posn 60 60)
                                (make-posn 60 61))
                          'up
                          (list DEFAULT-FOOD)))
(check-expect (handle-tick (make-world (list (make-posn 60 60)
                                             (make-posn 60 61)
                                             (make-posn 61 61)
                                             (make-posn 61 60)
                                             (make-posn 60 60))
                                       'up
                                       (list (make-posn 30 30))))
              false)

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

(define MT (empty-scene WIDTH HEIGHT))

;; small-square : color -> image
; Draws a very small square of the given color.
(define (small-square c) (square BLOCK-SIZE 'solid c))

(check-expect (small-square 'black) (square BLOCK-SIZE 'solid 'black))
(check-expect (small-square 'red) (square BLOCK-SIZE 'solid 'red))

(define black1 (small-square 'black))
(define red1 (small-square 'red))

;; draw-posns : listof[posn] color image -> image
; Draws the posns onto the image in the given color.
(define (draw-posns lop color img)
  (let ([sq (small-square color)])
    (foldr (lambda (posn image)
             (place-image sq
                          (posn-x posn)
                          (posn-y posn)
                          image))
           img
           lop)))

(check-expect (draw-posns (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'black MT)
              (place-image black1
                           50 50
                           (place-image black1
                                        50 51
                                        (place-image black1
                                                     50 52 MT))))
(check-expect (draw-posns (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52))
                          'red MT)
              (place-image red1
                           50 50
                           (place-image red1
                                        50 51
                                        (place-image red1
                                                     50 52 MT))))



;; draw-food : listof[posn] image -> image
; Draws the food units onto the image
(define (draw-food foods image)
  (draw-posns foods FOOD-COLOR image))
(check-expect (draw-food (list (make-posn 50 50)
                               (make-posn 50 51)
                               (make-posn 50 52)) MT)
              (place-image (small-square FOOD-COLOR)
                           50 50
                           (place-image (small-square FOOD-COLOR)
                                        50 51
                                        (place-image (small-square FOOD-COLOR)
                                                     50 52 MT))))

;; draw-snake : listof[posn] image -> image
; Draws the snake onto the image.
(define (draw-snake foods image)
  (draw-posns foods SNAKE-COLOR image))
(check-expect (draw-snake (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 52)) MT)
              (place-image (small-square SNAKE-COLOR)
                           50 50
                           (place-image (small-square SNAKE-COLOR)
                                        50 51
                                        (place-image (small-square SNAKE-COLOR)
                                                     50 52 MT))))


;; draw-direction : posn dir image -> image
; Draws the direction onto the image at the given location
(define (draw-direction loc dir img)
  (let ([directed-loc (add-posns loc (dir->posn dir))])
    (scene+line img
                (posn-x loc) (posn-y loc)
                (posn-x directed-loc) (posn-y directed-loc)
                ARROW-COLOR)))


(check-expect (draw-direction (make-posn 10 10) 'up MT)
              (scene+line MT
                          10 10
                          10 9
                          ARROW-COLOR))
(check-expect (draw-direction (make-posn 10 10) 'down MT)
              (scene+line MT
                          10 10
                          10 11
                          ARROW-COLOR))
(check-expect (draw-direction (make-posn 10 10) 'right MT)
              (scene+line MT
                          10 10
                          11 10
                          ARROW-COLOR))
(check-expect (draw-direction (make-posn 10 10) 'left MT)
              (scene+line MT
                          10 10
                          9 10
                          ARROW-COLOR))


;; draw-score : listof[posn] image -> image
; Draws the player's score onto the screen.
(define (draw-score snake image)
  (place-image/align (text (number->string (length snake))
                                           TEXT-SIZE TEXT-COLOR)
               2 2
               'left 'top
               image))
(check-expect (draw-score (list (make-posn 50 50)
                                (make-posn 50 51)
                                (make-posn 50 53))
                          MT)
              (place-image/align (text "3" TEXT-SIZE TEXT-COLOR)
               2 2
               'left 'top
               MT))
(check-expect (draw-score (list (make-posn 50 50))
                          MT)
              (place-image/align (text "1" TEXT-SIZE TEXT-COLOR)
               2 2
               'left 'top
               MT))


;; draw-world : world -> image
; Draws the world
(define (draw-world world)
  (scale WORLD-SCALE
         (if (false? world)
             (empty-scene WIDTH HEIGHT 'black)
             (draw-score (world-snake world)
                     (draw-direction (first (world-snake world))
                                     (world-dir world)
                                     (draw-snake (world-snake world)
                                                 (draw-food (world-food world)
                                                            MT)))))))
(check-expect (draw-world (make-world (list (make-posn 50 50)
                                            (make-posn 50 51)
                                            (make-posn 50 53))
                                      'up
                                      (list (make-posn 60 60))))
              (scale WORLD-SCALE (draw-score
                                  (list (make-posn 50 50)
                                        (make-posn 50 51)
                                        (make-posn 50 53))
                                  (draw-direction
                                   (make-posn 50 50) 'up
                                   (draw-snake
                                    (list (make-posn 50 50)
                                          (make-posn 50 51)
                                          (make-posn 50 53))
                                    (draw-food
                                     (list (make-posn 60 60)) MT))))))
(check-expect (draw-world false)
              (scale WORLD-SCALE (empty-scene WIDTH HEIGHT 'black)))


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
  (big-bang (make-world (build-list x (lambda (y) (make-posn 50 (+ y 50))))
                      'up
                      (list (make-posn 60 60)
                            (make-posn 30 30)))
          (on-tick handle-tick)
          (on-key handle-keyboard)
          (stop-when boolean?)
          (to-draw draw-world)))

