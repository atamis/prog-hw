;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Andrew Amis
; 10/10/11
; Prog Assignment 13
; http://fellowhuman.com/gbk/2011/10/06/prog-1-asg-13/

(require picturing-programs)


;                                                                                             
;                                                                                             
;                                                                                             
;                                                                  ;                          
;                                                            ;     ;                          
;                                                            ;     ;                          
;    ;;;;  ;   ;   ;;;   ; ;;    ;;;;   ;;;;   ;;;          ;;;    ;;;;   ; ;;    ;;;    ;;;  
;   ;   ;  ;   ;  ;   ;  ;;  ;  ;   ;  ;   ;  ;   ;  ;;;;;   ;     ;   ;  ;;  ;  ;   ;  ;   ; 
;   ;   ;   ; ;   ;;;;;  ;   ;  ;   ;  ;   ;  ;;;;;          ;     ;   ;  ;   ;  ;;;;;  ;;;;; 
;   ;   ;   ; ;   ;      ;      ;   ;  ;   ;  ;              ;     ;   ;  ;      ;      ;     
;   ;  ;;    ;    ;   ;  ;      ;  ;;  ;   ;  ;   ;          ;     ;   ;  ;      ;   ;  ;   ; 
;    ;; ;    ;     ;;;   ;       ;; ;   ;;;;   ;;;            ;;   ;   ;  ;       ;;;    ;;;  
;                                          ;                                                  
;                                      ;   ;                                                  
;                                       ;;;                                                   

; average-three : number number number -> number
; Averages the 3 arguments.
(define (average-three n1 n2 n3)
  ; n1..3  | number | 3, 4, 5
  ; return | number | 4
  (/ (+ n1 n2 n3) 3))
(check-expect (average-three 3 4 5) 4)
(check-within (average-three 10 42 45) 32 1)
(check-within (average-three 4 6 10) 6 1)


;                                     
;                                     
;                                     
;    ;;;           ;;;            ;   
;   ;   ;         ;   ;          ;;   
;   ;   ;             ;           ;   
;    ;;;            ;;            ;   
;   ;   ;             ;           ;   
;   ;   ;             ;           ;   
;   ;   ;             ;           ;   
;   ;   ;   ;;    ;   ;   ;;      ;   
;    ;;;    ;;     ;;;    ;;     ;;;  
;                                     
;                                     
;                                     

;; blue-circle-of-size : number -> image
; Draws a blue circle of the specified radius
(define (blue-circle-of-size radius)
  ; radius  | number | 7
  ; returns | image  | (circle 7 'solid 'blue)
  (circle radius 'solid 'blue))

(check-expect (blue-circle-of-size 10) (circle 10 'solid 'blue))
(check-expect (blue-circle-of-size 20) (circle 20 'solid 'blue))
(check-expect (blue-circle-of-size 30) (circle 30 'solid 'blue))

;(big-bang 7
;          (check-with number?)
;          (on-draw blue-circle-of-size 100 50)
;          (on-tick add1 1/2))


;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;             ; 
;    ;;;            ;;              ; 
;   ;   ;             ;            ;  
;   ;   ;             ;           ;   
;   ;   ;             ;          ;    
;   ;   ;   ;;    ;   ;   ;;    ;     
;    ;;;    ;;     ;;;    ;;    ;;;;; 
;                                     
;                                     
;                                     

(define MT.2 (empty-scene 100 50))

;; blue-circle-of-size : number -> image
; Draws a blue circle of the specified radius
(define (blue-circle-of-size2 radius)
  ; radius  | number | 7
  ; returns | image  | (circle 7 'solid 'blue)
  (overlay/align
   "middle" "middle"
   (circle radius 'solid 'blue)
   MT.2))

(check-expect (blue-circle-of-size2 10) (overlay/align "middle" "middle" (circle 10 'solid 'blue) MT.2))
(check-expect (blue-circle-of-size2 20) (overlay/align "middle" "middle" (circle 20 'solid 'blue) MT.2))
(check-expect (blue-circle-of-size2 30) (overlay/align "middle" "middle" (circle 30 'solid 'blue) MT.2))

;(big-bang 7
;          (check-with number?)
;          (on-draw blue-circle-of-size2)
;          (on-tick add1 1/2))


;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;             ; 
;    ;;;            ;;            ;;  
;   ;   ;             ;             ; 
;   ;   ;             ;             ; 
;   ;   ;             ;             ; 
;   ;   ;   ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;     ;;;    ;;     ;;;  
;                                     
;                                     
;                                     

; Empty scene for 8.3.3
(define MT.3 (empty-scene 100 100))

;; draw-square : number -> image
; Draws a square at the specified x coordinate
(define (draw-square x)
  ; x       | number | 3
  ; returns | image  | (overlay/xy (square 3 'solid 'red) 3 50 MT.3)
  (overlay/xy
   (square 4 'solid 'red)
   (- x) -50
   MT.3))

(check-expect
 (draw-square 10)
 (overlay/xy
  (square 4 'solid 'red)
  -10
  -50
  MT.3))
(check-expect
 (draw-square 30)
 (overlay/xy
  (square 4 'solid 'red)
  -30
  -50
  MT.3))
(check-expect
 (draw-square 34)
 (overlay/xy
  (square 4 'solid 'red)
  -34
  -50
  MT.3))


;(big-bang 0
;          (check-with number?)
;          (on-draw draw-square)
;          (on-tick add1 1/10))


;                                     
;                                     
;                                     
;    ;;;           ;;;             ;  
;   ;   ;         ;   ;           ;;  
;   ;   ;             ;          ; ;  
;    ;;;            ;;          ;  ;  
;   ;   ;             ;         ;;;;;;
;   ;   ;             ;            ;  
;   ;   ;             ;            ;  
;   ;   ;   ;;    ;   ;   ;;       ;  
;    ;;;    ;;     ;;;    ;;       ;  
;                                     
;                                     
;                                     

(define MT.4 (empty-scene 200 200))

;; centered-square : number -> image
; Draws a square of the specified size
(define (centered-square size)
  (overlay/align
   'middle 'middle
   (square size 'solid 'black)
   MT.4))


(check-expect (centered-square 3)
              (overlay/align 'middle 'middle
                             (square 3 'solid 'black)
                             MT.4))
(check-expect (centered-square 10)
              (overlay/align 'middle 'middle
                             (square 10 'solid 'black)
                             MT.4))
(check-expect (centered-square 30)
              (overlay/align 'middle 'middle
                             (square 30 'solid 'black)
                             MT.4))

;(big-bang 1
;          (check-with number?)
;          (to-draw centered-square)
;          (on-tick add1))


;                                     
;                                     
;                                     
;    ;;;           ;;;          ;;;;; 
;   ;   ;         ;   ;         ;     
;   ;   ;             ;         ;     
;    ;;;            ;;          ;;;;  
;   ;   ;             ;             ; 
;   ;   ;             ;             ; 
;   ;   ;             ;             ; 
;   ;   ;   ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;     ;;;    ;;     ;;;  
;                                     
;                                     
;                                     


(define MT.5 (empty-scene 200 200))

;; centered-rectangle : number -> image
; Draws a rectangle of the specified size in the center of a 200x200 field.
(define (centered-rectangle size)
  ; size    | number | 3
  ; returns | image  | (overlay/align 'middle 'middle (rectangle 3 6 'solid 'green) MT.4)
  (overlay/align
   'middle 'middle
   (rectangle size (* 2 size) 'solid 'green)
   MT.4))


(check-expect (centered-rectangle 3)
              (overlay/align 'middle 'middle
                             (rectangle 3 6 'solid 'green)
                             MT.4))
(check-expect (centered-rectangle 10)
              (overlay/align 'middle 'middle
                             (rectangle 10 20 'solid 'green)
                             MT.4))
(check-expect (centered-rectangle 30)
              (overlay/align 'middle 'middle
                             (rectangle 30 60 'solid 'green)
                             MT.4))

;(big-bang 1
;          (check-with number?)
;          (to-draw centered-rectangle)
;          (on-tick add1))


;                                     
;                                     
;                                     
;    ;;;           ;;;            ;;  
;   ;   ;         ;   ;          ;    
;   ;   ;             ;         ;     
;    ;;;            ;;          ;;;;  
;   ;   ;             ;         ;   ; 
;   ;   ;             ;         ;   ; 
;   ;   ;             ;         ;   ; 
;   ;   ;   ;;    ;   ;   ;;    ;   ; 
;    ;;;    ;;     ;;;    ;;     ;;;  
;                                     
;                                     
;                                     

;; Empty scene for 8.3.6
(define MT.6 (empty-scene 200 200))

;; y-coord number -> number
; Raises argument to the 2nd power then devides by 20.
(define (y-coord n)
  ; n       | number | 10
  ; returns | number | 5
  (/ (expt n 2) 20))
(check-expect (y-coord 3) 0.45)
(check-expect (y-coord 10) 5)
(check-expect (y-coord 9) 4.05)

;; moving-dot : number -> image
; Draws a dot at x=argument and y=argument^2/20
(define (moving-dot x)
  ; x       | number | 10
  ; returns | image  | (place-image (circle 2 'solid 'blue) 10 (/ (expt 10 2)
  ;                                 20) MT.6)
  (place-image
   (circle 2 'solid 'blue)
   x (y-coord x)
   MT.6))

(check-expect (moving-dot 10) (place-image
                               (circle 2 'solid 'blue)
                               10 (y-coord 10)
                               MT.6))
(check-expect (moving-dot 20) (place-image
                               (circle 2 'solid 'blue)
                               20 (y-coord 20)
                               MT.6))
(check-expect (moving-dot 30) (place-image
                               (circle 2 'solid 'blue)
                               30 (y-coord 30)
                               MT.6))
;(big-bang 0
;          (check-with number?)
;          (to-draw moving-dot)
;          (on-tick add1))


;                                     
;                                     
;                                     
;    ;;;           ;;;          ;;;;; 
;   ;   ;         ;   ;             ; 
;   ;   ;             ;             ; 
;    ;;;            ;;             ;  
;   ;   ;             ;           ;   
;   ;   ;             ;           ;   
;   ;   ;             ;           ;   
;   ;   ;   ;;    ;   ;   ;;      ;   
;    ;;;    ;;     ;;;    ;;      ;   
;                                     
;                                     
;                                     
; And 8.3.8, because I am not rewriting this stuff.

(define XCENTER 100) ; default, 100
(define YCENTER 100) ; 100
(define XSCALE   50) ; 50
(define YSCALE   30) ; 30
(define XDIV   1/10) ; 1/10
(define YDIV   1/10) ; 1/10

;; Empty scene for 8.3.7
(define MT.7 (empty-scene 200 200))

;; wavy-y-coord number -> number
; 100 + 30 sin(t/10)
(define (wavy-y-coord y)
  ; y       | number | 3
  ; returns | number | #i108.86560619984019
  (+ YCENTER (* YSCALE (sin (* y YDIV)))))

(check-within (wavy-y-coord 3) 108 1)
(check-within (wavy-y-coord 10) 125 1)
(check-within (wavy-y-coord 9) 123 1)

;; wavy-x-coord : number -> number
; 100 + 50 cos(t/10)
(define (wavy-x-coord x)
  ; x       | number | 3
  ; returns | number | #i147.7668244562803
  (+ XCENTER (* XSCALE (cos (* x XDIV)))))

(check-within (wavy-x-coord 3) 147 1)
(check-within (wavy-x-coord 10) 127 1)
(check-within (wavy-x-coord 100) 58 1)

;; moving-dot : number -> image
; Draws a dot at at a point specified by the functions wavy-x-coord and
; wavy-y-coord
(define (wavy-moving-dot x)
  ; x       | number | 10
  ; returns | image  | (place-image (circle 2 'solid 'blue) (wavy-x-coord 10)
  ;                                 (wavy-y-coord 10) MT.7)
  (place-image
   (circle 2 'solid 'blue)
   (wavy-x-coord x) (wavy-y-coord x)
   MT.7))

(check-expect (wavy-moving-dot 10)
              (place-image
               (circle 2 'solid 'blue)
               (wavy-x-coord 10) (wavy-y-coord 10)
               MT.7))
(check-expect (wavy-moving-dot 20)
              (place-image
               (circle 2 'solid 'blue)
               (wavy-x-coord 20) (wavy-y-coord 20)
               MT.7))
(check-expect (wavy-moving-dot 30)
              (place-image
               (circle 2 'solid 'blue)
               (wavy-x-coord 30) (wavy-y-coord 30)
               MT.7))

;(big-bang 0
;          (check-with number?)
;          (to-draw wavy-moving-dot)
;          (on-tick add1))


;                                     
;                                     
;                                     
;    ;;;           ;;;           ;;;  
;   ;   ;         ;   ;         ;   ; 
;   ;   ;             ;         ;   ; 
;    ;;;            ;;          ;   ; 
;   ;   ;             ;         ;   ; 
;   ;   ;             ;          ;;;; 
;   ;   ;             ;             ; 
;   ;   ;   ;;    ;   ;   ;;       ;  
;    ;;;    ;;     ;;;    ;;     ;;   
;                                     
;                                     
;                                     

;; progress-bar : width -> image
; Draws a progress bar width pixels wide
(define (progress-bar width)
  (overlay/align
   'left
   'middle
   (rectangle (min width 120) 20 'solid 'blue)
   (rectangle 120 20 'outline 'black)))

(check-expect
 (progress-bar 20)
 (overlay/align 'left 'middle
                (rectangle 20 20 'solid 'blue)
                (rectangle 120 20 'outline 'black)))
(check-expect
 (progress-bar 55)
 (overlay/align 'left 'middle
                (rectangle 55 20 'solid 'blue)
                (rectangle 120 20 'outline 'black)))
(check-expect
 (progress-bar 76)
 (overlay/align 'left 'middle
                (rectangle 76 20 'solid 'blue)
                (rectangle 120 20 'outline 'black)))

(big-bang 0
          (check-with number?)
          (to-draw progress-bar)
          (on-tick add1 1/4))