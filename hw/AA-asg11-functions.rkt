;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg11-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

;                                                  
;                                                  
;                                                  
;   ;;;;;;           ;;;        ;;           ;;;   
;   ;                  ;       ; ;             ;   
;   ;                  ;      ;; ;             ;   
;   ;;;;;              ;      ;  ;             ;   
;        ;;            ;     ;   ;             ;   
;         ;            ;    ;    ;             ;   
;         ;            ;    ;;;;;;;            ;   
;   ;    ;;   ;;       ;         ;    ;;       ;   
;    ;;;;     ;;     ;;;;;       ;    ;;     ;;;;; 
;                                                  
;                                                  
;                                                  
;                                                  


;; diamond : color number -> image
;; Creates a diamond with that side length and color
(define (diamond color side-length)
  ; color: the color of the rhombus
  ; side-length: a number representing the length of the side of the diamond.
  (rhombus side-length 45 "solid" color))

(check-expect (diamond "green" 30) (rhombus 30 45 "solid" "green"))
(check-expect (diamond "red" 50) (rhombus 50 45 "solid" "red"))


;                                                  
;                                                  
;                                                  
;   ;;;;;;           ;;;        ;;           ;;;;; 
;   ;                  ;       ; ;          ;    ;;
;   ;                  ;      ;; ;                ;
;   ;;;;;              ;      ;  ;                ;
;        ;;            ;     ;   ;               ; 
;         ;            ;    ;    ;             ;;  
;         ;            ;    ;;;;;;;           ;;   
;   ;    ;;   ;;       ;         ;    ;;     ;     
;    ;;;;     ;;     ;;;;;       ;    ;;    ;;;;;;;
;                                                  
;                                                  
;                                                  
;                                                  


;; text-box : string number number string
;; Draws a string in 18 point black font, with a rectangle of the provided
;; height, with the specified color.
(define (text-box string height width color)
  (overlay
   (text string 18 "black")
   (rectangle height width "solid" color)))

(check-expect (text-box "test" 50 100 "yellow")
              (overlay
               (text "test" 18 "black")
               (rectangle 50 100 "solid" "yellow")))
(check-expect (text-box "awesome" 70 400 "green")
              (overlay
               (text "awesome" 18 "black")
               (rectangle 70 400 "solid" "green")))

;                                                  
;                                                  
;                                                  
;   ;;;;;;           ;;;        ;;           ;;;;; 
;   ;                  ;       ; ;          ;     ;
;   ;                  ;      ;; ;                ;
;   ;;;;;              ;      ;  ;               ;;
;        ;;            ;     ;   ;            ;;;  
;         ;            ;    ;    ;               ;;
;         ;            ;    ;;;;;;;               ;
;   ;    ;;   ;;       ;         ;    ;;    ;    ;;
;    ;;;;     ;;     ;;;;;       ;    ;;     ;;;;; 
;                                                  
;                                                  
;                                                  
;                                                  


;; eye : number string -> image
;; Produces an eye with a pupil of 10 pixels with an iris with radius and color
;; specified
(define (eye radius color)
  ; radius: the radius of the iris
  ; color: the color of the iris
  (overlay
   (circle 10 "solid" "black")
   (circle radius "solid" color)))

(check-expect
 (eye 40 "blue")
 (overlay
  (circle 10 "solid" "black")
  (circle 40 "solid" "blue")))
(check-expect
 (eye 30 "green")
 (overlay
  (circle 10 "solid" "black")
  (circle 30 "solid" "green")))

;; spacer : number -> image
;; This adds an invisible spacer of the specified length.
(define (spacer length)
  ; length: the length of the spacer.
  (rectangle length 1 "outline" (make-color 0 0 0 0)))

(check-expect
 (spacer 100)
 (rectangle 100 1 "outline" (make-color 0 0 0 0)))
(check-expect
 (spacer 1000)
 (rectangle 1000 1 "outline" (make-color 0 0 0 0)))

;; two-eyes : number string -> image
;; Produces 2 "eyes" 100 pixels apart.
(define (two-eyes radius color)
  (beside
   (eye radius color)
   (spacer 100)
   (eye radius color)))

(check-expect
 (two-eyes 50 "green")
 (beside
  (eye 50 "green")
  (spacer 100)
  (eye 50 "green")))
(check-expect
 (two-eyes 100 "blue")
 (beside
  (eye 100 "blue")
  (spacer 100)
  (eye 100 "blue")))


;                                                  
;                                                  
;                                                  
;   ;;;;;;           ;;;        ;;              ;; 
;   ;                  ;       ; ;             ; ; 
;   ;                  ;      ;; ;            ;; ; 
;   ;;;;;              ;      ;  ;            ;  ; 
;        ;;            ;     ;   ;           ;   ; 
;         ;            ;    ;    ;          ;    ; 
;         ;            ;    ;;;;;;;         ;;;;;;;
;   ;    ;;   ;;       ;         ;    ;;         ; 
;    ;;;;     ;;     ;;;;;       ;    ;;         ; 
;                                                  
;                                                  
;                                                  
;                                                  


;; circle-in-square : number string string
;; Inscribes a circle within a square.
(define (circle-in-square size square-color circle-color)
  ; size: the size of the square and circle
  ; square-color: the color of the square
  ; circle-color: the color of the circle
  (overlay
   (circle (/ size 2) "solid" circle-color)
   (square size "solid" square-color)))

(check-expect
 (circle-in-square 50 "black" "white")
 (overlay
  (circle 25 "solid" "white")
  (square 50 "solid" "black")))
(check-expect
 (circle-in-square 100 "green" "red")
 (overlay
  (circle 50 "solid" "red")
  (square 100 "solid" "green")))


;                                                  
;                                                  
;                                                  
;   ;;;;;;           ;;;        ;;          ;;;;;; 
;   ;                  ;       ; ;          ;      
;   ;                  ;      ;; ;          ;      
;   ;;;;;              ;      ;  ;          ;;;;;  
;        ;;            ;     ;   ;               ;;
;         ;            ;    ;    ;                ;
;         ;            ;    ;;;;;;;               ;
;   ;    ;;   ;;       ;         ;    ;;    ;    ;;
;    ;;;;     ;;     ;;;;;       ;    ;;     ;;;;  
;                                                  
;                                                  
;                                                  
;                                                  


;; caption-below : image string -> image
;; Adds a caption to the provided image
(define (caption-below image string)
  (above
   image
   (text string 18 "black")))

(check-expect
 (caption-below (square 10 "solid" "black") "A square")
 (above
  (square 10 "solid" "black")
  (text "A square" 18 "black")))

(check-expect
 (caption-below (circle 100 "solid" "blue") "A circle")
 (above
  (circle 100 "solid" "blue")
  (text "A circle" 18 "black")))