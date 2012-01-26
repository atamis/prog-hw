;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AA-asg15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Assignment 12
; Andrew Amis
; 10.17.11
; http://fellowhuman.com/gbk/2011/10/17/prog-1-asg-15-various-world-types/
(require picturing-programs)


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;            ;   
;    ;;    ;   ;         ;   ;          ;;   
;     ;    ;   ;             ;           ;   
;     ;    ;  ;;             ;           ;   
;     ;    ; ; ;            ;            ;   
;     ;    ;;  ;           ;             ;   
;     ;    ;   ;          ;              ;   
;     ;    ;   ;   ;;    ;       ;;      ;   
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

; 18 point green
; a(n*b)
; World is a string.

;; draw-text : world -> image
; Draws the string.
(define (draw-green-text world)
  ; world   | world | "(3, 4)"
  ; returns | image | (text "(3, 4)" 18 ')
  (text
   (if (not (string? world)) 
       (number->string world) world)
   18 'green))
(check-expect (draw-green-text "test") (text "test" 18 'green))
(check-expect (draw-green-text "awesome") (text "awesome" 18 'green))
(check-expect (draw-green-text "!!!!!!") (text "!!!!!!" 18 'green))

;; append-b : string -> string
; Appends a "b" to the end of the string.
(define (append-b string)
  (string-append string "b"))
(check-expect (append-b "test") "testb")
(check-expect (append-b "asdf") "asdfb")
(check-expect (append-b "") "b")

#;(big-bang "a"
            (on-tick append-b 1)
            (to-draw draw-green-text 100 20))


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;           ;;;  
;    ;;    ;   ;         ;   ;         ;   ; 
;     ;    ;   ;             ;             ; 
;     ;    ;  ;;             ;             ; 
;     ;    ; ; ;            ;             ;  
;     ;    ;;  ;           ;             ;   
;     ;    ;   ;          ;             ;    
;     ;    ;   ;   ;;    ;       ;;    ;     
;    ;;;    ;;;    ;;    ;;;;;   ;;    ;;;;; 
;                                            
;                                            
;                                            

;; chop-first-char : string -> string
; Removes the first character of the given string
(define (chop-first-char-on-mouse world x y mouse-event)
  ; string  | string | "test"
  ; returns | string | "est"
  (if (and (mouse=? mouse-event "button-down")
           (not (= (string-length world) 0)))
      (substring world 1)
      world))
(check-expect (chop-first-char-on-mouse "test" 1 1 "button-down") "est")
(check-expect (chop-first-char-on-mouse "awesome" 1 1 "button-down") "wesome")
(check-expect (chop-first-char-on-mouse "blagoyavich" 1 1 "enter") "blagoyavich")

#;(big-bang "a"
            (on-tick append-b 1)
            (on-mouse chop-first-char-on-mouse)
            (to-draw draw-green-text 100 20))


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;           ;;;  
;    ;;    ;   ;         ;   ;         ;   ; 
;     ;    ;   ;             ;             ; 
;     ;    ;  ;;             ;           ;;  
;     ;    ; ; ;            ;              ; 
;     ;    ;;  ;           ;               ; 
;     ;    ;   ;          ;                ; 
;     ;    ;   ;   ;;    ;       ;;    ;   ; 
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

;; last-half : string -> string
; Returns the last half of the string.
; When dealing with odd-lengthed strings, it moves the split to the left.
(define (last-half string)
  ; string  | string | "test"
  ; returns | string | "st"
  (substring string (ceiling (/ (string-length string) 2))))

(check-expect (last-half "test") "st")
(check-expect (last-half "yes") "s")
(check-expect (last-half "there") "re")
(check-expect (last-half "") "")

;; first-half : string -> string
; Returns the first half of the string.
; When dealing with odd-lengthed strings, it moves the split to the left.
(define (first-half string)
  ; string  | string | "test"
  ; returns | string | "te"
  (substring string  0 (ceiling (/ (string-length string) 2))))

(check-expect (first-half "test") "te")
(check-expect (first-half "yes") "ye")
(check-expect (first-half "there") "the")
(check-expect (first-half "") "")

;; add-xyz : string -> string
; Adds xyz to the middle of the string
(define (add-xyz string)
  ; string  | string | "test"
  ; returns | string | "texyzst"
  (string-append (first-half string) "xyz" (last-half string)))
(check-expect (add-xyz "test") "texyzst")
(check-expect (add-xyz "yes") "yexyzs")
(check-expect (add-xyz "yexyzs") "yexxyzyzs")

#;(big-bang "cat"
          (on-tick add-xyz 1)
          (to-draw draw-green-text 200 20))


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;             ;  
;    ;;    ;   ;         ;   ;           ;;  
;     ;    ;   ;             ;          ; ;  
;     ;    ;  ;;             ;         ;  ;  
;     ;    ; ; ;            ;          ;;;;;;
;     ;    ;;  ;           ;              ;  
;     ;    ;   ;          ;               ;  
;     ;    ;   ;   ;;    ;       ;;       ;  
;    ;;;    ;;;    ;;    ;;;;;   ;;       ;  
;                                            
;                                            
;                                            

#;(big-bang 0
          (on-tick add1 1)
          (to-draw draw-green-text 200 20))


;                                            
;                                            
;                                            
;     ;     ;;;           ;;;          ;;;;; 
;    ;;    ;   ;         ;   ;         ;     
;     ;    ;   ;             ;         ;     
;     ;    ;  ;;             ;         ;;;;  
;     ;    ; ; ;            ;              ; 
;     ;    ;;  ;           ;               ; 
;     ;    ;   ;          ;                ; 
;     ;    ;   ;   ;;    ;       ;;    ;   ; 
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

(define MT (empty-scene 100 20))

;; green-text-offset : world -> image
; Prints the world as a string, offset by the world.
(define (green-text-offset number)
  (overlay/align/offset
   "left" "middle"
   (text (number->string number) 13 'green)
   (- number) 0 MT))

(big-bang 0
          (on-tick add1 1)
          (to-draw green-text-offset))

;                                            
;                                            
;                                            
;     ;     ;;;           ;;;            ;;  
;    ;;    ;   ;         ;   ;          ;    
;     ;    ;   ;             ;         ;     
;     ;    ;  ;;             ;         ;;;;  
;     ;    ; ; ;            ;          ;   ; 
;     ;    ;;  ;           ;           ;   ; 
;     ;    ;   ;          ;            ;   ; 
;     ;    ;   ;   ;;    ;       ;;    ;   ; 
;    ;;;    ;;;    ;;    ;;;;;   ;;     ;;;  
;                                            
;                                            
;                                            

;; World is a string

;; handle-mouse : world x y mouse-event -> world
; Converts the x y coordinates into a string to display
(define (handle-mouse w x y mouse-event)
  ; w           | world       | "test"
  ; x           | number      | 3
  ; y           | number      | 4
  ; mouse-event | mouse-event | "blah"
  ; returns     | world       | "(3, 4)"
  (string-append "("
                 (number->string x)
                 ", "
                 (number->string y) ")"))
(check-expect (handle-mouse "this doesn't matter" 3 5 "blah") "(3, 5)")
(check-expect (handle-mouse "this doesn't matter" 10 40 "blah") "(10, 40)")
(check-expect (handle-mouse "this doesn't matter" 141414 44 "blah") "(141414, 44)")

;; draw-text : world -> image
; Draws the string.
(define (draw-text world)
  ; world   | world | "(3, 4)"
  ; returns | image | (text "(3, 4)" 14 'black)
  (text world 14 'black))
(check-expect (draw-text "test") (text "test" 14 'black))
(check-expect (draw-text "awesome") (text "awesome" 14 'black))
(check-expect (draw-text "!!!!!!") (text "!!!!!!" 14 'black))

#;(big-bang ""
            (on-mouse handle-mouse)
            (to-draw draw-text 300 300))