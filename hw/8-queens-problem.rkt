#lang racket
;; 8 Queens Problem
; Andrew Amis
; 9.4.12
; http://fellowhuman.com/gbk/2012/09/04/prog-3-asg-2-n-queens/


;; profile : &body -> eval[&body]
; Macro to display the code being run, run the code while timing it, then
; display the result.
(define-syntax-rule (profile x)
  (begin (display (format "Profiling ~s: " 'x))
         (time x)))

(require test-engine/racket-tests
         picturing-programs)

(define-struct posn (x y) #:transparent)

(define (sub-posns p1 p2)
  (make-posn (- (posn-x p2) (posn-x p1))
             (- (posn-y p2) (posn-y p1))))

;; imap : λ{x number -> x} listof[x] -> listof[x]
; Like map but with index
(define (imap fxn lst)
  (letrec [(internal {λ (lst n)
                       (cond
                         [(empty? lst) empty]
                         [(cons? lst)
                          (cons (fxn (car lst) n)
                                (internal (cdr lst) (add1 n)))])})]
    (internal lst 0)))
            
            
; a square is a symbol that is either occupied, threatened, or vacant
; Represented by o, t, and v respectively.

;; a board is a list of lists of squares

(define board1 '((o t t t)
                 (t t v v)
                 (t v t v)
                 (t v v t)))

;; draw-board : board number -> image
; Creates a graphic of the board
(define (draw-board board size)
  (apply above
         (map {λ (row)
                (apply beside
                       (map {λ (sq)
                              (cond
                                [(symbol=? sq 'o)
                                 (square size 'solid 'red)]
                                [(symbol=? sq 't)
                                 (square size 'solid 'green)]
                                [(symbol=? sq 'v)
                                 (square size 'solid 'white)])}
                            row))}
              board)))

(draw-board board1 10)


;; build-board : number λ{number number -> square} -> board
; Builds a chess board of nxn size using the given function to determine square
; content
(define (build-board n fxn)
  (build-list n
              {λ (y)
                (build-list n
                            {λ (x)
                              (fxn x y)})}))

(check-expect (build-board 1 {λ (x y) 'v})
              '((v)))
(check-expect (build-board 4 {λ (x y) 'o})
              '((o o o o)
                (o o o o)
                (o o o o)
                (o o o o)))


;; threatened? : board posn posn -> boolean
; Returns true iff the second position is threatened by the first
(define (threatened? p1 p2)
  (or 
   (= (posn-x p1) (posn-x p2))
   (= (posn-y p1) (posn-y p2))
   (let [(p (sub-posns p1 p2))]
     (= (abs (posn-x p)) (abs (posn-y p))))))
            
            


(check-expect (threatened? (make-posn 0 0) (make-posn 0 1)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 1 0)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 1 1)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn -1 -1)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 1 -1)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn -1 1)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 0 0)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 1 2)) false)

;; queens : board -> listof[posns]
; Finds all the queens on the board
(define (queens board)
  (for/fold [(queens empty)]
    [(row board)
     (y (in-range (length board)))]
    (append
     (for/fold 
         [(posns empty)]
         [(square row)
                (x (in-range (length row)))]
       (if (symbol=? square 'o)
         (cons (make-posn x y) posns)
         posns))
     queens)))
    

(check-expect (queens board1) (list (make-posn 0 0)))
(check-expect (queens (list
                       (list 'o 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'o 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)))
              (list (make-posn 6 6) (make-posn 0 0)))


(define (2nth list x y) (list-ref (list-ref list y) x))
(define (nth= lst n val)
  (cond
    [(= n 0) (cons val (cdr lst))]
    [else
     (cons (car lst) (nth= (cdr lst) (sub1 n) val))]))
(define (2nth= lst x y val)
  (nth= lst y (nth= (list-ref lst y) x val)))

;; recalc-threatened : board -> board
; Recalculate the threatened squares for this board
(define (recalc-threatened b)
  (foldl
   {λ (queen-pos board)
     (imap {λ (row y)
             (for/list ([square row]
                        [x (in-range (length row))])
               (if (symbol=? square 'v)
                   (if (threatened? queen-pos (make-posn x y))
                       't
                       'v)
                   square))
             #;(imap {λ (square x)
                     (if (not (symbol=? square 'o))
                         (if (threatened? queen-pos (make-posn x y))
                             't
                             'v)
                         square)}
                   row)}
           board)}
     b
     (queens b)))

(draw-board (recalc-threatened (list
                       (list 'o 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'o 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v)
                       (list 'v 'v 'v 'v 'v 'v 'v 'v 'v 'v))) 10)




;; placement : n -> board
; Places n queens on an n sized board such that they do not threaten each other.
(define (placement n)
  (letrec [(ifoldl-replacement
            {λ (fxn list n)
              (cond
                [(empty? list) false]
                [else
                 (let [(c (fxn (car list) n))]
                   (if c
                       c
                       (ifoldl-replacement fxn (cdr list) (add1 n))))])})
           (internal {λ (board row-num)
                       ; Iterate through row, on unthreatened squares, place
                       ; queen. Recur with placed queen. If the recur returns
                       ; false, move to the next unthreatened square, and try
                       ; again. If it returns a board, return the board. If
                       ; your row number is n - 1, we're finished, and return
                       ; the board.
                       ; Thanks to Chris S. for help with the algorithm.
                         (if (= row-num n)
                             (recalc-threatened board)
                             (ifoldl-replacement
                              {λ (square x)
                                (if (symbol=? square 'v)
                                    (let [(r (internal
                                              (recalc-threatened
                                               (2nth= board x row-num 'o))
                                              (add1 row-num)))]
                                      r)
                                    false)
                                }
                              (list-ref board row-num)
                              0))
                       })]
    (internal (recalc-threatened (build-board n {λ (x y) 'v})) 0)))

(profile (draw-board (placement 4) 10))
(profile (draw-board (placement 5) 10))
(profile (draw-board (placement 8) 10))

(profile (placement 10))
(test)