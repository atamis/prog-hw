;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-trees+XML) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #15: Trees/XML
; Andrew Amis
; Started: 4.13.12
; Ended: ?
; http://fellowhuman.com/gbk/2012/04/13/prog-2-asg-web-pages-revisited/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.1.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct parent (children name date eyes))

(define Jim (make-parent empty 'Jim 2000 'brown))

;; Youngest Generation:
(define Gustav (make-parent (list Jim) 'Gustav 1988 'brown))

(define Fred&Eva (list Gustav))

;; Middle Generation:
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Eva (make-parent Fred&Eva 'Eva 1965 'blue))
(define Fred (make-parent Fred&Eva 'Fred 1966 'pink))

(define Carl&Bettina (list Adam Dave Eva))

;; Oldest Generation:
(define Carl (make-parent Carl&Bettina 'Carl 1926 'green))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green))


#| *** Templates *** |#

;; Person -> ???
#;(define (fun-for-person p)
    ... (person-name p)
    ... (person-year p)
    ... (fun-for-list-of-person (person-children p)) ...)

#;(define (fun-for-list-of-person lop)
    (cond
      [(empty? lop) ...]
      [(cons? lop)
       ... (fun-for-person (first lop))
       ... (fun-for-list-of-person (rest lop)) ...]))


;; blue-eyed-descendant? : parent  ->  boolean
;; to determine whether a-parent any of the descendants (children, 
;; grandchildren, and so on) have 'blue in the eyes field
(define (blue-eyed-descendant? a-parent)
  (or (symbol=? (parent-eyes a-parent) 'blue)
      (blue-eyed-children? (parent-children a-parent))))

;; blue-eyed-children? : list-of-children  ->  boolean
;; to determine whether any of the structures in aloc is blue-eyed
;; or has any blue-eyed descendant
(define (blue-eyed-children? aloc)
  (cond
    [(empty? aloc) false]
    [else (or (blue-eyed-descendant? (first aloc))
              (blue-eyed-children? (rest aloc)))]))


(blue-eyed-descendant? Eva)

(or (symbol=? (parent-eyes Eva) 'blue)
    (blue-eyed-children? (parent-children Eva)))

(or (symbol=? (parent-eyes (make-parent Fred&Eva 'Eva 1965 'blue)) 'blue)
    (blue-eyed-children? (parent-children
                          (make-parent Fred&Eva 'Eva 1965 'blue))))

(or (symbol=? 'blue 'blue)
    (blue-eyed-children? (parent-children
                          (make-parent Fred&Eva 'Eva 1965 'blue))))

(or true
    (blue-eyed-children? (parent-children
                          (make-parent Fred&Eva 'Eva 1965 'blue))))

true

(blue-eyed-descendant? Bettina)

(or (symbol=? (parent-eyes Bettina) 'blue)
    (blue-eyed-children? (parent-children Bettina)))

(or (symbol=? (parent-eyes
               (make-parent Carl&Bettina 'Bettina 1926 'green)) 'blue)
    (blue-eyed-children? (parent-children
                          (make-parent Carl&Bettina 'Bettina 1926 'green))))

(or (symbol=? 'green 'blue)
    (blue-eyed-children? (parent-children
                          (make-parent Carl&Bettina 'Bettina 1926 'green))))

(or false
    (blue-eyed-children? (parent-children
                          (make-parent Carl&Bettina 'Bettina 1926 'green))))

(or false
    (blue-eyed-children? Carl&Bettina))

(or false
    (blue-eyed-children? (list Adam Dave Eva)))

(or false
    (blue-eyed-children? (list (make-parent empty 'Adam 1950 'yellow)
                               (make-parent empty 'Dave 1955 'black)
                               (make-parent Fred&Eva 'Eva 1965 'blue))))


(or false
    (cond
      [(empty? (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))) false]
      [else (or (blue-eyed-descendant?
                 (first (list (make-parent empty 'Adam 1950 'yellow)
                              (make-parent empty 'Dave 1955 'black)
                              (make-parent Fred&Eva 'Eva 1965 'blue))))
                (blue-eyed-children?
                 (rest (list (make-parent empty 'Adam 1950 'yellow)
                             (make-parent empty 'Dave 1955 'black)
                             (make-parent Fred&Eva 'Eva 1965 'blue)))))]))

(or false
    (cond
      [false false]
      [else (or (blue-eyed-descendant?
                 (first (list (make-parent empty 'Adam 1950 'yellow)
                              (make-parent empty 'Dave 1955 'black)
                              (make-parent Fred&Eva 'Eva 1965 'blue))))
                (blue-eyed-children?
                 (rest (list (make-parent empty 'Adam 1950 'yellow)
                             (make-parent empty 'Dave 1955 'black)
                             (make-parent Fred&Eva 'Eva 1965 'blue)))))]))

(or false
    (or (blue-eyed-descendant?
         (first (list (make-parent empty 'Adam 1950 'yellow)
                      (make-parent empty 'Dave 1955 'black)
                      (make-parent Fred&Eva 'Eva 1965 'blue))))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (blue-eyed-descendant?
         (make-parent empty 'Adam 1950 'yellow))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or (symbol=? (parent-eyes
                       (make-parent empty 'Adam 1950 'yellow)) 'blue)
            (blue-eyed-children? (parent-children
                                  (make-parent empty 'Adam 1950 'yellow))))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or (symbol=? 'yellow 'blue)
            (blue-eyed-children? (parent-children
                                  (make-parent empty 'Adam 1950 'yellow))))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or false
            (blue-eyed-children? (parent-children
                                  (make-parent empty 'Adam 1950 'yellow))))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or false
            (blue-eyed-children? empty))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or false
            (cond
              [(empty? empty) false]
              [else (or (blue-eyed-descendant? (first empty))
                        (blue-eyed-children? (rest empty)))]))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or false
            (cond
              [true false]
              [else (or (blue-eyed-descendant? (first empty))
                        (blue-eyed-children? (rest empty)))]))
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or (or false
            false)
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or false
        (blue-eyed-children?
         (rest (list (make-parent empty 'Adam 1950 'yellow)
                     (make-parent empty 'Dave 1955 'black)
                     (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or false
        (blue-eyed-children?
         (list (make-parent empty 'Dave 1955 'black)
               (make-parent Fred&Eva 'Eva 1965 'blue)))))

(or false
    (or false
        (cond
          [(empty? (list (make-parent empty 'Dave 1955 'black)
                         (make-parent Fred&Eva 'Eva 1965 'blue))) false]
          [else (or (blue-eyed-descendant?
                     (first
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue))))
                    (blue-eyed-children?
                     (rest
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue)))))])))

(or false
    (or false
        (cond
          [false false]
          [else (or (blue-eyed-descendant?
                     (first
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue))))
                    (blue-eyed-children?
                     (rest
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue)))))])))

(or false
    (or false
        (cond
          [else (or (blue-eyed-descendant?
                     (first
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue))))
                    (blue-eyed-children?
                     (rest
                      (list (make-parent empty 'Dave 1955 'black)
                            (make-parent Fred&Eva 'Eva 1965 'blue)))))])))

(or false
    (or false
        (or (blue-eyed-descendant?
             (first
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue))))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (blue-eyed-descendant?
             (make-parent empty 'Dave 1955 'black))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or (symbol=? (parent-eyes
                           (make-parent empty 'Dave 1955 'black)) 'blue)
                (blue-eyed-children?
                 (parent-children (make-parent empty 'Dave 1955 'black))))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or (symbol=? 'black 'blue)
                (blue-eyed-children?
                 (parent-children (make-parent empty 'Dave 1955 'black))))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or false
                (blue-eyed-children?
                 (parent-children (make-parent empty 'Dave 1955 'black))))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or false
                (blue-eyed-children?
                 empty))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or false
                (cond
                  [(empty? empty) false]
                  [else (or (blue-eyed-descendant? (first empty))
                            (blue-eyed-children? (rest empty)))]))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or false
                (cond
                  [true false]
                  [else (or (blue-eyed-descendant? (first empty))
                            (blue-eyed-children? (rest empty)))]))
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or (or false
                false)
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or false
            (blue-eyed-children?
             (rest
              (list (make-parent empty 'Dave 1955 'black)
                    (make-parent Fred&Eva 'Eva 1965 'blue)))))))

(or false
    (or false
        (or false
            (blue-eyed-children?
             (list (make-parent Fred&Eva 'Eva 1965 'blue))))))

(or false
    (or false
        (or
         false
         (cond
           [(empty?
             (list (make-parent Fred&Eva 'Eva 1965 'blue))) false]
           [else (or
                  (blue-eyed-descendant?
                   (first (list (make-parent Fred&Eva 'Eva 1965 'blue))))
                  (blue-eyed-children?
                   (rest (list (make-parent Fred&Eva 'Eva 1965 'blue)))))]))))

(or false
    (or false
        (or
         false
         (cond
           [false false]
           [else (or
                  (blue-eyed-descendant?
                   (first (list (make-parent Fred&Eva 'Eva 1965 'blue))))
                  (blue-eyed-children?
                   (rest (list (make-parent Fred&Eva 'Eva 1965 'blue)))))]))))

(or false
    (or false
        (or
         false
         (or
          (blue-eyed-descendant?
           (first (list (make-parent Fred&Eva 'Eva 1965 'blue))))
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         (or
          (blue-eyed-descendant?
           (make-parent Fred&Eva 'Eva 1965 'blue))
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         (or
          (or (symbol=? (parent-eyes
                         (make-parent Fred&Eva 'Eva 1965 'blue)) 'blue)
              (blue-eyed-children? (parent-children
                                    (make-parent Fred&Eva 'Eva 1965 'blue))))
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         (or
          (or (symbol=? 'blue 'blue)
              (blue-eyed-children? (parent-children
                                    (make-parent Fred&Eva 'Eva 1965 'blue))))
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         (or
          (or true
              (blue-eyed-children? (parent-children
                                    (make-parent Fred&Eva 'Eva 1965 'blue))))
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         (or
          true
          (blue-eyed-children?
           (rest (list (make-parent Fred&Eva 'Eva 1965 'blue))))))))

(or false
    (or false
        (or
         false
         true)))

(or false
    (or false
        true))

true



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how-far-removed : person -> [number|false]
; determines how far a blue-eyed descendant, if one exists,
; is removed from the given parent. 
(define (how-far-removed parent)
  (letrec ([for-person (λ (parent)
                         (if (symbol=? (parent-eyes parent) 'blue)
                             0
                             (let ([lst (for-list (parent-children parent))])
                               (if (number? lst)
                                   (+ 1 lst)
                                   false))))]
           [for-list (λ (list)
                       (cond
                         [(empty? list) false]
                         [(cons? list)
                          (let ([x (for-person (car list))])
                            (if (number? x)
                                x
                                (for-list (cdr list))))]))])
    (for-person parent)))


(check-expect (how-far-removed Eva) 0)
(check-expect (how-far-removed Carl) 1)
(check-expect (how-far-removed Bettina) 1)
(check-expect (how-far-removed Fred) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.1.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; count-descendants : parent -> number
; Produces the total number of nodes in the tree, or the total number of
; descendents, including the root node/parent.
(define (count-descendants parent)
  (letrec ([for-person (λ (parent)
                         (+ 1 (for-list (parent-children parent))))]
           [for-list (λ (list)
                       (foldl (λ (x lastr)
                                (+ (for-person x) lastr)) 0 list))])
    (for-person parent)))
(check-expect (count-descendants Jim) 1)
(check-expect (count-descendants Gustav) 2)
(check-expect (count-descendants Eva) 3)
(check-expect (count-descendants Bettina) 6)

;; count-proper-descendants : parent -> number
; Counts the number of proper descendents, not including the root parent, in
; this tree
(define (count-proper-descendants parent)
  (- (count-descendants parent) 1))
(check-expect (count-proper-descendants Jim) 0)
(check-expect (count-proper-descendants Gustav) 1)
(check-expect (count-proper-descendants Eva) 2)
(check-expect (count-proper-descendants Bettina) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 15.1.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eye-colors : parent -> listof[symbols]
; Produces a list of symbols for all the eye colors in the given parent tree.
; Colors may appear more than once.
(define (eye-colors parent)
  (letrec ([for-person (λ (parent)
                         (cons (parent-eyes parent)
                               (for-list (parent-children parent))))]
           [for-list (λ (list)
                       (foldl (λ (x lastr)
                                (append (for-person x) lastr)) '() list))])
    (for-person parent)))
(check-expect (eye-colors Jim) '(brown))
(check-expect (eye-colors Gustav) '(brown brown))
(check-expect (eye-colors Eva) '(blue brown brown))
(check-expect (eye-colors Fred) '(pink brown brown))
(check-expect (eye-colors Carl) '(green blue brown brown black yellow))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Xexpr 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;