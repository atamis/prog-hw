;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname AA-arrangements) (read-case-sensitive #t) (teachpacks ((lib "arrangements-teachpack.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrangements-teachpack.rkt" "installed-teachpacks")))))
;; Prog 1: Rearranging Words
; Andrew Amis
; Started: 3.5.12
; Ended: ?
; http://fellowhuman.com/gbk/2012/03/05/
;        prog-1-extended-exercise-rearranging-words/
; Exercises 167 and 168





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 168 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert-after-letter : 1string word -> list-of-words
; Inserts letters on the inside and at the end of the word
(define 

;; insert-in-word : 1string word -> list-of-words
; Inserts the 1string at the front of, behind, and in the middle of the given
; word
(define (insert-in-word str word)
  (cond
    [(empty? word) (list (append word (list str)))]
    #;[(empty? (car word)) (list (append word (list str)))]
    [(cons? word)
     (append (insert-in-word str (cdr word))
            
    #;[(= 1 (length word)) `(,(append (list str) word)
                             ,(append word (list str)))]
    #;[(= 2 (length word)) `(,(append (list str) word)
                             ,(append (list (car word)) (list str) (cdr word))
                             ,(append word (list str)))]))


(check-expect (insert-in-word "d" empty) empty)
(check-expect (insert-in-word "d" '("a")) '(("d" "a") ("a" "d")))
(check-expect (insert-in-word "d" '("e" "r")) '(("d" "e" "r")
                                                ("e" "d" "r") ("e" "r" "d")))
#;(check-expect (same-word-set?
               (insert-in-word "d" '("e" "r" "t"))
               '(("d" "e" "r" "t")
                 ("e" "d" "r" "t")
                 ("e" "r" "d" "t")
                 ("e" "r" "t" "d")))
              true)



;; insert-everywhere/in-all-words : 1string list-of-words -> list-of-words
; Inserts the 1string in front of, in the middle of, and at the end of, all the
; words in the list.

(define (insert-everywhere/in-all-words str lst)
  (cond
    [(empty? lst) empty]
    [(empty? (car lst)) (list (list str))]
    [(cons? lst)
     (append (insert-in-word str (car lst))
             (insert-everywhere/in-all-words str (cdr lst)))]))

(insert-everywhere/in-all-words "d" '(("a")))

(check-expect (insert-everywhere/in-all-words "d" empty) empty)
(check-expect (insert-everywhere/in-all-words "d" (list empty)) '(("d")))

(check-expect (insert-everywhere/in-all-words "d"
                                              (cons (list "e" "r")
                                                    (cons (list "r" "e")
                                                          empty)))
              '(("d" "e" "r") ("e" "d" "r") ("e" "r" "d")
                              ("d" "r" "e") ("r" "d" "e") ("r" "e" "d")))
(check-expect (insert-everywhere/in-all-words "d" '(("a")))
              '(("d" "a") ("a" "d")))
(check-expect (insert-everywhere/in-all-words "d" '())
              empty)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 167 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; list-of-words is either...
;    empty
;    (cons word list-of-words)
; and represents a list of words


(define word1 (list "a" "d" "d"))
(define word2 (list "o" "d" "d"))
(define list-of-words1 (list word1 word2))

(check-expect (arrangements (list "d" "e"))
              '(("d" "e") ("e" "d")))
(check-expect (same-word-set? (arrangements (list "d" "e" "f"))
                              '(("d" "e" "f") ("d" "f" "e")
                                              ("e" "d" "f") ("e" "f" "d")
                                              ("f" "d" "e") ("f" "e" "d")))
              true)

(define (arrangements w)
  (cond
    [(empty? w) (list empty)]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))