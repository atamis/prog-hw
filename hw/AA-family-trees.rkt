;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-family-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #10: Family Trees
; Andrew Amis
; 3.20.12
; http://fellowhuman.com/gbk/2012/03/20/asg-family-trees/



;; An Ancestor is one of:
;;   - the symbol 'unknown
;;   - (make-person string num+ hand Ancestor Ancestor)
(define-struct person (name birth-year hand mother father))
;; where name is the person's name,
;;       birth-year is the number of the year in which the person was born,
;;       hand is either 'left or 'right, representing their dominant hand,
;;       and mother and father are the person's parents.


(define shela (make-person "Shela Goldsborrow" 1900 'right 'unknown 'unknown))
(define john (make-person "John Goldsborrow" 1900 'right 'unknown 'unknown))

(define margaret (make-person "Margaret Amis" 1900 'right shela john))
(define george (make-person "George Amis" 1900 'right 'unknown 'unknown))

(define andrew (make-person "Andrew Amis" 1900 'right margaret george))


(define valor (make-person "Valor Smith" 1900 'right 'unknown 'unknown))
(define prudence (make-person "Prudence Yter" 1900 'left 'unknown 'unknown))
(define andy (make-person "Andy Yter" 1974 'left prudence valor))

(define mora (make-person "Mora Baker" 1900 'left 'unknown 'unknown))
(define excitus (make-person "Excitus Tennyson" 1900 'left 'unknown 'unknown))
(define dolores (make-person "Dolores Umbrella"  1974 'right mora excitus))

(define victor (make-person "Victor Yter" 1993 'right dolores andy))
  
#;(define (fun-for-ancestor person)
    (cond
      [(symbol=? person 'unknown) ...]
      [else
       (person-name person)
       (person-birth-year person)
       (person-hand person)
       (fun-for-ancestor (person-mother person))
       (fun-for-ancestor (person-father person))]))