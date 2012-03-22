;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-family-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #11: Family Trees
; Andrew Amis
; 3.20.12
; http://fellowhuman.com/gbk/2012/03/21/asg-family-trees/



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

(define margaret (make-person "Margaret Amis" 1970 'right shela john))
(define george (make-person "George Amis" 1940 'right 'unknown 'unknown))

(define andrew (make-person "Andrew Amis" 1990 'right margaret george))


(define valor (make-person "Valor Smith" 1899 'right 'unknown 'unknown))
(define prudence (make-person "Prudence Yter" 1900 'left 'unknown 'unknown))
(define andy (make-person "Andy Yter" 1974 'left prudence valor))

(define mora (make-person "Mora Baker" 1900 'left 'unknown 'unknown))
(define excitus (make-person "Excitus Tennyson" 1900 'left 'unknown 'unknown))
(define dolores (make-person "Dolores Baker"  1974 'right mora excitus))

(define victor (make-person "Victor Yter" 1993 'right dolores andy))

(define george2 (make-person "George Amis" 1940 'left 'unknown 'unknown))
(define andrew2 (make-person "Andrew Amis" 1990 'right margaret george2))

(define andy2 (make-person "Andy Yter" 1974 'left 'unknown 'unknown))
(define dolores2 (make-person "Dolores Baker"  1974 'right 'unknown 'unknown))
(define victor2 (make-person "Victor Yter" 1993 'right dolores2 andy2))


(define orphan (make-person "Orphan McOrphanOrphan"
                             1993 'right 'unknown 'unknown))


;; unknown? : Anything -> Boolean
;; For convenience: tests if the given argument is the symbol 'unknown
(define (unknown? a) (and (symbol? a) (symbol=? a 'unknown)))

#;(define (fun-for-ancestor person)
    (cond
      [(unknown? person) ...]
      [else
       (person-name person)
       (person-birth-year person)
       (person-hand person)
       (fun-for-ancestor (person-mother person))
       (fun-for-ancestor (person-father person))]))
#;(define (fun-for-ancestor person)
    (cond
      [(unknown? person) 'unknown]
      [else
       (make-person (person-name person)
                    (person-birth-year person)
                    (person-hand person)
                    (fun-for-ancestor (person-mother person))
                    (fun-for-ancestor (person-father person)))]))

;; earliest-ancestor : person -> number
; Returns the earliest date of birth in that person's family tree


#;(check-expect (earliest-ancestor victor) 1899)
#;(check-expect (earliest-ancestor andrew) 1900)

;; count-known-family : ancestor -> number
; Counts the number of people in the family of the given person.
(define (count-known-family person)
  (cond
    [(unknown? person) 0]
    [else
     (+ 1 
        (count-known-family (person-mother person))
        (count-known-family (person-father person)))]))

(check-expect (count-known-family victor) 7)
(check-expect (count-known-family andrew) 5)

;; left-handed? : person -> boolean
; Returns true if the given person is left handed.
(define (left-handed? p)
  (and (symbol? (person-hand p))
       (symbol=? (person-hand p) 'left)))

(check-expect (left-handed? andy) true)
(check-expect (left-handed? mora) true)
(check-expect (left-handed? andrew) false)
(check-expect (left-handed? victor) false)

;; left-handed-ancestor? : ancestor -> boolean
; Returns whether the ancestor has ancestors that were left handed.
(define (left-handed-ancestor? person)
    (cond
      [(unknown? person) false]
      [else
       (or (left-handed? person)
            (left-handed-ancestor? (person-mother person))
            (left-handed-ancestor? (person-father person)))]))

(check-expect (left-handed-ancestor? victor) true)
(check-expect (left-handed-ancestor? andrew) false)

;; left-handed/both-sides? : person -> boolean
; Returns true if the person has lefthandedness on both sides of their family.
(define (left-handed/both-sides? person)
  (and (left-handed-ancestor? (person-mother person))
       (left-handed-ancestor? (person-father person))))
(check-expect (left-handed/both-sides? victor) true)
(check-expect (left-handed/both-sides? andrew) false)
(check-expect (left-handed/both-sides? andrew2) false)


;; number-of-generations : ancestor -> boolean
; Returns the number of generations in the family tree
(define (number-of-generations person)
  (cond
    [(unknown? person) 0]
    [else
     (+ 1
        (max (number-of-generations (person-mother person))
             (number-of-generations (person-father person))))]))
(check-expect (number-of-generations victor) 3)
(check-expect (number-of-generations andrew) 3)
(check-expect (number-of-generations victor2) 2)
(check-expect (number-of-generations orphan) 1)

;; Al 1923 L
;; Betty 1926 R
;; Carl 1955 L from Al & Betty
;; Irene 1930 R from ??? and ???
;; Dolores 1953 R from ??? and Irene
;; Elwood 1976 L from Carl & Dolores
;; Jake 1979 R from Carl & Dolores


(define Al (make-person "Al" 1923 'left 'unknown 'unknown))
(define Betty (make-person "Betty" 1926 'right 'unknown 'unknown))
(define Carl (make-person "Carl" 1955 'left Betty Al))

(define Irene (make-person "Irene" 1930 'right 'unknown 'unknown))
(define Dolores (make-person "Dolores" 1953 'right Irene 'unknown))

(define Elwood (make-person "Elwood" 1976 'left Dolores Carl))
(define Jake   (make-person "Jake" 1979 'right Dolores Carl))

;; update-father : person person -> person
; Changes the given person, making their father the second person given.
(define (update-father person father)
  (make-person (person-name person)
               (person-birth-year person)
               (person-hand person)
               (person-mother person)
               father))
(check-expect (update-father Jake Al)
              (make-person "Jake" 1979 'right Dolores Al))

;; list-known-family : person -> list
; Returns a list of all known people in the family.
(define (list-known-family person)
  (cond
    [(unknown? person) empty]
    [else
     (cons person 
           (append (list-known-family (person-mother person))
                   (list-known-family (person-father person))))]))

(check-expect (list-known-family Al) (list Al))
(check-expect (list-known-family Jake) (list Jake Dolores Irene Carl Betty Al))