;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-family-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Abstractions for ancestor

;; An Ancestor is one of:
;;   - the symbol 'unknown
;;   - (make-person string num+ hand Ancestor Ancestor)
(define-struct person (name birth-year hand mother father))
;; where name is the person's name,
;;       birth-year is the number of the year in which the person was born,
;;       hand is either 'left or 'right, representing their dominant hand,
;;       and mother and father are the person's parents.

;; unknown? : Anything -> Boolean
;; For convenience: tests if the given argument is the symbol 'unknown
(define (unknown? a) (and (symbol? a) (symbol=? a 'unknown)))

(define valor (make-person "Valor Smith" 1899 'right 'unknown 'unknown))
(define prudence (make-person "Prudence Yter" 1900 'left 'unknown 'unknown))
(define andy (make-person "Andy Yter" 1974 'left prudence valor))

(define mora (make-person "Mora Baker" 1900 'left 'unknown 'unknown))
(define excitus (make-person "Excitus Tennyson" 1900 'left 'unknown 'unknown))
(define dolores (make-person "Dolores Baker"  1974 'right mora excitus))

(define victor (make-person "Victor Yter" 1993 'right dolores andy))

(define shela (make-person "Shela Goldsborrow" 1900 'right 'unknown 'unknown))
(define john (make-person "John Goldsborrow" 1900 'right 'unknown 'unknown))

(define margaret (make-person "Margaret Amis" 1970 'right shela john))
(define george (make-person "George Amis" 1940 'right 'unknown 'unknown))

(define andrew (make-person "Andrew Amis" 1990 'right margaret george))


;; anc-foldf : { ancestor Y -> Y} Y ancestor -> Y
; Foldl, folds to father first.
(define (anc-foldf fxn base ancestor)
  (cond
    [(unknown? ancestor) base]
    [else
     (fxn ancestor
          (anc-foldf fxn
                     (anc-foldf fxn base (person-father ancestor))
                     (person-mother ancestor)))]))

;; anc-foldm : { ancestor Y -> Y} Y ancestor -> Y
; Foldl, folds to mother first.
(define (anc-foldm fxn base ancestor)
  (cond
    [(unknown? ancestor) base]
    [else
     (fxn ancestor
          (anc-foldf fxn
                     (anc-foldf fxn base (person-mother ancestor))
                     (person-father ancestor)))]))

(check-expect (anc-foldm (lambda (ancestor number) (add1 number)) 0 victor)
              (anc-foldf (lambda (ancestor number) (add1 number)) 0 victor))

;; count-known-family : ancestor -> number
; Counts family members
(define (count-known-family ancestor)
  (anc-foldm (lambda (ancestor number) (add1 number)) 0 ancestor))

(check-expect (count-known-family victor) 7)

;; anc-map : { ancestor -> ancestor } ancestor -> ancestor
; Maps the given function over the family tree
(define (anc-map fxn anc)
  (cond
    [(unknown? anc) 'unknown]
    [else
     (fxn (make-person (person-name anc)
                       (person-birth-year anc)
                       (person-hand anc)
                       (anc-map fxn (person-mother anc))
                       (anc-map fxn (person-father anc))))]))

(check-expect (anc-map (lambda (anc)
                         (make-person (person-name anc)
                                      (person-birth-year anc)
                                      (person-hand anc)
                                      (person-mother anc)
                                      (person-father anc))) victor)
              victor)

;; left-handed? : person -> boolean
; Returns true if the given person is left handed.
(define (left-handed? p)
  (and (symbol? (person-hand p))
       (symbol=? (person-hand p) 'left)))
(define (right-handed? p)
  (and (symbol? (person-hand p))
       (symbol=? (person-hand p) 'right)))

(check-expect (left-handed? andy) true)
(check-expect (left-handed? mora) true)
(check-expect (left-handed? victor) false)

;; anc-ormap : {ancestor -> boolean} ancestor -> boolean
; Ormap, but for ancestor!
(define (anc-ormap fxn anc)
  (anc-foldm (lambda (anc bool) (or (fxn anc) bool)) false anc))

(check-expect (anc-ormap left-handed? victor) true)
(check-expect (anc-ormap left-handed? andrew) false)

;; anc-andmap : {ancestor -> boolean} ancestor -> boolean
; Andmap, but for ancestor!
(define (anc-andmap fxn anc)
  (anc-foldm (lambda (anc bool) (and (fxn anc) bool)) true anc))

(check-expect (anc-andmap left-handed? victor) false)
(check-expect (anc-andmap right-handed? andrew) true)
