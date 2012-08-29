;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-2compound) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #16: Two Compound Inputs
; Andrew Amis
; Started: 4.18.12
; Ended: 4.25.12
; http://fellowhuman.com/gbk/2012/04/16/prog-2-asg-two-compound-inputs/

;; dlist-map : {X Y -> Z} listof[X] listof[Y] -> listof[Z]
; Applies the function to each pair in the given lists, in sync. Produces a
; list.
(define (dlist-map fxn l1 l2)
  (cond
    [(empty? l1) empty]
    [(cons? l1)
     (cons
      (fxn (car l1) (car l2))
      (dlist-map fxn (cdr l1) (cdr l2)))]))

(check-expect (dlist-map + '(1 2 3) '(1 2 3)) '(2 4 6))
(check-expect (dlist-map * '(1 2 3) '(1 2 3)) '(1 4 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; phone-record is a struct...
(define-struct phone-record (name number))
; representing a record in a phone book in which...
;   name is a string
;   number is a number

;; zip : listof[string] listof[number] -> listof[phone-record]
; Takes a list of strings and a list of numbers and produces a list of
; phone-records made by combining the first item of the list of names
; with the first item from the list of numbers, etc.
(define (zip slist nlist)
  (map make-phone-record slist nlist))

(check-expect (zip empty empty) empty)
(check-expect (zip (list "test") (list 4)) (list (make-phone-record "test" 4)))
(check-expect (zip (list "test" "awesome" "asdf")
                   (list 4 9 3))
              (list (make-phone-record "test" 4)
                    (make-phone-record "awesome" 9)
                    (make-phone-record "asdf" 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.6.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct card (id hours))
(define-struct worker (id name wage))
(define-struct wage (id wage))

;; hours->wages2 : listof[worker] listof[card] -> listof[wage]
; Computes wages for each employee
(define (hours->wages2 workers cards)
  (map (λ (worker card)
         (if (not (= (worker-id worker) (card-id card)))
             (error "id mismatch, check for missing records")
             (make-wage
              (worker-id worker)
              (* (worker-wage worker)
                 (card-hours card)))))
       (sort workers (λ (a b) (< (worker-id a) (worker-id b))))
       (sort cards (λ (a b) (< (card-id a) (card-id b))))))

(check-expect (hours->wages2 (list (make-worker 1 "Alice" 40)
                                   (make-worker 2 "Bob" 40)
                                   (make-worker 3 "Carol" 45))
                             (list (make-card 3 100)
                                   (make-card 1 50)
                                   (make-card 2 75)))
              (list (make-wage 1 2000)
                    (make-wage 2 3000)
                    (make-wage 3 4500)))
(check-error (hours->wages2 (list (make-worker 1 "Alice" 40)
                                  (make-worker 2 "Bob" 40))
                            (list (make-card 3 100)
                                  (make-card 2 75)))
             "id mismatch, check for missing records")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.6.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; linear-combination : listof[number] listof[number] -> number
; Takes two lists of numbers, and multiplies the numbers together in concert
; a_n * x_n, etc., then sums that.
(define (linear-combination a x)
  (foldl + 0 (map * a x)))

(check-expect (linear-combination '(1 2 3) '(3 2 1)) 10)
(check-expect (linear-combination '(3 3 3) '(3 2 1)) 18)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 17.6.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DNAprefix : listof[symbols] listof[symbols] -> boolean
; Checks to see if the first list is the prefix of the second.
(define (DNAprefix prefix string)
  (letrec ([fxn (λ (prefix string)
                  (cond
                    [(empty? prefix) true]
                    [(cons? prefix)
                     (and (equal? (car prefix) (car string))
                          (fxn (cdr prefix) (cdr string)))]))])
    (if (<= (length prefix) (length string))
        (fxn prefix string)
        false)))

(check-expect (DNAprefix (list 'a 't) (list 'a 't 'c)) true)
(check-expect (DNAprefix (list 'a 't) (list 'a)) false)
(check-expect (DNAprefix (list 'a 't) (list 'a 't))  true)
(check-expect (DNAprefix (list 'a 'c 'g 't) (list 'a 'g)) false)
(check-expect (DNAprefix (list 'a 'a 'c 'c) (list 'a 'c)) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Intersection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-possibles : listof[x] listof[x] -> listof[x]
; Returns a list of all the possible pairings of the things from the lists. 
(define (all-possibles l1 l2)
  (cond
    [(empty? l1) empty]
    [(cons? l1)
     (append
      (map (λ (x)
             (list (car l1) x))
           l2)
      (all-possibles (cdr l1) l2))]))
(check-expect (all-possibles '(a b) '(c)) '((a c) (b c)))
(check-expect (all-possibles '(a b) '(c d)) '((a c) (a d) (b c) (b d)))

;; intersection : listof[string] listof[string] -> listof[string]
; Returns a list of strings that appears in both the lists, but not one.
(define (intersection l1 l2)
  (map car
       (filter (λ (x) (string=? (car x) (cadr x)))
               (all-possibles l1 l2))))

(check-expect (intersection '("test" "awesome") '("test")) '("test"))
(check-expect (intersection '("test" "awesome") '("asdf")) empty)
(check-expect (intersection empty '("asdf")) empty)
(check-expect (intersection '("test" "awesome")
                            '("test" "awesome" "adsf"))
              '("test" "awesome"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Union ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-difference : list list -> list
; Subtracts the 2nd list from the first, ensuring that the resulting list has
; no elements from the 2nd list.
(define (set-difference l1 l2)
  (filter (λ (l1-el)
            (not (member? l1-el l2)))
          l1))
(check-expect (set-difference '(test) empty) '(test))
(check-expect (set-difference '(test awesome) '(awesome)) '(test))
(check-expect (set-difference '(test awesome) '(awesome asdf)) '(test))

;; union -> listof[string] listof[string] -> listof[string]
; Returns a list of strings that appears at least one of the given lists.
(define (union l1 l2)
  (append l1 (set-difference l2 l1)))

(check-expect (union '("test" "awesome") '("test")) '("test" "awesome"))
(check-expect (union '("test" "awesome") '("asdf")) '("test" "awesome" "asdf"))
(check-expect (union empty '("asdf")) '("asdf"))
(check-expect (union '("test" "awesome")
                     '("test" "awesome" "asdf"))
              '("test" "awesome" "asdf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Substring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; empty-string? string -> boolean
; Returns whether this is an empty string.
(define (empty-string? x) (string=? x ""))
(check-expect (empty-string? "") true)
(check-expect (empty-string? "f") false)

;; chop-first-char : string -> string
; Chops the first character of the given string.
(define (chop-first-char s) (substring s 1))
(check-expect (chop-first-char "test") "est")
(check-expect (chop-first-char "asdf") "sdf")

;; substring? : string string -> boolean
; Returns true if the first string is a substring of the 2nd string.
(define (substring? pattern string)
  (letrec ([substring-list
            (λ (pattern list)
              (cond
                [(empty? list) false]
                [(cons? list)
                 (or
                  (DNAprefix pattern list)
                  (substring-list pattern (cdr list)))]))])
    (substring-list (string->list pattern)
                    (string->list string))))
(check-expect (substring? "ab" "babble") true)
(check-expect (substring? "xyz" "abcdef") false)
(check-expect (substring? "" "some string") true)
(check-expect (substring? "ab" "xycdab") true)
(check-expect (substring? "abc" "abacus abcdef") true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Subsequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; subsequence? : string string -> boolean
; Returns whether the first string is a subsequence of the second. That is,
; that all the letters in the first string occur in the second string with
; any number of characters between them.
(define (subsequence? pattern string)
  (letrec ([for-list
            (λ (pattern list)
              (cond
                [(empty? pattern) true]
                [(and (empty? list) (not (empty? pattern))) false]
                [(cons? list)
                 (if (equal? (car pattern) (car list))
                     (for-list (cdr pattern) (cdr list))
                     (for-list pattern (cdr list)))]))])
    (for-list (string->list pattern)
              (string->list string))))

(check-expect (subsequence? "brow" "The quick brown fox") true)
(check-expect (subsequence? "box" "The quick brown fox") true)
(check-expect (subsequence? "frown" "The quick brown fox") false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lcsubstring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; substrings : string -> listof[string]
; Returns a list of all possible substrings.
(define (substrings string)
  (cond
    [(= (string-length string) 1) empty]
    [else
     (let ([fpart (substring string 0 (sub1 (string-length string)))]
           [lpart (substring string 1 (string-length string))])
       (append (list fpart lpart)
               (substrings fpart)
               (substrings lpart)))]))

(check-expect (substrings "t") empty)
(check-expect (substrings "ta") '("t" "a"))
(check-expect (substrings "tad") '("ta" "ad" "t" "a" "a" "d"))
(check-expect (substrings "tade") '("tad" "ade" "ta" "ad" "t" "a" "a" "d" "ad"
                                          "de" "a" "d" "d" "e"))

;; lcsubstring : string string -> string
; Returns the longest common substring between the 2 strings.
(define (lcsubstring string1 string2)
  (foldl 
   (λ (cur acu)
     (if (substring? cur string2)
         (if (> (string-length cur) (string-length acu))
             cur
             acu)
         acu))
   "" (cons string1 (substrings string1))))

(check-expect (lcsubstring "mickey mouse" "minnie mouse's house") " mouse")
(check-expect (lcsubstring "a" "a") "a")
(check-expect (lcsubstring "a" "ab") "a")
(check-expect (lcsubstring "asdfasdfasdf" "fasfasfasixzcvasfaweihsdf") "sdf")
(check-expect (lcsubstring "x" "z") "")

