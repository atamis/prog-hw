;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-binary-search-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Binary Trees and Binary Search Trees.
;; Andrew Amis -- atamiser@gmail.com -- 4/12/12.



;; a binary tree (BT) is either...
;    false
;    (make-node number symbol BT BT) 
(define-struct node (ssn name left right))

;; Templates
#;(define (fun-for-node node)
    (cond
      [(false? node) ...]
      [else
       (node-ssn node)
       (node-name node)
       (fun-for-node (node-left node))
       (fun-for-node (node-right node))]))

(define bst1 (make-node 5 'a (make-node 3 'b (make-node 2 'c false false)
                                        (make-node 4 'x false false))
                        (make-node 6 'd false false)))

; 5a
; |  \
; 3b  6d   
; | \
; 2c 4x


(define bt1 (make-node 10 'e (make-node 5 'f (make-node 8 'g false false)
                                        (make-node 3 'z false false))
                       (make-node 4 'h false false)))
; 10e
; |  \
; 5f  4h
; | \
; 8g 3z


(define bst2 (make-node 1 'i false false))
; 1i

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Trees drawn below their definition.

;; contains-bt : number bt -> boolean
; Determines whether the boolean tree contains the given number
(define (contains-bt num node)
  (cond
    [(false? node) false]
    [else
     (or (= (node-ssn node) num)
         (contains-bt num (node-left node))
         (contains-bt num (node-right node)))]))

(check-expect (contains-bt 5 bst1) true)
(check-expect (contains-bt 2 bst1) true)
(check-expect (contains-bt 4 bst1) true)
(check-expect (contains-bt 10 bst1) false)
(check-expect (contains-bt 10 bt1) true)
(check-expect (contains-bt 1 bst2) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; search-bt : num bt -> (symbol|boolean)
; Searches through the given binary tree for the number, and returns either the
; associated symbol or false.
(define (search-bt num node)
  (cond
    [(false? node) false]
    [(= num (node-ssn node)) (node-name node)]
    [(contains-bt num (node-left node))
     (search-bt num (node-left node))]
    [(contains-bt num (node-right node))
     (search-bt num (node-right node))]
    [else false]))

(check-expect (search-bt 10 bt1) 'e)
(check-expect (search-bt 3 bt1) 'z)
(check-expect (search-bt 11 bt1) false)
(check-expect (search-bt 1 bst2) 'i)
(check-expect (search-bt 4 bst1) 'x)
(check-expect (search-bt 40 bst1) false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
A binary-search-tree (short: BST) is a BT:

false is always a BST;

(make-node soc pn lft rgt) is a BST if

lft and rgt are BSTs,

all ssn numbers in lft are smaller than soc, and

all ssn numbers in rgt are larger than soc.

|#

;; inorder : bst -> listof[number]
; Returns all the numbers in the given bst in order.
(define (inorder node)
  (cond
    [(false? node) empty]
    [else
     (append (inorder (node-left node))
             (list (node-ssn node))
             (inorder (node-right node)))]))

(check-expect (inorder bst2) (list 1))
(check-expect (inorder bst1) (list 2 3 4 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; search-bst : number BST -> symbol
; Searches through the given BST and returns the pn from the node with the
; given number ID, or false.
(define (search-bst n bst)
  (cond
    [(false? bst) false]
    [(= n (node-ssn bst)) (node-name bst)]
    [else
     (let ([left (search-bst n (node-left bst))]
           [right (search-bst n (node-right bst))])
       (if (not (false? left)) left right))]))
(check-expect (search-bst 1 bst2) 'i)
(check-expect (search-bst 3 bst2) false)
(check-expect (search-bst 5 bst1) 'a)
(check-expect (search-bst 2 bst1) 'c)
(check-expect (search-bst 1 bst1) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-bst : bst number symbol
; Addes the given number-symbol pair to the given BST in the proper position.
(define (create-bst node n sym)
  (cond
    [(false? node) (make-node n sym false false)]
    [(< n (node-ssn node))
     (make-node (node-ssn node)
                (node-name node)
                (create-bst (node-left node) n sym)
                (node-right node))]
    [(> n (node-ssn node))
     (make-node (node-ssn node)
                (node-name node)
                (node-left node)
                (create-bst (node-right node) n sym))]))

(check-expect (create-bst false 50 'a) (make-node 50 'a false false))
(check-expect (create-bst (create-bst false 50 'a) 60 'b)
              (make-node 50 'a false
                         (make-node 60 'b false false)))

(check-expect (create-bst bst1 1 'z)
              (make-node 5 'a
                         (make-node 3 'b
                                    (make-node 2 'c
                                               (make-node 1 'z false false)
                                               false)
                                    (make-node 4 'x false false))
                         (make-node 6 'd false false)))


(define A-tree (create-bst
                (create-bst
                 (create-bst
                  (create-bst
                   (create-bst 
                    (create-bst 
                     (create-bst 
                      (create-bst 
                       (create-bst false 63 'a)
                       29 'b)
                      89 'c)
                     15 'd)
                    77 'l)
                   95 'g)
                  10 'h)
                 24 'i)
                99 'o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 14.2.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample
  '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))

;; create-bst-from-list : lonn -> bst
; From a list of list of numbers and symbols and converts it to a binary search
; tree. Uses the first number as the root node.
(define (create-bst-from-list lonn)
  (foldl (λ (cur rest) (create-bst rest
                                   (car cur)
                                   (cadr cur))) false lonn))


(check-expect (create-bst-from-list (list (list 1 'a)))
              (make-node 1 'a false false))
(check-expect (create-bst-from-list (reverse sample)) A-tree)

;; list->bst : lonn -> bst
; See create-bst-from-list
(define list->bst create-bst-from-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bonus ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bad-tree
  (list
   (list 99 'o)
   (list 95 'g)
   (list 89 'c)
   (list 77 'l)
   (list 63 'a)
   (list 29 'b)
   (list 24 'i)
   (list 15 'd)
   (list 10 'h)))

;; A list that was in order would produce the least useful BST because the
; function would just insert them in order, so you would have, essentially, a
; one branch BST with all the pairs in descending order. No speed gain.