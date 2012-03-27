;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AA-binary-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Prog 2 Asg #12: More Trees
; Andrew Amis
; Started: 3.27.12
; Ended: 3.27.12
; http://fellowhuman.com/gbk/2012/03/26/prog-2-asg-more-trees/



;; A BT (binary tree) is one of:
;;    empty
;;    (make-bt-node Number BT BT)
(define-struct bt-node (value L R))

;; Template
#;(define (fun-for-bt-node node)
    (cond
      [(empty? node) ...]
      [else
       (bt-node-value node)
       (fun-for-bt-node (bt-node-L node))
       (fun-for-bt-node (bt-node-r node))]))
#;(define (fun-for-bt-node node)
    (cond
      [(empty? node) ...]
      [else
       (make-bt-node (bt-node-value node)
                     (fun-for-bt-node (bt-node-L node))
                     (fun-for-bt-node (bt-node-r node)))]))





;; bt-fold : (Number X X -> X) X BT -> X
;; reduces the given tree to a value using the given function and base value.
(define (bt-fold combine base bt)
  (cond
    [(empty? bt) base]
    [(bt-node? bt)
     (combine (bt-node-value bt)
              (bt-fold combine base (bt-node-L bt))
              (bt-fold combine base (bt-node-R bt)))]))



;; bt-sum : bt -> number
; Adds all the numbers in the binary tree together.
(define (bt-sum node)
  (bt-fold (lambda (num laccum raccum) (+ num laccum raccum))
           0
           node))

(check-expect (bt-sum (make-bt-node 4 empty empty)) 4)
(check-expect (bt-sum (make-bt-node 4 (make-bt-node 3 empty empty) empty)) 7)





;; bt-map : {Number -> Number} bt -> bt
; Applies the given function to each item of bt, replacing that item in the tree
(define (bt-map fxn node)
  (cond
    [(empty? node) empty]
    [else
     (make-bt-node (fxn (bt-node-value node))
                   (bt-map fxn (bt-node-L node))
                   (bt-map fxn (bt-node-R node)))]))



(check-expect (bt-map add1 (make-bt-node 4 empty empty))
              (make-bt-node 5 empty empty))
(check-expect (bt-map add1 (make-bt-node 4 (make-bt-node 3 empty empty) empty))
              (make-bt-node 5 (make-bt-node 4 empty empty) empty))



;; tree-abs : bt -> bt
; Takes the absolute value of each item in the binary tree.
(define (tree-abs node)
  (bt-map abs node))
(check-expect (tree-abs (make-bt-node 4 empty empty))
              (make-bt-node 4 empty empty))
(check-expect (tree-abs (make-bt-node 4 (make-bt-node -3 empty empty) empty))
              (make-bt-node 4 (make-bt-node 3 empty empty) empty))