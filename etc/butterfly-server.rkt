#lang racket

(require test-engine/racket-tests
         2htdp/universe)

;; An iworld represents a world connected to the universe (server).
;; 2htdp/universe provides these functions to deal with iworlds:
;;   iworld-name : iworld -> String
;;      iworld=? : iworld iworld -> Boolean
;;       iworld? : Anything -> Boolean
;;
;; 2htdp/universe also provides three sample iworlds named iworld1, iworld2,
;;   and iworld3.

;; A Universe is a listof[iworld], representing the worlds that are currently
;; connected.  The first world in the list currently has the butterfly.
;;
;; Template:
#|
(define (fun-for-universe u)
  (cond
    [(empty? u) ...]
    [(cons? u)
     ... (first u)
     ... (fun-for-universe (rest u)) ...]))
|#

;; A Mail is a (make-mail iworld S-expression) representing a message from the
;;   universe to the given world.

;; A Bundle is a (make-bundle u mails worlds), where
;;      u     is a Universe representing the current state of the universe
;;      mails is a Listof[Mail] representing messages to be sent to the worlds
;; and worlds is a Listof[iworld] representing any iworlds the server wishes to
;;                 disconnect from the universe.

;; hookup : Universe iworld -> Bundle
;; Accepts a connection from a new iworld.
(define (hookup u new-world)
  (cond
    [(empty? u) (make-bundle (list new-world)
                             (list (make-mail new-world 'go))
                             empty)]
    [(cons? u)
     (make-bundle (snoc u new-world)
                             empty
                             empty)]))

(check-expect (hookup empty iworld1)
              (make-bundle (list iworld1)
                           (list (make-mail iworld1 'go))
                           empty))
(check-expect (hookup (list iworld1) iworld2)
              (make-bundle (list iworld1 iworld2)
                           empty
                           empty))
(check-expect (hookup (list iworld1 iworld2) iworld3)
              (make-bundle (list iworld1 iworld2 iworld3)
                           empty
                           empty))


;; listen : universe iworld message -> bundle
;; Accepts a message from the given world, and sends the butterfly on
;; accordingly.  If the message comes from a world that doesn't have the
;; butterfly, disconnect that world.
(define (listen u w msg)
  (cond
    [(empty? u) (make-bundle empty empty (list w))]
    [(cons? u)
     (let ([new-universe (snoc (rest u) (first u))])
       (if (and (iworld=? w (first u))
                (symbol=? msg 'done))
           (make-bundle new-universe
                        (list (make-mail (first new-universe) 'go))
                        empty)
           (make-bundle u empty empty)))]))

(check-expect (listen empty iworld1 'anything)
              (make-bundle empty empty (list iworld1)))
(check-expect (listen (list iworld1) iworld1 'done)
              (make-bundle (list iworld1)
                           (list (make-mail iworld1 'go)) empty))
(check-expect (listen (list iworld1 iworld2) iworld1 'done)
              (make-bundle (list iworld2 iworld1)
                           (list (make-mail iworld2 'go)) empty))
(check-expect (listen (list iworld1 iworld2) iworld2 'done)
              (make-bundle (list iworld1 iworld2)
                           empty empty))
(check-expect (listen (list iworld1 iworld2) iworld1 'anything)
              (make-bundle (list iworld1 iworld2)
                           empty
                           empty))



;; hangup : Universe iworld -> Bundle
;; React to the world w disconnecting from the universe.  If it has the
;; butterfly, send the butterfly to the next world (if there is one).
(define (hangup u w)
  (if (and (cons? u) (iworld=? (first u) w))
      (make-bundle (rest u)
                   (if (cons? (rest u))
                       (list (make-mail (second u) 'go))
                       empty)
                   (list w))
      (make-bundle (remove w u) empty (list w))))

(check-expect (hangup empty iworld1)
              (make-bundle empty empty (list iworld1)))
(check-expect (hangup (list iworld1 iworld2) iworld1)
              (make-bundle (list iworld2)
                           (list (make-mail iworld2 'go))
                           (list iworld1)))
(check-expect (hangup (list iworld1 iworld2) iworld2)
              (make-bundle (list iworld1)
                           empty
                           (list iworld2)))
(check-expect (hangup (list iworld1 iworld2) iworld2)
              (make-bundle (list iworld1)
                           empty
                           (list iworld2)))




;;;; Start the server running:

;; universe -> universe
;; Starts the universe:
(define (run u0)
  (universe u0
            (on-new hookup)
            (on-disconnect hangup)
            (on-msg listen)
            ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; snoc : listof[X] X -> listof[X]
;; adds x to the end of ls
(check-expect (snoc (list 1 2 3 4) 5)
              (list 1 2 3 4 5))
(define (snoc ls x)
  (cond [(empty? ls) (list x)]
        [(cons? ls)
         (cons (first ls) (snoc (rest ls) x))]))


(test)
