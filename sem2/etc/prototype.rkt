#lang racket

(require test-engine/racket-tests)

;; an object is...
; a list of...
;    lists in which...
;       the 1st item is a symbol, the slot name
;       the 2nd item is the value of the slot, which could be anything


;; The root object.
(define Object '((type Object)))


;; get : object symbol -> any
; Returns the value of the slot in the given object identified by the symbol. If
; the object doesn't have the slot, it recursively checks the prototype, until
; it runs out of objects.
(define (get object slot)
  (let ([x (assoc slot object)])
    (cond
      [x (cadr x)]
      [(assoc '__proto__ object)
       (get (get object '__proto__) slot)]
      [else #f])))

(define -> get)

(check-expect (->
               '((wheels 4) (__proto__ ((type Object))) (type Car)) 'wheels)
               4)
(check-expect (->
               '((wheels 4) (__proto__ ((type Object))) (type Car)) 'type)
               'Car)
(check-expect (->
               '((__proto__ ((wheels 4)
                             (__proto__ ((type Object))) (type Car)))
                 (type Awesome))
               'wheels)
               4)

;; set : object symbol any -> object
; Sets the value at the slot in the object.
(define (set object slot value)
  (let ([x (assoc slot object)])
    (if x
        (map {λ (object-item)
               (if (equal? slot (car object-item))
                   (list slot value)
                   object-item)}
             object)
        (cons (list slot value) object))))

(define ->= set)

(check-expect (->= empty 'x 4) '((x 4)))
(check-expect (->= empty 'asdf "This is a test") '((asdf "This is a test")))
(check-expect (->= '((asdf "This is a test")) 'asdf 5) '((asdf 5)))

;; new : object -> object
; Instantiates an object from the given object.
(define (new object)
  (set empty '__proto__ object))
(check-expect (new Object) '((__proto__ ((type Object)))))
(check-expect (new Car) '((__proto__
                           ((wheels 4) (type Car)
                                        (__proto__ ((type Object)))))))

;; clone : object symbol
; Creates a new top level object inheriting from the given object with the type
; specified by the given symbol
(define (clone object new-type)
  (set (new object) 'type new-type))

(check-expect (clone Object 'Test) '((type Test)
                                     (__proto__ ((type Object)))))
(check-expect (clone (clone Object 'Test) 'Awesome)
              '((type Awesome)
                (__proto__ ((type Test)
                                     (__proto__ ((type Object)))))))


;; Examples
(define Car (set (set (clone Object 'Car)
                      'wheels 4)
                 'maker "Toyota"))

(define honda (set (new Car) 'maker "Honda"))




(test)
