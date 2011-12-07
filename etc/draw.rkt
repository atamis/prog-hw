;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname draw) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require picturing-programs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The World is an image, of the canvas on which we are drawing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constants
(define W 200)
(define H 150)

; Background
(define BG (empty-scene W H ))
(define DOT (circle 2 'solid 'green))

;; world-state (make-world-state BG 0 0 false)
(define-struct world-state (image lx ly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; handle-mouse : World(old) number number mouse-event -> World(new)
; Draw a line between the current mouse point and the last when the mouse button
; is down.
(define (handle-mouse world x y event)
  (make-world-state
   (if (or (mouse=? event "button-down")
           (mouse=? event "drag"))
       (scene+line (world-state-image world)
                   (world-state-lx world)
                   (world-state-ly world) x y (make-color x y 0))
       (world-state-image world))
   x y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang (make-world-state BG 0 0)
          (on-mouse handle-mouse)
          (to-draw world-state-image))