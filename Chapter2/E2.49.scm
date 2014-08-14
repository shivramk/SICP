#lang racket

; #lang planet neil/sicp gives an error in sements->painter
; so we use soegaard/package directly in racket

(require (planet soegaard/sicp:2:1/sicp))

; a. painter that draws the outline of a frame
; I use 0.99 instead of 1.0 because 1.0 causes the line to exceed bounds
; and nothing is drawn
(define frame-painter
  (let ((v1 (make-vect 0.0 0.0))
        (v2 (make-vect 0.99 0.0))
        (v3 (make-vect 0.99 0.99))
        (v4 (make-vect 0.0 0.99)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v3 v4)
                             (make-segment v4 v1)))))

; b. paints X
(define x-painter
  (let ((v1 (make-vect 0.0 0.0))
        (v2 (make-vect 0.99 0.0))
        (v3 (make-vect 0.99 0.99))
        (v4 (make-vect 0.0 0.99)))
    (segments->painter (list (make-segment v1 v3)
                             (make-segment v2 v4)))))

; c. paints a diamond
(define diamond-painter
  (let ((v1 (make-vect 0.5 0.0))
        (v2 (make-vect 0.99 0.5))
        (v3 (make-vect 0.5 0.99))
        (v4 (make-vect 0.0 0.5)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v3 v4)
                             (make-segment v4 v1)))))

; d. wave painter
(define wave
  (let ((v1 (make-vect 0.4 0.0))
        (v2 (make-vect 0.5 0.3))
        (v3 (make-vect 0.6 0.0))
        (v4 (make-vect 0.75 0.0))
        (v5 (make-vect 0.59 0.45))
        (v6 (make-vect 1.0 0.15))
        (v7 (make-vect 0.6 1.0))
        (v8 (make-vect 0.64 0.85))
        (v9 (make-vect 0.6 0.67))
        (v10 (make-vect 0.75 0.67))
        (v11 (make-vect 1.0 0.35))
        (v12 (make-vect 0.4 1.0))
        (v13 (make-vect 0.36 0.85))
        (v14 (make-vect 0.4 0.67))
        (v15 (make-vect 0.3 0.67))
        (v16 (make-vect 0.15 0.59))
        (v17 (make-vect 0.0 0.85))
        (v18 (make-vect 0.0 0.67))
        (v19 (make-vect 0.15 0.4))
        (v20 (make-vect 0.3 0.6))
        (v21 (make-vect 0.36 0.5))
        (v22 (make-vect 0.25 0.0)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v4 v5)
                             (make-segment v5 v6)
                             (make-segment v7 v8)
                             (make-segment v8 v9)
                             (make-segment v9 v10)
                             (make-segment v10 v11)
                             (make-segment v12 v13)
                             (make-segment v13 v14)
                             (make-segment v14 v15)
                             (make-segment v15 v16)
                             (make-segment v16 v17)
                             (make-segment v18 v19)
                             (make-segment v19 v20)
                             (make-segment v20 v21)
                             (make-segment v21 v22)))))

(paint frame-painter)
(paint x-painter)
(paint diamond-painter)
(paint wave)
