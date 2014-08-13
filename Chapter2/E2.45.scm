#lang planet neil/sicp

(define (split op1 op2)
  (define (split-inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-inner painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  split-inner)

(define up-split (split below beside))
(define right-split (split beside below))

(paint (right-split einstein 4))
(paint (up-split einstein 4))
