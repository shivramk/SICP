#lang racket

; #lang planet neil/sicp gives an error in sements->painter
; so we use soegaard/package directly in racket

(require (planet soegaard/sicp:2:1/sicp))

; a. wave painter with a smile
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
        (v22 (make-vect 0.25 0.0))
        (v23 (make-vect 0.43 0.78))
        (v24 (make-vect 0.5 0.77))
        (v25 (make-vect 0.57 0.78)))
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
                             (make-segment v21 v22)
                             (make-segment v23 v24)
                             (make-segment v24 v25)))))

(paint-hires wave)

; b. corner-split with only one copy of up-split and right-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

(paint-hires (corner-split wave 4))

; c. square-limit using square-of-four and flipped painter

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

(paint-hires (square-limit wave 4))
