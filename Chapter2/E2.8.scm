#lang planet neil/sicp

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")")
  (newline))

; reasoning
; The lower bound of the result should be the lowest possible
; The upper bound of the result should be the highest possible
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define i1 (make-interval 3 4))
(define i2 (make-interval 1 1.5))

(print-interval (sub-interval i1 i2))