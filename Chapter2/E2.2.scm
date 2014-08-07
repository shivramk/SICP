#lang planet neil/sicp

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (average a b) (/ (+ a b) 2.0))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

(print-point (make-point 1 1)) ; 1, 1

(define seg1 (make-segment (make-point 2 3) (make-point 7 5)))
(print-point (midpoint-segment seg1)) ; 4.5, 4
