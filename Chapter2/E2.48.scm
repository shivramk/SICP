#lang planet neil/sicp

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (print-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ", ")
  (display (ycor-vect v))
  (display ")")
  (newline))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 1))

(define seg (make-segment v1 v2))
(print-vect (start-segment seg))
(print-vect (end-segment seg))
