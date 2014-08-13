#lang scheme

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

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(print-vect (make-vect 2 3))
(print-vect (add-vect (make-vect 2 3) (make-vect 4 1)))
(print-vect (sub-vect (make-vect 2 3) (make-vect 4 1)))
(print-vect (scale-vect 2.0 (make-vect 4 1)))