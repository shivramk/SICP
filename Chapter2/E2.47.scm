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

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame) (car frame))
(define (edge1-frame2 frame) (cadr frame))
(define (edge2-frame2 frame) (cddr frame))

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 1))
(define v3 (make-vect 2 2))

(define f1 (make-frame v1 v2 v3))
(define f2 (make-frame2 v1 v2 v3))

(print-vect (origin-frame f1))
(print-vect (edge1-frame f1))
(print-vect (edge2-frame f1))

(print-vect (origin-frame2 f2))
(print-vect (edge1-frame2 f2))
(print-vect (edge2-frame2 f2))
