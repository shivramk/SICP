#lang racket

(define (square x) (* x x))

(define (max a b)
  (if (> a b) a b))

(define (max3 a b c) (max a (max b c)))

(define (maxsqsum a b c)
  (+ (square (max3 a b c))
     (square (cond ((= a (max3 a b c)) (max b c))
                   ((= b (max3 a b c)) (max a c))
                   (else (max a b))))))
                 