#lang planet neil/sicp

(define tolerance 0.00001)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeat i)
    (if (= i 1)
      f
      (compose f (repeat (- i 1)))))
  (repeat n))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f) (lambda (x) (average x (f x))))
 
; by experimentation I figured that number of average-dampings
; required = lg (n) where lg is log to base 2

; log to the base 2 (floored)
(define (lg n) (floor (/ (log n) (log 2))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (lg n))
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nth-root 65536 16) ; should be 2
(nth-root 4096 12) ; should be 2
(nth-root 27 3) ; should be 3