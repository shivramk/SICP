#lang planet neil/sicp

(define (make-rat n d)
  (let ((sign (if (< (* n d) 0) -1 1))
        (g (gcd (abs n) (abs d))))
    (cons (/ (* sign (abs n)) g) (/ (abs d) g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x) 
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (make-rat -5 -10))
(print-rat (make-rat -2 5))
(print-rat (make-rat 4 -10))
(print-rat (make-rat 5 10))