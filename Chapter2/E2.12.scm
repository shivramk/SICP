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

(define (make-center-percent c p)
  (let ((diff (* c (/ p 100.0))))
    (make-interval (- c diff) (+ c diff))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (- (center i) (lower-bound i)) (center i))))

(define interval (make-center-percent 10 5))
(print-interval interval)
(upper-bound interval); 9.5
(lower-bound interval); 10.5
(center interval) ; 10.0
(percent interval) ; 5.0