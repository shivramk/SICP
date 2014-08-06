#lang planet neil/sicp

; iterative version of cont-frac 
; Start from the lowermost n/d and work our way upwards
(define (cont-frac n d k)
  (define (cont-frac-iter i s)
    (if (= i 0)
        s
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) s)))))
  (cont-frac-iter k 0))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x
        (* (- x) x)))
  (cont-frac n (lambda (i) (- (* 2 i) 1)) k))

(tan-cf 0.78539816 10) ; tan(pi/4) - should equal 1
(tan-cf 1.57079632 10) ; tan(pi/2) - should be a large number
