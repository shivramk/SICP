#lang planet neil/sicp

(define (fast-expt b n)
  (define (fiter x y z)
    (cond ((= z 0) y)
          ((even? z) (fiter (* x x) y (/ z 2)))
          (else (fiter x (* y x) (- z 1)))))
  (fiter b 1 n))