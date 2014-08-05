#lang racket

; Use the fixnum package for bitwise arithmetic
; Using / would kind of defeat the purpose of implementing *
(require racket/fixnum)

(define (double x) (fxlshift x 1))

(define (halve x) (fxrshift x 1))

(define (* a b)
  (define (muliter x y z)
    (cond ((= z 0) y)
          ((even? z) (muliter (double x) y (halve z)))
          (else (muliter x (+ x y) (- z 1)))))
  (muliter a 0 b))

