#lang racket

; Use the fixnum package for bitwise arithmetic
; Using / would kind of defeat the purpose of implementing *
(require racket/fixnum)

(define (double x) (fxlshift x 1))

(define (halve x) (fxrshift x 1))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

