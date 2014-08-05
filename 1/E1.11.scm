#lang planet neil/sicp

; recursive process
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; iterative process
(define (f2 n)
  (if (< n 3) n) (fiter 2 1 0 (- n 3)))

(define (fiter a b c count)
  (if (= count 0) (+ a (* 2 b) (* 3 c))
      (fiter (+ a (* 2 b) (* 3 c)) a b (- count 1))))