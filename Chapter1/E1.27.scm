#lang racket

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (next num)
  (if (= num 2) 3 (+ num 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder(square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (carmichael-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test-it a)
    (cond ((= a n) (fprintf (current-output-port) "~s fooled the test\n" n))
          ((try-it a) (test-it (+ a 1)))
          (else (fprintf (current-output-port) "~s couldn't fool the test\n" n))))
  (test-it 1))

(define (test-prime n)
  (if (prime? n) (fprintf (current-output-port) "~s is a prime number\n" n)
      (carmichael-test n)))

(test-prime 53)
(test-prime 20)

; Carmichael numbers
(test-prime 561)
(test-prime 1105)
(test-prime 1729)
(test-prime 2465)
(test-prime 2821)
(test-prime 6601)