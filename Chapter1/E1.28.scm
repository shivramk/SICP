#lang planet neil/sicp

(define (square x) (* x x))

(define (squaremodulo n v)
  (if (= (remainder (square v) n) 1) 0 (square v))) 

(define (non-trivial-root n v)
  (if (or (= v 1) (= v (- n 1))) (square v) (squaremodulo n v)))

(define (expmod base exp m)
  (define (expmod_ e)
    (cond ((= e 0) 1)
          ((even? e) (remainder (non-trivial-root exp (expmod_ (/ e 2))) m))
          (else (remainder (* base (expmod_ (- e 1))) m))))
  (expmod_ exp))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 10))

; Carmichael numbers - should return false
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)

; Prime numbers should return true
(prime? 2262367)
(prime? 12935411)
(prime? 11623273)
(prime? 7942999)
(prime? 2526479)
(prime? 6311)
(prime? 1259)
(prime? 2)