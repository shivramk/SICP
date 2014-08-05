#lang planet neil/sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder(square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 10))

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes start end)
  (define (runnext v)
    (timed-prime-test v) (search-for-primes (+ v 2) end))
  (if (<= start end) (runnext start)))

(search-for-primes 1001 1021)
(search-for-primes 10001 10051)
(search-for-primes 100001 100051)

; primes and their times (old)
; 1009 - 4
; 1013 - 4
; 1019 - 3
; 10007 - 8
; 10009 - 9
; 10037 - 9
; 100003 - 26
; 100019 - 26
; 100043 - 26

; primes and their times (new)
; 1009 - 19
; 1013 - 19
; 1019 - 20
; 10007 - 23
; 10009 - 23
; 10037 - 24
; 100003 - 25
; 100019 - 29
; 100043 - 27

; This data agrees with O(log n) growth we had predicted