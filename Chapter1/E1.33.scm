; iterative version of filtered-accumulate
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; a. sum primes between a and b
(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (id x) x)

(define (sum-primes a b)
  (filtered-accumulate prime? + 0 id a inc b))

(sum-primes 2 100) ; 1060

; b. product of all positive integers < n that are relatively prime to n 

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod-relative-prime n)
  (define (gcd1? a) (= (gcd a n) 1))
  (filtered-accumulate gcd1? * 1 id 1 inc (- n 1)))

(prod-relative-prime 10) ; 189
