#lang planet neil/sicp

; iterative version of accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; a. implement sum and product using accumulate
(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(define (id x) x)

; factorial implementation
(define (factorial n)
  (product id 1 inc n))

(factorial 5) ; 120
(factorial 10) ; 3628800

; sum 1 to n
(define (sum-n n)
  (sum id 1 inc n))

(sum-n 10) ; 55
(sum-n 100) ; 5050

; b. recursive version of accumulate
(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate2 combiner null-value term (next a) next b))))

(define (sum2 term a next b) (accumulate2 + 0 term a next b))
(define (product2 term a next b) (accumulate2 * 1 term a next b))

; factorial and sum. should give same results as before
(define (factorial2 n)
  (product2 id 1 inc n))

(factorial2 5) ; 120
(factorial2 10) ; 3628800

; sum 1 to n
(define (sum-n2 n)
  (sum2 id 1 inc n))

(sum-n2 10) ; 55
(sum-n2 100) ; 5050
