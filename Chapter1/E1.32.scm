#lang planet neil/sicp

; iterative version of product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (id x) x)

; integer division
(define (idiv x n)
  (if (= (remainder x n) 0)
      (/ x n)
      (- (/ x n) (/ (remainder x n) n))))

; a. factorial + pi
; factorial implementation
(define (factorial n)
  (product id 1 inc n))

(factorial 5) ; 120
(factorial 10) ; 3628800

(define (feven n)
  (+ 2 (* (idiv n 2) 2)))
(define (fodd n)
  (+ 3 (* (idiv n 2) 2)))

; compute the value of PI
(define (compute-pi)
  (* 4.0 (/ (product feven 1 inc 10000) 
            (product fodd 0 inc 9999))))

(compute-pi) ; 3.1417497057380523

; b. recursive version of product
(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product2 term (next a) next b))))

(define (factorial2 n)
  (product2 id 1 inc n))

(factorial 5) ; 120
(factorial 10) ; 3628800