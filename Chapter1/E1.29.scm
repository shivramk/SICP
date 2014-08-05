#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

 (define (cube x) (* x x x))

(define (myintegral f a b n)
  (define h (/ (- b a) n))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (integrate k sum y)
    (if (> k n) sum
        (integrate (+ k 1) 
                   (+ sum (* (factor k) (f y))) 
                   (+ y h))))
  (/ (* h (integrate 0 0.0 a)) 3))
           
(integral cube 0 1 0.001) ; 0.249999875000001
(myintegral cube 0 1 100.0) ; 0.2500000000000004
(myintegral cube 0 1 1000.0) ; 0.25000000000000044