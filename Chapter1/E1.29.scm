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

; without using sum
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

; using sum
(define (myintegral2 f a b n)
  (define h (/ (- b a) n))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (integrate k)
    (* (factor k) (f (+ a (* k h)))))
  (/ (* h (sum integrate 0 inc n)) 3))
           
(integral cube 0 1 0.001) ; 0.249999875000001
(myintegral cube 0 1 100.0) ; 0.2500000000000004
(myintegral cube 0 1 1000.0) ; 0.25000000000000044
(myintegral2 cube 0 1 100.0) ; 0.24999999999999992
(myintegral2 cube 0 1 1000.0) ; 0.2500000000000003
