(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeat i)
    (if (= i 1) 
      f
      (compose f (repeat (- i 1)))))
  (repeat n))

((repeated square 2) 5) ; 625
((repeated inc 5) 5) ; 10
