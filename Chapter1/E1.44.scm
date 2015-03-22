(define dx 0.00001)

(define (average a b c) (/ (+ a b c) 3.0))

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeat i)
    (if (= i 1)
      f
      (compose f (repeat (- i 1)))))
  (repeat n))

; return a smoothed version of function f
(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

; n-fold smoothed function
(((repeated smooth 3) square) 5)
  
