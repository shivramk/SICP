(define (iterative-improve good-enough? improve-guess)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve-guess guess))))
  try)

(define tolerance 0.00001)

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f) (lambda (x) (average x (f x))))

; sqrt using iterative-improve
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       tolerance))
  ((iterative-improve good-enough?
                      (average-damp (lambda (y) (/ x y)))) 1.0))

(sqrt 64) ; 8.00000000000017
(sqrt 81) ; 9.000000000007091
(sqrt 0.04) ; 0.20000092713015796

; fixed-point using iterative-improve
(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess))
       tolerance))
  ((iterative-improve good-enough? f) first-guess))

; Another sqrt to test fixed-point
(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt2 64) ; should be 8
(sqrt2 81) ; should be 9
(sqrt2 0.04) ; should be 0.2
