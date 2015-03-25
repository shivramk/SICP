;; The good-enough test fails for small numbers. For example
;;
;;     > (sqrt 0.0004)
;;     0.0354008825558513
;; 
;; It can be modified as follows

(define (good-enough? guess x)
  (< (/ (abs (- (square guess) x)) guess) 0.0001))

; Rest of the code
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; This gives much better results for both small and large number
;;
;;     > (sqrt 0.0004)
;;     0.020001426615330147
;;     > (sqrt 100)
;;     10.000000000139897
