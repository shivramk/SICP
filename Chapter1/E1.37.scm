; recursive version of cont-frac
(define (cont-frac n d k)
  (define (compute-cont-frac i)
    (if (>= i k) 
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (compute-cont-frac (+ i 1))))))
  (compute-cont-frac 1))

; phi
(/ (+ 1 (sqrt 5)) 2)

; a. approximate value of phi using cont-frac
; k = 12 is sufficient to get 4 decimal places of accuracy
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12))

; b. iterative version of cont-frac 
; Start from the lowermost n/d and work our way upwards
(define (cont-frac2 n d k)
  (define (cont-frac-iter i s)
    (if (= i 0)
        s
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) s)))))
  (cont-frac-iter k 0))

; should be same as above
(/ 1 (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 12))
