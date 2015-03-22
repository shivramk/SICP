(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

; takes 8 steps -> 4.55553609061889
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 3)

; takes 32 steps -> 4.555532257016376
(fixed-point (lambda (x) (/ (log 1000) (log x))) 3)

; Average damping takes fewer steps
