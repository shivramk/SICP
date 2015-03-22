; iterative version of cont-frac 
; Start from the lowermost n/d and work our way upwards
(define (cont-frac n d k)
  (define (cont-frac-iter i s)
    (if (= i 0)
        s
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) s)))))
  (cont-frac-iter k 0))

; integer division
(define (idiv x n)
  (if (= (remainder x n) 0)
      (/ x n)
      (- (/ x n) (/ (remainder x n) n))))

(define (get-d i)
  (let ((d3 (idiv (- i 1) 3)))
    (let ((diff (- i (* d3 3))))
      (cond ((= diff 1) 1)
            ((= diff 3) 1)
            (else (+ 2 (* d3 2)))))))

; value of e
(+ 2 (cont-frac (lambda (i) 1.0) get-d 20))
