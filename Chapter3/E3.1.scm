(define (make-accumulator value)
  (lambda (x)
    (begin (set! value (+ value x)) value)
    ))

(define A (make-accumulator 5))

(A 10) ; 15
(A 10) ; 25

