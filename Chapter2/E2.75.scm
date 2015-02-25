(define (apply-generic op arg) (arg op))

(define PI 3.141592653589793)

(define (make-from-mag-ang r a) 
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
           (* r (cos a)))
          ((eq? op 'imag-part) 
           (* r (sin a)))
          ((eq? op 'magnitude) r) 
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define point (make-from-mag-ang 1 (/ PI 2)))

(point 'real-part) ; should be close to 0
(point 'imag-part) ; 1
(point 'magnitude) ; 1
(point 'angle) ; PI/2
