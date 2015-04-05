;; We use a [Linear Congruential Generator](http://en.wikipedia.org/wiki/Linear_congruential_generator)

; random number generator state
(define randstate 0)

(define (next n)
  (modulo (+ (* 8121 n) 28411) 134456))

(define (rand what)
  (if (eq? what 'generate)
    (let ((newstate (next randstate)))
      (begin (set! randstate newstate)
             newstate))
    (lambda (newstate)
      (set! randstate newstate))))

(rand 'generate) ; 28411
(rand 'generate) ; 27646
(rand 'generate) ; 57
(rand 'generate) ; 87940

((rand 'reset) 0) ; Reset back

(rand 'generate) ; 28411
(rand 'generate) ; 27646
(rand 'generate) ; 57
(rand 'generate) ; 87940
