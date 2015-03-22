(define (make-monitored func)
  (define counter 0)
  (define (dispatch x)
    (if (eq? x 'how-many-calls?) 
      counter
      (begin (set! counter (+ counter 1))
             (func x))))
  dispatch)

(define s (make-monitored sqrt))
(s 'how-many-calls?) ; 0
(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 16) ; 4
(s 'how-many-calls?) ; 2
(s 'how-many-calls?) ; 2
