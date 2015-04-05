(define state 0)

(define (f v)
  (set! state (+ state v))
  state)

(+ (f 0) (f 1)) ; Gives 1

;; Scheme implementations evaluate their arguments from left to right
;; but to test whether our procedure works we can do this

(set! state 0)
(+ (f 1) (f 0)) ; Gives 2
