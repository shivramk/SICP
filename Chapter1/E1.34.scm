#lang planet neil/sicp

(define (square x) (* x x))
(define (f g) (g 2))

(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6

; What happens when we try to compute (f f)
;   f(g) = g(2)
; 
;   Therefore
;   f(f) = f(2)
;        = 2(2) 
;        = (2 2) in scheme
; 
; This will result in an error because 2 is not a function and cannot be applied
; The exact error message will depend on the implementation
; In racket scheme the error will be something like
; 
; application: not a procedure;
; expected a procedure that can be applied to arguments
;  given: 2
;  arguments...:
;   2
