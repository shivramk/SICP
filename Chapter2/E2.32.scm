#lang planet neil/sicp

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map 
                      (lambda (x) (append (list (car s)) x)) 
                      rest)))))

(subsets (list 1 2 3))

; How does subsets work
; Subsets is a recursive process where we need to handle two cases
; subsets (()) = (())
; subsets ((first rest)) = (subsets(rest) + (first + r) for each r in rest)
; To give a more concrete example
; subsets ((1 2 3)) = subsets((2 3)) + ((1) + r) for each r in subsets((2 3))
