#lang planet neil/sicp

(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)

(fringe (list x x))