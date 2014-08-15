#lang planet neil/sicp

; This procedure takes half as many steps as the unordered representation
; because we make use of the ordered nature just like element-of-set?
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))
        (else
         (cons x set))))

(adjoin-set 1 '())
(adjoin-set 2 '(1 2 3))
(adjoin-set 3 '(1 2 5))
(adjoin-set 0 '(1 2 3))
