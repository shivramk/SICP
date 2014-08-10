#lang planet neil/sicp

; Assumption: items is non empty
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))
