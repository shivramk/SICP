#lang planet neil/sicp

(define (equal? l1 l2)
  (if (and (pair? l1) (pair? l2))
      (if (eq? (car l1) (car l2))
          (equal? (cdr l1) (cdr l2))
          false)
      (and (null? l1) (null? l2))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '() '())
(equal? '(1) '(1))
(equal? '(a) '())