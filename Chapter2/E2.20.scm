#lang planet neil/sicp

(define (same-parity first . rest)
  (define (find-parity orig list)
    (cond ((null? list) list)
          ((= orig (remainder (car list) 2))
           (cons (car list) (find-parity orig (cdr list))))
          (else (find-parity orig (cdr list)))))
  (let ((p (remainder first 2)))
    (cons first (find-parity p rest))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
