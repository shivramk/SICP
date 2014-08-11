#lang planet neil/sicp

(define (for-each proc list)
  (define (proc-each l)
    (proc (car l))
    (for-each proc (cdr list)))
  (if (null? list)
      true
      (proc-each list)))

(for-each (lambda (x)
            (display x)
            (newline))
          (list 57 321 88))