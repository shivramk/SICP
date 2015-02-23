(include "E2.77.scm")

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(add 10 20)  ; 30
(mul 5 7)    ; 35
(define z1 (make-complex-from-real-imag 1 3))
(define z2 (make-complex-from-real-imag 4 7))
(add z1 z2)  ; (complex rectangular 5 . 10)
