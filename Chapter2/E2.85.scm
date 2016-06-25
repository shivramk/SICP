(include "E2.84.scm")

(define (install-integer-package)
  (put 'project '(integer) (lambda (x) (error "Cannot project further")))
  'done)

(define (install-real-package)
  (put 'project '(real) (lambda (x) 
                       (let ((numer (* x 1000000))
                             (denom 1000000))
                         (make-rational (inexact->exact numer) denom))))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'project '(rational) (lambda (x) 
                              (make-integer 
                                (inexact->exact 
                                  (round (/ (numer x) (denom x)))))))
  'done)

(define (install-complex-package)
  (define (equal-complex x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equal-complex)
  (put 'project '(complex) (lambda (x) 
                                 (make-real (real-part x))))
  'done)

; This method helps to avoid infinite recursion
(define (apply-generic-1 op arg)
  (let ((tag (type-tag arg)))
    (let ((proc (get op (list tag))))
      (if proc
        (proc (contents arg))
        (error "Type doesn't have operations" tag op)))))

(define (project x) (apply-generic-1 'project x))
(define (tower-index x) (apply-generic-1 'tower-index x))
(define (raise x) (apply-generic-1 'raise x))

(define (drop x)
  (if (= (tower-index x) 0) x
    (let ((projected (project x)))
      (if (equ? (raise projected) x)
        (drop projected) 
        x))))

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)

(define (apply-generic op . args)
  (display "(apply-generic ")
  (display op)
  (display " ")
  (display args)
  (display ")\n")
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
        (let ((ret (apply proc (map contents args))))
          (if (pair? ret) (drop ret) ret))
        (if (= (length args) 2)
          (let ((type1 (car type-tags)) 
                (type2 (cadr type-tags)))
            (if (eq? type1 type2)
              (error "No method for these types" (list op type-tags))
              (let ((i1 (tower-index (car args)))
                    (i2 (tower-index (cadr args)))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (> i1 i2)
                  (apply-generic op a1 (raise a2))
                  (apply-generic op (raise a1) a2)))))
          (error "No method for these types"
                 (list op type-tags)))))))

(drop (make-complex-from-real-imag 3 4)) ; (3 4)
(drop (make-complex-from-real-imag 3 0)) ; 3
(drop (make-real 3.5)) ; 7/2
(drop (make-real 3.0)) ; 3

(add (make-real 3.3) (make-real 1.2)) ; 9/2
