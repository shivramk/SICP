(include "E2.83.scm")

(define (install-integer-package)
  (put 'tower-index '(integer) (lambda (x) 0))
  'done)

(define (install-real-package)
  (put 'tower-index '(real) (lambda (x) 2))
  'done)

(define (install-rational-package)
  (put 'tower-index '(rational) (lambda (x) 1))
  'done)

(define (install-complex-package)
  (put 'tower-index '(complex) (lambda (x) 3))
  'done)

(define (tower-index x) (apply-generic 'tower-index x))

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
        (apply proc (map contents args)) 
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

(define x (make-integer 1))
(define y (make-rational 2 3))
(add x y) ; 5/3

(define k (make-real 0.1))
(add k (add x y)) ; 1.766666

(define z (make-complex-from-real-imag 1 -1))
(add z (add x y)) ; 2.6666, -1

(add x x); 2
