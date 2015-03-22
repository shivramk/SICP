(include "E2.80.scm")

(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  (put 'exp '(scheme-number scheme-number) 
       (lambda (x y) (tag (expt x y))))
  'done)

(install-scheme-number-package)

;; Functions to manage coercion
(define coercion-table (make-table))
(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
          (apply proc (map contents args)) 
          (if (= (length args) 2)
              (let ((type1 (car type-tags)) 
                    (type2 (cadr type-tags)) 
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Louis's coercion procedures
(define (scheme-number->scheme-number n) n) 
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a. What happens?
(define (exp x y) (apply-generic 'exp x y))
(define z1 (make-complex-from-real-imag 1 3))
(define z2 (make-complex-from-real-imag 0 0))
;; The program goes into an infinite loop (uncomment to see)
;;
;; (exp z1 z2)
;;
;; Since exp is not defined for (complex complex) apply-generic keeps doing
;; complex->complex coercion in an infinite loop

;; b. Is Louis correct? no because apply-generic works correctly without them
;; But first reset the coercion table
(define coercion-table (make-table))

(add z1 z2)
(add 3 4)

;; C. modified apply-generic
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
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1))
                           (a1 (car args))
                           (a2 (cadr args)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types" (list op type-tags)))))))
                (error "No method for these types"
                       (list op type-tags)))))))

;; Give it a try
(exp z1 z2)

