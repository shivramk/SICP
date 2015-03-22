(include "E2.81.scm") 

(define (check-coercions coercions)
  (cond ((null? coercions) #t)
        ((car coercions) (check-coercions (cdr coercions)))
        (else #f)))

(define (try-coerce op type-tags args)
  (if (pair? type-tags)
    (let ((new-type-tags (map (lambda (x) (car type-tags)) args))
          (coercions (map (lambda (x) 
                            (get-coercion (type-tag x) (car type-tags))) args)))
      (let ((proc (get op new-type-tags)))
        (if (and proc (check-coercions coercions))  
          (apply proc 
                 (map (lambda (x) 
                        ((get-coercion (type-tag x) (car type-tags)) x)) args))
          (try-coerce op (cdr type-tags) args))))
    (error "Could not coerce")))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
          (apply proc (map contents args)) 
          (try-coerce op type-tags args)))))

(define z1 (make-complex-from-real-imag 0 5))
(define z2 (make-complex-from-real-imag 3 0))

(put-coercion 'scheme-number
              'complex
              (lambda (x) (make-complex-from-real-imag x 0)))

(add z1 3)

;; Consider the following procedure
(put 'mul3 '(complex complex scheme-number)
     (lambda (x y z)
       (let ((temp (mul x y)))
         (make-complex-from-real-imag (* z (real-part temp))
                                      (* z (imag-part temp))))))

(define (mul3 x y z) (apply-generic 'mul3 x y z))

;; This works
((get 'mul3 '(complex complex scheme-number)) z1 z2 1)

;; This fails with "Could not coerce"
;; Ideally it should have tried the mul3 procedure defined above because
;; scheme-number to complex coercion exists
(mul3 z1 3 1)
