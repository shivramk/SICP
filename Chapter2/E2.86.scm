(include "ch2support.scm")

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (texify x) (apply-generic 'texify x))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equal-complex x y)
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put '=zero? '(complex) 
       (lambda (z) (zero? (magnitude z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equal-complex)
  (put 'texify '(complex)
       (lambda (x) (string-append
                     (texify (real-part x)) " + "
                     (texify (imag-part x)) "i")))
  (put 'tower-index '(complex) 
       (lambda (x) 3))
  (put 'project '(complex) (lambda (x) 
                                 (make-real (real-part x))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put '=zero? '(integer)
       (lambda (x) (zero? x)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put 'texify '(integer)
       (lambda (x) (number->string x)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'tower-index '(integer) 
       (lambda (x) 0))
  (put 'project '(integer) 
       (lambda (x) (error "Cannot project further")))
  (put 'square '(integer)
       (lambda (x) (tag (* x x))))
  (put 'square-root '(integer)
       (lambda (x) (make-real (sqrt x))))
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer integer)
       (lambda (x y) (make-real (atan x y))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put '=zero? '(real)
       (lambda (x) (zero? x)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'texify '(real)
       (lambda (x) (number->string x)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 
                                                (make-integer 0))))
  (put 'tower-index '(real) 
       (lambda (x) 2))
  (put 'project '(real) (lambda (x) 
                          (let ((numer (* x 1000000))
                                (denom 1000000)) 
                            (make-rational 
                              (inexact->exact (round numer)) denom))))
  (put 'square '(real)
       (lambda (x) (tag (* x x))))
  (put 'square-root '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))
  (put 'arctan '(real real)
       (lambda (x y) (tag (atan x y))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equal-complex x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (real-value x)
    (/ (numer x) (denom x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'equ? '(rational rational) equal-complex)
  (put 'texify '(rational)
       (lambda (x) (string-append "\frac{"
                                  (number->string (numer x))
                                  "}{"
                                  (number->string (denom x))
                                  "}")))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put '=zero? '(rational)
       (lambda (x) (zero? (numer x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'tower-index '(rational)
       (lambda (x) 1))
  (put 'project '(rational) (lambda (x)
                              (make-integer 
                                (inexact->exact 
                                  (round (/ (numer x) (denom x)))))))
  (put 'square '(rational)
       (lambda (x) (make-rat
                     (* (numer x) (numer x))
                     (* (denom x) (denom x)))))
  (put 'square-root '(rational)
       (lambda (x) (make-real (sqrt (real-value x)))))
  (put 'sine '(rational)
       (lambda (x) (make-real (sin (real-value x)))))
  (put 'cosine '(rational)
       (lambda (x) (make-real (cos (real-value x)))))
  (put 'arctan '(rational rational)
       (lambda (x y) (make-real (atan (real-value x) (real-value y)))))
  'done)

(define (equ? x y) (apply-generic 'equ? x y))
(define (raise x) (apply-generic 'raise x))
(define (tower-index x) (apply-generic 'tower-index x))

(install-integer-package)
(install-real-package)
(install-rational-package)

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

(define (apply-generic op . args)
  ;(display "(apply-generic ")
  ;(display op)
  ;(display " ")
  ;(display args)
  ;(display ")\n")
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

(define x1 (make-rational 2 3))
(define y1 (make-rational 4 5))
(define x2 (make-rational 1 3))
(define y2 (make-rational 1 5))
(define z (add (make-complex-from-real-imag x1 y1) 
     (make-complex-from-real-imag x2 y2))) ; (1 1)
(magnitude z) ; 1.414

(define k (make-complex-from-mag-ang (make-rational 11 5)
                                     (make-real 1.0)))
(magnitude k) ; 11/5
(add z k) ; (2.188 2.851)
