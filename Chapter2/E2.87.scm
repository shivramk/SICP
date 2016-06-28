(include "E2.86.scm")

(define (drop x)
  ; Ignore polynomials and integers
  (if (or (= (tower-index x) 10) (= (tower-index x) 0)) x
    (let ((projected (project x)))
      (if (equ? (raise projected) x)
        (drop projected) 
        x))))

(define (apply-generic op . args)
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
                (cond ((or (= i1 10) (= i2 10)) 
                       (error "Can't mix polynomials with other types"))
                      ((> i1 i2)
                       (apply-generic op a1 (raise a2)))
                      (else 
                       (apply-generic op (raise a1) a2))))))
          (error "No method for these types"
                 (list op type-tags)))))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; procedures same-variable? and variable? from section 2.3.2

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))) 
  
  ;; representation of terms and term lists
  (define (order term) (car term)) 
  (define (coeff term) (cadr term))

  (define (=zero-poly? poly) (null? poly))

  (define (adjoin-term term term-list) 
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list)) 
  (define (rest-terms term-list) (cdr term-list)) 
  (define (empty-termlist? term-list) (null? term-list)) 
  (define (make-term order coeff) (list order coeff))

  (define (texify-poly x)
    (define (print-var-power var power)
      (cond ((= power 0) "")
            ((= power 1) (symbol->string var))
            (else (string-append (symbol->string var) "^" 
                                 (number->string power)))))
    (define (plus? terms)
      (if (null? (cdr terms)) "" " + "))
    (define (print-poly var terms)
      (if (null? terms) ""
        (let ((order (order (car terms)))
              (coeff (coeff (car terms))))
          (if (eq? (type-tag coeff) 'polynomial)
            (string-append "(" (texify coeff) ")"
                           (print-var-power var order)
                           (plus? terms)
                           (print-poly var (cdr terms)))
            (string-append (texify coeff)
                           (print-var-power var order)
                           (plus? terms)
                           (print-poly var (cdr terms)))))))
    (print-poly (variable x) (term-list x)))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1) 
          (else
            (let ((t1 (first-term L1)) 
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else 
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2) 
    (if (empty-termlist? L1) 
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L) 
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2)))) 

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2)))) 
  (put 'texify '(polynomial) texify-poly)
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))) 
  (put 'tower-index '(polynomial)
       (lambda (x) 10))
  'done)

(define (make-polynomial var terms) 
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

; Helper method to define polynomial params
(define (pp params)
  (map (lambda (x)
         (list (car x) (make-integer (cadr x)))) params))

(define x1 (make-polynomial 'x (pp '((2 1) (1 -1) (0 2)))))
(define x2 (make-polynomial 'x (pp '((2 -1) (1 1) (0 1)))))
(define x3 (add x1 x2))

; Polynomial whose coefficients that are themselves polynomials
(define y1 (make-polynomial 'y (list (list 1 x2) (list 0 x1))))
(define y2 (make-polynomial 'y (list (list 1 x3) (list 0 x2))))
(define o1 (add y1 y2))
(define o2 (mul y1 y2))
