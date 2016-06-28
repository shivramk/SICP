(include "E2.87.scm")

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

  (define (=zero-poly? poly) (empty-termlist? (term-list poly)))

  (define (negate-poly L)
    (map (lambda (x)
           (make-term (order x) (negate (coeff x)))) L))

  (define (adjoin-term term term-list) 
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list)) 
  (define (rest-terms term-list) (cdr term-list)) 
  (define (empty-termlist? term-list) (null? term-list)) 
  (define (make-term order coeff) (list order coeff))

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

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (make-poly (variable p1)
                 (add-terms (term-list p1) (negate-poly (term-list p2))))
      (error "Polys not in same var: SUB-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'negate '(polynomial) 
       (lambda (p) (tag (make-poly (variable p) 
                                   (negate-poly (term-list p))))))
  'done)

(install-polynomial-package)

(define x1 (make-polynomial 'x (pp '((2 1) (1 1) (0 2)))))
(define x2 (make-polynomial 'x (pp '((2 -1) (1 1) (0 1)))))
(sub x1 x2)
(sub x1 x1)
