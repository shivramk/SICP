#lang planet neil/sicp

(define (deriv exp var) 
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        (else ((get 'deriv (operator exp))
               (operands exp) var)))) 

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a. We have converted the deriv procedure which was performing manual dispatch
;;    to a data directed style using the get operator. We can't assimilate
;;    number? and variable? into the data directed dispatch because numbers and
;;    variables can take different values and with our current approach we would
;;    an entry for each value in a operation-and-type table. For numbers that 
;;    would mean having an entry for numbers 1 2 3 ... 

;; Support functions for deriv
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Support for put and get (provided by SICP authors in ch2support.scm)
;; http://mitpress.mit.edu/sicp/code/ch2support.scm
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; b & c. Procedures for sums, products and exponentiation
(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  (define (deriv-exp exp var)
    (make-product
     (make-product (exponent exp)
                   (make-exponentiation (base exp) (- (exponent exp) 1)))
     (deriv (base exp) var)))
  
  (define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2) 
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2) (* m1 m2)))
          (else (list '* m1 m2))))
  
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list '** base exp))))
  
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  
  (define (addend s) (car s))
  (define (augend s)
    (cond ((= (length s) 2) (cadr s))
          (else (cons '+ (cdr s)))))
  
  (define (multiplier p) (car p))
  (define (multiplicand x)
    (cond ((= (length x) 2) (cadr x))
          (else (cons '* (cdr x)))))
  
  
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exp)
  'done)

(install-deriv-package)

(deriv '(* x y (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
(deriv '(* x y z) 'x)       ; (* y z)
(deriv '(+ x (** x 2)) 'x)  ; (+ 1 (* 2 x))

;; d. If we change the get call like so
;;
;;       ((get (operator exp) 'deriv) (operands exp) var)
;;
;;    Then we will have to change the put calls to
;; 
;;       (put '+ 'deriv deriv-sum)
;;       (put '* 'deriv deriv-product)
;;       (put '** 'deriv deriv-exp)
;; 