#lang planet neil/sicp

; This procedure remains unchanged
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


; a. We change the constructors and selectors to support infix notation
;    Assumption is that all expressions are full parenthesized and + & *
;    only take two arguments
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4
(deriv '(x * (x + y)) 'x)             ; (x + (x + y)) = 2x + y

; b. Add support for standard algebraic notation without changing 
;    the original deriv implementation
; We do this by implementing a prodedure `parenthesize` which 
; parenthesizes standard algrebraic expression. Parenthesized
; expressions can then be handled by the code already defined above

(define (deriv2 exp var)
  (deriv (parenthesize exp) var))

(define (precedence op1 op2)
  (if (eq? op1 '*)
      1
      (if (eq? op2 '*) 2 1)))

(define (parenthesize exp)
  (if (pair? exp)
      (if (= (length exp) 3)
          (map parenthesize exp)
          (let ((prec (precedence (cadr exp) (cadddr exp))))
            (if (= prec 1)
                (parenthesize (cons (list (car exp) (cadr exp) (caddr exp))
                                    (cdddr exp)))
                (parenthesize (list (car exp) (cadr exp) (cddr exp))))))
      exp))

(deriv2 '(x + 3 * (x + y + 2)) 'x) ; 4
(deriv2 '(x * x + x * y) 'x)       ; ((x + x) + y) = 2x + y