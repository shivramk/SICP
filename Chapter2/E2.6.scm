#lang planet neil/sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Using the subtitution model
; one = (add-1 zero)
;     = (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;     = (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;     = (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

; Again using substitution model
; two = (add-1 one)
;     = (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;     = (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;     = (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; convert church numeral to integer
(define (church2num n) ((n inc) 0))

(church2num two) ; 2

(define (+ a b) 
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(church2num (+ (+ two two) one)) ; 5