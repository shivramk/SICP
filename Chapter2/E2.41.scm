#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triples n)
  (flatmap (lambda (i) 
             (map (lambda (j) (cons i j)) (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-equals? s)
  (lambda (n)
    (= s (+ (car n) (cadr n) (caddr n)))))

(define (triples-sum n s)
  (filter (sum-equals? s) (triples n)))

(triples-sum 10 12)

