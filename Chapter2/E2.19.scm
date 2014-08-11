#lang planet neil/sicp

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? coin-values) (null? coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)

(define us-coins2 (list 25 50 5 10 1))
(cc 100 us-coins2) ; should be same as above as the order doesn't really matter

; Why does the order of coin-values not matter?
; The procedure cc doesn't make any assumptions about the ordering of the coin-values
; The only assumption is that the values in coin-values are unique