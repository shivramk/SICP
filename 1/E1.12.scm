#lang planet neil/sicp

; r, c is row and column
; indexing starts from 1

(define (pascal r c)
  (cond ((= c 1) 1)
        ((= c r) 1)
        (else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))

;
; > (pascal 1 1)
; 1
; > (pascal 3 2)
; 2
; > (pascal 4 2)
; 3
; > (pascal 5 3)
; 6
; > 