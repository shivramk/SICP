#lang planet neil/sicp

; a = bq + aq + ap
; b = bp + aq
;
; Applying again
; b' = b'p + a'q
;    = (bp + aq)p + (bq + aq + ap)q
;    = bp^2 + apq + bq^2 + aq^2 + apq
;    = b(p^2 + q^2) + a(q^2 + 2pq)
;
; This gives us
; p' = p^2 + q^2
; q' = q^2 + 2pq
;
; Trying this for a'
; a' = b'q + a'q + a'p
;    = (bp + aq)q + (bq + aq + ap)q  (bq + aq + ap)p
;    = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
;    = b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;    = bq' + aq' + ap

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))