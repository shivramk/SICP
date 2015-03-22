(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5) ; gives 21

; why is this so? Let us abrreviate double as d
;
; consider the innermost expression (double double)
; (d d) -> (lambda (x) (d (d x)))
;
; Now consider (double (double double))
; (d (d d)) -> (lambda (x) (d (d (d (d x)))))
;
; This makes ((double (double double)) inc)
; ((d (d d)) inc) -> (lambda (x) (d (d (d (d (inc x))))))
; 
; That is equivalent to 4 doublings or 2^4 = 16
; 16 + 5 = 21
