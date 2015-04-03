;; We represent the cons pair `(a, b)` as $n=2^a\cdot3^b$.
;; This can be done with any two numbers that are relatively prime.
;; To recover the car and car we just have to keep a count
;; of times the number $n$ can be divided by either 2 or 3.
;; This works because 2 and 3 will be the only prime factors of $n$

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (divcount n d)
  (if (= 0 (remainder n d))
      (+ 1 (divcount (/ n d) d))
      0))

(Define (car z) (divcount z 2))
(define (cdr z) (divcount z 3))

(car (cons 5 7)) ; 5
(cdr (cons 5 7)) ; 7
