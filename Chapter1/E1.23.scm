(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (next num)
  (if (= num 2) 3 (+ num 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes start end)
  (define (runnext v)
    (timed-prime-test v) (search-for-primes (+ v 2) end))
  (if (<= start end) (runnext start)))

(search-for-primes 1001 1021)
(search-for-primes 10001 10051)
(search-for-primes 100001 100051)

; primes and their times (old)
; 1009 - 4
; 1013 - 4
; 1019 - 3
; 10007 - 8
; 10009 - 9
; 10037 - 9
; 100003 - 26
; 100019 - 26
; 100043 - 26

; primes and their times (new)
; 1009 - 3
; 1013 - 3
; 1019 - 3
; 10007 - 7
; 10009 - 6
; 10037 - 6
; 100003 - 17
; 100019 - 17
; 100043 - 17

; Making the change doesn't really halve the time.. this is because there is
; the overhead of function call to next and also the if condition inside next
