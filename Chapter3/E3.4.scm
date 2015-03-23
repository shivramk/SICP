(define (call-the-cops)
  (display "Calling the cops\n"))

(define (make-account balance password)
  (define bad-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) 
             balance)
      "Insufficient funds"))  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? password pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (lambda (x) 
          (begin (set! bad-count (+ bad-count 1))
                 (if (> bad-count 7)
                   (call-the-cops) 
                   "Incorrect password")))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
