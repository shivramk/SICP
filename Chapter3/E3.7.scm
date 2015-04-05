(include "E3.3.scm")

(define (make-joint account origpassword newpassword)
  (define (dispatch pass m)
    (if (eq? pass newpassword)
      (account origpassword m) 
      (lambda (x) "Incorrect password for joint account")))
  dispatch)

(define peter-acc (make-account 1000 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 100) ; 900
((paul-acc 'rosebud 'withdraw) 50) ; 850
((peter-acc 'open-sesame 'deposit) 150) ; 1000
((paul-acc 'rosebud 'withdraw) 500) ; 500

((paul-acc 'rose 'withdraw) 500) ; "Incorrect password for joint account"
