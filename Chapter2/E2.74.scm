(include "ch2support.scm")

;; Tagging routines
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; Division 1 (Uses cons cells)
(define (install-division1-package)
  (define (field-name field) (car field))
  (define (field-value field) (cdr field))
  (define (get-field fieldname record)
    (if (null? record)
        '()
        (if (eq? fieldname (field-name (car record)))
            (field-value (car record))
            (get-field fieldname (cdr record)))))
  (define (get-salary record)
    (get-field 'salary record))
  
  (define (record-name record) (car record))
  (define (record-fields record) (cdr record))
  (define (get-record name records)
    (if (null? records)
        '()
        (if (eq? name (record-name (car records)))
            (attach-tag 'division1 (record-fields (car records)))
            (get-record name (cdr records)))))
  
  (put 'get-salary 'division1 get-salary)
  (put 'get-record 'division1 get-record)
  'done)

;; A divison 1 file
(define division1-file
  (attach-tag 'division1 '((ben . ((salary . 100000))) (alyssa . ((salary 110000))))))
  
;; Division 2 (Uses lists)
(define (install-division2-package)
  (define (field-name field) (car field))
  (define (field-value field) (cadr field))
  (define (get-field fieldname record)
    (if (null? record)
        '()
        (if (eq? fieldname (field-name (car record)))
            (field-value (car record))
            (get-field fieldname (cdr record)))))
  (define (get-salary record)
    (get-field 'salary record))
  
  (define (record-name record) (car record))
  (define (record-fields record) (cadr record))
  (define (get-record name records)
    (if (null? records)
        '()
        (if (eq? name (record-name (car records)))
            (attach-tag 'division2 (record-fields (car records)))
            (get-record name (cdr records)))))
  
  (put 'get-salary 'division2 get-salary)
  (put 'get-record 'division2 get-record)
  'done)

;; A divison2 file
(define division2-file
  (attach-tag 'division2 '((louis ((salary 90000))) (eva ((salary 118000))))))

;; Install packages
(install-division1-package)
(install-division2-package)

;; a. get-record
;;    For this procedure to work, each divisions file must have type tag
;;    The actual sturcture of the file doesn't matter as it taken care by
;;    the division specific procedure
(define (get-record name division-file)
  ((get 'get-record (type-tag division-file)) name (contents division-file)))

(get-record 'louis division2-file)
(get-record 'louis division1-file)
(get-record 'ben division1-file)

;; b. get-salary
;;    For this procedure to work, each divisions record must have type tag
;;    The actual sturcture of the record doesn't matter as it taken care by
;;    the division specific procedure
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

(get-salary (get-record 'eva division2-file)) ; 118000
(get-salary (get-record 'ben division1-file)) ; 100000

;; c. find-employee-record
(define (find-employee-record name divisions-file-list)
  (if (null? divisions-file-list)
      '()
      (let ((record (get-record name (car divisions-file-list))))
        (if (null? record)
            (find-employee-record name (cdr divisions-file-list))
            record))))

(get-salary (find-employee-record 'eva (list division1-file division2-file))) ; 118000

;; d. If Insatiable takes over a new company, no changes are required to
;;    get-salary, get-record and find-employee-record. The new company
;;    must however define the specific selectors required by the generic
;;    methods
