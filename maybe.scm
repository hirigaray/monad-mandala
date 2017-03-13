(define Just
  (lambda (a)
    (list 'Just a)))

(define Just?
  (lambda (a)
    (and (list? a)
         (equal? 'Just (car a)))))

(define Nothing
  (lambda ()
    'Nothing))

(define Nothing?
  (lambda (a)
    (equal? 'Nothing a)))

(define Unwrap
  (lambda (a)
    (cond
      ((Nothing? a) a)
      ((Just? a) (cadr a))
      (else (Nothing)))))

(define Filter-Nothing
  (lambda (l)
    (filter (not Nothing?) l)))
