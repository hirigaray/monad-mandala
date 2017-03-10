(define Just
  (lambda (a)
    (list 'Just a)))

(define Nothing
  (lambda ()
    'Nothing))

(define nothing?
  (lambda (a)
    (equal? 'Nothing a)))

(define just?
  (lambda (a)
    (and (list? a)
         (equal? 'Just (car a)))))

(define Unwrap
  (lambda (a)
    (cond
      ((nothing? a) a)
      ((just? a) (cadr a))
      (else (Nothing)))))
