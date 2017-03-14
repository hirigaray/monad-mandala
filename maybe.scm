(define >>=
  (lambda (f x)
    (cond
      ((Nothing? x) (Nothing))
      ((Just? x) (f (Unwrap x)))
      (else (error ">>=" "Tried to bind a non-Maybe value")))))

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
      ((Nothing? a) (error "Unwrap" "Tried to unwrap Nothing"))
      ((Just? a) (cadr a))
      (else (error "Unwrap" "Tried to unwrap a non-Maybe value")))))

(define filter-Nothing
  (lambda (l)
    (filter (lambda (a)
              (not (Nothing? a)))
            l)))
