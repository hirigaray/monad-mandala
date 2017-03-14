(define (panic! who msg)
  (display (string-append "panic: " who ": " msg))
  (newline)
  (exit))

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
      ((Nothing? a) (panic! "Unwrap" "Tried to unwrap Nothing"))
      ((Just? a) (cadr a))
      (else (panic! "Unwrap" "Tried to unwrap a non-Maybe value")))))

(define filter-Nothing
  (lambda (l)
    (filter (lambda (a)
              (not (Nothing? a)))
            l)))
