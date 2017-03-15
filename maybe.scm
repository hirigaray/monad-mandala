; Primitive definitions
(define Just
  (lambda (a)
    (list 'Just a)))

(define Nothing 'Nothing)

; Equality checkers
(define Just?
  (lambda (a)
    (and (list? a)
         (equal? 'Just (car a)))))

(define Nothing?
  (lambda (a)
    (equal? Nothing a)))

; Monadic operators
(define Return
  (lambda (a)
    (cond
      ((Nothing? a) Nothing)
      (else (Just a)))))

(define Bind
  (lambda (a f)
    (cond
      ((Just? a) (f (Unwrap a)))
      ((Nothing? a) Nothing)
      (else (error ">>=" "Tried to bind a non-Maybe value")))))

; Utility functions
(define Unwrap
  (lambda (a)
    (cond
      ((Just? a) (cadr a))
      ((Nothing? a) (error "Unwrap" "Tried to unwrap Nothing"))
      (else (error "Unwrap" "Tried to unwrap a non-Maybe value")))))

(define filter-Nothing
  (lambda (l)
    (filter (lambda (a)
              (not (Nothing? a)))
            l)))

(define-syntax Do
  (syntax-rules (<-)
    [(_ e) e]

    [(_ (var <- a) b ...)
     (Bind a (lambda (var) (Do b ...)))]

    [(_ a b ...)
     (Bind a (lambda (_) (Do b ...)))]))
