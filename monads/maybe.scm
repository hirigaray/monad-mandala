; Primitive definitions
(define Just
  (lambda (a)
    (list 'Just a)))

(define Nothing
  (quote Nothing))

; Equality checkers
(define Just?
  (lambda (a)
    (and (list? a)
         (equal? 'Just (car a)))))

(define Nothing?
  (lambda (a)
    (equal? Nothing a)))

(define Maybe?
  (lambda (a)
    (or (Just? a)
        (Nothing? a))))

; "Monad"ic operators
; Return is just (Just a)
(define Return
  (lambda (a)
    (Just a)))

; Extract takes a (Maybe a) value and returns a if it's not Nothing
(define Extract
  (lambda (ma)
    (cond
      ((Just? ma) (cadr ma))
      ((Nothing? ma) (error "Extract" "Tried to extract Nothing"))
      (else (error "Extract" "Tried to extract from a non-Maybe value")))))

; Bind takes a (Maybe a) value and a function f and applies f to a if it's not Nothing
(define Bind
  (lambda (ma f)
    (cond
      ((Just? ma) (f (Extract ma)))
      ((Nothing? ma) Nothing)
      (else (error ">>=" "Tried to bind a non-Maybe value")))))

; Utility functions
(define filter-Nothing
  (lambda (l)
    (filter (lambda (a)
              (not (Nothing? a)))
            l)))
