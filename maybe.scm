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

; Monadic operators
(define Extract
  (lambda (a)
    (cond
      ((Just? a) (cadr a))
      ((Nothing? a) (error "Extract" "Tried to extract Nothing"))
      (else (error "Extract" "Tried to extract a non-Maybe value")))))

(define Return
  (lambda (a)
    (cond
      ((Just? a) a)
      ((Nothing? a) Nothing)
      (else (Just a)))))

(define Bind
  (lambda (a f)
    (cond
      ((Just? a) (f (Extract a)))
      ((Nothing? a) Nothing)
      (else (error ">>=" "Tried to bind a non-Maybe value")))))


; Utility functions
(define filter-Nothing
  (lambda (l)
    (filter (lambda (a)
              (not (Nothing? a)))
            l)))

; Do notation
(define-syntax Do
  (syntax-rules (<-)
    ; The last expression is left as-is
    ((_ e) e)
    ; Replace bind expressions with Bind calls
    ((_ (var <- a) b ...)
     (Bind a (lambda (var) (Do b ...))))
    ; Replace regular expressions with Bind calls, but discard the result
    ((_ a b ...)
     (Bind a (lambda (_) (Do b ...))))))
