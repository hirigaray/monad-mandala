(load "maybe.scm")

(define (safe-division a b)
  (Do (x <- (Just a))
      (y <- (Just b))
      (Return (if (zero? y)
                Nothing
                (/ x y)))))

(define (safe-division-long a b)
  (Bind (Just a)
        (lambda (x)
          (Bind (Just b)
                (lambda (y)
                  (Return (if (zero? y)
                            Nothing
                            (/ x y))))))))
