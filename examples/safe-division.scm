(load "mandala.scm")

(define (safe-division a b)
  (Do (x <- (Just a))
      (y <- (Just b))
      (if (zero? y)
        Nothing
        (Return (/ x y)))))

(define (explicit-safe-division a b)
  (Bind (Just a)
        (lambda (x)
          (Bind (Just b)
                (lambda (y)
                  (if (zero? y)
                    Nothing
                    (Return (/ x y))))))))
