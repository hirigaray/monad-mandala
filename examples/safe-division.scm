(load "maybe.scm")

(define (safe-division a b)
  (Do (x <- (Just a))
      (y <- (Just b))
      (Return (if (zero? y)
                Nothing
                (/ x y)))))
