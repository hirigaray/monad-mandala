(load "monads/maybe.scm")

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
