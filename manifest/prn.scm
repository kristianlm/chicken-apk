
(import (only chicken.port with-output-to-port))

(define (prn . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (define P
        (lambda args
          (for-each
           (lambda (x)
             (cond ((number? x) (print* "\x1b[33m" x "\x1b[0m"))
                   ((string? x) (print* "\x1b[100m" x "\x1b[0m"))
                   ((symbol? x) (print* "\x1b[34m" x "\x1b[0m"))
                   ((pair? x)
                    (print* "(") (P (car x))
                    (for-each (lambda (x) (print* " ") (P x)) (cdr x))
                    (print* ")"))
                   (else (print* x))))
           args)))
      (P (car args))
      (apply P (cdr args))
      (newline))))

;; uncomment to enable printing
(define-syntax prn (syntax-rules () ((_ args ...) (begin))))
