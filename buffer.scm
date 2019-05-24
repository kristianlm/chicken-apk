
;; simple buffering mechanism. every (out "binary") adds the string to
;; a list. strings can be modified afterwards. this is useful if you
;; want to prefix some serialized data with that length of that
;; serialized data: you don't know the length before hand. adding the
;; length prefix with the correct length, then modifying it later
;; seems to be working quite well.

(define (make-buffer) (vector 0 '()))
(define (buffer-add! b str)
  (vector-set! b 0 (+ (if (string? str) (string-length str) (blob-size str)) (vector-ref b 0)))
  (vector-set! b 1 (cons str (vector-ref b 1))))
(define (buffer-pos b) (vector-ref b 0))
(define (buffer-strings b) (vector-ref b 1))

(define (write-buffer b)
  (for-each (lambda (o)
              (cond ((blob? o) (display (blob->string o)))
                    ((string? o) (display o))))
            (reverse (buffer-strings b))))

(define current-buffer (make-parameter (make-buffer)))
(define (current-buffer-pos) (buffer-pos (current-buffer)))

(define (wotb thunk)
  (parameterize ((current-buffer (make-buffer)))
    (thunk)
    (with-output-to-string (lambda () (write-buffer (current-buffer))))))

(define (out x)
  (buffer-add! (current-buffer) x)
  x)

