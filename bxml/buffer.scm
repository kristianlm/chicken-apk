;;; simple buffering mechanism. every (out "binary") adds the string to
;;; a list, and returns a procedure that can modify that string
;;; in-place.this is useful if you want to prefix some serialized data
;;; with that length of that serialized data: you don't know the length
;;; before hand. adding the length prefix with the correct length, then
;;; modifying it later seems to be working quite well.
;;;
;;; all write-uint* procedures return a procedure which lets you
;;; modify their value.

(import (only (chicken bitwise) arithmetic-shift bitwise-and)
        (only (chicken io) read-byte)
        (only (chicken string) conc)
        (only (chicken port) with-output-to-string)
        (only (chicken blob) blob->string blob-size blob?))

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



;; ================================================================================
;; writers and readers


(define (expect expected actual msg)
  (unless (equal? expected actual)
    (error (conc msg " (#x" (number->string actual 16) ")") actual))
  actual)


(define read-uint8 read-byte)

;; (string->blob (with-output-to-string (lambda () (write-uint16 #x1234))))
(define (write-uint8! n #!optional (dest (make-string 1)))
  (string-set! dest 0 (integer->char (bitwise-and #xFF n)))
  dest)

(define (write-uint8 n)
  (let ((dst (write-uint8! n)))
    (out dst)
    (lambda (n) (write-uint8! n dst))))

(define (read-uint16)
  (define a (read-byte))
  (define b (read-byte))
  (if (eof-object? a) a
      (+ (arithmetic-shift b 8)
         a)))

;; (string->blob (with-output-to-string (lambda () (write-uint16 #x1234))))
(define (write-uint16! n #!optional (dest (make-string 2)))
  (string-set! dest 0 (integer->char (bitwise-and #xFF n)))
  (string-set! dest 1 (integer->char (bitwise-and #xFF (arithmetic-shift n -8))))
  dest)

(define (write-uint16 n)
  (let ((dst (write-uint16! n)))
    (out dst)
    (lambda (n) (write-uint16! n dst))))

(define (read-uint32)
  (define a (read-byte))
  (define b (read-byte))
  (define c (read-byte))
  (define d (read-byte))
  (if (eof-object? a) a
      (+ (arithmetic-shift d 24)
         (arithmetic-shift c 16)
         (arithmetic-shift b 8)
         a)))

;; (string->blob (with-output-to-string (lambda () (write-uint32 #x12345678))))
(define (write-uint32! n #!optional (dest (make-string 4)))
  (string-set! dest 0 (integer->char (bitwise-and #xFF n))) (set! n (arithmetic-shift n -8))
  (string-set! dest 1 (integer->char (bitwise-and #xFF n))) (set! n (arithmetic-shift n -8))
  (string-set! dest 2 (integer->char (bitwise-and #xFF n))) (set! n (arithmetic-shift n -8))
  (string-set! dest 3 (integer->char (bitwise-and #xFF n)))
  dest)

(define (write-uint32 n)
  (let ((dst (write-uint32! n)))
    (out dst)
    (lambda (n) (write-uint32! n dst))))

;; haha not really. but it works.
(define (utf16->utf8 s)
  (let loop ((c (string->list s)) (r '()))
    (if (pair? c)
        (loop (cddr c) (cons (car c) r))
        (list->string (reverse r)))))

;; (utf8->utf16 "abc")
(define (utf8->utf16 s)
  (let loop ((c (string->list s)) (r '()))
    (if (pair? c)
        (loop (cdr c) (cons #\null (cons (car c) r)))
        (list->string (reverse r)))))

