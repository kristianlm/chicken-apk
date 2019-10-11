;; parsing string-pool of packaged android binary xml and android resource files
;;
;; note that ideally, packaged files would be fully parsed and then
;; serialized back up. but that is _a lot_ of work. so we just do it
;; for the string pools. this has some limitations: you must know the
;; existing string that you want to replace. for example:
;;
;; if you want to give an apk a different package name, you must know
;; the existing package name and the manifest must not reference that
;; same package name elsewhere.
;;
;; the parsed xml/resource is in some semi-structured form, combining
;; both structured and parsed data with blobs. this is ugly but seems
;; to work fairly well.
;;
;; also looked at https://github.com/iBotPeaches/Apktool/blob/3f85dfa6c9fc7f81a991b7104888f282a37a36cf/brut.apktool/apktool-lib/src/main/java/brut/androlib/res/decoder/StringBlock.java
;; but mostly https://github.com/skylot/jadx/blob/1830c273c08cf4bb3d06cbda61f5505bc3dab5d3/jadx-core/src/main/java/jadx/core/xmlgen/CommonBinaryParser.java
(import (chicken io)
        (chicken port)
        (chicken bitwise)
        (chicken string)
        ;; (chicken pretty-print)
        (chicken blob)
        (only (chicken process-context) command-line-arguments)
        srfi-1
        srfi-4
        srfi-69)

(include "buffer.scm")

;; hex version on print
(define (prn . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply print (map (lambda (x) (cond ((number? x) (conc "#x" x))
                                          ((string? x) (conc "\x1b32m" x "\x1b0m"))
                                          (else x))) args)))))

(define flag/utf8 #x100)

(define (expect expected actual msg)
  (unless (equal? expected actual)
    (error (conc msg " (#x" (number->string actual 16) ")") actual))
  actual)

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

(define (make-seekable-port str)
  (let ((pos 0))
    (lambda (a)
      (case a
        ((#:port)
         (make-input-port (lambda () ;; read
                            (if (< pos (string-length str))
                                (let ((c (string-ref str pos)))
                                  (set! pos (+ 1 pos))
                                  c)
                                #!eof))    
                          (lambda () #t)       ;; ready?
                          (lambda () #t)))
        ((#:pos) pos)
        (else (set! pos a))))))

(define (parse-string-pool here seek)
     
  (define start (+ (here) -2))

  (expect #x001c (read-uint16) "expected string pool header #x0028, got")

  (define size (read-uint32)) (prn "string pool size " size " start " start " so end is " (+ size start) )
  (define chunk-end (+ size start #|here|#))

  (define strings#     (read-uint32)) (prn "string count " strings#)
  (define style#       (read-uint32)) (prn "style# " style#)
  (define flags        (read-uint32)) (prn "flags " flags)
  (define stringStart  (read-uint32)) (prn "stringStart " stringStart  " (pos " (here) ")")
  (define styleStart   (read-uint32)) (prn "styleStart " styleStart)

  ;; if it's not utf8, it's utf16
  (define utf8? (= flag/utf8 (bitwise-and flags flag/utf8)))
  (prn "utf8: " utf8?)

  (unless (= style# 0)
    (error "style# ≠ 0, not implemented"))
  (unless (= flag/utf8 (bitwise-ior flags flag/utf8))
    (error "only flag/utf8 implemented" flags))

  (prn "====================================================================================================")

  (define indecies
    (let loop ((n strings#)
               (indecies '()))
      (if (> n 0)
          (loop (- n 1) (cons (read-uint32) indecies))
          (reverse indecies))))

  (prn "indecies " (map (lambda (i) (number->string i 16)) indecies))

  (define (read-utf8-string-length)
    (let ((l (read-byte)))
      (if (= 0 (bitwise-and l #x80))
          #f            ;; utf8 length upcoming next
          (read-byte))) ;; skip the utf16 byte len. I dont get it either.

    (define len ;; reading utf8 now
      (let ((u (read-byte)))
        (if (= 0 (bitwise-and u #x80))
            u ;; utf8 was 1 byte
            (let ((v (read-byte)))
              (+ v (arithmetic-shift (bitwise-and #x7F u) 8))))))

    len)

  (define (read-utf16-string-length)
    (define len
      (let ((u (read-uint16)))
        (if (= 0 (bitwise-and u #x8000))
            u
            (+ (read-uint16) (arithmetic-shift (bitwise-and #x7FFF u) 16)))))
    (* 2 len))

  (define strings
    (let loop ((n strings#)
               (r '()))
      (if (> n 0)
          (let* ((len (if utf8?
                          (read-utf8-string-length)
                          (read-utf16-string-length)))
                 (str ( (if utf8? identity utf16->utf8)
                        (read-string len)))
                 (null (if utf8? (read-byte) (read-uint16))))
            (unless (= 0 null)
              (prn "expected #\\null but got " null))
            ;;(prn "string " n " (" len "): " (with-output-to-string (lambda () (write str))))
            (loop (- n 1) (cons str r)))
          (reverse r))))
    
  ;;(prn "strings " strings)
  (prn "chunk end at " chunk-end)
  (seek chunk-end)
  
  `(string-pool ,(if utf8? 'utf8 'utf16) ,strings))

(define (unparse-string-pool data)

  (define (grab) ;; TODO: cleanup
    (if (pair? data)
        (let ((obj (car data)))
          (set! data (cdr data))
          obj)
        (error "unexpected end of data stream")))
  
  (define (write-utf16-string-length len)
    (if (< len #x8000)
        (write-uint16 len)
        ;;(+ (read-uint16) (arithmetic-shift (bitwise-and #x7FFF len) 16))
        (error "utf16 big length not implemented")
        ))

  (let ((header (grab)))
    (unless (eq? header 'string-pool) (error "expecting 'string-pool" header)))

  (define utf8? (if (eq? (grab) 'utf8) #t #f))
  (unless (eq? #f utf8?) (error "utf8 not implemented"))

  (define strings (grab))

  (define start (+ (current-buffer-pos) -2)) ;; start of chunk, including uint16 type
  (write-uint16 #x001c)
  (define size (write-uint32 #xFFFFBBBB)) ;; fixed later

  (write-uint32 (length strings))       ;; strings#
  (write-uint32 0)                      ;; style#
  (write-uint32 0)                      ;; flags
  (define stringStart (write-uint32 0)) ;; fixed later
  (write-uint32 0)                      ;; styleStart

  ;; list of procedures to change its index, since actual index isn't
  ;; known until we start writing the strings
  (define indecies
    (map (lambda (str) 
           (write-uint32 0))
         strings))

  (define start-strings (current-buffer-pos))
  (stringStart (- (current-buffer-pos) start)) ;; relative to chunk start
  (for-each
   (lambda (str idx!)
     
     (idx! (- (current-buffer-pos) start-strings))
     
     (write-utf16-string-length (string-length str))
     (out (utf8->utf16 str))
     (out "\x00\x00"))
   strings indecies)

  (out (make-string (remainder (current-buffer-pos) 4) #\null))
  
  (size (- (current-buffer-pos) start)))

(define (parse-xml manifest)
  
  (define p (make-seekable-port manifest))
  (define emit
    (let ((result '()))
      (case-lambda (() (reverse result))
                   ((x) (set! result (cons x result))))))
  (parameterize ((current-input-port (p #:port)))

    (define (seek byte-offset) (p byte-offset))
    ;; ==================== /io ====================

    (expect #x00080003 (read-uint32) "bad header: ")
    
    (define len (read-uint32)) (prn "total len " len)
    (expect #x0001 (read-uint16) "oh no! expecting string-pool chunk first")
    (define sp (parse-string-pool (lambda () (p #:pos)) seek))
    (emit sp)
    (define rest (string->blob (read-string)))
    (emit rest)
    (emit)))

(define (unparse-xml* data)
  (define (pull)
    (if (pair? data)
        (let ((obj (car data)))
          (set! data (cdr data))
          obj)
        (error "unexpected end of data stream")))

  (write-uint32 #x00080003) ;; header
  
  (define len (write-uint32 0))
  
  (write-uint16 #x0001) ;; string-pool type
  (define sp (pull))
  (unparse-string-pool sp)

  (define rest (pull))
  (out rest)
  (len (current-buffer-pos)))

(define (unparse-xml data)
  (wotb (lambda () (unparse-xml* data))))
