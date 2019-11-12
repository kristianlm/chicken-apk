;; parsing string-pool of packaged android binary xml
;;
;;
;; also looked at https://github.com/iBotPeaches/Apktool/blob/3f85dfa6c9fc7f81a991b7104888f282a37a36cf/brut.apktool/apktool-lib/src/main/java/brut/androlib/res/decoder/StringBlock.java
;; but mostly https://github.com/skylot/jadx/blob/1830c273c08cf4bb3d06cbda61f5505bc3dab5d3/jadx-core/src/main/java/jadx/core/xmlgen/CommonBinaryParser.java

(import (only (chicken io) read-string)
        (only (chicken bitwise) bitwise-ior arithmetic-shift)
        (only (chicken port) make-input-port))

(define flag/utf8 #x100)

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

  (prn "indecies " indecies)

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


(define (unparse-string-pool strings #!optional utf8?)

  (when utf8? (error "utf8 not implemented"))

  (define (write-utf16-string-length len)
    (if (< len #x8000)
        (write-uint16 len)
        ;;(+ (read-uint16) (arithmetic-shift (bitwise-and #x7FFF len) 16))
        (error "utf16 big length not implemented")))

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
