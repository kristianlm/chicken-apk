(import chicken.string chicken.io matchable)

(define original/number->string number->string)
(set! ##sys#number->string
      (lambda (x #!optional base)
        (if base
            (original/number->string x base)
            (conc (original/number->string x 10) "\x1b[92m#;x"
                  (original/number->string x 16) "\x1b[0m"))))


(include "string-pool.scm")

(import (only (chicken pretty-print) pp))

(begin
  (define type/null #x0000)
  (define type/string-pool #x0001)
  (define type/first-chunk #x0100)
  (define type/start-namespace #x0100)
  (define type/end-namespace #x0101)
  (define type/start-element #x0102)
  (define type/end-element #x0103)
  (define type/cdata #x0104)
  (define type/last-chunk #x017f)
  (define type/resource-map #x0180)

  (define att/reference #x01)
  (define att/string #x03)
  (define att/dec #x10) ;; number as decimal string
  (define att/hex #x11) ;; number as hex string
  (define att/bool #x12)
  )

(define (decode-value value type sp-ref)
  (cond ((= type att/string) (sp-ref value))
        ((= type att/dec) value)
        ((= type att/hex) `(hex ,(number->string value 16)))
        ((= type att/reference) `(@@ ,value))
        ((= type att/bool) (if (= value #xffffffff) #t #f))
        (else (list '¿ type value))))

(define (parse-xml manifest)

  (define p (make-seekable-port manifest))
  (parameterize ((current-input-port (p #:port)))

    (define (seek byte-offset)
      (p byte-offset))
    ;; ==================== /io ====================

    (seek 0)

    (expect #x00080003 (read-uint32) "bad header: ")

    (define len (read-uint32)) (prn "len " len)

    (let loop ((tree '(())) ;; this is gonna be treecky
               (string-pool #f)
               (resource-map #f))

      (define type (read-uint16)) (prn "type " type)

      (define (sp-ref idx)
        (prn "index is " idx " sp is " string-pool)
        (if (= idx #xffffffff) #f
            (if (>= idx (length string-pool))
                (error (conc "out of range " idx) string-pool)
                (list-ref string-pool idx))))
      (define (push element) (cons (reverse element) tree))
      (define (pop) (cons (cons (reverse (car tree)) (cadr tree))
                          (cddr tree)))

      (cond ((eof-object? type)
             (prn "well done everyone")
             (pp tree))
            ((= type type/string-pool)
             (loop tree
                   (match (parse-string-pool (lambda () (p #:pos)) seek)
                     (('string-pool 'utf16 strings)
                      strings))
                   resource-map))
            ((= type type/resource-map)
             (let ()
               (expect 8 (read-uint16) "header size ≠ 8")

               (define size (read-uint32))
               (define len (quotient (- size 8) 4)) ;; what is going on?
               (prn "resource size " size ", len " len)
               (loop tree string-pool
                     (list-tabulate len (lambda (i) (read-uint32))))))
            ((= type type/start-namespace)
             (let ()
               (expect #x10 (read-uint16) "  expecting ns header #x10")
               (expect #x18 (read-uint32) "  expecting chunk header 24")
               (define line (read-uint32)) (prn "  line# " line)
               (define comment (sp-ref (read-uint32))) (prn "  comment " comment)
               (define prefix (sp-ref (read-uint32))) (prn "  prefix «" prefix "»")
               (define uri (sp-ref (read-uint32))) (prn "  uri «" uri "»")
               (loop (push `(@ns (,prefix ,uri))) string-pool resource-map)))
            ((= type type/end-namespace)
             (let ()
               (expect #x10 (read-uint16) "/namespace header ≠ #x10")
               (expect #x18 (read-uint32) "/namespace header chunk ≠ #x18")
               (define line (read-uint32))
               (define comment (read-uint32))
               (define prefix (read-uint32))
               (define uri (read-uint32))
               (loop (pop) string-pool resource-map)))
            ((= type type/start-element)
             (let ()
               (expect #x10 (read-uint16) "  element header size ≠ #x10")
               (define chunk-size-or-something-like-that (read-uint32)) (prn "chunk-size " chunk-size-or-something-like-that)
               (define line (read-uint32))    (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " (sp-ref comment))
               (define ns (read-uint32))      (prn "  ns: " (sp-ref ns))
               (define tag (sp-ref (read-uint32))) (prn "  tag: " tag)
               (expect #x14 (read-uint16) "ns-name sttribute start ≠ #x14")
               (expect #x14 (read-uint16) "ns-name attribute size ≠ #x14")
               (define att-count (read-uint16)) (prn "  att-count " att-count)
               (define id-index (read-uint16))
               (define class-index (read-uint16))
               (define style-index (read-uint16))
               (define element
                 `(,(string->symbol tag)
                   (@ ,@(reverse
                         (list-tabulate
                          att-count
                          (lambda (i)
                            (define att-ns (read-uint32)) (prn "  att-ns " (sp-ref att-ns))
                            (define att-name (sp-ref (read-uint32))) ;;(prn "  att-name " (sp-ref att-name))
                            (define att-raw-value (read-uint32))
                            (expect #x08 (read-uint16) "attribute value size ≠ #x08")
                            (expect 0 (read-byte) "res0 ≠ 0")
                            (define att-type (read-byte)) (prn "  att-type " att-type)
                            (define att-value (decode-value (read-uint32) att-type sp-ref))
                            (prn "  ## " att-name "=" att-value)
                            (list (string->symbol att-name) att-value)))))))
               (loop (push element) string-pool resource-map)))
            ((= type type/end-element)
             (let ()
               (expect #x10 (read-uint16) "  element header size ≠ #x10")
               (expect #x18 (read-uint32) "  header chunk ≠ #x18")
               (define line (read-uint32))    (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " (sp-ref comment))
               (define ns (read-uint32))      (prn "  ns: " (sp-ref ns))
               (define tag (sp-ref (read-uint32))) (prn "  tag: " tag)
               (loop (pop) string-pool resource-map)))
            (else
             (prn "  error; don't know how to handle type " type)
             (pp tree)))
      )
    ))


(define (unparse-xml* data)
  (define (pull)
    (if (pair? data)
        (let ((obj (car data)))
          (set! data (cdr data))
          obj)
        (error "unexpected end of data stream")))

  (write-uint32 #x00080003) ;; header

  (define len (write-uint32 0)) ;; fixed later

  (write-uint16 #x0001) ;; string-pool type
  (define sp (pull))
  (unparse-string-pool sp)

  (define rest (pull))
  (out rest)
  (len (current-buffer-pos)))


(define (unparse-xml data)
  (wotb (lambda () (unparse-xml* data))))

(pp (parse-xml (read-string)))
