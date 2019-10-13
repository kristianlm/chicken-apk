;;; android binary xml parsing
;;;
;;; parsing an undocumented binary format is non-trivial, so this is
;;; done in two stages: blob -> ibax -> sxml
;;;
;;; think of blob => machine code
;;;          ibax => assembly      (intermediate binary android xml)
;;;          sxml => some programming language
;;;
;;; ibax typically looks like this:
;;;
;;; ((string-pool utf16 ("label" "icon" "name" "debuggable" "minSdkVersion" "…"))
;;;  (resource-map (16844147 16844146 16844076)) ;; don't know what this is
;;;  (<ns> (str 17) (str 22)) ;; prefix url
;;;    (<element> (str #f) (str 24) (((str 5) 1) ((str 11) 28))) ;; ns tag (attributes)
;;;      (<element> (str #f) (str 30)) ;; always of type (str x)
;;;      (</element> (str #f) (str 30))
;;;    (</element> (str #f) (str 24))
;;;  (</ns> (str 17) (str 22)))
;;;
;;; where (str n) points to the nth element in the string-pool. we
;;; assume string-pool declarations come before references to them
;;; (seems to be the case in the binary output I've seen).
;;;
;;; sxml typically looks like this:
;;;
;;; (@ns (android "http://schemas.android.com/apk/res/android")
;;;      (manifest
;;;        (@ (android versionCode 1)
;;;           (android versionName "1.0")
;;;           (android compileSdkVersion 28)
;;;           (android compileSdkVersionCodename "9")
;;;           (package "org.call_cc.template.sotest")
;;;           (platformBuildVersionCode 28)
;;;           (platformBuildVersionName 9))
;;;        (uses-sdk (@ (minSdkVersion 21) (targetSdkVersion 28)))
;;;        (application
;;;          (@ (label (ref 2131099648))
;;;             (icon (ref 2131034112))
;;;             (debuggable #t)
;;;             (allowBackup #t)
;;;             (supportsRtl #t)
;;;             (roundIcon (ref 2131034113)))
;;;          (activity
;;;            (@ (android name "org.call_cc.template.sotest.MainActivity"))
;;;            (intent-filter
;;;              (action   (@ (android name "android.intent.action.MAIN")))
;;;              (category (@ (android name "android.intent.category.LAUNCHER"))))))))

(import (only chicken.string conc)
        (only chicken.io read-string)
        (only matchable match match-lambda)
        (only srfi-1 unfold filter))

;; OBS: we need buffer.scm and friends

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

(define (decode-value value type)
  (cond ((= type att/string) `(str ,value))
        ((= type att/dec) value)
        ((= type att/hex) `(hex ,value))
        ((= type att/reference) `(ref ,value))
        ((= type att/bool) (if (= value #xffffffff) #t #f))
        (else `(¿ type value))))

;; I'm tierd of seeing (str 4294967295)
(define (str index) `(str ,(if (= index #xffffffff) #f index)))

;; bxml is a string representation of android's binary xml
(define (bxml->ibax bxml)

  (define p (make-seekable-port bxml))
  (parameterize ((current-input-port (p #:port)))

    (define (seek byte-offset)
      (p byte-offset))
    ;; ==================== /io ====================

    (seek 0)

    (expect #x00080003 (read-uint32) "bad header: ")

    (define len (read-uint32)) (prn "len " len)

    (let loop ((tree '()))

      (define type (read-uint16)) (prn "type " type)

      (cond ((eof-object? type)
             (prn "well done everyone")
             (reverse tree))

            ((= type type/string-pool)
             (loop (cons (parse-string-pool (lambda () (p #:pos)) seek)
                         tree)))
            ((= type type/resource-map)
             (let ()
               (expect 8 (read-uint16) "header size ≠ 8")

               (define size (read-uint32))
               (define len (quotient (- size 8) 4)) ;; what is going on?
               (prn "resource size " size ", len " len)
               (define mapping
                 (unfold (lambda (x) (>= x len))
                         (lambda (x) (read-uint32))
                         add1 0))
               (loop (cons `(resource-map ,mapping) tree))))

            ((= type type/start-namespace)
             (let ()
               (expect #x10 (read-uint16) "  expecting ns header #x10")
               (expect #x18 (read-uint32) "  expecting chunk header 24")
               (define line (read-uint32)) (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " comment)
               (define prefix (read-uint32)) (prn "  prefix «" prefix "»")
               (define uri (read-uint32)) (prn "  uri «" uri "»")
               (loop (cons `(<ns> ,(str prefix) ,(str uri))
                           tree))))
            ((= type type/end-namespace)
             (let ()
               (expect #x10 (read-uint16) "/namespace header ≠ #x10")
               (expect #x18 (read-uint32) "/namespace header chunk ≠ #x18")
               (define line (read-uint32))
               (define comment (read-uint32))
               (define prefix (read-uint32))
               (define uri (read-uint32))
               (loop (cons `(</ns> ,(str prefix) ,(str uri)) tree))))

            ((= type type/start-element)
             (let ()
               (expect #x10 (read-uint16) "  element header size ≠ #x10")
               (define chunk-size (read-uint32)) (prn "  chunk-size " chunk-size)
               (define line (read-uint32))    (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " comment)
               (define ns (read-uint32))      (prn "  ns: " ns)
               (define tag (read-uint32)) (prn "  tag: " tag)
               (expect #x14 (read-uint16) "ns-name sttribute start ≠ #x14")
               (expect #x14 (read-uint16) "ns-name attribute size ≠ #x14")
               (define att-count (read-uint16)) (prn "  att-count " att-count)
               (define id-index (read-uint16)) (prn "  id-index: " id-index)
               (define class-index (read-uint16)) (prn "  class-index: " class-index)
               (define style-index (read-uint16)) (prn "  style-index: " style-index)
               (define element
                 `(<element>
                   ,(str ns) ,(str tag)
                   (@ ,@(unfold
                         (lambda (x) (>= x att-count))
                         (lambda (i)
                           (define att-ns (read-uint32)) (prn "    att-ns " att-ns)
                           (define att-name (read-uint32)) ;;(prn "  att-name " (sp-ref att-name))
                           (define att-raw-value (read-uint32))
                           (expect #x08 (read-uint16) "attribute value size ≠ #x08")
                           (expect 0 (read-byte) "res0 ≠ 0")
                           (define att-type (read-byte)) (prn "    att-type " att-type)
                           (define att-value (decode-value (read-uint32) att-type))
                           (prn "    ## " att-name "=" att-value)
                           `(,(str att-ns) ,(str att-name) ,att-value))
                         add1 0))))
               (loop (cons element tree))))

            ((= type type/end-element)
             (let ()
               (expect #x10 (read-uint16) "  element header size ≠ #x10")
               (expect #x18 (read-uint32) "  header chunk ≠ #x18")
               (define line (read-uint32))    (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " comment)
               (define ns (read-uint32))      (prn "  ns: " ns)
               (define tag (read-uint32)) (prn "  tag: " tag)
               (loop (cons `(</element> ,(str ns) ,(str tag)) tree))))

            (else
             (prn "  error; don't know how to handle type " type)
             (pp tree))))))

(define (write-ibax ibax)

  (write-uint32 #x00080003) ;; header
  (define len (write-uint32 0))

  (let loop ((ibax ibax))
    (if (pair? ibax)
        (match (car ibax)

          (('string-pool 'utf8 (strings ...)) (error "utf8 unparsing not implemented"))

          (('string-pool 'utf16 (strings ...))
           (write-uint16 type/string-pool) ;; string-pool type
           (unparse-string-pool strings)
           (loop (cdr ibax)))

          (('resource-map (mapping ...))
           (write-uint16 type/resource-map)
           (write-uint16 #x08)                         ;; header size
           (write-uint32 (+ 8 (* 4 (length mapping)))) ;; mystery counting technique
           (for-each write-uint32 mapping)
           (loop (cdr ibax)))

          (('<ns>  ('str prefix) ('str url))
           (let ()
             (write-uint16 type/start-namespace)
             (write-uint16 #x10) ;; header element size
             (define here (current-buffer-pos))
             (define chunk-size (write-uint32 0)) ;; fixed later
             (write-uint32 0)                     ;; line
             (write-uint32 #xffffffff)            ;; comment
             (write-uint32 (or prefix #xffffffff))
             (write-uint32 (or url #xffffffff))
             (chunk-size (+ 4 (- (current-buffer-pos) here)))
             (loop (cdr ibax))))

          (('</ns>  ('str prefix) ('str url))
           (let ()
             (write-uint16 type/end-namespace)
             (write-uint16 #x10) ;; header element size
             (define here (current-buffer-pos))
             (define chunk-size (write-uint32 0)) ;; fixed later
             (write-uint32 0)                     ;; line
             (write-uint32 #xffffffff)            ;; comment
             (write-uint32 (or prefix #xffffffff))
             (write-uint32 (or url #xffffffff))
             (chunk-size (+ 4 (- (current-buffer-pos) here)))
             (loop (cdr ibax))))

          (('<element> ('str ns) ('str tag) ('@ attributes ...))
           (let ()
             (write-uint16 type/start-element)
             (write-uint16 #x10) ;; element header size
             (define here (current-buffer-pos))
             (define chunk-size (write-uint32 #x00))
             (write-uint32 #xffffffff)          ;; line
             (write-uint32 #xffffffff)          ;; comment
             (write-uint32 (or ns #xffffffff))  ;; element ns
             (write-uint32 (or tag #xffffffff)) ;; tag
             (write-uint16 #x14)                ;; start?
             (write-uint16 #x14)                ;; stop?
             (write-uint16 (length attributes)) ;; att-count
             (write-uint16 0)                   ;; id-index
             (write-uint16 0)                   ;; class-index
             (write-uint16 0)                   ;; style-index
             (for-each (match-lambda
                        ( (('str ns) ('str key) val)
                          (receive (val type)
                              (match val
                                (#t (values #xffffffff att/bool))
                                (#f (values #x00000000 att/bool))
                                (('str val) (values val att/string))
                                (('ref val) (values val att/reference))
                                ((? number? val) (values val att/dec))
                                (else (error "unknown attribute value type " val)))
                            (write-uint32 (or ns #xffffffff)) ;; attribute ns
                            (write-uint32 key)  ;; name
                            (write-uint32 val)  ;; raw-value
                            (write-uint16 #x08) ;; value size
                            (write-uint8 #x00)  ;; res0
                            (write-uint8 type)  ;; type
                            (write-uint32 val)  ;; value
                            ))
                        (else (error "no matching patt" else)))
                       attributes)
             (chunk-size (+ 4 (- (current-buffer-pos) here)))
             (loop (cdr ibax))))

          (('</element> ('str ns) ('str tag))
           (let ()
             (write-uint16 type/end-element)
             (write-uint16 #x10) ;; element header size
             (define here (current-buffer-pos))
             (define chunk-size (write-uint32 #x00))
             (write-uint32 #xffffffff)          ;; line
             (write-uint32 #xffffffff)          ;; comment
             (write-uint32 (or ns #xffffffff))  ;; ns
             (write-uint32 (or tag #xffffffff)) ;; tag
             (chunk-size (+ 4 (- (current-buffer-pos) here)))
             (loop (cdr ibax))))

          (('))

          (else (error "unknown: " else)
                (loop (cdr ibax))))))

  (len (current-buffer-pos)))

(define (ibax->bxml ibax)
  (wotb (lambda () (write-ibax ibax))))

;; turn ibax into nested sxml
(define (ibax->sxml ibax)

  (let loop ((ibax ibax)
             (sp #f)
             (rm #f)
             (ns '()) ;; alist ((prefix "http://..") ...)
             (tree '(())))

    (define (push element) (cons (reverse element) tree))
    (define (pop) (cons (cons (reverse (car tree)) (cadr tree))
                        (cddr tree)))
    (define (decode value)
      (cond ((boolean? value) value)
            ((number? value) value)
            (else
             (match value
               (('str x) (if (eq? x #f) #f (list-ref sp x)))
               (('hex x) x) ;; ok to loose this was stored as hex?
               (('ref x) value) ;; don't know how to resolve refs to resources.arsc
               (else (error "unknown value" value))))))

    (define (ns-find uri) ;; uri is string
      (cond ((assoc uri ns) => cadr)
            (else (error "namespace not found" uri ns))))

    (if (pair? ibax)
        (match (car ibax)

          (('string-pool encoding strings)
           (loop (cdr ibax) strings rm ns tree))

          (('resource-map rm)
           (loop (cdr ibax) sp rm ns tree))

          (('<ns> prefix uri)
           (loop (cdr ibax) sp rm
                 (cons `(,(decode uri) ,(string->symbol (decode prefix))) ns)
                 (push `(@ns (,(string->symbol (decode prefix)) ,(decode uri))) )))

          (('</ns> prefix uri)
           (loop (cdr ibax) sp rm
                 (filter (match-lambda ((pfx uri*) (equal? uri uri*))
                                       (else (error "invalid namespace entry" else)))
                         ns)
                 (pop)))

          (('<element> element-ns tag ('@ attributes ...))
           (loop (cdr ibax) sp rm ns
                 (push `(,(string->symbol (decode tag))
                         (@ ,@(map (match-lambda
                                    ((nsuri an av)
                                     (let ((uri (decode nsuri)))
                                       (if uri
                                           (list (ns-find uri)
                                                 (string->symbol (decode an))
                                                 (decode av))
                                           (list (string->symbol (decode an))
                                                 (decode av))))))
                                   attributes))))))

          (('</element> element-ns tag)
           (loop (cdr ibax) sp rm ns (pop)))

          (else
           (prn "  error; don't know how to handle ibax" (car ibax))
           (pp tree)))
        ;; final result:
        (caar tree))))


(define (sxml->ibax sxml)

  (define sp '())
  (define (str! str) ;; append to string pool
    (unless (string? str) (error "can not add non-string to sp" str))
    (let ((index (length sp)))
      (set! sp (cons str sp))
      `(str ,index)))

  (define ibax '())
  (define (ins! ibax1)
    (set! ibax (cons ibax1 ibax)))

  (let loop ((sxmls (list sxml))
             (ns '())) ;; alist ((prefix (str %uri)) ...) (not ordered like above)

    (if (pair? sxmls)
        (match (car sxmls)
          (('@ns (prefix uri) rest ...)
           (let ((prefix* (str! (symbol->string prefix)))
                 (uri*    (str! uri)))
             (ins!  `(<ns> ,prefix* ,uri*))
             (loop rest (cons `(,prefix ,uri*) ns))
             (ins!  `(</ns> ,prefix* ,uri*))
             (loop (cdr sxmls) ns)))

          ;; typical attributes: ( (android label "hello" ) (package "com.example") )
          ((element ('@ attributes ...) rest ...)
           (let ()

             ;; add strings to sp, turn everything to uint32
             (define (encode x)
               (cond ((string? x) (str! x))
                     ((symbol? x) (str! (symbol->string x)))
                     (else x)))

             (define (ns-find pfx) ;; pfx is prefix symbol
               (cadr (assoc pfx ns)))

             (define atts
               (map (match-lambda

                     ((ans key val) ;; attribute ns
                      `(,(ns-find ans) ,(encode key) ,(encode val)))

                     ((key val) ;; no attribute namespace
                      `((str #f) ,(encode key) ,(encode val)))

                     (else (error "invalid attibute pattern" else)))
                    attributes))
             (set! element (str! (symbol->string element)))
             (ins! `(<element> (str #f) ,element (@ ,@atts)))
             (loop rest ns)
             (ins! `(</element> (str #f) ,element))
             (loop (cdr sxmls) ns)))

          ((element rest ...)
           (error "missing (@ attributes ...) in element" (list element rest)))

          (else (error "internal sxml-ibax error: no match" sxmls)))

        `((string-pool utf16 ,(reverse sp))
          (resource-map ())
          ,@(reverse ibax)))))

(define sxml->bxml (o ibax->bxml sxml->ibax))
(define bxml->sxml (o ibax->sxml bxml->ibax))
