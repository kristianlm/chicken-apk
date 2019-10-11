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
;;; sxml typically looks like this:
;;;
;;; (@ns ("android" "http://schemas.android.com/apk/res/android")
;;;      (manifest
;;;        (@ (versionCode 1)
;;;           (versionName "1.0")
;;;           (compileSdkVersion 28)
;;;           (compileSdkVersionCodename "9")
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
;;;            (@ (name "org.call_cc.template.sotest.MainActivity"))
;;;            (intent-filter
;;;              (action (@ (name "android.intent.action.MAIN")))
;;;              (category (@ (name "android.intent.category.LAUNCHER"))))))))


(import chicken.string chicken.io matchable)

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

(define (decode-value value type)
  (cond ((= type att/string) `(str ,value))
        ((= type att/dec) value)
        ((= type att/hex) `(hex ,value))
        ((= type att/reference) `(ref ,value))
        ((= type att/bool) (if (= value #xffffffff) #t #f))
        (else `(¿ type value))))

;; I'm tierd of seeing (str 4294967295)
(define (str index) `(str ,(if (= index #xffffffff) #f index)))

(define (parse-xml* manifest)

  (define p (make-seekable-port manifest))
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
               (loop (cons `(resource-map ,(list-tabulate len (lambda (i) (read-uint32)))) tree))))
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
               (define chunk-size-or-something-like-that (read-uint32)) (prn "chunk-size " chunk-size-or-something-like-that)
               (define line (read-uint32))    (prn "  line# " line)
               (define comment (read-uint32)) (prn "  comment " comment)
               (define ns (read-uint32))      (prn "  ns: " ns)
               (define tag (read-uint32)) (prn "  tag: " tag)
               (expect #x14 (read-uint16) "ns-name sttribute start ≠ #x14")
               (expect #x14 (read-uint16) "ns-name attribute size ≠ #x14")
               (define att-count (read-uint16)) (prn "  att-count " att-count)
               (define id-index (read-uint16)) (prn "id-index: " id-index)
               (define class-index (read-uint16)) (prn "class-index: " class-index)
               (define style-index (read-uint16)) (prn "style-index: " style-index)
               (define element
                 `(<element>
                   ,(str ns) ,(str tag)
                   ,(reverse
                     (list-tabulate
                      att-count
                      (lambda (i)
                        (define att-ns (read-uint32)) (prn "  att-ns " att-ns)
                        (define att-name (read-uint32)) ;;(prn "  att-name " (sp-ref att-name))
                        (define att-raw-value (read-uint32))
                        (expect #x08 (read-uint16) "attribute value size ≠ #x08")
                        (expect 0 (read-byte) "res0 ≠ 0")
                        (define att-type (read-byte)) (prn "  att-type " att-type)
                        (define att-value (decode-value (read-uint32) att-type))
                        (prn "  ## " att-name "=" att-value)
                        `(,(str att-name) ,att-value))))))
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

;; turn ibax into nested sxml
(define (parse-xml manifest)

  (define ibax (parse-xml* manifest))


  (let loop ((ibax ibax)
             (sp #f)
             (rm #f)
             (tree '(())))

    (define (push element) (cons (reverse element) tree))
    (define (pop) (cons (cons (reverse (car tree)) (cadr tree))
                        (cddr tree)))
    (define (decode value)
      (cond ((boolean? value) value)
            ((number? value) value)
            (else
             (match value
               (('str x) (list-ref sp x))
               (('hex x) x) ;; ok to loose this was stored as hex?
               (('ref x) value) ;; don't know how to resolve refs to resources.arsc
               (else (error "unknown value" value))))))

    (if (pair? ibax)
        (match (car ibax)

          (('string-pool encoding strings)
           (loop (cdr ibax) strings rm tree))

          (('resource-map rm)
           (loop (cdr ibax) sp rm tree))

          (('<ns> prefix uri)
           (loop (cdr ibax) sp rm (push `(@ns (,(decode prefix) ,(decode uri))) )))

          (('</ns> prefix uri)
           (loop (cdr ibax) sp rm (pop)))

          (('<element> ns tag attributes)
           (loop (cdr ibax) sp rm
                 (push `(,(string->symbol (decode tag))
                         ,@(if (null? attributes)
                               '()
                               `((@ ,@(map (lambda (lst) (list (string->symbol (decode (car lst)))
                                                               (decode (cadr lst))))
                                           attributes))))))))

          (('</element> ns tag)
           (loop (cdr ibax) sp rm (pop)))

          (else
           (prn "  error; don't know how to handle ibax" (car ibax))
           (pp tree)))
        ;; final result:
        (caar tree))))


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
