(import (only (chicken string) conc substring-index)
        (only (chicken irregex) irregex-match)
        (only (chicken io) read-string)
        (only (chicken port) copy-port)
        (only (chicken file) file-exists? find-files move-file)
        (only (chicken file posix) regular-file?)
        (only (chicken pathname) decompose-pathname make-pathname)
        (android minizip)
        (android apk)
        (android bxml))

(define final.apk "../chicken-repl.apk")

;; android-specific: make sure *all* .so files under ./lib have a
;; prefix "lib"
(find-files
 "lib"
 #:test regular-file?
 #:action
 (lambda (path _)
   (receive (dir file ext) (decompose-pathname path)
     (unless (eq? 0 (substring-index "lib" file))
       (print "moving " file " => " (conc "lib" file))
       (move-file path (make-pathname dir (conc "lib" file) ext))))))

(define (zipper-copy z port)
  (copy-port port (zipper-port z)))

(with-zipper
 "tmp-unaligned.apk"
 (lambda (z)
   (print "repackaging apk => tmp-unaligned.apk")
   ;; process original files
   (unzip-for-each
    "../template/app/build/outputs/apk/debug/app-debug.apk"
    (lambda (uz path port)
      (define (copy!) ;; copy source in zip into target zip
        (zipper-new z path #:method (unzipper-method uz))
        (zipper-copy z port))

      ;;(print "processing " path)
      (cond ((irregex-match `(: (* any) "libtemplate.so") path) (copy!))
            ((substring-index "lib/" path)
             (if (file-exists? path)
                 #f
                 (copy!)))
            ((substring-index "META-INF" path) #f) ;; skip old signature

            ((equal? path "AndroidManifest.xml")
             (print "rewriting manifest ...")
             (let ((ibax (bxml->ibax (read-string #f port))))
               ;; (begin (import chicken.pretty-print) (pp ibax))
               (display
                (ibax->bxml ibax) ;; string
                (zipper-new z path #:method (unzipper-method uz)) ;; port
                )))

            (else (copy!)))))

   (print "adding files under ./lib")
   (find-files "lib"
               #:test regular-file?
               #:action
               (lambda (path _)
                 (zipper-new z path)
                 (call-with-input-file path (lambda (p) (zipper-copy z p)))))))

(print "signing ...")  (apk-sign "tmp-unaligned.apk")
(print "aligning ...") (apk-align "tmp-unaligned.apk" final.apk)
(print "done: " final.apk)
