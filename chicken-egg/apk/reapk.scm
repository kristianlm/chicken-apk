(import (chicken foreign)
        (chicken memory)
        (only (chicken string) conc substring-index)
        (only (chicken process) system)
        (only (chicken process-context) get-environment-variable)
        (only (chicken file) file-exists?)
        (only (chicken pathname) make-pathname)
        matchable)

(define (pointer-pointer-ref x)
  ((foreign-lambda* c-pointer (((c-pointer (c-pointer void)) x)) "return(*x);")
   x))

(define (pointer-pointer-set! x v)
  ((foreign-lambda* void ( ((c-pointer (c-pointer void)) x) (c-pointer v)) "*x = v;")
   x v))

(foreign-declare "

// callback used by repackage.c
C_word edit_apk_file(C_word user, char* fname, char** data, uint64_t* size);

#define Z_SOLO
#define IOAPI_NO_64

#include \"minizip/ioapi.c\"
#include \"minizip/zip.c\"
#include \"minizip/unzip.c\"
#include \"apk/repackage.c\"
")

(define-external
  (edit_apk_file (scheme-object user)
                 (c-string fname)
                 ((c-pointer (c-pointer char)) &data)
                 ((c-pointer unsigned-integer64) &size))
  scheme-object ;; return type

  (print "&data=" &data)
  (define data (pointer-pointer-ref &data))
  (print "data=" data)
  
  (define size (pointer-u64-ref &size))
  (define original (make-string size))
  (move-memory! data original size)
  (free data)

  (define str (user fname original))
  (pointer-pointer-set! &data #f) ;; mark as skipped
  (when str
    (let ((result (allocate (string-length str))))
      (move-memory! str result)
      (pointer-pointer-set! &data result)
      (pointer-u64-set! &size (string-length str))))
  
  (print "@ "  " " size " bytes" )
  user)

(define (rezip source.apk dest.apk process_fname_data)
  (unless (procedure? process_fname_data) (error "expecting edit procedure" process_fname_data))
  ((foreign-safe-lambda int "rezip" c-string c-string scheme-object)
   source.apk dest.apk process_fname_data))

(define (discard-meta+lib file)
  (cond ((eq? 0 (substring-index "META-INF/" file)) #f)
        ((eq? 0 (substring-index "lib/" file)) #f)
        (else #t)))

(define (apk-repackage) (error "TODO"))
;; (define (apk-repackage src dst renaming #!optional (keep-file? discard-meta+lib))
;;   (rezip src dst
;;          (if (procedure? renaming) renaming
;;              (lambda (fname data)
;;                (and (keep-file? fname)
;;                     (cond ((assoc fname renaming) =>
;;                            (lambda (renames*)
;;                              (define renames (cdr renames*))
;;                              (unparse-xml
;;                               (string-pool-replace
;;                                (parse-xml data)
;;                                (lambda (str)
;;                                  (print "*****" renames)
;;                                  (cond ((assoc str renames) =>
;;                                         (lambda (old+new)
;;                                           (cadr old+new)))
;;                                        (else str)))))))
;;                           (else data)))))))

(define (apk-sign filename #!optional
                  (keystore (make-pathname (list (get-environment-variable "HOME")
                                                 ".android")
                                           "debug.keystore"))
                  (storepass "android")
                  (alias "androiddebugkey"))
  (if (file-exists? keystore)
      (system (conc "jarsigner -digestalg SHA-256 -sigalg SHA256withRSA"
                    " -keystore \"" keystore "\""
                    " -storepass \"" storepass "\""
                    " " filename
                    "  " alias))
      (error (conc "cannot find keystore " keystore "\ncreate one with:\n  keytool -genkey -v"
                   " -keystore \"" keystore "\""
                   " -alias \"" alias "\""
                   " -keyalg RSA -keysize 2048 -validity 10000\n  default password is 'android'\n"))))

(define apk-align (foreign-lambda void "apk_align" c-string c-string))
