(import (only (chicken gc) set-finalizer!)
        (only (chicken port) make-input-port port-for-each set-port-name!)
        (only (chicken string) conc)
        (only (chicken time posix) seconds->utc-time)
        (only (chicken memory representation) number-of-bytes)
;;        (only (chicken io) )
        srfi-4)
(foreign-declare "

#define Z_SOLO
#define IOAPI_NO_64

#include \"minizip/ioapi.c\"
#include \"minizip/zip.c\"
#include \"minizip/unzip.c\"
")

(define-record unzFile pointer first?)
(define-record zipFile pointer closed?)

(define-foreign-type unzFile c-pointer
  (lambda (x) (unzFile-pointer x))
  (lambda (x) (make-unzFile x #f)))

(define-foreign-type zipFile c-pointer
  (lambda (x) (zipFile-pointer x))
  (lambda (x) (make-zipFile x #f)))

(define (CHECK ret msg what)
  (if (eq? ret (foreign-value "UNZ_OK" int))
      (void)
      (error msg what)))

(define (unzip-close* file)
  (CHECK ((foreign-lambda int "unzClose" unzFile) file) "unable to close file" file))

(define (unzipper-read! uz dest size)
  ((foreign-lambda int "unzReadCurrentFile" unzFile scheme-pointer int) uz dest size))

(define (unzipper-open! uz)
  (CHECK ((foreign-lambda int "unzOpenCurrentFile" unzFile) uz) 'unzipper-open! uz))

(define (unzipper-close! uz)
  (let ((ret ((foreign-lambda int "unzCloseCurrentFile" unzFile) uz)))
    (cond ((eq? ret (foreign-value "UNZ_CRCERROR" int)) #f)
          ((eq? ret (foreign-value "UNZ_OK" int)) #t)
          (else (error "error closing unzipper file" uz ret)))))

(define (unzipper-first! uz)
  (CHECK ((foreign-lambda int "unzGoToFirstFile" unzFile) uz) 'unzipper-first uz)
  (unzFile-first?-set! uz #t))

(define (unzipper-port* uz)
  (define port
    (make-input-port (lambda ()
                       (let* ((s (make-string 1))
                              (read (unzipper-read! uz s 1)))
                         (cond ((= read 0) #!eof)
                               ((< read 0) (error "could not read from file" uz read))
                               (else (string-ref s 0)))))
                     (lambda () #t) ;; ready?
                     (lambda () (unzipper-close! uz))))
  (set-port-name! port (conc "unzipper " (unzipper-filename uz)))
  port)

(define (unzipper-next! uz #!optional (eof #f))
  (let ((ret (if (unzFile-first? uz)
                 (foreign-value "UNZ_OK" int) ;; pretend-call went ok
                 ((foreign-lambda int "unzGoToNextFile" unzFile) uz))))
    (unzFile-first?-set! uz #f)
    (cond ((eq? ret (foreign-value "UNZ_END_OF_LIST_OF_FILE" int)) eof)
          ((eq? ret (foreign-value "UNZ_OK" int))
           (unzipper-open! uz)
           (unzipper-port* uz))
          (else (error "could not go to next file" uz ret)))))

(define (unzipper pathname)
  (let ((obj ((foreign-lambda unzFile "unzOpen" c-string) pathname)))
    (when (eq? #f (unzFile-pointer obj)) (error "could not open" pathname 'unzipper))
    (unzipper-first! obj)
    (set-finalizer! obj unzip-close*)))

(define (unzipper-filename uz)
  ((foreign-lambda* c-string ((unzFile uz)) "
  char fname[16384]; // TODO check ret below
  unzGetCurrentFileInfo(uz, 0, fname, 16384, NULL, 0, NULL, 0);
  return(fname);")
   uz))

(define (unzipper-size-uncompressed uz)
  ((foreign-lambda* unsigned-long ((unzFile uz)) "
  unz_file_info info; // TODO check ret
  unzGetCurrentFileInfo(uz, &info, 0, 0, NULL, 0, NULL, 0);
  return(info.uncompressed_size);") uz))

(define (unzipper-compression-method uz)
  ((foreign-lambda* unsigned-long ((unzFile uz)) "
  unz_file_info info; // TODO check ret
  unzGetCurrentFileInfo(uz, &info, 0, 0, NULL, 0, NULL, 0);
  return(info.compression_method);") uz))

(define (unzip-walk path proc)
  (let ((uz (unzipper path)))
    (port-for-each (lambda (port) (proc uz (unzipper-filename uz) port))
                   (lambda () (unzipper-next! uz #!eof)))))

;;; ==================== zipper ====================

;; close but no sigar
;; idempotent
(define (zip-close z #!optional (comment #f) )
  (unless (zipFile-closed? z)
    (zipFile-closed?-set! z #t)
    ((foreign-lambda int "zipClose" zipFile (const c-string)) z comment)))

(define (zipper pathname #!optional (append 'create))
  (let ((z ((foreign-lambda zipFile "zipOpen" c-string int)
            pathname
            (case append
              ((create)      (foreign-value "APPEND_STATUS_CREATE"      int))
              ((createafter) (foreign-value "APPEND_STATUS_CREATEAFTER" int))
              ((addinzip)    (foreign-value "APPEND_STATUS_ADDINZIP"    int))
              (else (error "append ∉ {create createafter addinzip}" append))))))
    (if (equal? #f (zipFile-pointer z))
        (error "could not create zipper" pathname)
        (set-finalizer! z zip-close))))

(define zipper-new*
  (foreign-lambda* int ((zipFile z) (c-string filename)
                        (int method) (int level)
                        (int sec) (int min) (int hour) (int mday) (int mon) (int year))
                   "
  zip_fileinfo zipfi;
  zipfi.tmz_date.tm_sec = sec;
  zipfi.tmz_date.tm_min = min;
  zipfi.tmz_date.tm_hour = hour;
  zipfi.tmz_date.tm_mday = mday;
  zipfi.tmz_date.tm_mon = mon;
  zipfi.tmz_date.tm_year = year;
  zipfi.dosDate = 0;
  zipfi.external_fa = 0;
  zipfi.internal_fa = 0;
  return(zipOpenNewFileInZip(z, filename, &zipfi, NULL, 0, NULL, 0, NULL, method, level));
" ))

(define (zipper-new z filename #!key (method 'deflated) level (time (seconds->utc-time)))
  (zipper-new* z filename
                (case method
                  ((deflated) (foreign-value "Z_DEFLATED" int))
                  ((none #f)  0)
                  (else (error "method must be 'deflated or #f (none)" method)))
                (case level ;; numeric value 0-9 (Z_DEFAULT_COMPRESSION = -1)
                  ((#f) (foreign-value "Z_DEFAULT_COMPRESSION" int))
                  (else level))
                (vector-ref time 0)            ;; sec
                (vector-ref time 1)            ;; min
                (vector-ref time 2)            ;; hour
                (vector-ref time 3)            ;; mday
                (vector-ref time 4)            ;; month
                (+ 1900 (vector-ref time 5)))) ;; year

(define (zipper-write z str #!optional (len (number-of-bytes str)))
  ((foreign-lambda int "zipWriteInFileInZip" zipFile scheme-pointer unsigned-int) z str len))
(define (zipper-close z)
  (CHECK ((foreign-lambda int "zipCloseFileInZip" zipFile) z) "could not close zipper" 'zipper-close))


(define-record-printer unzFile
  (lambda (x p) (display (conc  "#<unzipper>") p)))
