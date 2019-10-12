(import (only (chicken gc) set-finalizer!)
        (only (chicken port) make-input-port port-for-each set-port-name!)
        (only (chicken string) conc)
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

(define-foreign-type unzFile c-pointer
  (lambda (x) (unzFile-pointer x))
  (lambda (x) (make-unzFile x #f)))

;;(define-foreign-type zipFile c-pointer)

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
    (port-for-each (lambda (p) (proc uz (unzipper-filename uz) p))
                   (lambda () (unzipper-next! uz #!eof)))))

(define-record-printer unzFile
  (lambda (x p) (display (conc  "#<unzipper>") p)))
