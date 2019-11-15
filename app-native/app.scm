(foreign-declare "
// #include <jni.h>
#include <string.h>
#include <stdlib.h>
#include <android/log.h>

char my_chicken_inited = 0;
char _libs[2048];

// called by template app
int app(const char* libs) {
  strncpy(_libs, libs, 2048);
  if(my_chicken_inited == 0) {
    CHICKEN_run(C_toplevel);
    my_chicken_inited = 1;
  }
  return 0;
}

// maybe gc_hook is useful here.
// see https://github.com/chicken-mobile/android-ndk/blob/master/android-log/android-log.scm
")

;; ==================== critical setup ====================
(import (only chicken.platform repository-path)
        (only chicken.string conc))
(repository-path (list (foreign-value "_libs" c-string)))

;; fix loading dynamic extensions android only allows .so files that
;; begin with lib. so we store everything with that prefix and prefix
;; everything we look for.
(set! chicken.load#find-dynamic-extension
      (let ((fde chicken.load#find-dynamic-extension))
        (lambda (id inc?)
          (or (fde id inc?)
              (fde (conc "lib" id) inc?)))))

;; ==================== main ====================
(import (only chicken.platform return-to-host)
        (only chicken.port make-input-port set-port-name! set-buffering-mode!)
        (only chicken.file.posix file-read)
        (only srfi-18 thread-wait-for-i/o!)
        (only chicken.repl repl))

;; just in case
(define (android/log tag msg)
  ((foreign-lambda*
    void ((c-string tag) (c-string msg))
    "__android_log_write(ANDROID_LOG_DEBUG, tag, msg);")
   tag msg))

(current-input-port
 (make-input-port
  (lambda () ;; read
    (thread-wait-for-i/o! 0 #:input)
    (string-ref (car (file-read 0 1)) 0))
  (lambda () (char-ready?)) ;; ready?
  (lambda () #f))) ;; close
(set-port-name! (current-input-port) "stdin/nonblocking")

(define android/libs (foreign-value "_libs" c-string))

(import (only chicken.pretty-print pp))

(define (code str #!optional code)
  (if code
      (conc "\x1b{{" str "\x1b{#" code "\x1b}}")
      (conc "\x1b{{" str "\x1b}}")))

(import (only (chicken platform) chicken-version))
(print "

# Welcome

This is a CHICKEN " (chicken-version) " REPL. This REPL is running on a separate Java thread. "
(code "(current-input-port)") " reads from stdin/app and does not block other srfi-18 threads. "
(code "(current-output-port)") " is picked up by the app UI.

Many eggs are already available.

## nrepl

" (code "(import srfi-18 nrepl)
(thread-start! (lambda () (nrepl 9999)))") "

$ adb shell localhost 9999 # maybe that will work

## Stopwatch background thread

" (code "(import srfi-18 chicken.time)
(define start (current-milliseconds))
(define dummy
 (thread-start!
  (lambda ()
   (let loop ()
    (print* \"\\rstopwatch \" (- (current-milliseconds) start) \"ms\")
    (thread-sleep! 0.1)
    (loop)))))") "

Which you can kill like " (code "this" "(thread-terminate! dummy)") ".

## Silly print flood testing

\x1b{{
(import srfi-18)
(define dummy
 (thread-start!
  (lambda ()
   (let loop ((n 0))
     (print (make-string (inexact->exact (floor (* 20 (+ 1 (sin (* 0.1 n)))))) #\\#))
     (thread-sleep! 0.05)
     (loop (+ n 1))))))
\x1b}}

## scrollview speed tester

\x1b{{
(import chicken.time chicken.string srfi-18)
(let loop ((n 0) (last (current-milliseconds)))
  (let ((now (current-milliseconds)))
    (let ((elapsed (- now last)))
      (print \"elapsed \" elapsed \" #\" n)
      (android/log \"CHICKEN\" (conc \"elapsed \" elapsed \" #\" n)))
    (thread-sleep! 0.01)
    (loop (+ n 1) now)))
\x1b}}

## A UTF8 egg test

" (code "(import (prefix utf8 utf8:))
(print \"string-length says \" (string-length \"µ°π\"))
(print \"utf8:string-length says \" (utf8:string-length \"µ°π\"))
") "

## You can query for \x1b{{various features\x1b{#(import chicken.platform)
(features)
(system-cache-directory)
(system-config-directory)
(software-version)
(software-type)
(machine-byte-order)\x1b}}

## You can also exit the app using \x1b{{(exit)\x1b}}

")

(repl)
