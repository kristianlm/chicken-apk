(import (only (chicken pretty-print) pp)
        (only (chicken process-context) command-line-arguments)
        (android minizip)
        (android bxml)
        (only chicken.io read-string)
        matchable)

(match (command-line-arguments)

  (("manifest" file.apk)
   (define path (car (command-line-arguments)))
   (unzip-for-each file.apk
                   (lambda (uz path port)
                     (if (equal? path "AndroidManifest.xml")
                         (pp (bxml->sxml (read-string #f port)))))))

  (else
   (print "no match for command-line-arguments " (command-line-arguments))
   (print "usage:
  manifest <file.apk>
     prints app.apk's AndroidManifest.xml as sxml")
   (exit 1)))
