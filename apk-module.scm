(module (android apk)
(parse-xml unparse-xml
           apk-repackage
           apk-sign
           apk-align
           rezip string-pool-replace)
(import scheme (chicken base))
(include "string-pool.scm")

(include "reapk.scm")

)
