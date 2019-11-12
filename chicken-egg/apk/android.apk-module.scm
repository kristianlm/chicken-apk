(module (android apk) (apk-repackage
                       apk-sign
                       apk-align
                       rezip
                       discard-meta+lib)
(import scheme (chicken base))
;;(include "string-pool.scm")

(include "apk/reapk.scm")

)
