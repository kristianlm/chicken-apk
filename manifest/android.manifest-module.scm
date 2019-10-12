(module (android manifest) (sxml->ibax
                            ibax->sxml
                            bxml->ibax
                            ibax->bxml
                            sxml->bxml
                            bxml->sxml)
(import scheme (chicken base))
(include "manifest/prn.scm")
(include "manifest/buffer.scm")
(include "manifest/string-pool.scm")
(include "manifest/binary-xml.scm"))
