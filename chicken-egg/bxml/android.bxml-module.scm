(module (android bxml) (sxml->ibax
                        ibax->sxml
                        bxml->ibax
                        ibax->bxml
                        sxml->bxml
                        bxml->sxml)
(import scheme (chicken base))
(include "bxml/prn.scm")
(include "bxml/buffer.scm")
(include "bxml/string-pool.scm")
(include "bxml/bxml.scm"))
