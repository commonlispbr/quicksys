(ql:quickload :staple :silent t)
(staple:generate :ql-meta
                 :if-exists :overwrite)
(sb-ext:exit :code 0)
