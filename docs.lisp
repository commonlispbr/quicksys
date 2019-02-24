(ql:quickload :ql-meta)
(ql:quickload :staple :silent t)

(staple:generate :ql-meta
                 :packages '(#:ql-meta))
(sb-ext:exit :code 0)
