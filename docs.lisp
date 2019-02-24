(ql:quickload :quicksys)
(ql:quickload :staple :silent t)

(staple:generate :quicksys
                 :packages '(#:quicksys))
(sb-ext:exit :code 0)
