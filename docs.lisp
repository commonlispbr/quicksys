(pushnew (uiop/os:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(ql:quickload :quicksys :silent t)
(ql:quickload :staple :silent t)

(staple:generate :quicksys
                 :packages '(#:quicksys))

(sb-ext:exit :code 0)
