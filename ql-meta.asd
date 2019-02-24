;;;; ql-meta.asd

(asdf:defsystem #:ql-meta
    :description "QL-META tracks multiple Quicklisp distributions"
    :author "Manoel Vilela & Lucas Vieira"
    :license  "MIT"
    :version "0.1.0"
    :homepage "https://lisp.com.br/quicklisp-meta/"
    :bug-tracker "https://github.com/commonlispbr/quicklisp-meta/issues"
    :source-control (:git "https://github.com/commonlispbr/quicklisp-meta.git")
    :serial t
    :pathname "src"
    :depends-on (:quicklisp)
    :components ((:file "package")
                 (:file "ql-meta")))

(asdf:defsystem #:ql-meta/test
  :description "QL-META test suit"
  :author "Manoel Vilela & Lucas Vieira"
  :license  "MIT"
  :version "0.1.0"
  :serial t
  :pathname "t"
  :depends-on (:ql-meta :prove)
  :components ((:file "test"))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) c)))
