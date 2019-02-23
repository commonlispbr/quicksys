(defpackage #:ql-meta/test
  (:use #:cl
        #:ql-meta
        #:prove))


(in-package :ql-meta/test)


(defparameter *dists-mock*
  '((test1 (:url "http://test1.com"))
    (test2 (:url "http://test2.com"))))

(plan nil)

(diag "== Sanity checks!")

(is (get-dists-names *dists-mock*) '(test1 test2))
(is (get-dists-urls *dists-mock*) '("http://test1.com"
                                    "http://test2.com"))

(finalize)
