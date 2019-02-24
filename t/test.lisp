(defpackage #:ql-meta/test
  (:use #:cl
        #:prove)
  (:documentation "Collection of unit tests for QL-META"))


(in-package :ql-meta/test)


(setq ql-meta:*dists*
  '((test1 (:url "http://test1.com"))
    (test2 (:url "http://test2.com"))))

(plan nil)

(diag "== Testing: get-dists-names & get-dists-urls!")

(is (ql-meta:get-dists-names) '(test1 test2)
    "get-dists-names test 1")
(is (ql-meta:get-dists-urls) '("http://test1.com"
                               "http://test2.com")
    "get-dists-urls test 1")

(diag "== Testing: get-dist!")
(ok (typep (ql-meta:get-dist :test1) 'list)
    "get-dist keyword")
(ok (typep (ql-meta:get-dist 'test2) 'list)
    "get-dist symbol")
(is (ql-meta:get-dist 'test3) nil
    "get-dist symbol invalid")
(ok (typep (ql-meta:get-dist "test2") 'list)
    "get-dist string")

(diag "== Testing: dist-apropos-list!")

(is ql-meta:*dists* (ql-meta:dist-apropos-list '*)
    "dist-apropos-list wildcard *")
(is ql-meta:*dists* (ql-meta:dist-apropos-list "")
    "dist-apropos-list empty string")
(is 'TEST1 (caar (ql-meta:dist-apropos-list :test1))
    "dist-apropos-list name search")
(is '(TEST1 TEST2) (mapcar #'car (ql-meta:dist-apropos-list ".com"))
    "dist-apropos-list url search")

(finalize)
