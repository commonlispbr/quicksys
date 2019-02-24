(defpackage #:quicksys/test
  (:use #:cl
        #:prove)
  (:documentation "Collection of unit tests for QUICKSYS"))


(in-package :quicksys/test)


(setq quicksys:*dists*
  '((test1 (:url "http://test1.com"))
    (test2 (:url "http://test2.com"))))

(plan nil)

(diag "== Testing: get-dists-names & get-dists-urls!")

(is (quicksys:get-dists-names) '(test1 test2)
    "get-dists-names test 1")
(is (quicksys:get-dists-urls) '("http://test1.com"
                               "http://test2.com")
    "get-dists-urls test 1")

(diag "== Testing: get-dist!")
(ok (typep (quicksys:get-dist :test1) 'list)
    "get-dist keyword")
(ok (typep (quicksys:get-dist 'test2) 'list)
    "get-dist symbol")
(is (quicksys:get-dist 'test3) nil
    "get-dist symbol invalid")
(ok (typep (quicksys:get-dist "test2") 'list)
    "get-dist string")

(diag "== Testing: dist-apropos-list!")

(is quicksys:*dists* (quicksys:dist-apropos-list '*)
    "dist-apropos-list wildcard *")
(is quicksys:*dists* (quicksys:dist-apropos-list "")
    "dist-apropos-list empty string")
(is 'TEST1 (caar (quicksys:dist-apropos-list :test1))
    "dist-apropos-list name search")
(is '(TEST1 TEST2) (mapcar #'car (quicksys:dist-apropos-list ".com"))
    "dist-apropos-list url search")

(finalize)
