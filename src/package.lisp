;;;; package.lisp

(defpackage #:ql-meta
  (:use #:cl #:quicklisp-client)
  (:export :*dists*
           :get-dists-names
           :get-dists-urls))
