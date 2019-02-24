;;;; package.lisp

(defpackage #:ql-meta
  (:use #:cl)
  (:export #:*dists*
           #:install
           #:installedp
           #:uninstall
           #:get-dist
           #:get-dist-url
           #:get-dist-properties
           #:get-dists-urls
           #:get-dists-names))
