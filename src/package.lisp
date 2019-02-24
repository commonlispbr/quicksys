;;;; package.lisp

(defpackage #:ql-meta
  (:use #:cl)
  (:export #:*dists*
           #:install
           #:installedp
           #:uninstall
           #:dist-name
           #:dist-url
           #:dist-properties
           #:get-dist
           #:get-dists-urls
           #:get-dists-names
           #:apropos-dist
           #:apropos-dist-list
           #:quickload)
  (:documentation
   "QL-META provides a collection of tools to handle multiple quicklisp dists.

EXAMPLES

;; search for a dist
* (ql-meta:apropos-dist '*)
#<DIST BODGE / http://bodge.borodust.org/dist/org.borodust.bodge.txt>
#<DIST CL21 / http://dists.cl21.org/cl21.txt>
#<DIST ULTRALISP / http://dist.ultralisp.org>
#<DIST SHIRAKUMO / http://dist.tymoon.eu/shirakumo.txt>

;; install a dist
* (ql-meta:install :ultralisp)


;; install a dist temporary just to load a system
* (ql-meta:quickload 'trivial-gamekit :dist 'bodge )
"
   ))
