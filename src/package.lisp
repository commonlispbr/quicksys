;;;; package.lisp

(defpackage #:quicksys
  (:use #:cl)
  (:nicknames :qs)
  (:export #:*dists*
           #:install-dist
           #:installedp
           #:uninstall-dist
           #:dist-name
           #:dist-url
           #:dist-properties
           #:get-dist
           #:get-dists-urls
           #:get-dists-names
           #:dist-apropos
           #:dist-apropos-list
           #:quickload)
  (:documentation
   "QUICKSYS provides a collection of tools to load systems from
   multiple quicklisp dists.

EXAMPLES

;; search for a dist
* (quicksys:dist-apropos '*)
#<DIST BODGE / http://bodge.borodust.org/dist/org.borodust.bodge.txt>
#<DIST CL21 / http://dists.cl21.org/cl21.txt>
#<DIST ULTRALISP / http://dist.ultralisp.org>
#<DIST SHIRAKUMO / http://dist.tymoon.eu/shirakumo.txt>

;; install a dist
* (quicksys:install :ultralisp)


;; install a dist temporary just to load a system
* (quicksys:quickload 'trivial-gamekit :dist 'bodge )
"
   ))
