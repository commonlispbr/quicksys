#|

Manoel Vilela & Lucas Vieira © 2019 MIT

|#

(in-package #:ql-meta)


;; DIST: alist :: key -> plist
;; key: symbol
(defparameter *dists*
  '((bodge     (:url "http://bodge.borodust.org/dist/org.borodust.bodge.txt"
                :realname org.borodust.bodge))
    (cl21      (:url "http://dists.cl21.org/cl21.txt"))
    (ultralisp (:url "http://dist.ultralisp.org"))
    (shirakumo (:url "http://dist.tymoon.eu/shirakumo.txt")))
  "*DISTS* it's a ALIST with PLIST of distributions available in QL-META")



(defun %dist-id (dist-name)
  "%DIST-ID converts DIST-NAME to a inner key DIST representation."
  (if (typep dist-name 'string)
      (intern (string-upcase dist-name) :ql-meta)
      (intern (symbol-name dist-name) :ql-meta)))

(defun %dist-realname (dist)
  (string-downcase
   (symbol-name (or (getf (cadr dist) :realname)
                    (car dist)))))

(defun dist-string (dist)
  "DIST-STRING returns the DIST in a human readable format."
  (format nil "#<DIST ~A / ~A>"
          (car dist)
          (dist-url dist)))

(defun dist-properties (dist)
  "GET-LIST-PROPERTIES return a list of properties"
  (cadr dist))


(defun dist-url (dist)
  "DIST-URL return the :url of a DIST"
  (getf (dist-properties dist) :url))


(defun get-dist (dist-name)
  "GET-DIST retrieve a DIST based on DIST-NAME"
  (assoc (%dist-id dist-name)
         *dists*))

(defun get-dists-urls (&optional (dists *dists*))
  "GET-DISTS-URLS return the urls defined in *DISTS*"
  (loop for dist in dists
        collect (dist-url dist)))


(defun get-dists-names (&optional (dists *dists*))
  "GET-DISTS-NAMES return the names defined in *DISTS*"
  (loop for (key plist) in dists
        collect key))

(defun installedp (dist)
  "INSTALLEDP check if DIST is installed thourgh QL-DIST."
  (let ((dist-obj (ql-dist:find-dist (%dist-realname dist))))
    (and dist-obj (ql-dist:installedp dist-obj))))

(defun install (dist-name &key (force nil))
  "INSTALL a DIST-NAME using QL-DIST

As default use the parameters (:prompt nil :replace t) on
ql-dist:install-dist to avoid human interation.

If DIST-NAME didn't exists as key of *DISTS* this function
will raises a error.
"
  (let ((dist (get-dist dist-name)))
    (cond ((null dist)
           (error (format nil "error: ~a not found" dist-name)))
          ((and (not force)
                (installedp dist)) t)
          (t (apply #'ql-dist:install-dist
                    (cons (dist-url dist)
                          '(:prompt nil :replace t)))))))

(defun uninstall (dist-name)
  "UNINSTALL a DIST-NAME using QL-DIST

RETURN T when the unsinstalling it's sucessful.
Otherwise nil, like the dist-name it's not exists.
  "
  (let ((dist (get-dist dist-name)))
    (when (and dist (installedp dist))
      (let* ((dist-obj (ql-dist:find-dist (%dist-realname dist))))
        (ql-dist:uninstall dist-obj)))))

(defun quickload (system &key (dist nil) (silent nil))
  "QUICKLOAD wraps QL:QUICKLOAD installing DIST first"
  (let* ((%dist (get-dist dist))
         (installed-before (and %dist (installedp %dist))))
    (when dist
      (install dist))
    (ql:quickload system :silent silent)
    (unless installed-before
      (uninstall dist))))


(defgeneric apropos-dist-list (term)
  (:documentation
   "APROPOS-DIST-LIST return a list of DISTs based in a matching TERM.

This function consider the %dist-realname for search and the dist-url.
 ")
  (:method ((term symbol))
    (apropos-dist-list (symbol-name term)))
  (:method ((term string))
    (let ((result '())
          (nterm  (remove-if (lambda (c)
                               (eq c #\*))
                             (string-downcase term))))
      (dolist (dist *dists* (nreverse result))
        (when (or (search nterm (%dist-realname dist))
                  (search nterm (dist-url dist)))
          (push dist result))))))

(defgeneric apropos-dist (term)
  (:documentation
   "APROPOS-DIST search for DIST with TERM and print them to *STANDARD-OUTPUT*")
  (:method (term)
    (mapcan (lambda (dist)
               (format t "~A~%" (dist-string dist)))
         (apropos-dist-list term))
    (values)))
