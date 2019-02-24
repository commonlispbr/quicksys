#|

Manoel Vilela & Lucas Vieira © 2019 MIT

|#

(in-package #:quicksys)


;; DIST: alist :: key -> plist
;; key: symbol
(defparameter *dists*
  '((bodge     (:url "http://bodge.borodust.org/dist/org.borodust.bodge.txt"
                :realname org.borodust.bodge))
    (cl21      (:url "http://dists.cl21.org/cl21.txt"))
    (ultralisp (:url "http://dist.ultralisp.org"))
    (shirakumo (:url "http://dist.tymoon.eu/shirakumo.txt")))
  "*DISTS* is an ALIST of PLISTS, each being one of the distributions available
in QL-META.")



(defun %dist-id (dist-name)
  "%DIST-ID converts DIST-NAME to an inner key representation."
  (if (typep dist-name 'string)
      (intern (string-upcase dist-name) :quicksys)
      (intern (symbol-name dist-name) :quicksys)))

(defun %dist-realname (dist)
  "%DIST-REALNAME generates the name of a DIST as a downcase string."
  (string-downcase
   (symbol-name (or (getf (cadr dist) :realname)
                    (car dist)))))

(defun dist-string (dist)
  "DIST-STRING produces a DIST representation in a human-readable format."
  (format nil "#<DIST ~A / ~A>"
          (car dist)
          (dist-url dist)))

(defun dist-properties (dist)
  "DIST-PROPERTIES yields the list of properties of a DIST."
  (cadr dist))


(defun dist-url (dist)
  "DIST-URL yields the url property of a DIST"
  (getf (dist-properties dist) :url))


(defun get-dist (dist-name)
  "GET-DIST retrieves a DIST based on DIST-NAME, if existing. Otherwise,
yields NIL."
  (assoc (%dist-id dist-name)
         *dists*))

(defun get-dists-urls (&optional (dists *dists*))
  "GET-DISTS-URLS yields a list of all dist urls defined in *DISTS*"
  (loop for dist in dists
        collect (dist-url dist)))


(defun get-dists-names (&optional (dists *dists*))
  "GET-DISTS-NAMES yields a list of all dist names defined in *DISTS*"
  (loop for (key plist) in dists
        collect key))

(defun installedp (dist)
  "INSTALLEDP checks whether a DIST was installed through QL-DIST."
  (let ((dist-obj (ql-dist:find-dist (%dist-realname dist))))
    (and dist-obj (ql-dist:installedp dist-obj))))

(defun install-dist (dist-name &key (force nil))
  "INSTALL-DIST a dist DIST-NAME using QL-DIST.

As default, use the parameters (:prompt nil :replace t) on
ql-dist:install-dist to avoid human interaction.

If DIST-NAME doesn't exist as a key in *DISTS*, this function
raises an error."
  (let ((dist (get-dist dist-name)))
    (cond ((null dist)
           (error (format nil "error: ~a not found" dist-name)))
          ((and (not force)
                (installedp dist)) t)
          (t (apply #'ql-dist:install-dist
                    (cons (dist-url dist)
                          '(:prompt nil :replace t)))))))

(defun uninstall-dist (dist-name)
  "UNINSTALL-DIST a dist DIST-NAME using QL-DIST.

Yields NIL on uninstallation error and when the dist DIST-NAME were not
installed in the first place. Otherwise, yields T."
  (let ((dist (get-dist dist-name)))
    (when (and dist (installedp dist))
      (let* ((dist-obj (ql-dist:find-dist (%dist-realname dist))))
        (ql-dist:uninstall dist-obj)))))

(defun quickload (system &key (dist nil) (silent nil))
  "QUICKLOAD wraps QL:QUICKLOAD.

If DIST is specified, QUICKLOAD will attempt to fetch the system from it. If the
specified DIST were not installed prior to system installation, it is removed
again.

Specifying SILENT suppresses output."
  (let* ((%dist (get-dist dist))
         (installed-before (and %dist (installedp %dist))))
    (when dist
      (install-dist dist))
    (ql:quickload system :silent silent)
    (unless installed-before
      (uninstall-dist dist))))


(defgeneric dist-apropos-list (term)
  (:documentation
   "DIST-APROPOS-LIST yields a list of DISTs based in a matching TERM.

This function considers %dist-realname and dist-url when searching.")
  (:method ((term symbol))
    (dist-apropos-list (symbol-name term)))
  (:method ((term string))
    (let ((result '())
          (nterm  (remove-if (lambda (c)
                               (eq c #\*))
                             (string-downcase term))))
      (dolist (dist *dists* (nreverse result))
        (when (or (search nterm (%dist-realname dist))
                  (search nterm (dist-url dist)))
          (push dist result))))))

(defgeneric dist-apropos (term)
  (:documentation
   "DIST-APROPOS searches for a dist containing TERM and prints it to
*STANDARD-OUTPUT*.

This function effectively wraps DIST-APROPOS-LIST so it is printed nicely on
console.")
  (:method (term)
    (mapcan (lambda (dist)
               (format t "~A~%" (dist-string dist)))
         (dist-apropos-list term))
    (values)))
