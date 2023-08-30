(defpackage #:deptree
  (:use #:cl)
  (:export #:deptree #:systems-paths #:systems-archive))

(in-package #:deptree)

(defparameter *excluded-systems* '("uiop" "asdf" "sb-cltl2" "sb-bsd-sockets" "sb-posix" "sb-rotate-byte"))

(defun match-clause (exp)
  (cond ((atom exp) (member exp *features*))
	((eql :or (car exp)) (some 'match-clause (cdr exp)))
	((eql :and (car exp)) (every 'match-clause (cdr exp)))
	((eql :not (car exp)) (notany 'match-clause (cdr exp)))
	(t nil)))

(defun dependencies-of (system &optional (p-path "?!?!?"))
  (let* ((system-instance (asdf:find-system system))
	 (s-pname (asdf:system-source-directory system-instance))
	 (s-path (and s-pname (namestring s-pname)))
	 (sub-p (search p-path s-path))
	 (dependants (asdf:system-depends-on system-instance))
	 (deps (loop for d in dependants
		  when (atom d) nconc (dependencies-of d s-path)
		  when (and (consp d)
			    (eql (first d) :feature)
			    (stringp (third d))
			    (match-clause (second d)))
		  nconc (cons (third d) (dependencies-of (third d) s-path)))))
    (if sub-p
	deps
	(cons system deps))))

(defun deptree (system)
  (set-difference
   (remove-duplicates (cdr (dependencies-of system)) :test #'string=)
   *excluded-systems*
   :test #'string=))

(defun systems-paths (dependencies)
  (mapcar #'(lambda (name)
	      (asdf:system-source-directory (asdf:find-system name)))
	  dependencies))

(defun systems-archive (dependencies tarball-pathname &key (sanitize-p t) (path-prefix ""))
  (let* ((paths (systems-paths dependencies)))
    (tar:with-open-archive (a tarball-pathname :direction :output)
      (loop for p in paths
	 for dir = (pathname-directory p)
	 do
	   (let ((*default-pathname-defaults* (make-pathname :directory (butlast dir))))
	     (tar-create:create-archive a (last dir) :recursep t
					:prefix path-prefix
					:filter (if sanitize-p
						    #'(lambda (pathname)
							(let* ((name (pathname-name pathname))
							       (marker (if name
									   (if (pathname-type pathname)
									       (concatenate 'string (namestring name) "."
											    (namestring (pathname-type pathname)))
									       (namestring name))
									   (namestring (car (last (pathname-directory pathname)))))))
							  (not (find marker '(".git" ".gitignore" ".hg" ".hgignore" ".github") :test #'string=))))
						    (constantly t))))))))
