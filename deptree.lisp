(defpackage #:deptree
  (:use #:cl)
  (:export #:deptree #:systems-paths #:systems-archive))

(in-package #:deptree)

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
	 (dependants (asdf:system-depends-on system-instance)))
    (cond ((null dependants) nil)
	  (t (let ((deps (loop for d in dependants
			    when (atom d) nconc (dependencies-of d s-path)
			    when (and (consp d)
				      (eql (first d) :feature)
				      (stringp (second d))
				      (match-clause (second d)))
			    nconc (cons (third d) (dependencies-of (third d) s-path)))))
	       (if sub-p
		   deps
		   (cons system deps)))))))

(defun deptree (system)
  (remove-duplicates (dependencies-of system) :test #'string=))

(defun systems-paths (dependencies)
  (mapcar #'(lambda (name) 
	      (asdf:system-source-directory (asdf:find-system name)))
	  dependencies))

(defun systems-archive (dependencies tarball-name)
  )
