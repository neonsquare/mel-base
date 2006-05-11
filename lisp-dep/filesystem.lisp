(in-package :mel.filesystem)

#+(and cmu unix)
(defun map-directory (fn directory)
  (let ((dir (unix:open-dir directory)))
    (when dir
      (unwind-protect
	  (loop
	   (let ((file (unix:read-dir dir)))
	     (if file
		 (unless (char= #\. (char file 0))
		   (funcall fn file))
	       (return))))
	(unix:close-dir dir)))))

#+(and sbcl unix)
(defun map-directory (fn directory)
  (let ((dir (sb-posix:opendir directory)))
    (when dir
      (unwind-protect
	   (loop
	    (let ((dirent (sb-posix:readdir dir)))
	      (if (not (sb-alien:null-alien dirent))
		  (let ((file (sb-posix:dirent-name dirent)))
		    (unless (char= #\. (char file 0))
		      (funcall fn file)))
		  (return))))
	(sb-posix:closedir dir)))))

#+clisp
(defun map-directory (fn directory)
  (let ((files (make-pathname :name :wild
			       :defaults directory))
	(CUSTOM:*PARSE-NAMESTRING-DOT-FILE* :name))
    (map nil (lambda (file)
	       (let ((name (pathname-name file))
		     (type (pathname-type file)))
		 (unless (or (char= #\. (char name 0))
			     (null name))
		   (if (or (eq type :wild)
			   (null type))
		       (funcall fn name)
		       (funcall fn (concatenate 
				    'string
				    (pathname-name file)
				    "."
				    (pathname-type file)))))))
	 (directory files))))

#+(and lispworks unix)
(defun map-directory (fn directory)
  (files::fast-map-directory directory 
			     (lambda (file)
			       (unless (char= #\. (char file 0))
				 (funcall fn (copy-seq file))))))

#-(or cmu sbcl clisp (and :lispworks (or :unix :macosx)))
(defun map-directory (fn directory)
  (map nil (lambda (file)
	     (let ((name (pathname-name file))
		   (type (pathname-type file)))
	       (unless (or (char= #\. (char name 0))
			   (null name))
		 (if (or (eq type :wild)
			 (null type))
		     (funcall fn name)
		     (funcall fn (concatenate 
				  'string
				  (pathname-name file)
				  "."
				  (pathname-type file)))))))
	   (files::list-files directory)))

(defun directory-pathname (pathname)
  (make-pathname :defaults pathname :name nil :type nil))

;; (make-pathname :name name :defaults directory)
;; SBCL (make-pathname :defaults name :directory (pathname-directory directory))
(defun append-name (directory name)
  (concatenate 'string (namestring directory)
	       name))

(defun only-name-and-type (pathname)
  (let ((name (pathname-name pathname))
	(type (pathname-type pathname)))
    (unless name
      (setf name ""))
    (if (or (eq type :wild) (null type))
	name
	(concatenate 'string name "." type))))