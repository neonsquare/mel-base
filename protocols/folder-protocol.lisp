;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2004, Jochen Schmidt <js@codeartist.org>.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :mel.internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mail Folder Protocol ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass folder () ()
  (:documentation
   "Protocol class for the Mail folder protocol"))

(defclass basic-folder (folder)
  ((name :accessor name :initarg :name))
  (:documentation
   "Superclass of all folders supporting the folder protocol"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Operators ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric REGISTER-FOLDER (folder name))
(defgeneric SERIALIZE-FOLDER (folder stream))

(defvar *registered-folders* (make-hash-table :test 'equal))

(defun coerce-name (name)
  (typecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun folder-registry-truename ()
  (ensure-directories-exist
   (make-pathname :name nil 
		  :type nil
		  :directory (append (pathname-directory (user-homedir-pathname))
				     (list ".mel" "registry")))))

(defmethod register-folder ((folder folder) name)
  (let ((name (coerce-name name)))
    (with-open-file (stream (make-pathname
			     :defaults (folder-registry-truename)
			     :name name :type "mel" :case :local)
			    :if-exists :supersede 
			    :if-does-not-exist :create
			    :direction :output)
      (serialize-folder folder stream))))

(define-condition mel-error () ())

(define-condition folder-not-found 
    (mel-error)
  ((name :reader folder-not-found-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Folder ~A not found."
                     (folder-not-found-name condition)))))

(defun find-folder (name &optional (error-p t))
  (let* ((name (coerce-name name))
	 (in-memory (gethash name *registered-folders*)))
    (handler-case
	(or in-memory
	    (setf (gethash name *registered-folders*)
		  (with-open-file (stream (make-pathname 
					   :name name
					   :type "mel"
					   :defaults (folder-registry-truename)
					   :case :local))
		    (eval (read stream)))))
      (file-error ()
	(when error-p
	  (error 'folder-not-found :name name))))))

(defgeneric CLOSE-FOLDER (folder)
  (:documentation
"Close the given folder"))