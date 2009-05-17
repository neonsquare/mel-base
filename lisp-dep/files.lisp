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

(defpackage :org.codeartist.files
  (:use :common-lisp)
  (:nicknames :files))

(in-package :org.codeartist.files)

#+(and :lispworks :unix)
(fli:define-c-typedef (--u-long (:foreign-name "__u_long"))
                      (:unsigned :long))

#+(and :lispworks :unix)
(fli:define-c-typedef (--quad-t (:foreign-name "__quad_t")) :long-long)
#+(and :lispworks :unix)
(fli:define-c-typedef (--u-quad-t (:foreign-name "__u_quad_t"))
                      (:struct (--val (:c-array --u-long 2))))

#+(and :lispworks :unix)
(fli:define-c-typedef (--ino-t (:foreign-name "__ino_t")) (:unsigned :long))

#+(and :lispworks :unix)
(fli:define-c-typedef (--off-t (:foreign-name "__off_t")) :long)

#+(and :lispworks :unix (not :macosx))
(fli:define-c-struct (dirent (:foreign-name "dirent"))
                     (d_ino --ino-t)
                     (d_off --off-t)
                     (d_reclen (:unsigned :short))
                     (d_type (:unsigned :char))
                     (d_name (:c-array :char 256)))


#+(and :lispworks :macosx)
(fli:define-c-struct (dirent (:foreign-name "dirent"))
                     (d_fileno --ino-t)
                     (d_reclen (:unsigned :short))
                     (d_type (:unsigned :char))
                     (d_namlen (:unsigned :char))
                     (d_name (:c-array :char 256)))

(defconstant +EBADF+ 9)
(defconstant +EACCES+ 13)  ; Permission denied.
(defconstant +EMFILE+ 24)  ; Too many file descriptors in use by process.
(defconstant +ENFILE+ 23)  ; Too many files are currently open in the system.
(defconstant +ENOENT+ 2)   ; Directory  does  not  exist,  or  name  is an empty string.
(defconstant +ENOMEM+ 12)  ; Insufficient memory to complete the operation.
(defconstant +ENOTDIR+ 20) ; name is not a directory.

#+(and :lispworks (or :unix :macosx))
(fli:define-foreign-function (c-opendir "opendir" :source)
    ((--name (:reference :ef-mb-string)))
  :result-type :pointer :language :c)

#+(and :lispworks (or :unix :macosx))
(fli:define-foreign-function (c-closedir "closedir" :source)
    ((--dirfd :pointer))
  :result-type :int :language :c)

#+(and :lispworks (or :unix :macosx))
(fli:define-foreign-function (c-readdir "readdir" :source)
    ((--dirfd :pointer))
  :result-type (:pointer (:struct dirent)) :language :c)

#+(and :lispworks (or :unix :macosx))
(defclass directory-iterator ()
  ((directory-pathname :accessor directory-pathname :initarg :pathname)
   dirfd
   (current-entry :accessor current-entry :initform (make-array 256 :element-type lw:*default-character-element-type* :fill-pointer 0))))

#+(and :lispworks (or :unix :macosx))
(defmethod open-directory ((dir directory-iterator))
  (setf (slot-value dir 'dirfd) (c-opendir (directory-pathname dir)))
  (read-next-dirent dir) ; "."
  (read-next-dirent dir) ; ".."
  dir)

#+(and :lispworks (or :unix :macosx))
(defun read-next-dirent (dir)
  (let ((dirent (c-readdir (slot-value dir 'dirfd))))
    (unless (fli:null-pointer-p dirent)
      (let ((d_name (fli:foreign-slot-pointer dirent 'd_name)))
        (setf (fill-pointer (current-entry dir)) 0)
        (loop for i from 0 below 256
              for c = (fli:foreign-aref d_name i)
              until (eql #\Null c)
              do (vector-push c (current-entry dir))))
      t)))

#+(and :lispworks (or :unix :macosx))
(defun fast-map-directory (dir fn)
  (unless (stringp dir) (setf dir (namestring dir)))
  (let ((cursor (open-directory (make-instance 'directory-iterator :pathname (string dir)))))
    (unwind-protect
        (loop while (read-next-dirent cursor)
              do (funcall fn (current-entry cursor)))
      (c-closedir (slot-value cursor 'dirfd)))))

(defun list-files (pathspec)
  (let ((wildspec (make-pathname :name :wild :type :wild :defaults pathspec)))
    #+lispworks (directory wildspec :test (complement #'lw:file-directory-p))
    #+allegro (remove-if #'excl:file-directory-p (directory wildspec))
    #+openmcl (directory wildspec :directories nil :files t)
    #-(or lispworks allegro openmcl) (remove-if #'null (directory wildspec) :key #'pathname-name)))

(defun list-directories (pathspec)
  (let ((wildspec (make-pathname :name :wild :type :wild :defaults pathspec)))
    #+lispworks (directory wildspec :test #'lw:file-directory-p)
    #+allegro (remove-if-not #'excl:file-directory-p (directory wildspec))
    #+openmcl (directory wildspec :directories t :files nil)
    #-(or lispworks allegro openmcl) (remove-if-not #'null (directory wildspec) :key #'pathname-name)))

(defun map-files (fn pathspec &key (test (constantly t))(recursivep nil))
  (dolist (f (list-files pathspec))
    (when (funcall test f)
      (funcall fn f)))
  (when recursivep
    (dolist (d (list-directories pathspec))
      (map-files fn d :test test :recursivep recursivep))))
