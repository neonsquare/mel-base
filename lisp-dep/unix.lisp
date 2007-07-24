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

(in-package :mel.unix)

;; Unix stat() interface

#+(or (and lispworks unix) sbcl clisp)
(defun %stat (filename)
  #+(and lispworks unix) (system:get-file-stat filename)
  #+sbcl (sb-posix:stat filename)
  #+clisp (posix:file-stat filename))

; unix:unix-stat result in CMUCL:
; (values  success    0
;          st-dev     1
;          st-ino     2
;          st-mode    3
;          st-nlink   4
;     	   st-uid     5
;	   st-gid     6
;	   st-rdev    7
;	   st-size    8
;	   st-atime   9
;	   st-mtime   10
;	   st-ctime   11
;	   st-blksize 12
;	   st-blocks) 13

(defun stat-last-access (stat)
  #+(and lispworks unix) (system:file-stat-last-access (%stat stat))
  #+sbcl (sb-posix:stat-atime (%stat stat))
  #+cmu (nth-value 9 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-atime (%stat stat))
  #+openmcl (declare (ignore stat))
  #+openmcl nil)

(defun stat-last-change (stat)
  #+(and lispworks unix) (system:file-stat-last-change (%stat stat))
  #+sbcl (sb-posix:stat-ctime (%stat stat))
  #+cmu (nth-value 11 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-ctime (%stat stat))
  #+openmcl (declare (ignore stat))
  #+openmcl nil)

(defun stat-last-modify (stat)
  #+(and lispworks unix) (system:file-stat-last-modify (%stat stat))
  #+sbcl (sb-posix:stat-mtime (%stat stat))
  #+cmu (nth-value 10 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-mtime (%stat stat))
  #+openmcl (nth-value 3 (ccl::%stat stat)))

(defun stat (filename)
  (values t
	  (stat-last-access filename)
	  (stat-last-change filename)
	  (stat-last-modify filename)))

;;; Directory Changes

#+(or sbcl (and lispworks unix) cmu clisp openmcl)
(defun directory-contents-changed-p (directory tag)
  (assert (file-directory-p directory) (directory) "File ~S is no directory" directory)
  (let ((mtime (stat-last-modify directory)))
    (if (eql mtime tag)
	nil
      mtime)))

#-(or sbcl (and lispworks unix) cmu clisp)
(defun directory-contents-changed-p (directory tag)
  (declare (ignore directory))
  tag)


;;; Lisp<->Unix time conversion
(defun universal-to-unix-time (universal-time)
  (- universal-time 2208988800))

(defun unix-to-universal-time (unix-time)
  (+ unix-time 2208988800))

;;; Setting access and modification times
#+(and :lispworks :unix)
(fli:define-c-typedef (--time-t (:foreign-name "__time_t")) :long)

#+(and :lispworks :unix)
(fli:define-c-struct (utimbuf (:foreign-name "utimbuf"))
                     (actime --time-t)
                     (modtime --time-t))

#+(and :lispworks :unix)
(fli:define-foreign-function (c-utime "utime" :source)
                             ((--file (:reference :ef-mb-string))
                              (--file-times
                               (:pointer (:const (:struct utimbuf)))))
                             :result-type
                             :int
                             :language
                             :c)

#+(and :lispworks :unix)
(defun utime (file access-time modification-time)
  (let* ((stat (system:get-file-stat file))
         (atime (if access-time 
                  (universal-to-unix-time access-time)
                  (system:file-stat-last-access stat)))
         (mtime (if modification-time
                  (universal-to-unix-time modification-time)
                  (system:file-stat-last-modify stat))))
    (fli:with-dynamic-foreign-objects ()
      (let ((utimbuf (fli:allocate-dynamic-foreign-object :type 'utimbuf)))
        (setf (fli:foreign-slot-value utimbuf 'actime) atime
              (fli:foreign-slot-value utimbuf 'modtime) mtime)
        (c-utime (princ-to-string (truename file)) utimbuf)))))

