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

(in-package :mel.environment)

;; FFI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GETPID: Get the pid of the daemon ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(and unix #.(cl:if (cl:find-package "UFFI") '(and) '(or)))
(uffi:def-function "getpid" ():returning :int)

#-#.(cl:if (cl:find-package "UFFI") '(and) '(or))
(defun getpid ()
#+clisp
(system:process-id)
#+(and lispworks unix)
(system::getpid)
#+(and sbcl unix)
(sb-unix:unix-getpid)
#+(and cmu unix)
(unix:unix-getpid)
#+openmcl
(ccl::getpid)
#+(or (and lispworks (not unix))
      (and sbcl (not unix))
      (not (or clisp openmcl)))
(random 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GETHOSTNAME Getting the name of the host ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Host CL implementations

#+(and lispworks unix)
(fli:define-foreign-function (gethostname "gethostname" :source)
    ((string (:reference-return (:ef-mb-string :limit 256)))
     (n :int))
  :lambda-list (&aux string (n 256))
  :calling-convention :cdecl)

#+openmcl
(defun gethostname ()
  "Returns the hostname"
  (ccl::%stack-block ((resultbuf 256))  
    (if (zerop (c-gethostname resultbuf 256))
	(ccl::%get-cstring resultbuf)
	(error "gethostname() failed."))))

#+(And clisp unix)
(defun gethostname ()
  (posix:uname-nodename (posix:uname)))

#+(and :clisp :win32)
(ffi:def-call-out c-gethostname
    (:name \"gethostname\")
    (:arguments
        (name (ffi:c-ptr
            (ffi:c-array-max ffi:char 256)) :out :alloca)
        (len ffi:int))
    (:language :stdc)
    (:return-type ffi:int)
    (:library "wsock32.dll"))
	
#+(and :clisp :win32)
(defun gethostname ()
    "Returns the hostname"
    (multiple-value-bind (success name) (c-gethostname 256)
        (if (zerop success)
                 (ext:convert-string-from-bytes name
custom:*FOREIGN-ENCODING*)
            (error (strerr errno)))))

;; UFFI Implementation
#+(and (not (or clisp openmcl)) unix #.(cl:if (cl:find-package "UFFI") '(and) '(or)))
(uffi:def-function ("gethostname" c-gethostname)
    ((name (* :unsigned-char))
     (len :int))
  :returning :int)

#+(and (not (or clisp openmcl))
       (and unix #.(cl:if (cl:find-package "UFFI") '(and) '(or))))
(defun gethostname ()
  "Returns the hostname"
  (uffi:with-foreign-object (name '(:array :unsigned-char 256))
    (if (zerop (c-gethostname (uffi:char-array-to-pointer name) 256))
	(uffi:convert-from-foreign-string name)
	(error "gethostname() failed."))))

;; If no GETHOSTNAME is yet defined - generate a dummy stub
#-#. (cl:if (cl:fboundp 'gethostname) '(and) '(or))
(progn
  (warn "Uses dummy GETHOSTNAME function - try loading UFFI before compiling mel-base")
(defun gethostname ()
  "localhost"))
