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

(in-package :mel.network)

;; Comment by <js@crispylogics.com> This function doesn't return the usocket
;; itself and therefore demands that a connection can be closed using CL:CLOSE
(defun make-connection (&key (remote-host "localhost") (remote-port 0)
			(element-type '(unsigned-byte 8)) (ssl nil ssl-p))
  (let ((fd (usocket:socket-stream
	     (usocket:socket-connect remote-host remote-port
				     :element-type element-type))))
    (maybe-ssl-connection ssl-p ssl remote-port fd)))

(defun maybe-ssl-connection (ssl-p ssl port fd)
  (if (or (and ssl-p ssl)
	  (and (not ssl-p) (ssl-default port)))
      (make-ssl-connection fd)
      fd))

(defun ssl-default (port)
  (member port '(993 995 465 585)))

#-lispworks
(defun make-ssl-connection (fd)
  (cl+ssl:make-ssl-client-stream fd :external-format :iso-8859-1))

#+lispworks
(defun make-ssl-connection (sock-stream)
  (comm:attach-ssl sock-stream :ssl-side :client)
  sock-stream)