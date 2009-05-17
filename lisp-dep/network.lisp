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

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+lispworks (require "comm"))


;; Socket Streams

#+cmu
(defun make-connection 
    (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (sys:make-fd-stream  
   (ext:connect-to-inet-socket remote-host remote-port)
   :input t :output t :element-type element-type))

#+sbcl
(defun make-connection 
  (&key (remote-host "localhost") (remote-port 0)(element-type '(unsigned-byte 8)))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket 
				   (sb-bsd-sockets:host-ent-address
				    (sb-bsd-sockets:get-host-by-name
				     remote-host))
				   remote-port)
    (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
						     :element-type :default
						     )))
      stream)))

#+acl
(defun make-connection 
  (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (socket:make-socket :element-type element-type :remote-host remote-host :remote-port remote-port))

#+lispworks
(defun make-connection 
    (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (comm:open-tcp-stream remote-host remote-port
			:direction :io
			:element-type (if (eq element-type 'character) lw:*default-character-element-type* element-type)
			:errorp t))

#+abcl
(defun make-connection 
    (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (let ((socket (ext:make-socket remote-host remote-port)))
    (ext:get-socket-stream socket :element-type element-type)))

#+clisp
(defun make-connection 
    (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (socket:socket-connect
   remote-port remote-host
   :element-type element-type))

#+openmcl
(defun make-connection
  (&key (remote-host "localhost") (remote-port 0) (element-type '(unsigned-byte 8)))
  (openmcl-socket:make-socket :format (if (subtypep element-type 'character)
					  :text
					:binary)
			      :remote-host remote-host :remote-port remote-port))