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

(defpackage mel.system
  (:use #:cl #:asdf))

(in-package :mel.system)

(defsystem mel-base
    :version "0.9.0"
    :components ((:module "folders"
			  :depends-on 
			  ("protocols"
			   "base64-stream"
			   "line-terminator-filter" 
			   "encapsulating-stream"
			   "lisp-dep"
			   "hmac-md5")
			  :components ((:module "maildir"
						:components ((:file "folder")))
				       (:module "imap"
						:components ((:file "folder")))
				       (:module "pop3"
						:components ((:file "folder")))
				       (:module "smtp"
						:components ((:file "folder")))))
		 (:module "lisp-dep"
			  :components
			  ((:file "packages")
			   #+openmcl (:file "openmcl-fixes" :depends-on ("packages"))
			   (:file "files" :depends-on ("packages"))
			   (:file "filesystem" :depends-on ("files" "packages"))
			   (:file "unix" :depends-on ("packages" "utils"))
			   (:file "environment" :depends-on ("packages" #+openmcl "openmcl-fixes"))
			   (:file "network" :depends-on ("packages"))
			   (:file "utils" :depends-on ("packages"))))

		 (:module "protocols"
			  :depends-on ("packages" "mime")
			  :components
			  ((:file "folder-protocol")
			   (:file "folder-metainfo")
			   (:file "message-metainfo")
			   (:file "message-transport")
			   (:file "message-cache" :depends-on ("folder-protocol"))
			   (:file "receiver-protocol" :depends-on ("message-cache" "folder-protocol"))
			   (:file "sender-protocol")))

		 (:file "package-tools")
		 (:file "smeta")
		 (:file "packages" :depends-on ("package-tools" "lisp-dep" #-sbcl "md5"))
		 #-sbcl (:file "md5")
		 (:file "hmac-md5" :depends-on ("packages" #-sbcl "md5"))
		 (:file "rfc2822" :depends-on ("packages" "smeta"))
		 (:file "mime" :depends-on ("rfc2822"))
		 (:file "multiparts" :depends-on ("mime" "protocols"))
		 (:file "quoted-printable-stream" :depends-on ("encapsulating-stream"))
		 (:file "base64-stream" :depends-on ("encapsulating-stream"))
		 (:file "encapsulating-stream" :depends-on ("packages"))
		 (:file "line-terminator-filter" :depends-on ("encapsulating-stream"))
		 (:file "compose-message" :depends-on ("protocols" "mime")))
    :depends-on (#+sbcl sb-posix #+sbcl sb-md5 #+sbcl sb-bsd-sockets #+cmu uffi flexi-streams))
