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

(defun flagp (message flag)
  "Test for flag"
  (flagp-using-folder (folder message) message flag))

(defmethod flagp-using-folder ((folder basic-folder) (message simple-flag-mixin) flag)
  (find flag (flags message) :test #'eq))

(defun mark-message (message flag)
  "Add flag to message"
  (mark-message-using-folder (folder message) message flag))

(defmethod mark-message-using-folder :after
    ((folder basic-folder) (message simple-flag-mixin) flag)
  (pushnew flag (flags message)))

(defun unmark-message (message flag)
  "Remove flag from message"
  (unmark-message-using-folder (folder message) message flag))

(defmethod unmark-message-using-folder :after ((folder basic-folder) (message message) flag)
  (setf (flags message) (remove flag (flags message) :test #'eq)))

#+nil
(defmethod message-md5sum ((message mel.mime:mime-message))
  (or (mel.mime:md5-sum message)
      (setf (slot-value message 'mel.mime:md5-sum)
	    (with-open-stream (stream (open-message-input-stream message))
		  (md5:md5sum-stream stream)))))

#+nil
(defmethod content-md5sum ((message mel.mime:mime-message))
  (or (mel.mime:content-md5-sum message)
      (setf (slot-value message 'mel.mime:content-md5-sum)
	    (with-open-stream (stream (message-body-stream message))
		  (md5:md5sum-stream stream)))))

#+nil
(defmethod unique-message-digest ((message mel.mime:mime-message))
  (let ((universal (mel.mime:date message)))
    (format nil "~A-~D" 
	    (mel.mime::url-encode-base64 (content-md5sum message))
	    (if universal 
		(mel.unix:unix-to-universal-time universal)
	      0))))
