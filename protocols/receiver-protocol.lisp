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

;; Conditions
(define-condition message-not-found 
    (mel-error)
  ((folder  :initarg :folder
            :reader message-folder)
   (uid :initarg :uid 
        :reader message-uid))
  (:report (lambda (condition stream)
             (format stream "Message with UID ~A not found in folder ~A."
                     (message-uid  condition)
                     (message-folder condition)))))

(defclass receiver () ()
  (:documentation
   "Procotol class for folders that support
    the message receiver protocol"))

#+nil(defclass basic-receiver (receiver basic-folder) ())

(defclass basic-receiver (equal-message-cache-mixin receiver basic-folder) ())

(defgeneric OPEN-MESSAGE-INPUT-STREAM-USING-FOLDER (folder message start))
(defgeneric SKIP-HEADERS-USING-FOLDER (folder message stream))
(defgeneric MESSAGE-BODY-STREAM-USING-FOLDER (folder message))
(defgeneric MESSAGE-HEADER-STREAM-USING-FOLDER (folder message))
(defgeneric MESSAGE-SIZE-USING-FOLDER (folder message))

(defmethod find-message ((folder receiver) uid &key (if-does-not-exist :error))
  "Find a message in a folder using its unique id (unique within this folder)"
  (ecase if-does-not-exist
    (:create (make-instance 'mime-message
                 :uid uid
                 :folder folder))
    (:error (error 'message-not-found :uid uid :folder folder))
    ((nil) nil)))

(defun open-message-input-stream (message &optional (start 0))
  "Given a message-object which is contained within an folder create a stream
to read the message in rfc2822 format"
  (open-message-input-stream-using-folder (folder message) message start))

(defun message-header-stream (message)
  (message-header-stream-using-folder (folder message) message))

(defun message-body-stream (message)
  (message-body-stream-using-folder (folder message) message))

(defun skip-headers (stream message)
  (skip-headers-using-folder (folder message) message stream))

(defun message-size (message)
  (message-size-using-folder (folder message) message))

;; Multimethods

(defmethod skip-headers-using-folder ((folder basic-receiver) message stream)
  (setf (mel.mime:body-start message) (mel.mime:skip-rfc2822-header stream)))

(defmethod message-header-stream-using-folder ((folder basic-receiver) (message message))
  (open-message-input-stream-using-folder folder message 0))

#+nil
(defmethod message-body-stream-using-folder ((folder basic-receiver) (message message))
  (let ((stream (open-message-input-stream message)))
    (setf (mel.mime:header-fields message) (mel.mime:read-rfc2822-header stream))
    stream))

(defmethod message-body-stream-using-folder ((folder basic-receiver) (message message))
  (let ((body-start (mel.mime:body-start message)))
    (if body-start
	(open-message-input-stream message body-start)
      (let ((stream (open-message-input-stream message)))
	(skip-headers stream message)
	stream))))

(defgeneric ENSURE-HEADERS-READ (parent message))

(defmethod ensure-headers-read ((folder basic-receiver) (message message))
  (unless (mel.mime:header-fields message)
    (with-open-stream (stream (message-header-stream-using-folder folder message))
      (setf (mel.mime:header-fields message) (mel.mime:read-rfc2822-header stream)))))

(defmethod mel.mime::field :before ((name symbol) (message mel.mime::rfc2822-header-mixin))
  (when (folder message)
    (ensure-headers-read (folder message) message)))

(defmethod message-size-using-folder ((folder basic-receiver) (message message))
  (with-open-stream (stream (open-message-input-stream message))
    (loop for count upfrom 0
          for c = (read-char stream nil nil)
          while c
          finally (return count))))

(defmethod ensure-all-headers ((folder basic-receiver) &key hook)
  (let* ((messages (remove-if-not #'null (messages folder) :key #'mel.mime:header-fields))
	 (count (length messages)))
    (loop for i downfrom count
	  for message in messages
	  do (funcall hook message)
	  (ensure-headers-read (folder message) message))))

(defun message-string (message)
  (message-string-using-folder (folder message) message))

(defmethod message-string-using-folder ((folder basic-receiver) (message message))
  (with-output-to-string (out)
    (with-open-stream (stream (open-message-input-stream message))
      (loop for c = (read-char stream nil nil)
	    while c do (write-char c out)))))