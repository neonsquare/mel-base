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

(defclass message-cache-mixin ()
  ((message-cache :accessor message-cache)))

(defclass hash-table-message-cache-mixin (message-cache-mixin) ())

(defclass equal-message-cache-mixin (hash-table-message-cache-mixin) ())

(defclass eql-message-cache-mixin (hash-table-message-cache-mixin) ())

(defclass eq-message-cache-mixin (hash-table-message-cache-mixin) ())

(defmethod allocate-message-cache ((folder eq-message-cache-mixin))
  (make-hash-table :test 'eq))

(defmethod allocate-message-cache ((folder eql-message-cache-mixin))
  (make-hash-table :test 'eql))

(defmethod allocate-message-cache ((folder equal-message-cache-mixin))
  (make-hash-table :test 'equal))

(defmethod initialize-instance :after ((object message-cache-mixin) &key)
  (setf (message-cache object) (allocate-message-cache object)))

(defmethod map-message-cache (fn (folder hash-table-message-cache-mixin))
  (maphash fn (message-cache folder)))

(defmethod uid= ((folder eq-message-cache-mixin) message1 message2)
  (eq (uid message1) (uid message2)))

(defmethod uid= ((folder eql-message-cache-mixin) message1 message2)
  (eql (uid message1) (uid message2)))

(defmethod uid= ((folder equal-message-cache-mixin) message1 message2)
  (equal (uid message1) (uid message2)))

;; receiver-protocol

(defgeneric FIND-MESSAGE (folder uid &key if-does-not-exist))

(defmethod find-message ((folder hash-table-message-cache-mixin) uid &key (if-does-not-exist :error))
  "This method provides caching of message objects. New message
   objects get only consed when a message with the given uid
   does not yet exist in the message-cache."
  (declare (ignore if-does-not-exist))
  (or (gethash uid (message-cache folder))
      (setf (gethash uid (message-cache folder)) (call-next-method))))

;; folder-protocol
(defmethod close-folder :after ((folder hash-table-message-cache-mixin))
  (clrhash (message-cache folder)))