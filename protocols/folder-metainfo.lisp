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

(defgeneric SHORT-NAME (folder))

(defgeneric COUNT-MESSAGES (folder))

(defgeneric MAP-MESSAGES (fn folder))

(defgeneric MESSAGES (folder))
(defgeneric RECENT-MESSAGES (folder))
(defgeneric UNREAD-MESSAGES (folder))

(defmethod messages ((folder basic-folder))
  (map-messages (lambda (m) (declare (ignore m)))
		folder))

(defmethod recent-messages ((folder basic-folder))
  (map-recent-messages (lambda (m) (declare (ignore m)))
		       folder))

(defmethod map-recent-messages (fn (folder basic-folder))
  (let ((all-messages (messages folder))
	non-recent-messages)
    (map-message-cache
     (lambda (uid message)
       (declare (ignore uid))
       (push message non-recent-messages))
     folder)
    (mapcar fn (set-difference all-messages non-recent-messages
                               :test #'uid=))))

(defmethod map-recent-messages :around (fn (folder basic-folder))
  (declare (ignore fn))
  (map nil (lambda (m) (unmark-message m :recent))
       (call-next-method)))

(defmethod count-messages ((folder folder))
  (length (messages folder)))

(defmethod short-name ((folder folder))
  (string-capitalize (name folder)))

(defmethod print-object ((folder basic-folder) stream)
  (print-unreadable-object (folder stream :type t :identity t)
    (when (slot-boundp folder 'name)
      (princ (name folder) stream))))