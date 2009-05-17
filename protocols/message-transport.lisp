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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file implements the message-transport protocol of ;;
;;; Mel.                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mel.internal)

(defgeneric COPY-MESSAGE-USING-FOLDERS (message message-folder sink-folder))
(defgeneric DELETE-MESSAGE-USING-FOLDER (folder message))
(defgeneric MOVE-MESSAGE-USING-FOLDERS (message message-folder sink-folder))

(defgeneric CLEAR-FOLDER (folder))
(defgeneric COPY-FOLDER (source-folder sink-folder))
(defgeneric MOVE-FOLDER (source-folder sink-folder))

;; Message related methods

(defun copy-message (message folder)
  (copy-message-using-folders message (folder message) folder))

(defun delete-message (message)
  (delete-message-using-folder (folder message) message))

(defun move-message (message folder)
  (move-message-using-folders message (folder message) folder))

(defmethod copy-message-using-folders :before ((message message) message-folder (sink-folder folder))
  "Check if folders are really different"
  (declare (ignore message-folder))
  (when (eq sink-folder (folder message))
    (error "Cannot copy message into its own folder")))

#+(or sbcl cmu)
(defmethod copy-message-using-folders ((message message) message-folder (sink-folder folder))
  "Copy a message (contained in some folder) into another folder"
  (declare (ignore message-folder))
  (with-open-stream (source (open-message-input-stream message))
    (with-open-stream (sink (open-message-storing-stream sink-folder message))
      (loop for c = (read-char source nil nil)
            while c do (write-char c sink)))))

;; Using Block-IO
#-(or sbcl cmu)
(defmethod copy-message-using-folders ((message message) message-folder (sink-folder folder))
  "Copy a message (contained in some folder) into another folder"
  (declare (optimize (speed 3) (safety 0))
	   (ignore message-folder))
  (with-open-stream (source (open-message-input-stream message))
    (with-open-stream (sink (open-message-storing-stream sink-folder message))
      (let ((buffer (make-string 8192)))
	(declare #-(or sbcl cmu)(dynamic-extent buffer))
	(loop for count of-type fixnum = (read-sequence buffer source :start 0 :end 8192)
	  until (zerop count)
	  do 
	  (write-sequence buffer sink :start 0 :end count))))))

(defmethod move-message-using-folders :before ((message message) message-folder (sink-folder folder))
  "Check if folders are really different"
  (declare (ignore message-folder))
  (when (eq sink-folder (folder message))
    (error "Cannot copy message into its own folder")))

#+problem
(defmethod move-message-using-folders :after ((message message) message-folder (sink-folder folder))
  "Set origin folder"
  (declare (ignore message-folder))
  (unless (origin-folder message)
    (setf (origin-folder message) message-folder
	  (folder message) sink-folder)))

(defmethod move-message-using-folders :around ((message message) message-folder (sink-folder folder))
  "Set message folder"
  (declare (ignore message-folder))
  (let ((uid (call-next-method)))
    (setf (folder message) sink-folder
	  (uid message) uid)
    uid))

(defmethod move-message-using-folders ((message message) message-folder (sink-folder folder))
  "Copy a message (contained in some folder) into another folder"
  (let ((uid (copy-message-using-folders message message-folder sink-folder)))
    (delete-message-using-folder message-folder message)
    uid))

;; Folder related methods

(defmethod clear-folder ((folder folder))
  (map nil #'delete-message (messages folder)))

(defmethod copy-folder ((source-folder basic-folder) (sink-folder basic-folder))
  "Copy the contents of one folder two another folder. More efficient
implementations might be defined on particular folder-types"
  (loop for message in (messages source-folder)
        do (copy-message message sink-folder)))

(defmethod move-folder ((source-folder basic-folder) (sink-folder basic-folder))
  "Copy the contents of one folder two another folder. More efficient
implementations might be defined on particular folder-types"
  (loop for message in (messages source-folder)
        do (move-message message sink-folder)))