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

;; Encapsulating streams
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass encapsulating-input-stream (mel.gray-stream:fundamental-character-input-stream)
  ((encapsulated-input-stream :accessor encapsulated-input-stream :initarg :input-stream))))

#-(or sbcl cmu)
(defmethod mel.gray-stream:stream-read-char ((stream encapsulating-input-stream))
  (mel.gray-stream:stream-read-char (encapsulated-input-stream stream)))

#-(or sbcl cmu)
(defmethod mel.gray-stream:stream-unread-char ((stream encapsulating-input-stream) character)
  (mel.gray-stream:stream-unread-char (encapsulated-input-stream stream) character))

#+(or sbcl cmu)
(defmethod mel.gray-stream:stream-read-char ((stream encapsulating-input-stream))
  (read-char (encapsulated-input-stream stream)))

#+(or sbcl cmu)
(defmethod mel.gray-stream:stream-unread-char ((stream encapsulating-input-stream) character)
  (unread-char character (encapsulated-input-stream stream)))

#-abcl
(defmethod close ((stream encapsulating-input-stream) &key abort)
  (close (encapsulated-input-stream stream) :abort abort))

#+abcl
(defmethod stream-close ((stream encapsulating-input-stream) &key abort)
  (close (encapsulated-input-stream stream) :abort abort))


(defclass encapsulating-output-stream (mel.gray-stream:fundamental-character-output-stream)
  ((encapsulated-output-stream :accessor encapsulated-output-stream :initarg :output-stream)))

#-(or sbcl cmu)
(defmethod mel.gray-stream:stream-write-char ((stream encapsulating-output-stream) character)
  (mel.gray-stream:stream-write-char (encapsulated-output-stream stream) character))

#+(or sbcl cmu)
(defmethod mel.gray-stream:stream-write-char ((stream encapsulating-output-stream) character)
  (write-char character (encapsulated-output-stream stream)))

#+(or sbcl cmu)
(defmethod mel.gray-stream:stream-write-sequence ((stream encapsulating-output-stream) sequence &optional start end)
  (write-sequence sequence (encapsulated-output-stream stream) :start (or start 0) :end end))


#-abcl
(defmethod close ((stream encapsulating-output-stream) &key abort)
  (close (encapsulated-output-stream stream) :abort abort))

#+abcl
(defmethod stream-close ((stream encapsulating-output-stream) &key abort)
  (close (encapsulated-output-stream stream) :abort abort))
