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

(defclass line-terminator-input-stream (encapsulating-input-stream)())
(defclass line-terminator-output-stream (encapsulating-output-stream)())   

(defclass unix-line-terminator-input-stream (encapsulating-input-stream)())
(defclass mac-line-terminator-input-stream (encapsulating-input-stream)())
(defclass rfc-line-terminator-input-stream (encapsulating-input-stream)())

(defmethod mel.gray-stream:stream-read-char ((stream unix-line-terminator-input-stream))
  (let* ((eis (encapsulated-input-stream stream))
         (c (read-char eis nil :eof)))
    (case c
      (#\return (mel.gray-stream:stream-read-char stream))
      (#\linefeed #\newline)
      (otherwise c))))

(defmethod mel.gray-stream:stream-read-char ((stream mac-line-terminator-input-stream))
  (let* ((eis (encapsulated-input-stream stream))
         (c (read-char eis nil :eof)))
    (case c
      (#\linefeed (mel.gray-stream:stream-read-char stream))
      (#\return #\newline)
      (otherwise c))))

(defmethod mel.gray-stream:stream-read-char ((stream rfc-line-terminator-input-stream))
  (let* ((eis (encapsulated-input-stream stream))
         (c (read-char eis nil :eof)))
    (case c
      (#\return (mel.gray-stream:stream-read-char stream))
      (#\linefeed #\newline)
      (otherwise c))))

(defclass unix-line-terminator-output-stream (encapsulating-output-stream)())
(defclass mac-line-terminator-output-stream (encapsulating-output-stream)())
(defclass rfc-line-terminator-output-stream (encapsulating-output-stream)())

(defmethod mel.gray-stream:stream-write-char ((stream unix-line-terminator-output-stream) character)
  (let ((eis (encapsulated-output-stream stream)))
    (case  character 
      (#\newline (write-char #\linefeed eis))
      (#\return nil)
      (otherwise (write-char character eis)))))

(defmethod mel.gray-stream:stream-write-char ((stream mac-line-terminator-output-stream) character)
  (let ((eis (encapsulated-output-stream stream)))
    (case  character 
      (#\newline (write-char #\return eis))
      ;; on openmcl newline and linefeed are the same character
      #-openmcl (#\linefeed nil)
      (otherwise (write-char character eis)))))

(defmethod mel.gray-stream:stream-write-char ((stream rfc-line-terminator-output-stream) character)
  (let ((eis (encapsulated-output-stream stream)))
    (case character
      (#\newline (write-char #\return eis) (write-char #\linefeed eis))
      (#\return nil)
      ;; on openmcl newline and linefeed are the same character
      #-openmcl(#\linefeed nil)
      (otherwise (write-char character eis)))))