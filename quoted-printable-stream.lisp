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

(in-package :weird-mail.internal)

;; Quoted Printable input-stream

(defclass quoted-printable-input-stream (encapsulating-input-stream)())

(defparameter *digit-chars* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun char-integer (char)
  (position (char-upcase char) *digit-chars*))   

(defmethod mel.gray-stream:stream-read-char ((stream quoted-printable-input-stream))
  (let* ((eis (encapsulated-input-stream stream))
         (c (read-char eis nil :eof)))
    (if (eql c #\=)
      (handler-case
          (let ((c (peek-char nil eis)))
            (cond ((digit-char-p c 16) (read-char eis) (code-char (+ (* (char-integer c) 16)
                                                                     (char-integer (read-char eis)))))
                  ((eql c #\return) (read-char eis)(read-char eis)(read-char stream))
                  ((eql c #\newline) #+nil(warn "Newline read instead of CR")(read-char eis)(read-char stream))
                  (t (warn "Illegal quoted printable"))))
        (end-of-file () (error "Unexpected end of file")))
      c)))