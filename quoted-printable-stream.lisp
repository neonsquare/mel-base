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

(defclass quoted-printable-input-stream (encapsulating-input-stream)
  ((octet-count :initarg :octet-count :initform 0 :accessor octet-count)))

(defparameter *digit-chars* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun char-integer (char)
  (position (char-upcase char) *digit-chars*))

(defmethod mel.gray-stream:stream-read-char ((stream quoted-printable-input-stream))
  (let* ((eis (encapsulated-input-stream stream))
         (c (if (zerop (octet-count stream)) :eof (read-char eis nil :eof))))
    (flet ((get-char (eis)
	     (if (zerop (octet-count stream))
		 (signal 'end-of-file)
		 (let ((c (read-char eis)))
		   (decf (octet-count stream))
		   c))))
    (unless (eq c :eof)
      (decf (octet-count stream)))
    (if (eql c #\=)
      (handler-case
          (let ((c (peek-char nil eis)))
            (cond ((digit-char-p c 16) 
		   (get-char eis)
		   (code-char (+ (* (char-integer c) 16)
				 (char-integer (get-char eis)))))
                  ((eql c #\return) (get-char eis)(get-char eis)(read-char stream))
                  ((eql c #\newline) #+nil(warn "Newline read instead of CR")(get-char eis)(read-char stream))
                  (t (warn "Illegal quoted printable"))))
        (end-of-file () (error "Unexpected end of file")))
      c))))

(defun decode-quoted-printable (qp)
  (with-input-from-sequence (s qp)
    (with-open-stream (in (make-instance 'quoted-printable-input-stream
                                         :octet-count (length qp)
                                         :input-stream s))
      (with-output-to-string (out)
        (loop for c = (read-char in nil :eof)
              until (eq c :eof)
              do (when c (write-char c out)))))))

(defun decode-quoted-printable-header (string)
  (if (and (> (length string) 2)
           (eql (char string 0) #\=)
           (eql (char string 1) #\?))
      (let* ((start (search "?Q?" string))
             (end (search "?=" string :start1 0 :start2 (or start 0))))
	(if (and start end)
	  (let ((encoded (subseq string (+ start 3) end)))
	    (decode-quoted-printable encoded))
	  string))
      string))