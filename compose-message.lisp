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

(in-package :mel.mime)

(defparameter *mime-base-header-fields*
  '((:mime-version . "1.0")))
    
(defparameter *message-counter* 0)

(defun generate-message-id ()
  (format nil "<~D~D~D@~A>" 
          (mel.unix:universal-to-unix-time (get-universal-time))
          (mel.environment:getpid)
	  (incf *message-counter*)
          (mel.environment:gethostname)))

(defun make-message
    (&key (subject "") from to cc attached-files date additional-header-fields (body ""))
    (declare (ignore additional-header-fields))
  (let ((message (make-instance 'mime-message
				:folder nil)))
    (setf (header-fields message) (copy-tree *mime-base-header-fields*))
    (setf (date message) (or date (get-universal-time)))
    (setf (message-id message) (generate-message-id))
    (setf (subject message) subject)
    (when from
      (setf (from message) (address from)))
    (when to
      (setf (to message) (if (consp to) (mapcar #'address to) (address to))))
    (when cc
      (setf (cc-list message) (if (consp cc) (mapcar #'address cc) (address cc))))

    (when body
      (finalize-message  message :attached-files attached-files :body body))
      message))

(defun finalize-message (message &key attached-files body)
  (finalize-message-using-folder (folder message) message
				 :attached-files attached-files
				 :body body))

(defmethod finalize-message-using-folder ((folder null)(message mime-message) &key attached-files body)
  (with-open-file (stream (merge-pathnames (message-id message) (ensure-directories-exist "/tmp/mel/"))
			  :direction :output
			  :if-exists :supersede)
    (unless body (error "Message not finalized: supply a body"))
    (if attached-files
	(make-multipart-body message attached-files body stream)
	(make-message-body message body stream))))

(defun make-message-from-file
    (file)
  (let ((message (make-instance 'mime-message
				:folder (pathname file))))
    (with-open-file (s file)
      (setf (header-fields message) (read-rfc2822-header s)))
    message))

(defun make-message-body (message body stream)
  (etypecase body
      (string (write-sequence body stream))
      (function (funcall body message stream))
      (stream (loop for c = (read-char body nil :eof)
		    until (eq c :eof) do (write-char c stream)))
      (symbol (ecase body
		(:interactive 
		 (with-open-file (in (edit-message-body message) :if-does-not-exist :error)
  		   (loop for c = (read-char in nil :eof)
			 until (eq c :eof) do (write-char c stream))))))))

(defun split-string (string)
  (loop for start = 0 then (1+ end)
	for end = (position-if (lambda (c)
				 (case c
				   ((#\space #\tab) t)
				   (otherwise nil)))
			       string :start start)
	collect (subseq string start end)
	unless end do (loop-finish)))

(defvar *mime-types-file* (or (probe-file "/etc/mime.types")
                              (probe-file (merge-pathnames "mime.types" *load-truename*))))

(defun parse-mime-table (&optional (file *mime-types-file*))
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil nil)
	  while line
	  for tokens = (remove-if (lambda (s) (zerop (length s)))
				  (split-string line))
	  if (and tokens
		  (rest tokens)
		  (not (eql (char line 0)
			    #\#)))
	  collect (cons (rest tokens)
			(first tokens)))))

(defparameter *mime-table* 
  (if *mime-types-file*
    (parse-mime-table *mime-types-file*) 
    nil))

(defun guess-content-type (file)
  (let ((pathname-type (pathname-type file :case :common)))
    (or
    (cdr (find-if (lambda (item)
	       (member pathname-type (car item) :test #'equalp))
                   *mime-table*))
     "application/octet-stream")))

(defun make-multipart-body (message files body stream)
  (let ((boundary-tag (make-boundary-tag)))
    (setf (field :content-type message t)
	  (make-content-type :multipart :mixed :boundary boundary-tag))
    (format stream "This is a MIME Multipart Message.~A~A" #\return #\linefeed)
    (format stream "--~A~A~A~A~A" boundary-tag #\return #\linefeed #\return #\linefeed)
    (make-message-body message body stream)
    (dolist (file files)
      (format stream "~A~A--~A~A~A" #\return #\linefeed boundary-tag #\return #\linefeed)
      (let ((content-type (if (consp file) (second file) (guess-content-type file)))
	    (file (if (consp file) (first file) file)))
	(format stream "Content-Type: ~A~A~A" content-type #\return #\linefeed)
	(format stream "Content-Transfer-Encoding: base64~A~A" #\return #\linefeed)
	(format stream "Content-Disposition: attachment; filename=\"~A\"~A~A"
		(enough-namestring file (make-pathname :defaults file :name nil :type nil))
		#\return #\linefeed)
	(write-char #\return stream)
	(write-char #\linefeed stream)
	(with-open-file (in file :direction :input :element-type '(unsigned-byte 8))
			(loop with buffer = (make-array 60 :element-type '(unsigned-byte 8))
			      for count = (read-sequence buffer in)
			      while (> count 0)
			      do (write-sequence (encode-base64 buffer) stream)
			      	(write-char #\return stream)
				(write-char #\linefeed stream)))))
    (format stream "--~A--~A~A"  boundary-tag #\return #\linefeed)))

(defmethod open-message-input-stream-using-folder ((folder pathname) (message message) start)
  (declare (ignore start))
  (open folder :direction :input))

(defmethod ensure-headers-read ((folder pathname) (message message))
  (unless (weird-mail.mime:header-fields message)
    (with-open-stream (stream (message-header-stream-using-folder folder message))
      (setf (weird-mail.mime:header-fields message) (weird-mail.mime:read-rfc2822-header stream)))))

(defmethod open-message-input-stream-using-folder ((folder (eql nil)) (message message) start)
  (let ((headers (with-output-to-string (s) (write-rfc2822-header (header-fields message) s))))
    (let ((header-stream (make-sequence-input-stream headers))
	  (body-stream (open (merge-pathnames (message-id message) "/tmp/mel/") :direction :input :if-does-not-exist :error
			     :external-format #-lispworks :default #+lispworks '(:latin-1 :eol-style :lf))))
      (let ((header-length (length headers)))
	(cond ((zerop start) (make-concatenated-stream header-stream body-stream))
	      ((< start header-length) 
	       (file-position header-stream start)
	       (make-concatenated-stream header-stream body-stream))
	      ((> start header-length)
	       (file-position body-stream (- start header-length))
	       body-stream)
	      (t body-stream))))))

(defmethod message-body-stream-using-folder ((folder pathname) (message message))
  (let ((body-start (weird-mail.mime:body-start message)))
    (if body-start
	(open-message-input-stream message body-start)
      (let ((stream (open-message-input-stream message)))
	(skip-rfc2822-header stream)
	stream))))

(defun edit-message-body (message)
  (let ((file (merge-pathnames (concatenate 'string (message-id message) "-body") "/tmp/mel/")))
  (ed file)
  file))
				