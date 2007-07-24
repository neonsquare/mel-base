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

(in-package :mel.folders.smtp)

;;;;;;;;;;;;;;;;;;;;;;;
;; SMTP Relay Folder ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Dependencies
;; MAKE-CONNECTION
;; Encapsulating Streams

(defclass smtp-folder (basic-folder) ())

(defclass smtp-relay-folder (smtp-folder basic-sender)
  ((host :accessor host :initarg :host)
   (smtp-port :accessor smtp-port :initarg :port)
   (username :accessor username :initarg :username)
   (password :accessor password :initarg :password)
   (connection-stream :accessor connection-stream :initform nil)
   (state :accessor state :initform :disconnected :initarg :state)
   (greeting :accessor greeting :initform nil)
   (properties :accessor properties :initform nil)))

(defclass lmtp-folder (smtp-relay-folder)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass smtp-message-storing-stream (encapsulating-output-stream)
  ((message :accessor message :initarg :message)
   (folder :accessor folder :initarg :folder)
   (state :accessor state :initform :start-of-line)))
)

(defmethod mel.gray-stream:stream-write-char ((stream smtp-message-storing-stream) character)
  (let ((eis (encapsulated-output-stream stream)))
    (handler-case
        (cond ((and (eq (state stream) :start-of-line)
		     (eql #\. character))
		(write-string ".." eis)
		#\.)
              (t (case character
		   (#\return (setf (state stream) :cr) #\return)
		   (#\linefeed (write-char #\return eis)
			       (write-char #\linefeed eis)
			       (setf (state stream) :start-of-line)
			       #\newline)
		   (otherwise 
		    (cond ((eq (state stream) :cr)
			   (write-char #\return eis)
			   (write-char #\linefeed eis)
			   (setf (state stream) :start-of-line)
			   (write-char character stream)) ;; Call protocol recursively
			  (t (setf (state stream) :in-line)
			     (write-char character eis)))))))
      (end-of-file () (error "Unexpected end of stream")))))


#-abcl
(defmethod cl:close ((stream smtp-message-storing-stream) &key abort)
  (unless abort
    (let ((eis (encapsulated-output-stream stream)))
      (write-char #\return eis)
      (write-char #\linefeed eis)
      (write-char #\. eis)
      (write-char #\return eis)
      (write-char #\linefeed eis)
      (send-smtp-command (folder stream) :quit)))
  (close (encapsulated-output-stream stream) :abort abort))

#+abcl
(defmethod stream-close ((stream smtp-message-storing-stream) &key abort)
  (unless abort
    (let ((eis (encapsulated-output-stream stream)))
      (write-char #\return eis)
      (write-char #\linefeed eis)
      (write-char #\. eis)
      (write-char #\return eis)
      (write-char #\linefeed eis)
      (send-smtp-command (folder stream) :quit)))
  (close (encapsulated-output-stream stream) :abort abort))


(defmethod open-message-storing-stream ((folder smtp-relay-folder) message)
  (let ((stream (make-instance 'smtp-message-storing-stream
			       :output-stream (ensure-connection folder)
			       :message message
			       :folder folder)))
    (let ((mail-from (address-spec (or (sender message) (from message))))
	  (recipients (let ((to (to message)))
			(mapcar #'address-spec (if (consp to)
						   (append (to message) (cc-list message))
						 (cons (to message) (cc-list message)))))))
      (send-smtp-command folder :mail-from mail-from)
      (dolist (recipient recipients)
	(send-smtp-command folder :rcpt-to recipient))
      (send-smtp-command folder :data))
    stream))

(defmethod open-message-storing-stream :around ((folder smtp-relay-folder) message)
  (declare (ignore message))
  (call-next-method))

;; Internals ;;

(defun read-delimited-line (in-stream &key (endp #'weird-mail.mime::accept-crlf) (eof-error-p t) (eof-value nil))
  (with-output-to-string (line)
    (handler-case
        (loop (when (funcall endp in-stream) (return-from read-delimited-line 
                                                 (values (get-output-stream-string line) nil)))
              (let ((c (read-char in-stream)))
                (write-char c line)))
      (end-of-file ()(let ((result (get-output-stream-string line)))
                       (if (plusp (length result))
                         (return-from read-delimited-line 
                           (values result t))
                         (if eof-error-p
                           (error 'end-of-file :stream in-stream)
                           (values eof-value t))))))))

(defun make-smtp-relay-folder (&key (host "mail.web.de")(port 25) username password)
  (make-instance 'smtp-relay-folder 
                 :name (format nil "smtp://~A!~A@~A:~A" username password host port)
                 :host host
                 :port port
                 :username username
                 :password password
                 :state :disconnected))

(defmethod serialize-folder ((folder smtp-relay-folder) stream)
  (with-standard-io-syntax
    (write `(make-smtp-relay-folder :host ,(host folder)
	     :port ,(smtp-port folder)
	     :username ,(username folder)
	     :password ,(password folder))
	   :stream stream)))

(defun auth-plain-ticket (user password)
  (let ((ticket (make-array (+ (length user) (length password) 2) :element-type '(unsigned-byte 8))))
    (setf (aref ticket 0) 0)
    (setf (aref ticket (1+ (length user))) 0)
    (loop for c across user
          for i from 1 do (setf (aref ticket i) (char-code c)))
    (loop for c across password
          for i from (+ 2 (length user)) do (setf (aref ticket i) (char-code c)))
	  
    (values (weird-mail.mime::encode-base64 ticket) ticket)))

(defmethod ensure-connection ((folder smtp-relay-folder))
  (when (eq (state folder) :disconnected)
    (setf (connection-stream folder) (mel.network:make-connection
				      :element-type 'character
                                      :remote-host (host folder)
                                      :remote-port (smtp-port folder)))
    (setf (greeting folder) (nth-value 3 (read-smtp-response (connection-stream folder)))
          (state folder) :connected)
    (format t "Greeting ~A~%" (greeting folder))
    (send-smtp-command folder :ehlo (mel.environment:gethostname))
    (format t "Properties ~A~%" (properties folder))
    (when (search "PLAIN" (getf (properties folder) :auth) :test #'equalp)
      (send-smtp-command folder :auth "PLAIN" (auth-plain-ticket (username folder) (password folder))))
    #+nil(send-smtp-command folder :helo (mel.environment:gethostname)))
  (when (eql (read-char-no-hang (connection-stream folder) nil :eof) :eof)
    (setf (state folder) :disconnected)
    (ensure-connection folder))
  (connection-stream folder))

(defmethod ensure-connection ((folder lmtp-folder))
  (when (eq (state folder) :disconnected)
    (setf (connection-stream folder) (mel.network:make-connection
				      :element-type 'character
                                      :remote-host (host folder)
                                      :remote-port (smtp-port folder)))
    (setf (greeting folder) (read-delimited-line (connection-stream folder))
          (state folder) :connected)
    (send-smtp-command folder :lhlo (mel.environment:gethostname)))
  (when (eql (read-char-no-hang (connection-stream folder) nil :eof) :eof)
    (setf (state folder) :disconnected)
    (ensure-connection folder))
  (connection-stream folder))


(defun decode-reply-code (code)
  (case code
    (211 (values :system-status                 :completed         :information))
    (214 (values :help-message                  :completed         :information))
    (220 (values :service-ready                 :completed         :connection))
    (221 (values :service-closing-transmission  :completed         :connection))
    (235 (values :authentication-successful     :completed         :authentication))
    (250 (values :mail-action-ok                :completed         :mail-system))
    (251 (values :user-not-local                :completed         :mail-system))
    (252 (values :cannot-verify-user            :completed         :mail-system))
    (334 (values :start-authentication          :intermediate      :connection))
    (354 (values :start-mail-input              :intermediate      :mail-system))
    (421 (values :service-not-available         :transient-failure :connection))
    (450 (values :mailbox-busy                  :transient-failure :mail-system))
    (451 (values :local-processing-error        :transient-failure :mail-system))
    (452 (values :insufficient-system-storage   :transient-failure :mail-system))
    (500 (values :command-unrecognized          :permanent-failure :syntax))
    (501 (values :bad-parameters                :permanent-failure :syntax))
    (502 (values :command-not-implemented       :permanent-failure :syntax))
    (503 (values :bad-command-sequence          :permanent-failure :syntax))
    (504 (values :parameter-not-implemented     :permanent-failure :syntax))
    (550 (values :mailbox-unavailable           :permanent-failure :mail-system))
    (551 (values :user-not-local                :permanent-failure :mail-system))
    (552 (values :insufficient-system-storage   :permanent-failure :mail-system))
    (553 (values :bad-mailbox-name              :permanent-failure :mail-system))
    (554 (values :transaction-failed            :permanent-failure :mail-system))
    (otherwise (values :unknown-reply           :permantent-failure :syntax))))

(defun read-smtp-response (stream)
  (loop for response = (read-delimited-line stream)
	for reply-code = (parse-integer response :start 0 :end 3)
	while (char= (char response 3) #\-)
	collect (subseq response 4) into replies
	finally (return (multiple-value-bind (reply severity category)
			    (decode-reply-code reply-code)
			  (values reply severity category (nconc replies
								(list (subseq response 4))))))))

(defun command-string (cmd)
  (ecase cmd
    (:mail-from "MAIL FROM:")
    (:rcpt-to "RCPT TO:")
    (:helo "HELO")
    (:ehlo "EHLO")
    (:lhlo "LHLO")
    (:data "DATA")
    (:quit "QUIT")
    (:auth "AUTH")))

(defmethod validate-command-in-state ((cmd t) (state t))
  t)

(defmethod send-smtp-command :around ((connection smtp-folder) cmd &rest args)
  (handler-case
     (progn (ensure-connection connection)
       (validate-command-in-state cmd (state connection))
       (format (connection-stream connection) "~A~{ ~A~}~A~A" (command-string cmd) args #\return #\linefeed)
       (force-output (connection-stream connection))
       (call-next-method))
    (end-of-file () (setf (state connection) :disconnected)
                 (error "Command aborted (Server Disconnect)"))))

(defmacro define-smtp-command (cmd &body body)
  `(defmethod send-smtp-command ((folder smtp-folder) (cmd (eql ,cmd)) &rest args)
     (declare (ignorable args))
     (multiple-value-bind (reply severity category responses)
	 (read-smtp-response (connection-stream folder))
       (case severity
         ((:completed :intermediate) ,@body)
         ((:transient-failure :permanent-failure)
	  (error "Error when sending ~A ~A ~A (~A)" cmd reply category
		 (string-trim " " (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) responses))))
         (otherwise (error "Critical Error: cmd: ~A reply: ~A category: ~A~%~A~%" cmd reply category responses))))))

(define-smtp-command :helo)

(define-smtp-command :ehlo
    (setf (properties folder)
	  (loop for response in (rest responses)
		for end = (position #\space response)
		nconcing (list (intern (subseq response 0 (or end (length response)))
				       (find-package :keyword))
			       (if end 
				   (string-trim " " (subseq response end))
				   t)))))

(define-smtp-command :lhlo
    (setf (properties folder)
	  (loop for response in (rest responses)
		for end = (position #\space response)
		nconcing (list (intern (subseq response 0 (or end (length response)))
				       (find-package :keyword))
			       (if end 
				   (string-trim " " (subseq response end))
				   t)))))


(define-smtp-command :mail-from
    )

(define-smtp-command :rcpt-to
    )

(define-smtp-command :data
    )

(define-smtp-command :auth
    )


(define-smtp-command :quit
    (close (connection-stream folder))
  (setf (state folder) :disconnected))

