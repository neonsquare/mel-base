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

(in-package :mel.folders.pop3)

;;;;;;;;;;;;;;;;;
;; POP3 folder ;;
;;;;;;;;;;;;;;;;;

;; Dependencies
;; MAKE-CONNECTION
;; Encapsulating Streams


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

(defun validate-command-in-state (command state)
  (case state 
    (:authorization (member command '(:user :quit :apop)))
    (:authorized (member command '(:user :pass :quit :apop)))
    (:transaction (member command '(:stat :list :retr :dele :noop :rset :quit :top :uidl)))
    (otherwise (error "Command ~A not valid in state ~A" command state))))

(defclass pop3-folder (basic-receiver)
  ((greeting :accessor greeting)
   (connection-stream :accessor connection-stream :initarg :connection-stream)
   (host :accessor host :initarg :host)
   (port :accessor pop3-port :initarg :port)
   (username :accessor username :initarg :username)
   (password :accessor password :initarg :password)
   (state :accessor state :initarg :state :initform :disconnected)
   (selected-messages :accessor selected-messages :initform nil)
   (uid-table :accessor uid-table :initform (make-hash-table :test 'equal))
   (idx-table :accessor idx-table :initform (make-hash-table :test 'equal))
   (size-table :accessor size-table :initform (make-hash-table :test 'equal))
   #+nil(message-table :accessor message-table :initform (make-hash-table :test 'equal))
   (storage-stream :accessor storage-stream :initform nil)))

(defclass pop3-message-input-stream (encapsulating-input-stream)
  ((state :accessor stream-state :initform :start-of-line)
   (last-state :accessor last-state :initform nil)))

(defun parse-pop3-response (response)
  (if (weird-mail.mime::string-prefixp "+OK" response)
    (values :ok (string-left-trim " " (subseq response 3)))
    (values :error (string-left-trim " " (subseq response 4)))))
            
(defun make-pop3-folder (&key (host "pop.web.de")(port 110) username password)
  (make-instance 'pop3-folder 
                 :name (format nil "pop3://~A!~A@~A:~A" username password host port)
                 :host host
                 :port port
                 :username username
                 :password password
                 :state :disconnected))

(defmethod serialize-folder ((folder pop3-folder) stream)
  (with-standard-io-syntax
    (write `(make-pop3-folder :host ,(host folder)
	     :port ,(pop3-port folder)
	     :username ,(username folder)
	     :password ,(password folder))
	   :stream stream)))

(defmethod ensure-connection ((folder pop3-folder))
  (when (eq (state folder) :disconnected)
    (setf (connection-stream folder) (mel.network:make-connection 
				      :element-type 'character
                                      :remote-host (host folder)
                                      :remote-port (pop3-port folder)))
    (setf (greeting folder) (read-line (connection-stream folder))
          (state folder) :authorization)
    (send-pop3-command folder :user (username folder))
    (send-pop3-command folder :pass (password folder)))
  (when (eql (read-char-no-hang (connection-stream folder) nil :eof) :eof)
    (setf (state folder) :disconnected)
    (ensure-connection folder))
  (connection-stream folder))

(defun rehash-messages (folder)
  (clrhash (uid-table folder))
  (clrhash (idx-table folder))
  (loop for (idx . uid) in (send-pop3-command folder :uidl)
        do (setf (gethash uid (uid-table folder)) idx
		 (gethash idx (idx-table folder)) uid)))

(defun rehash-sizes (folder)
  (clrhash (size-table folder))
  (loop for (idx . size) in (send-pop3-command folder :list)
    do (setf (gethash (gethash idx (idx-table folder)) (size-table folder)) size)))

(defmacro define-pop3-command (cmd &body body)
  `(defmethod send-pop3-command ((folder pop3-folder) (cmd (eql ,cmd)) &rest args)
     (declare (ignorable args))
     (multiple-value-bind (state response) 
	 (parse-pop3-response (read-delimited-line (connection-stream folder)))
       (ecase state
         (:ok ,@body)
         (:error (error "Error when sending ~A ~A (~A)" cmd (username folder) (string-trim " " response)))))))

(defmethod send-pop3-command :around ((connection pop3-folder) cmd &rest args)
  (handler-case
     (progn (ensure-connection connection)
       (validate-command-in-state cmd (state connection))
       (format (connection-stream connection) "~A~{ ~A~}~A~A" cmd args #\return #\linefeed)
       (force-output (connection-stream connection))
       (call-next-method))
    (end-of-file () (setf (state connection) :disconnected)
                 (error "Command aborted (Server Disconnect)"))))

(define-pop3-command :user
  (setf (state folder) :authorized)
  (values))

(define-pop3-command :pass
  (setf (state folder) :transaction)
  (rehash-messages folder)
  (rehash-sizes folder)
  (values))

(define-pop3-command :dele (values))


(define-pop3-command :rset (values))

(define-pop3-command :quit
  (setf (state folder) :disconnected)
  (close (connection-stream folder))
  (values))

(define-pop3-command :list
  (cond ((null args) (with-open-stream (long (make-instance 'pop3-message-input-stream 
                                                            :input-stream (connection-stream folder)))
                       (loop for i upfrom 1
                             for line = (read-line long nil nil)
                             while line
                             collect (multiple-value-bind (message-nr pos) 
                                         (parse-integer line :start 0 :junk-allowed t)
                                       (let ((octets (parse-integer line :start (1+ pos) :junk-allowed t)))
                                         (cons message-nr octets))))))
        (t
         (multiple-value-bind (message-nr pos) (parse-integer response :start 0 :junk-allowed t)
           (let ((octets (parse-integer response :start (1+ pos) :junk-allowed t)))
             (cons message-nr octets))))))

(define-pop3-command :uidl
  (cond ((null args) (with-open-stream (long (make-instance 'pop3-message-input-stream 
                                                            :input-stream (connection-stream folder)))
                       (loop for i upfrom 1
                             for line = (read-line long nil nil)
                             while line
                             collect (multiple-value-bind (message-nr pos) 
                                         (parse-integer line :start 0 :junk-allowed t)
                                       (let ((pop-uid (string-trim " " (subseq line (1+ pos)))))
                                         (cons message-nr pop-uid))))))
        (t
         (multiple-value-bind (message-nr pos) (parse-integer response :start 0 :junk-allowed t)
           (let ((pop-uid (string-trim " " (subseq response (1+ pos)))))
             (cons message-nr pop-uid))))))

(define-pop3-command :retr (values))

(define-pop3-command :top (values))

;;;;;;;;;;;;;;;;;;;;;
;; Folder Protocol ;;
;;;;;;;;;;;;;;;;;;;;;


(defmethod close-folder ((folder pop3-folder))
  (unless (eq (state folder) :disconnected)
    (send-pop3-command folder :quit)))

;;; Implementation of the POP3-MESSAGE-INPUT-STREAM

(defclass input-buffer-mixin ()
  ((input-buffer :accessor input-buffer :initform (make-string 4096))
   (input-buffer-limit :accessor input-buffer-limit :initform 0)
   (input-buffer-index :accessor input-buffer-index :initform 0)))

#+nil
(defmethod fill-buffer ((stream input-buffer-mixin))
  (read-string-non-blocking (input-buffer stream) stream
			    :start (input-buffer-index stream)
			    :end (input-buffer-limit stream)))

(defmethod (setf state) :before (new-value (stream pop3-message-input-stream))
  (declare (ignore new-value))
  (push (stream-state stream) (last-state stream)))

(defmethod mel.gray-stream:stream-unread-char :after ((stream pop3-message-input-stream) character)
  (declare (ignore character))
  (setf (stream-state stream) (pop (last-state stream))))

(defmethod mel.gray-stream::stream-peek-char ((stream pop3-message-input-stream))
  (let ((eis (encapsulated-input-stream stream)))
    (cond ((eql (stream-state stream) :eof) :eof)
          ((and (eq (stream-state stream) :start-of-line)
                (eql #\. (peek-char nil eis nil nil))
                (read-char eis nil nil)
                (weird-mail.mime::accept-crlf eis))
           :eof)
          ((eq (stream-state stream) :lf)  #\linefeed)
          (t (handler-case
                       (peek-char nil eis)
               (end-of-file () (error "Unexpected end of stream")))))))

(defmethod mel.gray-stream:stream-read-char ((stream pop3-message-input-stream))
  (let ((eis (encapsulated-input-stream stream)))
    (handler-case
        (cond ((eql (stream-state stream) :eof) :eof)           ; If its :eof it persists.
              ((and (eq (stream-state stream) :start-of-line)   
                    (eql #\. (peek-char nil eis))        ; A dot at the start of a line 
                    (read-char eis)                      ; followed by a CRLF sequence	
                    (weird-mail.mime::accept-crlf eis))			 ; marks the EOF (CRLF.CRLF)    
               (setf (stream-state stream) :eof)
               :eof)
              ((eq (stream-state stream) :lf) (setf (stream-state stream) :start-of-line) #\linefeed) ; A LF following a CR lets the line start new
              (t (cond ((weird-mail.mime::accept-crlf eis) (setf (stream-state stream) :lf) #\return) ;; Begin a CRLF Sequence
                       (t (setf (stream-state stream) :in-line)(read-char eis)))))
      (end-of-file () (error "Unexpected end of stream")))))

; Cases in block parsing
; [xxxCL.CL][]->[xxx]
; [xxxCL.C] [L]->[xxx]
; [xxxCL.]  [CL]->[xxx]
; [xxxCL]   [.CL]->[xxx]
; [xxxC]    [L.CL]->[xxx]
; [xxx]     [CL.CL]->[xxx]
; 6 Patterns
; 4 with CL
; 3 with CL.
; 5 with C
;1) (search "CL.") CL,CX,XX (three cases over)




#-abcl
(defmethod close ((stream pop3-message-input-stream) &key abort)
  (unless abort (loop for c = (read-char stream nil nil) while c)))

#+abcl
(defmethod stream-close ((stream pop3-message-input-stream) &key abort)
  (unless abort (loop for c = (read-char stream nil nil) while c)))



(defmethod message-header-stream-using-folder ((folder pop3-folder) (message message))
  (send-pop3-command folder :top (idx message) 0)
  (make-instance 'pop3-message-input-stream :input-stream (connection-stream folder)))

(defmethod open-message-input-stream-using-folder ((folder pop3-folder) message start)
  (send-pop3-command folder :retr (idx (find-message folder (uid message))))
  (let ((stream (make-instance 'pop3-message-input-stream :input-stream (connection-stream folder))))
    (loop repeat start do (read-char stream))
    stream))

;;; Folder Protocol Support

(defun idx (message)
  (multiple-value-bind (idx existsp)
      (gethash (uid message) (uid-table (folder message)))
    (cond (existsp idx)
          (t
           (rehash-messages (folder message))
           (idx message)))))

(defmethod map-messages (fn (folder pop3-folder))
  (nreverse (mapcar (lambda (uid) 
		      (let ((message (find-message folder uid :if-does-not-exist :create)))
			(funcall fn message)
			message))
                    (mapcar #'cdr (send-pop3-command folder :uidl)))))

(defmethod message-size-using-folder ((folder pop3-folder) (message message))
  (multiple-value-bind (size existsp)
      (gethash (uid message) (size-table (folder message)))
    (cond (existsp size)
	  (t (rehash-messages folder)
	     (rehash-sizes folder)
	     (message-size-using-folder folder message)))))

(defmethod copy-folder ((source-folder pop3-folder) (sink-folder folder))
  "Overload for more efficient message transfer"
  (dolist (message (sort (messages source-folder)
		     #'< :key #'message-size))
    (with-open-stream (source (open-message-input-stream-using-folder
			       source-folder message 0))
      (with-open-stream (sink (open-message-storing-stream sink-folder message))
        (let ((buffer (make-string 4096)))
	  (declare (dynamic-extent buffer))
	  (loop for position = (read-sequence buffer source)
	        until (zerop position)
	        do (write-sequence buffer sink :start 0 :end position)))
	#+nil(loop for c = (read-char source nil nil)
              while c do (write-char c sink))))))

(defmethod copy-folder ((source-folder folder) (sink-folder pop3-folder))
  (cerror "Cancel copying of folder ~A to the pop3 folder ~A" "POP3 folder are not supported as message sinks" source-folder sink-folder))

(defmethod move-folder ((source-folder pop3-folder) sink-folder)
  (dolist (uid (mapcar #'cdr (send-pop3-command source-folder :uidl)))
    (let ((message (find-message source-folder uid)))
      (copy-message message sink-folder)
      (send-pop3-command source-folder :dele (idx message)))))

(defmethod clear-folder ((folder pop3-folder))
  (handler-case
      (dolist (uid (mapcar #'cdr (send-pop3-command folder :uidl)))
        (send-pop3-command folder :dele (idx (find-message folder uid))))
    (error () 
           (send-pop3-command folder :rset)
           (error "Could not clear folder - transaction rolled back")))
  (send-pop3-command folder :quit))

(defmethod delete-message-using-folder ((folder pop3-folder) message)
  (send-pop3-command folder :dele (idx (find-message folder (uid message)))))

(defmethod count-messages ((folder pop3-folder))
  (length (send-pop3-command folder :list)))

(defmethod short-name ((folder pop3-folder))
  (format nil "pop3://~A@~A" (username folder) (host folder)))
