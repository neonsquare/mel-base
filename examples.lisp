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

(defpackage :mel-examples
  (:use :mel :cl :mel.pop3-server))

(in-package :mel-examples)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Working with mail folders ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1) Opening and creating mail folders
;; There are three kinds of mail folders so far: Maildir, POP3 and SMTP-Relay
;; Each mail folder has a dedicated constructor named make-<foldertype>

;; a) Open an existing Maildir folder

(defvar *inbox* (make-maildir-folder "/home/user/Mail/inbox/"))
; Note: the trailing slash in a Maildir folders name is mandatory!

;; b) Create a new (empty) Maildir folder

(defvar *outbox* (make-maildir-folder "/tmp/test-outbox/" :if-does-not-exist :create))

;; c) Open a POP3 folder

(defvar *pop3* (make-pop3-folder :host "pop.domain.test" 
				 :username "foo" 
				 :password "secret"))
; Note: It makes no sense to "create" a POP3 folder like above with maildir

(defvar *smtp* (make-smtp-relay-folder :host "smtp.domain.test"
				       :username "someone"
				       :password "else"))


;; 2) Access messages
;; Mel provides access to messages through proxy message objects which
;; stand as placeholders for the real messages. Depending on the support of the
;; underlying concrete folder representation the access is optimized to load
;; message-data only on demand. A sophisticated caching architecture enables
;; fast repeated access though.

;; How many messages are in a folder?
(defvar *how-many* (count-messages *inbox*))

;; Getting the messages of a folder
(defvar *messages* (messages *inbox*))

;; Print subjects and originator of all messages
(dolist (message *messages*)
  (format t "~A ~A~%" (from message) (subject message)))

;; Other header fields accessable from messages are:
;; * MESSAGE-ID   : The rfc2822 unique message id
;; * SENDER       : The sender of the message
;; * TO           : The recipient(s)
;; * CC-LIST      : additional recipients
;; * DATE         : the sending date as universal-time value
;; * CONTENT-TYPE : (VALUES super-type subtype parameters-as-alist)
;; and more...
;; All those fields are parsed from there string representation in a
;; objects which are more accessible to Lisp. Email Addresses are objects
;; of the class RFC2822-ADDRESS. The date field is parsed using DATE-TO-UNIVERSAL-TIME
;; into a Lisp universal time value.


;; The raw header fields can be accessed using the FIELD accessor
;; The field-name is given as keyword and is case insensitive.
;; If a field does not exist FIELD returns NIL
(defvar *date* (field :date (first *messages*)))
(defvar *x-spam* (field :x-spam (first *messages*)))

;; How to access the message body
(defvar *body* (with-output-to-string (out)
		 (with-open-stream (stream (message-body-stream (first *messages*)))
		   (loop for c = (read-char stream nil nil)
			 while c do (write-char c out)))))
;; Note this fetches the whole message body with no special handling of multiparts

;; How to access the parts of a multipart message
;; You can access the parts of a message using the PARTS function
;; This will give you a list of PART objects similar to messages
;; but contained in another message. The first part is always the whole
;; a part representing the whole message
(defvar *parts* (parts (make-parts (find :multipart *messages* :key #'content-type))))

;; Print the content types of all parts
;; You can access header fields of parts the same way as with messages
(dolist (part *parts*)
  (multiple-value-bind (super sub) (content-type part)
    (format t "~A/~A" super sub)))

;; The body of a part can get accessed using MESSAGE-BODY-STREAM

;; 3) How to copy/move/delete messages

;; a) Copy a message from one folder to another

(defvar *pop3-messages* (messages *pop3*))
(copy-message (first *pop3-messages*) *inbox*)

;; b) Delete a message from a folder
(defvar *copied-message* (find (message-id (first *pop3-messages*)) (messages *inbox*)))
(delete-message *copied-message*)

;; c) Move a message from one folder to another
(move-message (first *pop3-messages*) *inbox*)

;; d) Copy/Move/Clear whole folders
(copy-folder *pop3* *inbox*)
; (move-folder *pop3* *inbox*)
; (clear-folder *pop3)

;; 4) How to send messages via SMTP
;; Sending messages works by using a "SMTP-Relay" folder an the normal API
;; for moving/copying of messages. The question remains how one creates a
;; _new_ message not taken from any other folder:

(defvar *new-message* (make-message :subject "Test Message"
				    :from "sender@domain.test"
				    :to "receiver@somewhere.test"
				    :body "My body"))

;; Sending the message
(copy-message *new-message* *smtp*)