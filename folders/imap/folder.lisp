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

(in-package :mel.folders.imap)

;;;;;;;;;;;;;;;;;
;; IMAP folder ;;
;;;;;;;;;;;;;;;;;

;; Dependencies
;; MAKE-CONNECTION

(defvar *imap-debug* nil)

;;; We want to assign sequence numbers to messages of a folder.
;;; 

(defclass sequence-table-mixin ()
  ((seq-table :initform (make-array 5000 :adjustable t :fill-pointer 1 :initial-element nil))))

(defmethod (setf sequence-number) (n (message message))
  "Set the sequence number of a message"
  (with-slots (seq-table) (folder message)
    (unless (> (array-dimension seq-table 0) n)
      (adjust-array seq-table (* 2 n) :fill-pointer n))
    (unless (> (length seq-table) n)
      (setf (fill-pointer seq-table) (1+ n)))
    (setf (aref seq-table n) message)))

(defmethod sequence-number ((message message))
  "Return the sequence number of a message.
   This is an O(N) operation!"
  (with-slots (seq-table) (folder message)
    (position message seq-table :test #'eq)))

(defmethod sequence-number-message ((folder sequence-table-mixin) n)
  (with-slots (seq-table) folder
    (let ((message (aref seq-table n)))
      (unless message (error "Sequence number ~d does not reference a message" n))
      message)))

(defmethod expunge-sequence-number ((folder sequence-table-mixin) n)
  (with-slots (seq-table) folder
    (let ((exists (length seq-table)))
      (rotatef (aref seq-table n)
	       (aref seq-table exists))
      (loop for i from n below exists
	    do (rotatef (aref seq-table i)
			(aref seq-table (1+ i))))
      (setf (aref seq-table exists) nil)
      (decf (fill-pointer seq-table)))))

(defmethod close-folder :after((folder sequence-table-mixin))
  (with-slots (seq-table) folder
    (fill seq-table nil)
    (setf (fill-pointer seq-table) 1)))

(defclass network-folder-mixin ()
  ((connection :accessor connection)
   (host :accessor host :initarg :host)
   (port :accessor imap-port :initarg :port)
   (username :accessor username :initarg :username)
   (password :accessor password :initarg :password)))

(defclass imap-folder (sequence-table-mixin 
		       network-folder-mixin 
		       eql-message-cache-mixin
		       basic-receiver
		       basic-sender)
  ((mailbox :accessor mailbox :initarg :mailbox :initform "INBOX")
   (capabilities :reader capabilities :initform nil)
   (uidvalidity :reader uidvalidity)
   (uid-table :accessor uid-table :initform (make-hash-table :test 'eql))
   (size-table :accessor size-table :initform (make-hash-table :test 'eql))
   (exists :initform 0)
   (recent :initform 0)
   (unseen :initform nil)
   (messages :initform nil)
   (last-command :initform nil :accessor last-command)
   (state :accessor state :initarg :state :initform :disconnected)))
                  
(defun make-imap-folder (&key (host "imap.web.de")(port 143) username password (mailbox "INBOX"))
  (make-instance 'imap-folder 
                 :name (format nil "imap://~A!~A@~A:~A" username password host port)
                 :host host
                 :port port
                 :username username
                 :password password
		 :mailbox mailbox
                 :state :disconnected))

(defmethod serialize-folder ((folder imap-folder) stream)
  (with-standard-io-syntax
    (write `(make-imap-folder :host ,(host folder)
	     :port ,(imap-port folder)
	     :username ,(username folder)
	     :password ,(password folder))
	   :stream stream)))

;;;;;;;;;;;;;;;;;;;
;; IMAP Protocol ;;
;                ;;
; LOGIN          ;;
; SELECT         ;;
; SEARCH         ;;
; FETCH          ;;
; CLOSE          ;;
;;;;;;;;;;;;;;;;;;;

;; Atom
; atom-specials: "(){" SPACE CTL list_wildcards quoted_specials
; quoted-specials: '"\'
; list-wildcards: "%*"
; 1*(non-special)

(defvar *atom-specials* '(#\( #\) #\{ #\space #\% #\* #\" #\\))

(declaim (inline atom-char-p))		
(defun atom-char-p (char)
  (declare (optimize (speed 3) (safety 1)))
  (and (characterp char)
       (not (or (let ((code (char-code char)))
		  (or (<= #x00 code #x1f)
		      (= code #x7f)))
		(member char *atom-specials*)))))

(declaim (inline atom-char-p))
(defun read-atom (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((string (with-output-to-string (s)
		  (loop for c = (peek-char nil stream nil nil)
			while (atom-char-p c)
			do (read-char stream)(write-char c s)))))
    string))

;; Number unsigned 32 bit
; 1*(digit)
(declaim (inline read-number))
(defun read-number (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((string (with-output-to-string (s)
		  (loop for c = (peek-char nil stream nil nil)
			while (and c (digit-char-p c))
			do (read-char stream)(write-char c s)))))
    (declare (optimize (speed 3) (safety 1))
	     (dynamic-extent string))
    (parse-integer string)))

;; String
; quoted
; '"' *(7-bit ASCII without CR LF) '"'

(declaim (inline read-quoted-string))
(defun read-quoted-string (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((char (read-char stream)))
    (with-output-to-string (out)
      (ecase char
	(#\" (loop for c = (read-char stream)
		   while (case c
			   (#\\ (write-char (read-char stream) out) t)
			   (#\" nil)
			   (otherwise (write-char c out) t))))))))

(define-condition imap-paren-closed () ())
(define-condition imap-bracket-closed () ())
(define-condition end-of-imap-response () ())

; * OK [PERMANENTFLAGS (\* \Draft \Answered \Flagged \Deleted \Seen)] Limited

(declaim (inline accept-char))
(defun accept-char (c stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((char (peek-char nil stream nil nil)))
    (if (and char (char= c char))
	(read-char stream)
	nil)))

;; Fix keywords
;; OK NO BAD
;;

(defvar *imap-read-case* :keyword)

(defun convert-atom (string)
  (ecase *imap-read-case*
    (:keyword (intern (string-upcase string) :keyword))
    (:upcase (string-upcase string))
    (:downcase (string-downcase string) :keyword)
    (:preserve string)))

(defun imap-read (stream &optional (eof-errorp t) eof-value)
  (loop for c = (peek-char nil stream eof-errorp eof-value)
	while (case c
		((#\space #\tab) (read-char stream) t)
		(otherwise nil)))
  (let ((char (peek-char nil stream eof-errorp eof-value)))
    (cond #+faithful-input((accept-char #\linefeed stream)
			   (if eof-errorp
			       (error 'end-of-imap-response)
			       (return-from imap-read eof-value)))
          #-faithful-input((char= #\return char) (read-char stream) 
			   (accept-char #\linefeed stream)
			   (if eof-errorp
			       (error 'end-of-imap-response)
			       (return-from imap-read eof-value)))
          ((char= #\" char) (read-quoted-string stream))

	  ((char= #\( char) (read-char stream) (imap-read-delimited-list stream)) 
	  ((char= #\[ char) (read-char stream) (imap-read-bracket-list stream)) 
	  ((char= #\) char) (read-char stream)  (signal 'imap-paren-closed))
	  ((char= #\] char) (read-char stream)  (signal 'imap-bracket-closed))

	  ;; Read chunk
	  ((char= #\{ char) (read-char stream)
	   (let ((number (read-number stream)))
	     (unless (accept-char #\} stream) (error "Syntax error"))
	     #-faithful-input(unless (accept-char #\return stream) (error "Syntax error"))
	     (unless (accept-char #\linefeed stream) (error "Syntax error"))
	     #-faithful-input
	     (let ((buffer (make-array number :element-type '(unsigned-byte 8))))
	       (read-sequence buffer stream)
	       buffer)
	     #+faithful-input
	     (let ((buffer (make-array number :element-type '(unsigned-byte 8))))
	       (setf (stream-element-type stream) '(unsigned-byte 8))
	       (read-sequence buffer stream)
	       (setf (stream-element-type stream) 'character)
	       (map 'string (lambda (b) (code-char b)) buffer))))
	  ((char= #\\ char) (read-char stream)
	   (let ((char (peek-char nil stream)))
	     (if (eql #\* char)
		 :*
		 (let ((atom (read-atom stream)))
		   (cond ((zerop (length atom)) (error "Zero length \\atom"))
			 ((string-equal atom "NIL") nil)
			 ((every #'digit-char-p atom) (parse-integer atom))
			 (t (convert-atom atom)))))))
	  ((atom-char-p char)
	   (let ((atom (read-atom stream)))
	     (cond ((zerop (length atom)) (error "Zero length atom"))
		   ((string-equal atom "NIL") nil)
		   ((every #'digit-char-p atom) (parse-integer atom))
		   (t (convert-atom atom)))))
	  (t (read-char stream)))))

(declaim (inline imap-read-delimited-list))
(defun imap-read-delimited-list (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let (forms)
    (handler-case
	(loop for form = (imap-read stream)
	      do (push form forms))
      (imap-paren-closed ()
	(nreverse forms)))))

(declaim (inline imap-read-bracket-list))
(defun imap-read-bracket-list (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let (forms)
    (handler-case
	(loop for form = (imap-read stream)
	      do (push form forms))
      (imap-bracket-closed ()
	(nreverse forms)))))

;; Response

(declaim (inline read-response))
(defun read-response (stream)
  (let (result)
    (handler-case
        (let ((tag (imap-read stream)))
          (push tag result)
          (let ((*imap-read-case* (if (eq :+ tag) :preserve :keyword)))
            (loop for form = (imap-read stream)
                  do (push form result))))
      (end-of-imap-response () (let ((result (nreverse result)))
				 (if *imap-debug*
				     (print result)
				     result))))))

(defun skip-responses (stream)
  (loop for response = (read-response stream)
	do  (print response *standard-output*)
	  (case (second response)
	    (:ok (return nil))
	    ((:no :bad (error "Error on response"))))))

(defmethod process-response ((folder imap-folder) &key
			     on-recent
			     on-unseen
			     on-exists
			     on-expunge
			     on-size
			     on-uid
			     on-body
			     on-list
			     on-header
			     on-bodystructure
			     on-continuation)
  (let ((stream (ensure-connection folder)))
    (let (attributes part)
      (labels ((process-untagged-number (type arguments)
		 (let ((number type)
		       (atom (first arguments)))
		   (when *imap-debug*
		     (assert (numberp number) (number) "Is not a number")
		     (assert (keywordp atom) (atom) "Is not an atom"))
		   (cond ((eq :exists atom)
			  (setf (slot-value folder 'exists) number)
			  (when on-exists
			    (funcall on-exists number)))
			 ((eq :recent atom)
			  (setf (slot-value folder 'recent) number)
			  (when on-recent
			    (funcall on-recent number)))
			 ((eq :unseen atom)
			  (setf (slot-value folder 'unseen) number)
			  (when on-unseen
			    (funcall on-unseen number)))
			 ((eq :expunge atom)
			  (expunge-sequence-number folder number)
			  (when on-expunge
			    (funcall on-expunge number)))
			 ((eq :fetch atom)
			  (let ((form (second arguments)))
			    (when *imap-debug* (assert (and form (consp form)) (form) "Is not a form"))
			    (process-fetch number form))))))
	       (process-fetch (n form)
		 ;; Message
		 (when-let (message (getf form :BODYSTRUCTURE))
		   (when on-bodystructure
		     (funcall on-bodystructure message))
		   (setf part message))
		 (when-let (message (getf form :BODY[HEADER]))
		   (with-input-from-sequence (s message)
		     (setf (weird-mail.mime:header-fields (sequence-number-message folder n))
			   (mel.mime:read-rfc2822-header s)))
		   (when on-header
		     (funcall on-header message))
		   (setf part message))
		 (when-let (message (getf form :|BODY[TEXT]|))
		   (when on-body
		     (funcall on-body message))
		   (setf part message))
		 (when-let (message (getf form  :|BODY[]|))
		   (when on-body
		     (funcall on-body message))
		   (setf part message))
	       
		 ;; Attributes
		 (let (message)
		   (when-let (uid (getf form :uid))
		     ;; We got a FETCH response with
		     ;; an UID. We use this for creation
		     ;; of the message objects
		   
		     ;; Create or find cached message object associated
		     ;; with given UID
		     (setf message (find-message folder uid :if-does-not-exist :create))
		     ;; Register sequence number with message
		     (setf (sequence-number message) n)
		     (when on-uid
		       ;; Call hook
		       (funcall on-uid message)))
		   (let ((flags (getf form :flags :n/a)))
		     (unless (eq flags :n/a)
		       (if message
			   (setf (flags message)
				 flags)
			   (setf (flags (sequence-number-message folder n))
				 flags))))
		   
		   (when-let (size (getf form :RFC822.SIZE))
		     (when on-size
		       (let ((message (or message (sequence-number-message folder n))))
			 (setf (gethash (uid message) (size-table folder))
			       size))
		       (funcall on-size message)))
		   (push form attributes))))

	;; Process next IMAP response
	(loop (destructuring-bind (tag type &rest arguments)
		  (read-response stream)
		(force-output t)
		(cond ((eq tag :+)
		       (when on-continuation
			 (funcall on-continuation type arguments)))
		       ((keywordp tag)
			(case type
			  (:ok (return (values part attributes)))
			  ((:no :bad) (error "Error on response: ~A (cmd: ~A)" 
					     arguments (if *imap-debug* (last-command folder) 
							   "N/A")))))
		      ((eql tag #\*)
		       (cond ((numberp type)
			      (process-untagged-number type arguments))
			     ((eq :list type)
			      (when on-list
				(funcall on-list 
					 (first arguments)
					 (rest arguments))))
			     ((eql type :ok)
			      (case (first arguments)
				(:uidvalidity 
				 (if (uidvalidity folder)
				     (unless (= (first (second arguments))
						(uidvalidity folder))
				       (setf (slot-value folder 'uidvalidity)
					     (first (second arguments)))
				       (clrhash (size-table folder))
				       (clrhash (uid-table folder)))
				     (setf (slot-value folder 'uidvalidity)
					   (first (second arguments)))))
				(:capabilities
				 (setf (slot-value folder 'capabilities)
				       (second arguments))))))))))))))


(defun authenticate-cram-md5 (sink-folder)
  (format (connection sink-folder) "CRAM AUTHENTICATE CRAM-MD5")
  (write-char #\return (connection sink-folder))
  (write-char #\linefeed (connection sink-folder))
  (force-output (connection sink-folder))
  (process-response 
   sink-folder 
   :on-continuation (lambda (arg1 arguments)
                      (declare (ignore arguments))
                      (let ((timestamp (map 'string #'code-char (mel.mime::decode-base64 arg1))))
                        (format t "(Challenge) ~A~%" timestamp)
                        (let ((username (mel.cipher:string-to-octets (format nil "~A " (username sink-folder))))
                              (digest (mel.cipher:hmac-md5 timestamp (password sink-folder))))
                          (let ((response (concatenate 'vector username digest)))
                            (format t "(Response) ~A~%" (map 'string #'code-char response))
                            (write-string (mel.mime::encode-base64 response)
                                          (connection sink-folder))
                            (write-char #\return (connection sink-folder))
                            (write-char #\linefeed (connection sink-folder))
                            (force-output (connection sink-folder))))))))
                          

(defun make-imap-connection (folder) 
  (let ((connection (mel.network:make-connection
		     :remote-host (host folder)
		     :remote-port (imap-port folder)
		     :element-type '(unsigned-byte 8))))
    (setf (connection folder) connection)
    (setf (state folder) :connected)
    (read-response connection)

    (format connection "c01 LOGIN ~A ~A~A~A" 
	    (username folder) (password folder) #\return #\linefeed)

;; Not really tested yet
;    (authenticate-cram-md5 folder)

    (force-output connection)
    (process-response folder)
    connection))

(defun send-command (folder string &rest args)
  (handler-case
      (let ((stream (ensure-connection folder)))
	(when *imap-debug*
	  (setf (last-command folder)
		(apply #'format nil
		       string args)))
	(apply #'format stream
	       string args)
	(write-char #\return stream)
	(write-char #\linefeed stream)
	(force-output stream))
    (end-of-file () (setf (state folder) :disconnected))))

(defmethod ensure-connection ((folder imap-folder))
  (when (eq (state folder) :disconnected)
    (setf (connection folder) (make-imap-connection folder))
    (setf (state folder) :connected)
    (select-mailbox folder))
  (handler-case
      (connection folder)
    (cl:end-of-file () (setf (state folder) :disconnected)
	       (ensure-connection folder))))

;; Required untagged responses: FLAGS EXISTS RECENT
;; Optional untagged responses: UNSEEN, PERMANENTFLAGS
(defun select-mailbox (folder)
  (unless (connection folder)
    (ensure-connection folder))
  (let ((stream (connection folder)))
    (format stream "~A select ~A~A~A" "t01" (mailbox folder)
	    #\return #\linefeed)
    (force-output stream)
    (let (recent exists)
      (process-response folder 
			:on-recent (lambda (n) (setf recent n))
			:on-exists (lambda (n) (setf exists n)))
      (values exists recent))))

(defun noop (folder)
  (send-command folder "~A noop" "t01")
  (process-response folder))

(defun create-mailbox (folder mailbox)
  (send-command folder "~A create ~A" "create" mailbox)
  (process-response folder))

(defun list-mailboxes (folder pattern)
  (send-command folder "~A list \"\" ~S" "create" pattern)
  (let (result)
    (process-response folder :on-list (lambda (flags args)
					(push (list* flags args) result)))
    result))

(defun close-mailbox (folder)
  (when (eq (state folder) :connected)
    (send-command folder "c1 close")
    (process-response folder)
    (setf (state folder) :disconnected)
    ;; Added by KTR on 07/17/2008 to close the network connection
    (close (connection folder))))

(defun examine-mailbox (folder)
  (send-command folder "~A select ~A" "t01" (mailbox folder))
  (process-response folder))

(defun expunge-mailbox (folder)
  (send-command folder "~A expunge" "t01")
  (process-response folder))

(defmethod update-mailbox ((folder imap-folder) callback)
  (let ((non-recent (hash-table-count (message-cache folder)))
	(exists (count-messages folder)))
    (cond ((/= exists non-recent)
	   (clrhash (size-table folder))
	   (ensure-connection folder)
	   
	   (send-command folder "~A fetch 1:~A (UID FLAGS)" ; (UID RFC822.SIZE FLAGS)"
			 "UPDATE" (slot-value folder 'exists))
	   (let (messages)
	     (process-response folder
			       :on-uid (lambda (message)
					 (push message messages)
					 (funcall callback message)))
	     (setf (slot-value folder 'messages) messages)))
	  (t (slot-value folder 'messages)))))
	  

(defun append-message (sink-folder message-string)
  (let ((message-size (length message-string)))
    (send-command sink-folder "~A APPEND ~A () {~D}"
		  "APPEND" (mailbox sink-folder) message-size)
  (process-response 
   sink-folder 
   :on-continuation (lambda (type arguments)
		      (declare (ignore type arguments))
		      (write-sequence message-string (connection sink-folder))
		      (write-char #\return (connection sink-folder))
		      (write-char #\linefeed (connection sink-folder))
		      (force-output (connection sink-folder))))))

(defmethod fetch-message ((folder imap-folder) uid)
  (send-command folder "~A uid fetch ~A (UID BODY[])"
	    "MESSAGE" uid)
  (let (result)
    (process-response folder :on-body (lambda (body) (setf result body)))
    result))

(defmethod fetch-message-body ((folder imap-folder) uid)
  (send-command folder "~A uid fetch ~A (UID BODY[TEXT])"
	    "BODY" uid)
  (let (result)
    (process-response folder :on-body (lambda (body) (setf result body)))
    result))

(defmethod fetch-message-header ((folder imap-folder) uid)
  (send-command folder "~A uid fetch ~A (UID BODY[HEADER])"
	    "HEADER" uid #\return #\linefeed)
  (let (result)
    (process-response folder :on-header (lambda (header) (setf result header)))
    result))

(defmethod fetch-all-message-headers ((folder imap-folder))
  (send-command folder "~A fetch 1:* (UID BODY[HEADER])"
	    "HEADER" #\return #\linefeed)
    (process-response folder :on-header (lambda (header) (declare (ignore header)))))


(defmethod fetch-message-bodystructure ((folder imap-folder) uid)
  (send-command folder "~A uid fetch ~A (UID BODYSTRUCTURE)"
	  "BODYSTRUCTURE" uid)
  (let (result)
    (process-response folder :on-bodystructure (lambda (b) (setf result b)))
    result))

(defun canonicalize-bodystructure (bodystructure)
  (labels ((keyword (string)
	     (typecase string
	       (symbol string)
	       (string (intern (string-upcase string) #.(find-package :keyword)))))
	   (canonicalize-parameters (params)
	     (loop for (name value) on params by #'cddr
		   nconc (list (keyword name)
			       value)))
	   (canonicalize-simple-part (bs)
	     `(,(keyword (first bs))
	       ,(keyword (second bs))
	       ,(canonicalize-parameters (third bs))
	       nil nil
	       ,(keyword (sixth bs))
	       ,(seventh bs)
	       ,(eighth bs)
	       nil nil nil))
	   (canonicalize-multipart (bs)
	     (loop for tail on bs
		   while (consp (first tail))
		   collect (canonicalize-bodystructure (first tail)) into parts
		   finally (return `(,@parts 
				     ,(keyword (first tail))
				     ,(canonicalize-parameters (second tail))
				     nil nil)))))
    (etypecase (car bodystructure)
      (cons (canonicalize-multipart  bodystructure))
      (string (canonicalize-simple-part bodystructure))
      (symbol (canonicalize-simple-part bodystructure)))))

(defmethod mel.mime::compute-bodystructure-using-folder 
    ((folder imap-folder) (message mel.mime::bodystructure-mixin))
  (let ((bodystructure (fetch-message-bodystructure folder (uid message))))
    (canonicalize-bodystructure bodystructure)))

(defmethod search-mailbox ((folder imap-folder) query)
  (send-command folder "~A uid search ~A" "t01" query)
  (process-response folder))

(defmethod mark-deleted ((folder imap-folder) uid)
  (send-command folder "~A uid store ~A +flags (\\Deleted)" "t01"
	  uid)
  (process-response folder))

;;;;;;;;;;;;;;;;;;;;;
;; Folder Protocol ;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod close-folder ((folder imap-folder))
  (unless (eq (state folder) :disconnected)
    (ignore-errors (close-mailbox folder))
    (setf (connection folder) nil
	  (state folder) :disconnected)
    (clrhash (uid-table folder))
    (clrhash (size-table folder))
    (setf (slot-value folder 'messages) nil
	  (slot-value folder 'exists) 0
	  (slot-value folder 'recent) 0)
    ))

(defmethod open-message-input-stream-using-folder 
    ((folder imap-folder) message start)
  (let ((stream (make-sequence-input-stream (fetch-message folder
							 (uid message)))))
    (file-position stream (+ (file-position stream) start))
    stream))

(defmethod message-body-stream-using-folder ((folder imap-folder) message)
  (make-sequence-input-stream (fetch-message-body folder
						(uid message))))

(defmethod message-header-stream-using-folder ((folder imap-folder) message)
  (let ((stream (make-sequence-input-stream (fetch-message-header folder
								(uid message)))))
    stream))

;;; Folder Protocol Support

(defmethod copy-message-using-folders :around ((message message) (message-folder imap-folder) (sink-folder imap-folder))
  "Copy a message between two imap folders. We can optimize this case if the folders are on the same server."
  (if (and (equal (host message-folder) (host sink-folder))
           (equal (username message-folder) (username sink-folder))
           (equal (password message-folder) (password sink-folder))
           (equal (imap-port message-folder) (imap-port sink-folder)))
      (progn
        (send-command message-folder "~A uid copy ~A ~A" "UID" (uid message) (mailbox sink-folder))
        (process-response message-folder :on-uid (lambda (m) m)))
      ;; if we're not using the same server, play it safe
      (call-next-method)))

(defmethod map-messages (fn (folder imap-folder))
  (update-mailbox folder fn))

(defmethod message-size-using-folder ((folder imap-folder) (message message))
  "A message's size is cached in the folder object. If there is a cache fault
   a FETCH for the RFC822.SIZE of the message is sent. Doing this for many
   messages at once may be slow because of the roundtrip delay. I deliberately
   left out RFC822.SIZE from the message-list FETCH (see UPDATE-MAILBOX) because
   it slowed it down to much."
  (multiple-value-bind (size existsp)
      (gethash (uid message) (size-table folder))
    (if existsp
	size
	(let ((seq (sequence-number message)))
	  (unless seq
	    (error "No sequence number for message ~A (UID: ~A)" message (uid message)))
	  (send-command folder "~A fetch ~A:~A (UID RFC822.SIZE)"
			"SIZE" seq seq)
	  (process-response folder :on-size (lambda (m) m))
	  (gethash (uid message) (size-table folder))))))

(defmethod copy-folder ((source-folder imap-folder) (sink-folder folder))
  "Overload for more efficient message transfer. Up to now messages are
   always fetched completely as a string buffer. This may lead to large
   memory consumption for big mails (And to problems with Lispsystems which
   have relatively small array-dimension-limits"

  (dolist (message (messages source-folder))
    (with-open-stream (sink (open-message-storing-stream sink-folder message))
      (let ((buffer (fetch-message source-folder (uid message))))
		(write-sequence buffer sink)))))

(defmethod copy-folder ((source-folder folder) (sink-folder imap-folder))
  (cerror "Cancel copying of folder ~A to the imap folder ~A"
	  "imap folders are not yet supported as message sinks"
	  source-folder sink-folder))

(defmethod delete-message-using-folder ((folder imap-folder) message)
  (mark-deleted folder (uid message))
  (expunge-mailbox folder))

(defmethod count-messages ((folder imap-folder))
  "Count messages by sending an EXAMINE command. EXAMINE
   triggers an EXISTS response which contains the number
   of available messages in the imap folder."
  (noop folder)
  (slot-value folder 'exists))

(defmethod short-name ((folder imap-folder))
  (format nil "imap://~A@~A" (username folder) (host folder)))

#+nil
(defmethod find-message ((folder imap-folder) uid)
  "Return cached message or create and cache a new one."
  (or (gethash uid (uid-table folder))
      (setf (gethash uid (uid-table folder)) (call-next-method))))

(defun collect-ranges (predicate folder &key (key #'identity))
  "A utility function that loops over all messages from the lowest
   sequence id to the highest. For each coherent subsequence of messages
   for which the predicate is true a pair containing the start and end
   position is collected."
  (with-slots (seq-table) folder
    (let ((start 1)
	  (count (length seq-table)) ranges)
      (loop while (and start (< start count))
	    do (setf start (position-if predicate seq-table :key key :start start))
	    (unless start
	      (return-from collect-ranges ranges))
	    (let ((end (position-if-not predicate seq-table :key key
				    :start start)))
	      (if end
		  (push (cons start (1- end)) ranges)
		  (push (cons start "*") ranges))
	      (setf start (if end (1+ end))))) ranges)))

(defmethod ensure-all-headers ((folder imap-folder) &key hook)
  "Read all yet unread headers. First we construct a list of
   sequence id range pairs for those messages that do not have
   any cached header-fields yet. After that we send a FETCH command
   for each range in that list."
  (let ((ranges (collect-ranges #'null folder :key #'mel.mime:header-fields)))
    (loop for (start . end) in ranges
	  do
	  (send-command folder "~A fetch ~a:~a (BODY[HEADER])"
			"HEADER" start end #\return #\linefeed)
	  (process-response
	   folder :on-header
	   (lambda (header)
	     (funcall hook header))))))

#+nil
(defmethod flagp-using-folder ((folder imap-folder) message (flag (eql :recent)))
  (multiple-value-bind (cell existsp)
      (gethash (uid message) (uid-cache folder))
    (unless existsp (error "Message does not exist in uid cache"))
    (ecase (car cell)
      (:new t)
      (:cur nil))))

(defmethod mark-message-using-folder ((folder imap-folder) message (flag (eql :recent)))
  (declare (ignore message))
  (cerror "Continue without setting the RECENT flag" "It is not possible to modify the RECENT flag manually"))

(defmethod unmark-message-using-folder ((folder imap-folder) message (flag (eql :recent)))
  (declare (ignore message))
  #+nil
  (cerror "Continue without unsetting the RECENT flag" "It is not possible to modify the RECENT flag manually"))

;; Sender Protocol

(defclass imap-message-storing-stream (encapsulating-output-stream)
  ((message :accessor message :initarg :message)
   (folder :accessor stream-folder :initarg :folder)))

(defmethod close ((stream imap-message-storing-stream) &key abort)
  (let ((message (get-output-stream-string 
		  (encapsulated-output-stream stream))))
    (format t "Close message storing stream~%")
    (format t "Storing message of size ~D~%" (length message))
    (force-output t)
    (unless abort
      (append-message (stream-folder stream) 
		      message))))

(defmethod open-message-storing-stream ((folder imap-folder) message)
  (format t "Open message storing stream~%")
  (force-output t)
  (let ((stream (make-instance 'imap-message-storing-stream
			       :output-stream (make-string-output-stream)
			       :message message
			       :folder folder)))
    stream))