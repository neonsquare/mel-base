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

(define-condition mime-parse-error (parse-error)
  ((context :accessor mime-parse-context :initarg :context :initform nil))
  (:report (lambda (condition stream)
	     (when (mime-parse-context condition)
		 (format stream "A MIME parse error occured while ~A" (mime-parse-context condition))))))

  
(defun mime-parse-error (context-fmt &rest params)
  (let ((context (apply #'format nil context-fmt params)))
    (error 'mime-parse-error :context context)))

;; :warn :error :quiet
(defvar *buggy-software-action* :quiet) 

(defun buggy (message)
  (case *buggy-software-action*
    (:quiet )
    (:warn (warn message))
    (:error (mime-parse-error message))))

(defgeneric FOLDER (message))
(defgeneric UID (message))


(defclass simple-flag-mixin ()
  ((flags :accessor flags :initarg :flags :initform (list :recent))))

(defclass message (simple-flag-mixin)
  ((folder :accessor folder :initarg :folder)
   (origin-folder :accessor origin-folder :initarg :origin-folder)
   (deleted-p :accessor deleted-p :initarg :deleted-p :initform nil)
   (uid :accessor uid :initarg :uid)
   (md5-sum :reader md5-sum :initform nil)
   (content-md5-sum :reader content-md5-sum :initform nil)
   (state :accessor state :initform :new)))

(defclass rfc2822-header-mixin ()
  ((header-fields :accessor header-fields :initform nil :initarg :header-fields)
   (message-id :accessor message-id :initform nil)
   (date :accessor date :initform nil)
   (subject :accessor subject :initform nil)
   (from :accessor from :initform nil)
   (reply-to :accessor reply-to :initform nil)
   (to :accessor to :initform nil)
   (sender :accessor sender :initform nil)
   (cc-list :accessor cc-list :initform nil)))

(defclass rfc2822-basic-body-mixin ()
  ((body-start :accessor body-start :initform nil :initarg :body-start)
   (content :accessor content :initform nil :initarg :content)))

(defclass rfc2822-basic-message (rfc2822-header-mixin rfc2822-basic-body-mixin message) ())

(declaim (inline transmit-field-body))
(defun transmit-field-body (in-stream out-stream)
  (declare (optimize (speed 3) (safety 0)))
  (let ((octets 0))
    (declare (fixnum octets))
  (flet ((accept (&rest acceptable-chars)
           #-(or sbcl cmu)(declare (dynamic-extent acceptable-chars))
           (let ((acceptable-chars (or acceptable-chars '(#\space #\tab)))
                 (c (peek-char nil in-stream)))
	     (declare (type character c))
             (when (member c acceptable-chars)
               (read-char in-stream)
	       (incf octets)))))
    #-(or sbcl cmu)(declare (dynamic-extent #'accept))
    (let (c)
      (tagbody

       start (setf c (read-char in-stream nil nil))
             (case c
               ((nil) (go exit))
               (#\newline (incf octets) (go nl))
               (#\return (incf octets) (go crlf))
               (otherwise (incf octets) (write-char c out-stream) (go start)))

       crlf (cond ((accept #\linefeed) (incf octets)(go nl) )
		  ((accept #\return)
		   ;; Courier IMAP Server wants Unix-Style lineending in
		   ;; Messages. Give it CRLF and it will spew CRCRLF
		   ;; onto your feet. We workaround it by swallowing the
		   ;; ambigous CR
		   (buggy "Buggy Courier IMAP Server")
		   (incf octets)(go crlf))
                  (t #+nil (cerror "Ignore and read further" "Linefeed expected")
                     (buggy "Linefeed expected")
                     (go nl)))
     

       nl (cond ((accept #\space #\tab) (incf octets) (write-char #\newline out-stream) 
                 (write-char #\space out-stream) (go start))
                (t (go exit)))

       exit)))
  octets))

(declaim (inline accept-newline))
(defun accept-newline (stream)
  (declare (optimize (speed 3) (safety 0)))
  (let ((c (peek-char nil stream nil nil)))
    (when c
      (case c
        (#\newline (read-char stream) 1)
        (#\return (read-char stream) 
                  (cond ((eql (peek-char nil stream) #\linefeed)
                         (read-char stream) 2)
			;; Buggy Courier Lineending
			((eql (peek-char nil stream) #\return)
			 (read-char stream)
			 (if (eql (peek-char nil stream) #\linefeed)
			     3
			     2))
                        (t (mime-parse-error "accepting newline: LF expected after CR"))))
        (otherwise nil)))))

(declaim (inline accept-crlf))
(defun accept-crlf (stream)
  (declare (optimize (speed 3) (safety 0))
	   (notinline accept-crlf))
  (let ((c (peek-char nil stream)))
    (when (eql c #\return)
        (read-char stream)
        (cond ((eql (peek-char nil stream) #\linefeed) (read-char stream)  t)
              (t (unread-char #\return stream) nil)))))

(declaim (inline accept-char))
(defun accept-char (char stream)
  (declare (optimize (speed 3) (safety 0)))
  (let ((next-char (peek-char nil stream)))
    (when (and char (char= next-char char))
      (read-char stream) t)))

(declaim (inline intern-header-name))
(defun intern-header-name (string)
  (declare (type string string)
           (optimize (speed 3) (safety 0) (debug 0)))
  (intern (ecase (readtable-case *readtable*)
            (:upcase (string-upcase string))
            (:downcase (string-downcase string))
            (:preserve (string-downcase string))
            (:invert (string-upcase string)))
          (find-package :keyword)))

(defun skip-rfc2822-header (stream &optional (file-position 0))
  "Skip mail headers and return position in message"
  (flet ((skip-to-cr/lf ()
	   (loop for c = (prog1 (read-char stream) (incf file-position))
		 until (case c
			 ((#\return #\linefeed) t)
			 (otherwise nil))
		 finally (return (case c
				   (#\return (cond ((accept-char #\linefeed stream)
						    (prog1 :rfc (incf file-position)))
						   ((accept-char #\return stream)
						    (read-char stream)
						    :courier)
						   (t :mac)))
				   (#\linefeed :unix))))))
			  
    (loop with line-ending-style = :rfc
	  for style = (skip-to-cr/lf)
	  do 
	  (unless (eq line-ending-style style)
	    (buggy "Switched lineending-style")
	    (setf line-ending-style style))
	  (case line-ending-style
	    (:mac (when (accept-char #\return stream) (return (1+ file-position))))
	    (:unix (when (accept-char #\linefeed stream) (return (1+ file-position))))
	    (:rfc (when (accept-crlf stream) (return (+ 2 file-position))))
	    (:courier (when (and (accept-char #\return stream)(accept-crlf stream))
			(return (+ 3 file-position))))
	    ))))

(defun read-rfc2822-header (stream)
  (let ((octets 0) fields
        (field-name-buffer (make-array 2048 :element-type 'character :fill-pointer 0)))
    (declare (type string field-name-buffer)
             #-sbcl(dynamic-extent field-name-buffer))
    (handler-case
        (flet ((field-name-char-p (c)
                 (or (char<= #\! c #\9) (char<= #\; c #\~))))
          (with-output-to-string (field-name field-name-buffer)
            (with-output-to-string (field-body)
              (loop
               (loop for c = (prog1 (read-char stream) (incf octets))
		     while (field-name-char-p c) do (write-char c field-name))
               (incf octets (transmit-field-body stream field-body))
               (push (cons (intern-header-name field-name-buffer)
                           (get-output-stream-string field-body)) fields)
               (setf (fill-pointer field-name-buffer) 0)
               (when-let (n (accept-newline stream)) (incf octets) (return)))))
          (values (nreverse fields) octets))
    (end-of-file () (values (nreverse fields) octets)))))

(defun parse-rfc2822-header (string)
  (with-input-from-sequence (s string)
    (read-rfc2822-header s)))

(defun write-rfc2822-header (alist &optional (stream *standard-output*))
  (loop for (name . body) in alist
	do (format stream "~A:~A~A~A"
		   (string-capitalize name)
		   body #\return #\linefeed))
  (write-char #\return stream)
  (write-char #\linefeed stream))
	
(defmethod field ((name symbol) (message rfc2822-header-mixin))
  (field name (header-fields message)))

(defmethod field ((name symbol) (fields list))
  (let ((result (loop for (field-name . field-body) in fields
                      if (eq name field-name) collect field-body)))
    (if (null (rest result)) (first result) result)))

(defmethod (setf field) (value (name symbol) (message rfc2822-header-mixin) &optional (unique t))
  (let* ((fields (header-fields message))
	 (result (loop for cons in fields
		       if (eq name (car cons)) collect cons)))
    (when (and unique (rest result))
      (setf fields (remove name fields :test #'eq :key #'car)))
    (if result
	(setf (cdr (first result)) value)
      (push (cons name value) fields))
    (setf (header-fields message) fields)))
      
(defmacro define-field-reader (accessor-name field-name (message) &body forms)
  `(defmethod ,accessor-name :before ((,message rfc2822-header-mixin))
      (unless (slot-value ,message ',accessor-name)
	(let ((,accessor-name (field ,field-name ,message)))
	  (if (consp ,accessor-name) (setf ,accessor-name (first ,accessor-name)))
	  (when ,accessor-name (setf (slot-value ,message ',accessor-name)
				     (progn ,@forms)))))))

(defmacro define-field-writer (accessor-name field-name (message) &body forms)
  `(defmethod (setf ,accessor-name) :after (,accessor-name (,message rfc2822-header-mixin))
    (let ((string-values (progn ,@forms))
	  (header-fields (remove ,field-name (header-fields ,message) :key #'car)))
      (if (consp string-values)
	  (loop for string-value in string-values
		do (push (cons ,field-name (concatenate 'string " " string-value)) header-fields))
	  (push (cons ,field-name (concatenate 'string " " string-values)) header-fields))
      (setf (header-fields ,message) header-fields))))

(defun clean-header-field (string)
  (string-trim '(#\space #\tab #\return #\linefeed) string))

(define-field-reader message-id :message-id (message)
  (clean-header-field message-id))

(define-field-writer message-id :message-id (message)
   message-id)

(define-field-reader date :date (message)
  (date-to-universal-time (clean-header-field date)))

(define-field-writer date :date (message)
  (universal-time-to-date date))

(define-field-reader subject :subject (message)
  (clean-header-field subject))

(define-field-writer subject :subject (message)
  subject)

(defconstant +vt+ (code-char 11))

;;; rfc2822 scanner
(defun parse-rfc2822 
       (string &key (start 0) (end (length string)) (ignore-comments t) (ignore-space t) (split-atoms nil))
  " ==> token, type, new-index"
  (unless (> end (or start end)) (return-from parse-rfc2822 (values nil :epsilon nil)))
  (macrolet ((get-char ()
               '(if (> end (incf index))(char string index) :epsilon))
             (backtrack ()
               '(if (> end (1+ index)) (go :backtrack) (return-from parse-rfc2822 (values nil :epsilon nil)))))
    (flet ((collect-to (char index token-type)
             (with-output-to-string (s)
               (loop for c = (get-char)
                     until (eql c char)
                     do (case c
                          (#\\ (let ((c (get-char)))(when (characterp c)(write-char c s))))
                          (:epsilon (return (values (get-output-stream-string s) token-type index))) ; return invalid token
                          (otherwise (write-char c s)))
                     finally (return-from parse-rfc2822 
                               (values (get-output-stream-string s)
                                       token-type (1+ index)))))))
      (tagbody :backtrack
               (let ((c (char string start))
                     (index start))
                 (let ((code (char-code c)))
                   (when (and (or (<= 0 code 31)(= code 127))(not (= code 9))(not (= code 11)))
                     (return-from parse-rfc2822 (values c :control (1+ index) start))))
                 (case c
                   ((#\space #\tab +vt+) (loop (let ((c (get-char)))
                                                 (case c
                                                   ((#\space #\tab +vt+) nil)
                                                   (:epsilon (return (if ignore-space (values nil :epsilon index start)
                                                                       (values " " :space index start))))
                                                   (otherwise (when ignore-space (setf start index)(backtrack))
                                                              (return-from parse-rfc2822 (values " " :space index start)))))))
                   ((#\< #\> #\@ #\; #\: #\. #\,) (return-from parse-rfc2822 (values c :special (1+ index) start)))
                   (ctls (return-from parse-rfc2822 (values c :control (1+ index))))
                   (#\" (collect-to #\" index :quoted-string))
                   (#\[ (collect-to #\] index :domain-literal))
                   (#\( (if ignore-comments
                          (let ((open-parens 1))
                            (loop (let ((c (get-char)))
                                    (case c
                                      (#\\ (get-char))
                                      (:epsilon (return (values nil :epsilon index start)))
                                      (#\( (incf open-parens)) 
                                      (#\) (decf open-parens)
                                           (when (zerop open-parens)
                                             (setf start (1+ index)) ; skip over comment
                                             (backtrack)))
                                      (otherwise nil)))))
                          (error "not implemented")))
                   (otherwise
                    (if split-atoms
                      (case c
                        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
                         (return-from parse-rfc2822 (values c :digit (1+ index) start)))
                        (otherwise (return-from parse-rfc2822 (values c :char (1+ index) start))))
                      (with-output-to-string (s)
                        (write-char c s)
			(let (dot-atomp)
			  (loop 
                           (multiple-value-bind (c type) 
			       (if (> end (incf index))
				   (parse-rfc2822 string :split-atoms t :ignore-space nil 
						  :start index)
				   (values nil nil))
                             (if (case type 
				   ((:char :digit) t)
				   (:special (if (eq c #\.) (setf dot-atomp t) nil))
				   (otherwise nil))
				 (write-char c s)
				 (return-from parse-rfc2822 (values (get-output-stream-string s) 
								    (if dot-atomp :dot-atom :atom)
								    index start)))))))))))))))

(defmethod token-type-test-function ((operation (eql 'or)) &rest args)
  (lambda (type)
    (find type args)))

(defun accept-rfc2822-token 
    (string expected-type 
     &key (start 0) (end (length string))
     (expected-value nil) (test #'equal) (type-test (lambda (type) (eq type expected-type))))
  (multiple-value-bind (token type new-index) 
      (parse-rfc2822 string :start start :end end)
    (if (and (funcall type-test type)
             (or (not expected-value) (funcall test token expected-value)))
      (values token new-index)
      (values nil))))

(defun collect-all (string type 
		    &key (start 0) (end (length string))
		    (value nil valuep) (value-test #'equal))
  (declare (ignore value valuep value-test))
  (let ((type-test (etypecase type
		     (symbol (lambda (token-type) 
			       (eq type token-type)))
		     (cons (lambda (token-type)
			     (find token-type type))))))
    (declare (type function type-test))
  (let (tokens)
    (loop (multiple-value-bind (token token-type new-index)
	      (parse-rfc2822 string
			     :start start
			     :end end)
	    (unless (funcall type-test token-type)
	      (return-from collect-all (values (nreverse tokens)
					       new-index)))
	    (push token tokens)
	    (setf start new-index))))))

(defun next-token-of-type (string type 
			   &key (start 0) (end (length string))
			   (value nil valuep) (value-test #'equal))
  (let ((type-test (etypecase type
		     (symbol (lambda (token-type) 
			       (eq type token-type)))
		     (cons (lambda (token-type)
			     (find token-type type))))))
    (declare (type function type-test))
    (loop (multiple-value-bind (token token-type new-index token-start)
	      (parse-rfc2822 string
			     :ignore-space nil
			     :start start
			     :end end)
	    (cond ((eq token-type :epsilon)
		   (return-from next-token-of-type nil))
		  ((and (funcall type-test token-type)
			(or (not valuep) (funcall value-test token value)))
		   (return-from next-token-of-type (values token token-start new-index))))
	    (setf start new-index)))))

(defmacro bind-rfc2822-tokens ((string (&key (start 0) end) &rest tokens) &body forms)
  (let ((string-sym (gensym "STRING"))
	(index-sym (gensym "INDEX"))
	(end-sym (gensym "END")))
  `(block bind-rfc2822-tokens
     (let* ((,string-sym ,string)
	    (,index-sym ,start)
	    (,end-sym (or ,end (length ,string-sym))))
       (let* (,@(mapcar (lambda (token)
                          (list (first token)
                                `(multiple-value-bind (result new-index)
                                     (accept-rfc2822-token ,string-sym ,(second token) 
                                                           :start ,index-sym
				                           :end ,end-sym
                                                           :expected-value ,(third token))
                                   (unless result (return-from bind-rfc2822-tokens nil))
                                   (setf ,index-sym new-index)
                                   result)))
                        tokens))
         ,@forms)))))

(defun collect-rfc2822-tokens (string)
  (let ((index 0) tokens)
    (loop (multiple-value-bind (token type new-index) (parse-rfc2822 string :start index)
            (if (eq type :epsilon)
              (return (nreverse tokens))
              (progn (setf index new-index) (push token tokens)))))))


;; RFC2822 Address

;; Make address-specs EQ
(defvar *address-spec-table* (make-hash-table :test 'equal))

(defun intern-address-spec (address-spec)
  (let ((upcased-address-spec (string-upcase address-spec)))
    (multiple-value-bind (interned-address-spec existsp)
	(gethash upcased-address-spec *address-spec-table*)
      (if existsp
	  interned-address-spec
	  (setf (gethash upcased-address-spec *address-spec-table*)
		address-spec)))))

(defclass address ()
  ((display-name :accessor display-name :initarg :display-name)
   (address-spec :accessor address-spec :initform nil :initarg :address-spec)))

;; Used when parsing the address fails
;; to return the original string in the display-name
;; slot
(defclass invalid-address (address)
  ())

(defclass group-address (address)
  ((mailbox-list :accessor mailbox-list :initarg :mailbox-list :initform nil)
   (display-name :initform "Anonymous")))

(defclass mailbox-address (address)
  ((display-name :accessor display-name :initarg :display-name)
   (address-spec :accessor address-spec :initarg :address-spec)))

(declaim (inline address-eq))
(defun address-eq (address1 address2)
  (declare (notinline address-eq))
  (eq (slot-value address1 'address-spec)
      (slot-value address2 'address-spec)))

;; display-name   ::= 1* (ATOM | QUOTED-STRING)
;; addr-spec      ::= (ATOM | DOT-ATOM | QUOTED-STRING) "@" domain
;; angle-addr     ::= "<" addr-spec ">"
;; mailbox-addess ::= (0*(display-name) angle-addr)
;;                    | addr-spec

(defun parse-addr-spec (string &key (start 0) (end (length string)) (errorp t))
  (multiple-value-bind (local-part type next-index)
      (parse-rfc2822 string :start start :end end)
    (unless (case type
	      ((:atom :dot-atom :quoted-string) t)
	      (otherwise nil))
      (if errorp
	  (mime-parse-error "parsing of addr-spec ~S (start ~A, end ~A) failed. (local-part of wrong type ~S)" string start end type)
	  (return-from parse-addr-spec nil)))
    (multiple-value-bind (at type next-index)
	(parse-rfc2822 string :start next-index :end end)
      (declare (ignore type))
	(unless (eql at #\@)
	  (return-from parse-addr-spec (values local-part nil next-index)))
	(multiple-value-bind (domain type next-index)
	    (parse-rfc2822 string :start next-index :end end)
	  (unless domain
	    (if errorp
		(mime-parse-error "parsing of addr-spec ~S (start ~A, end ~A failed. (domain n/a ~S)" string start end type)
		(return-from parse-addr-spec nil)))
	  (values local-part
		  domain
		  next-index)))))
    
(defun parse-mailbox-address (string &key (start 0) (end (length string)) (errorp t))
  (let ((at-start (nth-value 1 (next-token-of-type string :special :start start :end end :value #\@ :value-test #'eql))))
    (let ((angle-start (if at-start 
			   (nth-value 1 (next-token-of-type string :special :start start :end at-start :value #\< :value-test #'eql))
			   (nth-value 1 (next-token-of-type string :special :start start :end end :value #\< :value-test #'eql)))))

      (cond (angle-start
	     ;; Address is of form [display-name] "<" addr-spec ">"
	     (multiple-value-bind (local-part domain next-index)
		 (parse-addr-spec string :start (1+ angle-start) :errorp errorp :end end)
	       (if (stringp local-part)
		   (values
		    (make-instance 'mailbox-address
				   :address-spec (intern-address-spec (apply #'concatenate 
									     'string 
									     local-part (if domain
											    `("@" ,domain)
											    nil)))
				   :display-name (if (zerop angle-start)
						     nil
						     (subseq string start angle-start)))
		    (let ((angle-end (position #\> string :start next-index)))
		      (if angle-end
			  (1+ angle-end)
			  nil)))
		   (if errorp
		       (mime-parse-error "parsing mailbox-address: no local-part")
		       nil))))
	    (t (multiple-value-bind (local-part domain next-index)
		   (parse-addr-spec string :start start :end end :errorp errorp)
		 (if (stringp local-part)
		     (values
		      (make-instance 'mailbox-address 
				     :address-spec (intern-address-spec (apply #'concatenate 'string local-part 
										     (if domain
											 `("@" ,domain)
											 nil))))
		      next-index)
		     (if errorp
			 (mime-parse-error "parsing mailbox-address: no local-part")
			 nil))))))))

(defun white-space-p (c)
  (case c
    ((#\space #\linefeed #\return #\tab) t)
    (otherwise nil)))

(defun parse-mailbox-list (string  &key (start 0) (end (length string)))
  (let (mailboxes)
    (nreverse
    (loop (multiple-value-bind (mailbox next-index)
	      (parse-mailbox-address 
	       string 
	       :start start :end end :errorp nil)
	    (unless mailbox
	      (return mailboxes))
	    (push mailbox mailboxes)
	    (setf start (nth-value 1 (next-token-of-type string 
							 :special
							 :start next-index
							 :end end
							 :value #\, :value-test #'eql)))
	    (if start
		(incf start)
		(return mailboxes))
	    (setf start (or (position-if-not #'white-space-p string :start start)
			    start))
	    )))))

(defun parse-group-address (string &key (start 0) (end (length string)) (errorp t))
  (let* ((semikolon-start (nth-value 1 (next-token-of-type string :special :start start :end end :value #\; :value-test #'eql)))
	 (list-start (nth-value 2 
				(next-token-of-type string 
						    :special 
						    :start start 
						    :end semikolon-start 
						    :value #\: 
						    :value-test #'eql))))
    (unless list-start
      (if errorp
	  (mime-parse-error "parsing group-address: ~A is not a group address" string)
	  (return-from parse-group-address nil)))
    ;; Address is of form [display-name] "<" addr-spec ">"
    (values (make-instance 'group-address
			   :mailbox-list (parse-mailbox-list string :start list-start :end semikolon-start)
			   :display-name (subseq string start (1- list-start)))
	    (1+ semikolon-start))))

(defun parse-address (string &key (start 0) (end (length string)) (errorp t))
  (restart-case 
      (let ((first-special (next-token-of-type string :special :start start :end end)))
	(ecase first-special
	  #+nil((nil) (mime-parse-error "parsing address: No address found"))
	  (#\: (parse-group-address string :start start 
				    :end end 
				    :errorp errorp))
	  ((#\< #\@ nil) (parse-mailbox-address string 
					    :start start 
					    :end end 
					    :errorp errorp))))
    (use-as-display-name () 
      (make-instance 'invalid-address
		     :display-name string
		     :address-spec nil))))

(defun parse-address-list (string  &key (start 0) (end (length string)))
  (let (addresses)
    (nreverse
    (loop (multiple-value-bind (address next-index)
	      (parse-address 
	       string 
	       :start start :end end :errorp nil)
	    (unless address
	      (return addresses))
	    (push address addresses)
	    (setf start (nth-value 1 (next-token-of-type string 
							 :special
							 :start next-index
							 :end end
							 :value #\, :value-test #'eql)))
	    (if start
		(incf start)
		(return addresses))
	    (setf start (or (position-if-not #'white-space-p string :start start)
			    start))
	    )))))

(defun address (address-designator)
  (etypecase address-designator
    (string (parse-address address-designator))
    (address address-designator)))

(defgeneric GROUP-MEMBER-P (group message))

(defmethod group-member-p ((group mailbox-address) (message mailbox-address))
  (address-eq group message))

(defmethod group-member-p ((group group-address) (message mailbox-address))
  (some (lambda (gm) (address-eq gm message)) (mailbox-list group)))

(defmethod group-member-p ((group mailbox-address) (message group-address))
  (some (lambda (m) (address-eq group m)) (mailbox-list message)))

(defmethod group-member-p ((group group-address) (message group-address))
  (not (null (intersection (mailbox-list group)
			   (mailbox-list message)
			   :test #'address-eq))))


;; Group Tables
;; Fast lookup for big groups
(defun make-group-table (addresses)
  (let ((table (make-hash-table :test 'eq)))
    (labels ((ladjoin (address)
             (typecase address
               (group-address (map nil #'ladjoin (mailbox-list address)))
               (mailbox-address
		(setf (gethash (address-spec address)
			       table) address)))))
      (dolist (address (mapcar #'address addresses))
	(ladjoin address))
      table)))

(defun member-address-p (group address)
  (etypecase (address address)
    (mailbox-address
     (nth-value 1 (gethash (address-spec address)
			   group)))
    (group-address 
     (member-address-p group
		       (mailbox-list address)))
    (cons 
     (some (lambda (a)
	     (nth-value 1 (gethash (address-spec a)
				   group)))
	   address))))

(defmethod print-object ((object invalid-address) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (if (and (slot-boundp object 'display-name)
                 (display-name object))
            (format stream "~A" (display-name object))
            (write-string "invalid address" stream)))
      (if (and (slot-boundp object 'display-name)
               (display-name object))
          (format stream "~A" (display-name object))
          (write-string "invalid address" stream))))

(defmethod print-object ((object mailbox-address) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (if (and (slot-boundp object 'display-name)
                 (display-name object))
            (format stream "~A <~A>" (display-name object) (address-spec object))
            (format stream "<~A>" (address-spec object))))
      (if (and (slot-boundp object 'display-name)
               (display-name object))
          (format stream "~A <~A>" (display-name object) (address-spec object))
          (format stream "<~A>" (address-spec object)))))

(defmethod print-object ((object group-address) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (if (slot-boundp object 'display-name)
            (format stream "~A:~A~{,~A~};" (display-name object) (first (mailbox-list object)) (rest (mailbox-list object)))
            (format stream "anonymous:~A~{,~A~};"  (first (mailbox-list object)) (rest (mailbox-list object)))))
      (if (slot-boundp object 'display-name)
          (format stream "~A:~A~{,~A~};" (display-name object) (first (mailbox-list object)) (rest (mailbox-list object)))
          (format stream "anonymous:~A~{,~A~};"  (first (mailbox-list object)) (rest (mailbox-list object))))))


(defun parse-rfc2822-address (string)
  (let ((angle (position #\< string)))
    (if angle
        (let ((angle-end (position #\> string)))
          (make-instance 'mailbox-address 
                         :address-spec (string-trim '(#\space) (subseq string (1+ angle) angle-end))
                         :display-name (string-trim '(#\space) (subseq string 0 angle))))
      (make-instance 'mailbox-address
		     :address-spec (string-trim '(#\space) string)))))



(define-field-reader from :from (message)
  (parse-address from))

(define-field-writer from :from (message)
  (princ-to-string from))

(define-field-reader reply-to :reply-to (message)
  (parse-address reply-to))

(define-field-writer reply-to :reply-to (message)
  (princ-to-string reply-to))


(define-field-reader to :to (message)
  (parse-address-list to))

(defun princ-objects-separated (list &optional (separator " ,") (stream *standard-output*))
  (if (listp list)
      (if (rest list)
	  (loop for item in (butlast list)
		do (princ item stream) (write-string separator stream)
		finally (princ (car (last list)) stream))
	 (princ (first list) stream))
    (princ list stream)))

(define-field-writer to :to (message)
  (with-output-to-string (s)
   (princ-objects-separated to " ," s)))

(define-field-reader sender :sender (message)
   (parse-rfc2822-address sender))

(define-field-writer sender :sender (message)
   (princ-to-string sender))

(define-field-reader cc-list :cc (message)
    (parse-address-list cc-list))

(define-field-writer cc-list :cc (message)
  (with-output-to-string (s)
   (princ-objects-separated cc-list " ," s)))


;;;
;;; Date & Times
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
;  (meta:enable-meta-syntax)
(deftype alpha-char () '(and character (satisfies alpha-char-p)))
(deftype %digit-char () '(and character (satisfies digit-char-p)))
(deftype sign-char () '(and character (member #\+ #\-)))
)



(defun date-to-universal-time (date)
  (flet ((match-month (string)
	   (flet ((month (name num)
			(if (string= name string :end2 3)
			  num
			  (mime-parse-error "parsing date: ~A did not match a month" string))))
	     (declare (inline month))
	     (case (char string 0)
	       (#\F (month "Feb" 2))
	       (#\S (month "Sep" 9))
	       (#\O (month "Oct" 10))
	       (#\N (month "Nov" 11))
	       (#\D (month "Dec" 12))
	       (otherwise
		(case (char string 2)
		  (#\y (month "May" 5))
		  (#\g (month "Aug" 8))
		  (#\l (month "Jul" 7))
		  (otherwise
		   (case (char string 1)
		     (#\p (month "Apr" 4))
		     (#\u (month "Jun" 6))
		     (otherwise
		      (case (char string 0)
			(#\M (month "Mar" 3))
			(#\J (month "Jan" 1))
			(otherwise 
			 (mime-parse-error"parsing date: ~A did not match a month" 
					  string))))))))))))

    (let (last-result)
      (smeta:with-string-meta (buffer date)
	(labels ((make-result ()
		   (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
		 (skip-day-of-week (&aux c)
		   (declare (ignorable c))
		   (smeta:match (:and (:while (:type alpha-char c)) (skip-delimiters))))
		 (skip-delimiters ()
		   (smeta:match (:while (:or #\: #\, #\space #\-))))
		 (skip-ws () (smeta:match (:while (:or #\space))))
		 (word (&aux (old-index smeta:index) c
			     (result (make-result)))
		   (or (smeta:match (:and (skip-delimiters)
					  (:type alpha-char c)
					  (vector-push-extend c result)
					  (:while (:and (:type alpha-char c) (vector-push-extend c result)))
					  (setf last-result result)))
		       (progn (setf smeta:index old-index) nil)))
		 (integer (&aux (old-index smeta:index) c
				(result (make-result)))
		   (or (smeta:match (:and (skip-delimiters) (:type %digit-char c) (vector-push-extend c result)
					  (:while (:and (:type %digit-char c) (vector-push-extend c result)))
					  (setf last-result (parse-integer result))))
		       (progn (setf smeta:index old-index) nil)))
		 (date (&aux day month year hours minutes seconds zone)
		   (and (smeta:match (:and (skip-day-of-week)
					   (:or (:and (word) (setf month last-result)
						      (integer) (setf day last-result))
						(:and (integer) (setf day last-result)
						      (word) (setf month last-result)))
					   (integer) (setf year last-result)
					   (integer) (setf hours last-result) 
					   (integer) (setf minutes last-result) 
					   (integer) (setf seconds last-result)
					   (skip-ws)
					   (:or (:and (:type sign-char zone) (setf zone (case zone
											  (#\+ -1)
											  (#\- 1)))
						      (integer) (setf zone (* zone last-result)))
						(:and (word) (setf zone 0))
                                                ;; some mailers (esp hotmail around 2001) don't set zones.
                                                (setf zone 0))))
					; (values seconds minutes hours day month)
			(multiple-value-bind (h m) (floor zone 100)
			  (unless (and (< -24 h +24)
				       (< -60 m +60)) (setf h 0 m 0))
			  (encode-universal-time seconds minutes hours day
						 (match-month (string-capitalize month))
						 year (+ h (/ m 60))))
			)))
	  (date))))))
  	  
(let (last-date)
  (defun universal-time-to-date (ut &optional (time-zone 0))
    (flet ((two-digits (number)
	     (let ((string (make-string 2)))
	       (multiple-value-bind (a b) (floor number 10)
		 (setf (char string 0) (digit-char a)
		       (char string 1) (digit-char b)))
	       string))
	   (month (number)
	     (ecase number
	       (1 "Jan")(2 "Feb")(3 "Mar")
	       (4 "Apr")(5 "May")(6 "Jun")
	       (7 "Jul")(8 "Aug")(9 "Sep")
	       (10 "Oct")(11 "Nov")(12 "Dec")))
	   (week-day (number)
	     (ecase number
	       (0 "Mon")(1 "Tue")(2 "Wed")
	       (3 "Thu")(4 "Fri")(5 "Sat")(6 "Sun")))
	   (the-time (hour minute second)
	     (let ((string (make-string 8)))
	       (multiple-value-bind (h1 h2)
		   (floor hour 10)
		 (setf (char string 0) (digit-char h1)
		       (char string 1) (digit-char h2)))
	       (setf (char string 2) #\:)
	       (multiple-value-bind (m1 m2)
		   (floor minute 10)
		 (setf (char string 3) (digit-char m1)
		       (char string 4) (digit-char m2)))
	       (setf (char string 5) #\:)
	       (multiple-value-bind (s1 s2)
		   (floor second 10)
		 (setf (char string 6) (digit-char s1)
		       (char string 7) (digit-char s2)))
	       string)))

	  (or (and (eql ut (caar last-date))
		   (eql time-zone (cdar last-date))
		   (cdr last-date))
	     (let ((result 
		    (multiple-value-bind
			  (sec min hour date month year day-of-week dsp time-zone)
			(decode-universal-time ut time-zone)
		      (declare (ignore dsp))
		      (concatenate 'string 
				   (week-day day-of-week)
				   ", "
				   (two-digits date)
				   " "
				   (month month)
				   " "
				   (princ-to-string year)
				   " "
				   (the-time hour min sec)
				   (if (zerop time-zone)
				       " GMT"
				       "")))))
	       		  (setf last-date (cons (cons ut time-zone) result))
			  result)))))
