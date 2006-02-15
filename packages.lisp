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

(in-package :cl-user)

(defpackage mel.dictionary
  (:use :mel.utils :cl)
  (:nicknames weird-mail.dictionary)
  (:export "DICTIONARY"
	   "MAKE-DICTIONARY"
	   "*GERMAN*"
	   "LOOKUP-WORD"))

;; The interface for normal use of the mail-library
(defpackage* :mel.public
  (:use :cl :mel.utils)
  (:nicknames weird-mail.public)
  (:use-external #+weird-mail.tester :weird-mail.tester)
  (:export
   ;; Classes
   "MESSAGE" ; <- workaround
   "MIME-MESSAGE" ; <- workaround
   "UID" ; <- workaround
   "FOLDER"
   "BASIC-FOLDER"
   "BASIC-RECEIVER"
   "BASIC-SENDER"
   "RECEIVER"
   "SENDER"

   ;; Folder Functions
   "FIND-FOLDER"
   "REGISTER-FOLDER"
   "SERIALIZE-FOLDER"
   "CLOSE-FOLDER"
   "COUNT-MESSAGES"
   "COPY-FOLDER"
   "MOVE-FOLDER"
   "CLEAR-FOLDER"
   "FIND-MESSAGE"
   "OPEN-MESSAGE-INPUT-STREAM"
   "OPEN-MESSAGE-STORING-STREAM"
   "MESSAGE-HEADER-STREAM"
   "MESSAGE-BODY-STREAM"
   "MESSAGES"
   "MAP-MESSAGES"
   "RECENT-MESSAGES"
   "MAP-RECENT-MESSAGES"
   "NAME"
   "SHORT-NAME"

   ;; Message Functions
   "FOLDER"
   "UID"
   "FLAGS"
   "FLAGP"
   "COPY-MESSAGE"
   "MOVE-MESSAGE"
   "DELETE-MESSAGE"
   "MARK-MESSAGE"
   "UNMARK-MESSAGE"
   "MESSAGE-SIZE"
   "MESSAGE-MD5SUM"
   "UNIQUE-MESSAGE-DIGEST"
   "MESSAGE-STRING"

   "FETCHMAIL"
   "*DEFAULT-SINK-FOLDER*"
   "*DEFAULT-SOURCE-FOLDER*"

   ;; Conditions
   "MEL-ERROR"
   "MESSAGE-NOT-FOUND"
))

;; The interface for mail-library extension
(defpackage* :mel.internal
    (:use :cl :mel.utils)
  (:nicknames weird-mail.internal)
  (:use-external :weird-mail.public)
  (:export 
   "MESSAGE-CACHE-MIXIN"
   "EQUAL-MESSAGE-CACHE-MIXIN"
   "EQL-MESSAGE-CACHE-MIXIN"
   "EQ-MESSAGE-CACHE-MIXIN"
   "MESSAGE-CACHE"

   "SIMPLE-FLAG-MIXIN"
   "FLAGP-USING-FOLDER"
   "MARK-MESSAGE-USING-FOLDER"
   "UNMARK-MESSAGE-USING-FOLDER"

   "DELETE-MESSAGE-USING-FOLDER"
   "OPEN-MESSAGE-INPUT-STREAM-USING-FOLDER"
   "MESSAGE-BODY-STREAM-USING-FOLDER"
   "MESSAGE-HEADER-STREAM-USING-FOLDER"
   "MESSAGE-SIZE-USING-FOLDER"
   "COPY-MESSAGE-USING-FOLDERS"
   "MOVE-MESSAGE-USING-FOLDERS"
   "ENSURE-HEADERS-READ"
   "MESSAGE-STRING-USING-FOLDER"

   "ENCAPSULATING-INPUT-STREAM"
   "ENCAPSULATING-OUTPUT-STREAM"
   "ENCAPSULATION-INPUT-STREAM"
   "ENCAPSULATION-OUTPUT-STREAM"
   "ENCAPSULATED-INPUT-STREAM"
   "ENCAPSULATED-OUTPUT-STREAM"
   "QUOTED-PRINTABLE-INPUT-STREAM"
   "DECODE-QUOTED-PRINTABLE"
   "DECODE-QUOTED-PRINTABLE-HEADER"
   ))

(defpackage* :mel.mime
    (:use :cl :mel.utils)
  (:nicknames weird-mail.mime)
    (:use-external :mel.internal)
    (:export
     "MESSAGE"
     "RFC2822-BASIC-MESSAGE"
     "SKIP-RFC2822-HEADER"
     "READ-RFC2822-HEADER"
     "PARSE-RFC2822-HEADER"
     "RFC2822-ADDRESS"
     "ADDRESS"
     "ADDRESS-EQ"
     "INVALID-ADDRESS"
     "USE-AS-DISPLAY-NAME"
     "MAILBOX-ADDRESS"
     "GROUP-ADDRESS"
     "MAILBOX-LIST"
     "MAKE-GROUP-TABLE"
     "MEMBER-ADDRESS-P"
     "PRINT-RFC2822-ADDRESS"
     "PARSE-RFC2822-ADDRESS"
     "PARSE-ADDRESS"
     "PARSE-ADDRESS-LIST"
     "PARSE-RFC2822"
     "ACCEPT-RFC2822-TOKEN"
     "BIND-RFC2822-TOKENS"
     "COLLECT-RFC2822-TOKENS"
     "DATE-TO-UNIVERSAL-TIME"
     "UNIVERSAL-TIME-TO-DATE"
     "HEADER-FIELDS"
     "DISPLAY-NAME"
     "ADDRESS-SPEC"
     "MAKE-CONTENT-TYPE"
     "MAKE-BOUNDARY-TAG"
     "STATE"
     "BODY-START"
     "FIELD"
     "MESSAGE-ID"
     "DATE"
     "SUBJECT"
     "FROM"
     "TO"
     "REPLY-TO"
     "SENDER"
     "CC-LIST"
     "MD5-SUM"
     "CONTENT-MD5-SUM"

     ;; mime.lisp
     "MIME-MESSAGE"
     "BODYSTRUCTURE"
     "COMPUTE-BODYSTRUCTURE"
     "MAKE-TOPLEVEL-PART-FROM-BODYSTRUCTURE"
     "MAKE-TOPLEVEL-PART"
     "TOPLEVEL-PART"
     "PART"
     "MULTIPART"
     "MULTIPART/mixed"
     "MULTIPART/alternative"
     "MULTIPART/parallel"

     "MIME-VERSION"
     "CONTENT-TYPE"
     "CONTENT-TRANSFER-ENCODING"
     "CONTENT-ID"
     "CONTENT-DESCRIPTION"
     "CONTENT-OCTETS"
     "BOUNDARY-TAG"
     "PARTS"
     "PART-STREAM"
     "PART-BODY-STREAM"
     "PART-STRING"
     "PART-BODY-STRING"
     "FIND-VIEWABLE-PART"
     "PART-START"
     "PART-END"

     ;; compose-message.lisp
     "MAKE-MESSAGE"
     "FINALIZE-MESSAGE"
;     "MAKE-MESSAGE-BODY"
;     "EDIT-MESSAGE-BODY"
     ))

(defpackage* :mel.cipher
    (:use :cl :mel.utils #+sbcl :sb-md5 #-sbcl :mel.cipher.md5)
    (:use-external :mel.internal)
    (:export
     "HMAC-MD5"
     "STRING-TO-OCTETS"
     "OCTETS-TO-HEX"))


;; Folders
(defpackage mel.folders.smtp
  (:use :cl :mel.utils

	:mel.network
	:mel.environment

	:weird-mail.internal 
	:weird-mail.mime)
  (:nicknames weird-mail.folders.smtp)
  (:export "SMTP-RELAY-FOLDER"
	   "MAKE-SMTP-RELAY-FOLDER"
	   "HOST"
	   "SMTP-PORT"))

(defpackage :mel.folders.maildir
  (:use :cl 
   :mel.internal

   :mel.unix
   :mel.environment

   :mel.mime
   :mel.utils)
  (:nicknames :weird-mail.folders.maildir)
  (:export "MAILDIR-FOLDER"
	   "MAKE-MAILDIR-FOLDER"
           "PROBABILITY"))

(defpackage mel.folders.pop3
  (:use :cl 
	:mel.internal
	:mel.mime

	:mel.network

	:mel.utils)
  (:nicknames weird-mail.folders.pop3)
  (:export "POP3-FOLDER"
	   "MAKE-POP3-FOLDER"))

(defpackage mel.folders.imap
  (:use :cl
   :mel.internal
   :mel.mime

   :mel.network

   :mel.utils)
  (:nicknames weird-mail.folders.imap)
  (:export "IMAP-FOLDER"
	   "MAKE-IMAP-FOLDER"))

;; Additional
(defpackage mel.pop3-server
  (:use :weird-mail.internal :cl :mel.utils)
  (:nicknames weird-mail.pop3-server)
  (:export "START-POP3-SERVER"
	   "POP3-SERVER"))

;; All collected external interfaces
(defpackage* mel
    (:nicknames weird-mail)
    (:use-external
     :weird-mail.public
     :weird-mail.mime
     :weird-mail.folders.smtp
     :weird-mail.folders.maildir
     :weird-mail.folders.pop3
     :mel.folders.imap
     :weird-mail.pop3-server))

(defpackage :mel-user
  (:use :cl :mel))
