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

#-cormanlisp
(defpackage :mel.gray-stream
  (:use :cl)
  (:import-from #+lispworks :stream #+cmu :lisp #+clisp :gray #+cormanlisp :gray-streams
                #+openmcl :ccl #+mcl :ccl #+allegro :excl #+sbcl :sb-gray #+abcl :gray-streams
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream
                #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:stream-element-type
                #:stream-listen
                #:stream-read-byte
                #:stream-read-char
                #:stream-peek-char
                #:stream-write-byte
                #:stream-write-char
                #:stream-read-char-no-hang
                #:stream-force-output
                #:stream-finish-output
                #:stream-clear-input
                #:stream-clear-output
                #:stream-line-column
                #-(or clisp openmcl) #:stream-read-sequence
                #:stream-unread-char
                #:stream-read-line
                #-(or clisp openmcl) #:stream-write-sequence
                #:stream-write-string
                #+lispworks #:stream-write-buffer
                #+lispworks #:stream-read-buffer
                #+lispworks #:stream-fill-buffer
                #+lispworks #:stream-flush-buffer
		#+abcl #:stream-close
                #+lispworks #:with-stream-input-buffer
                #+lispworks #:with-stream-output-buffer)
  (:export 
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:stream-element-type
   #:stream-listen
   #:stream-read-byte
   #:stream-read-char
   #:stream-write-byte
   #:stream-write-char
   #:stream-read-char-no-hang
   #+abcl #:stream-close
   #:stream-force-output
   #:stream-finish-output
   #:stream-clear-input
   #:stream-clear-output
   #:stream-line-column
   #-clisp #:stream-read-sequence
   #:stream-unread-char
   #:stream-read-line
   #-clisp #:stream-write-sequence
   #:stream-write-string
   #:stream-write-buffer
   #:stream-read-buffer
   #:stream-fill-buffer
   #:stream-flush-buffer
   #:with-stream-input-buffer
   #:with-stream-output-buffer))

(defpackage :mel.utils
  (:use :cl)
  (:export
   "WHEN-LET"
   "REBINDING"
   "WITH-UNIQUE-NAMES"
   "FILE-DIRECTORY-P"
   "WITH-INPUT-FROM-SEQUENCE"
   "MAKE-SEQUENCE-INPUT-STREAM"))

(defpackage mel.unix
  (:use :mel.utils :cl)
  (:nicknames weird-mail.unix)
  (:export "STAT"
	   "STAT-LAST-ACCESS"
	   "STAT-LAST-CHANGE"
	   "STAT-LAST-MODIFY"
	   "DIRECTORY-CONTENTS-CHANGED-P"
	   "UNIVERSAL-TO-UNIX-TIME"
	   "UNIX-TO-UNIVERSAL-TIME"
	   "UTIME"))

(defpackage :mel.network
  (:use :mel.utils :cl)
  (:export "MAKE-CONNECTION"))

(defpackage :mel.environment
  (:use :mel.utils :cl)
  (:export "GETPID"
	   "GETHOSTNAME"))

(defpackage :mel.filesystem
  (:use :mel.utils :cl)
  (:export "MAP-DIRECTORY"
	   "ONLY-NAME-AND-TYPE"
	   "APPEND-NAME"
	   "DIRECTORY-PATHNAME"))