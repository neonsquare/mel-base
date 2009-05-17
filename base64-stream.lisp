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

(defparameter base-64-alphabet 
  (load-time-value (make-array 64 :element-type 'base-char
			       :initial-contents '(
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m 
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))))

(defparameter url-base-64-alphabet 
  (load-time-value (make-array 64 :element-type 'base-char
			       :initial-contents '(
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m 
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\_))))


(declaim (inline base64-char))
(defun base64-char (base64)
  (aref base-64-alphabet base64))

(declaim (inline url-base64-char))
(defun url-base64-char (base64)
  (aref url-base-64-alphabet base64))

(declaim (inline char-base64))
(defun char-base64 (char)
  (case char
    (#\A 0)    (#\B 1)    (#\C 2)    (#\D 3)
    (#\E 4)    (#\F 5)    (#\G 6)    (#\H 7)
    (#\I 8)    (#\J 9)    (#\K 10)    (#\L 11)
    (#\M 12)    (#\N 13)    (#\O 14)    (#\P 15)
    (#\Q 16)    (#\R 17)    (#\S 18)    (#\T 19)
    (#\U 20)    (#\V 21)    (#\W 22)    (#\X 23)
    (#\Y 24)    (#\Z 25)    (#\a 26)    (#\b 27)
    (#\c 28)    (#\d 29)    (#\e 30)    (#\f 31)
    (#\g 32)    (#\h 33)    (#\i 34)    (#\j 35)
    (#\k 36)    (#\l 37)    (#\m 38)    (#\n 39)
    (#\o 40)    (#\p 41)    (#\q 42)    (#\r 43)
    (#\s 44)    (#\t 45)    (#\u 46)    (#\v 47)
    (#\w 48)    (#\x 49)    (#\y 50)    (#\z 51)
    (#\0 52)    (#\1 53)    (#\2 54)    (#\3 55)
    (#\4 56)    (#\5 57)    (#\6 58)    (#\7 59)
    (#\8 60)    (#\9 61)    (#\+ 62)    (#\/ 63)
    (otherwise nil)))

(declaim (inline url-char-base64))
(defun url-char-base64 (char)
  (case char
    (#\A 0)    (#\B 1)    (#\C 2)    (#\D 3)
    (#\E 4)    (#\F 5)    (#\G 6)    (#\H 7)
    (#\I 8)    (#\J 9)    (#\K 10)    (#\L 11)
    (#\M 12)    (#\N 13)    (#\O 14)    (#\P 15)
    (#\Q 16)    (#\R 17)    (#\S 18)    (#\T 19)
    (#\U 20)    (#\V 21)    (#\W 22)    (#\X 23)
    (#\Y 24)    (#\Z 25)    (#\a 26)    (#\b 27)
    (#\c 28)    (#\d 29)    (#\e 30)    (#\f 31)
    (#\g 32)    (#\h 33)    (#\i 34)    (#\j 35)
    (#\k 36)    (#\l 37)    (#\m 38)    (#\n 39)
    (#\o 40)    (#\p 41)    (#\q 42)    (#\r 43)
    (#\s 44)    (#\t 45)    (#\u 46)    (#\v 47)
    (#\w 48)    (#\x 49)    (#\y 50)    (#\z 51)
    (#\0 52)    (#\1 53)    (#\2 54)    (#\3 55)
    (#\4 56)    (#\5 57)    (#\6 58)    (#\7 59)
    (#\8 60)    (#\9 61)    (#\- 62)    (#\_ 63)
    (otherwise nil)))

(defmethod encode-base64 ((sequence sequence) &key (start 0) (end (length sequence)))
  "Encode the octet-sequence sequence in base64 format"
  (let ((index (1- start))
	(buffer-index -1)
	(rest (mod end 3)))
    (let ((buffer (make-string (multiple-value-bind (div rest)
				  (floor (- end start) 3) (+ (* div 4) (if (zerop rest) 0 4))))))
    (loop for quantum = 0
	  while (< index (1- (- end rest)))
	  do (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
		   (ldb (byte 8 8)  quantum) (elt sequence (incf index))
	           (ldb (byte 8 0)  quantum) (elt sequence (incf index)))
	  (setf (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 18) quantum))
		(aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 12) quantum))
		(aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 6) quantum))
		(aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 0) quantum)))
	  finally 
	  (cond ((= rest 2) (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
				  (ldb (byte 8 8)  quantum) (elt sequence (incf index))
				  (ldb (byte 8 0)  quantum) 0)
		 (setf (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 18) quantum))
		       (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 12) quantum))
		       (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 6) quantum))
		       (aref buffer (incf buffer-index)) #\=))
		((= rest 1) (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
				  (ldb (byte 8 8)  quantum) 0)
		 (setf (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 18) quantum))
		       (aref buffer (incf buffer-index)) (base64-char (ldb (byte 6 12) quantum))
		       (aref buffer (incf buffer-index)) #\=
		       (aref buffer (incf buffer-index)) #\=))))
    buffer)))

(defun url-encode-base64 (sequence &key (start 0) (end (length sequence)))
  "Encode the octet-sequence sequence in base64 format"
  (let ((index (1- start))
	(buffer-index -1)
	(rest (mod end 3)))
    (let ((buffer (make-string (multiple-value-bind (div rest)
				  (floor (- end start) 3) (+ (* div 4) (if (zerop rest) 0 4))))))
    (loop for quantum = 0
	  while (< index (1- (- end rest)))
	  do (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
		   (ldb (byte 8 8)  quantum) (elt sequence (incf index))
	           (ldb (byte 8 0)  quantum) (elt sequence (incf index)))
	  (setf (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 18) quantum))
		(aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 12) quantum))
		(aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 6) quantum))
		(aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 0) quantum)))
	  finally 
	  (cond ((= rest 2) (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
				  (ldb (byte 8 8)  quantum) (elt sequence (incf index))
				  (ldb (byte 8 0)  quantum) 0)
		 (setf (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 18) quantum))
		       (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 12) quantum))
		       (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 6) quantum))
		       (aref buffer (incf buffer-index)) #\=))
		((= rest 1) (setf (ldb (byte 8 16) quantum) (elt sequence (incf index))
				  (ldb (byte 8 8)  quantum) 0)
		 (setf (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 18) quantum))
		       (aref buffer (incf buffer-index)) (url-base64-char (ldb (byte 6 12) quantum))
		       (aref buffer (incf buffer-index)) #\=
		       (aref buffer (incf buffer-index)) #\=))))
    buffer)))

(defun decode-base64 (string)
  (let ((buffer-index -1)
	(rest 0))
    (macrolet ((until (expr)
		 `(loop for e = ,expr
		   until e
		   finally (return e))))
      (when (char= (char string (1- (length string))) #\=)
	(incf rest)
	(when (char= (char string (- (length string) 2)) #\=)
	  (incf rest)))
      (let ((buffer (make-array (- (/ (* (length string) 6) 8) rest) :element-type '(unsigned-byte 8))))
	(loop with quantum = 0
	      for char across string
	      until (eql char #\=)
	      with byte-pos = 18
	      do (setf (ldb (byte 6 byte-pos) quantum) (or (char-base64 char) 0))
	      (if (zerop byte-pos)
		  (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16) quantum)
			(elt buffer (incf buffer-index)) (ldb (byte 8 8)  quantum)
			(elt buffer (incf buffer-index)) (ldb (byte 8 0)  quantum)
			byte-pos 18)
		  (decf byte-pos 6))
	      finally 

	      (case (+ byte-pos 6)
		(6 (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16)  quantum))
		   (setf (elt buffer (incf buffer-index)) (ldb (byte 8 8)  quantum)))
		(12 (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16) quantum)))
		(otherwise nil)))
	buffer))))

(defun url-decode-base64 (string)
  (let ((buffer-index -1)
	(rest 0))
    (macrolet ((until (expr)
		 `(loop for e = ,expr
		   until e
		   finally (return e))))
      (when (char= (char string (1- (length string))) #\=)
	(incf rest)
	(when (char= (char string (- (length string) 2)) #\=)
	  (incf rest)))
      (let ((buffer (make-array (- (/ (* (length string) 6) 8) rest) :element-type '(unsigned-byte 8))))
	(loop with quantum = 0
	      for char across string
	      until (eql char #\=)
	      with byte-pos = 18
	      do (setf (ldb (byte 6 byte-pos) quantum) (or (char-base64 char) 0))
	      (if (zerop byte-pos)
		  (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16) quantum)
			(elt buffer (incf buffer-index)) (ldb (byte 8 8)  quantum)
			(elt buffer (incf buffer-index)) (ldb (byte 8 0)  quantum)
			byte-pos 18)
		  (decf byte-pos 6))
	      finally 

	      (case (+ byte-pos 6)
		(6 (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16)  quantum))
		   (setf (elt buffer (incf buffer-index)) (ldb (byte 8 8)  quantum)))
		(12 (setf (elt buffer (incf buffer-index)) (ldb (byte 8 16) quantum)))
		(otherwise nil)))
	buffer))))