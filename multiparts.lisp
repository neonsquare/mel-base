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

;; Multiparts
;; /mixed (default) - bundle, ordered
;; /alternative - choose last part supported
;; /digest - change default to message/rfc822
;; /parallel - show all in parallel

(defclass part (bodystructure-mixin mime-header-mixin mime-body-mixin)
  ((part-number :accessor part-number :initarg :part-number :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (start :accessor part-start :initarg :start :initform nil)
   (end :accessor part-end :initarg :end :initform nil)
   (md5-sum :reader md5-sum :initarg :md5-sum :initform nil)
   
   (parameters :reader content-parameters :initarg :parameters)
   (content-supertype :reader content-supertype :initarg :supertype)
   (content-subtype :reader content-subtype :initarg :subtype)))

(defclass simple-part (part)
  ((content-encoding :reader content-encoding :initarg :encoding)
   (content-octets :reader content-octets :initarg :octets)
   (content-lines :reader content-lines :initarg :lines)))

(defmethod part-path ((part part))
  (when (and (slot-boundp part 'part-number)
	     (slot-boundp part 'parent))
    (typecase (parent part)
      (message nil)
      (part (nconc (part-path (parent part)) (list (part-number part)))))))
    
(defmethod print-object ((object part) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (or (part-path object) "N/A"))
    #+nil
    (when header-fields
      (multiple-value-bind (super sub) (content-type object)
	(when (and super sub)
	  (format stream " ~A/~A" super sub))))
    #+nil
    (when (and start end)
      (format stream " start=~D end=~D" start end))))
      
(defmethod folder ((part part))
  (parent part))
	  
(defmethod ensure-headers-read ((parent mime-header-mixin) (part part))
  (unless (header-fields part)
    (with-open-stream (stream (open-message-input-stream part 0))
      (setf (header-fields part) (read-rfc2822-header stream)))))

(defgeneric parts (message))

(defclass multipart (part)
  ((parts :accessor %parts :initform nil :initarg :parts)))

(defclass multipart/mixed (multipart)())
(defclass multipart/alternative (multipart)())
(defclass multipart/parallel (multipart)())

;;
;; (pos-1 pos2 pos3... end-boundary)
;; (1+ (1- (length profile))) parts
;; (nth (1- nth) profile) = part-offset

(defun string-prefixp (prefix string)
  (let ((plen (length prefix)))
    (and (>= (length string) plen)
         (string= prefix string :end1 plen :end2 plen))))

;; Body Structure as defined in RFC2060 (IMAP)
; non-multipart:
; (supertype subtype parameters nil nil encoding octets lines nil nil nil)
; multipart
; ((part)(part)... multipart-type parameters nil nil)

(defmethod content-type ((message part))
  (if (slot-boundp message 'content-supertype)
      (values (content-supertype message)
	      (content-subtype message)
	      (content-parameters message))
      (call-next-method)))

(defmethod content-transfer-encoding ((message simple-part))
  (if (slot-boundp message 'content-encoding)
      (content-encoding message)
      (call-next-method)))

(defun multipart-type-class (type)
  (case type
    (:alternative 'multipart/alternative)
    (:mixed 'multipart/mixed)
    (:parallel 'multipart/parallel)
    (otherwise 'multipart)))

(defmethod toplevel-part ((message bodystructure-mixin))
  (or (cached-toplevel-part message)
      (setf (cached-toplevel-part message)
	    (make-toplevel-part message))))

(defmethod parts ((message mime-message))
  (parts (toplevel-part message)))

(defmethod parts ((message simple-part))
  (list message))

(defmethod parts ((message multipart))
  (%parts message))

(defmethod make-toplevel-part ((message bodystructure-mixin))
  (make-toplevel-part-from-bodystructure message
					 (bodystructure message)))

(defun make-toplevel-part-from-bodystructure (parent bodystructure &optional nth)
  (etypecase (car bodystructure)
    (cons (let ((non-parts (member-if #'symbolp bodystructure))
		(subparts (butlast bodystructure 4)))
	    (let ((multipart-type (first non-parts))
		  (parameters (second non-parts)))
	      (let ((part (make-instance (multipart-type-class multipart-type)
					 :parent parent
					 :part-number (or nth 0)
					 :supertype :multipart
					 :subtype multipart-type
					 :parameters parameters
					 )))
		(setf (%parts part) (let ((i 0))
				     (mapcar (lambda (p)
					       (make-toplevel-part-from-bodystructure
						part p (incf i)))
					     subparts)))
		part))))
    (symbol (make-instance 'simple-part
			   :parent parent
			   :part-number (or nth 0)
			   :supertype (first bodystructure)
			   :subtype (second bodystructure)
			   :parameters (third bodystructure)
			   :octets (nth 6 bodystructure)
			   :lines (nth 7 bodystructure)
			   :encoding (nth 5 bodystructure)))))

(defmethod bodystructure :before ((message bodystructure-mixin))
  (unless (slot-boundp message 'bodystructure)
    (setf (bodystructure message) (compute-bodystructure message))))

(defun read-lines-and-octets (in-stream)
  (let ((octets 0)
        (lines 0))
    (flet ((peek ()
	     (peek-char nil in-stream nil :eof))
	   (consume ()
	     (prog1
		 (read-char in-stream)
	       (incf octets))))
	     (tagbody
	      start (let ((c (peek)))
		      (case c
			(#\return (consume) (go cr))
			(#\linefeed (consume) (go lf))
                        (:eof (go eof))
			(otherwise (consume) (go start))))
		
	      lf (go newline)
		
	      cr 
		(let ((c (peek)))
		  (case c
		    (#\linefeed 
                     (consume) (go newline))
                    (:eof (incf lines) (go eof))
		    (otherwise
                     (go newline))))
		
	      newline 
                (incf lines)
                (go start)
              
              eof
              (return-from read-lines-and-octets
                (values lines octets))))))

(defun scan-forward-boundary-tag (in-stream boundary)
  (let ((tag (concatenate 'string "--" boundary))
        (match 0)
        (octets 0)
        (lines 0)
        (line-ending-octets 0))
    (flet ((peek ()
	     (peek-char nil in-stream nil :eof))
           (skip () (read-char in-stream))
	   (consume ()
	     (prog1
		 (read-char in-stream)
	       (incf octets))))
	     (tagbody
              init (let ((c (peek)))
                     (if (char= c #\-)
                       (go possible-boundary)
                       (go start)))
	      start (let ((c (peek)))
		      (case c
			(#\return (skip) (go cr))
			(#\linefeed (skip) (go lf))
			(otherwise (consume) (go start))))
		
	      lf (setf line-ending-octets 1) (go newline)
		
	      cr 
		(let ((c (peek)))
		  (case c
		    (#\linefeed (setf line-ending-octets 2)
                                (skip) (go newline))
		    (otherwise
                     (setf line-ending-octets 1)
                     (go newline))))
		
	      newline 
                (let ((c (peek)))
                  (case c
                    (#\- (go possible-boundary))
                    (otherwise (incf octets line-ending-octets)
                               (incf lines)
                               (setf line-ending-octets 0)
                               (go start))))

              possible-boundary
                  (if (= match (length tag))
                    (go boundary-matched)
                    (let ((c (peek)))
                      (cond ((char= c (char tag match))
                             (skip) (incf match)(go possible-boundary))
                            (t (incf octets (+ line-ending-octets match))
                               (setf match 0 line-ending-octets 0)
                               (incf lines)
                               (go start)))))
              boundary-matched
                  (let ((c (peek)))
                    (case c
                      (#\- (skip)
                           (case c
                             (#\- (go end-boundary))
                             (otherwise (go boundary))))
                      (otherwise (go boundary))))
             
              boundary
                  (loop until (case (peek) ((#\return #\linefeed :eof) t)
                                (otherwise nil))
                        do (skip))
                  (loop while (case (peek) ((#\return #\linefeed) t)
                                (otherwise nil))
                        do (skip))
                  (return-from scan-forward-boundary-tag
                    (values octets lines nil))
                  
              end-boundary
                   (loop until (case (peek) ((#\return #\linefeed :eof) t)
                                 (otherwise nil))
                         do (skip))
                  (return-from scan-forward-boundary-tag
                    (values octets lines t))
                   ))))
			        
(defun compute-bodystructure (message)
  (compute-bodystructure-using-folder (folder message) message))

(defun read-multipart-body-1 (part stream)
  (let ((boundary (boundary-tag part)))
    (let (parts
	  last-part)
      (loop
       (multiple-value-bind (octets lines endp)
	   (scan-forward-boundary-tag stream boundary)
	 (when last-part
	   #+nil (incf (seventh last-part) octets)
	   (setf (seventh last-part) octets)
	   (setf (eighth last-part) lines)
	   (setf last-part nil))
	 (cond (endp
		; (format t "End tag of boundary=~A~%" boundary)
		(multiple-value-bind (super sub params)
		    (content-type part)
		  (declare (ignore super))
		  (let ((result `(,@(nreverse parts) ,sub ,params nil nil)))
		    ; (format t "Multipart-Structure: ~A~%" result)
		    (return result))))
	       (t
		(multiple-value-bind (headers hoctets) (read-rfc2822-header stream)
                  (declare (ignore hoctets))
		 ; (format t "Headers read ~A" hoctets)
		  (let ((content-type (or (cdr (assoc :content-type headers))
					  "text/plain")))
		    (multiple-value-bind (super sub params) (parse-content-type content-type)
		      (declare (ignore params))
		    (let ((next-part (make-instance (if (eq super :multipart)
							(multipart-type-class sub)
							'simple-part)
						    :header-fields (or headers 
								       '((:content-type . "text/plain"))))))
		    (if (eq super :multipart)
			(push (read-multipart-body-1 next-part stream) parts)
			(push (setf last-part (read-simple-body next-part)) parts)))))))))))))

(defun read-simple-body (part)
  (multiple-value-bind (super sub params)
      (content-type part)
    (unless (getf params :charset)
        (setf params (list* :charset "us-ascii" params)))
  (let ((result (list super sub params nil nil (content-transfer-encoding part) nil nil nil nil nil)))
    (force-output t)
    result)))

;; super sub params nil nil encoding octets lines nil nil nil
(defun read-single-body (part stream)
  (multiple-value-bind (lines octets)
      (read-lines-and-octets stream)
    (multiple-value-bind (super sub params)
	(content-type part)
      (unless (getf params :charset)
        (setf params (list* :charset "us-ascii" params)))
      (let ((result (list super sub params nil nil (content-transfer-encoding part) octets lines nil nil nil)))
	result))))

(defmethod compute-bodystructure-using-folder (folder (message bodystructure-mixin))
  (declare (ignore folder))
  (with-open-stream (stream (message-body-stream message))
    (if (eq :multipart (content-type message))
	(read-multipart-body-1 message stream)
	(read-single-body message stream))))

(defmethod nth-part-stream (nth boundary-tag (message mime-header-mixin))
    (let* ((boundary-tag (concatenate 'string "--" boundary-tag)))
      (declare (dynamic-extent boundary-tag))
      (let ((s (message-body-stream message)))
	(if (zerop nth)
	    s
	  (loop for line = (read-line s nil nil)
		while line
		do (when (string-prefixp boundary-tag line)
		     (if (= nth 1) 
			 (return-from nth-part-stream s)
		       (decf nth))))))))

#+nil
(defmethod part-stream ((part part))
  (nth-part-stream (part-number part) (parent part))
  (with-slots (start end) part
     (if (and start end)
	 ;; A start and end position of the part are given.
	 ;; Find the root parent of the given part to locate the the physical
	 ;; location of the whole message
         (let ((parent (loop for parent = (parent part) then (parent parent)
                             until (typep parent 'message)
                             finally (return parent))))
	   ;; Open a message input stream beginning at
	   ;; the start of the part
	   (open-message-input-stream parent start))
       ;; No part boundaries are given.
       ;; Locate it by scanning through the message
       )))

(defmethod part-stream ((part part))
  "Return a stream to the start of the part"
  (let* ((message (loop for parent = (parent part) then (parent parent)
                        until (typep parent 'message)
                        finally (return parent)))
         (stream (message-body-stream message))
         (boundary (boundary-tag (parent part))))
    (loop repeat (part-number part)
          do (scan-forward-boundary-tag stream boundary))
    stream))

(defmethod part-body-stream ((part part))
  "Skip header to beginning of part body and return stream"
  (let ((part-stream (part-stream part)))
    (read-rfc2822-header part-stream)
    part-stream))

(defmethod open-message-input-stream-using-folder
           (folder (part part) start)
  (declare (ignore folder))
  (let ((stream (part-stream part)))
    (when start
      (loop repeat start do (read-char stream)))
    stream))

(defmethod message-body-stream-using-folder (folder (part part))
  (declare (ignore folder))
  (part-body-stream part))

(defmethod nth-part (nth (message mime-header-mixin))
  (make-instance 'part :parent message :part-number nth))

(defun part-body-string (part)
  (with-output-to-string (out)
    (with-open-stream (s (message-body-stream part))
      (loop repeat (content-octets part)
	    for c = (read-char s)
	    do (write-char c out)))))

#+nil
(defun part-string (part)
  (with-output-to-string (out)
    (with-open-stream (s (part-stream part))
      (loop for pos = (+ (skip-rfc2822-header s)
			 (part-start part)) then (incf pos)
	    while (< pos (part-end part))
            do (let ((c (read-char s)))
                 (case c
                   ((#\return #\linefeed) (write-char #\^ out) )
                   (#\newline (write-char #\# out))
                   (otherwise (write-char c out))))))))

(defun find-viewable-part (message)
  (let ((parts (parts message)))
    ;; The viewable part of a message is either...
    (or ;; The first toplevel text part
        (find :text parts :key #'content-type)
        ;; The first text part of a toplevel alternative multipart
       (let ((alternative (find :alternative parts :key (lambda (p) (nth-value 1 (content-type p))))))
	  (when alternative
	    (find :text (parts alternative)
		  :key #'content-type)))
       (let ((mixed (find :mixed parts :key (lambda (p) (nth-value 1 (content-type p))))))
	  (when mixed
	    (find :text (parts mixed)
		  :key #'content-type)))
       ;; The very first part of the message
	(first parts))))
