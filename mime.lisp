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

(defclass mime-header-mixin (rfc2822-header-mixin) ())

(defclass mime-body-mixin (rfc2822-basic-body-mixin) ())

(defclass bodystructure-mixin ()
  ((bodystructure :accessor bodystructure)
   (cached-toplevel-part :accessor cached-toplevel-part
			 :initform nil)))

(defclass mime-message (bodystructure-mixin mime-header-mixin mime-body-mixin message) ())

(defun parse-mime-version (string)
  (bind-rfc2822-tokens (string () (major-version :atom) (del :special #\.) (minor-version :atom))
    (declare (ignore del))
    (values (parse-integer major-version)
            (parse-integer minor-version))))

(defun alist->plist (alist)
  (loop for (a . b) in alist
	nconc (list a b)))

(defun parse-content-type (string)
  "default Content-type: text/plain; charset=us-ascii
   => (values super sub parameters)"
  (flet ((substring (string start &optional end)
           (let ((end (or end (length string))))
             (make-array (- end start) 
                         :element-type (array-element-type string)
                         :displaced-to string 
                         :displaced-index-offset start))))
    (declare (inline substring))
    (let* ((pstart (position #\; string))
           (type/subtype (substring string 0 pstart)))
      (declare (dynamic-extent type/subtype))
      (flet ((keyword (string)
		      (intern
		       (string-upcase
			(string-left-trim '(#\space #\return #\newline #\tab)
					  string))
		       #.(find-package :keyword))))
	(declare (dynamic-extent #'keyword))
      (let ((delimiter (position #\/ type/subtype)))
        (if pstart
          (let ((params 
		 (loop with result = nil
		       with next = nil
		       for start = pstart then next 
		       while start
		       do (let ((del (position #\= string :start start)))
			    (if (not del)
				(return result)
			      (progn
				(setf next (position #\; string :start (1+ start)))
				
				(push (cons 
				       (keyword (subseq string (1+ start) del))
                                       (accept-rfc2822-token (apply #'subseq string 
                                                                    (1+ del)
                                                                    (if next (list next)))
                                                             nil
                                                             :type-test (mel.mime::token-type-test-function 'or :atom :dot-atom :quoted-string)))
				      result))))
		       finally (return result))))
            (values (keyword (subseq type/subtype 0 delimiter))
                    (keyword (subseq type/subtype (1+ delimiter)))
                    (alist->plist params)))
	  (values (keyword (subseq type/subtype 0 delimiter))
		  (keyword (subseq type/subtype (1+ delimiter))))))))))

(defun make-content-type (super sub &rest params)
  (format nil "~A/~A~{; ~A=\"~A\"~}" 
	  (string-downcase super)
	  (string-downcase sub) params))

(defun make-boundary-tag ()
  (format nil "~A~A" 
	  (write-to-string (get-universal-time) :base 36 :escape nil)
	  (write-to-string (random 9999999) :base 36 :escape nil)))
	  
(defun make-content-transfer-encoding (encoding)
  (string-capitalize encoding))

(defun parse-content-transfer-encoding (string)
  (bind-rfc2822-tokens (string () (mechanism :atom))
       (intern (string-upcase mechanism) #.(find-package :keyword))))

;;; Mime Field Attributes

(defmethod mime-version ((message mime-header-mixin))
  (when-let (version-field (field :mime-version message))
    (parse-mime-version version-field)))

(defmethod (setf mime-version) (value (message mime-header-mixin))
  (setf (field :mime-version message t) value))
    
(defgeneric CONTENT-TYPE (message))

(defmethod content-type ((message mime-header-mixin))
  (when-let (content-type-field (field :content-type message))
    (when (consp content-type-field)
      (setf content-type-field (first content-type-field)))
;    (ignore-errors
      (multiple-value-bind (super sub params)
	  (parse-content-type content-type-field)
	(if (eq super :multipart)
	    (if (not (getf params :boundary))
		(values :text :plain params)
	      (values super sub params))
	  (values super sub params)))));)
	    

(defmethod content-transfer-encoding ((message mime-header-mixin))
  (when-let (encoding-field (field :content-transfer-encoding message))
    (parse-content-transfer-encoding encoding-field)))

(defmethod content-id ((message mime-header-mixin))
  (field :content-id message))

(defmethod content-description ((message mime-header-mixin))
  (field :content-description message))

(defmethod boundary-tag ((message mime-header-mixin))
  (getf (nth-value 2 (content-type message)) :boundary)
  #+nil
  (cdr (assoc :boundary (nth-value 2 (content-type message)))))
