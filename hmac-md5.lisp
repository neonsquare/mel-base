(in-package :mel.cipher)

(defun string-to-octets (string) (let ((buffer (make-array (length string) :element-type '(unsigned-byte 8))))
					    (dotimes (i (length string) buffer)
					      (setf (aref buffer i) (char-code (char string i))))))

(defun hmac-md5 (text key)
  (let ((text (if (stringp text) (string-to-octets text) text))
	(key (if (stringp key) (string-to-octets key) key))
	(ipad (make-array 64
			  :element-type '(unsigned-byte 8) 
			  :initial-element #x36))
	(opad (make-array 64 
			  :element-type '(unsigned-byte 8) 
			  :initial-element #x5c)))

    (when (> (length key) 64)
      (setf key (md5sum-sequence key)))
 ;   (setf ipad (replace ipad key))
 ;   (setf opad (replace opad key))
    (dotimes (i (length key))
      (setf (aref ipad i) (logxor (aref key i)
				  #x36)
	    (aref opad i) (logxor (aref key i)
				  #x5c)))

    (let ((buffer (make-array (+ (length text)
				 (length ipad))
			      :element-type '(unsigned-byte 8)
			      :initial-contents (concatenate 'vector ipad text))))

    (let ((digest (md5sum-sequence buffer)))
      (let ((buffer (make-array (+ (length digest)
				   (length ipad))
				:element-type '(unsigned-byte 8)
				:initial-contents (concatenate 'vector opad digest))))
	(md5sum-sequence buffer))))))

(defun octets-to-hex (octets)
  (with-output-to-string (s)
    (loop for b across octets do (format s "~X" b))))