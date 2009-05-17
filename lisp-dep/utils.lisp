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

(in-package :mel.utils)

(defmacro with-unique-names ((&rest bindings) &body body)
"
Syntax:
  WITH-UNIQUE-NAMES ({var | (var prefix)}*) declaration* form* => result* 
Description:
  Executes a series of forms with each var bound to a fresh,
  uninterned symbol. The uninterned symbol is created as if 
  by a call to gensym with the string denoted by prefix or,
  if prefix is not supplied, the string denoted by var as 
  argument.

  The variable bindings created are lexical unless special 
  declarations are specified.

  The forms are evaluated in order, and the values of all but
  the last are discarded (that is, the body is an implicit 
  progn).
"
;; Implementation taken from
;; http://www.cliki.net/Common%20Lisp%20Utilities
;; Author is Vebjorn Ljosa
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
                             (if (consp binding) 
				 binding 
				 (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
    ,@body))

(defmacro rebinding (bindings &body body)
  "
Syntax:
  REBINDING ( { var | (var prefix) }* ) form*  =>  result

Description:
  Evaluates a series of forms in the lexical environment that is
  formed by adding the binding of each var to a fresh, uninterned
  symbol, and the binding of that fresh, uninterned symbol to var's
  original value, i.e., its value in the current lexical
  environment.

  The uninterned symbol is created as if by a call to GENSYM with
  the string denoted by prefix---or, if prefix is not supplied, the
  string denoted by var---as argument.

  The forms are evaluated in order, and the values of all but the
  last are discarded (that is, the body is an implicit progn).
"
;; Implementation taken from
;; http://groups.google.com/groups?q=rebinding&hl=en&lr=&ie=UTF-8&c2coff=1&selm=cy3wv0fya0p.fsf%40ljosa.com&rnum=1
;; Author is Vebjorn Ljosa
  (loop for binding in bindings
	for var = (if (consp binding) (car binding) binding)
	for name = (gensym)
	collect `(,name ,var) into renames
	collect ``(,,var ,,name) into temps
	finally (return `(let ,renames
			  (with-unique-names ,bindings
			    `(let (,,@temps)
			      ,,@body))))))

(defmacro when-let ((variable form) &body body)
  `(let ((,variable ,form))
    (when ,variable
      ,@body)))

(defun file-directory-p (x)
  #+cmu (eq :directory (unix:unix-file-kind
			(namestring x)))
  #+sbcl (eq :directory (sb-unix:unix-file-kind
			(namestring x)))
  #+acl (eq :directory (excl:filesys-type x))
  #+(or mcl openmcl) (and (ccl:directory-pathname-p x)
			  (probe-file x))
  #+lispworks (lw:file-directory-p x)
  #+abcl (null (pathname-name (truename x)))
  #+(and clisp lisp=cl) (nth-value 0 (ignore-errors (ext:probe-directory x)))
  #+(and clisp (not lisp=cl)) (nth-value 0 (ignore-errors (lisp:probe-directory x)))
  #-(or cmu sbcl acl mcl lispworks abcl clisp)(error "file-directory-p not implemented"))

(defmacro with-input-from-sequence ((sym seq) &body forms)
  (rebinding (seq)
    (with-unique-names (in)
      `(if (stringp ,seq)
         (with-input-from-string (,sym ,seq)
           ,@forms)
         (flexi-streams:with-input-from-sequence (,in ,seq)
           (let ((,sym (flexi-streams:make-flexi-stream ,in)))
           ,@forms))))))

(defmethod make-sequence-input-stream ((seq string))
  (make-string-input-stream seq))

(defmethod make-sequence-input-stream ((seq vector))
  (flexi-streams:make-flexi-stream (flexi-streams:make-in-memory-input-stream seq)))
