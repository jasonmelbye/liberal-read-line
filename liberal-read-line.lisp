(in-package :cl-user)
(defpackage :liberal-read-line
  (:use :cl)
  (:export :liberal-read-line))

(in-package :liberal-read-line)

;; A liberal read-line reads characters until encountering any
;; sequence of line ending characters, or end of file. The following
;; characters are taken to be line ending characters: #\newline,
;; #\return, #\linefeed (CLHS 13.1.7). Consecutive line ending
;; characters will be taken to be one line ending. A downside to this
;; approach is that if consecutive empty lines matter, they will be
;; processed by this read-line as a single line ending.

(defparameter *line-endings* '(#\newline #\return #\linefeed))

(defun consume-line-ending-chars (stream)
  (loop for p = (peek-char nil stream nil nil)
     while (member p *line-endings*)
     do (read-char stream nil nil)))

(defun liberal-read-line (&optional (stream *standard-input*)
			    (eof-error-p t) eof-value recursive-p)
  (let ((reached-eof nil))
    (let ((chars (loop for c = (read-char stream nil nil recursive-p)
		    ; Until eof (nil) or end char
		    until (member c (cons nil *line-endings*))
		    collect c
		    finally (if (null c) (setf reached-eof t)))))
      (let ((line (coerce chars 'string)))
	(if reached-eof
	    (if chars
		(values line t)
		(if eof-error-p
		    (error 'end-of-file-reached)
		    (values eof-value t)))
	    (progn
	      (consume-line-ending-chars stream)
	      (values line nil)))))))
