(cl:in-package :xdoc)

(defclass xml-input ()
  ((source :type stream
	   :initarg :source
	   :reader source)
   (line :type integer
	 :initform 1
	 :accessor line)
   (pos :type integer
	:initform 0
	:accessor pos)
   (buffer :type string
	   :initform (make-array 30 :element-type 'character :fill-pointer 0)
	   :reader buffer)
   (read-ptr :type integer
	     :initform 0
	     :accessor read-ptr)))

(defun getch (in)
  (if (= (read-ptr in) (length (buffer in)))
      (read-char (source in))
      (prog1 (aref (buffer in) (read-ptr in))
	(incf (read-ptr in)))))

(defun move-cursor (char in)
  (if (char= #\newline char)
      (setf (pos in) 0
	    (line in) (1+ (line in)))
      (incf (pos in))))

(defun get-char (in)
  (let ((char (getch in)))
    (move-cursor char in)
    char))

(defun flush-buffer (in)
  (let ((new (- (length (buffer in)) (read-ptr in))))
    (replace (buffer in) (buffer in)
	     :start1 0 :end1 new
	     :start2 (read-ptr in) :end2 (length (buffer in)))
    (setf (read-ptr in) 0
	  (fill-pointer (buffer in)) new)))

(defun fill-buffer (n in)
  (when (> n (- (array-total-size (buffer in)) (length (buffer in))))
    (flush-buffer in))
  (let ((count (- (length (buffer in)) (read-ptr in))))
    (when (> n count)
      (incf (fill-pointer (buffer in)) (- n count))
      (read-sequence (buffer in) (source in)
		     :start (+ (read-ptr in) count)
		     :end (+ (read-ptr in) n)))))

(defun peek-string (n in)
  (fill-buffer n in)
  (subseq (buffer in) (read-ptr in) (+ n (read-ptr in))))

(defun first-char (in)
  (if (= (read-ptr in) (length (buffer in)))
      (peek-char nil (source in))
      (char (buffer in) (read-ptr in))))

(defun discard (n in)
  (dotimes (i n)
    (get-char in)))
