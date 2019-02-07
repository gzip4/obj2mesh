
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "split-sequence")
  (ql:quickload "ieee-floats"))



(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))


(defun write-u32 (val stream)
  (write-byte (logand #xff (ash val 0)) stream)
  (write-byte (logand #xff (ash val -8)) stream)
  (write-byte (logand #xff (ash val -16)) stream)
  (write-byte (logand #xff (ash val -24)) stream)
  val)


(defun make-varr ()
  (make-array 1 :adjustable t :fill-pointer 0))


(defun vpush (elt vector)
  (vector-push-extend elt vector))


(defun parse-face (s)
  (let ((face (split-sequence:split-sequence #\Space s)))
    (when (/= 3 (length face))
      (error "wrong faces configuration"))
    (loop for vertex in face
       collect (loop for idx in (split-sequence:split-sequence #\/ vertex)
		    collect (1- (parse-integer idx))))))


(defun emit-face (face arr v vt vn)
  (loop for ($v $vt $vn) in face
     do (vpush
	 (list (aref v $v)
	       (aref vt $vt)
	       (aref vn $vn))
	 arr)))


(defun read-obj (obj-file)
  (with-open-file (f obj-file)
    (loop with v = (make-varr)
       with vt = (make-varr)
       with vn = (make-varr)
       with r = (make-varr)
       with pos = 0
       with count = nil
       for c1 = (read-char f nil)
       for c2 = (read-char f nil)
       while (and c1 c2)
       do (case c1
	    (#\o
	     (push (read-line f) count)
	     (push pos count)		; start vertex
	     (push 0 count))		; count vertex
	    (#\v
	     (case c2
	       (#\Space (vpush (list (read f) (read f) (read f)) v))
	       (#\t (vpush (list (read f) (read f)) vt))
	       (#\n (vpush (list (read f) (read f) (read f)) vn))))
	    (#\f
	     (incf (first count) 3)
	     (incf pos 3)
	     (let ((face (parse-face (read-line f))))
	       (emit-face face r v vt vn)))
	    (t
	     (read-line f nil) nil))
       finally (return (cons (nreverse count) r)))))
  

(defun write-plain-floats (bs data start count)
  (dotimes (i count)
    (let ((vertex (aref data (+ start i))))
      (loop for vkind in vertex
	 do (loop for x in vkind do (write-u32 (ieee-floats:encode-float32 x) bs)))))
  (values))
    

(defun write-obj (obj-file &key dir (ext ".mesh") (exporter 'write-plain-floats))
  (flet ((suff (fn) (concatenate 'string fn ext)))
    (let ((data (read-obj obj-file)))
      (setf dir (if dir (directory-namestring dir)
		    (directory-namestring obj-file)))
      (loop with lst = (car data)
	 for fn = (pop lst)
	 for start = (pop lst)
	 for count = (pop lst)
	 while fn
	 do (with-open-file (f (merge-pathnames (suff fn) dir)
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create
			       :element-type 'unsigned-byte)
	      (funcall exporter f (cdr data) start count))))))



;; try out GL_ELEMENT_ARRAY_BUFFER data

(defun %indexed-structure (a start count)
  (let ((ht (make-hash-table :test 'equal))
	(idx (make-varr))
	(out (make-varr)))
    (loop with n = 0
       for v across (subseq a start (+ start count))
       do (multiple-value-bind (i found)
	      (gethash v ht)
	    (if found
		(vpush i idx)
		(progn
		  (setf (gethash v ht) n)
		  (vpush n idx)
		  (vpush v out)
		  (incf n)))))
    (cons idx out)))
  

(defun %write-obj (obj-file &key path)
  (flet ((mesh (fn) (concatenate 'string fn ".mesh")))
    (let ((data (read-obj obj-file)))
      (setf path (if path (directory-namestring path)
		     (directory-namestring obj-file)))
      (loop with lst = (car data)
	 for fn = (pop lst)
	 for start = (pop lst)
	 for count = (pop lst)
	 while fn
	 collect (%indexed-structure (cdr data) start count)))))

