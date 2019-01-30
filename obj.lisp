
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "split-sequence")
  (ql:quickload "ieee-floats"))



(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))


(defun write-u32 (val stream)
  (loop for sh in '(0 -8 -16 -24)
     do (write-byte (logand #xff (ash val sh)) stream))
  val)


(defun write-float (x stream)
  (write-u32 (ieee-floats:encode-float32 x) stream))


(defun make-varr ()
  (make-array 1 :adjustable t :fill-pointer 0))


(defun vpush (elt vector)
  (vector-push-extend elt vector))


(defun parse-face (s)
  (let ((face (split-sequence:split-sequence #\Space s)))
    (when (/= 3 (length face))
      (error "wrong faces configuration, triangles allowed only"))
    (loop for vertex in face
       collect (loop for idx in (split-sequence:split-sequence #\/ vertex)
		    collect (parse-integer idx)))))


(defun emit-face (face arr v vt vn)
  (loop for ($v $vt $vn) in face
     do (progn
	  (vpush (aref v (1- $v)) arr)
	  (vpush (aref vt (1- $vt)) arr)
	  (vpush (aref vn (1- $vn)) arr))))


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
    (let ((idx (* 3 (+ start i))))
      (loop for x in (aref data (+ 0 idx)) do (write-float x bs))
      (loop for x in (aref data (+ 1 idx)) do (write-float x bs))
      (loop for x in (aref data (+ 2 idx)) do (write-float x bs))
      (values))))


(defun write-obj (obj-file &key path (exporter 'write-plain-floats))
  (flet ((mesh (fn) (concatenate 'string fn ".mesh")))
    (let ((data (read-obj obj-file)))
      (setf path (if path (directory-namestring path)
		     (directory-namestring obj-file)))
      (loop with lst = (car data)
	 for fn = (pop lst)
	 for b = (pop lst)
	 for len = (pop lst)
	 while fn
	 do (with-open-file (f (merge-pathnames (mesh fn) path)
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create
			       :element-type 'unsigned-byte)
	      (funcall exporter f (cdr data) b len))))))
