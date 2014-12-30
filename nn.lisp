(defconstant e 2.718281828459045d0)


(defun lastcar (l)
  (car (last l)))

(defun make-node (n-outputs &optional (function 'tanh))
  (make-instance 'node
		 :weights (loop for i from 1 to n-outputs
				appending (list (random 1.0d0)))
		 :func function))
(defclass node ()
  ((func :accessor node-func
	 :initform 'tanh
	 :initarg :func)
   (weights :accessor node-weights
	    :initform 'nil
	    :initarg :weights)))

(defun make-net (&rest layer-spec)
  (make-instance 'nnet :layers (make-nn layer-spec)))
				  
(defgeneric value (node input))
(defmethod value ((n node) input)
  (let ((in (funcall (node-func n) input)))
    (mapcar (lambda (x) (* x in))
	    (node-weights n))))

(defclass nnet ()
  ((layers :initarg :layers
	   :accessor net-layers)))

(defgeneric activate (nnet inputs))

(defgeneric nth-layer (nnet n))

(defgeneric input-count (nnet))
(defgeneric node-count (nnet layer))
(defgeneric layer-count (nnet))
(defgeneric subnet (nnet start &optional end))

(defmethod activate ((net nnet) inputs)
  (calc-nn (net-layers net) inputs))

(defmethod nth-layer ((net nnet) n)
  (nth n (net-layers net)))

(defmethod node-count ((net nnet) layer)
  (length (nth layer (net-layers net))))

(defmethod input-count ((net nnet))
  (node-count net 0))

(defmethod layer-count ((net nnet))
  (length (slot-value net 'layers)))


(defmethod subnet ((net nnet) start &optional (end (layer-count net)))
  (sub-nn (net-layers net) start end))


(defun make-input-layer (nodes outputs)
  (loop for i from 1 to nodes appending (list (make-node outputs 'identity))))

(defun make-layer (nodes outputs &optional (function 'tanh))
  (loop for i from 1 to nodes 
	appending (list 
		   (make-node outputs function))))

(defun make-nn (layers)
  (if (find 0 layers) (format t "Cannot make network with 0 nodes in a layer.~%")
    (if (< (length layers) 3) nil
      (append (list (make-input-layer (car layers) (cadr layers)))
	      (loop with l = (cdr layers)
		    while (not (null (cdr l)))
		    appending (list (make-layer (car l) (cadr l)))
		    do (setf l (cdr l)))
	      (list (make-layer (lastcar layers) (lastcar layers)))))))

(defun calc-layer (inputs layer)
  (apply 'mapcar '+
	 (mapcar 'value
		 layer 
		 inputs)))

(defun calc-nn (nn inputs)
  (reduce 'calc-layer nn :initial-value inputs))
  
(defun sub-nn (nn start &optional (end (length nn)))
  (subseq nn start end))

(defun calc-error (outputs target)
  (reduce '+ (mapcar (lambda (ot tg) (expt (- ot tg) 2))
		     outputs
		     target)))

(defun delta-n (l n delta)
  (incf (nth n l) delta))

(defun partial-derivatives (nn inputs delta)
  (loop for i from 0 to (length inputs)
	
