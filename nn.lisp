(defconstant e 2.718281828459045d0)


(defun lastcar (l)
  (car (last l)))
;;layer:
;;      node_1
;;      ...
;;      node_n

;;node:
;;      weight_input_1 ----summ-func-
;;      ...              /
;;      weight_input_n -/


;; inputs -> node -> value
;; (defun calc-node (inputs node)
;;   (map * ))

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

(defgeneric value (node input))
(defmethod value ((n node) input)
  (let ((in (funcall (node-func n) input)))
    (mapcar (lambda (x) (* x in))
	    (node-weights n))))

(defun make-input-layer (nodes outputs)
  (loop for i from 1 to nodes appending (list (make-node outputs 'identity))))

(defun make-layer (nodes outputs &optional (function 'tanh))
  (loop for i from 1 to nodes 
	appending (list 
		   (make-node outputs function))))

(defun make-nn (&rest layers)
  (if (< (length layers) 3) nil
    (append (list (make-input-layer (car layers) (cadr layers))
		  (loop with l = (cdr layers)
			while (not (null (cdr l)))
			appending (make-layer (car l) (cadr l))
			do (setf l (cdr l))))
	    (list (make-layer (lastcar layers) (lastcar layers))))))
			  
		    

;	      (list (make-layer (lastcar layers) (lastcar layers) 'identity))))))

(defun calc-layer (layer inputs)
  (apply 'mapcar '+
	 (mapcar 'value
		 layer 
		 inputs)))

(defun calc-nn (nn inputs)
(  (calc-layer (car nn) inputs))
(defun sub-nn (nn start &optional (end (length nn)))
  (subseq nn start end))


(defun calc-error (outputs target)
  (reduce '+ (mapcar (lambda (ot tg) (expt (- ot tg) 2))
		     outputs
		     target)))

(defun activate (nn &rest inputs)
  (calc-nn nn inputs))
