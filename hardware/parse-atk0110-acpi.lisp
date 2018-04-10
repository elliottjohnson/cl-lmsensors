(in-package :cl-lmsensors)

(defun parse-atk0110-acpi-alist (alist)
  (cons (car alist)
	(loop for data-line in (cdr alist)
	   when (listp (cdr data-line))
	   collect
	     (let ((name (car data-line))
		   crit
		   max
		   min
		   input)
	       (loop for (key . value) in (cdr data-line)
		  do (cond ((scan "_crit$"  key)
			    (setf crit  value))
			   ((scan "_max$"   key)
			    (setf max   value))
			   ((scan "_input$" key)
			    (setf input value))
			   ((scan "_min$"   key)
			    (setf min   value))))
	       (list name
		     input
		     ;; status :ok :warn or :critical
		     (cond ((and input max min (<= min input max)) :ok)
			   ((and input min) (<= input min) :warn)
			   ((and input max (>= input max) (if crit :warn :critical)))
			   ((and input crit (>= input crit)) :critical))
		     ;; percent of max.. or optionally percent between min and max.
		     (cond ((and min input max (not (zerop (- max min))))
			    (* 100 (/ (- input min) (- max min))))
			   ((and input max (not (zerop max)))
			    (* 100 (/ input max))))
		     ;; percent of crit
		     (when (and input crit (not (zerop crit)))
		       (* 100 (/ input crit))))))))

(add-hardware-parsing-method "atk0110-acpi"
			     #'parse-atk0110-acpi-alist)
