(in-package :cl-lmsensors)

(defun parse-coretemp-isa-alist (alist)
  (cons (car alist)
	(loop for data-line in (cdr alist)
	   when (listp (cdr data-line))
	   collect
	     (let ((name (car data-line))
		   crit-alarm
		   crit
		   max
		   input)
	       (loop for (key . value) in (cdr data-line)
		  do (cond ((scan "_crit_alarm$" key)
			    (setf crit-alarm value))
			   ((scan "_crit$" key)
			    (setf crit value))
			   ((scan "_max$" key)
			    (setf max value))
			   ((scan "_input$" key)
			    (setf input value))))
	       (list name
		     input
		     ;; status :ok :warn :critical
		     (cond ((and input max (<= input max)) :ok)
			   ((and input crit (<= input crit)) :warn)
			   ((and input crit (>  input crit)) :critical))
		     ;; percent of max
		     (when (and input max (not (zerop max)))
		       (* 100 (/ input max)))
		     ;; percent of critical
		     (when (and input crit (not (zerop crit)))
		       (* 100 (/ input crit)))
		     ;; percent of critical alarm.
		     (when (and input crit-alarm (not (zerop crit-alarm)))
		       (* 100 (/ input crit-alarm))))))))

(add-hardware-parsing-method "coretemp-isa"
			     #'parse-coretemp-isa-alist)
