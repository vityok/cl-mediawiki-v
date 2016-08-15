;;; Dumps category contents and optionally conducts namespace
;;; translation of the listed articles

(ql:quickload :cl-mediawiki)

;; --------------------------------------------------------

(defparameter *api-url* "http://uk.wikipedia.org/w"
  "Root Wiki API URL used to query the server.")

(defparameter *default-file* "category-contents.txt")

;; --------------------------------------------------------

(defun dump-category (cmtitle &key
				(cmlimit 100)
				(out-stream nil))
  (wiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
				       :url *api-url*
				       :request-delay 10))
    (multiple-value-bind (list cmcontinue)
	(wiki:list-category-members cmtitle
				    :cmprop 'title
				    :cmlimit cmlimit)
      (dolist (row list)
	(format out-stream "~a~%" (cdr (find :title row :key #'car))))

      (loop :while cmcontinue
	 :do (multiple-value-bind (ll cc)
		 (wiki:list-category-members cmtitle
					     :cmprop 'title
					     :cmlimit cmlimit
					     :cmcontinue cmcontinue)
	       (dolist (row ll)
		 (format out-stream "~a~%" (cdr (find :title row :key #'car))))
	       (setf cmcontinue cc))))))

;; --------------------------------------------------------

(defun dump-category-to-file (cmtitle &key (file *default-file*))
  "Dumps contents of the given category into a file."
  (with-open-file (out file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (dump-category cmtitle :out-stream out)))

;; EOF
