;;; Dumps category contents and optionally conducts namespace
;;; translation of the listed articles

(in-package :cl-mediawiki-util)

;; --------------------------------------------------------

(defparameter *api-url* "http://uk.wikipedia.org/w"
  "Root Wiki API URL used to query the server.")

(defparameter *default-file* "category-contents.txt")

;; --------------------------------------------------------

(defun dump-category (cmtitle &key
				(cmlimit 300)
                                (cmnamespace nil)
				(out-stream nil))
  "Dumps articles (namespace 0) from a wiki category to the given
output stream."

  (wiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
				       :url *api-url*
				       :request-delay 10))
    (multiple-value-bind (list cmcontinue)
	(wiki:list-category-members cmtitle
				    :cmprop 'title
				    :cmlimit cmlimit
                                    :cmnamespace cmnamespace)
      (dolist (row list)
	(format out-stream "~a~%" (cdr (find :title row :key #'car))))

      (loop :while cmcontinue
	 :do (multiple-value-bind (ll cc)
		 (wiki:list-category-members cmtitle
					     :cmprop 'title
					     :cmlimit cmlimit
                                             :cmnamespace cmnamespace
					     :cmcontinue cmcontinue)
	       (dolist (row ll)
		 (format out-stream "~a~%" (cdr (find :title row :key #'car))))
	       (setf cmcontinue cc))))))

;; --------------------------------------------------------

(defun dump-category-to-file (cmtitle &key (file *default-file*) (append nil))
  "Dumps contents of the given category into a file."
  (with-open-file (out file
		       :direction :output
		       :if-exists (if append :append :supersede)
		       :if-does-not-exist :create)
    (dump-category cmtitle :out-stream out)))

;; --------------------------------------------------------

(defun dump-categories-to-file (&key
                                  (categories-file *categories-file-name*)
                                  (to-file *default-file*))
  "Reads a simple newline-separated list of categories from the given
`categories-file' and dumps articles from them into the `to-file'
file.

Doesn't check for duplicates, so some post-processing is necessary."

  (let* ((categories (load-list categories-file))
         (categories-count (length categories))
         (processed-count 1))
    (with-open-file (out to-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)

      (dump-category (pop categories) :cmnamespace "0" :out-stream out)
      (dolist (category categories)
        (dump-category category :cmnamespace "0" :out-stream out)
        (format t "processed: ~a out of ~a~%" (incf processed-count) categories-count)
        (sleep 10)))))

;; EOF
