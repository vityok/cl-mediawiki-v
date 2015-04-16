;; Given a list of categories query all articles in them and compile a
;; list of missing articles (most popular red links)

;; This is somewhat similar to the missing topics tool, but works by
;; querying the Wikipedia server API and uses a dumped list of all
;; article titles

;; We are using Tokyo cabinet to store some dictionaries on the disk
;; because, for example, all page titles do not fit the memory very
;; well. Storing them on the disk prevents memory from being
;; exhausted.

;; Main entry functions:
;;
;; GENERATE-ARTICLES-QUEUE - based on the list of categories listed in
;; file specified by *CATEGORIES-FILE-NAME* generate a list of all
;; article titles that these categories contain
;;
;; PROCESS-ARTICLES-QUEUE - given the article titles queue, process it
;; and generate a dump of missing topics
;;
;; AGGREGATE-MISSING-TITLES - given the dump of missing topics
;; calculate the number of times a title is being missed.
;;
;; Main input data:
;;
;; - List of categories to process specified in the file with
;; *CATEGORIES-FILE-NAME* name

(ql:quickload :alexandria)
(ql:quickload :cl-tokyo-cabinet)
(ql:quickload :cl-mediawiki)

;; --------------------------------------------------------

(defparameter *categories-file-name* #P"categories.txt"
  "Contains the list of directories.")

(defparameter *all-titles-dump* "ukwiki-20150404-all-titles.txt"
  "A file with `List of all page' titles downloaded and ungzipped from the
  Wikipedia database dumps:
  https://dumps.wikimedia.org/backup-index.html")

(defparameter *api-url* "http://uk.wikipedia.org/w"
  "Root Wiki API URL used to query the server.")

(defparameter *articles-file-name* #P"articles-visited.txt"
  "Contains the list of visited articles.")

(defparameter *queue-file-name* #P"articles-queue.txt"
  "List of articles in the processing queue.")

(defparameter *missing-titles-file-name* #P"missing-titles-dump.txt"
  "Dump of missing titles.")

(defparameter *article-titles-db-file* "titles-dictionary.db"
  "File where the article titles dictionary DB is stored. See: *all-titles-dump*")

(defparameter *aggregation-threshold* 10
  "Minimal number of times an article must be missing to be included in the resulting table.")

(defparameter *missing-titles-map* "missing-titles-map.db"
  "Used as an out-of-memory map to calculate the number of times a
  title is being mentioned in the missing titles dump.")

(defvar *article-titles-db* nil
  "DB handle of the dictionary.")

(defvar *visited-articles-list* '())

;; --------------------------------------------------------

(defun load-list (fn)
  (with-open-file (in fn :direction :input)
    (loop for line = (read-line in nil)
       while line
       collect line)))

;; --------------------------------------------------------

(defun open-dict (dbf dictf)
  "If the dictionary DB does not exist, create and fill it. Returns
handle to the DB."
  (unless (probe-file dbf)
    ;; (b-tree:open dbf :type :string)
    (let ((db (make-instance 'tc:tc-bdb)))
      (tc:dbm-open db dbf :write :create)
      (with-open-file (in dictf :direction :input)
	(loop :for line = (read-line in nil)
	   :count line :into cnt
	   :when (= (rem cnt 10000) 0)
	   :do (format t "titles: ~a~%" cnt)
	   :while line
	   :do (tc:dbm-put db line "y")))
      (tc:dbm-close db)))
  (let ((db (make-instance 'tc:tc-bdb)))
    (tc:dbm-open db dbf :read)))

;; --------------------------------------------------------

(defun close-dict (db)
  (tc:dbm-close db))

;; --------------------------------------------------------

(defun dump-queue (queue)
  (with-open-file (fout *queue-file-name*
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (dolist (i queue)
      (format fout "~A~%" i))))

;; --------------------------------------------------------

(defun dump-missing-titles (titles)
  (when titles
    (with-open-file (fout *missing-titles-file-name*
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
      (dolist (i titles)
	(format fout "~A~%" i)))))

;; --------------------------------------------------------

(defun process-articles-queue ()
  "Given the article titles queue, process it and generate the list of
missing topics."
  (let ((queue (load-list *queue-file-name*))
	(dict (open-dict *article-titles-db-file* *all-titles-dump*))
	;; we will process articles in batches of this size
	(batch-size 15)
	(batch '()))

    (format t "laded Queue[~a] and the Dictionary~%" (length queue))
    (cl-mediawiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
						 :url *api-url*
						 :request-delay 10))
      (loop
	 if (and (< (length batch) batch-size)
		 (> (length queue) 0))
	 do (push (pop queue) batch)
	 else
	 do
	   (let ((njs (wiki:get-page-links batch :plnamespace 0 :pllimit 3500))
		 (missing-titles '()))
	     (format t "processing batch: ~a~%got queue length: ~a~%" batch (length queue))
	     ;; missing-titles holds the list of missing topics for
	     ;; this particular batch of articles
	     (dolist (page-links
		       ;; since we've got links from several pages, get all
		       ;; of them in separate lists
		       (map 'list
			    #'(lambda (x)
				(wiki:get-value (cdr x) :links))
			    njs))
	       ;; (format t "links from page: ~a~%" page-links)
	       (dolist (page-link page-links)
		 ;; page-links is a list of :NS and :TITLE tuples
		 (let ((link-title (substitute #\_ #\Space
					       (wiki:get-value page-link :title)
					       :test #'char=)))
		   (unless (tc:dbm-get dict link-title)
		     ;; this is a missing title from the titles dictionary
		     (format t "missing title: ~a~%" link-title)
		     (push link-title missing-titles)))))
	     (dump-missing-titles missing-titles)
	     (setf batch nil)
	     (dump-queue queue))
	 while (> (length queue) 0)))
    (close-dict dict)))

;; --------------------------------------------------------

(defun generate-articles-queue ()
  "Walks through the list of the categories and dumps all pages that
these categories contain."
  ;; initialize the dictionary of all titles
  (let ((categories-list (load-list *categories-file-name*))
	(titles-queue '()))
    (cl-mediawiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
						 :url *api-url*
						 :request-delay 10))
      (dolist (cat categories-list)
	;; query the list of articles in the directory cat and add it
	;; to queue of articles to process
	(let ((njs
	       (cl-mediawiki:list-category-members cat
						   :cmnamespace 0
						   :cmlimit 1000)))
	  (format t "~&new items: ~A~%" (map 'list
					     #'(lambda (x) (wiki:get-value x :title))
					     njs))
	  (dolist (i njs)
	    ;; go through the articles and filter out really new
	    ;; titles, duplicate articles should not be queued
	    (unless (find (wiki:get-value i :title) titles-queue :test #'string-equal)
	      (push (wiki:get-value i :title) titles-queue)))
	  ;; queue is dumped after every processed directory to
	  ;; enhance data persistence
	  (dump-queue titles-queue))))))

;; --------------------------------------------------------

(defun aggregate-missing-titles ()
  "Calculate the number of times a title is being missed."

  (unless (probe-file *missing-titles-map*)
    (let ((db (make-instance 'tc:tc-bdb)))
      (tc:dbm-open db *missing-titles-map* :write :create)
      (with-open-file (in *missing-titles-file-name* :direction :input)
	(loop :for line = (read-line in nil)
	   :while line
	   :for val = (tc:dbm-get db line)
	   :count line :into cnt
	   :when (= (rem cnt 10000) 0)
	   :do (format t "titles: ~a~%" cnt)
	   :do (if val
		   (tc:dbm-put db line (format nil "~a" (+ 1 (parse-integer val))) :mode :replace)
		   (tc:dbm-put db line "1" :mode :replace))))
      (tc:dbm-close db)))

  (let ((db (make-instance 'tc:tc-bdb))
	(missing-titles '())
	(ordered-titles '())
	(*aggregation-threshold* 10))

    (tc:dbm-open db *missing-titles-map* :read)
    (tc:with-iterator (it db)
      (tc:iter-first it)
      (loop for i = (tc:iter-next it)
	 while i
	 if (> (parse-integer (tc:iter-get it)) *aggregation-threshold*)
	 do (push (list (tc:iter-key it)
			(parse-integer (tc:iter-get it)))
		  missing-titles)))
    (tc:dbm-close db)
    (setf ordered-titles (sort missing-titles #'> :key #'second))

    (format t "missing titles over the threshold: ~a~% "
	    (length missing-titles))
    (with-open-file (out "report.txt"
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :supersede)
      (format out "{|~%!Кількість згадок!!Заголовок~%")
      (loop for line in ordered-titles do
	   (format out "|-~%!~a~%| [[~a]]~%" (second line)
		   (substitute #\Space #\_ (first line) :test #'char=)))
      (format out "|}"))))

;; EOF
