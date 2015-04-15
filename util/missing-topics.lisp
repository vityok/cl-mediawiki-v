;; Given a list of categories query all articles in them and compile a
;; list of missing articles (most popular red links)

;; This is somewhat similar to the missing topics tool, but works by
;; querying the Wikipedia server API and uses a dumped list of all
;; article titles

;; We are using Tokyo cabinet to store some dictionaries on the disk
;; because, for example, all page titles do not fit the memory very
;; well. Storing them on the disk prevents memory from being
;; exhausted.

(ql:quickload :cl-ppcre)
(ql:quickload :log5)
(ql:quickload :alexandria)
(ql:quickload :cl-tokyo-cabinet)


(log5:defcategory :debug)
(log5:start-sender 'warnings-and-worse
 		   (log5:stream-sender :location *standard-output*)
 		   :category-spec '(log5:dribble+)
 		   :output-spec '(log5:time log5:message log5:context))

(ql:quickload :cl-mediawiki)

;; --------------------------------------------------------


(defparameter *categories-file-name* #P"categories.txt"
 "Contains the list of directories.")
(defparameter *articles-file-name* #P"articles.txt"
  "Contains the list of visited articles.")
(defparameter *queue-file-name* #P"queue.txt"
  "List of articles in the processing queue.")
(defparameter *missing-titles-file-name* #P"missing-titles.txt"
  "List of missing titles.")
(defparameter *all-titles-dump* "ukwiki-20150404-all-titles.txt")
(defparameter *article-titles-db-file* "titles.db"
  "File where the article titles dictionary DB is stored.")
(defvar *article-titles-db* nil
  "DB handle of the dictionary.")

(defvar *categories-list* '())
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

(defun get-namespace-id (ns)
  "Given canonical namespace name NS query namespaces list from API
and find the one with the given name. Returns ID of the found
namespace."
  (cl-mediawiki:with-mediawiki ("http://uk.wikipedia.org/w")
    (let ((nslist
	   (get-value (cl-mediawiki:siteinfo :siprop :namespaces)
			   :namespaces)))
      (get-value
       (find-if
	#'(lambda (x)
	    (string-equal ns
			  (get-value x #'cdr :canonical)))
	nslist)
       #'cdr :id))))

;; --------------------------------------------------------

(defun get-subcats-titles (cat &key (ns 14))
  (cl-mediawiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
				       :url "http://uk.wikipedia.org/w"
				       :request-delay 10))
    (multiple-value-bind (js cont)
	(cl-mediawiki:list-category-members cat :cmnamespace ns)
      (let ((res (map 'list
		      #'(lambda (x) (get-value x :title))
		      js)))
	(loop (when (null cont) (return))
	  (multiple-value-bind (njs ncont)
	      (cl-mediawiki:list-category-members cat
					  :cmnamespace ns
					  :cmcontinue cont
					  :cmlimit 500)
	    (setf cont ncont)
	    (format t "~&continue: ~A~%" cont)
	    (format t "~&new items: ~A~%" (map 'list
			       #'(lambda (x) (get-value x :title))
			       njs))
	    (format t "~&current res: ~A~%" res)
	    (setf res
		  (append res
			  (map 'list
			       #'(lambda (x) (get-value x :title))
			       njs)))))
	res))))
;; (get-subcats-titles "Категорія:Математика")

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

(defun walk-tree (&optional
		  (root-category *default-root-category*)
		  (queue (get-subcats-titles root-category)))
  (dump-queue queue)
  (setf *black-list* (load-list *black-list-file-name*))
  (loop for i = (pop queue)
     while i
     do (progn
	  (format t "~&queue contains ~A items~%" (length queue))
	  (format t "~&list contains ~A items~%" (length *categories-list*))
	  (unless (or (find i *black-list* :test #'string-equal)
		      (find i *categories-list* :test #'string-equal))
	    (format t "~&[[~A]] is a new item~%" i)
	    (push i *categories-list*)
	    (with-open-file (fout *categories-file-name*
				  :direction :output
				  :if-exists :append
				  :if-does-not-exist :create)
	      (format fout "~A~%" i))
	    (setf queue
		  (append queue
			  (get-subcats-titles i)))
	    (dump-queue queue)))))

;; --------------------------------------------------------

(defun continue-walk-tree ()
  "Given the article titles queue, process it and generate the list of
missing topics."
  (let ((queue (load-list *queue-file-name*))
	(dict (open-dict *article-titles-db-file* *all-titles-dump*))
	;; we will process articles in batches of this size
	(batch-size 15)
	(batch '()))

    (format t "laded Queue[~a] and the Dictionary~%" (length queue))
    (cl-mediawiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
						 :url "http://uk.wikipedia.org/w"
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

(defun start-walk-tree ()
  ;; initialize the dictionary of all titles
  (close-dict (open-dict *article-titles-db-file* *all-titles-dump*))
  (setf *categories-list* (load-list *categories-file-name*))

  (let ((titles-queue '()))
    (cl-mediawiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
						 :url "http://uk.wikipedia.org/w"
						 :request-delay 10))
      (dolist (cat *categories-list*)
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
    
  (unless (probe-file "topics-hash.db")
    (let ((db (make-instance 'tc:tc-bdb)))
      (tc:dbm-open db "topics-hash.db" :write :create)
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
	(threshold 10))
    
    (tc:dbm-open db "topics-hash.db" :read)
    (tc:with-iterator (it db)
      (tc:iter-first it)
      (loop for i = (tc:iter-next it)
	 while i
	 if (> (parse-integer (tc:iter-get it)) threshold)
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

;; --------------------------------------------------------

(defun get-cat-name (cat)
  (subseq cat (length "Категорія:")))

;; --------------------------------------------------------

(defun gen-page ()
  (let ((categories-list (load-list
			  *categories-file-name*)) ; all categories we
					; know about
	(categories-dict (make-hash-table)) ; categories in the hash
					; table for their leading
					; character
	(categories-keys '())	  ; essentially this is section titles
	)
    (dolist (i categories-list)
      (let ((first-char (elt (get-cat-name i) 0)))
	(push i (gethash first-char categories-dict))
	))
    (setf categories-keys (alexandria:hash-table-keys categories-dict))
    (setf categories-keys (sort categories-keys #'char-lessp))
    (with-open-file (out #P"page.wiki"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "Наведений нижче список містить [[Вікіпедія:Категорії|категорії]] на тему [[Математика]].")
      (format out "  Кількість категорій в списку: ~A.~%" (length categories-list))
      (format out "~%{{АБВ}}~%~%")
      (format out "== Список ==")
      (dolist (k categories-keys)
	(let* ((v (gethash k categories-dict))
	       (sorted-values (reverse v)))
	  ;;(sort sorted-values #'string>)
	  (format t "~A: ~A~%" k (length v))
	  (format out "~&~%=== ~A ===~%" k)
	  (format out "~&~{[[:~A|~A]]~^&nbsp;— ~}~%"
		  (loop for n in sorted-values
		     appending (list n) into pairs
		     appending (list (get-cat-name n)) into pairs
		     finally (return pairs)))))
      (format out "~%== Дивіться також ==
{{Портал математика}}
* [[Вікіпедія:Проект:Математика|Проект математика на Вікіпедії]]

[[Категорія:Математика]]
[[Категорія:Математичні списки|*]]
"))))

;; EOF
