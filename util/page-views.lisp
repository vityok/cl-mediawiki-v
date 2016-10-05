;;; Uses the Wikimedia PageviewsAPI to query page popularity

;; See API documentation at: https://wikitech.wikimedia.org/wiki/Analytics/PageviewAPI
;;
;; See further documentation for the RUN function

(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :external-program)
(ql:quickload :cl-string-match)
(ql:quickload :split-sequence)

;; --------------------------------------------------------

(defparameter *api-root* "http://wikimedia.org/api/rest_v1")
(defparameter *discussion-prefix* "Обговорення:"
  "A prefix used to name discussion articles/pages.")
(defparameter *views-file* "article-views.txt"
  "File where raw numbers are stored")

;; --------------------------------------------------------

;; GET /metrics/pageviews/per-article/{project}/{access}/{agent}/{article}/{granularity}/{start}/{end}

;; project

;; If you want to filter by project, use the domain of any Wikimedia
;; project, for example 'en.wikipedia.org', 'www.mediawiki.org' or
;; 'commons.wikimedia.org'. If you are interested in all pageviews
;; regardless of project, use all-projects.

;; access: all-access, desktop, mobile-web, mobile-app

;; If you want to filter by access method, use one of desktop,
;; mobile-app or mobile-web. If you are interested in pageviews
;; regardless of access method, use all-access

;; agent: all-agents, user, spider, bot

;; If you want to filter by agent type, use one of user, bot or
;; spider. If you are interested in pageviews regardless of agent
;; type, use all-agents

;; article

;; The title of any article in the specified project. Any spaces
;; should be replaced with underscores. It also should be URI-encoded,
;; so that non-URI-safe characters like %, / or ? are
;; accepted. Example: Are_You_the_One%3F

;; granularity: daily

;; The time unit for the response data. As of today, the only
;; supported granularity for this endpoint is daily

;; start

;; The date of the first day to include, in YYYYMMDD format

;; end

;; The date of the last day to include, in YYYYMMDD format

;; Example:

;; http://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/de.wikipedia/all-access/user/Johann_Wolfgang_von_Goethe/daily/2015101300/2015102700

(defun get-article-page-views (article &key
					 (project "uk.wikipedia")
					 (start)
					 (end)
					 (attempts 3))

  (let ((url (format nil
		     "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/~a/all-access/user/~a/daily/~a/~a"
		     project
		     (drakma:url-encode (substitute #\_ #\Space article) :utf-8)
		     start
		     end))
	(metrics-tmp "metrics_tmp.json"))
    (format t "url: ~a~%" url)

    (flet ((attempt ()
	     (external-program:run "/usr/bin/curl"
				   (list url "-o" metrics-tmp))

	     ;; (drakma:http-request url :want-stream t)
	     (with-open-file (stream metrics-tmp
				     :direction :input)
	       ;; (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
	       (let ((json (cl-json:decode-json stream)))

		 (unless (string= (cdr (assoc :type json))
				  "https://restbase.org/errors/query_error")
		   (let ((items (cdar json)))
		     (loop :for item :in items
			:sum (cdr (find :views item :key #'car)))))))))

      (loop for i from 0 to attempts
	 for views = (attempt)
	 when views return views
	 do (progn
	      (format t "doing another attempt~%")
	      (sleep 15))))))

;; --------------------------------------------------------

(defun run-page-views (&key
			 (start "2016090100")
			 (end "2016093000"))

  "Runs article names from one file - `INPUT-FILE', and outputs aticle
name and total number of views in a tab-separated file `OUTPUT-FILE'."

  ;; One way to post-process data is to use SQLite:

  ;; 1) Create a table and set TAB as a separator:

  ;; sqlite> create table visits (name text, clicks int);
  ;; sqlite> .mode tabs

  ;; 2) Import data:

  ;; sqlite> .import article-views.txt visits

  ;; And the result is:

  ;; sqlite> SELECT * FROM visits ORDER BY clicks DESC LIMIT 100;

  ;; http://stackoverflow.com/questions/26065872/how-to-import-a-tsv-file-with-sqlite3


  (let ((input-file "category-contents.txt")
	(output-file *views-file*)
	(article-no 1))

    (with-open-file (in input-file :direction :input)
      (with-open-file (out output-file :direction :output :if-exists :supersede)
	(loop for article = (read-line in nil)
	   while article do
	     (when (sm:prefixed-with article *discussion-prefix*)
	       (format t "~a " article-no)
	       (let* ((article-name (subseq article (length *discussion-prefix*)))
		      (article-views (get-article-page-views article-name :start start :end end)))

		 (format out "~a	~a~%"
			 article-name
			 article-views)
		 (force-output out)
		 (incf article-no)
		 (sleep 15))))))))

;; --------------------------------------------------------

(defun report-page-views (&key (limit 100))
  "Create a wiki table listing top articles."

  (with-open-file (in *views-file*
		      :direction :input)
    (let* ( ;; SELECT
	   (all-articles
	    (loop for line = (read-line in nil)
	       while line
	       for (name visits-str) = (split-sequence:split-sequence #\Tab line)
	       collect `(,name ,(parse-integer visits-str))))

	   ;; ORDER: sort breaks all-articles
	   (sorted-articles (sort all-articles #'> :key #'second))
	   ;; LIMIT
	   (top-articles  (subseq sorted-articles 0 limit))

	   ;; TOTALS
	   (total-views (reduce #'+ sorted-articles :key #'second :initial-value 0))
	   (top-views (reduce #'+ top-articles :key #'second :initial-value 0)))

      ;; REPORT
      (format t "На момент аналізу проект мав ~a статей, які були переглянуті ~,,' ,:D раз. Перелічені нижче Топ-100 статей були переглянуті в сумі ~,,' ,:D раз, що складає ~a% від переглядів всіх статей проекту.~%~%"
	      (length sorted-articles)
	      total-views
	      top-views
	      (round (* (/ top-views total-views) 100)))
      (format t "{|~%! Рейтинг || Стаття || Кількість переглядів~%|-~%")
      (loop for (article-name article-views) in top-articles
	   for num from 1 upto 100
	 do (format t "| ~a || [[~a]]~1,64T || ~a ~%|-~%" num article-name article-views))
      (format t "|}"))))

;; EOF
