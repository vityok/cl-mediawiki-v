;;; Uses the Wikimedia PageviewsAPI to query page popularity

;; See API documentation at: https://wikitech.wikimedia.org/wiki/Analytics/PageviewAPI
;;
;; See further documentation for the RUN-PAGE-VIEWS function

(in-package :cl-mediawiki-util)

;; --------------------------------------------------------

(defparameter *api-root* "http://wikimedia.org/api/rest_v1")
(defparameter *discussion-prefix* "Обговорення:"
  "A prefix used to name discussion articles/pages.")
(defparameter *views-file* "article-views.txt"
  "File where raw numbers are stored")

;; --------------------------------------------------------

(defun get-prev-month-start ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~d~2,'0D0100" year (- month 1))))

(defun get-prev-month-end ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~d~2,'0D0100" year month)))

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

(defun get-query-url (project article start end)
  "URL-encode article name and combine it with the pattern to produce
the URL for querying daily views."

  (format nil
	  "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/~a/all-access/user/~a/daily/~a/~a"
	  project
	  (drakma:url-encode (substitute #\_ #\Space article) :utf-8)
	  start
	  end))

;; --------------------------------------------------------

(defun attempt-article-page-views (project article start end)
  "Actually sends HTTP request, parses JSON response and calculates
the total views, minimum, maximum and median views per day in a list."

  (let ((url (get-query-url project article start end))
	(metrics-tmp "metrics_tmp.json"))
    (format t "url: ~a~%" url)

    (external-program:run "/usr/bin/curl"
			  (list url "-o" metrics-tmp))

    ;; (drakma:http-request url :want-stream t)
    (with-open-file (stream metrics-tmp
			    :direction :input)
      ;; (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
      (let ((json (cl-json:decode-json stream)))
	(when (listp json)
	  (cond
            ((string= (cdr (assoc :type json))
                      "https://restbase.org/errors/query_error")
             (error "Query error, check the metrics_tmp.json file for clues"))

            ((string= (cdr (assoc :type json))
                      "https://restbase.org/errors/not_found")
             ;; very likely this article didn't exist yet in the
             ;; specified time period, just report as no views at all
             (list 0
                   0
                   0
                   0.0))

            (t
             ;; article data is present, do the math
             (let* ((items (cdar json))
                    (daily-views (map 'list
                                      #'(lambda (item)
                                          (cdr (find :views item :key #'car)))
                                      items)))
               ;; collect and then calculate sum and median
               (list (reduce #'+ daily-views)
                     (apply #'min daily-views)
                     (apply #'max daily-views)
                     (float (alexandria:median daily-views)))))))))))

;; --------------------------------------------------------

(defun get-article-page-views (article &key
					 (project "uk.wikipedia")
					 (start)
					 (end)
					 (attempts 3))

  (loop :for i :from 0 :to attempts
     :for views = (attempt-article-page-views project article start end)
     :when views :return views
     :do
     (progn
       (format t "doing another attempt~%")
       (sleep 15))
     :finally
     (progn
       (format t "failed to get information for: [[~a]]~%" article))))

;; --------------------------------------------------------

(defun run-page-views (&key
			 (start (get-prev-month-start))
			 (end (get-prev-month-end))
                         (articles-file *default-file*))

  "Runs article names from one file - `INPUT-FILE', and outputs aticle
name and total number of views in a tab-separated file `OUTPUT-FILE'.

In order to obtain articles for a given category use
`DUMP-CATEGORY-TO-FILE' function.

Call `REPORT-PAGE-VIEWS' to get the summary table."

  ;; One way to post-process data is to use SQLite:

  ;; 1) Create a table and set TAB as a separator:

  ;; sqlite> create table visits (name text, clicks int);
  ;; sqlite> .mode tabs

  ;; 2) Import data:

  ;; sqlite> .import article-views.txt visits

  ;; And the result is:

  ;; sqlite> SELECT * FROM visits ORDER BY clicks DESC LIMIT 100;

  ;; http://stackoverflow.com/questions/26065872/how-to-import-a-tsv-file-with-sqlite3

  ;; Another is to call REPORT-PAGE-VIEWS function

  (let ((output-file *views-file*)
	(article-no 1))

    (with-open-file (in articles-file :direction :input)
      (with-open-file (out output-file :direction :output :if-exists :supersede)
	(loop for article = (read-line in nil)
	   while article do
	     (when (sm:prefixed-with article *discussion-prefix*)
	       (format t "~a " article-no)
	       (let* ((article-name (subseq article (length *discussion-prefix*)))
		      (article-views (get-article-page-views article-name :start start :end end)))

		 (format out "~a~{	~a~}~%"
			 article-name
			 article-views)
		 (force-output out)
		 (incf article-no)
		 (sleep 15))))))))

;; --------------------------------------------------------

(defun report-page-views (&key (limit 100))
  "Create a wiki table listing top articles based on data produced by
`RUN-PAGE-VIEWS'."

  (with-open-file (in *views-file*
		      :direction :input)
    (let* ( ;; SELECT
	   (all-articles
	    (iter
              (for line = (read-line in nil))
              (while line)
              (for (name sum-str min-str max-str med-str) = (split-sequence:split-sequence #\Tab line))
              (when sum-str
                (for sum-val = (parse-integer sum-str))
                (for min-val = (parse-integer min-str))
                (for max-val = (parse-integer max-str))
                (for med-val = (parse-float:parse-float med-str))
                (when sum-val
                  (collect `(,name ,sum-val ,min-val ,max-val ,med-val))))))

	   ;; ORDER: sort breaks all-articles
	   (sorted-articles (sort all-articles #'> :key #'second))
	   ;; LIMIT
	   (top-articles  (subseq sorted-articles 0
                                  ;; in case if there are less
                                  ;; articles than the limit
                                  (min limit
                                       (length sorted-articles))))

	   ;; TOTALS
	   (total-views (reduce #'+ sorted-articles :key #'second :initial-value 0))
	   (top-views (reduce #'+ top-articles :key #'second :initial-value 0)))

      ;; REPORT
      (format t "На момент аналізу проект мав ~a статей, які були переглянуті {{formatnum:~D}} раз. Перелічені нижче Топ-100 статей були переглянуті в сумі {{formatnum:~D}} раз, що складає ~a% від переглядів всіх статей проекту.~%~%"
	      (length sorted-articles)
	      total-views
	      top-views
	      (round (* (/ top-views total-views) 100)))
      (format t "{|
! rowspan=2 | Рейтинг
! rowspan=2 | Стаття
! colspan=4 | Кількість переглядів
|-
! Всього
! мін
! макс
! Медіана
|-~%")
      (loop for (article-name sum-views min-views max-views med-views) in top-articles
         for num from 1 upto (length top-articles)
	 do (format t "| ~a || [[~a]]~1,64T || ~a || ~a || ~a || ~a~%|-~%"
                    num article-name sum-views min-views max-views (round med-views)))
      (format t "|}"))))

;; EOF
