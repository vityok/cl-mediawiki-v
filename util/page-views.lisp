;;; Uses the Wikimedia PageviewsAPI to query page popularity

;; See API documentation at: https://wikitech.wikimedia.org/wiki/Analytics/PageviewAPI
;;
;; See further documentation for the RUN function

(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :external-program)

;; --------------------------------------------------------

(defparameter *api-root* "http://wikimedia.org/api/rest_v1")

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
					 (end))

  (let ((url (format nil
		     "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/~a/all-access/user/~a/daily/~a/~a"
		     project
		     (drakma:url-encode (substitute #\_ #\Space article) :utf-8)
		     start
		     end))
	(metrics-tmp "metrics_tmp.json"))
    (format t "url: ~a~%" url)

    (external-program:run "/usr/bin/curl"
			  (list url "-o" metrics-tmp))

    ;; (drakma:http-request url :want-stream t)
    (with-open-file (stream metrics-tmp
			    :direction :input)
      ;; (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
      (let ((items (cdar (cl-json:decode-json stream))))
	(loop :for item :in items
	   :sum (cdr (find :views item :key #'car)))))))


(defun run ()
  "Runs article names from one file - `INPUT-FILE', and outputs aticle
name and total number of views in a tab-separated file `OUTPUT-FILE'."

  ;; One way to post-process data is to use SQLite:

  ;; 1) Create a table and set TAB as a separator:

  ;; sqlite> create table visits (name text, clicks int);
  ;; sqlite> .mode tabs

  ;; 2) Import data:

  ;; sqlite> .import article-views.txt visits

  ;; And the result is:

  ;; sqlite> select * from visits order by clicks limit 10;

  ;; http://stackoverflow.com/questions/26065872/how-to-import-a-tsv-file-with-sqlite3


  (let ((input-file "category-contents.txt")
	(output-file "article-views.txt")
	(start "2016070100")
	(end "2016073100"))
    (with-open-file (in input-file :direction :input)
      (with-open-file (out output-file :direction :output :if-exists :supersede)
	(loop for article = (read-line in nil)
	   while article do
	     (progn
	       (format out "~a	~a~%"
		       article
		       (get-article-page-views article :start start :end end))
	       (force-output out)
	       (sleep 15)))))))

;; EOF
