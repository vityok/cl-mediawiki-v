;;; Uses the Wikimedia PageviewsAPI to query page popularity

;; See API documentation at: https://wikitech.wikimedia.org/wiki/Analytics/PageviewAPI
;;
;; See further documentation for the RUN-PAGE-VIEWS function
;;
;; TODO: Current approach doesn't take into account Redirects, only
;; direct hits at the article are included in the rating

;; Original Web UI uses the following request to the Wikipedia server
;; to obtain the list of redirects for a given article:

;; /**
;;   * Get all redirects of a page
;;   * @param  {String} pageName - name of page we want to get data about
;;   * @return {Deferred} - Promise resolving with redirect data
;;   */
;;  getRedirects(pageName) {
;;    const dfd = $.Deferred();
;;
;;    const promise = $.ajax({
;;      url: `https://${this.project}.org/w/api.php`,
;;      jsonp: 'callback',
;;      dataType: 'jsonp',
;;      data: {
;;        action: 'query',
;;        format: 'json',
;;        formatversion: 2,
;;        prop: 'redirects',
;;        rdprop: 'title|fragment',
;;        rdlimit: 500,
;;        titles: pageName
;;      }
;; });

(in-package :cl-mediawiki-util)

;; --------------------------------------------------------

(defparameter *api-root* "http://wikimedia.org/api/rest_v1")

(defparameter *discussion-prefix* "Обговорення:"
  "A prefix used to name discussion articles/pages.")

(defparameter *category-prefix* "Категорія:")

(defparameter *views-file* "article-views.txt"
  "File where raw numbers are stored")

(defparameter *page-views-query-interval* 1
  "Time in seconds to wait between consequtive requests to the server
  while in the `RUN-PAGE-VIEWS'.")

(defparameter *curl* "/usr/bin/curl"
  "curl program location.")

;; --------------------------------------------------------

(defun get-prev-month-start ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore second minute hour date day daylight-p zone))
    (if (= month 1) ;; january
        (format nil "~d~2,'0D0100" (- year 1) 12)
        (format nil "~d~2,'0D0100" year (- month 1)))))

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

  ;; URL/URI is constructed in two steps to avoid the flaw in the
  ;; PURI:URI code (initialize-instance :after uri):
  ;; https://github.com/archimag/puri-unicode/commit/212df232823ddcb8a217a3adf09af3cff9704cd6
  (let ((url (puri:uri "https://wikimedia.org")))
    (setf (puri:uri-path url)
          (format nil
                  "/api/rest_v1/metrics/pageviews/per-article/~a/all-access/user/~a/daily/~a/~a"
                  project
                  (drakma:url-encode (substitute #\_ #\Space article) :utf-8)
                  start
                  end))
    url))

;; --------------------------------------------------------

(defun attempt-article-page-views (project article start end)
  "Actually sends HTTP request, parses JSON response and calculates
the total views, minimum, maximum and median views per day in a list.

Returns NIL when failed to retrieve data for any reason."

  ;; mount -t tmpfs -o size=20M /dev/shm /home/victor/tmpfs/

  (let* ((url (format nil "~a" (get-query-url project article start end)))
         (result nil)
         (default-result (list 0 0 0 0.0)))

    (log-for trace "url: ~a" url)

    ;; TODO: is it possible to reuse the open socket for querying the
    ;; server? but keep in mind that the implementation must be
    ;; thread-safe
    (handler-case
	;; (multiple-value-bind (data-stream status-code headers uri)

	(let ((proc (sb-ext:run-program "/usr/bin/curl"
					`("--retry" "5" ,url)
					:output :stream
					:error nil
					:wait nil))
	      (status-code 0))
	  ;;   (drakma:http-request url
	  ;; 			 :external-format-out :UTF-8
	  ;; 			 :external-format-in :UTF-8
	  ;; 			 :want-stream T
	  ;; 			 :close T)
	  ;; (declare (ignore headers)
	  ;; 	   (ignore uri))

	  (cond			      ; DISPATCH STATUS CODES
	    (proc		      ;; (= status-code 200)	; HTTP OK

             (let ((json (cl-json:decode-json (sb-ext:process-output proc))))
	       (when (listp json)
		 (setf result
		       (cond
			 ((or (string= (cdr (assoc :type json))
				       "https://restbase.org/errors/query_error")
			      (string= (cdr (assoc :type json))
				       "https://mediawiki.org/wiki/HyperSwitch/errors/not_found"))
			  (log-for error "query error or article not found {~a}" article)
			  default-result)

			 ((string= (cdr (assoc :type json))
				   "https://restbase.org/errors/not_found")
			  ;; very likely this article didn't exist yet in the
			  ;; specified time period, just report as no views at all
			  (log-for error "article {~a} not found" article)
			  default-result)

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
				  (float (alexandria:median daily-views))))))))))
	    ((= status-code 404)	; HTTP NOT FOUND
	     (log-for error "information for [[~a]] is missing and unlikely to appear. move on" article)
	     (setf result default-result))

	    (t				; all other problems
             (log-for error "failed request for article [[~a]]; status code: ~a;~%uri: ~a"
                      article status-code url)
	     (format t "status code: ~a;~%uri: ~a~%" status-code url)))
	  (sb-ext:process-wait proc)
	  (sb-ext:process-close proc))
      (condition (msg)
        (log-for error "failed request for [[~a]]: ~a~%" article msg)))
    result))

;; --------------------------------------------------------

(defun get-article-page-views (article &key
					 (project "uk.wikipedia")
                                         (start (get-prev-month-start))
                                         (end (get-prev-month-end))
					 (attempts *request-attempts*))
  "Returns a list: (total views, minimal daily news, maximum daily
news, median daily views) for a given article."

  (dotimes (attempt attempts)
    (let ((views
           (attempt-article-page-views project article start end)))
      (when views
        (return-from get-article-page-views views))
      (log-for trace "doing another attempt")
      (sleep 15)))

  (log-for error "failed to get information for: [[~a]]~%" article)
  (list 0 0 0 0.0))

;; --------------------------------------------------------

(defun run-page-views (&key
			 (start (get-prev-month-start))
			 (end (get-prev-month-end))
                         (articles-file *default-file*)
                         (views-file *views-file*))

  "Runs article names from one file - `INPUT-FILE', and outputs aticle
name and total number of views in a tab-separated file `VIEWS-FILE'.

In order to obtain articles for a given category use
`DUMP-CATEGORY-TO-FILE' function.

Call `REPORT-PAGE-VIEWS' to get the summary table."

  (log-for trace "Downloading page views data for start:~a end:~a file:~a" start end articles-file)

  (let ((processed-articles 0))

    (when (probe-file views-file)
      (with-open-file (views views-file :direction :input)
	(iter 
	  (for line = (read-line views nil))
	  (while line)
	  (for (num-str) = (split-sequence:split-sequence #\Tab line))
	  (setf processed-articles (parse-integer num-str))))
      (log-for trace "resuming work from article number ~a" processed-articles))

    (with-open-file (in articles-file
			:direction :input)
      (with-open-file (out views-file
                           :direction :output
			   :if-does-not-exist :create
                           :if-exists :append)
	(iter
          (with article-no = 1)
	  (with line-no = 0)
          (for article = (read-line in nil))
          (while article)
          ;; (log-for trace "~a " article-no)
	  
          ;; categories can be present, but ignore them
	  (when (> line-no processed-articles)
            (unless (sm:prefixed-with article *category-prefix*)
              (let* ((article-name (if (sm:prefixed-with article *discussion-prefix*)
                                       (subseq article (length *discussion-prefix*))
                                       article))
                     (article-views (get-article-page-views article-name
							    :start start :end end)))

		(format out "~a	~a~{	~a~}~%"
			line-no
			article-name
			article-views)
		(force-output out)
		(incf article-no)
		(sleep *page-views-query-interval*))))
	  (incf line-no))))))

;; --------------------------------------------------------

(defparameter *page-classes-presentation*
  '(("невідомого рівня" "N/A")
    ("Списки" "'''С'''")
    ("IV рівня" "'''IV'''")
    ("III рівня" "'''III'''")
    ("II рівня" "'''II'''")
    ("I рівня" "'''I'''")
    ("Добрі статті" "[[File:Dobra6.png|15px|alt=Добра стаття]]")
    ("Вибрані списки" "[[File:Feat lists.svg|15px|alt=Вибраний список]]")
    ("Вибрані статті" "[[File:FA gold ukr.png|15px|alt=Вибрана стаття]]"))
  "Patterns to be used to detect page classes in order from the
lowest-quality class to the highest-quality class.

First are the 'no' or 'low' quality, and the last comes the 'best'
quality assessment classes.

Second item in the list is Wiki markup used to represent this class.")

;; --------------------------------------------------------

(defun get-page-class-presentation (title)
  "Query class (quality assessment level) of the page with the given
TITLE and return its Wiki markup representation."

  ;; todo: page categories API query is capable of handling multiple
  ;; titles at a time, but this requires additional handling and
  ;; sorting out of the query results. Would be nice to have it
  ;; integrated
  (log-for trace "getting quality assessment for {~a}" title)

  (let ((raw-categories
         (wiki:retry-query
          (wiki:get-value
           (wiki:get-page-categories title)
           #'cdar :categories)
          :attempts *request-attempts*)))

    ;; todo: very likely this code blob can be rewritten into a more
    ;; robust and concise style. It extracts category titles from
    ;; the Cons tree returned by the query function and then tries
    ;; to find page class Id in the title.
    (loop :for raw-category :in raw-categories
       :for category-title = (cdr (find :title raw-category :key #'car))
       :thereis
       (loop :for present :in *page-classes-presentation*
          :when (sm:string-contains-brute (car present) category-title)
          :return (second present)))))

;; --------------------------------------------------------

(defparameter *months*
  '((1 "січня")
    (2 "лютого")
    (3 "березня")
    (4 "квітня")
    (5 "травня")
    (6 "червня")
    (7 "липня")
    (8 "серпня")
    (9 "вересня")
    (10 "жовтня")
    (11 "листопада")
    (12 "грудня")))

;; --------------------------------------------------------

(defun format-timestamp (ts)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time ts)
    (declare (ignore second minute hour day daylight-p zone))
    (format nil "~a ~a ~a"
            date
            (second (find month *months* :key #'car :test #'=))
            year)))

;; --------------------------------------------------------

(defun report-page-views (&key (limit 100)
                            (show-class NIL)
                            (views-file *views-file*)
                            (out *standard-output*)
                            (time-stamp (get-universal-time))
                            (project-name ""))
  "Create a wiki table listing top articles based on data produced by
`RUN-PAGE-VIEWS'.

VIEWS-FILE: file where RUN-PAGE-VIEWS output is stored.

OUT: stream to ouput results to."

  ;; TODO: use cl-locale to handle strings for text generation

  (with-open-file (in views-file
		      :direction :input)
    (wiki:with-mediawiki ((make-instance 'cl-mediawiki:mediawiki
                                         :url *api-url*
                                         :request-delay 10))
      (let* ( ;; SELECT
             (all-articles
              (iter
                (for line = (read-line in nil))
                (while line)
                (for (num name sum-str min-str max-str med-str) = (split-sequence:split-sequence #\Tab line))
		(declare (ignore num))
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
        (format out "Станом на ~a проект ~a мав ~a статей, які були переглянуті {{formatnum:~D}} раз протягом попереднього місяця. Перелічені нижче Топ-100 статей були переглянуті в сумі {{formatnum:~D}} раз, що складає ~a% від переглядів всіх статей проекту.~%~%"
                (format-timestamp time-stamp)
                project-name
                (length sorted-articles)
                total-views
                top-views
                (round (* (if (> total-views 0)
                              (/ top-views total-views)
                              0)
                          100)))
        (format out "{|
! rowspan=2 | Рейтинг
! rowspan=2 | Рівень
! rowspan=2 | Стаття
! colspan=4 | Кількість переглядів
|-
! Всього
! Мін
! Макс
! Медіана
|-~%")
        (loop :for (article-name sum-views min-views max-views med-views) in top-articles
           :for num :from 1 :upto (length top-articles)
           :do (format out "| ~a || ~a || [[~a]]~1,64T || ~a || ~a || ~a || ~a~%|-~%"
                       num
                       (if show-class
                           (get-page-class-presentation (concatenate 'string *discussion-prefix* article-name))
                           "")
                       article-name sum-views min-views max-views (round med-views)))
        (format out "|}")
#|
== Статистика ==

{|
! Дата
! Кількість статей
! Переглядів всього
! Переглядів топ-100
|-
|#
        ;; summary information to track project's progress
        (format out "~%~%|-~%| ~a~%| ~a~%| {{formatnum:~D}}~%| {{formatnum:~D}}~%"
                (format-timestamp time-stamp)
                (length sorted-articles)
                total-views
                top-views)))))

;; EOF
