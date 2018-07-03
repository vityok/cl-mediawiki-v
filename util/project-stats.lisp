;;; Generate TOP100 stats for wiki projects. Combines all steps into
;;; a single function call

(in-package :cl-mediawiki-util)

;; --------------------------------------------------------

#+ironclad
(defun digest-line (line)
  "Produce a hopefuly uniqe digest (key) for the given string."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :crc32
    (flexi-streams:string-to-octets line))))

;; --------------------------------------------------------

(defun generate-wiki-project-stats (project-name)
  "Given PROJECT-NAME dump all its articles, then query their
impressions, generate the top 100 and produce wiki text for the
resulting page.

Remembers the date when the dump has been produced, automatically
generates all dates in the summary.

PROJECT-NAME name of the Wiki project.
"
  ;; todo: make it possible to resume stats collection in case if it
  ;; has been abruptly shut down (software crash, network glitches,
  ;; etc.)

  (let* ((now (get-universal-time))
         (project-key (substitute-if #\_
                                     #'(lambda (c)
                                         (or (char= c #\Space)
                                             (char= c #\') (char= c #\")
                                             (char= c #\:)))
                                     project-name))
         (project-category (format nil "Категорія:Статті проекту ~a" project-name))
         (category-contents (format nil "ps-category-contents-~a.txt" project-key))
         (views-file (format nil "ps-views-file-~a.txt" project-key))
         (report-file (format nil "ps-report-file-~a.txt" project-key)))

    (unless (probe-file category-contents)
      (dump-category-to-file project-category :file category-contents)
      (log-for trace "dumped category contents, now downloading page views"))
    (unless (probe-file views-file)
      (run-page-views :articles-file category-contents :views-file views-file)
      (log-for trace "downloaded page views, now generating top100 and getting their quality"))
    (unless (probe-file report-file)
      (with-open-file (out report-file
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (report-page-views :show-class T
                           :views-file views-file
                           :time-stamp now
                           :out out
                           :project-name project-name))
      (log-for trace "final report for project '~a' is available at: ~a"
                    project-name report-file))
    (log-for trace "done")))

;; --------------------------------------------------------

(defun generate-wiki-project-stats-bg (project-name)
  "Runs GENERATE-WIKI-PROJECT-STATS in background.
Makes it possible to process multiple projects at once."

  ;; todo: log progress in the file to separate concurrently running
  ;; threads
  (bt:make-thread
   (lambda ()
     (generate-wiki-project-stats project-name))
   :name (format nil "Stats for Wiki project {~a}" project-name)))

;; --------------------------------------------------------

(defun generate-wiki-project-stats-batch (projects &key (workers-count 3))
  "Given a list of projects dispatch WORKERS-COUNT worker threads to
handle them all in parallel."

  (let ((jobs-queue (queues:make-queue :simple-cqueue))
        (central-lock (bt:make-lock "central lock")))
    ;; here we rely on the thread-safety of the simple-cqueue
    (dolist (proj projects)
      (queues:qpush jobs-queue proj))
    ;; create pool of worker threads
    (let ((workers
           (iter
             (for worker from 0 below workers-count)
             (collect
                 (bt:make-thread
                  (lambda ()
                    ;; a worker thread gets next project name from the
                    ;; queue and generates report for it. Exits when
                    ;; there are no projects left
                    (iter
                      (for project-name = (queues:qpop jobs-queue nil))
                      (while project-name)
                      (bt:with-lock-held (central-lock)
                        ;; use central lock to dump thread info to the
                        ;; stdout to avoid clutter
                        (format t "Worker {~a} collecting info for: {~a}~%"
                                worker project-name))
                      (generate-wiki-project-stats project-name))
                    (format t "Worker {~a} done~%" worker))
                  :name (format nil "Worker {~a}" worker))))))

      ;; wait for workers to end their jobs
      (dolist (worker workers)
        (bt:join-thread worker)))))

;; EOF
