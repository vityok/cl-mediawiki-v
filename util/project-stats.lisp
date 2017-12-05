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

  (let* ((now (get-universal-time))
         (project-key (drakma:url-encode (substitute #\_ #\Space project-name)
                                         :utf-8))
         (project-category (format nil "Категорія:Статті проекту ~a" project-name))
         (category-contents (format nil "ps-category-contents-~a.txt" project-key))
         (views-file (format nil "ps-views-file-~a.txt" project-key))
         (report-file (format nil "ps-report-file-~a.txt" project-key)))

    (dump-category-to-file project-category :file category-contents)
    (format t "dumped category contents, now downloading page views~%")
    (run-page-views :articles-file category-contents :views-file views-file)
    (format t "downloaded page views, now generating top100 and getting their quality~%")
    (with-open-file (out report-file
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (report-page-views :show-class T
                         :views-file views-file
                         :time-stamp now
                         :out out))
    (format t "final report for project '~a' is available at: ~a~%"
            project-name report-file)))

;; --------------------------------------------------------

(defun generate-wiki-project-stats-bg (project-name)
  "Runs GENERATE-WIKI-PROJECT-STATS in background.
Makes it possible to process multiple projects at once."
  (bt:make-thread
   (lambda ()
     (generate-wiki-project-stats project-name))
   :name (format nil "Stats for Wiki project {~a}" project-name)))

;; EOF
