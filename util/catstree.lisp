;; given a starting directory traverse the subcategories tree and
;; output a list of all subcategories

;; http://en.wikipedia.org/wiki/List_of_mathematics_categories
;;
;; Entry point: START-WALKING-CATEGORIES-TREE

(in-package :cl-mediawiki-util)

;; --------------------------------------------------------

(log5:defcategory :debug)
(log5:start-sender 'warnings-and-worse
 		   (log5:stream-sender :location *standard-output*)
 		   :category-spec '(log5:dribble+)
 		   :output-spec '(log5:time log5:message log5:context))

;; --------------------------------------------------------

(defparameter *default-root-category* "Категорія:Математика"
  "Starting directory for the walk.")
(defparameter *categories-file-name* #P"categories.txt"
  "Contains the resulting list of directories.")
(defparameter *black-list-file-name* #P"blacklist.txt"
  "This file contains the list of directories that we want to avoid.")
(defparameter *queue-file-name* #P"queue.txt"
  "List of directories in the processing queue.")

(defvar *categories-list* '())
(defvar *black-list* '())

;; --------------------------------------------------------

(defun load-list (fn)
  "Load a simple newline-separated list from the file with the given
filename."
  (with-open-file (in fn :direction :input)
    (loop for line = (read-line in nil)
       while line
       collect line)))

;; --------------------------------------------------------

(defun get-value (js &rest keys)
  "JS is a list of key-value pairs as parsed by JSON:DECODE-JSON.

Keys are either keywords or function designators."
  ;; todo: probably REDUCE is a better approach
  (let ((ret js))
    (loop for key in keys
       do
       (setf ret (if (functionp key)
		     (apply key (list ret))
		     (cdr (assoc key ret)))))
    ret))

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
             (format t "~&WALKER: continue: ~A~%" cont)
             (format t "~&WALKER: new items: ~A~%" (map 'list
                                                        #'(lambda (x) (get-value x :title))
                                                        njs))
             (format t "~&WALKER: current res: ~A~%" res)
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

(defun walk-tree (&key
                    (root-category *default-root-category*)
                    (queue (get-subcats-titles root-category))
                    (existing-values :append))
  (format t "~&WALKER: root-category: ~a, queue contains ~a items~%"
          root-category (length queue))
  (dump-queue queue)
  (setf *black-list* (load-list *black-list-file-name*))
  (loop for i = (pop queue)
     while i
     do (progn
	  (format t "~&WALKER: queue contains ~A items~%" (length queue))
	  (format t "~&WALKER: list contains ~A items~%" (length *categories-list*))
	  (unless (or (find i *black-list* :test #'string-equal)
		      (find i *categories-list* :test #'string-equal))
	    (format t "~&WALKER: [[~A]] is a new item~%" i)
	    (push i *categories-list*)
	    (with-open-file (fout *categories-file-name*
				  :direction :output
				  :if-exists existing-values
				  :if-does-not-exist :create)
	      (format fout "~A~%" i)
              ;; don't erase previous item
              (setf existing-values :append))
	    (setf queue
		  (append queue
			  (get-subcats-titles i)))
	    (dump-queue queue)))))

;; --------------------------------------------------------

(defun continue-walk-tree ()
  (let ((queue (load-list *queue-file-name*)))
    (setf *categories-list* (load-list *categories-file-name*))
    (walk-tree :root-category nil :queue queue)))

;; --------------------------------------------------------

(defun start-walking-categories-tree (root-category)
  "Entry point into walking the categories tree.

Given the `ROOT-CATEGORY' starts traversal of the underlying
categories with the `WALK-TREE' function.

This function is created to guarantee that all intermediate variables
and settings are reset to their default values so that the traversal
would have a clean start."

  (let ((*categories-list* '())
        (*black-list* '())
        (*default-root-category* root-category))

    (walk-tree :existing-values :supersede)))

;; --------------------------------------------------------

(defun get-cat-name (cat)
  (subseq cat (length "Категорія:")))

;; --------------------------------------------------------

(defun gen-catstree-page ()
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
