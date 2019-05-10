;;; Generating Wiki project stats, the Top-100 list

;; Приклад створення списків Топ-100 для проектів Вікіпедії із
;; використанням функції GENERATE-WIKI-PROJECT-STATS-BATCH, яка
;; використовує пул тредів для опрацювання поставленого завдання.
;;
;; Ця фукнція отримує назву проекту Вікіпедії а потім опрацьовує
;; статті в катеогрії "Категорія:Статті проекту {Назва проекту}".
;;
;; Статистика переглядів береться зі служби PageviewAPI
;; (див. util/page-views.lisp для подальших деталей).
;;
;; Нарешті, результати роботи зберігаються у файлі
;; "ps-report-file-{Назва проекту}.txt"
;;
;; Для запуску роботи (цього файлу), слід скористатись командою:

;; sbcl --load 'project-stats-task.lisp' --eval '(cl-mediawiki-util:batch-1)' | tee stdout.log
;; sbcl --load 'project-stats-task.lisp' --eval '(cl-mediawiki-util:batch-2)' | tee stdout.log
;; sbcl --load 'project-stats-task.lisp' --eval '(cl-mediawiki-util:batch-3)' | tee stdout.log
;;
;; lx86cl --load 'project-stats-task.lisp' | tee stdout.log

(ql:quickload :cl-mediawiki-util)
(in-package :cl-mediawiki-util)


(defparameter *projects*
  '(
    "Фізика"		  ;; Вікіпедія:Проект:Фізика/Статистика
    "Український футбол" ;; Вікіпедія:Проект:Український футбол/Статистика
    "Сінгапур"		 ;; Вікіпедія:Проект:Сінгапур/Статистика
    "Медицина"		 ;; Вікіпедія:Проект:Медицина/Статистика
    "Молекулярна біологія" ;; Вікіпедія:Проект:Молекулярна біологія/Статистика

    "Математика"	 ;; Вікіпедія:Проект:Математика/Статистика
    "Кінематограф"	 ;; Вікіпедія:Проект:Кінематограф/Статистика
    "Комп'ютерні науки" ;; Вікіпедія:Проект:Комп'ютерні науки/Статистика
    "Ентомологія"       ;; Вікіпедія:Проект:Ентомологія/Статистика


    "Військова історія" ;; Вікіпедія:Проект:Військова історія/Статистика
    "Військова техніка" ;; Вікіпедія:Проект:Військова техніка/Статистика
    "Відеоігри"		;; Вікіпедія:Проект:Відеоігри/Статистика
    "Біологія"		;; Вікіпедія:Проект:Біологія/Статистика

    ))



(defun task-run (proj-no)
  (generate-wiki-project-stats (nth proj-no *projects*))
  #+sbcl
  (sb-ext:quit))

(export 'task-run)
