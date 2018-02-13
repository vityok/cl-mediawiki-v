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

;; sbcl --load 'project-stats-task.lisp' | tee stdout.log

(ql:quickload :cl-mediawiki-util)
(in-package :cl-mediawiki-util)

(generate-wiki-project-stats-batch
 '("Комп'ютерні науки" ;; Вікіпедія:Проект:Комп'ютерні науки/Статистика
   "Військова техніка" ;; Вікіпедія:Проект:Військова техніка/Статистика
   "Математика"        ;; Вікіпедія:Проект:Математика/Статистика
   "Фізика"            ;; Вікіпедія:Проект:Фізика/Статистика
   "Ентомологія"       ;; Вікіпедія:Проект:Ентомологія/Статистика
   "Молекулярна біологія" ;; Вікіпедія:Проект:Молекулярна біологія/Статистика
   "Сінгапур"             ;; Вікіпедія:Проект:Сінгапур/Статистика
   "Медицина"             ;; Вікіпедія:Проект:Медицина/Статистика
   "Український футбол" ;; Вікіпедія:Проект:Український футбол/Статистика
   "Біологія"           ;; Вікіпедія:Проект:Біологія/Статистика
   "Кінематограф"       ;; Вікіпедія:Проект:Кінематограф/Статистика
   "Відеоігри"          ;; Вікіпедія:Проект:Відеоігри/Статистика
   ))

#+sbcl
(sb-ext:quit)