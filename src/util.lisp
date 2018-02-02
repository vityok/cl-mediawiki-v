;; Copyright (c) 2008 Accelerated Data Works, Russ Tyndall
;; Copyright (c) 2017 bitbucket.org/vityok

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; This file has a few necessary random utility functions

(in-package :cl-mediawiki)

;; --------------------------------------------------------

(defun symbolize-string (str &optional (package :keyword))
  "Turns a string into a happy symbol

   ex: ''foo bar_bast'' -> FOO-BAR-BAST
  "
  (etypecase str
    (string (intern (nsubstitute
		     #\- #\_
		     (nsubstitute #\- #\space (string-upcase str) :test #'char=)
		     :test #'char=)
		    package))
    (symbol str)))

;; --------------------------------------------------------

(defun user-anon-p (&optional ui)
  "Returns T if user is anonymous (not logged in).

Parameters:

  UI userinfo obtained through USERINFO."
  (string= "" (get-value
	       (if ui ui (userinfo))
	       :anon)))

;; --------------------------------------------------------

(defun has-messages-p (&optional ui)
  "Returns T if user has messages.

Parameters:

  UI userinfo obtained through USERINFO."
  (string= "" (get-value
	       (if ui ui (userinfo))
	       :hasmsg)))

;; --------------------------------------------------------

(defmacro retry-query (form &key (attempts 5))
  "Wraps a query to the wiki server and retries it number of ATTEMPTS
until it follows through."
  (let ((attempt (gensym "attmpt"))
        (msg (gensym "msg"))
        (result (gensym "rslt")))

    `(dotimes (,attempt (+ ,attempts 1))
       (when (= ,attempt ,attempts)
         (log5:log-for error "failed to perform query after ~a attempts" ,attempts)
         (return))
       (handler-case (multiple-value-list ,form)
         (condition (,msg) (log5:log-for error "request failed: ~a" ,msg))
         (:no-error (,result) (return (values-list ,result)))))))

;; (retry-query (format nil "abc") :attempts 5)

;; EOF
