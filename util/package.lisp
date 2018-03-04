;; -*- lisp -*-
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


(defpackage :cl-mediawiki-util
  (:use :common-lisp :iterate)
  (:nicknames :mw-util)
  (:export
   #:gen-catstree-page
   #:start-walking-categories-tree
   #:dump-category-to-file

   #:run-page-views
   #:report-page-views))

;; --------------------------------------------------------

(in-package :cl-mediawiki-util)

(defparameter *request-attempts* 5
  "Sometimes due to network or server glitches HTTP requests can
fail. This parameter regulates number of times request will be retried
before giving up.")

;; --------------------------------------------------------

(log5:defcategory :debug)
(log5:start-sender 'dribble-and-worse
 		   (log5:stream-sender :location *standard-output*)
 		   :category-spec '(log5:dribble+)
 		   :output-spec '(log5:time log5:message log5:context))

(log5:start-sender 'error-and-worse
                   (log5:stream-sender :location "errors.log")
                   :category-spec '(error)
                   :output-spec '(log5:time log5:message))

;; --------------------------------------------------------

(defparameter *log-lock* (bt:make-lock "log-lock"))

(defmacro log-for (category-spec message &rest args)
  "This is thread-safety wrapper around LOG5:LOG-FOR macro. Its only
purpose is to wrap logging output into a global thread-safe block."

  `(bt:with-lock-held (*log-lock*)
     (log5:log-for ,category-spec ,message ,@args)))

;; EOF
