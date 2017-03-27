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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-mediawiki-util.system)
    (defpackage :cl-mediawiki-util.system
	(:use :common-lisp :asdf))))

;; --------------------------------------------------------

(in-package :cl-mediawiki-util.system)

;; --------------------------------------------------------

(defsystem :cl-mediawiki-util
    :description "Utilities implemented using mediawiki's api/cl-mediawiki."
    :components ((:module :util
                          :serial T
                          :components ((:file "package")
                                       (:file "catstree")
                                       (:file "dump-category" )
                                       (:file "missing-topics" )
                                       (:file "page-views"))))
    :depends-on (:cl-mediawiki
                 :drakma :cl-ppcre :cl-json
                 :log5 :alexandria :cl-tokyo-cabinet
                 :external-program :cl-string-match
                 :split-sequence :parse-float
                 :iterate))

;; EOF
