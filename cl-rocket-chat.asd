#|
  This file is a part of cl-rocket-chat project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

#|
  rocket.chat client

  Author: 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rocket-chat-asd
  (:use :cl :asdf))
(in-package :cl-rocket-chat-asd)

(defsystem cl-rocket-chat
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-annot
               :dexador
               :cl-json)
  :components ((:module "src"
                :components
                ((:file "cl-rocket-chat"))))
  :description "rocket.chat client"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-rocket-chat-test))))
