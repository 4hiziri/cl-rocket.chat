#|
  This file is a part of cl-rocket-chat project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rocket-chat-test-asd
  (:use :cl :asdf))
(in-package :cl-rocket-chat-test-asd)

(defsystem cl-rocket-chat-test
  :author "4hiziri"
  :license ""
  :depends-on (:cl-rocket-chat
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-rocket-chat"))))
  :description "Test system for cl-rocket-chat"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
