;;;; web-spider.asd

(asdf:defsystem #:web-spider
  :description "Describe web-spider here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "web-spider")))

