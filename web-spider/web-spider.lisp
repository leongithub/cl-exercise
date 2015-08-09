;;;; web-spider.lisp

;; 用法：
;; (ql:quickload :web-spider)
;; (in-package :web-spider)
;; (sicp-exercise-*)

(in-package #:web-spider)

;;; "web-spider" goes here. Hacks and glory await!

;; 由于搬新家，可能暂时没有网络，所以写个迷你爬虫，把 sicp 的习题爬下来
;; 当然，这个小程序离真正的爬虫还远，只满足目前的需求

(defparameter *base-path* "/Users/leon/Documents/sicp-exercise/")

;; http://eli.thegreenplace.net/tag/sicp此网址保存习题
(defun sicp-exercise-eli ()
  (let* ((dir-parent (ensure-directories-exist
		      (concatenate 'string *base-path* "eli/")))
	 (dir-list (cdr (cl-ppcre:split "/" dir-parent)))
	 (body (drakma:http-request "http://eli.thegreenplace.net/tag/sicp")))
    (cl-ppcre:do-register-groups (uri)
	("<td><a href='(.*)'>.*</a></td>" body)
      (let ((filespec (make-pathname
		       :directory (cons :absolute dir-list)
		       :name (car (last (cl-ppcre:split "/" uri)))
		       :type "html")))
	(unless (probe-file filespec)
	  (spider-to-file uri filespec))))))

;; http://community.schemewiki.org/?sicp-solutions此网址保存习题
;; 与sicp-exercise-eli模式好像，应该能抽象出个宏，但又有些不太一样的地方
;; 留给以后优化
(defun sicp-exercise-solutions ()
  (let* ((dir-parent (ensure-directories-exist
		      (concatenate 'string *base-path* "solutions/")))
	 (dir-list (cdr (cl-ppcre:split "/" dir-parent)))
	 (body (drakma:http-request "http://community.schemewiki.org/?sicp-solutions"))
	 (base-uri "http://community.schemewiki.org/"))
    (cl-ppcre:do-register-groups (uri)
	("<a href=\"/(\\?sicp-ex-.{3,4})\">sicp-ex-" body)
      (let ((filespec (make-pathname
		       :directory (cons :absolute dir-list)
		       :name uri
		       :type "html")))
	(unless (probe-feile filespec)
	  (spider-to-file (concatenate 'string base-uri uri)
			  filespec))))))
	 
;; 通过一个url保存到文件名为<title>的html文件中
;; 这里把http-request的返回值都写出来只是为了清晰而已，完全用nth-value可以只获得body
(defun spider-to-file (spider-uri filespec)
  (multiple-value-bind
	(body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request spider-uri)
    (declare (ignore status-code headers uri stream must-close reason-phrase))
    (with-open-file (out filespec
			 :direction :output
			 :if-exists :supersede
			 :external-format :utf-8)
      (princ body out))))
    
;; 根据body中title，生成文件名
;; 感觉爬虫应该用uri命名比较好，所以暂不用此方法
(defun generation-html-filename (body)
  (concatenate 'string
	       (svref (nth-value
		       1
		       (cl-ppcre:scan-to-strings "<title>(.*)</title>" body))
		      0)
	       ".html"))
