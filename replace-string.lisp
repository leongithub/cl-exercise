;;;; 用来批量替换文件中的字符串  test

;; 应用场景：在看Android源码的时候，发现values-*/strings.xml有很多
;; <string name="account_phone" product="tablet">Tablet-only, unsynced</string>
;; <string name="account_phone" product="default">Phone-only, unsynced</string>
;;
;; <string name="fail_reason_too_many_vcard" product="nosdcard">Too many vCard files are in the storage.</string>
;; <string name="fail_reason_too_many_vcard" product="default">Too many vCard files are on the SD card.</string>
;; 会使我用Android studio引入源码自己build报错，所以要留下default ，注释掉其他的
;;
;; 程序设计：由于这些strings.xml文件分布在res/values*/strings.xml，res目录大部分是vaules*目录，所以就把
;; res目录下的所有目录加载进来，然后再与strings.xml合并为一个完整的文件路径。进入文件，查看每一行，是否包
;; 含"product="字符串，如果有再查看product的值是否不为“default”，如果符合替换条件，在输出原本行的同时加上
;; 注释前后缀。

(defun replace-strxml (dir-res)
  (dolist (filepath (find-files dir-res))
    (replace-string filepath "<!--" "-->")))

(defun find-files (dir-parent)
  (let ((dir-object (make-pathname :name :wild :type :wild :defaults dir-parent)))
    (mapcar #'(lambda (dir)
		(merge-pathnames dir
				 (make-pathname :name "strings"
						:type "xml")))
	    (directory dir-object))))

(defun replace-string (filepath pre-str post-str)
  (with-open-file (in filepath :direction :input :if-does-not-exist nil)
    (if in
	(with-open-file (out filepath :direction :output :if-exists :overwrite)
	  (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	      ((eql line 'eof))
	    (let ((matchp (and (setf pos (search "product=" line))
			       (not (search "default" line 
					    :start2 (+ pos 9) :end2 (+ pos 16))))))
	      (if matchp (princ pre-str out))
	      (princ line out)
	      (if matchp (princ post-str out))
	      (terpri out)))))))
