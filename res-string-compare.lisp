;;;; 由于说原生对小语种支持缺失，如：values/strings.xml 有 <string name="a">a</string>，而values-fi/strings.xml 没有此字段。
;;;; 此代码寻找出缺失的字符串

(defparameter *base-strings* "values")
(defparameter *compare-strings-array*
  (list "values-ca" "values-ca-rES" "values-cs" "values-cs-rCZ" "values-da" "values-da-rDK" "values-de" "values-de-rAT" "values-de-rch" "values-de-rDE" "values-el" "values-el-rGR" "values-en-rGB" "values-es" "values-es-rES" "values-fi" "values-fi-rFI" "values-fr" "values-fr-rBE" "values-fr-rCH" "values-fr-rFR" "values-hu" "values-hu-rHU" "values-it" "values-it-rIT" "values-nb" "values-nb-rNO" "values-nl-rBE" "values-nl-rNL" "values-pl" "values-pl-rPL" "values-pt" "values-pt-rBR" "values-pt-rPT" "values-ro" "values-ro-rRO" "values-ru" "values-ru-rRU" "values-sv" "values-sv-rSE" "values-tr" "values-tr-rTR" "values-zh-rCN" "values-zh-rHK" "values-zh-rTW"))

(defun main (dir-res)
  (with-open-file (out "~/compare-strings-result.txt"
		       :direction :output
		       :if-exists :supersede)
    (with-open-file (in (rtn-strings-xml dir-res *base-strings*)
			:direction :input
			:if-does-not-exist nil)
      (when in
	(dolist (dir-value *compare-strings-array*)
	  (file-position in 0)
	  (with-open-file (in2 (rtn-strings-xml dir-res dir-value)
			       :direction :input
			       :if-does-not-exist nil)
	    (format out "~%~%~A~%" dir-value)
	    (if in2
		(compare-strings in in2 out)
		(format out "file ~A is not exist~%" dir-value))))))))


(defun compare-strings (in in2 out)
  (do ((line (read-line in nil 'eof)
	     (read-line in nil 'eof))
       (line-number 1 (1+ line-number)))
      ((eql line 'eof))
    (file-position in2 0)
    (let ((compare-line-p (search "name=" line)))
      (if compare-line-p
	  (let ((name (subseq line
			      compare-line-p
			      (search "\""
				      line
				      :start2 (+ compare-line-p 6)))))
	    (do ((line2 (read-line in2 nil 'eof)
			(read-line in2 nil 'eof)))
		((eql line2 'eof)
		 (format out "~A: ~A~%" line-number line))
	      (if (search name line2)
		  (return))))))))


(defun rtn-strings-xml (dir-res dir-values)
  (merge-pathnames (make-pathname :directory `(:relative ,dir-values)
				  :name "strings"
				  :type "xml")
		   dir-res))
