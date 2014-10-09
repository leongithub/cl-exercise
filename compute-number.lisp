;; 需求：
;; 由于公司网页表格里有很多这样的数据：1+2+3+2+4
;; 最后要求去掉最大最小，然后算平均值，最后两个平均值按照a*8+b*2计算得除结果。
;; 应该有的计算器会支持，但还是练习下cl吧:)

;; 解决步骤：
;; 1.先把1+2+3去掉+("123")，然后转换成list(#\1 #\2 #\3)，再转换成数字列表(1 2 3)
;; 2.套用a*8+b*2计算

(defun compute (x y)
  (let ((ppx (preproccess x))
	(ppy (preproccess y)))
    (+ (* 8
	  (princ (n-sub-max-min-to-average ppx)))
       (* 2
	  (princ (n-sub-max-min-to-average ppy))))))

(defun n-sub-max-min-to-average (seq)
  (/ (- (reduce #'+ seq)
	(reduce #'max seq)
	(reduce #'min seq))
     (- (length seq) 2.0)))

(defun preproccess (x)
  (char-list->number-list
   (concatenate 'list 
		(remove #\+
			(string x)))))
 
(defun char-list->number-list (char-lst)
  (mapcar #'(lambda (c)
	      (parse-integer (string c)))
	  char-lst))

