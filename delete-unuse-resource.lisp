;;;; 最近对项目中的无用资源（主要图片和xml文件）进行清理
;;;; 用法:
;;;; (delete-unuse-resource "path-to-project")
;;;; (delete-unuse-resource "path-to-project" '(other-path-to-search))

(defconstant +buffer-size+ 8192)

(defun delete-unuse-resource (project-pathname &optional search-project-lst)
  (pushnew project-pathname search-project-lst :test 'equal)
  (dolist (pathname search-project-lst)
    (unless (com.gigamonkeys.pathnames:file-exists-p pathname)
      (error "~a is not exists" pathname)))
  (let ((result (find-out-unuse-resource project-pathname search-project-lst)))
    (print (mapcar #'(lambda (p) (namestring p)) result))
    (when (y-or-n-p "是否删除以上资源")
      (dolist (p result 'done)
        (delete-file p)))))

(defun find-out-unuse-resource (project-pathname search-project-lst)
  (labels ((find-out-unuse-resource-1 (name)
             (let ((regex (concatenate 'string name "[^a-z_]")))
               (dolist (project search-project-lst)
                 (com.gigamonkeys.pathnames:walk-directory
                  project
                  #'(lambda (p)
                      (with-open-file (stream p)
                        (when (cl-ppcre:scan regex (%read-body stream))
                          (return-from find-out-unuse-resource-1))))
                  :test #'(lambda (p)
                            (let ((type (pathname-type p)))
                              (or (string= type "java")
                                  (string= type "xml"))))
                  :list-directory-test #'(lambda (p)
                                           (let ((last-dir (car (last (pathname-directory p)))))
                                             (not (string= "build" last-dir))))))
               name)))
    (let (lst
          (res-dir (make-pathname :directory
                                  (append (pathname-directory
                                           (com.gigamonkeys.pathnames:pathname-as-directory project-pathname))
                                          (list "res")))))
      (dolist (x (com.gigamonkeys.pathnames:list-directory res-dir))
        (let ((last-dir (car (last (pathname-directory x)))))
          (when (or (search "drawable" last-dir)
                    (search "layout" last-dir))
            (dolist (resource (com.gigamonkeys.pathnames:list-directory x))
              (when (find-out-unuse-resource-1 (pathname-name (pathname-name resource)))
                (push resource lst))))))
      (nreverse lst))))

;; drakma's %read-body
(defun %read-body (stream)
  "Helper function to read from stream into a buffer of character, which is returned."
  (let ((buffer (make-array +buffer-size+ :element-type 'character))
        (result (make-array 0 :element-type 'character :adjustable t)))
    (loop for index = 0 then (+ index pos)
       for pos = (read-sequence buffer stream)
       do (adjust-array result (+ index pos))
         (replace result buffer :start1 index :end2 pos)
       while (= pos +buffer-size+))
    result))
