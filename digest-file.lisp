;;;; cl版本的 sha1 和 md5 校验码，可以在 windows 上用 cl 方便的校验文件

(defun sha1sum (pathname)
  (digest-sum :sha1 pathname))

(defun md5sum (pathname)
  (digest-sum :md5 pathname))

(defmacro digest-sum (digest-name pathname)
  (let ((digester (gensym)))
    `(let ((,digester (ironclad:make-digest ,digest-name)))
       (ironclad:byte-array-to-hex-string
        (ironclad:digest-file ,digester ,pathname)))))
