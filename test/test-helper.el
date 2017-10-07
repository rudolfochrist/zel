;;; test-helper.el --- Helpers for zel-test.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defmacro with-empty-frecent-list (&rest body)
  (declare (indent defun))
  `(let ((zel--frecent-list '()))
     ,@body))


(defmacro with-prefilled-frecent-list (&rest body)
  (declare (indent defun))
  `(let ((zel--frecent-list '((,(expand-file-name "files/one")
                               ((:frecency-num-timestamps . 10)
                                (:frecency-timestamps . (1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073
                                                         1504108854.0087073))
                                (:frecency-total-count . 11))))))
     ,@body))


(cl-defmacro with-visited-files ((&rest files) &body body)
  (declare (indent defun))
  (let ((buffers (cl-gensym "buffers"))
        (buffer (cl-gensym "buffer")))
    `(let ((,buffers (mapcar (lambda (file)
                               (find-file-noselect file t))
                             ',files)))
       (unwind-protect
           (progn ,@body)
         (dolist (,buffer ,buffers)
           (kill-buffer ,buffer))))))


;;; test-helper.el ends here
