;;; zel-test.el --- Tests for zel -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'zel)

;;; PREREQUISITES

(zel-install)

;;; TEST


(ert-deftest add-entries-to-frecent-list ()
  (with-empty-frecent-list
    (with-visited-files ("files/one" "files/two" "files/three")
      (should (= (length zel--frecent-list)
                 3)))))


(ert-deftest frequent-files-have-higher-ranks ()
  "A more frequent visited file has a higher rank."
  (with-empty-frecent-list
    (with-visited-files ("files/one" "files/two")
      ;; visit one buffer again. Multiple times to increase ranking.
      (with-current-buffer "one"
        (dotimes (_ 25)
          (zel--update-frecent-list)))
      (should (< (cl-position (expand-file-name "files/one") zel--frecent-list
                              :test #'string=
                              :key #'car)
                 (cl-position (expand-file-name "files/two") zel--frecent-list
                              :test #'string=
                              :key #'car))))))


(ert-deftest frequently-visited-files-are-higher-ranked-that-recent-files ()
  (with-prefilled-frecent-list
    (with-visited-files ("files/one" "files/two")
      (should (> (zel--entry-score (assoc (expand-file-name "files/one")
                                          zel--frecent-list))
                 (zel--entry-score (assoc (expand-file-name "files/two")
                                          zel--frecent-list)))))))


(ert-deftest remove-low-ranked-items-from-list ()
  (let ((zel--aging-multiplier 0.001))
    (with-prefilled-frecent-list
      (with-visited-files ("files/one" "files/two")
        (should (= (length zel--frecent-list)
                   2))
        ;; bomb the list. Making sure that that item will not be
        ;; removed.
        (with-current-buffer "one"
          (dotimes (_ 1000)
            (zel--update-frecent-list)))
        (should (> (zel--frecent-sum)
                   zel--aging-threshold))
        (zel--cleanup-history)
        (should (= (length zel--frecent-list)
                   1))))))

;;; zel-test.el ends here
