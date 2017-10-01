;;; zel.el --- Access frecent files easily  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: tbd
;; Version: 0.1.0-pre
;; Package-Requires: ((emacs "25") cl-lib frecency)
;; Keywords: convenience, files, matching

;;; Commentary:

;; tbd

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; use-package

;; tbd.

;;;;; Manual

;; Install these required packages:

;; - tbd

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'zel)

;;;; Usage

;;;; Credits

;; - https://github.com/rupa/z

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'frecency)

;;;; Variables

(defgroup zel ()
  "Access frecent files easily."
  :group 'convenience
  :group 'files
  :group 'matching)


(defcustom zel-history-file "~/.emacs.d/zel-history"
  "File where the history is saved."
  :type 'file)


(defvar zel--aging-threshold 9000
  "Threshold used to clean out old items.

When the sum of all entries reach this threshold older items are
removed.")


(defvar zel--aging-multiplier 0.99
  "Multiplier used on each item do determine the age of it.

If the age of an item after applying the multiplier is less than
1 it's determined to be too old and gets removed.")


(defvar zel--frecent-list nil
  "The list with the frecent items.")


;;;; Functions

;;;;; Commands

(cl-defmacro zel--with-history-buffer (&body body)
  (declare (indent defun))
  (let ((buffer (cl-gensym "buffer")))
    `(let ((,buffer (find-file-noselect (expand-file-name zel-history-file) t)))
       (with-current-buffer ,buffer
         ,@body))))


(defun zel-write-history ()
  "Writes the current frecent list to the `zel-history-file'."
  (interactive)
  (zel--with-history-buffer
    (erase-buffer)
    (goto-char (point-min))
    (print zel--frecent-list (current-buffer))
    (save-buffer)))


(defun zel-load-history ()
  "Load the history file found under `zel-history-file'."
  (interactive)
  (zel--with-history-buffer
    (goto-char (point-min))
    (setq zel--frecent-list (read (current-buffer)))))


;;;; Footer

(provide 'zel)

;;; zel.el ends here
