;;; variables.el --- Tests for mmm-mode variables -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the mmm-vars file.

;;; Code:

(require 'mmm-vars)
(require 'mmm-mode)
(require 'lisp-mnt)

(ert-deftest verify-mmm-version ()
  "Verify variable `mmm-version' and function `mmm-version'.
Both should match the Version header in mmm-mode.el."
  ;; Find the correct path to the mmm-mode.el file (which contain the
  ;; `Version:' header) by finding using the name of the file where
  ;; `mmm-mode' is defined.
  (let ((lmver (lm-version (find-lisp-object-file-name 'mmm-mode nil)))
	(inhibit-message t)) ; don't print (mmm-version)
    (should (string= lmver mmm-version))
    (should (string-match (regexp-quote lmver) (mmm-version)))))

;;; variables.el ends here
