;;; mmm-test-util.el --- Test helpers for mmm-mode tests

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Ola Nilsson <ola.nilsson@gmail.com>

;; {{{ GPL

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


;;}}}

;;; Commentary:

;; This file contains help functions and macros for mmm-mode tests

;;; Code:

(require 'font-core)

(defmacro mmm-with-global-font-lock (&rest body)
  "Fake active `global-font-lock-mode' for BODY."
  ;; This is a hack.
  ;; Locally bind `noninteractive' to nil and set upp hooks as
  ;; `global-font-lock-mode' would have.  Assumes
  ;; global-font-lock-mode has been avaluated so all the required
  ;; global-font-lock-mode-X exist.
  (declare (indent 'defun))
  `(let ((after-change-major-mode-hook after-change-major-mode-hook)
	 (find-file-hook find-file-hook)
	 (change-major-mode-hook change-major-mode-hook)
	 noninteractive)
     ;; the enable part of the global minor mode
     (add-hook 'after-change-major-mode-hook
	       #'global-font-lock-mode-enable-in-buffers)
     (add-hook 'find-file-hook #'global-font-lock-mode-check-buffers)
     (add-hook 'change-major-mode-hook #'global-font-lock-mode-cmhh)
     ,@body))

(provide 'mmm-test-util)
;;; mmm-test-util.el ends here
