;; Copyright (C) 2013  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'mmm-erb)

(defvar mmm-erb-text
  "<%= foo do %>
     <div class=\"clear\"/>
   <% end %>")

(defconst mmm-erb-edge-emacs (string-lessp "24.3.50" emacs-version))

(defun mmm-erb-current-overlay-string ()
  (buffer-substring-no-properties
   (overlay-start mmm-current-overlay)
   (overlay-end mmm-current-overlay)))

(defmacro mmm-erb-deftest (name &rest body)
  (let ((expected-result (and (eq (car body) :expected-result)
                              (nth 1 body))))
    (when expected-result
      (setq body (nthcdr 2 body)))
    `(ert-deftest ,(intern (format "mmm-erb-%s" name)) ()
       :expected-result ,(or expected-result :passed)
       (ert-with-test-buffer nil
         (let ((buffer-file-name "foo.html.erb")
               (mmm-global-mode 'maybe)
               mmm-parse-when-idle
               mmm-mode-ext-classes-alist)
           (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
           (html-erb-mode)
           (mmm-mode-on-maybe)
           (should mmm-mode)
           ,@body)))))

(put 'mmm-erb-deftest 'lisp-indent-function 'defun)

(mmm-erb-deftest parses-buffer
  (insert mmm-erb-text)
  (mmm-apply-all)
  (should (not mmm-current-overlay))
  (search-backward "foo")
  (should (mmm-update-current-submode))
  (should (string= " foo do " (mmm-erb-current-overlay-string)))
  (search-forward "end")
  (should (mmm-update-current-submode))
  (should (string= " end " (mmm-erb-current-overlay-string))))

(defun mmm-erb-assert-string-syntax ()
  (goto-char (point-min))
  (search-forward "\"")
  (should (nth 3 (syntax-ppss)))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss)))))

(defun mmm-erb-assert-non-string-syntax ()
  (goto-char (point-min))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss))))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss)))))

(mmm-erb-deftest attribute-values-are-strings
  (insert mmm-erb-text)
  (mmm-apply-all)
  (mmm-erb-assert-string-syntax))

(mmm-erb-deftest quotes-outside-tags-dont-make-strings
  :expected-result (if mmm-erb-edge-emacs :passed :failed)
  (insert "<% foo do %><p>\"foo bar\"</p><% end %>")
  (mmm-apply-all)
  (mmm-erb-assert-non-string-syntax))

(mmm-erb-deftest gt-inside-subregion-doesnt-change-nesting
  (insert "<% if 2 > 1 %><div class=\"foo\"/><% end %>")
  (mmm-apply-all)
  (mmm-erb-assert-string-syntax))

(mmm-erb-deftest lt-inside-subregion-doesnt-change-nesting
  :expected-result (if mmm-erb-edge-emacs :passed :failed)
  (insert "<% if 2 < 1 %><p>\"foo bar\"</p><% end %>")
  (mmm-apply-all)
  (mmm-erb-assert-non-string-syntax))
