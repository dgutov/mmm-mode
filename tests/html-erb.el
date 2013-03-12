(require 'ert)
(require 'ert-x)
(require 'mmm-erb)

(defvar mmm-erb-text
  "<%= foo do %>
     <div class=\"clear\"/>
   <% end %>")

(defun mmm-erb-current-overlay-string ()
  (buffer-substring-no-properties
   (overlay-start mmm-current-overlay)
   (overlay-end mmm-current-overlay)))

(defmacro mmm-erb-deftest (name &rest body)
  `(ert-deftest ,(intern (format "mmm-erb-%s" name)) ()
     (ert-with-test-buffer nil
       (let ((buffer-file-name "foo.html.erb")
             (mmm-global-mode 'maybe)
             mmm-parse-when-idle
             mmm-mode-ext-classes-alist)
         (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
         (html-erb-mode)
         (mmm-mode-on-maybe)
         (should mmm-mode)
         ,@body))))

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

(mmm-erb-deftest attribute-values-are-strings
  (insert mmm-erb-text)
  (mmm-apply-all)
  (goto-char (point-min))
  (search-forward "\"")
  (should (nth 3 (syntax-ppss)))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss)))))

(mmm-erb-deftest quotes-outside-tags-dont-make-strings
  (insert "<% foo do %><p>\"foo bar\"</p><% end %>")
  (mmm-apply-all)
  (goto-char (point-min))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss))))
  (search-forward "\"")
  (should (not (nth 3 (syntax-ppss)))))
