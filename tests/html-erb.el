(require 'ert)
(require 'ert-x)
(require 'mmm-erb)

(defvar erb-text
  "<%= foo do %>
     <div class=\"clear\"/>
   <% end %>")

(defun mmm-erb-current-overlay-string ()
  (buffer-substring-no-properties
   (overlay-start mmm-current-overlay)
   (overlay-end mmm-current-overlay)))

(ert-deftest mmm-erb-parses-buffer ()
  (ert-with-test-buffer nil
    (let ((buffer-file-name "foo.html.erb")
          (mmm-global-mode 'maybe)
          mmm-mode-ext-classes-alist)
      (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
      (insert erb-text)
      (html-erb-mode)
      (mmm-mode-on-maybe)
      (should mmm-mode)
      (should (not mmm-current-overlay))
      (search-backward "foo")
      (should (mmm-update-current-submode))
      (should (string= " foo do " (mmm-erb-current-overlay-string)))
      (search-forward "end")
      (should (mmm-update-current-submode))
      (should (string= " end " (mmm-erb-current-overlay-string))))))
