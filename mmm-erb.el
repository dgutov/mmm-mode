(mmm-add-group
 'html-js
 '((js-script-cdata
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
   (js-script
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                 @ "" _ "" @ "\n</script>" @)))))

(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>")
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?c css-tag nil @ "<style type=\"text/css\">\n"
                 @ "" _ "" @ "\n</style>" @)))))

(mmm-add-classes
 '((erb :submode ruby-mode :front "<%[#=]?" :back "-?%>"
        :match-face (("<%#" . mmm-comment-submode-face)
                     ("<%=" . mmm-output-submode-face)
                     ("<%" . mmm-code-submode-face))
        :insert ((?% erb-code nil @ "<%" @ " " _ " " @ "%>" @)
                 (?# erb-comment nil @ "<%#" @ " " _ " " @ "%>" @)
                 (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))
   (ejs :submode js-mode :front "<%[#=]?" :back "-?%>"
        :match-face (("<%#" . mmm-comment-submode-face)
                     ("<%=" . mmm-output-submode-face)
                     ("<%" . mmm-code-submode-face))
        :insert ((?% ejs-code nil @ "<%" @ " " _ " " @ "%>" @)
                 (?# ejs-comment nil @ "<%#" @ " " _ " " @ "%>" @)
                 (?= ejs-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))

(pushnew '(indent-line-function buffer) mmm-save-local-variables)

(dolist (mode (list 'html-erb-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.html\\(\\.erb\\)?\\'" 'html-js)
  (mmm-add-mode-ext-class mode "\\.html\\(\\.erb\\)?\\'" 'html-css)
  (mmm-add-mode-ext-class mode "\\.html\\.erb\\'" 'erb)
  (mmm-add-mode-ext-class mode "\\.jst\\.ejs\\'" 'ejs))

;;;###autoload
(define-derived-mode html-erb-mode html-mode "ERB-HTML"
  (add-hook 'mmm-html-erb-mode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-ruby-mode-submode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-css-mode-submode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-js-mode-submode-hook 'mmm-erb-process-submode nil t))

(defun mmm-erb-process-submode ()
  (setq indent-line-function 'mmm-erb-indent-line))

(defun mmm-erb-indent-line ()
  (interactive)
  (mmm-update-submode-region)
  (destructuring-bind (primary-indent-function submode-indent-function)
      (mapcar (lambda (mode) (cadr (assoc 'indent-line-function
                                     (get mode 'mmm-local-variables))))
              (list mmm-primary-mode mmm-current-submode))
    (let (added-whitespace)
      (if (and mmm-current-overlay mmm-current-submode
               ;; Region starts before the current line.
               (< (overlay-start mmm-current-overlay)
                  (point-at-bol)))
          (if (<= (overlay-end mmm-current-overlay)
                  (save-excursion (back-to-indentation) (point)))
              ;; We're at a closing tag.
              (mmm-erb-indent-to-region-start)
            (save-restriction
              (save-excursion
                (goto-char (overlay-start mmm-current-overlay))
                (when (not (looking-at "^\\|\\s-*$"))
                  ;; Submode region has text on the same line as the opening tag,
                  ;; pad it with whitespace to make the following lines line up.
                  (setq added-whitespace (current-column))
                  (insert-char ?\s added-whitespace)))
              (narrow-to-region (overlay-start mmm-current-overlay)
                                (overlay-end mmm-current-overlay))
              (funcall submode-indent-function)
              (when added-whitespace
                ;; Remove the padding.
                (save-excursion
                  (goto-char (overlay-start mmm-current-overlay))
                  (delete-char added-whitespace))))
            ;; If submode indent function moved us to bol,
            ;; we're on the top level, indent according to the primary mode.
            (when (zerop (current-indentation))
              (mmm-erb-indent-to-region-start
               (mmm-erb-indent-offset mmm-primary-mode))))
        (funcall primary-indent-function)))))

(defvar mmm-erb-offset-var-alist
  '((html-erb-mode . sgml-basic-offset)
    (nxml-mode . nxml-child-indent)))

(defun mmm-erb-indent-offset (mode)
  (let ((name (cdr (assoc mode mmm-erb-offset-var-alist))))
    (when name (symbol-value name))))

(defun mmm-erb-indent-to-region-start (&optional additional-offset)
  (let* ((indent (current-indentation))
         (offset (- (current-column) indent)))
    (indent-line-to
     (save-excursion
       (goto-char (1- (overlay-start mmm-current-overlay)))
       (+ (current-indentation)
          (or additional-offset 0))))
    (when (> offset 0) (forward-char offset))))

(provide 'mmm-erb)
