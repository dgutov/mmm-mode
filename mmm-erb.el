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

(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-css)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

;;;###autoload
(define-derived-mode html-erb-mode html-mode "ERB-HTML"
  (setq sgml-unclosed-tags nil) ; Simplifies indentation logic.
  (add-hook 'mmm-html-erb-mode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-ruby-mode-submode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-css-mode-submode-hook 'mmm-erb-process-submode nil t)
  (add-hook 'mmm-js-mode-submode-hook 'mmm-erb-process-submode nil t))

(defun mmm-erb-process-submode ()
  (setq indent-line-function 'mmm-erb-indent-line))

(defun mmm-erb-indent-line ()
  (interactive)
  (mmm-update-submode-region)
  (if (and mmm-current-overlay mmm-current-submode
           (< (overlay-start mmm-current-overlay) (point-at-bol)))
      ;; Region starts before the current line (hence, contains indentation).
      (mmm-erb-indent-line-submode)
    (mmm-erb-indent-line-primary)))

(defun mmm-erb-indent-line-submode ()
  (let (added-whitespace)
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
        (funcall (mmm-erb-orig-indent-function mmm-current-submode))
        (when added-whitespace
          ;; Remove the padding.
          (save-excursion
            (goto-char (overlay-start mmm-current-overlay))
            (delete-char added-whitespace))))
      ;; If submode indent function moved us to bol,
      ;; we're on the top level, indent according to the primary mode.
      (when (zerop (current-indentation))
        (mmm-erb-indent-to-region-start
         (mmm-erb-indent-offset mmm-primary-mode))))))

(defun mmm-erb-indent-to-region-start (&optional additional-offset)
  (let* ((indent (current-indentation))
         (offset (- (current-column) indent)))
    (indent-line-to
     (save-excursion
       (goto-char (1- (overlay-start mmm-current-overlay)))
       (+ (current-indentation)
          (or additional-offset 0))))
    (when (> offset 0) (forward-char offset))))

(defun mmm-erb-indent-line-primary ()
  (save-excursion
    (let* ((here (point))
           ;; Before previous line's tag.
           (start (progn (forward-line -1)
                         (back-to-indentation)
                         (let ((lcon (sgml-lexical-context)))
                           (when (eq (car lcon) 'tag)
                             ;; Tag spreads several lines.
                             (goto-char (cdr lcon))
                             (back-to-indentation)))
                         (point)))
           (regions (mmm-regions-in start here))
           (offset (- (current-column) (current-indentation)))
           (n 0))
      ;; Collect indent modifier depending on type of tags.
      (loop for region in regions
            for type = (mmm-erb-scan-region region)
            when type do
            (if (eq type 'close)
                (when (plusp n) (decf n))
              (incf n (if (eq type 'close) 0 1))))
      (let ((eol (progn (goto-char here) (end-of-line 1) (point))))
        ;; There can be primary mode regions in the list, so we loop.
        ;; Look for "else" and "end" instructions.
        ;; If a block start instruction comes first, don't adjust modifier.
        (loop for region in (mmm-regions-in here eol)
              for type = (mmm-erb-scan-region region)
              until (eq type 'open)
              when (memq type '(middle close)) do (decf n)))
      (goto-char here)
      (funcall (mmm-erb-orig-indent-function mmm-primary-mode))
      (let* ((indent (current-indentation))
             (indent-step (mmm-erb-indent-offset mmm-primary-mode)))
        (indent-line-to (+ indent (if n (* indent-step n) 0)))
        (when (> offset 0) (forward-char offset))))))

(defun mmm-erb-scan-region (region)
  (when region ; Can be nil if a line is empty, for example.
    (destructuring-bind (submode beg end) region
      (let ((scan-fn (plist-get '(ruby-mode mmm-erb-scan-erb
                                  js-mode   mmm-erb-scan-ejs)
                                submode)))
        (when scan-fn
          (save-excursion
            (goto-char beg)
            (skip-syntax-forward "-")
            (funcall scan-fn end)))))))

(defun mmm-erb-scan-erb (limit)
  (cond ((looking-at "\\(?:if\\|unless\\|for\\|while\\)\\b") 'open)
        ((looking-at "\\(?:else\\|elsif\\)\\b") 'middle)
        ((looking-at "end\\b\\|}") 'close)
        ((re-search-forward (concat "\\(?: +do +\\| *{ *\\)"
                                    "\\(?:|[A-Za-z0-9_, ]*|\\)? *") limit t)
         'open)))

(defun mmm-erb-scan-ejs (limit)
  (cond ((looking-at "\\(?:if\\|for\\|while\\)\\b") 'open)
        ((looking-at "} *else\\b") 'middle)
        ((looking-at "}") 'close)
        ((re-search-forward " *{ *" limit t) 'open)))

(defun mmm-erb-orig-indent-function (mode)
  (cadr (assoc 'indent-line-function (get mode 'mmm-local-variables))))

(defvar mmm-erb-offset-var-alist
  '((html-erb-mode . sgml-basic-offset)
    (nxml-mode . nxml-child-indent)))

(defun mmm-erb-indent-offset (mode)
  (let ((name (cdr (assoc mode mmm-erb-offset-var-alist))))
    (when name (symbol-value name))))

(provide 'mmm-erb)
