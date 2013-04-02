(ert-deftest mmm-beginning-of-syntax-goes-to-outer-beginning ()
  (ert-with-test-buffer nil
    (let (mmm-mode-ext-classes-alist
          mmm-parse-when-idle)
      (insert "aaa [zz {x} zz] bbb")
      (fundamental-mode)
      (mmm-mode-on)
      (mmm-ify-by-regexp 'text-mode "\\[" 0 "\\]" 0 nil)
      (mmm-ify-by-regexp 'prog-mode "{" 0 "}" 0 nil)
      (search-backward "}")
      (mmm-beginning-of-syntax)
      (should (looking-back "\\[" (1- (point)))))))
