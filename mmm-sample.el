;;; mmm-sample.el --- Sample MMM submode classes

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-sample.el,v 1.3 2000/04/30 08:03:11 mas Exp $

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains several sample submode classes for use with MMM
;; Mode. For a more detailed and useful example, see `mmm-mason.el'.

;;; Code:

(when t
  (require 'mmm-auto))

;;{{{ CSS embedded in HTML

;; This is the simplest example. Many applications will need no more
;; than a simple regexp.
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))

;;}}}
;;{{{ HTML Here-documents

;; A little more complicated regexp to match all Perl here-documents
;; whose identifiers include the string "HTML". We use :save-matches
;; to match the end of the named here-document. This class has not
;; been extensively tested.

(mmm-add-classes
 '((perl-html
    :submode html-mode
    :front "<<\\([a-zA-Z0-9_-]*HTML[a-zA-Z0-9_-]*\\).*\n"
    :back "^~1$"
    :save-matches 1)))

;;}}}
;;{{{ Javascript in HTML

(defvar mmm-javascript-mode
  (if (fboundp 'javascript-mode) 'javascript-mode 'c++-mode)
  "What mode to use for Javascript regions.
The default is `javascript-mode' if there is a function by that name,
otherwise `c++-mode'.  Some people prefer `c++-mode' regardless.")

;; We use two classes here, one for code in a <script> tag and another
;; for code embedded as a property of an HTML tag, then another class
;; to group them together.
(mmm-add-group
 'html-js
 `((js-tag
    :submode ,mmm-javascript-mode
    :front "<script\[^>\]*>"
    :back"</script>")
   (js-inline
    :submode ,mmm-javascript-mode
    :front "on\w+=\""
    :back "\"")))

;;}}}

;; NOT YET UPDATED
;;{{{ ELisp in File Variables;-COM-
;-COM-
;-COM-;; This example is more complicated. It uses a function rather than a
;-COM-;; regexp to find the regions. The idea is that the sexp after an
;-COM-;; 'eval:' file variable should be in Emacs Lisp Mode, since it's
;-COM-;; getting executed as Emacs Lisp.
;-COM-
;-COM-(defun* mmm-elisp-in-file-vars (start end)
;-COM-  (save-match-data
;-COM-    (goto-char (point-max))
;-COM-    (backward-page)
;-COM-    ;; Fool the compiler so it doesn't think this is a local
;-COM-    ;; variables list.
;-COM-    (when (search-forward "Local\ Variables:" end t)
;-COM-      (goto-char (max (point) start))
;-COM-      (mmm-find-evals (min (or (save-excursion (search-forward "End:" nil t))
;-COM-                               (return-from mmm-elisp-in-file-vars))
;-COM-                           end)))))
;-COM-
;-COM-(defun mmm-find-evals (bound)
;-COM-  (if (search-forward "eval: " bound t)
;-COM-      (cons (cons (prog1 (point) (forward-sexp)) (point))
;-COM-            (mmm-find-evals bound))
;-COM-    nil))
;-COM-
;-COM-(add-to-list 'mmm-classes-alist
;-COM-  '(eval-elisp (:function emacs-lisp-mode mmm-elisp-in-file-vars)))
;-COM-
;;}}}
;;{{{ HTML in PL/SQL;-COM-
;-COM-
;-COM-;; This one is the most complex example. In PL/SQL, HTML is generally
;-COM-;; output as a (single quote delimited) string inside a call to htp.p or
;-COM-;; its brethren. The problem is that there may be strings outside of
;-COM-;; htp.p calls that should not be HTML, so we need to only look inside
;-COM-;; these calls. The situation is complicated by PL/SQL's rule that two
;-COM-;; sequential single quotes in a string mean to put a single quote
;-COM-;; inside the string.
;-COM-
;-COM-;; These functions have not been thoroughly tested, and always search
;-COM-;; the entire buffer, ignoring START and END.
;-COM-
;-COM-(defun mmm-html-in-plsql (start end)
;-COM-  (save-match-data
;-COM-    (let ((case-fold-search t))
;-COM-      (and (re-search-forward "htp.p\\(\\|rn\\|rint\\)1?(" nil t)
;-COM-           (mmm-html-in-plsql-in-htp
;-COM-            ;; Find the end of the procedure call
;-COM-            (save-excursion (forward-char -1) (forward-sexp) (point))
;-COM-            start end)))))
;-COM-
;-COM-(defun mmm-html-in-plsql-in-htp (htp-end start end)
;-COM-  (let (beg end)
;-COM-    (or (and (re-search-forward "'" htp-end 'limit)
;-COM-	     (setf beg (match-end 0))
;-COM-	     ;; Find an odd number of 's to end the string.
;-COM-	     (do ((lgth 0 (length (match-string 0))))
;-COM-		 ((oddp lgth) t)
;-COM-	       (re-search-forward "'+" nil t))
;-COM-	     (setf end (1- (match-end 0)))
;-COM-	     (cons (cons beg end)
;-COM-		   (mmm-html-in-plsql-in-htp htp-end start end)))
;-COM-	;; No more strings in the procedure call; look for another.
;-COM-	(and (eql (point) htp-end)
;-COM-	     (mmm-html-in-plsql start end)))))
;-COM-
;-COM-(add-to-list 'mmm-classes-alist
;-COM-  '(htp-p (:function html-mode mmm-html-in-plsql)))
;-COM-
;;}}}

(provide 'mmm-sample)

;;; mmm-sample.el ends here