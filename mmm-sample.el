;;; mmm-sample.el --- Sample MMM submode classes

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-sample.el,v 1.7 2000/07/23 05:33:24 mas Exp $

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

(require 'mmm-auto)

;;{{{ CSS embedded in HTML

;; This is the simplest example. Many applications will need no more
;; than a simple regexp.
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))

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
    :back"</script>"
    :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode ,mmm-javascript-mode
    :front "on\w+=\""
    :back "\"")))

;;}}}
;;{{{ Here-documents

;; Here we match the here-document syntax used by Perl and shell
;; scripts.  We assume that the delimiter of the here-document is the
;; mode name with or without `-mode', such as <<HTML or <<HTML-MODE.
;; Hyphens may be replaced with underscores to make <<HTML_MODE and
;; the mode name is converted to lower case.  Additional characters
;; may be added after `MODE', such as <<HTML_MODE_EOF.

(defun mmm-here-doc-get-mode (string)
  (string-match "[a-zA-Z_-]+" string)
  (setq string (match-string 0 string))
  (let* ((case-fold-search t)
         (modestr (if (string-match "mode" string)
                      (downcase (substring string 0 (match-end 0)))
                    (concat (downcase string) "-mode"))))
    (while (string-match "_" modestr)
      (setq modestr (replace-match "-" nil nil modestr)))
    (setq modestr (intern modestr))
    (if (fboundp modestr)
        modestr
      (signal 'mmm-no-matching-submode nil))))

(mmm-add-classes
 '((here-doc
    :front "<<\\([a-zA-Z0-9_-]+\\).*\n"
    :back "^~1$"
    :save-matches 1
    :match-submode mmm-here-doc-get-mode
    :insert ((?d here-doc "Here-document Name: " @ "<<" str _ "\n"
                 @ "\n" @ str "\n" @))
    )))

;;}}}
;;{{{ Embperl

(defvar mmm-embperl-perl-mode
  (if (fboundp 'cperl-mode) 'cperl-mode 'perl-mode)
  "What mode to use for Perl sections in Embperl files.
Usually either `perl-mode' or `cperl-mode'. The default is
`cperl-mode' if that is available, otherwise `perl-mode'.")

(mmm-add-group
 'embperl
 '((embperl-perl
    :submode cperl-mode
    :front "\\[\\([-\\+!\\*\\$]\\)"
    :back "~1\\]"
    :save-matches 1
    :insert ((?p embperl "Region Type (Character): " @ "[" str
                 @ " " _ " " @ str "]" @)
             (?+ embperl+ ?p . "+")
             (?- embperl- ?p . "-")
             (?! embperl! ?p . "!")
             (?* embperl* ?p . "*")
             (?$ embperl$ ?p . "$")
             )
    )
   (embperl-comment
    :submode text-mode
    :front "\\[#"
    :back "#\\]"
    :insert ((?# embperl-comment nil @ "[#" @ " " _ " " @ "#]" @))
    )))

;;}}}
;;{{{ File Variables

;; This submode class puts file local variable values, specified with
;; a `Local Variables:' line as in (emacs)File Variables, into Emacs
;; Lisp Mode.  It is a good candidate to put in `mmm-global-classes'.

(defun mmm-file-variables-verify ()
  (let ((bounds
         (save-excursion
           (save-match-data
             (goto-char (point-max))
             (backward-page)
             (search-forward "Local Variables:")
             (cons (point)
                   (progn (search-forward "End:")
                          (beginning-of-line)
                          (point)))))))
    (and (> (match-beginning 0) (car bounds))
         (< (match-end 0) (cdr bounds)))))

(defun mmm-file-variables-find-back (bound)
  (forward-sexp)
  (if (> (point) bound)
      nil
    (looking-at "")
    t))

(mmm-add-classes
 '((file-variables
    :front "^[a-zA-Z-*]+: "
    :front-verify mmm-file-variables-verify
    :back mmm-file-variables-find-back
    :submode emacs-lisp-mode
    )))

;;}}}

;; NOT YET UPDATED
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