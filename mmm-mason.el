;;; mmm-mason.el --- MMM submode class for Mason components

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-mason.el,v 1.7 2000/05/10 08:08:15 mas Exp $

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

;; This file contains the definition of an MMM Mode submode class for
;; editing Mason components.

;;{{{ CPerl Workaround

;; Note that CPerl mode tends to think it starts in the middle of an
;; expression at the beginning of a submode region, and indents the
;; first statement a couple extra columns. A workaround I've found is
;; to begin all multiline Perl sections with an empty statement:

;; <%perl>;
;; print $var;
;; </%perl>

;; It isn't pretty, but it isn't ugly either, and it works better than
;; anything else I can think of.

;;}}}
;;{{{ PSGML Workaround

;; Michael Alan Dorman has reported problems with psgml-mode and
;; Mason. He suggests adding these lines to .emacs to turn it off.

;; (delete '("\\.html$" . sgml-html-mode) auto-mode-alist)
;; (delete '("\\.shtml$" . sgml-html-mode) auto-mode-alist)

;; Maybe these problems will be fixed in the future.

;;}}}
;;{{{ autohandlers and dhandlers

;; Another useful thing to do with Mason is putting autohandlers and
;; dhandlers in html-mode (and thus, assuming mmm-mode-ext-classes is
;; set up correctly, mmm-mode/mason). One way to do this is a line
;; such as

;; %# -*- mode: html; mmm-classes: mason -*-

;; at the top of each such file. Another option is to a line like this
;; to .emacs, which will put all files named "autohandler" or
;; "dhandler" into html-mode:

;; (add-to-list 'auto-mode-alist '("\\(auto\\|d\\)handler\\'" . html-mode))

;;}}}

;;; Code:

(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-auto)

;;{{{ Perl Tags

(defvar mmm-mason-perl-mode
  (if (fboundp 'cperl-mode) 'cperl-mode 'perl-mode)
  "What mode to use for Perl sections in Mason files.
Usually either `perl-mode' or `cperl-mode'. The default is
`cperl-mode' if that is available, otherwise `perl-mode'.")

(defvar mmm-mason-perl-tags
  '("perl" "init" "cleanup" "once" "filter" "shared"
    "perl_init" "perl_cleanup" "perl_once" "perl_filter"))

(defvar mmm-mason-pseudo-perl-tags
  '("args" "perl_args" "attr" "flags"))

(defvar mmm-mason-non-perl-tags
  '("doc" "perl_doc" "text" "perl_text" "def" "perl_def" "method"))

(defvar mmm-mason-perl-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-mason-perl-tags t) ">")
  "Matches tags beginning Mason sections containing Perl code.
Saves the name of the tag matched.")

(defvar mmm-mason-pseudo-perl-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-mason-pseudo-perl-tags t) ">")
  "Match tags beginning Mason sections that look like Perl but aren't.
Saves the name of the tag matched.")

(defvar mmm-mason-tag-names-regexp
  (regexp-opt (append mmm-mason-perl-tags mmm-mason-non-perl-tags) t)
  "Matches any Mason tag name after the \"<%\". Used to verify that a
\"<%\" sequence starts an inline section.")

(defun mmm-mason-verify-inline ()
  (not (looking-at mmm-mason-tag-names-regexp)))

;;}}}
;;{{{ Add Classes

(mmm-add-group
 'mason
 `((mason-text
    :submode nil
    :front "<%text>"
    :back "</%text>"
    :insert ((?t mason-<%text> nil @ "<%text>" @ "\n"
                 _ "\n" @ "</%text>" @)))
   (mason-doc
    :submode text-mode
    :front "<%doc>"
    :back "</%doc>"
    :face nil
    :insert ((?d mason-<%doc> nil @ "<%doc>" @ "\n"
                 _ "\n" @ "</%doc>" @)))
   (mason-perl
    :submode ,mmm-mason-perl-mode
    :front ,mmm-mason-perl-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :insert ((?, mason-<%TAG> "Perl section: " @ "<%" str ">" @
                 ";\n" _ "\n" @ "</%" str ">" @)
             (?< mason-<%TAG> ?, . nil)
             (?p mason-<%perl> ?, . "perl")
             (?i mason-<%init> ?, . "init")
             (?c mason-<%cleanup> ?, . "cleanup")
             (?o mason-<%once> ?, . "once")
             (?l mason-<%filter> ?, . "filter")
             (?s mason-<%shared> ?, . "shared")))
   (mason-pseudo-perl
    :submode ,mmm-mason-perl-mode
    :front ,mmm-mason-pseudo-perl-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :insert ((?. mason-pseudo-<%TAG> "Pseudo-perl section: " @ "<%" str ">" @
                 "\n" _ "\n" @ "</%" str ">" @)
             (?> mason-pseudo-<%TAG> ?, . nil)
             (?a mason-<%args> ?. . "args")
             (?f mason-<%flags> ?. . "flags")
             (?r mason-<%attr> ?. . "attr")))
   (mason-inline
    :submode ,mmm-mason-perl-mode
    :front "<%"
    :front-verify mmm-mason-verify-inline
    :back "%>"
    :insert ((?% mason-<%-%> nil @ "<%" @ " " _ " " @ "%>" @)
             (?5 mason-<%-%> ?% . nil)))
   (mason-call
    :submode ,mmm-mason-perl-mode
    :front "<&"
    :back "&>"
    :insert ((?& mason-<&-&> nil @ "<&" @ " " _ " " @ "&>" @)
             (?7 mason-<&-&> ?% . nil)))
   (mason-one-line
    :submode ,mmm-mason-perl-mode
    :front "^%"
    :back "\n"
    :insert ((return mason-%-line nil (mmm-mason-start-line)
                     @ "%" @ " " _ @ '(mmm-mason-end-line) "\n" @)
             (?# mason-%-comment nil (mmm-mason-start-line)
                 @ "%" @ "# " _ @ '(mmm-mason-end-line) "\n" @)
             (?3 mason-%-comment ?# . nil)))))

;;}}}
;;{{{ One-line Sections

(defun mmm-mason-start-line ()
  (if (= (point)
         (save-excursion (beginning-of-line) (point)))
      ""
    "\n"))

(defun mmm-mason-end-line ()
  (if (= (char-after (point)) (string-to-char "\n"))
      (delete-char 1)))

;;}}}

(provide 'mmm-mason)

;;; mmm-mason.el ends here