;;; mmm-class.el --- MMM submode class variables and functions

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-class.el,v 1.1 2000/04/27 10:33:27 mas Exp $

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

;; This file contains variable and function definitions for
;; manipulating and applying MMM submode classes. See `mmm-vars.el'
;; for variables that list classes.

;;; Code:

(require 'font-lock)
(require 'cl)
(require 'mmm-vars)
(require 'mmm-region)

;;{{{ Get Class Specifications

(defun mmm-get-class-spec (class)
  "Get the class specification for CLASS.
CLASS can be either a symbol to look up in `mmm-classes-alist' or a
class specifier itself."
  (cond ((symbolp class)        ; A symbol must be looked up
         (or (cdr (assq class mmm-classes-alist))
             (signal 'mmm-invalid-submode-class (list class))))
        ((listp class)          ; A list must be a class spec
         class)
        (t (signal 'mmm-invalid-submode-class (list class)))))

;;}}}
;;{{{ Apply Classes

(defun* mmm-apply-class
    (class &optional (start (point-min)) (stop (point-max)) face)
  "Apply the submode class CLASS from START to STOP in FACE."
  ;; The "special" class t means do nothing. It is used to turn on
  ;; MMM Mode without applying any classes.
  (unless (eq class t)
    (apply #'mmm-ify :start start :stop stop :face face
           (mmm-get-class-spec class))))

(defun* mmm-apply-classes
    (classes &key (start (point-min)) (stop (point-max)) face)
  "Apply all submode classes in CLASSES, in order.
All classes are applied regardless of any errors that may occur in
other classes. If any errors occur, `mmm-apply-classes' exits with an
error once all classes have been applied."
  (let (invalid-classes)
    (dolist (class classes)
      (condition-case err
          (mmm-apply-class class start stop face)
        (mmm-invalid-submode-class
         ;; Save the name of the invalid class, so we can report them
         ;; all together at the end.
         (add-to-list 'invalid-classes (second err)))))
    (when invalid-classes
      (signal 'mmm-invalid-submode-class invalid-classes))))

;;}}}
;;{{{ Apply All Classes

(defun* mmm-apply-all (&key (start (point-min)) (stop (point-max)))
  "MMM-ify START to STOP by mode/ext, `mmm-classes', and history."
  (mmm-clear-overlays start stop 'strict)
  (mmm-apply-classes (mmm-get-all-classes) :start start :stop stop)
  (mmm-update-current-submode)
  (mmm-refontify-maybe start stop))

(defun mmm-refontify-maybe (start stop)
  "Re-fontify from START to STOP."
  (and (featurep 'font-lock)
       font-lock-mode
       (if (or start stop)
           (font-lock-fontify-region (or start (point-min))
                                     (or stop (point-max)))
         (font-lock-fontify-buffer))))

;;}}}
;;{{{ Scan for Regions

(defun* mmm-ify
    (&rest all &key classes handler submode face
           (start (point-min)) (stop (point-max))
           front back save-matches (case-fold-search t)
           (beg-sticky (not (number-or-marker-p front)))
           (end-sticky (not (number-or-marker-p back)))
           (front-offset 0) (back-offset 0) front-verify back-verify
           front-form back-form
           &allow-other-keys)
  "Create submode regions from START to STOP according to arguments.
If CLASSES is supplied, it must be a list of valid CLASSes. Otherwise,
the rest of the arguments are for an actual class being applied. See
`mmm-classes-alist' for information on what they all mean."
  (cond
   ;; If we have a class list, apply them all.
   (classes
    (mmm-apply-classes classes :start start :stop stop :face face))
   ;; Otherwise, apply this class.
   ;; If we have a handler, call it.
   (handler
    (apply handler all))
   ;; Otherwise, we search from START to STOP for submode regions,
   ;; continuining over errors, until we don't find any more. If FRONT
   ;; and BACK are number-or-markers, this should only execute once.
   (t
    (mmm-save-all
     (goto-char start)
     (loop for (beg end front-form back-form) =
           (apply #'mmm-match-region :start (point)
                  (mmm-save-keywords front back stop
                    save-matches front-offset back-offset front-verify
                    back-verify front-form back-form))
           while beg
           while (/= beg end)   ; Sanity check
           do
           (condition-case nil
               (mmm-make-region submode beg end :face face
                 :front front-form :back back-form
                 :beg-sticky beg-sticky :end-sticky end-sticky)
             ;; If our region is invalid, go back to the end of the front
             ;; match and continue on.
             (mmm-invalid-parent (goto-char (- beg front-offset)))))))))

;;}}}
;;{{{ Match Regions

(defun* mmm-match-region
    (&key front back start stop front-verify back-verify front-offset
          back-offset save-matches front-form back-form)
  "Find the first valid region between point and STOP.
Return \(BEG END FRONT-FORM BACK-FORM) specifying the region. See
`mmm-match-and-verify' for the valid values of FRONT and BACK
\(markers, regexps, or functions)."
  (when (mmm-match-and-verify front start stop front-verify)
    (let ((beg (+ (match-end 0) front-offset))
          (front-form (mmm-get-form front-form)))
      (when (mmm-match-and-verify (mmm-format-matches back save-matches)
                                  beg stop back-verify)
        (let ((end (+ (match-beginning 0) back-offset))
              (back-form (mmm-get-form back-form)))
          (values beg end front-form back-form))))))

(defun mmm-match-and-verify (pos start stop &optional verify)
  "Find first match for POS between point and STOP satisfying VERIFY.
Return non-nil if a match was found, and set match data. POS can be a
number-or-marker, a regexp, or a function.

If POS is a number-or-marker, it is used as-is. If it is a string, it
is searched for as a regexp until VERIFY returns non-nil. If it is a
function, it is called with argument STOP and must return non-nil iff
a match is found, and set the match data. Note that VERIFY is ignored
unless POS is a regexp."
  (cond
   ;; A marker can be used as-is, but only if it's in bounds.
   ((and (number-or-marker-p pos) (>= pos start) (<= pos stop))
    (goto-char pos)
    (looking-at ""))            ; Set the match data
   ;; Strings are searched for as regexps.
   ((stringp pos)
    (loop always (re-search-forward pos stop 'limit)
          until (or (not verify) (mmm-save-all (funcall verify)))))
   ;; Otherwise it must be a function.
   ((functionp pos)
    (funcall pos stop))))

;;}}}
;;{{{ Get Delimiter Forms

(defun mmm-get-form (form)
  "Return the delimiter form specified by FORM.
If FORM is nil, call `mmm-default-get-form'. If FORM is a string,
return it. If FORM is a function, call it. If FORM is a list, return
its `car' \(usually in this case, FORM is a one-element list
containing a function to be used as the delimiter form."
  (cond ((stringp form) form)
        ((not form) (mmm-default-get-form))
        ((functionp form) (funcall form))
        ((listp form) (car form))))

(defun mmm-default-get-form ()
  (regexp-quote (match-string 0)))

;;}}}

(provide 'mmm-class)

;;; mmm-class.el ends here