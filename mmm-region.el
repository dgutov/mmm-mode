;;; mmm-region.el --- Manipulating and behavior of MMM submode regions

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-region.el,v 1.2 2000/04/30 01:47:04 mas Exp $

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

;; This file provides the functions and variables to create, delete,
;; and inspect submode regions, as well as functions that make them
;; behave like the submode with respect to syntax tables, local maps,
;; font lock, etc.

;;; Code:

(require 'cl)
(require 'mmm-compat)
(require 'mmm-utils)
(progn
  (require 'mmm-vars))

;; CREATION & DELETION
;;{{{ Markers

(defun mmm-make-marker (pos beg-p sticky-p)
  "Make a marker at POS that is or isn't sticky.
BEG-P represents whether the marker delimits the beginning of a
region \(or the end of it). STICKY-P is whether it should be sticky,
i.e. whether text inserted at the marker should be inside the region."
  (let ((mkr (set-marker (make-marker) pos)))
    (set-marker-insertion-type mkr (if beg-p (not sticky-p) sticky-p))
    mkr))

;;}}}
;;{{{ Make Submode Regions

(defun* mmm-make-region
    (submode beg end &rest rest &key (front "") (back "")
             (beg-sticky t) (end-sticky t) face
             &allow-other-keys)
  "Make a submode region from BEG to END of SUBMODE in FACE.
FACE defaults to `mmm-default-submode-face'. FRONT and BACK are
regexps or functions to match the correct delimiters--see
`mmm-match-front' and `mmm-match-back'. BEG-STICKY and END-STICKY
determine whether the front and back of the region, respectively, are
sticky with respect to new insertion. All other keyword arguments are
stored as properties of the overlay, un-keyword-ified."
  (mmm-mode-on)
  ;; For now, complain about overlapping regions. Most callers should
  ;; trap this and continue on. In future, submode regions will be
  ;; allowed to sit inside others.
  (when (mmm-overlays-in beg end)
    (signal 'mmm-invalid-parent nil))
  (when submode
    (mmm-update-mode-info submode))
  ;; Conditionally sticky overlays are by default sticky. Then the
  ;; insert-in-front and -behind functions fix them.
  (let ((ovl (make-overlay beg end nil (not beg-sticky) end-sticky)))
    ;; This loop covers front, back, beg-sticky, end-sticky, and
    ;; anything else the caller wants to put on the overlay. It also
    ;; does face, but we re-do that later because we want to
    ;; defaultify it.
    (loop for (var val) on rest by #'cddr
          do (overlay-put ovl (intern (substring (symbol-name var) 1)) val))
    (mapcar #'(lambda (pair) (overlay-put ovl (car pair) (cadr pair)))
            `((mmm t)           ; Mark our overlays
              (mmm-mode ,submode)
              ;; These have special meaning to Emacs
              (,mmm-evaporate-property t)
              (face ,(or face (if submode 'mmm-default-submode-face)))
              ))
    (when submode
      (save-excursion
        (goto-char (overlay-start ovl))
        (mmm-run-submode-hook submode)))
    (mmm-update-current-submode)
    ovl))

;;}}}
;;{{{ Clear Overlays

;; See also `mmm-clear-current-region'.

(defun mmm-clear-overlays (&optional start stop strict)
  "Clears all MMM overlays between START and STOP.
If STRICT, only clear those strictly included, rather than partially."
  (mapcar #'delete-overlay
        (mmm-overlays-in (or start (point-min))
                         (or stop (point-max))
                         strict))
  (mmm-update-current-submode))

;;}}}

;; INSPECTION
;;{{{ Current Overlays

;; Emacs counts an overlay starting at POS as "at" POS, but not an
;; overlay ending at POS. XEmacs is more sensible and uses beg- and
;; end-stickiness to determine whether an endpoint is within an
;; extent. Here we want to act like XEmacs does.

(defun mmm-overlay-at (&optional pos type)
  "Return the highest-priority MMM Mode overlay at POS.
TYPE is passed on to `mmm-overlays-at', which see."
  (car (mmm-overlays-at (or pos (point)) type)))

(defun mmm-overlays-at (&optional pos type)
  "Return a list of the MMM overlays at POS, in decreasing priority.
TYPE should be nil, `beg', `end', `none', or `all'. If `none', return
only overlays strictly including POS. If nil, return overlays starting
at POS only if they are beg-sticky, and those ending at POS only if
they are end-sticky. If `beg', return all overlays starting at POS but
none ending at POS, if `end', return all overlays ending at POS
but none starting at POS, and if `all', return both."
  (or pos (setq pos (point)))
  (remove-if-not #'(lambda (ovl)
                     (mmm-included-p ovl pos type))
                 (mmm-overlays-in (1- pos) (1+ pos))))

(defun mmm-included-p (ovl pos type)
  (cond ((eql (overlay-start ovl) pos)
         (case type
           ((none end) nil)
           ((nil) (overlay-get ovl 'beg-sticky))
           ((beg all) t)))
        ((eql (overlay-end ovl) pos)
         (case type
           ((none beg) nil)
           ((nil) (overlay-get ovl 'end-sticky))
           ((end all) t)))
        (t t)))

(defun mmm-overlays-in (start stop &optional strict)
  "Return the MMM overlays in START to STOP, in decreasing priority.
If STRICT is non-nil, include only those overlays which are entirely
contained in the region, including their delimiters \(if any)."
  (mmm-sort-overlays
   (remove-if-not #'(lambda (ovl)
                      (and (overlay-get ovl 'mmm)
                           (or (not strict)
                               (>= stop (mmm-back-end ovl))
                               (<= start (mmm-front-start ovl)))))
                  (overlays-in start stop))))

(defun mmm-sort-overlays (overlays)
  "Sort OVERLAYS in order of decreasing priority."
  (sort (copy-list overlays)
        #'(lambda (x y) (> (or (overlay-get x 'priority) 0)
                           (or (overlay-get y 'priority) 0)))))

;;}}}
;;{{{ Current Submode

(defvar mmm-current-submode nil
  "What submode we think we are currently in.
May be out of date; call `mmm-update-current-submode' to correct it.")
(make-variable-buffer-local 'mmm-current-submode)

(defun mmm-update-current-submode (&optional pos)
  "Set the `mmm-current-submode' to the `mmm-submode-at' POS. 
Return non-nil iff the value changed."
  (not (eq (prog1 mmm-current-submode
	     (setq mmm-current-submode
                   (mmm-submode-at (or pos (point)))))
	   mmm-current-submode)))

(defun mmm-submode-at (&optional pos type)
  "Return the submode at POS \(or point), or NIL if none.
TYPE is passed on to `mmm-overlays-at', which see."
  (let ((ovl (mmm-overlay-at (or pos (point)) type)))
    (if ovl (overlay-get ovl 'mmm-mode))))

;;}}}
;;{{{ Match Front & Back

(defun mmm-match-front (ovl)
  "Return non-nil if the front delimiter of OVL matches as it should.
Sets the match data to the front delimiter, if it is a regexp,
otherwise calls it as a function with point at the beginning of the
overlay and one argument being the overlay. The function should return
non-nil if the front delimiter matches correctly, and set the match
data appropriately."
  (let ((front (overlay-get ovl 'front)))
    (save-excursion
      (goto-char (overlay-start ovl))
      (if (stringp front)
          ;; It's a regexp
          (mmm-looking-back-at front)
        ;; It's a function
        (funcall front ovl)))))

(defun mmm-match-back (ovl)
  "Return non-nil if the back delimiter of OVL matches as it should.
Sets the match data to the back delimiter, if it is a regexp,
otherwise calls it as a function with point at the end of the overlay
and one argument being the overlay. The function should return non-nil
if the back delimiter matches correctly, and set the match data
appropriately."
  (let ((back (overlay-get ovl 'back)))
    (save-excursion
      (goto-char (overlay-end ovl))
      (if (stringp back)
          ;; It's a regexp
          (looking-at back)
        (funcall back ovl)))))

;;}}}
;;{{{ Delimiter Boundaries

(defun mmm-front-start (ovl)
  "Return the position at which the front delimiter of OVL starts.
If OVL is not front-bounded correctly, return its start position."
  (save-match-data
    (if (mmm-match-front ovl)
        (match-beginning 0)
      (overlay-start ovl))))

(defun mmm-back-end (ovl)
  "Return the position at which the back delimiter of OVL ends.
If OVL is not back-bounded correctly, return its end position."
  (save-match-data
    (if (mmm-match-back ovl)
        (match-end 0)
      (overlay-end ovl))))

;;}}}

;; BASIC UPDATING
;;{{{ Submode Info

(defun mmm-update-mode-info (mode)
  "Make sure the `mmm-*' properties of MODE are present.
These properties are used to store the required information about the
mode for it to be a submode or a major mode with submodes."
  (unless (get mode 'mmm-mode-name)
    (save-excursion
      (set-buffer (get-buffer-create "*mmm-temp*"))
      (funcall mode)
      (when (featurep 'font-lock)
        ;; XEmacs doesn't have global-font-lock-mode (or rather, it
        ;; has nothing but global-font-lock-mode).
        (unless mmm-xemacs (turn-on-font-lock-if-enabled))
        ;; Ensure font-lock-variables are present, and get them.
        (font-lock-set-defaults)
        (loop for (prop value) in (mmm-get-font-lock-properties)
              do (put mode prop value)))
      ;; Get non-font-lock information
      (loop for (prop value) in (mmm-get-mode-properties)
            do (put mode prop value))
      (kill-buffer (current-buffer)))))

(defun mmm-get-mode-properties ()
  `((mmm-syntax-table ,(syntax-table))
    (mmm-local-map ,(current-local-map))
    (mmm-local-variables ,(mmm-get-local-variables))
    (mmm-mode-name ,mode-name)))
(defun mmm-get-font-lock-properties ()
  `((mmm-fontify-region-function ,font-lock-fontify-region-function)
    (mmm-beginning-of-syntax-function ,font-lock-beginning-of-syntax-function)
    (mmm-font-lock-mode ,font-lock-mode)))

;;}}}
;;{{{ Updating Hooks

(defun mmm-update-submode-region ()
  "Update all MMM properties correctly for the current position.
This function does the actual work of setting the different local
maps, syntax tables, etc. for submodes."
  ;; This next line is necessary because some derived modes can fool
  ;; MMM Mode into thinking they're really the parent mode. For
  ;; example, texinfo-mode looks like text-mode to the major mode
  ;; hook, and hence doesn't get its properties updated.
  (mmm-update-mode-info major-mode)
  (when (mmm-update-current-submode)
    (if mmm-current-submode
	(setq mode-name
	      (mmm-format-string mmm-submode-mode-line-format
                `(("~M" . ,(get major-mode 'mmm-mode-name))
                  ("~m" . ,(get mmm-current-submode 'mmm-mode-name)))))
      (setq mode-name (get major-mode 'mmm-mode-name)))
    (mmm-update-for-mode (or mmm-current-submode major-mode) t)))

(defun mmm-update-for-mode (mode &optional fontify)
  (mmm-update-mode-info mode)
  (set-syntax-table (get mode 'mmm-syntax-table))
  (mmm-real-use-local-map (or (cdr (assoc mode mmm-local-maps-alist))
                              (get mode 'mmm-local-map)))
  (mmm-set-local-variables mode)
  (and (featurep 'font-lock)
       fontify
       (get mode 'mmm-font-lock-mode)
       (font-lock-mode 1)))

(defun mmm-add-hooks ()
  (make-local-hook 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'mmm-mode-off nil 'local)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'mmm-update-submode-region nil 'local))

(defun mmm-remove-hooks ()
  (remove-hook 'change-major-mode-hook 'mmm-mode-off 'local)
  (remove-hook 'post-command-hook 'mmm-update-submode-region 'local))

;;}}}
;;{{{ Local Variables

(defun mmm-set-local-variables (mode)
  "Set the local variables saved for MODE."
  (mapcar #'(lambda (var)
              (make-local-variable (car var))
              (set (car var) (cadr var)))
          (get mode 'mmm-local-variables)))

(defun mmm-get-local-variables ()
  "Get the local variables to save from this buffer."
  (mapcar #'(lambda (var)
	      (list var (and (boundp var)
			     (symbol-value var))))
	  mmm-save-local-variables))

;;}}}
;;{{{ Local Maps

;; This is for the benefit of commands such as `vm-mail', which calls
;; `mail-mode' but then changes the local map afterwards. It's kludgy,
;; I know, but at the moment I don't have time to think of a neater
;; solution.

(defvar mmm-local-maps-alist ()
  "Which local maps have been changed in this buffer.")
(make-variable-buffer-local 'mmm-local-maps-alist)

;; Save the real function away for our use.
(fset 'mmm-real-use-local-map (symbol-function 'use-local-map))

(defadvice use-local-map (after mmm-keep-record activate compile)
  "Keep track of which local maps have been changed in which buffers."
  (mmm-valid-buffer
   (mmm-update-current-submode)
   (let* ((mode (or mmm-current-submode major-mode))
          (map (assq mode mmm-local-maps-alist)))
     (if map
         (setcdr map (current-local-map))
       (push (cons mode (current-local-map)) mmm-local-maps-alist)))))

;;}}}

;; FONT LOCK
;;{{{ Get Submode Regions

(defun mmm-submode-changes-in (start stop)
  "Return a list of all submode-change positions from START to STOP.
The list is sorted in order of increasing buffer position."
  (sort (remove-duplicates
         (list* start stop
                (mapcan #'(lambda (ovl)
                            `(,(overlay-start ovl)
                              ,(overlay-end ovl)))
                        (mmm-overlays-in start stop))))
        #'<))

(defun mmm-regions-in (start stop)
  "Return a list of regions of the form (MODE BEG END) whose disjoint
union covers the region from START to STOP."
  (let ((regions 
         (maplist #'(lambda (pos-list)
                      (if (cdr pos-list)
                          (list (or (mmm-submode-at (car pos-list) 'beg)
                                    major-mode)
                                (car pos-list) (cadr pos-list))))
                  (mmm-submode-changes-in start stop))))
    (setcdr (last regions 2) nil)
    regions))


(defun mmm-regions-alist (start stop)
  "Return a list of lists of the form \(MODE . REGIONS) where REGIONS
is a list of elements of the form \(BEG END). The disjoint union all
of the REGIONS covers START to STOP."
  (let ((regions (mmm-regions-in start stop)))
    (mapcar #'(lambda (mode)
                (cons mode
                      (mapcan #'(lambda (region)
                                  (if (eq mode (car region))
                                      (list (cdr region))))
                              regions)))
            ;; All the modes
            (remove-duplicates (mapcar #'car regions)))))

;;}}}
;;{{{ Fontify Regions

(defun mmm-fontify-region (start stop &optional loudly)
  "Fontify from START to STOP keeping track of submodes correctly."
  (when loudly
    (message "Fontifying %s with submode regions..." (buffer-name)))
  ;; For some reason `font-lock-fontify-block' binds this to nil, thus
  ;; preventing `mmm-beginning-of-syntax' from doing The Right Thing.
  ;; I don't know why it does this, but let's undo it here.
  (let ((font-lock-beginning-of-syntax-function 'mmm-beginning-of-syntax))
    (mapcar #'(lambda (elt)
                (when (get (car elt) 'mmm-font-lock-mode)
                  (mmm-fontify-region-list (car elt) (cdr elt))))
            (mmm-regions-alist start stop)))
  (mmm-update-for-mode (or mmm-current-submode major-mode))
  (when loudly (message nil)))

(defun mmm-fontify-region-list (mode regions)
  "Fontify REGIONS, each like \(BEG END), in mode MODE."
  (save-excursion
    (let ((major-mode mode)
          (func (get mode 'mmm-fontify-region-function)))
      (mmm-update-for-mode major-mode)
      (mapcar #'(lambda (reg)
                  (funcall func (car reg) (cadr reg) nil))
              regions))))
;;}}}
;;{{{ Beginning of Syntax

(defun mmm-beginning-of-syntax ()
  (goto-char
   (let ((ovl (mmm-overlay-at (point)))
         (func (get (or mmm-current-submode major-mode)
                    'mmm-beginning-of-syntax-function)))
     (max (if ovl (overlay-start ovl) (point-min))
          (if func (progn (funcall func) (point)) (point-min))
          (point-min)))))

;;}}}

(provide 'mmm-region)

;;; mmm-region.el ends here