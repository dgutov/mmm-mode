;;; mmm-vars.el --- Variables for MMM Mode

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-vars.el,v 1.6 2000/06/23 22:58:54 mas Exp $

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

;; This file provides the definitions for the variables used by MMM
;; Mode, as well as several functions to manipulate them. It also
;; defines the errors that MMM Mode can signal.

;;; Code:

(require 'mmm-compat)

;; MISCELLANEOUS
;;{{{ Shut up the Byte Compiler

;; Otherwise it complains about undefined variables.
(eval-when-compile
  (defvar mmm-save-local-variables)
  (defvar mmm-mode-string)
  (defvar mmm-submode-mode-line-format)
  (defvar mmm-mode-ext-classes-alist)
  (defvar mmm-mode-prefix-key)
  (defvar mmm-global-mode)
  (defvar mmm-classes-alist))

;;}}}
;;{{{ Error Conditions

;; Signalled when we try to put a submode region inside one where it
;; isn't meant to go.
(put 'mmm-invalid-parent
     'error-conditions
     '(mmm-invalid-parent mmm-error error))
(put 'mmm-invalid-parent
     'error-message
     "Invalid submode region parent")

;; Signalled when we try to apply a submode class that doesn't exist.
(put 'mmm-invalid-submode-class
     'error-conditions
     '(mmm-invalid-submode-class mmm-error error))
(put 'mmm-invalid-submode-class
     'error-message
     "Invalid or undefined submode class")

;;}}}

;; USER VARIABLES
;;{{{ Customization Group

(defgroup mmm nil
  "Multiple Major Modes in one buffer."
  :group 'tools)

;;}}}
;;{{{ Save Local Variables

(defcustom mmm-save-local-variables 
  '(comment-start 
    comment-end
    comment-line-start-skip     ; For fortran-mode
    comment-start-skip
    comment-column
    comment-indent-function
    comment-line-break-function
    sentence-end
    font-lock-keywords
    font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-syntax-table
    font-lock-mark-block-function       ; Replace this?
    font-lock-syntactic-keywords
    indent-line-function
    skeleton-transformation)
  "Which local variables to save for secondary major modes.
Changing the value of this variable after MMM Mode has been activated
in some buffer may produce unpredictable results."
  :group 'mmm
  :type '(repeat (symbol :tag "Variable")))

;;}}}
;;{{{ Default Submode Face

(defface mmm-default-submode-face 
  '(
    (t (:background "gray85"))
    )
  "Face used to indicate submode overlays by default.
This can be overridden for specific submodes created by any method;
see the documentation for that method. It is recommended that only the
background color be set for this face, in order not to mess with
font-lock too much."
  :group 'mmm)


;;}}}
;;{{{ Mode Line Format

(defcustom mmm-mode-string " MMM"
  "*String to display in mode line as MMM minor mode indicator."
  :group 'mmm
  :type 'string)

(defcustom mmm-submode-mode-line-format "~M[~m]"
  "*Format of the Major Mode Mode-line display when point is in a
submode region. ~M means the name of the default major mode, ~m means
the name of the submode."
  :group 'mmm
  :type 'string)

;;}}}
;;{{{ Submode Classes

(defvar mmm-classes nil
  "*List of classes of submodes that apply to a file.
Generally set in a local variables list. Can either be one symbol, or
a list of symbols. Automatically buffer-local.")
(make-variable-buffer-local 'mmm-classes)

;;}}}
;;{{{ Modes and Extensions

(defcustom mmm-mode-ext-classes-alist nil
  "Alist of submode classes for major modes and/or file extensions.
This variable can now be directly modified.

Elements look like \(MODE EXT CLASS), where MODE is a major mode, EXT
is a regexp to match a filename such as in `auto-mode-alist', and
CLASS is a submode class. CLASS is activated in all buffers in mode
MODE \(if non-nil) and whose filenames match EXT \(if non-nil). If
both MODE and EXT are nil, CLASS is activated in all buffers. If CLASS
is the symbol t, MMM Mode is turned on in all buffers matching MODE
and EXT, but no classes are activated.

See `mmm-global-mode'."
  :group 'mmm
  :type '(repeat (list (symbol :tag "Major Mode")
                       (string :tag "Filename Regexp")
                       (symbol :tag "Class")))
  :require 'mmm-mode)

(defun mmm-add-mode-ext-class (mode ext class)
  "Add an element to `mmm-mode-ext-classes-alist', which see.
That variable can now be directly modified, so this function is
unnecessary. It probably won't go away, though."
  (if (assq class mmm-classes-alist)
      (add-to-list 'mmm-mode-ext-classes-alist (list mode ext class))
    (signal 'mmm-invalid-submode-class (list class))))

;;}}}
;;{{{ Key Bindings

(defcustom mmm-mode-prefix-key [(control ?c) ?%]
  "Prefix key for the MMM Minor Mode Keymap."
  :group 'mmm
  :type 'vector)

(defcustom mmm-command-modifiers '(control)
  "List of key modifiers for MMM command keys.
The MMM commands in the MMM Mode map, after `mmm-mode-prefix-key',
are bound to default keys with these modifiers added. This variable
must be set before MMM Mode is loaded to have an effect.

It is suggested that the value of this variable be either nil or
\(control), as the default keys are either plain keys or have only a
meta modifier. The shift modifier is not particularly portable between
Emacsen. The values of this variable and `mmm-insert-modifiers' should
be disjoint."
  :group 'mmm
  :type '(repeat (symbol :tag "Modifier")))

(defcustom mmm-insert-modifiers '()
  "List of key modifiers for MMM submode insertion keys.
When a key pressed after `mmm-mode-prefix-key' has no MMM Mode command
binding, and its modifiers include these, then its basic type, plus any
modifiers in addition to these, is looked up in classes' :insert
specifications.

It is suggested that the value of this variable be either nil or
\(control), allowing submode classes to specify the presence or
absence of the meta modifier. The shift modifier is not particularly
portable between Emacsen. The values of `mmm-command-modifiers' and
this variable should be disjoint."
  :group 'mmm
  :type '(repeat (symbol :tag "Modifier")))

(defcustom mmm-use-old-command-keys nil
  "Non-nil means to Use the old command keys for MMM Mode.
MMM Mode commands then have no modifier while insertion commands have
a control modifier, i.e. `mmm-command-modifiers' is set to nil and
`mmm-insert-modifiers' is set to \(control). If nil, the values of
these variables are as the default, or whatever the user has set them
to. This variable must be set before MMM Mode is loaded."
  :group 'mmm
  :type 'boolean)

(defun mmm-use-old-command-keys ()
  "Use the old command keys \(no control modifer) in MMM Mode."
  (setq mmm-command-modifiers '()
        mmm-insert-modifiers '(control)))

;;}}}
;;{{{ MMM Hooks

(defcustom mmm-mode-hook ()
  "Hook run when MMM Mode is enabled in a buffer.

A hook named mmm-<major-mode>-hook is also run, if it exists. For
example, `mmm-html-mode-hook' is run whenever MMM Mode is entered with
HTML mode the dominant mode.

A hook named mmm-<submode>-submode-hook is run when a submode region
of a given mode is created. For example, `mmm-cperl-mode-submode-hook'
is run whenever a CPerl mode submode region is created, in any buffer.
When submode hooks are run, point is guaranteed to be at the start of
the newly created submode region.

Finally, a hook named mmm-<class>-class-hook is run whenever a buffer
is first mmm-ified with a given submode class. For example,
`mmm-mason-class-hook' is run whenever the `mason' class is first
applied in a buffer."
  :group 'mmm
  :type 'hook)

(defun mmm-run-constructed-hook (body &optional suffix)
  "Run the hook named `mmm-<BODY>-<SUFFIX>-hook', if it exists.
If SUFFIX is nil or unsupplied, run `mmm-<BODY>-hook' instead."
  (let ((hook (intern-soft (if suffix
                               (format "mmm-%s-%s-hook" body suffix)
                             (format "mmm-%s-hook" body)))))
    (if hook (run-hooks hook))))

(defun mmm-run-major-hook ()
  (mmm-run-constructed-hook major-mode))

(defun mmm-run-submode-hook (submode)
  (mmm-run-constructed-hook submode "submode"))

(defvar mmm-class-hooks-run ()
  "List of submode classes for which hooks have already been run in
the current buffer.")
(make-variable-buffer-local 'mmm-class-hooks-run)

(defun mmm-run-class-hook (class)
  (unless (member class mmm-class-hooks-run)
    (mmm-run-constructed-hook class "class")
    (add-to-list 'mmm-class-hooks-run class)))

;;}}}
;;{{{ Major Mode Hook

(defcustom mmm-major-mode-hook ()
  "Hook run whenever a new major mode is finished starting up.
MMM Mode implements this with a hack \(see comments in the source) so
that `mmm-global-mode' will function correctly, but makes this hook
available so that others can take advantage of the hack as well.

Note that file local variables have *not* been processed by the time
this hook is run. If a function needs to inspect them, it should also
be added to `find-file-hooks'. However, `find-file-hooks' is not run
when creating a non-file-based buffer, or when changing major modes in
an existing buffer."
  :group 'mmm
  :type 'hook)

(defun mmm-run-major-mode-hook ()
  (run-hooks 'mmm-major-mode-hook))

;;}}}
;;{{{ MMM Global Mode

(defcustom mmm-global-mode nil
  "*Specify in which buffers to turn on MMM Mode automatically.

- If nil, MMM Mode is never enabled automatically.
- If t, MMM Mode is enabled automatically in all buffers.
- If any other symbol, MMM mode is enabled only in those buffers that
  have submode classes associated with them. See `mmm-classes' and
  `mmm-mode-ext-classes-alist' for more information."
  :group 'mmm
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (other :tag "Maybe" maybe))
  :require 'mmm-mode)

;;}}}
;;{{{ "Never" Modes

(defcustom mmm-never-modes
  '(help-mode
    Info-mode
    dired-mode
    comint-mode
    shell-mode)
  "List of modes in which MMM Mode is *never* activated."
  :group 'mmm
  :type '(repeat (symbol :tag "Mode")))

;;}}}

;; NON-USER VARIABLES
;;{{{ Classes Alist

;; :parent could be an all-class argument. Same with :keymap.
(defvar mmm-classes-alist nil
  "*Alist containing all defined mmm submode classes.
Each element looks like \(CLASS . ARGS) where CLASS is a symbol
representing the submode class and ARGS is a list of keyword
arguments, called a \"class specifier\". There are a large number of
accepted keyword arguments.

The argument CLASSES, if supplied, must be a list of other submode
classes \(or class specifiers), representing other classes to call.
FACE, if supplied, overrides FACE arguments to these classes, but all
other arguments to this class are ignored.

The argument HANDLER, if supplied, overrides any other processing. It
must be a function, and all the arguments are passed to it as
keywords, and it must do everything. See `mmm-ify' for what sorts of
things it must do. This back-door interface should be cleaned up.

The argument FACE, if supplied, overrides `mmm-default-submode-face'
in specifying the display face of the submode regions. It must be a
valid display face.

If CLASSES and HANDLER are not supplied, SUBMODE must be. It specifies
the submode to use for the submode regions, as a symbol such as
`cperl-mode' or `emacs-lisp-mode'.

FRONT and BACK are the means to find the submode regions, and can be
either buffer positions \(number-or-markers), regular expressions, or
functions. If they are absolute buffer positions, only one submode
region is created, from FRONT to BACK. This is generally not used in
named classes. \(Unnamed classes are created by interactive commands
in `mmm-interactive-history').

If FRONT is a regexp, then that regexp is searched for, and the end of
its match, plus FRONT-OFFSET, becomes the beginning of the submode
region. If FRONT is a function, that function is called instead, and
must act somewhat like a search, in that it should start at point,
take one argument as a search bound, and set the match data. A similar
pattern is followed for BACK, save that the end of the submode region
becomes the beginning of its match, plus BACK-OFFSET. FRONT- and
BACK-OFFSET default to 0.

FRONT-VERIFY and BACK-VERIFY, if supplied, must be functions that
inspect the match data to see if a match found by FRONT or BACK
respectively is valid.

If SAVE-MATCHES is supplied, it must be a number, and means to format
BACK, if it is a regexp, by replacing strings of the form \"~N\" by
the corresponding value of \(match-string n) after matching FRONT,
where N is between 0 and SAVE-MATCHES.

FRONT-FORM and BACK-FORM, if given, must supply a regexp used to match
the *actual* delimiter. If they are strings, they are used as-is. If
they are functions, they are called and must inspect the match data.
If they are lists, their `car' is taken as the delimiter. The default
for both is \(regexp-quote \(match-string 0)).

The last case is usually used for functions. Such a function must take
1-2 arguments, the first being the overlay in question, and the second
meaning to insert the delimiter and adjust the overlay rather than
just matching the delimiter. See `mmm-match-front', `mmm-match-back',
and `mmm-end-current-region'.

CASE-FOLD-SEARCH, if specified, controls whether the search is
case-insensitive. See `case-fold-search'. It defaults to `t'.

INSERT specifies the keypress insertion spec for such submode regions.
INSERT's value should be list of elements of the form \(KEY NAME .
SPEC). Each KEY should be either a character, a function key symbol,
or a dotted list \(MOD . KEY) where MOD is a symbol for a modifier
key. The use of any other modifier than meta is discouraged, as
`mmm-insert-modifiers' defaults to \(control), and other modifiers are
not very portable. Each NAME should be a symbol representing the
insertion for that key. Each SPEC can be either a skeleton, suitable
for passing to `skeleton-insert' to create a submode region, or a
dotted pair \(OTHER-KEY . ARG) meaning to use the skeleton defined for
OTHER-KEY but pass it the argument ARG as the `str' variable, possible
replacing a prompt string. Skeletons for insertion should have the
symbol `_' where point \(or wrapped text) should go, and the symbol
`@' in four different places: at the beginning of the front delimiter,
the beginning of the submode region, the end of the submode region,
and the end of the back delimiter.")

(defun mmm-add-classes (classes)
  "Add the submode classes CLASSES to `mmm-classes-alist'."
  (dolist (class classes)
    (add-to-list 'mmm-classes-alist class)))

(defun mmm-add-group (group classes)
  "Add CLASSES and a group named GROUP containing them all."
  (mmm-add-classes classes)
  (add-to-list 'mmm-classes-alist
               (list group :classes (mapcar #'first classes))))

;;}}}
;;{{{ Version Number

(defconst mmm-version "0.3.10"
  "Current version of MMM Mode.")

(defun mmm-version ()
  (interactive)
  (message "MMM Mode version %s by Michael Abraham Shulman" mmm-version))

;;}}}
;;{{{ Interactive History

(defvar mmm-interactive-history nil
  "History of interactive mmm-ification in the current buffer.
Elements are either submode class symbols or class specifications. See
`mmm-classes-alist' for more information.")
(make-variable-buffer-local 'mmm-interactive-history)

(defun mmm-add-to-history (class)
  (add-to-list 'mmm-interactive-history class))

(defun mmm-clear-history ()
  "Clears history of interactive mmm-ification in current buffer."
  (interactive)
  (setq mmm-interactive-history nil))

;;}}}
;;{{{ Mode/Ext Manipulation

(defvar mmm-mode-ext-classes ()
  "List of classes associated with current buffer by mode and filename.
Set automatically from `mmm-mode-ext-classes-alist'.")
(make-variable-buffer-local 'mmm-mode-ext-classes)

(defun mmm-get-mode-ext-classes ()
  "Return classes for current buffer from major mode and filename.
Uses `mmm-mode-ext-classes-alist' to find submode classes."
  (or mmm-mode-ext-classes
      (setq mmm-mode-ext-classes
            (mapcar #'third
                    (remove-if-not #'mmm-mode-ext-applies
                                   mmm-mode-ext-classes-alist)))))

(defun mmm-clear-mode-ext-classes ()
  "Clear classes added by major mode and filename."
  (setq mmm-mode-ext-classes nil))

(defun mmm-mode-ext-applies (element)
  (destructuring-bind (mode ext class) element
    (and (if mode (eq mode major-mode) t)
         (if ext (and (buffer-file-name)
                      (save-match-data
                        (string-match ext (buffer-file-name))))
           t))))

(defun mmm-get-all-classes ()
  "Return a list of all classes applicable to the current buffer.
These come from mode/ext associations, `mmm-classes', and interactive
history."
  (append mmm-interactive-history
          (if (listp mmm-classes) mmm-classes (list mmm-classes))
          (mmm-get-mode-ext-classes)))

;;}}}

(provide 'mmm-vars)

;;; mmm-vars.el ends here