;;; mmm-auto.el --- loading and enabling MMM Mode automatically

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>
;; Version: $Id: mmm-auto.el,v 1.3 2000/04/30 08:03:09 mas Exp $

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

;; This file contains functions and hooks to load and enable MMM Mode
;; automatically. It sets up autoloads for the main MMM Mode functions
;; and interactive commands, and also sets up MMM Global Mode.

;;{{{ Comments on MMM Global Mode

;; This is a kludge partially borrowed from `global-font-lock-mode'.
;; The idea is the same: we have a function (here `mmm-mode-on-maybe')
;; that we want to be run whenever a major mode starts. Unfortunately,
;; there is no hook (like, say `major-mode-hook') that all major modes
;; run when they are finished. They just run their own, specific hook.
;; So what we're going to do is find a way to insinuate our function
;; into *all* those hooks. (This is a bit different from what global
;; font-lock mode does--it uses `post-command-hook').

;; In order to do this magic, we rely on the fact that there *is* a
;; hook that all major modes run when *beginning* their work. They
;; must call `kill-all-local-variables', which in turn runs
;; `change-major-mode-hook'. So we add a function to *that* hook which
;; inspects the call stack to find the mode function which is calling
;; it (mode functions are recognizable by ending in "-mode"), and add
;; our function to that mode's hook.

;; Actually, in the interests of generality, what it adds to that
;; mode's hook is the function `mmm-run-major-mode-hook', which in
;; turn runs the hook `mmm-major-mode-hook'. Our desired function
;; `mmm-mode-on-maybe' is then added to that hook. This way, if the
;; user wants to run something else on every major mode, they can just
;; add it to `mmm-major-mode-hook' and take advantage of this hack.

;; In out-of-the box Emacs, almost all major modes will be four frames
;; back. The frames are:
;; 1. mmm-major-mode-change
;; 2. run-hooks(change-major-mode-hook)
;; 3. kill-all-local-variables
;; 4. DESIRED-MAJOR-mode
;; When gnuserv is loaded, it adds an extra layer (a function called
;; `server-kill-all-local-variables'), making five. I can imagine
;; other packages doing the same thing, so for safety's sake, if we
;; don't find a function whose name ends in `-mode', we keep looking
;; until we run out of frames. I'm 99% sure that there will always be
;; at least four frames, though.

;;}}}

;;; Code:

(require 'cl)
(when t
  (require 'mmm-vars))

;;{{{ Autoloads

;; To shut up the byte compiler.
(eval-and-compile
  (autoload 'mmm-mode-on "mmm-mode" "Turn on MMM Mode. See `mmm-mode'.")
  (autoload 'mmm-mode "mmm-mode"
    "Minor mode to allow multiple major modes in one buffer.
Without ARG, toggle MMM Mode. With ARG, turn MMM Mode on iff ARG is
positive and off otherwise." t))

(autoload 'mmm-ify-by-class "mmm-cmds" "" t)
(autoload 'mmm-ify-by-regexp "mmm-cmds" "" t)
(autoload 'mmm-ify-region "mmm-cmds" "" t)
(autoload 'mmm-parse-buffer "mmm-cmds" "" t)
(autoload 'mmm-parse-region "mmm-cmds" "" t)
(autoload 'mmm-parse-block "mmm-cmds" "" t)
(autoload 'mmm-clear-current-region "mmm-cmds" "" t)
(autoload 'mmm-reparse-current-region "mmm-cmds" "" t)
(autoload 'mmm-end-current-region "mmm-cmds" "" t)
(autoload 'mmm-insertion-help "mmm-cmds" "" t)
(autoload 'mmm-insert-region "mmm-cmds" "" t)

;;}}}
;;{{{ Automatic Hook Adding

(defun mmm-major-mode-change ()
  "Add mode hooks to turn MMM Mode on where appropriate.
Actually adds `mmm-run-major-mode-hook' to all major mode hooks."
  (unless (window-minibuffer-p (selected-window))
    (loop for lookback from 4
          for frame = (backtrace-frame lookback)
          while frame
          if (mmm-get-mode-hook (cadr frame))
          do (add-hook it 'mmm-run-major-mode-hook)
          and return t)))
(add-hook 'change-major-mode-hook 'mmm-major-mode-change)

(defun mmm-get-mode-hook (function)
  "If FUNCTION is a mode function, get its hook variable.
Otherwise, return nil."
  (when (symbolp function)
    (let ((name (symbol-name function)))
      (and (> (length name) 5)
           (string= (substring name -5) "-mode")
           (intern (format "%s-hook" name))))))

;;}}}
;;{{{ MMM Global Mode

(defun mmm-mode-on-maybe ()
  "Conditionally turn on MMM Mode.
Turn on MMM Mode if `global-mmm-mode' is non-nil and there are classes
to apply, or always if `global-mmm-mode' is t."
  (cond ((eq mmm-global-mode t) (mmm-mode-on))
        ((not mmm-global-mode))
        ((mmm-get-all-classes) (mmm-mode-on))))

;; Add our function to our hook.
(add-hook 'mmm-major-mode-hook 'mmm-mode-on-maybe)

;; File Local variables don't get set by the time the major mode is
;; starting up, apparently. So we need to add the hook here too.
(add-hook 'find-file-hooks 'mmm-mode-on-maybe)

(defalias 'mmm-add-find-file-hooks 'mmm-add-find-file-hook)

(defun mmm-add-find-file-hook ()
  "Equivalent to \(setq mmm-global-mode 'maybe).
This function is deprecated and may be removed in future."
  (setq mmm-global-mode 'maybe))

;;}}}

(provide 'mmm-auto)

;;; mmm-auto.el ends here