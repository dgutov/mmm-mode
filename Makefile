# Using srcdir, builddir, and VPATH enables separate build dir with
# $ make -f $srcdir
srcdir := $(dir $(MAKEFILE_LIST))
VPATH = $(srcdir)
builddir = .

EMACS ?= emacs
MAKEINFO = makeinfo
MAKEINFOFLAGS =

ELFILES = mmm-auto.el mmm-class.el mmm-cmds.el mmm-compat.el mmm-cweb.el \
          mmm-defaults.el mmm-erb.el mmm-mason.el mmm-mode.el mmm-myghty.el \
          mmm-noweb.el mmm-region.el mmm-rpm.el mmm-sample.el mmm-univ.el \
          mmm-utils.el mmm-vars.el
ELCFILES = $(ELFILES:.el=.elc)
TESTS = highlighting.el html-erb.el

all: mmm.info $(ELCFILES)

mmm.info: mmm.texi
	$(MAKEINFO) $(MAKEINFOFLAGS) -o $@ $^

mmm.html: mmm.texi
	$(MAKEINFO) --html $(MAKEINFOFLAGS) -o $@ $^

docs: html info
html: mmm.html
info: mmm.info

%.elc: %.el
	$(EMACS) --batch -Q -L $(builddir) -L $(srcdir) \
	  --eval '(setq byte-compile-dest-file-function (lambda (_) "$@"))' \
	  -f batch-byte-compile '$<'

check: all
	$(EMACS) --batch -Q -L $(builddir) -L $(srcdir) -L $(srcdir)/tests \
	  $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

clean-lisp:
	$(RM) -f $(ELCFILES)

clean-info:
	$(RM) -f mmm.info

clean-html:
	$(RM) -rf mmm.html

clean: clean-lisp clean-info clean-html

.PHONY: check clean clean-html clean-info clean-lisp docs html info
