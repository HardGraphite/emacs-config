GREP_FLAGS = --with-filename --line-number --only-matching --color=auto

INIT_XXX_FILES = $(wildcard init-*.el)

.PHONY: help
help:
	@echo 'make version    --- Print Emacs version info'
	@echo 'make debug      --- Start Emacs with `--debug-init` and `debug-on-error=t`'
	@echo 'make get-pkgs   --- Install packages'
	@echo 'make ls-inits   --- List init-*.el files'
	@echo 'make ls-heads   --- List heading titles in configurations'
	@echo 'make ls-pkgs    --- List package names mentioned in `use-package`'

.PHONY: version
version:
	@emacs --version | head -n 2

.PHONY: debug
debug:
	@emacs --debug-init --eval '(setq debug-on-error t)'

.PHONY: get-pkgs
get-pkgs:
	@emacs --install-packages --debug-init --eval '(switch-to-buffer "*Messages*")'

.PHONY: ls-inits
ls-inits:
	@for name in $(patsubst init-%.el,%,${INIT_XXX_FILES}); do \
		echo $$name; \
	done;

.PHONY: ls-heads
ls-heads:
	@grep ${GREP_FLAGS} --perl-regexp \
		';{4,6}\s+\K([A-Za-z0-9 ]+)\s+;+' ${INIT_XXX_FILES}

.PHONY: ls-pkgs
ls-pkgs:
	@grep ${GREP_FLAGS} --perl-regexp \
		'\(hek-usepkg\s+\K([a-zA-Z0-9\-_]+)' ${INIT_XXX_FILES}
