EMACS ?= emacs

HEK_XXX_DIR        = lisp
HEK_XXX_FILES      = $(wildcard ${HEK_XXX_DIR}/hek-*.el)
HEK_AUTOLOADS_FILE = ${HEK_XXX_DIR}/hek-autoloads.el
HEK_XXX_FILES     := $(filter-out ${HEK_AUTOLOADS_FILE},${HEK_XXX_FILES})

.PHONY: help
help:
	@echo 'make version    --- Print Emacs version info'
	@echo 'make debug      --- Start Emacs with `--debug-init` and `debug-on-error=t`'
	@echo 'make autoloads  --- Generate autoloads file for lisp/*.el files'
	@echo 'make compile    --- byte compile lisp/*.el and themes/*.el files'
	@echo 'make clean      --- delete generated files'
	@echo 'make packages   --- Install packages'

.PHONY: version
version:
	@"${EMACS}" --version | head -n 2

.PHONY: debug
debug:
	@"${EMACS}" --debug-init --eval '(setq debug-on-error t)'

.PNONY: autoloads
autoloads: ${HEK_AUTOLOADS_FILE}

.PNONY: compile
compile:
	@"${EMACS}" --batch \
		--directory 'lisp' \
		--load 'init-compat.el' \
		--load 'init-config.el' \
		--load 'init-system.el' \
		--eval '(require `cus-edit)' \
		--eval '(batch-byte-compile t)' \
		lisp/*.el themes/*.el

.PHONY: clean
clean:
	@rm lisp/*.elc themes/*.elc "${HEK_AUTOLOADS_FILE}"

.PHONY: packages
packages:
	@"${EMACS}" \
		--install-packages \
		--debug-init \
		--eval '(switch-to-buffer "*Messages*")'

${HEK_AUTOLOADS_FILE}: ${HEK_XXX_FILES}
	@"${EMACS}" --batch \
		--chdir "${HEK_XXX_DIR}" \
		--funcall loaddefs-generate-batch \
		"$(notdir $@)" .
