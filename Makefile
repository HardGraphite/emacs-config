EMACS ?= emacs

HEK_XXX_DIR        = lisp
HEK_XXX_FILES      = $(wildcard ${HEK_XXX_DIR}/hek-*.el)
HEK_AUTOLOADS_FILE = ${HEK_XXX_DIR}/hek-autoloads.el
HEK_XXX_FILES     := $(filter-out ${HEK_AUTOLOADS_FILE},${HEK_XXX_FILES})

Q_INIT_DIR ?= $(or ${TMPDIR},/tmp)/emacs-q.${USER}
Q_INIT_FILE = ${Q_INIT_DIR}/init.el
Q_LOAD_DIR ?= ${Q_INIT_DIR}/lisp

BATCH_CONF = $(or ${TMPDIR},/tmp)/emacs-batch-init.${USER}.el

${BATCH_CONF}: config.el
	sed '/^;;;###batch-config-begin$$/,/^;;;###batch-config-end$$/{//!b};d' config.el > $@

.PHONY: help
help:
	@echo 'make version    --- print Emacs version info'
	@echo 'make debug      --- start Emacs with `--debug-init` and `debug-on-error=t`'
	@echo 'make Q          --- start Emacs with `-Q` at a temporary directory'
	@echo 'make setup      --- do setup (combination of several targets)'
	@echo 'make autoloads  --- generate autoloads file for lisp/*.el files'
	@echo 'make bytecode   --- byte compile lisp/*.el and themes/*.el files'
	@echo 'make clean      --- delete generated *.elc and autoloads files'
	@echo 'make packages   --- install packages'
	@echo 'make pkgclean   --- delete installed packages'

.PHONY: version
version:
	@"${EMACS}" --version | head -n 2

.PHONY: debug
debug:
	@"${EMACS}" --debug-init --eval '(setq debug-on-error t)'

.PHONY: Q
Q:
	@[ -d "${Q_INIT_DIR}" ] || mkdir "${Q_INIT_DIR}"
	@[ -d "${Q_LOAD_DIR}" ] || mkdir "${Q_LOAD_DIR}"
	@[ -e "${Q_INIT_FILE}" ] \
	|| echo -e ';; init.el -*- lexical-binding: t -*-' \
		'\n\n(defun restart-emacs () (interactive) (kill-emacs 100))' \
		'\n(global-set-key (kbd "<f4>") `kill-emacs)' \
		'\n(global-set-key (kbd "<f5>") `restart-emacs)' \
		'\n\n;;;\n\n' \
	> "${Q_INIT_FILE}"
	@echo "running Emacs at ${Q_INIT_DIR} ..."
	@echo "the user load directory is: ${Q_LOAD_DIR}"
	@while "${EMACS}" -Q \
		--chdir="${Q_INIT_DIR}" --init-directory="${Q_INIT_DIR}" \
		--directory="${Q_LOAD_DIR}" --load "${Q_INIT_FILE}" \
		--eval "(find-file-other-window \"${Q_INIT_FILE}\")" \
		--eval '(goto-char (point-max))'; \
	[ $$? -eq 100 ]; do echo 'restarting Emacs...'; done; echo 'quit Emacs'

.PHONY: setup
setup: autoloads packages bytecode

.PHONY: autoloads
autoloads: ${HEK_AUTOLOADS_FILE}

${HEK_AUTOLOADS_FILE}: ${HEK_XXX_FILES}
	@"${EMACS}" --batch \
		--chdir "${HEK_XXX_DIR}" \
		--funcall loaddefs-generate-batch \
		"$(notdir $@)" .

.PHONY: bytecode
bytecode: ${BATCH_CONF}
	@"${EMACS}" --batch \
		--directory 'lisp' \
		--load '${BATCH_CONF}' \
		--eval '(require `cus-edit)' \
		--eval '(batch-byte-compile t)' \
		lisp/*.el themes/*.el

.PHONY: clean
clean:
	@rm -v lisp/*.elc themes/*.elc "${HEK_AUTOLOADS_FILE}"

.PHONY: packages
packages:
	@"${EMACS}" \
		--install-packages \
		--debug-init \
		--eval '(switch-to-buffer "*Messages*")'

.PHONY: pkgclean
pkgclean: ${BATCH_CONF}
	@"${EMACS}" --batch \
		--directory 'lisp' \
		--load '${BATCH_CONF}' \
		--eval '(princ (concat package-user-dir "\0"))' \
		--eval '(when package-quickstart-file (princ (concat package-quickstart-file "\0")))' \
		--eval '(when (bound-and-true-p native-comp-eln-load-path) (princ (concat (car native-comp-eln-load-path) "\0")))' \
	| xargs -0 -- rm -Rv
