EMACS ?= emacs
EMACS_BATCH_MAINT = ${EMACS} --batch --init-directory '/home/yd/.config/emacs/' --load 'tools/batch-maint.el'
EMACS_BATCH_MAINT_EXEC = ${EMACS_BATCH_MAINT} --funcall batch-maint-exec

all: help

.PHONY: help
help:
	@echo 'make Makefile       -- Re-generate this Makefile.'
	@echo 'make list-commands  -- List maintenance commands that can be targets.'

.PHONY: list-commands
list-commands:
	${EMACS_BATCH_MAINT} --funcall batch-maint-list-commands

Makefile: tools/batch-maint.el
	${EMACS_BATCH_MAINT} --funcall batch-maint-gen-makefile

.PHONY: hek/gen-aloads
hek/gen-aloads:
	${EMACS_BATCH_MAINT_EXEC} hek/gen-aloads

.PHONY: hek/compile
hek/compile:
	${EMACS_BATCH_MAINT_EXEC} hek/compile

.PHONY: hek/clean-up
hek/clean-up:
	${EMACS_BATCH_MAINT_EXEC} hek/clean-up

.PHONY: pkg/install
pkg/install:
	${EMACS_BATCH_MAINT_EXEC} pkg/install

.PHONY: pkg/delete
pkg/delete:
	${EMACS_BATCH_MAINT_EXEC} pkg/delete

.PHONY: eln/delete
eln/delete:
	${EMACS_BATCH_MAINT_EXEC} eln/delete

.PHONY: setup
setup:
	${EMACS_BATCH_MAINT_EXEC} setup

.PHONY: clean-up
clean-up:
	${EMACS_BATCH_MAINT_EXEC} clean-up

