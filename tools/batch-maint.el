;;; batch-maint.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Maintenance tools.

;;; Code:

(require 'comp)
(require 'cus-edit) ; for byte compile
(require 'subr-x)

(unless noninteractive
  (warn "Package `batch-maint' is loaded within an interactive terminal."))

(defconst batch-maint--this-file
  (expand-file-name load-file-name))
(defconst batch-maint--root-dir
  (expand-file-name "../" (file-name-directory batch-maint--this-file)))
(unless (file-exists-p (expand-file-name "config.el" batch-maint--root-dir))
  (error "batch-maint: wrong config root directory: %s" batch-maint--root-dir))

(defvar batch-maint--log t)

(defun batch-maint--log (msg &rest args)
  "Print formatted log."
  (when batch-maint--log
    (apply 'message (concat "** [batch-maint] " msg) args)))

(defun batch-maint--load-batch-config (&optional force)
  "Evaluate code between \";;;###batch-config-begin\" and \";;;###batch-config-end\" in config.el."
  (when (or force (not (boundp 'config/emacs-conf-dir)))
    (batch-maint--log "loading batch-config...")
    (add-to-list 'load-path (expand-file-name "lisp" batch-maint--root-dir))
    (with-current-buffer (generate-new-buffer "*batch-maint batch-config*" t)
      (insert-file-contents (expand-file-name "config.el" batch-maint--root-dir))
      (goto-char (point-min))
      (let ((last-pos (point)))
        (while-let ((block-begin (search-forward ";;;###batch-config-begin" nil t)))
          (delete-region last-pos block-begin)
          (when (search-forward ";;;###batch-config-end" nil t)
            (setq last-pos (match-beginning 0))))
        (delete-region last-pos (point-max)))
      (eval-buffer))))

(defvar batch-maint--cmd-list nil)

(defun batch-maint--cmdfun (name)
  "Convert command name to its function name."
  (intern (concat "batch-maint--cmd-"
                  (if (symbolp name) (symbol-name name) name))))

(defmacro batch-maint--defcmd (name docstring &rest body)
  "Define a command."
  (let ((fname (batch-maint--cmdfun name)))
    (add-to-list 'batch-maint--cmd-list fname t)
    `(defun ,fname () ,docstring ,@body)))
(put 'batch-maint--defcmd 'lisp-indent-function 'defun)

(defun batch-maint--cmdcall (name)
  "Call command function,"
  (funcall (batch-maint--cmdfun name)))

(batch-maint--defcmd hek/gen-aloads
  "Generate autoloads for lisp/*.el files."
  (let ((lisp-dir (expand-file-name "lisp" batch-maint--root-dir)))
    (batch-maint--log "generating `hek-autoloads.el'...")
    (loaddefs-generate lisp-dir (expand-file-name "hek-autoloads.el" lisp-dir))))

(batch-maint--defcmd hek/compile
  "Byte (and native if available) compile lisp/*.el and themes/*.el files."
  (let ((default-directory batch-maint--root-dir)
        (do-native-comp (native-comp-available-p)))
    (batch-maint--load-batch-config)
    (dolist (dir '("lisp" "themes"))
      (dolist (el-file (file-expand-wildcards (concat dir "/*.el") t))
        (batch-maint--log "checking `%s'..." el-file)
        (when (file-newer-than-file-p el-file (concat el-file "c"))
          (batch-maint--log "byte-compiling `%s'..." el-file)
          (byte-compile-file el-file))
        (when (and do-native-comp
                   (file-exists-p (concat el-file "c"))
                   (let ((eln-file (comp-lookup-eln el-file)))
                     (or (null eln-file)
                         (file-newer-than-file-p el-file eln-file))))
          (batch-maint--log "native-compiling `%s'..." el-file)
          (native-compile el-file))))))

(batch-maint--defcmd hek/clean-up
  "Delete generated files in lisp/ and themes/."
  (let ((default-directory batch-maint--root-dir))
    (dolist (x '( "lisp/hek-autoloads.el" "lisp/*.elc" "themes/*.elc" ))
      (batch-maint--log "deleting %s..." x)
      (mapc #'delete-file (file-expand-wildcards x t)))))

(defvar batch-maint--native-comp-async nil)

(batch-maint--defcmd pkg/install
  "Install packages."
  (batch-maint--load-batch-config)
  (let ((command-line-args '("--install-packages"))
        (user-emacs-directory config/emacs-conf-dir)
        (package-native-compile t)
        (native-comp-jit-compilation nil))
    (load (expand-file-name "lisp/hek-autoloads.el" batch-maint--root-dir) nil t t)
    (load (expand-file-name "config.el" batch-maint--root-dir) nil t t))
  (when (native-comp-available-p)
    (batch-maint--log "waiting for native-comp...")
    (setq batch-maint--native-comp-async t)
    (add-hook 'native-comp-async-all-done-hook
              (lambda () (setq batch-maint--native-comp-async nil)))
    (while batch-maint--native-comp-async
      (sleep-for 5))
    (batch-maint--log "waiting for native-comp... done")))

(batch-maint--defcmd pkg/delete
  "Delete installed packages."
  (batch-maint--load-batch-config)
  (dolist (x (list package-user-dir package-quickstart-file))
    (batch-maint--log "deleting %s..." x)
    (if (file-directory-p x) (delete-directory x t) (delete-file x))))

(batch-maint--defcmd eln/delete
  "Delete eln cache."
  (when (native-comp-available-p)
    (batch-maint--load-batch-config)
    (let ((eln-dir (car native-comp-eln-load-path)))
      (when (file-writable-p eln-dir)
        (batch-maint--log "deleting %s..." eln-dir)
        (delete-directory eln-dir t)))))

(batch-maint--defcmd setup
  "Do setup (combination of several commands)."
  (let ((command-line-args-left
         '( hek/gen-aloads
            pkg/install
            hek/compile )))
   (batch-maint-exec t)))

(batch-maint--defcmd clean-up
  "Do cleanup (combination of several commands)."
  (let ((command-line-args-left
         '( hek/clean-up
            pkg/delete
            eln/delete )))
   (batch-maint-exec t)))

(defun batch-maint-list-commands ()
  "List available commands."
  (let ((name-prefix (symbol-name (batch-maint--cmdfun ""))))
   (dolist (name batch-maint--cmd-list)
     (message "%-20s -- %s"
              (string-remove-prefix name-prefix (symbol-name name))
              (documentation name))))
  (kill-emacs))

(defun batch-maint-gen-makefile ()
  "Generate a Makefile in the project's root directory to use this package from command line."
  (let ((makefile-abs-path (expand-file-name "Makefile" batch-maint--root-dir))
        (this-file-path "tools/batch-maint.el")
        (cmd-name-prefix (symbol-name (batch-maint--cmdfun ""))))
    (with-current-buffer (generate-new-buffer "*batch-maint gen-makefile*" t)
      (insert "EMACS ?= emacs\n")
      (insert "EMACS_BATCH_MAINT = ${EMACS} --batch --init-directory '" batch-maint--root-dir "' --load '" this-file-path "'\n")
      (insert "EMACS_BATCH_MAINT_EXEC = ${EMACS_BATCH_MAINT} --funcall batch-maint-exec\n")
      (insert "\nall: help\n\n")
      (insert ".PHONY: help\n"
              "help:\n"
              "\t@echo 'make Makefile       -- Re-generate this Makefile.'\n"
              "\t@echo 'make list-commands  -- List maintenance commands that can be targets.'\n"
              ?\n)
      (insert ".PHONY: list-commands\n"
              "list-commands:\n"
              "\t${EMACS_BATCH_MAINT} --funcall batch-maint-list-commands\n\n")
      (insert "Makefile: " this-file-path ?\n
              "\t${EMACS_BATCH_MAINT} --funcall batch-maint-gen-makefile\n\n")
      (dolist (name batch-maint--cmd-list)
        (let ((cmd (string-remove-prefix cmd-name-prefix (symbol-name name))))
          (insert ".PHONY: " cmd ?\n
                  cmd ?\: ?\n
                  ?\t "${EMACS_BATCH_MAINT_EXEC} " cmd  ?\n
                  ?\n)))
    (write-region (point-min) (point-max) makefile-abs-path)
    (kill-buffer))))

(defun batch-maint-exec (&optional non-toplevel)
  "Execute maintenance commands in batch mode.
The rest command line arguments are the commands to execute."
  (unless (or non-toplevel noninteractive)
    (warn "`batch-maint-exec' is not called in batch mode."))
  (while command-line-args-left
    (let ((cmd (car command-line-args-left)))
      (batch-maint--log "EXECUTING COMMAND `%s' ..." cmd)
      (batch-maint--cmdcall cmd)
      (setq command-line-args-left (cdr command-line-args-left))))
  (unless non-toplevel
    (kill-emacs)))

(provide 'batch-maint)
;;; batch-maint.el ends here
