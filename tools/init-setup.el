;;; init-setup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This is an alternative to the `make setup' command.

;; To use this script, a load form that loads this file should be inserted at
;; the beginning of init file to do setup works. After running Emacs once, the
;; load form shall be removed.

;;; Code:

(load (expand-file-name "tools/batch-maint.el" user-emacs-directory))

(setq inhibit-redisplay nil)
(switch-to-buffer "*Messages*")

(batch-maint--cmdcall 'hek/gen-aloads)

(add-to-list 'command-line-args "--install-packages")

(defun isetup--after-init ()
  (batch-maint--cmdcall 'hek/compile)

  (if (not (and package-native-compile (native-comp-available-p)))
      (isetup--done)
    (message "** waiting for native-compiling")
    (add-hook 'native-comp-async-all-done-hook #'isetup--done)))
(run-with-idle-timer 3 nil #'isetup--after-init)

(defun isetup--done ()
  (message "
+-------------------------------------+
|  ======== Emacs is ready! ========  |
| DO NOT forget to remove the code to |
| load 'init-setup.el' script.        |
+-------------------------------------+
"))

;;; init-setup.el ends here
