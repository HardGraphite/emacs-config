;;; init-setup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This is an alternative to the `make setup' command.

;; To use this script, a load form that loads this file should be inserted at
;; the beginning of init file to do setup works. After running Emacs once, the
;; load form shall be removed.

;;; Code:

(defconst isetup--conf-dir user-emacs-directory)

(message "setup: autoloads...")
(loaddefs-generate (concat isetup--conf-dir "lisp")
                   (concat isetup--conf-dir "lisp/hek-autoloads.el"))
(message "setup: autoloads done")

(message "setup: packages...")
(add-to-list 'command-line-args "--install-packages")

(defun isetup--after-init ()
  (message "setup: packages done")

  (message "setup: bytecode...")
  (byte-recompile-directory (concat isetup--conf-dir "lisp") 0)
  (message "setup: bytecode done")

  (if (not (and package-native-compile (native-comp-available-p)))
      (isetup--done)
    (when (bound-and-true-p isetup--wait-native-comp-timer)
      (cancel-timer isetup--wait-native-comp-timer))
    (setq isetup--wait-native-comp-timer
          (run-with-timer 10 10 #'isetup--wait-native-comp))
    (message "setup: native-compiling packages...")))
(add-hook 'after-init-hook #'isetup--after-init)

(defun isetup--wait-native-comp ()
  (unless (or byte-native-compiling comp-native-compiling)
    (cancel-timer isetup--wait-native-comp-timer)
    (setq isetup--wait-native-comp-timer nil)
    (isetup--done)))

(defun isetup--done ()
  (message "== Emacs is ready! ==\n\nDO NOT forget to remove \"init-setup.el\"-loading code from init file."))

;;; init-setup.el ends here
