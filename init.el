;; -*- lexical-binding: t -*-

;; Add package path.
(add-to-list 'load-path
  (concat (file-name-directory load-file-name) "lisp"))

;; Load initialization scripts.
(let ((init-prefix (concat (file-name-directory load-file-name) "init-"))
      (init-list '("config" "compat" "system" "theme"
                   "complete" "editor" "langs" "tools" "keymaps")))
  (dolist (name init-list)
    (load (concat init-prefix name) nil t nil t)))
(load custom-file t t)

;; Check whether package `gcmh' is loaded.
;; See the first line of file `early-init.el'.
(unless (boundp 'gcmh-mode)
  (setq gc-cons-threshold #x100000) ;; 1 MiB
  (error "Package `gcmh' is not loaded."))

;; init.el ends here.
