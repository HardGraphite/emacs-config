;; -*- lexical-binding: t -*-

;; Use a dark background color before a theme is loaded.
(set-background-color "#555")

;; Add package path.
(add-to-list 'load-path
  (concat (file-name-directory load-file-name) "lisp"))

;; Load initialization scripts.
(let ((init-dir (file-name-directory load-file-name))
      (init-list
        '("config"
          "compat"
          "system"
          "theme"
          "complete"
          "editor"
          "langs"
          "tools"
          "keymaps")))
  (dolist (c init-list)
    (load (concat init-dir "init-" c))))
(when (file-readable-p custom-file)
  (load custom-file))

;; Check whether package `gcmh' is loaded.
;; See the first line of file `early-init.el'.
(unless (boundp gcmh-mode)
  (setq gc-cons-threshold #x100000) ; 1 MiB
  (error "Package `gcmh' is not loaded."))

;; init.el ends here.
