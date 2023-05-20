;; -*- lexical-binding: t -*-

;; Add package path.
(add-to-list 'load-path
  (concat (file-name-directory load-file-name) "lisp"))

;; Load initialization scripts.
(let ((init-prefix (concat (file-name-directory load-file-name) "init-"))
      (init-list '("config" "compat" "system" "theme"
                   "complete" "editor" "langs" "tools" "keymaps"))
      (file-name-handler-alist nil))
  (dolist (name init-list)
    (load (concat init-prefix name) nil t nil t))
  (load custom-file t t))

;; init.el ends here.
