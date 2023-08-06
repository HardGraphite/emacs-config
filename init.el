;; -*- lexical-binding: t; no-byte-compile: t -*-

(let ((init-dir (file-name-directory load-file-name))
      (file-name-handler-alist nil))

  ;; Add package path and define auto-loads.
  (let* ((lisp-dir (concat init-dir "lisp"))
         (lisp-autoloads-file (expand-file-name "hek-autoloads" lisp-dir)))
    (add-to-list 'load-path lisp-dir)
    (load lisp-autoloads-file nil t nil t))

  ;; Load initialization scripts.
  (let ((init-prefix (concat init-dir "init-"))
        (init-list '("config" "compat" "system" "theme"
                     "complete" "editor" "langs" "tools" "keymaps")))
    (dolist (name init-list)
      (load (concat init-prefix name) nil t nil t)))

  ;; Load custom file.
  (load custom-file t t)

  )

;; init.el ends here.
