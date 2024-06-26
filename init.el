;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Unset frame initial colors. These are set in the `early-init.el'.
;; The initial frame's face shall has been initialized.
(setq default-frame-alist
      (seq-reduce
       (lambda (lst item) (assq-delete-all item lst))
       '(background-color foreground-color)
       default-frame-alist))

;; Load init scripts.
(let ((init-dir (file-name-directory load-file-name))
      (file-name-handler-alist nil)
      (inhibit-redisplay t))

  ;; Add package path and define auto-loads.
  (let* ((lisp-dir (concat init-dir "lisp"))
         (lisp-autoloads-file (expand-file-name "hek-autoloads" lisp-dir)))
    (add-to-list 'load-path lisp-dir)
    (load lisp-autoloads-file nil t nil t))

  ;; Load configurations.
  (load (concat init-dir "config") nil t nil t)

  ;; Load custom file.
  (load custom-file t t)

  )
