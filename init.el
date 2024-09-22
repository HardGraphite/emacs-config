;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Unset frame initial colors. These are set in the `early-init.el'.
;; The initial frame's face shall has been initialized.
(setq default-frame-alist
      (seq-reduce
       (lambda (lst item) (assq-delete-all item lst))
       '(background-color foreground-color)
       default-frame-alist))

;; Initialize.
(let ((init-dir (file-name-directory load-file-name))
      (file-name-handler-alist nil)
      (inhibit-redisplay t))

  ;; Parse options.
  (dolist (name-and-switch
           '((option/install-packages . "--install-packages")
             (option/minimal          . "--minimal")))
    (when (member (cdr name-and-switch) command-line-args)
      (setq command-line-args (delete (cdr name-and-switch) command-line-args))
      (set (car name-and-switch) t)))

  ;; Add package path and define auto-loads.
  (let* ((lisp-dir (concat init-dir "lisp"))
         (lisp-autoloads-file (expand-file-name "hek-autoloads" lisp-dir)))
    (add-to-list 'load-path lisp-dir)
    (load lisp-autoloads-file nil t nil t))

  ;; Execute init scripts.
  (load (concat init-dir "userconf") t t)
  (load (concat init-dir "init-base") nil t nil t)
  (unless (bound-and-true-p option/minimal)
    (load (concat init-dir "init-main") nil t nil t)
    (load custom-file t t)))
