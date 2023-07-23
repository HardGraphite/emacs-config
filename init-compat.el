;; --- Compat code for future features and different platforms -*- lexical-binding: t; no-byte-compile: t -*-

(unless (>= emacs-major-version 29) ;; before Emacs 29

  ;; From `startup.el'.
  (defun startup-redirect-eln-cache (cache-directory)
    (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
    (push (expand-file-name (file-name-as-directory cache-directory)
                            user-emacs-directory)
          native-comp-eln-load-path))

  ;; Will be define in `pixel-scroll.el'.
  (defun pixel-scroll-precision-mode (&optional x))

  )
