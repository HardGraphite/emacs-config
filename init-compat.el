;; --- Compat code for future features and different platforms -*- lexical-binding: t -*-

(defconst IS-EMACS29+  (>= emacs-major-version 29))

(when (not IS-EMACS29+)

;; From `startup.el'.
(defun startup-redirect-eln-cache (cache-directory)
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  (push (expand-file-name (file-name-as-directory cache-directory)
                          user-emacs-directory)
        native-comp-eln-load-path))

(defun pixel-scroll-precision-mode (&optional x))

)
