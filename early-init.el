;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Defer garbage collection during initialization.
;; Make sure that package `gcmh' has been installed and will be loaded.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (eql gc-cons-threshold most-positive-fixnum)
              (setq gc-cons-threshold #x1000000) ;; 16 MiB
              (error "Package `gcmh' might not be loaded.")))
          98)

;; Do not auto load packages.
(setq package-enable-at-startup nil)

;; Set the `default-frame-alist'.
(setq default-frame-alist
      '(;; Use a dark background color before a theme is loaded.
        (background-color . "#282c34")
        (foreground-color . "#bbc2cf")
        ;; Disable unwanted GUI elements.
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars)))
;; Inhibit some modes before they are enabled.
(setq menu-bar-mode   nil
      tool-bar-mode   nil
      scroll-bar-mode nil
      use-dialog-box  nil)

;; Simplify startup messages.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(put 'inhibit-startup-echo-area-message 'saved-value t)
