;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Defer garbage collection during initialization.
;; Make sure that package `gcmh' has been installed and will be loaded.
;; See the last line of file `init.el'.
(setq gc-cons-threshold most-positive-fixnum)

;; Do not auto load packages.
(setq package-enable-at-startup nil
      package-quickstart        nil)

;; Disable unwanted GUI elements.
(setq default-frame-alist
  '((menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars)))
(setq menu-bar-mode   nil
      tool-bar-mode   nil
      scroll-bar-mode nil)
(setq use-dialog-box nil)

;; Simplify startup messages.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(put 'inhibit-startup-echo-area-message 'saved-value t)
