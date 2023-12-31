;; --- Emacs system setups -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Emacs directories and files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory        *my-emacs-data-dir*
      custom-theme-directory      (concat *my-emacs-conf-dir* "themes/")
      package-user-dir            (concat *my-emacs-data-dir* "packages")
      package-quickstart-file     (concat *my-emacs-data-dir* "package-quickstart.el")
      auto-save-list-file-prefix  nil ;; (concat *my-emacs-data-dir* "auto-save-list/saves-")
      custom-file                 (concat *my-emacs-conf-dir* "custom.el"))

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (concat *my-emacs-data-dir* "eln-cache/")))


;;;;; Package management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Step up built-in package manager.
(require 'package)
(if *my-mirror-elpa*
    (setq package-archives
          `(("gnu"    . ,(car *my-mirror-elpa*))
            ("nongnu" . ,(cdr *my-mirror-elpa*))
            ("melpa"  . ,*my-mirror-melpa*)))
  (add-to-list 'package-archives
               (cons "melpa" *my-mirror-melpa*)
               t))
(setq package-quickstart t)
(package-initialize)

;;; Prepare `hek-usepkg', which helps to setup packages.
(eval-when-compile
  (require 'hek-usepkg))
(hek-usepkg-gitpkg-initialize)

;;; Install packages.
(let ((package-install-switch "--install-packages"))
  (when (member package-install-switch command-line-args)
    (setq command-line-args (delete package-install-switch command-line-args)) ;; Remove the switch.
    (setq hek-usepkg-ensure t
          hek-usepkg-debug t)
    (setq package-native-compile t)
    (setq native-comp-async-query-on-exit t
          native-comp-verbose 2)
    (when package-quickstart
      (add-hook 'kill-emacs-hook #'package-quickstart-refresh))
    (add-hook 'kill-emacs-hook #'hek-usepkg-gitpkg-quickstart-refresh)
    (package-refresh-contents)))


;;;;; Frame and basic UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Title.
(setq frame-title-format '("%b — Emacs"))
(setq icon-title-format frame-title-format)

;; Resize method.
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; Window split.
(setq split-width-threshold  (* 2 72)
      split-height-threshold (* 2 25))

;; Minibuffer tweak.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.5)

;; Disable tooltip.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Cursor.
(setq x-stretch-cursor t)
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)


;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Confirm before quit.
(setq confirm-kill-emacs 'y-or-n-p)

;;; Use UTF-8.
(set-default-coding-systems 'utf-8)

;;; Message buffer, scratch buffer.
(setq messages-buffer-max-lines 100
      initial-scratch-message   ";; This buffer is NOT part of GNU Emacs.\n\n"
      initial-major-mode        #'fundamental-mode)


;;;;; Other hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Increase read chunck size. For faster file read and LSP performance.
(setq read-process-output-max #x100000) ; 1 MiB

;;; Inhibit fontification when inputing, helping a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Disable bidi (Bidirectional Display) and decrease long line thresholds.
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; Clever carbage collection.
(hek-usepkg gcmh
  :from package
  :init
  (defun +gcmh-setup ()
    (setq gcmh-idle-delay 10
          gcmh-high-cons-threshold #x1000000) ;; 16 MiB
    (gcmh-mode 1))
  :hook
  (emacs-startup-hook . +gcmh-setup))
