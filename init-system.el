;; --- Emacs system setups -*- lexical-binding: t -*-

;;;;; Frame and basic UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Title.
(setq frame-title-format '("%b — GNU Emacs"))
(setq icon-title-format frame-title-format)

;; Resize method.
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; Full screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Minibuffer tweak.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.5)

;; Disable tooltip.
(when (bound-and-true-p tooltip-mode) (tooltip-mode -1))

;; Cursor.
(setq x-stretch-cursor t)
(blink-cursor-mode -1)


;;;;; Emacs directories and files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory        *my-emacs-data-dir*
      package-user-dir            (concat *my-emacs-data-dir* "elpa")
      auto-save-list-file-prefix  (concat *my-emacs-data-dir* "auto-save-list/saves-")
      custom-file                 (expand-file-name "custom.el" *my-emacs-conf-dir*))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" *my-emacs-conf-dir*))

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" *my-emacs-data-dir*)))


;;;;; Package management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Step up built-in package manager.
(require 'package)
(cond ((eq *my-package-mirror* 'cn) ;; mirror in China
       (setq package-archives
       '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
      ((eq *my-package-mirror* nil) ;; original
       (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
      (t (error "illegal value of *my-package-mirror*")))
(package-initialize)

;;; Prepare `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Install packages.
(let ((package-install-switch "--install-packages"))
  (when (member package-install-switch command-line-args)
    (setq command-line-args (delete package-install-switch command-line-args))
    (setq use-package-always-ensure t
          use-package-verbose t)))


;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Confirm before quit.
(setq confirm-kill-emacs 'y-or-n-p)

;;; Use UTF-8.
(set-default-coding-systems 'utf-8)

;;; Message buffer, scratch buffer.
(setq messages-buffer-max-lines 100
      initial-scratch-message   nil)


;;;;; Other hackes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Increase read chunck size. For faster file read and LSP performance.
(setq read-process-output-max #x100000) ; 1 MiB

;;; Inhibit fontification when inputing, helping a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Clever carbage collection.
(use-package gcmh
  :defer t
  :config
  (setq gcmh-idle-delay 20
        gcmh-high-cons-threshold #x1000000) ; 16 MiB
  :hook
  (after-init . gcmh-mode))
