;; --- Configuration definitions -*- lexical-binding: t; no-byte-compile: t -*-

(unless (eq system-type 'gnu/linux)
  (error "expecting GNU/Linux system!"))

(defconst *my-emacs-conf-dir*   (file-name-as-directory (expand-file-name "~/.config/emacs")))
(defconst *my-emacs-data-dir*   (file-name-as-directory (expand-file-name "~/.local/share/emacs")))
(defconst *my-emacs-cache-dir*  (file-name-as-directory (expand-file-name "~/.cache/emacs")))

(defconst *my-package-mirror* nil) ;; = nil / 'cn

(defconst *my-code-font-family*  "JetBrains Mono")  ;; Mono font for coding.
(defconst *my-code-font-height*  150)
(defconst *my-mono-font-family*  "Iosevka Fixed Slab") ;; Mono font for UI.
(defconst *my-mono-font-height*  155)
(defconst *my-term-font-family*  "Ubuntu Mono")     ;; Mono font for terminal.
(defconst *my-term-font-height*  165)
(defconst *my-text-font-family*  "sans")            ;; Sans font for other text.
(defconst *my-text-font-height*  160)
(defconst *my-nerd-font-family*  "Symbols Nerd Font")

(defconst *my-fontset-fonts*
  '((han . (font-spec :family "Source Han Sans CN"))))

(defconst *my-shell* "/usr/bin/fish")
