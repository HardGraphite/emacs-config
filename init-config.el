;; --- Configuration definitions -*- lexical-binding: t -*-

(unless (eq system-type 'gnu/linux)
  (error "expecting GNU/Linux system!"))

(defconst *my-emacs-conf-dir*   (file-name-as-directory (expand-file-name "~/.config/emacs")))
(defconst *my-emacs-data-dir*   (file-name-as-directory (expand-file-name "~/.local/share/emacs")))
(defconst *my-emacs-cache-dir*  (file-name-as-directory (expand-file-name "~/.cache/emacs")))

(defconst *my-package-mirror* nil) ;; = nil / 'cn

(defconst *my-code-font-family*  "JetBrains Mono")  ;; Mono font for coding.
(defconst *my-code-font-height*  150)               ;; A comfortable size.
(defconst *my-mono-font-family*  "Source Code Pro") ;; Mono font for UI.
(defconst *my-mono-font-height*  140)               ;; Smaller than code font.
(defconst *my-term-font-family*  "Ubuntu Mono")     ;; Mono font for terminal.
(defconst *my-term-font-height*  170)               ;; Similar to code font.
(defconst *my-text-font-family*  "sans")            ;; Sans font for other text.
(defconst *my-text-font-height*  160)               ;; As you like.

(defconst *my-shell* "/usr/bin/fish")
