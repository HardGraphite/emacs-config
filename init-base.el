;;; -*- lexical-binding: t; no-byte-compile: t; outline-regexp: ";;;;;\\*+"; -*-


;;;;;* CONFIGURE

;; Edit "userconf.el" to customize values here.

;;; Location info
;; Package `hek-yinyang' will need theme.
(unless (boundp 'calendar-latitude)
  (setq calendar-latitude 30))
(unless (boundp 'calendar-longitude)
  (setq calendar-longitude 120))

;;; Package achieves (mirrors)
;; + default
;;(defconst config/pkg-mirror-elpa nil)                    ; `nil' or `(gnu . nongnu)'
;;(defconst config/pkg-mirror-melpa "https://melpa.org/packages/")
;; + China
(defvar config/pkg-mirror-elpa
  '("https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" .
    "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
(defvar config/pkg-mirror-melpa
  "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")

;;; Fonts
(defvar config/code-font '("JetBrains Mono"      . 150)) ; Mono font for coding.
(defvar config/mono-font '("Iosevka Fixed Slab"  . 155)) ; Mono font for UI.
(defvar config/term-font '("Ubuntu Mono"         . 165)) ; Mono font for terminal.
(defvar config/text-font '("Roboto"              . 160)) ; Sans font for other text.
(defvar config/nerd-font '("Symbols Nerd Font"   . 150)) ; Nerd font for icons.
(defvar config/cjkx-font '("Sarasa Mono Slab SC" . 150)) ; CJK chars font.

;;; Others
(defvar config/shell "/usr/bin/fish")

;;; System specific
(pcase system-type
  ('windows-nt
   (setq config/shell         "pwsh.exe"
         default-directory    "~/Desktop/"))
  ('android
   (setq default-directory    "/sdcard/"))
  )


;;;;;* SYSTEM

;;;;;** Emacs directories and files

(if (and (file-equal-p user-emacs-directory "~/.config/emacs/")
         (file-directory-p "~/.config/emacs/") ;; `file-equal-p': if files do not exist, the return value is unspecified.
         (file-directory-p "~/.local/share/"))
    (progn
      (defconst config/emacs-conf-dir "~/.config/emacs/")
      (defconst config/emacs-data-dir "~/.local/share/emacs/"))
   (defconst config/emacs-conf-dir user-emacs-directory)
   (defconst config/emacs-data-dir (concat user-emacs-directory "data/")))
(unless (file-equal-p config/emacs-conf-dir (file-name-directory load-file-name))
  (error "`config/emacs-conf-dir' is incorrect: %S" config/emacs-conf-dir))

(setq user-emacs-directory        config/emacs-data-dir
      custom-theme-directory      (concat config/emacs-conf-dir "themes/")
      package-user-dir            (concat config/emacs-data-dir "packages")
      package-quickstart-file     (concat config/emacs-data-dir "package-quickstart.el")
      auto-save-list-file-prefix  nil ;; (concat config/emacs-data-dir "auto-save-list/saves-")
      custom-file                 (concat config/emacs-conf-dir "custom.el"))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (concat config/emacs-data-dir "eln-cache/")))


;;;;;** Package management

;;; Setup built-in package manager.
(require 'package)
(if config/pkg-mirror-elpa
    (setq package-archives
          `(("gnu"    . ,(car config/pkg-mirror-elpa))
            ("nongnu" . ,(cdr config/pkg-mirror-elpa))
            ("melpa"  . ,config/pkg-mirror-melpa)))
  (add-to-list 'package-archives
               (cons "melpa" config/pkg-mirror-melpa)
               t))
(setq package-quickstart t)
(package-initialize)
