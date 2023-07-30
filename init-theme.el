;; --- Fonts and color theme -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global default font.
(set-face-attribute 'default nil
  :family *my-code-font-family* :height *my-code-font-height*)

;;; Minibuffer default font.
(defconst +my-minibuffer-font-remapping-alist
  `((default :family ,*my-mono-font-family* :height ,*my-mono-font-height*)))
(defun +my-minibuffer-font-setup ()
  (set (make-local-variable 'face-remapping-alist)
       +my-minibuffer-font-remapping-alist))

;;; Mode line defualt font.
(defun +my-modeline-font-setup ()
  (set-face-attribute 'mode-line nil;; TODO: mode-line-active instead of mode-line for Emacs 29+
    :family *my-mono-font-family* :height *my-mono-font-height*)
  (set-face-attribute 'mode-line-inactive nil
    :family *my-mono-font-family* :height *my-mono-font-height*))

(add-hook 'minibuffer-setup-hook #'+my-minibuffer-font-setup)
;; (+my-modeline-font-setup) ;; Call after Emacs theme is loaded.

;;; Ligatures.
(hek-usepkg hek-ligature
  :from local
  :init
  (setq hek-ligature-table
    '((prog-mode
        (?= . ".\\(?:=+\\|>\\)")
        (?< . ".\\(?:=>\\|[<=-]\\)")
        (?> . ".[>=]")
        (?+ . ".[+=]")
        (?- . ".\\(?:[=>]\\|-+\\)")
        (?* . ".[*=/]")
        (?/ . ".\\(?:/+\\|[=*]\\)")
        (?% . ".[%=]")
        (?& . ".[&=]")
        (?| . ".[|=]")
        (?^ . ".[\\^=]")
        (?~ . ".[~=]")
        (?! . ".\\(?:==\\|[!=]\\)")
        (?: . ".[:=]")
        (?. . ".\\.+")
        (?# . ".\\(?:#+}\\|[([{]\\)")
        (?_ . "._+"))))
  (autoload 'hek-ligature-mode "hek-ligature")
  :hook
  (prog-mode-hook . hek-ligature-mode))

;;; nerd-icons :: use Nerd Font icons, an alternative to `all-the-icons'
;;; https://github.com/rainstormstudio/nerd-icons.el
(hek-usepkg nerd-icons
  :from package
  :init
  (setq nerd-icons-font-family *my-nerd-font-family*))


;;;;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'hek-one-dark t)

;;; Dark/light themes.
(hek-usepkg hek-yinyang
  :from local
  :init
  (setq hek-yinyang-dark-theme  'hek-one-dark
        hek-yinyang-light-theme 'hek-one-light
        hek-yinyang-sunrise     '(08 . 00)
        hek-yinyang-sunset      '(17 . 00))
  :config
  (add-hook 'hek-yinyang-switch-hook #'+my-modeline-font-setup)
  (hek-yinyang-mode t))

;;; Solaire mode :: different background darkness between special / file buffers
;;; https://github.com/hlissner/emacs-solaire-mode
(hek-usepkg solaire-mode
  :from package
  :config
  (solaire-global-mode 1))
