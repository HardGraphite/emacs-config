;; --- Fonts and color theme -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global default font.
(custom-set-faces
 `(default ((t :family ,*my-code-font-family* :height ,*my-code-font-height*))))
(dolist (conf *my-fontset-fonts*)
  (set-fontset-font t (car conf) (eval (cdr conf))))

;;; Minibuffer default font.
(defconst +my-minibuffer-font-remapping-alist
  `((default :family ,*my-mono-font-family* :height ,*my-mono-font-height*)))
(defun +my-minibuffer-font-setup ()
  (set (make-local-variable 'face-remapping-alist)
       +my-minibuffer-font-remapping-alist))

;;; Mode line defualt font.
(custom-set-faces
  `(mode-line-active ((t :family ,*my-mono-font-family* :height ,*my-mono-font-height*)))
  `(mode-line-inactive ((t :family ,*my-mono-font-family* :height ,*my-mono-font-height*))))

(add-hook 'minibuffer-setup-hook #'+my-minibuffer-font-setup)

;;; Ligatures.
(hek-usepkg hek-ligature
  :from local
  :init
  (setq hek-ligature-table
        '((verilog-mode
           (?= . ".=")
           (?! . ".=")
           (?+ . ".[+=]")
           (?- . ".[-=]")
           (?* . ".[*=/]")
           (?/ . ".[/=*]")
           (?@ . ".("))
          (prog-mode
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
           (?~ . ".\\(?:~+\\|=\\)")
           (?! . ".\\(?:==\\|[!=]\\)")
           (?: . ".[:=]")
           (?. . ".\\.+")
           (?# . ".\\(?:#+}\\|[([{]\\)")
           (?_ . "._+"))))
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
        hek-yinyang-auto-sunrise-sunset t)
  :config
  (hek-yinyang-mode t))

;;; Solaire mode :: different background darkness between special / file buffers
;;; https://github.com/hlissner/emacs-solaire-mode
(hek-usepkg solaire-mode
  :from package
  :config
  (solaire-global-mode 1))
