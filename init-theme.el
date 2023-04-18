;; --- Fonts and color theme -*- lexical-binding: t -*-

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
(+my-modeline-font-setup)

;;; Ligatures.
(use-package hek-ligature
  :ensure nil ;; my code
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
  :hook
  ((prog-mode . hek-ligature-mode)))

;;; all-the-icons, https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)


;;;;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; doom-themes, https://github.com/doomemacs/themes
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config) ;; for neotree
  ;; (setq doom-themes-treemacs-theme "doom-atom") (doom-themes-treemacs-config);; for treemacs
  ;; (doom-themes-org-config) ;; for org mode
  )

;; ;;; Dark/light themes.
;; (use-package hek-yinyang
;;   :ensure nil ;; my code
;;   :init
;;   (setq hek-yinyang-dark-theme  'doom-vibrant
;;         hek-yinyang-light-theme 'doom-one-light
;;         hek-yinyang-sunrise     '(08 . 00)
;;         hek-yinyang-sunset      '(16 . 00))
;;   :config
;;   ;; Set mode line fonts.
;;   (add-hook 'hek-yinyang-update-hook +my-modeline-font-setup)
;;   (hek-yinyang-mode t))

;;; Solaire mode :: different background darkness between special / file buffers
;;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :init
  (solaire-global-mode 1))
