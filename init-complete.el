;; --- Completion facilities -*- lexical-binding: t -*-

;; TODO: consult, consult-flycheck, embark, embark-consult, wgrep

;;;;; Completion algorithm and category ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; orderless, https://github.com/oantolin/orderless
(use-package orderless
  :init
  ;(setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;      orderless-component-separator #'orderless-escapable-split-on-space)
  )

(dolist (name '(emacs21 emacs22 substring initials))
  (setq completion-styles-alist (assq-delete-all name completion-styles-alist)))
(setq completion-styles '(flex basic) ;; normal buffer
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local completion-styles '(orderless basic)))) ;; minibuffer

;;;;; Minibuffer completion front-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; vertico, https://github.com/minad/vertico
(use-package vertico
  :init
  (setq vertico-scroll-margin 2
        vertico-count 10
        vertico-cycle t)
  (vertico-mode 1)
  :bind
  (:map vertico-map
        ("M-j" . vertico-next)
        ("M-k" . vertico-previous)))
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)))
;; (use-package vertico-indexed
;;   :after vertico
;;   :ensure nil
;;   :init
;;   (vertico-indexed-mode 1))
;; (use-package vertico-reverse
;;   :after vertico
;;   :ensure nil
;;   :init
;;   (vertico-reverse-mode 1)
;;   :bind
;;   (:map vertico-reverse-map
;;         ("M-k" . vertico-next)
;;         ("M-j" . vertico-previous)))

;;; marginalia, https://github.com/minad/marginalia
(use-package marginalia
  :init
  (marginalia-mode 1))

;;;;; Code (in-buffer) completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; company-mode, https://github.com/company-mode/company-mode
;; (use-package company
;;   :init
;;   (global-company-mode 1)
;;   :config
;;   (set-face-attribute 'company-tooltip nil
;;     :family *my-term-font-family* :height *my-term-font-height*)
;;   (setq company-idle-delay 0.1
;;         company-minimum-prefix-length 1
;;         completion-ignore-case t
;;         company-tooltip-align-annotations t
;;         company-frontends
;;           '(company-pseudo-tooltip-frontend
;;             company-echo-metadata-frontend)
;;         company-backends
;;           '((company-capf :with company-yasnippet)
;;             company-dabbrev)
;;         company-dabbrev-other-buffers nil
;;         company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase nil)
;;   :bind
;;   (:map company-active-map
;;         ("C-n" . company-select-next)
;;         ("M-j" . company-select-next)
;;         ("C-p" . company-select-previous)
;;         ("M-k" . company-select-previous)))

;;; corfu, https://github.com/minad/corfu
(use-package corfu
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'valid
        corfu-echo-documentation t)
  ;; (set-face-attribute 'corfu-default nil
  ;;   :family *my-term-font-family* :height (- *my-term-font-height* 5))
  (set-face-attribute 'corfu-current nil
    ;; :family *my-term-font-family* :height (- *my-term-font-height* 5)
    :background "#1a3826")
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (require 'hek-corfu-nerd-icons)
  (add-to-list 'corfu-margin-formatters
               #'hek-corfu-nerd-icons-margin-formatter)
  :bind
  (:map corfu-map
        ("M-<escape>" . corfu-quit)
        ("<escape>" . (lambda () (interactive) (meow-insert-exit) (corfu-quit))) ;; for meow-mode
        ("M-j" . corfu-next)
        ("M-k" . corfu-previous)
        ("M-v" . corfu-scroll-up)
        ("M-V" . corfu-scroll-down)))

;;; cape, https://github.com/minad/cape
(use-package cape
  :init
  (dolist (capf
           '(cape-dabbrev
             ;; cape-file
             ;; cape-elisp-block
             ;; cape-history
             ;; cape-keyword
             ;; cape-tex
             ;; cape-sgml
             ;; cape-rfc1345
             ;; cape-abbrev
             ;; cape-dict
             ;; cape-symbol
             ;; cape-line
             ))
  (add-to-list 'completion-at-point-functions capf)))

;; Hacks for a better Eglot experience.
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;; (defun +eglot-setup-capfs ()
;;   (setq-local
;;    completion-at-point-functions
;;    (list (cape-super-capf
;;           #'eglot-completion-at-point
;;           #'cape-dabbrev))))
;; (add-hook 'eglot-managed-mode-hook #'+eglot-setup-capfs)


;;;;; Code templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; YASnippet, https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :defer t
  :init
  ;; I really don't like yasnippet's way of storing snippets.
  (setq yas-snippet-dirs nil)
  ;; ;; So, let me do it myself.
  ;; (let ((my-snippet-dir (concat *my-emacs-conf-dir* "snippets")))
  ;;   (load (expand-file-name "snippet-util" my-snippet-dir))
  ;;   (snippet-util-load-to-yasnippet my-snippet-dir))
  :hook
  ((prog-mode tex-mode) . yas-minor-mode))

;; ;;; TempEl :: code templates (snippets)
;; ;;; https://github.com/minad/tempel
;; (use-package tempel
;;   :defer t
;;   :bind
;;   (:map tempel-map
;;         ("M-[" . #'tempel-previous)
;;         ("M-]" . #'tempel-next)))
;; (use-package lsp-snippet-tempel
;;   :ensure nil ;; local
;;   :config
;;     (lsp-snippet-tempel-eglot-init))
