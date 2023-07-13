;; --- Completion facilities -*- lexical-binding: t -*-

;; TODO: consult, consult-flycheck, embark, embark-consult, wgrep

;;;;; Completion algorithm and category ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; orderless, https://github.com/oantolin/orderless
(hek-usepkg orderless
  :from package
  :init
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;      orderless-component-separator #'orderless-escapable-split-on-space)
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
(hek-usepkg vertico
  :from package
  :init
  (setq vertico-scroll-margin 2
        vertico-count 10
        vertico-cycle t)
  (vertico-mode 1)
  :bind
  (vertico-map
   ("M-j" . vertico-next)
   ("M-k" . vertico-previous)))
(hek-usepkg vertico-directory
  :after vertico
  :bind
  (vertico-map
   ("RET" . vertico-directory-enter)))

;;; marginalia, https://github.com/minad/marginalia
(hek-usepkg marginalia
  :from package
  :init
  (marginalia-mode 1))

;;;;; Code (in-buffer) completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; corfu, https://github.com/minad/corfu
(hek-usepkg corfu
  :from package
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
  (corfu-map
   ("M-<escape>" . corfu-quit)
   ("<escape>" . (lambda () (interactive) (meow-insert-exit) (corfu-quit))) ;; for meow-mode
   ("M-j" . corfu-next)
   ("M-k" . corfu-previous)
   ("M-v" . corfu-scroll-up)
   ("M-V" . corfu-scroll-down)))

;;; cape, https://github.com/minad/cape
(hek-usepkg cape
  :from package
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

;;; TempEl :: code templates (snippets)
;;; https://github.com/minad/tempel
(hek-usepkg tempel
  :from package
  :config
  (setq hek-tempel-snippets-dir (concat *my-emacs-conf-dir* "snippets/"))
  (require 'hek-tempel-snippets)
  (push #'hek-tempel-snippets tempel-template-sources)
  :bind
  (("M-=" . tempel-insert))
  :bind~
  (tempel-map
   ("S-TAB" . tempel-previous)
   ("TAB" . tempel-next)
   ("M-<escape>" . tempel-done)))

;;; lsp-snippet :: bridge between TempEl and Eglot
;;; https://github.com/svaante/lsp-snippet
;; (hek-usepkg lsp-snippet-tempel
;;   ;; :from package-vc "https://github.com/svaante/lsp-snippet"
;;   :from local
;;   :after eglot
;;   :config
;;   (lsp-snippet-tempel-eglot-init))

;; FIXME: Since `lsp-snippet' is not so stable until now, while `eglot' itself
;; only supports `yasnippet' as the template engine, I use `yasnippet' for
;; `eglot' completion only at present, until there is a better solution.

;;; YASnippet :: A template system
;;; https://github.com/joaotavora/yasnippet
(hek-usepkg yasnippet
  :from package
  :defer t
  :init
  (setq yas-snippet-dirs nil)
  :hook
  (eglot-managed-mode-hook . yas-minor-mode))
