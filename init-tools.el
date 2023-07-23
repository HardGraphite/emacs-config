;; --- Extra utilities -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Dashboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hek-usepkg hek-homepage
  :from local
  :when (or (not command-line-args)
            (not (cdr command-line-args))) ;; No extra arguments.
  :init
  (setq initial-major-mode #'fundamental-mode
        inhibit-startup-screen t)
  (defun +hek-homepage-init ()
    (when (string-equal (buffer-name) "*scratch*")
      (setq hek-homepage-logo (concat *my-emacs-conf-dir* "misc/gnu_emacs.png")
            hek-homepage-banner "Hello, world!"
            hek-homepage-links
            `((("Scratch as Lisp playground" "i" .
                ,(lambda () (interactive) (hek-homepage-cleanup #'lisp-interaction-mode)))
               ("Scratch as text pad" "t" .
                ,(lambda () (interactive) (hek-homepage-cleanup #'text-mode))))
              (("Find file" "f" . find-file)
               ("Open recent file" "r" . consult-recent-file)
               ("Switch to project" "p" . project-switch-project))
              (("Other buffer" "b" . consult-buffer)
               ("Quit" "Q" . save-buffers-kill-terminal))))
      (hek-homepage-setup)
      (when (boundp meow-mode)
        (meow-mode -1))
      ;; (when (boundp solaire-mode)
      ;;   (solaire-mode -1))
      ))
  (add-hook 'emacs-startup-hook #'+hek-homepage-init 80))


;;;;; Directory view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; dirvish :: an improved version of `dired'
;;; https://github.com/alexluigit/dirvish
(hek-usepkg dirvish
  :from package
  :config
  (setq dirvish-use-header-line 'global
        dirvish-attributes '(vc-state subtree-state nerd-icons file-time file-size)
        dirvish-subtree-state-style 'nerd
        dirvish-subtree-always-show-state t)
  :bind~
  (dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ("`" . dirvish-layout-toggle)
   ("/" . dired-isearch-filenames))
  :hook
  (after-init-hook . dirvish-override-dired-mode))


;;;;; Terminal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs-libvterm, https://github.com/akermu/emacs-libvterm
(hek-usepkg vterm
  :from package
  :init
  (defun +shell-vterm (&optional buffer-name)
    (if (stringp buffer-name)
      (if-let (buffer (get-buffer buffer-name))
        (switch-to-buffer buffer)
        (vterm buffer-name))
      (vterm)))
  (advice-add 'shell :override #'+shell-vterm)
  :config
  (setq vterm-shell *my-shell*
        vterm-kill-buffer-on-exit t)
  (defun +vterm-local-init ()
    (buffer-face-set :family *my-term-font-family* :height *my-term-font-height*))
  :bind
  (("<f12>" . vterm-other-window))
  :bind~
  (vterm-mode-map
   ("C-<escape>" . vterm-send-next-key)
   ("C-y" . vterm-copy-mode)
   ("C-x" . vterm--self-insert)
   ("C-c" . vterm--self-insert))
  :hook
  (vterm-mode-hook . +vterm-local-init))


;;;;; Version control systems ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit, https://github.com/magit/magit
(hek-usepkg magit
  :from package
  :defer t
  :config
  (setq magit-save-repository-buffers nil))

;; diff-hl, https://github.com/dgutov/diff-hl
(hek-usepkg diff-hl
  :from package
  :hook
  ;; (dired-mode-hook . diff-hl-dired-mode) ;; use `dirvish-vc' instead
  ((prog-mode-hook tex-mode-hook markdown-mode) . diff-hl-mode)
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))
