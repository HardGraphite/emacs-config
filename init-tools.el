;; --- Extra utilities -*- lexical-binding: t -*-

;;;;; Dashboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-dashboard, https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (when (daemonp)
    (setq initial-buffer-choice
      (lambda ()
        (dashboard-refresh-buffer)
        ;;(get-buffer-create "*dashboard*")
        )))
  :config
  (setq dashboard-display-icons-p t
        dashboard-banner-logo-title nil
        dashboard-items
          '((recents . 5) (projects . 5) (bookmarks . 5) (agenda . 5) (registers . 5))
        dashboard-projects-backend 'project-el ;; or projectile if installed
        dashboard-center-content  t
        dashboard-set-init-info   nil
        dashboard-init-info       nil
        dashboard-set-footer      t
        dashboard-footer          nil
        ;;dashboard-footer-icon     nil
        dashboard-footer-messages nil)
  (defun dashboard-display-icons-p ()
    t)
  (defun dashboard-insert-footer ()
    (insert "\n")
    (dashboard-insert-center
      dashboard-footer-icon
      " "
      (propertize (concat "GNU Emacs " emacs-version (when (daemonp) " (client)"))
        'face 'dashboard-footer)
      "\n"))
  (set-face-attribute 'dashboard-banner-logo-title nil
    :family *my-text-font-family*)
  (set-face-attribute 'dashboard-heading nil
    :family *my-text-font-family* :height 170)
  )


;;;;; Terminal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs-libvterm, https://github.com/akermu/emacs-libvterm
(use-package vterm
  :defer t
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
  :bind
  (:map vterm-mode-map
   ("C-<escape>" . vterm-send-next-key)
   ("C-y" . vterm-copy-mode)
   ("C-x" . vterm--self-insert)
   ("C-c" . vterm--self-insert))
  :hook
  (vterm-mode . +vterm-local-init))


;;;;; Version control systems ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit, https://github.com/magit/magit
(use-package magit
  :defer t
  :config
  (setq magit-save-repository-buffers nil))

;; diff-hl, https://github.com/dgutov/diff-hl
(use-package diff-hl
  :defer t
  :hook
  (dired-mode . diff-hl-dired-mode)
  ((prog-mode tex-mode markdown-mode) . diff-hl-mode)
  (magit-pre-refresh diff-hl-magit-pre-refresh)
  (magit-post-refresh diff-hl-magit-post-refresh))

