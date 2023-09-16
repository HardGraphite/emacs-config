;; --- Extra utilities -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Dashboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not use dashboard now. Instead, put contents in scratch buffer at startup,
;; and erase buffer after first key stroke. Make sure `inhibit-startup-screen'
;; is non-nil so that the contents in scratch buffer is visible at startup.

(defun +my-scratch-hello-clear ()
  (remove-hook 'pre-command-hook #'+my-scratch-hello-clear)
  (with-current-buffer "*scratch*"
    (erase-buffer)
    (lisp-interaction-mode)
    (insert ";; " (buffer-name) " :: " mode-name ?\n ?\n ?\n)
    (backward-char)))

(defun +my-scratch-hello-setup ()
  (remove-hook 'emacs-startup-hook #'+my-scratch-hello-setup)
  (with-current-buffer "*scratch*"
    (require 'hek-subr)
    (add-hook 'pre-command-hook #'+my-scratch-hello-clear)
    (insert ?\n)

    ;; Emacs logo.
    (insert-image (create-image (concat *my-emacs-conf-dir* "misc/gnu_emacs.png")))
    (hek-center-line)
    (insert ?\n)

    ;; Emacs version.
    (insert (propertize
             (concat "GNU Emacs "
                     emacs-version
                     (when (daemonp) " (client)"))
             'face '(variable-pitch
                     (:slant italic :height 120))))
    (hek-center-line)
    (insert ?\n ?\n)

    (goto-char (point-min))))

(if (and command-line-args (cdr command-line-args)) ;; has extra arguments
    (+my-scratch-hello-clear)
  (setq initial-major-mode #'fundamental-mode)
  (add-hook 'emacs-startup-hook #'+my-scratch-hello-setup))


;;;;; Input methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hek-usepkg hek-exim
  :from local
  :config
  ;; Fcitx 5, D-Bus
  (require 'dbus)
  (setq hek-exim-get-source-function #'hek-exim-fcitx5-dbus-get-state
        hek-exim-set-source-function #'hek-exim-fcitx5-dbus-set-state)
  (when (featurep 'meow)
    (setq hek-exim-automodal-hooks
          '(meow-insert-enter-hook . meow-insert-exit-hook))
    (hek-exim-automodal-mode))
  (hek-exim-inlinetext-mode)
  (setq hek-exim-verbose t) ;; For debug.
  :bind
  (("<f9>" . hek-exim-toggle)
   ("S-<f9>" . hek-exim-inlinetext-create)))


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
        vterm-kill-buffer-on-exit t
        vterm-keymap-exceptions ())
  ;; Disable solaire-mode.
  (when (boundp '+solaire-mode-exclude-mode-list) ;; See init-theme.el
    (add-to-list '+solaire-mode-exclude-mode-list 'vterm-mode))
  ;; Key map for modal editing normal state.
  (defvar +vterm-modal-normal-state-map (make-sparse-keymap))
  (dolist (x '((yank . vterm-yank)
               (undo . vterm-undo)
               (xterm-paste . vterm-xterm-paste)
               (yank-pop . vterm-yank-pop)
               (mouse-yank-primary . vterm-yank-primary)
               (self-insert-command . vterm--self-insert)
               (beginning-of-defun . vterm-previous-prompt)
               (end-of-defun . vterm-next-prompt)))
    (define-key +vterm-modal-normal-state-map
                (vector 'remap (car x)) (cdr x)))
  ;; Buffer local configuration.
  (defun +vterm-local-init ()
    ;; Use a different font.
    (buffer-face-set
     (list :family *my-term-font-family*
           :height *my-term-font-height*))
    ;; Meow integration.
    (add-hook 'meow-insert-enter-hook
              (lambda ()
                (use-local-map vterm-mode-map)
                (vterm-reset-cursor-point))
              nil t)
    (add-hook 'meow-insert-exit-hook
              (lambda ()
                (use-local-map +vterm-modal-normal-state-map))
              nil t)
    )
  ;; HACK: keep cursor-type unchanged.
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add
   'vterm--redraw :around
   (lambda (fn &rest args)
     (let ((cursor-type cursor-type))
       (apply fn args))))
  :bind
  (("<f12>" . vterm-other-window))
  :bind~
  (vterm-mode-map
   ("C-<escape>" . vterm-send-next-key)
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
