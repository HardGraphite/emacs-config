;; --- Editing replated functions -*- lexical-binding: t; no-byte-compile: t -*-

(require 'hek-subr)

;;;;; Mode line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hek-usepkg hek-modeline
  :from local
  :config
  (hek-modeline-mode 1))


;;;;; Scrolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 5)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command #'hek-scroll-down)
(defalias 'scroll-down-command #'hek-scroll-up)

;;;;; Line & column ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Line number, current line, unused lines.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (when (or buffer-file-name
             (derived-mode-p 'prog-mode 'text-mode))
     (display-line-numbers-mode 1)
     (hl-line-mode 1)
     (setq indicate-empty-lines t))))
;; -- line number
(setq display-line-numbers-width-start 500)
(custom-set-faces
 `(line-number
   ((t :family ,*my-mono-font-family*
       :height ,(- *my-mono-font-height* 10)
       :slant italic)))
 '(line-number-current-line
   ((t :inherit line-number
       :slant normal
       :foreground "olive drab"))))
;; -- current line
(setq hl-line-sticky-flag nil) ;; Don't show highlights in all windows.

;;; Column indicator.
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;;; Indentation indicator.
(add-hook
 'prog-mode-hook
 (lambda ()
   (unless indent-tabs-mode
     (hek-hl-indent-mode))))


;;;;; Brackets and pairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Auto insert pairs.
(electric-pair-mode 1)
(defun +electric-pair-inhibit (char)
  (or (eq char (char-after))
      (eq (char-syntax (following-char)) ?w)))
(setq electric-pair-inhibit-predicate '+electric-pair-inhibit)

;;; Show parenthesis.
(show-paren-mode 1)
(setq show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(custom-set-faces
 '(show-paren-match
   ((t :foreground unspecified :background unspecified
       :weight ultra-bold :underline t))))

;;; rainbow-delimiters :: highlights delimiters such as parentheses according to the depth
;;; https://github.com/Fanael/rainbow-delimiters
(hek-usepkg rainbow-delimiters
  :from package
  :hook
  (emacs-lisp-mode-hook . rainbow-delimiters-mode))

;;;;; Whitespace and Coding style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Whitespace visualization.
(setq whitespace-line-column nil
      whitespace-style '(face tabs tab-mark trailing missing-newline-at-eof)
      whitespace-display-mappings '((tab-mark ?\t [?» ?\t])
                                    (newline-mark ?\n [?↵ ?\n])
                                    (space-mark ?\  [?·] [?.])))
(custom-set-faces
 '(trailing-whitespace ((t (:strike-through "#ff6c6b" :background unspecified)))))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

;;; Delete trailing whitespace on save.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode 'text-mode)
              (delete-trailing-whitespace))))

;;; hungry-delete, https://github.com/nflath/hungry-delete
(hek-usepkg hungry-delete
  :from package
  :init
  (global-hungry-delete-mode)
  :config
  (setq hungry-delete-join-reluctantly nil
        hungry-delete-chars-to-skip " \t"))

;;; Tab.
(setq-default
  indent-tabs-mode  nil
  tab-width         8)

;;; editorconfig, https://github.com/editorconfig/editorconfig-emacs.
(hek-usepkg editorconfig
  :from package
  :init (editorconfig-mode 1))

;;;;; Spell / grammar check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Spell check
(add-hook 'prog-mode-hook #'flyspell-prog-mode) ;; TODO: Check spells in identifiers.
(add-hook 'text-mode-hook #'flyspell-mode)
(with-eval-after-load "flyspell"
  (require 'hek-spell)
  (hek-spell-mode 1))

;; TODO: Do grammar check with languagetool or ltex.

;;; hl-todo :: Highlight TODO keywords
;;; https://github.com/tarsius/hl-todo
(hek-usepkg hl-todo
  :from package
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"    warning bold italic underline)
          ("XXX"     warning bold italic underline)
          ("HACK"    font-lock-constant-face bold italic underline)
          ("FIXME"   error   bold italic underline)
          ("NOTE"    success bold italic underline)))
  :hook
  (prog-mode-hook . hl-todo-mode))

;;;;; File save, revert and record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; File auto save, backup and lock.
(setq load-prefer-newer  t
      make-backup-files  nil
      auto-save-default  nil
      create-lockfiles   nil)

;;; File auto revert.
(setq auto-revert-verbose t
      auto-revert-interval 30)
(global-auto-revert-mode 1)

;;; Recent files.
(setq recentf-auto-cleanup 'never)
(add-hook 'emacs-startup-hook #'recentf-mode)


;;;;; Search and navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; consult, https://github.com/minad/consult
(hek-usepkg consult
  :from package
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-preview-key '(:debounce 0.25 any)
        consult-fontify-max-size 65536
        consult-preview-raw-size 65536)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")
  :bind
  (;; Replace default commands.
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ))

;;; rg.el :: search tool based on ripgrep
;;; https://github.com/dajva/rg.el
(hek-usepkg rg
  :from package
  :config
  ;; HACK: use package.el
  (defun rg-project-root (file)
    (project-root (project-current)))
  :bind
  (("C-x p g" . rg-project))
  :bind~
  (rg-mode-map
   ("?" . rg-menu)))

;;; avy :: Jump to things in Emacs tree-style
;;; https://github.com/abo-abo/avy
(hek-usepkg avy
  :from package
  :config
  (setq avy-style 'at-full
        avy-timeout-seconds 0.4
        avy-highlight-first t)
  ;; :bind
  ;; key bindings are defined in `init-keymaps.el'.
  )

;;; Xref search
(setq xref-search-program 'ripgrep)


;;;;; Tabs, windows, and buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ace-window :: select window by number
;;; https://github.com/abo-abo/ace-window
(hek-usepkg ace-window
  :from package
  :config
  (setq aw-minibuffer-flag t
        aw-ignore-current  t)
  :bind
  (("C-x o" . ace-window)))

;;; winner (builtin) :: records of the changes in window layout configuration
(hek-usepkg winner
  :from builtin
  :init
  (setq winner-ring-size 16
        winner-dont-bind-my-keys t)
  :hook
  (emacs-startup-hook . winner-mode))

;;; Tab bar (builtin)
(hek-usepkg tab-bar
  :from builtin
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show t
        tab-bar-auto-width nil
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
        tab-bar-separator "  "
        tab-bar-new-tab-choice "*scratch*")
  (custom-set-faces
   `(tab-bar-tab ((t :family ,*my-mono-font-family* :height ,(- *my-mono-font-height* 5) :overline t)))
   '(tab-bar-tab-inactive ((t :slant italic :overline nil)))))


;;;;; Project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hek-usepkg project
  :from builtin
  :config
  ;; --- HACK: add a project find function
  (defvar +project-root-marker-file-list
    '(".git" "Makefile" "CMakeLists.txt")
    "Files that may exist in project root.")
  (defun +project-root-marker-file-exists-p (dir)
    (catch 'found
      (dolist (file +project-root-marker-file-list)
        (when (file-exists-p (concat dir file))
          (throw 'found t)))))
  (defun +project-root-marker-file-find-root (path)
    (when-let ((root-path (locate-dominating-file path #'+project-root-marker-file-exists-p)))
      (cons 'transient root-path)))
  (add-to-list 'project-find-functions #'+project-root-marker-file-find-root)
  ;; ---
  )


;;;;; Compilations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +compile-auto-hide (buf msg)
  (unless (cl-search "abnormally" msg)
    (quit-window nil (get-buffer-window buf))))

(add-hook 'compilation-finish-functions #'+compile-auto-hide)
