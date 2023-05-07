;; --- Editing replated functions -*- lexical-binding: t -*-

;;;;; Mode line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; doom-modeline, https://github.com/seagle0128/doom-modeline
;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-continuous-word-count-modes '()
;;         doom-modeline-indent-info t
;;         doom-modeline-gnus nil
;;         doom-modeline-irc nil
;;         doom-modeline-battery nil
;;         doom-modeline-time nil
;;         doom-modeline-env-version nil
;;         doom-modeline-always-visible-segments '()
;;         ))

(use-package hek-modeline
  :ensure nil ;; my code
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


;;;;; Line & column ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Line number.
;(global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4)
(set-face-attribute 'line-number nil
  :family *my-mono-font-family* :height *my-mono-font-height* :slant 'italic)
(set-face-attribute 'line-number-current-line nil
  :family *my-mono-font-family* :height *my-mono-font-height* :slant 'normal)

;;; Highlight current line.
;(global-hl-line-mode 1)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;; Column number.
(column-number-mode 1)
(setq column-number-indicator-zero-based nil) ;; 1 = 1.

;;; Column indicator.
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;;; Indentation indicator.
(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'bitmap ;; Emacs with xpm support is required.
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0.5)
  :hook
  ((prog-mode tex-mode) . highlight-indent-guides-mode))


;;;;; Brackets and pairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Auto insert pairs.
(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;;; Show parenthesis.
(show-paren-mode 1)
(setq show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(set-face-background 'show-paren-match 'unspecified)


;;;;; Whitespaces and Coding style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Basic whitespaces.
(whitespace-mode 1)
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;;; Tab.
(setq-default
  indent-tabs-mode  nil
  tab-width         8)

;;; C style (cc-mode).
(defun +llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))
(defconst +my-c-style
  ;; GNU (built-in) +
  ;; LLVM (llvm-project/llvm/utils/emacs/emacs.el) +
  ;; customization
  '((fill-column . 80)
    (indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c++-indent-level . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-hanging-braces-alist
     . ((substatement-open before after)
        (arglist-cont-nonempty)))
    (c-offsets-alist
     . ((statement-block-intro . +)
        (statement-case-open . +)
        (statement-cont . +llvm-lineup-statement)
        (substatement-open . +)
        (substatement-label . 0)
        (label . 0)
        (knr-argdecl-intro . +)
        (arglist-intro . ++) ;; . c-lineup-arglist-intro-after-paren
        (arglist-close . c-lineup-arglist)
        (inline-open . 0)
        (member-init-intro . ++)
        (innamespace . 0)
        (brace-list-open . +)
        (brace-list-intro
         . (first
            c-lineup-2nd-brace-entry-in-arglist
            c-lineup-class-decl-init-+ +))
        (topmost-intro-cont
         . (first
            c-lineup-topmost-intro-cont
            c-lineup-gnu-DEFUN-intro-cont))))
    (c-special-indent-hook
     . ())
    (c-block-comment-prefix . "")))
(require 'llvm-c-style)
(c-add-style "my" +my-c-style)
(setq c-default-style '((java-mode . "java") (other . "my")))

;;; editorconfig, https://github.com/editorconfig/editorconfig-emacs.
(use-package editorconfig
  :init (editorconfig-mode 1))

;;;;; Spell / grammar check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Spell check
(add-hook 'prog-mode-hook #'flyspell-prog-mode) ;; TODO: Check spells in identifiers.
(add-hook 'text-mode-hook #'flyspell-mode)

;; TODO: Do grammar check with languagetool or ltex.


;;;;; Auto save and revert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; File auto save, backup and lock.
(setq load-prefer-newer  t
      make-backup-files  nil
      auto-save-default  nil
      create-lockfiles   nil)

;;; File auto revert.
(global-auto-revert-mode 1)
(setq auto-revert-verbose t
      auto-revert-use-notify t)


;;;;; Search and navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; consult, https://github.com/minad/consult
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key '(:debounce 0.75 any))
  (consult-customize consult-goto-line consult-line :preview-key '(:debounce 0.3 any))
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

;; ;;; avy, https://github.com/abo-abo/avy
;; (use-package avy
;;   :config
;;   (setq avy-style 'at-full
;;         avy-highlight-first t)
;;   :bind
;;   ("C-'" . avy-goto-char-2)
;;   ("C-\"" . avy-goto-word-1))

;;; ace-window, https://github.com/abo-abo/ace-window
(use-package ace-window
  :config
  (setq aw-minibuffer-flag t
        aw-ignore-current  t)
  :bind
  ("C-x o" . ace-window))

;;; Xref search
(setq xref-search-program 'ripgrep)


;;;;; Project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package project
  :ensure nil ;; built-in
  :config
  ;; --- hacks ---
  ;; -- add a project find function
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
  ;; --- hacks end ---
  )

;; ;;; Projectile, https://github.com/bbatsov/projectile
;; (use-package projectile
;;   :init
;;   (projectile-mode 1)
;;   :config
;;   (setq projectile-require-project-root t
;;         projectile-auto-discover nil
;;         projectile-completion-system 'default ;; vertico
;;         projectile-sort-order 'recently-active
;;         projectile-switch-project-action #'projectile-dired
;;         projectile-mode-line-prefix "PROJECT")
;;   :bind
;;   ("C-x p" . projectile-command-map))


;;;;; Compilations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +compile-auto-hide (buf msg)
  (unless (cl-search "abnormally" msg)
    (quit-window nil (get-buffer-window buf))))

(add-hook 'compilation-finish-functions #'+complile-auto-hide)
