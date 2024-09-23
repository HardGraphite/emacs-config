;;; -*- lexical-binding: t; no-byte-compile: t; outline-regexp: ";;;;;\\*+"; -*-

(require 'hek-subr)


;;;;;* SYSTEM

;;;;;** Package management

;;; Prepare `hek-usepkg', which helps to setup packages.
(eval-when-compile
  (require 'hek-usepkg))
(hek-usepkg-gitpkg-initialize)

;;; Install packages.
(when (bound-and-true-p option/install-packages)
  (setq hek-usepkg-ensure t
        hek-usepkg-debug t)
  (setq package-native-compile t
        native-comp-async-query-on-exit t)
  (when package-quickstart
    (add-hook 'kill-emacs-hook #'package-quickstart-refresh))
  (add-hook 'kill-emacs-hook #'hek-usepkg-gitpkg-quickstart-refresh)
  (package-refresh-contents))


;;;;;** Frame and basic UI

;; Title.
(setq frame-title-format (list (concat "%b — Emacs" (when (daemonp) " Client")))
      icon-title-format t)

;; Resize method.
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; Window split.
(setq split-width-threshold  (* 2 72)
      split-height-threshold (* 2 25))

;; Minibuffer tweak.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.5)

;; Disable tooltip.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Cursor.
(setq x-stretch-cursor t)
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)


;;;;;** System misc

;;; Confirm before quit.
(setq confirm-kill-emacs 'y-or-n-p)

;;; Use UTF-8.
(set-default-coding-systems 'utf-8)

;;; Message buffer, scratch buffer.
(setq messages-buffer-max-lines 100
      initial-scratch-message   ";; This buffer is NOT part of GNU Emacs.\n\n"
      initial-major-mode        #'fundamental-mode)


;;;;;** Hacks

;;; Increase read chunk size. For faster file read and LSP performance.
(setq read-process-output-max #x100000) ; 1 MiB

;;; Inhibit fontification when inputing, helping a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Disable bidi (Bidirectional Display) and decrease long line thresholds.
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; GCMH :: the Garbage Collector Magic Hack
;;; https://github.com/emacsmirror/gcmh
(hek-usepkg gcmh
  :from package
  :init
  (defun +gcmh-setup ()
    (setq gcmh-idle-delay 'auto
          gcmh-auto-idle-delay-factor 16
          gcmh-high-cons-threshold #x1000000) ;; 16 MiB
    (gcmh-mode 1))
  :hook
  (emacs-startup-hook . +gcmh-setup))


;;;;;** Specific system

(pcase system-type
  ('android
   ;; Work with Termux.
   (let ((termux-bin-dir "/data/data/com.termux/files/usr/bin"))
     (setenv "PATH" (concat termux-bin-dir ":" (getenv "PATH")))
     (push termux-bin-dir exec-path))
   ;; Convenient bars and key bindings.
   (require 'hek-touchscreen)
   (add-to-list 'default-frame-alist '(tool-bar-position . bottom))
   (menu-bar-mode)
   (tool-bar-mode)
   (let ((img-dir (concat config/emacs-conf-dir "images/")))
     (hek-touchscreen-bar-add 'esc "Esc key" #'hek-touchscreen-esc (concat img-dir "esc.xpm"))
     (hek-touchscreen-bar-add 'tab "Tab key" #'hek-touchscreen-tab (concat img-dir "tab.xpm"))
     (hek-touchscreen-bar-mode))
   (setq touch-screen-display-keyboard t)
   (global-set-key (kbd "<volume-up>") #'hek-touchscreen-toggle-disp-kbd)
   (global-set-key (kbd "<volume-down>") #'hek-touchscreen-C-g)
   ;; Disable text conversion by default. Will be handled later by pakcage `hek-exim'.
   (setq overriding-text-conversion-style nil)
   ))


;;;;;* THEME

;;;;;** Fonts

;;; Global default font.
(custom-set-faces
 `(default ((t :family ,(car config/code-font) :height ,(cdr config/code-font)))))
(let ((spec (font-spec :family (car config/cjkx-font))))
  (set-fontset-font t 'han spec)
  (set-fontset-font t 'cjk-misc spec))

;;; Minibuffer default font.
(defconst +my-minibuffer-font-remapping-alist
  `((default :family ,(car config/mono-font) :height ,(cdr config/mono-font))))
(defun +my-minibuffer-font-setup ()
  (set (make-local-variable 'face-remapping-alist)
       +my-minibuffer-font-remapping-alist))
(add-hook 'minibuffer-setup-hook #'+my-minibuffer-font-setup)

;;; Mode line defualt font.
(custom-set-faces
  `(mode-line-active ((t :family ,(car config/mono-font) :height ,(- (cdr config/mono-font) 10))))
  `(mode-line-inactive ((t :family ,(car config/mono-font) :height ,(- (cdr config/mono-font) 10)))))

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
  (setq nerd-icons-font-family (car config/nerd-font)))


;;;;;** Color theme

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
  :init
  (defvar +solaire-mode-exclude-mode-list ()
    "List of extra modes to enable solaire-mode.")
  :config
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          (or (buffer-file-name (buffer-base-buffer))
              (derived-mode-p 'prog-mode 'text-mode)
              (memq major-mode +solaire-mode-exclude-mode-list))))
  ;; Use a different font.
  (custom-set-faces
   `(solaire-default-face
     ((t :family ,(car config/mono-font) :height ,(cdr config/mono-font)))))
  (solaire-global-mode 1))


;;;;;** Effects

;;; Pulse effects from package `pulse.el'.
(setq pulse-delay 0.04
      pulse-iterations 12)


;;;;;* EDITOR

;;;;;** Appearance & behavior

;;;;;*** Mode line

(hek-usepkg hek-modeline
  :from local
  :config
  (hek-modeline-mode 1))

;;;;;*** Scrolling

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

;;;;;*** Line & column

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
   ((t :family ,(car config/mono-font)
       :height ,(- (cdr config/mono-font) 10)
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
(hek-usepkg indent-bars
  :from package
  :config
  (setq indent-bars-width-frac 0.2
        indent-bars-pattern "."
        indent-bars-highlight-current-depth nil
        indent-bars-display-on-blank-lines nil ;; for better performance
        indent-bars-starting-column 0
        indent-bars-color '(highlight :face-bg t :blend 0.2))
  (when (and (eq window-system 'pgtk)
             (< emacs-major-version 30))
    (setq indent-bars-prefer-character t))
  :hook ((prog-mode-hook tex-mode-hook) . indent-bars-mode))

;;;;;*** Brackets and pairs

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

;;;;;*** Whitespace and Coding style

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

;;;;;*** Special highlights

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


;;;;;** Tabs, windows, and buffers

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
   `(tab-bar-tab ((t :family ,(car config/mono-font) :height ,(- (cdr config/mono-font) 5) :overline t)))
   '(tab-bar-tab-inactive ((t :slant italic :overline nil)))))


;;;;;** File

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


;;;;;** Project

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


;;;;;** Compilation

(defun +compile-auto-hide (buf msg)
  (unless (cl-search "abnormally" msg)
    (quit-window nil (get-buffer-window buf))))

(add-hook 'compilation-finish-functions #'+compile-auto-hide)


;;;;;** Search and navigation

;;; consult :: Consulting completing-read
;;; https://github.com/minad/consult
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


;;;;;** Completion

;; TODO: embark, embark-consult, wgrep

;;;;;*** Completion algorithm and category

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
      completion-category-overrides
      '((consult-location (styles orderless basic))
        (file (styles partial-completion))))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local completion-styles '(orderless basic)))) ;; minibuffer

;;;;;*** Minibuffer completion

;;; vertico :: VERTical Interactive COmpletion
;;; https://github.com/minad/vertico
(hek-usepkg vertico
  :from package
  :defer 0 ;; load after init
  :config
  (setq vertico-scroll-margin 2
        vertico-count 10
        vertico-cycle t)
  (vertico-mode 1)
  :bind~
  (vertico-map
   ("M-j" . vertico-next)
   ("M-k" . vertico-previous)))
(hek-usepkg vertico-directory
  :after vertico
  :bind~
  (vertico-map
   ("RET" . vertico-directory-enter)))

;;; vertico-posframe :: an extension for `vertico' to show contents in a child frame
;;; https://github.com/tumashu/vertico-posframe
(hek-usepkg vertico-posframe
  :from package
  :after vertico
  :config
  (defun +vertico-posframe-size (buffer)
    (let ((h (1+ vertico-count))
          (w (min 120 (- (frame-width) 2))))
      (list :height h :width w :min-height h :min-width w)))
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
        vertico-posframe-size-function #'+vertico-posframe-size
        vertico-posframe-border-width 3
        vertico-posframe-parameters
        '(;; (alpha-background . 72) ;; XXX: `alpha-background' may not work on some window systems
          (left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

;;; marginalia :: Marginalia in the minibuffer
;;; https://github.com/minad/marginalia
(hek-usepkg marginalia
  :from package
  :init
  (marginalia-mode 1))

;;;;;*** Code (in-buffer) completion

;;; corfu, https://github.com/minad/corfu
(hek-usepkg corfu
  :from package
  :defer 0
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'valid
        corfu-echo-documentation t)
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (require 'hek-corfu-nerd-icons)
  (add-to-list 'corfu-margin-formatters
               #'hek-corfu-nerd-icons-margin-formatter)
  (global-corfu-mode 1)
  ;; extension: corfu-popupinfo
  (setq corfu-popupinfo-delay '(1.0 . 0.6))
  (corfu-popupinfo-mode 1)
  :bind~
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

;;;;;*** Code templates

;;; TempEl :: code templates (snippets)
;;; https://github.com/minad/tempel
(hek-usepkg tempel
  :from package
  :config
  (setq hek-tempel-snippets-dir (concat config/emacs-conf-dir "snippets/"))
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
;; only supports `yasnippet' as the template engine, `yasnippet' is used for
;; `eglot' completion only at present, until there is a better solution.

;;; YASnippet :: A template system
;;; https://github.com/joaotavora/yasnippet
(hek-usepkg yasnippet
  :from package
  :defer t
  :init
  (setq yas-snippet-dirs nil
        yas-verbosity 0)
  (fset 'yas--message #'ignore)
  )


;;;;;** Spell / grammar check

;;; Spell check
(defun +flyspell-setup ()
  (require 'ispell)
  (if (not (executable-find ispell-program-name))
      (unload-feature 'ispell)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode) ;; TODO: Check spells in identifiers.
    (add-hook 'text-mode-hook #'flyspell-mode)
    (require 'flyspell)
    (require 'hek-spell)
    (hek-spell-mode 1))
  (fmakunbound '+flyspell-setup))
(add-hook 'emacs-startup-hook #'+flyspell-setup)

;; TODO: better spell checks.


;;;;;* LANGUAGES

;;;;;** Common programming support

;;; Eglot :: LSP (language server protocol) support
(hek-usepkg eglot
  :from builtin
  :init
  ;; eglot options
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  ;; LSP servers
  (setq eglot-server-programs
        '(((c-mode c++-mode c-ts-mode c++-ts-mode)
           "clangd" "--function-arg-placeholders=0" "--header-insertion=never")
          ((cmake-mode cmake-ts-mode) "cmake-language-server")
          (python-base-mode "pyright-langserver" "--stdio")
          (bash-ts-mode "bash-language-server" "start")
          (lua-mode "lua-language-server")
          ((tex-mode bibtex-mode) "texlab")))
  ;; mode hooks
  (dolist (config eglot-server-programs)
    (let ((mode-list (car config)))
      (unless (listp mode-list)
        (setq mode-list (list mode-list)))
      (dolist (mode mode-list)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  #'eglot-ensure))))
  :hook ;; DO NOT delete this line.
  )

;;; ElDoc box :: displays ElDoc documentations in a childframe
;;; https://github.com/casouri/eldoc-box/tree/master
(hek-usepkg eldoc-box
  :from package
  :config
  (custom-set-faces
   `(eldoc-box-body ((t :family ,(car config/mono-font) :height ,(cdr config/mono-font)))))
  :hook
  (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  )

;;; treesit :: Tree-sitter support, providing syntax highlights and querying.
(hek-usepkg treesit
  :from builtin
  :when-ensure
  (setq treesit-language-source-alist
        '(;; (bash "https://github.com/tree-sitter/tree-sitter-bash")
          ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (dolist (x treesit-language-source-alist)
    (let ((lang (car x)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))
  :init
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode))))

;;; Dape :: DAP (Debug Adapter Protocol) support
;;; https://github.com/svaante/dape
(hek-usepkg dape
  :from package
  :defer t
  :config
  (setq dape-buffer-window-arrangment 'right)
  )

;;;;;** C-like languages

(with-eval-after-load "cc-mode"

  (defconst +my-c-style
    '((fill-column . 80)
      (indent-tabs-mode . nil)
      (c-basic-offset . 4)
      (c++-indent-level . 4)
      (c-comment-only-line-offset . (0 . 0))
      (c-block-comment-prefix . "")
      (c-hanging-braces-alist
       . ((defun-open after)
          (substatement-open after)
          (brace-list-open after)))
      (c-offsets-alist
       . ((arglist-intro . +) ;; c-lineup-arglist-intro-after-paren
          (arglist-cont-nonempty . 0)
          (arglist-cont . 0)
          (arglist-close . 0) ;; c-lineup-arglist
          (statement-block-intro . +)
          (statement-case-open . +)
          (statement-cont . +)
          (substatement-open . 0)
          (substatement-label . 0)
          (label . 0)
          (brace-list-open . 0)
          (brace-list-intro . +)
          (topmost-intro . 0)
          (topmost-intro-cont . 0)
          (inline-open . 0)
          (member-init-intro . +)
          (innamespace . 0)
          (template-args-cont . +)
          ))
      (c-special-indent-hook
       . ())
      ))

  (c-add-style "my" +my-c-style)
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode  . "awk")
          (other     . "my")))
  )

(with-eval-after-load "c-ts-mode"

  (defconst +c-ts-mode-indent-style
    ;; Adapted from function `c-ts-mode--indent-styles'.
    '((c-ts-mode--for-each-tail-body-matcher prev-line c-ts-mode-indent-offset)
      (c-ts-mode--else-heuristic prev-line c-ts-mode-indent-offset)
      ((parent-is "translation_unit") column-0 0)
      ((query "(ERROR (ERROR)) @indent") prev-line 0)
      ((node-is ")") parent-bol 0)
      ((node-is "]") parent-bol 0)
      ((node-is "else") parent-bol 0)
      ((node-is "case") parent-bol 0)
      ((node-is "preproc_arg") no-indent)
      ((and (parent-is "comment") c-ts-common-looking-at-star) c-ts-common-comment-start-after-first-star -1)
      (c-ts-common-comment-2nd-line-matcher c-ts-common-comment-2nd-line-anchor 1)
      ((parent-is "comment") prev-adaptive-prefix 0)
      ((node-is "labeled_statement") standalone-parent 0)
      ((parent-is "labeled_statement") c-ts-mode--standalone-grandparent c-ts-mode-indent-offset)
      ((node-is "preproc") column-0 0)
      ((node-is "#endif") column-0 0)
      ((match "preproc_call" "compound_statement") column-0 0)
      ((n-p-gp nil "preproc" "translation_unit") column-0 0)
      ((and no-node (parent-is "\\(?:\n\\|preproc\\)")) c-ts-mode--standalone-parent-skip-preproc c-ts-mode--preproc-offset)
      ((match nil "preproc_\\(?:\\(?:el\\)?if\\)" nil 3 3) c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((match nil "preproc_ifdef" nil 2 2) c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((match nil "preproc_else" nil 1 1) c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((parent-is "preproc") c-ts-mode--anchor-prev-sibling 0)
      ((parent-is "function_definition") parent-bol 0)
      ((parent-is "pointer_declarator") parent-bol 0)
      ((parent-is "\\`declaration\\'") parent-bol 0)
      ((parent-is "conditional_expression") parent-bol c-ts-mode-indent-offset)
      ((parent-is "assignment_expression") parent-bol c-ts-mode-indent-offset)
      ((parent-is "concatenated_string") parent-bol c-ts-mode-indent-offset)
      ((parent-is "comma_expression") parent-bol c-ts-mode-indent-offset)
      ((parent-is "init_declarator") parent-bol c-ts-mode-indent-offset)
      ((parent-is "parenthesized_expression") parent-bol c-ts-mode-indent-offset)
      ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
      ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)
      ((parent-is "binary_expression") parent-bol c-ts-mode-indent-offset)
      ((query "(for_statement initializer: (_) @indent)") parent-bol c-ts-mode-indent-offset)
      ((query "(for_statement condition: (_) @indent)") parent-bol c-ts-mode-indent-offset)
      ((query "(for_statement update: (_) @indent)") parent-bol c-ts-mode-indent-offset)
      ((query "(call_expression arguments: (_) @indent)") parent c-ts-mode-indent-offset)
      ((parent-is "call_expression") parent-bol c-ts-mode-indent-offset)
      ((node-is "}") standalone-parent 0)
      ((n-p-gp nil nil "namespace_definition") grand-parent 0) ; C++
      ((node-is "access_specifier") parent-bol 0) ; C++
      ((parent-is "declaration_list") parent-bol c-ts-mode-indent-offset) ; C++
      ((match nil "initializer_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "initializer_list") c-ts-mode--anchor-prev-sibling 0)
      ((match nil "enumerator_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
      ((parent-is "enumerator_list") c-ts-mode--anchor-prev-sibling 0)
      ((match nil "field_declaration_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
      ((parent-is "field_declaration_list") c-ts-mode--anchor-prev-sibling 0)
      ((or (and (parent-is "compound_statement") c-ts-mode--first-sibling) (match null "compound_statement")) standalone-parent c-ts-mode-indent-offset)
      ((parent-is "compound_statement") c-ts-mode--anchor-prev-sibling 0)
      ((node-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
      ((match "expression_statement" nil "body") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "else_clause") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
      ((match "while" "do_statement") parent-bol 0) ; (do_statement "while")
      ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)
      ((node-is "field_initializer_list") parent-bol c-ts-mode-indent-offset) ; C++
      (no-node prev-line 0)
      ))

  (defun +c-ts-mode-indent-style ()
    +c-ts-mode-indent-style)

  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style #'+c-ts-mode-indent-style)

  )

;;;;;** CMake

;;; CMake mode
;;; https://gitlab.kitware.com/cmake/cmake/-/blob/master/Auxiliary/cmake-mode.el
(hek-usepkg cmake-mode
  :from package
  :defer t
  :config
  (setq cmake-tab-width 4)
  :bind~
  (cmake-mode-map
   ("C-c C-h" . cmake-help)))

;;;;;** Lua

;;; lua mode
;;; https://github.com/immerrr/lua-mode

(hek-usepkg lua-mode
  :from package
  :defer t
  )

;;;;;** Markdown

;;; Markdown mode
;;; https://github.com/jrblevin/markdown-mode
(hek-usepkg markdown-mode
  :from package
  :defer t
  :config
  (setq markdown-split-window-direction 'right
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-mouse-follow-link nil
        markdown-command
        '("pandoc" "--from=markdown" "--to=html5" "--no-highlight")
        markdown-fontify-code-blocks-natively t
        markdown-code-lang-modes
        '(("c" . c-mode)
          ("c++" . c++-mode)
          ("cpp" . c++-mode)
          ("python" . python-mode)
          ("sh" . sh-mode)
          ("shell" . sh-mode))))

;;;;;** Org Mode

(hek-usepkg org
  :from builtin
  :defer t
  :config
  (defface +org-num '((t :height 0.9 :slant italic)) "")
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-tags-column 0 ; don't flushright tags
        org-num-face '+org-num)
  (defun +org-mode-buffer-setup ()
    (org-num-mode)
    (org-modern-mode)
    (org-appear-mode)
    (when display-fill-column-indicator-mode
      (display-fill-column-indicator-mode -1))
    (when (> (buffer-size) 0) ; default read-only if not empty
      (read-only-mode)))
  (add-hook 'org-mode-hook #'+org-mode-buffer-setup 90))

;;; org-modern :: Modern Org Style
;;; https://github.com/minad/org-modern
(hek-usepkg org-modern
  :from package
  :defer t
  :config
  (setq org-modern-star 'replace
        org-modern-replace-stars "◉◈◇"
        org-modern-table-vertical 1
        org-modern-table-horizontal 1.0 ; default height, to make the `org-modern--table-overline' hacking below work well; the author of this package seems to try to decrease the line height of horizontal table lines, which some how does not work in my configuration
        org-modern--table-overline '(:strike-through t) ; WARNING: this variable is defined with a defconst form
        org-modern-list '((?+ . "◦") (?- . "•") (?* . "◦"))
        org-modern-block-name '("◾" . "◽")
        org-modern-block-fringe nil ; `display-line-numbers-mode' makes this ugly
        ))

;; org-appear :: Make invisible parts of Org elements appear visible
;; https://github.com/awth13/org-appear
(hek-usepkg org-appear
  :from package
  :defer t
  :config
  (setq org-appear-trigger 'on-change
        org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autoentities t
        org-appear-autosubmarkers t))

;;;;;** Verilog HDL / SystemVerilog

(hek-usepkg verilog-mode
  :from builtin
  :defer t
  :config
  (setq verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-auto-inst-vector nil
        verilog-auto-lineup '(all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-highlight-grouping-keywords t
        verilog-indent-level 4
        verilog-indent-level-behavioral 4
        verilog-indent-level-declaration 4
        verilog-indent-level-directive 4
        verilog-indent-level-module 4
        verilog-indent-lists nil
        verilog-case-indent 4
        verilog-cexp-indent 4
        verilog-indent-begin-after-if nil
        )
  ;; (setq verilog-linter "verilator -–lint-only -Wall")
  (require 'hek-flymake-verilator)
  (when (ignore-errors (hek-flymake-verilator-setup) t)
    (flymake-mode 1)))


;;;;;* APPLICATIONS

;;;;;** Dashboard

;; Do not use dashboard now. Instead, put contents in scratch buffer at startup,
;; and erase buffer after first key stroke. Make sure `inhibit-startup-screen'
;; is non-nil so that the contents in scratch buffer is visible at startup.

(defconst +my-scratch-hello-message initial-scratch-message)
(setq initial-scratch-message nil)

(defun +my-scratch-hello-clear ()
  (remove-hook 'pre-command-hook #'+my-scratch-hello-clear)
  (with-current-buffer "*scratch*"
    (let ((inhibit-redisplay t))
      (erase-buffer)
      (lisp-interaction-mode)
      (insert +my-scratch-hello-message ?\n)
      (backward-char))))

(defun +my-scratch-hello-setup ()
  (remove-hook 'emacs-startup-hook #'+my-scratch-hello-setup)
  (with-current-buffer "*scratch*"
    (require 'hek-subr)
    (add-hook 'pre-command-hook #'+my-scratch-hello-clear)
    (insert ?\n)

    ;; Emacs logo.
    (insert-image (create-image (concat config/emacs-conf-dir "images/gnu_emacs.png")))
    (hek-center-line nil (when (daemonp) 14))
    (insert ?\n)

    ;; Emacs version.
    (insert (propertize
             (concat "GNU Emacs "
                     emacs-version
                     (when (daemonp) " (server)"))
             'face '(variable-pitch
                     (:slant italic :height 120))))
    (hek-center-line)
    (insert ?\n)

    (goto-char (point-min))))

(if (and command-line-args (cdr command-line-args)) ;; has extra arguments
    (+my-scratch-hello-clear)
  (setq initial-major-mode #'fundamental-mode)
  (add-hook 'emacs-startup-hook #'+my-scratch-hello-setup))


;;;;;** Input methods

(hek-usepkg hek-exim
  :from local
  :config
  (pcase system-type
    ('gnu/linux
     (hek-exim-fcitx5-dbus-init-state)
     (setq hek-exim-get-source-function #'hek-exim-fcitx5-dbus-get-state
           hek-exim-set-source-function #'hek-exim-fcitx5-dbus-set-state))
    ('windows-nt
     (setq hek-exim-get-source-function #'w32-get-ime-open-status
           hek-exim-set-source-function #'w32-set-ime-open-status))
    ('android
     (hek-exim-textconv-init-state)
     (setq hek-exim-get-source-function #'hek-exim-textconv-get-state
           hek-exim-set-source-function #'hek-exim-textconv-set-state))
    (_
     (user-error "input method is not available"))
    )
  (when (featurep 'meow)
    (setq hek-exim-automodal-hooks
          '(meow-insert-enter-hook . meow-insert-exit-hook))
    (hek-exim-automodal-mode))
  (hek-exim-inlinetext-mode)
  (setq hek-exim-verbose t) ;; For debug.
  :bind
  (("M-SPC" . hek-exim-toggle)
   ("M-S-SPC" . hek-exim-inlinetext-create)))


;;;;;** Undo-redo

;;; vundo :: visual undo true
;;; https://github.com/casouri/vundo
(hek-usepkg vundo
  :from package
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;;;;** Directory view

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


;;;;;** Terminal

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
  (setq vterm-shell config/shell
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
     (list :family (car config/term-font)
           :height (cdr config/term-font)))
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


;;;;;** Version control systems

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


;;;;;* KEY BINDINGS

;;;;;** Customized keymaps

(defvar-keymap +my-app-prefix-map
  "d"  #'dirvish  ;; dired
  "g"  #'magit
  "t"  #'vterm  ;; shell
  )
(fset '+my-app-prefix-map +my-app-prefix-map)

(defvar-keymap +my-goto-prefix-map
  "d"  #'xref-find-definitions
  "r"  #'xref-find-references
  "o"  #'xref-pop-marker-stack
  "O"  #'xref-pop-to-location
  "h"  #'eldoc-doc-buffer
  "D"  #'eglot-find-declaration
  "I"  #'eglot-find-implementation
  "T"  #'eglot-find-typeDefinition
  "y"  #'consult-imenu ;; imenu
  "l"  #'consult-goto-line
  "m"  #'consult-mark
  )
(fset '+my-goto-prefix-map +my-goto-prefix-map)

(defvar-keymap +my-diagnose-prefix-map
  "e"  (lambda (prev) (interactive "P")
         (if prev (flymake-goto-prev-error) (flymake-goto-next-error)))
  "f"  #'eglot-code-action-quickfix
  "s"  #'flyspell-goto-next-error
  "S"  #'flyspell-correct-word-before-point
  )
(fset '+my-diagnose-prefix-map +my-diagnose-prefix-map)

(defvar-keymap +my-rectedit-prefix-map
  "d"  #'kill-rectangle
  "y"  #'copy-rectangle-as-kill
  "D"  #'delete-rectangle
  "p"  #'yank-rectangle
  "C"  #'clear-rectangle
  "o"  #'open-rectangle
  "c"  #'string-rectangle
  "i"  #'string-insert-rectangle
  "SPC" #'rectangle-mark-mode
  )
(fset '+my-rectedit-prefix-map +my-rectedit-prefix-map)

(defvar-keymap +my-editing-prefix-map
  "l"  #'downcase-dwim
  "u"  #'upcase-dwim
  "U"  #'capitalize-dwim
  "j"  #'join-line
  "s"  #'isearch-forward
  "S"  #'isearch-forward-regexp
  "r"  #'query-replace
  "R"  #'query-replace-regexp
  "p"  #'consult-yank-from-kill-ring
  "P"  #'clipboard-yank
  "Y"  #'clipboard-kill-ring-save
  "/"  #'comment-line
  "?"  #'comment-or-uncomment-region
  "e"  +my-rectedit-prefix-map
  )
(fset '+my-editing-prefix-map +my-editing-prefix-map)

(defvar-keymap +my-search-prefix-map
  "s"  #'rg-literal
  "S"  #'rg
  )
(fset '+my-search-prefix-map +my-search-prefix-map)

(defvar-keymap +my-tabbar-prefix-map
   "T"  #'tab-bar-mode
   "t"  #'tab-switch
   "D"  #'dired-other-tab
   "F"  #'find-file-other-tab
   "b"  #'switch-to-buffer-other-tab
   "SPC" #'other-tab-prefix
   "+"  #'tab-new
   "*"  #'tab-duplicate
   "n"  #'tab-rename
   "x"  #'tab-close
   "."  #'tab-close-other
   "u"  #'tab-undo
   "m"  #'tab-move
   "["  #'tab-previous
   "]"  #'tab-next
   "l"  #'tab-list
   )
(fset '+my-tabbar-prefix-map +my-tabbar-prefix-map)

(defvar-keymap +my-window-prefix-map
   "0"  #'delete-window
   "1"  #'delete-other-windows
   "."  #'delete-other-windows
   "2"  #'split-window-below
   "-"  #'split-window-below
   "_"  (lambda () (interactive) (select-window (split-window-below)))
   "3"  #'split-window-right
   "\\" #'split-window-right
   "|"  (lambda () (interactive) (select-window (split-window-right)))
   "4"  #'ctl-x-4-map
   "5"  #'ctl-x-5-map
   "o"  #'other-window-prefix
   "w"  #'ace-select-window
   "h"  #'windmove-left
   "j"  #'windmove-down
   "k"  #'windmove-up
   "l"  #'windmove-right
   "R"  #'ace-swap-window
   "x"  #'delete-window
   "H"  #'windmove-delete-left
   "J"  #'windmove-delete-down
   "K"  #'windmove-delete-up
   "L"  #'windmove-delete-right
   "^"  #'enlarge-window
   ">"  #'enlarge-window-horizontally
   "v"  #'shrink-window
   "<"  #'shrink-window-horizontally
   "="  #'balance-windows
   "r"  #'window-configuration-to-register
   "u"  #'winner-undo
   "U"  #'winner-redo
   )
(fset '+my-window-prefix-map +my-window-prefix-map)

(defvar-keymap +my-buffer-prefix-map
   "b"  #'consult-buffer
   "B"  #'consult-buffer-other-window
   "l"  #'list-buffers
   "o"  #'mode-line-other-buffer
   "j"  #'previous-buffer
   "k"  #'next-buffer
   "x"  #'kill-buffer
   "s"  #'save-buffer
   "S"  #'save-some-buffers
   "R"  #'read-only-mode
   "n"  #'narrow-to-region
   "N"  #'widen
   )
(fset '+my-buffer-prefix-map +my-buffer-prefix-map)

(defvar-keymap +my-open-prefix-map
   "f"  #'find-file
   "F"  #'find-file-other-window
   "r"  #'consult-recent-file
   "s"  #'consult-find
   "h"  #'hexl-find-file
   "d"  #'dired
   "D"  #'dired-other-window
   )
(fset '+my-open-prefix-map +my-open-prefix-map)

(fset '+my-project-prefix-map project-prefix-map)

(defvar-keymap +my-register-prefix-map
  "r"  #'consult-register
  "l"  #'consult-register-load
  "s"  #'consult-register-store
  "."  #'point-to-register
  "w"  #'window-configuration-to-register
  "k"  #'kmacro-to-register
  "j"  #'jump-to-register
  "y"  #'copy-to-register
  "x"  #'copy-rectangle-to-register
  "n"  #'number-to-register
  "p"  #'insert-register
  "A"  #'append-to-register
  "I"  #'prepend-to-register
  "+"  #'increment-register
  )
(fset '+my-register-prefix-map +my-register-prefix-map)

(defvar-keymap +my-kmacro-prefix-map
   "p"  #'consult-kmacro
   "q"  #'kbd-macro-query
   "n"  #'kmacro-name-last-macro
   "d"  #'kmacro-delete-ring-head
   "j"  #'kmacro-cycle-ring-next
   "k"  #'kmacro-cycle-ring-previous
   "e"  #'kmacro-edit-macro
   "E"  #'kmacro-edit-lossage
   "c"  #'kmacro-insert-counter
   "C"  #'kmacro-set-counter
   "+"  #'kmacro-add-counter
   "F"  #'kmacro-set-format
   "r"  #'kmacro-to-register
   )
(fset '+my-kmacro-prefix-map +my-kmacro-prefix-map)

(defvar-keymap +my-pairedit-prefix-map
  "'"  #'hek-surround-region
  "\"" #'hek-surround-region
  "`"  #'hek-surround-region
  "("  #'hek-surround-region
  "["  #'hek-surround-region
  "{"  #'hek-surround-region
  "<"  #'hek-surround-region
  "s"  #'hek-surround-region
  "x"  #'hek-unsurround-region
  "d"  #'hek-unsurround-region
  )
(fset '+my-pairedit-prefix-map +my-pairedit-prefix-map)


;;;;;** Modify pre-defined keymap

(let ((map global-map)
      (defs `((,(kbd "C-M-v") . scroll-other-window)
              (,(kbd "C-M-S-v") . scroll-other-window-down)
              ("\C-xm" . nil)
              ("\C-x4m" . nil)
              ("\C-x5m" . nil))))
  (dolist (def defs)
    (define-key map (car def) (cdr def))))


;;;;;** Modal editing

;;; Meow, https://github.com/meow-edit/meow
(hek-usepkg meow
  :from package
  :init
  ;; --- pre-start configs ---
  (setq meow-use-cursor-position-hack t
        meow-use-enhanced-selection-effect t)
  ;; --- start meow ! ---
  (meow-global-mode 1)
  :config
  ;; --- functions ---
  (defun +meow-reverse-or-negarg ()
    (interactive)
    (call-interactively (if mark-active #'meow-reverse #'negative-argument)))
  (defun +meow-delete-region-or-char ()
    (interactive)
    (call-interactively (if mark-active 'delete-region 'delete-char)))
  ;; --- key bindings ---
  (meow-normal-define-key
    ;; -- numbers --
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    '("-" . +meow-reverse-or-negarg)
    ;; -- basic cursor moving --
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)
    ;; -- word-based selections --
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    ;; -- object-based selections --
    '("x" . meow-line)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("m" . meow-join)
    ;; -- thing-based selections --
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    ;; -- find-based selections --
    '("f" . meow-find)
    '("t" . meow-till)
    ;; -- selection manipulating --
    ;;'("-" . meow-reverse)
    '("g" . meow-cancel-selection)
    '("z" . meow-pop-selection)
    ;; -- kill, delete, copy & paste --
    '("d" . meow-kill)
    '("D" . +meow-delete-region-or-char)
    '("y" . meow-save)
    '("p" . meow-yank)
    '("r" . meow-replace)
    ;; -- search/jump --
    '("/" . consult-line)
    '("?" . avy-goto-line)
    '(";" . avy-goto-word-or-subword-1)
    '(":" . avy-goto-char-timer)
    '("n" . meow-search)
    '("v" . scroll-up-command)
    '("V" . scroll-down-command)
    '("M" . recenter-top-bottom)
    ;; -- undo/redo --
    '("u" . meow-undo)
    '("U" . vundo) ;; candidates undo-redo / meow-undo-in-selection
    ;; -- enter insert mode --
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("c" . meow-change)
    ;; -- grab (beacon mode) --
    '("G" . meow-grab)
    '("Y" . meow-sync-grab)
    '("R" . meow-swap-grab)
    '("Z" . meow-pop-grab)
    ;; -- others --
    '("q" . meow-quit)
    '("Q" . kill-current-buffer)
    '("'" . repeat)
    '("\"" . +my-pairedit-prefix-map)
    '("<escape>" . ignore)
    )
  (setq meow-char-thing-table
    '((?\( . round)
      (?\) . round)
      (?\[ . square)
      (?\] . square)
      (?\{ . curly)
      (?\} . curly)
      (?'  . string)
      (?\" . string)
      (?y . symbol)
      (?w . window)
      (?b . buffer)
      (?p . paragraph)
      (?l . line)
      (?f . defun)
      (?. . sentence)))
  (meow-motion-overwrite-define-key
    ;; '("j" . meow-next)
    ;; '("k" . meow-prev)
    '("M-h" . backward-char)
    '("M-j" . next-line)
    '("M-k" . previous-line)
    '("M-l" . forward-char)
    '("<escape>" . ignore)
    )
  (meow-leader-define-key
    '("1" . meow-digit-argument)
    '("2" . meow-digit-argument)
    '("3" . meow-digit-argument)
    '("4" . meow-digit-argument)
    '("5" . meow-digit-argument)
    '("6" . meow-digit-argument)
    '("7" . meow-digit-argument)
    '("8" . meow-digit-argument)
    '("9" . meow-digit-argument)
    '("0" . meow-digit-argument)
    '("g" . ignore)
    '("\\" . +my-app-prefix-map)
    '("j" . +my-goto-prefix-map)
    '("d" . +my-diagnose-prefix-map)
    '("e" . +my-editing-prefix-map)
    '("s" . +my-search-prefix-map)
    '("t" . +my-tabbar-prefix-map)
    '("w" . +my-window-prefix-map)
    '("b" . +my-buffer-prefix-map)
    '("o" . +my-open-prefix-map)
    '("r" . +my-register-prefix-map)
    '("k" . +my-kmacro-prefix-map)
    '("p" . +my-project-prefix-map)
    '("TAB" . other-window)
    '("RET" . execute-extended-command)
    `("SPC" . ,(lambda () (interactive)
                 (pulse-momentary-highlight-one-line nil 'cursor)
                 (hek-describe-buffer-file)))
    '("`" . mode-line-other-buffer)
    '("/" . meow-keypad-describe-key)
    '("?" . meow-cheatsheet)
    )
  (meow-define-keys 'insert
    '("M-h" . backward-char)
    '("M-j" . next-line)
    '("M-k" . previous-line)
    '("M-l" . forward-char)
    '("M-b" . forward-word)
    '("M-e" . backword-word)
    )
  (setf (alist-get 'meow-kill meow-selection-command-fallback) #'delete-char)
  ;; --- behaviors ---
  (setq meow-keypad-message nil
        meow-display-thing-help t ;; See the hack on `meow-thing-prompt' below.
        meow-keypad-describe-delay 1
        meow-keypad-self-insert-undefined nil
        meow-keypad-start-keys '((?c . ?c) (?x . ?x))
        meow-keypad-meta-prefix ?M
        meow-keypad-ctrl-meta-prefix ?m
        meow-select-on-change nil)
  (setq meow-mode-state-list
        '((vterm-mode . insert)))
  (setq meow-grab-fill-commands
        '(query-replace query-replace-regexp))
  ;; HACK: delay before xxx-of-thing help.
  (defun meow-thing-prompt (prompt-text)
    (if meow-display-thing-help
        (or (read-char prompt-text nil meow-keypad-describe-delay)
            (read-char (concat (meow--render-char-thing-table) "\n" prompt-text)))
      (read-char prompt-text)))
  ;; --- visual elements ---
  (setq meow-replace-state-name-list
    '((normal . "N")
      (motion . "M")
      (keypad . "K")
      (insert . "I")
      (beacon . "B")))
  (setq meow-cursor-type-insert '(bar . 3)
        meow-cursor-type-region-cursor '(bar . 1)
        meow-cursor-type-motion 'hollow)
  ;;(meow-setup-indicator) ;; Rendered by doom-modeline.
  ;;(meow-setup-line-number)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  )
